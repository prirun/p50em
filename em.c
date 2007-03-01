/* Pr1me Computer emulator, Jim Wilcoxson (prirun@gmail.com), April 4, 2005
   Copyright (C) 2005, Jim Wilcoxson (prirun@gmail.com).  All Rights Reserved.

   Emulates a Prime Computer system by:
   - booting from a Prime disk image (normal usage)
   - booting from a Prime MAGSAV tape
   - restoring a Prime R-mode .save image from the host file system

   This is a project in development, so please don't publish it.
   Comments, suggestions, corrections, and general notes that you're
   interested in a Prime emulation project are welcome and
   appreciated.

   -------------
   Usage:  (to boot from pdev 2466, dev '26, unit 3):

   $ time ./em -tport 8000 -cpuid 5 -boot 14714 -map MFD.2462/PRIRUN/RING0.MAP MFD.2462/PRIRUN/RING3.MAP 2>err

   Disk boot device is 14uc4, tape is 10005, 
   where u=1/3/5/7 for units 0/1/2/3
   and c=1/3/5/7 for controller 26/27/...
      (See complete boot table below)

   NOTE: the -map command is optional, but is needed to set the
   real-time clock automatically.  If not available, use the Primos SE
   command to set the clock manually after the system boots.

   -------------
   Usage:  (to load and start an R-mode runfile directly from the Unix FS)

   $ ./em -ss 114 -boot *DOS64 2>/dev/null   (-ss optional)


   -------------
   Usage:  to load SAM.SAVE from Unix FS and run diagnostics from pdev 2466

$ time ./em  -cpuid 5 -ss 14114 -boot SAM.SAVE 2>err

[SAM Rev. 16.2, DTS Release: 0004.A, Copyright (c) 1990, Prime Computer, Inc.]
Enter physical device = 2466
QUICK VERIFY MODE Enabled; Enter 'RESET QVFY' for normal operation. 
Enter 'SET DCM' to display CASE messages. 
Enter 'LOAD;RUN' for Default Execution

SAM> 
   
   --------------
   Usage:  to load initial boot from tape, then prompt for disk pdev

$ time ./em -boot 1005 -tport 8000 -cpuid 5
Boot file is dev14u0             <--- note tape drive boot
Sense switches set to 1005       <--- these cause pdev prompt
[BOOT Rev. 20.2.3 Copyright (c) 1987, Prime Computer, Inc.]

PHYSICAL DEVICE=2466

DISK ERROR, STATUS: 000001 
PHYSICAL DEVICE=

   ---------------
   Usage:  to load .SAVE image from tape:

$ time ./em -boot 10005
[BOOT Rev. 20.2.3 Copyright (c) 1987, Prime Computer, Inc.]

RUN FILE TREENAME=MFD>DOS>DOS.SAVE

BOOTING FROM MT0    MFD>DOS>DOS.SAVE


PRIMOS II REV 20.0 03/15/85 (AT 170000) 
Copyright (c) Prime Computer, Inc. 1985.
PRIMOS II is being phased out.  To boot PRIMOS return to CP mode. 
("BOOT 14xxx" will autoboot PRIMOS.)

OK: 
   ---------------

   Instruction details are spewed to trace.log depending on the trace flags.

   IMPORTANT NOTE: this only runs on a big-endian machine, like the Prime.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <setjmp.h>
#include <sys/time.h>
#include "syscom/keys.ins.cc"
#include "syscom/errd.ins.cc"

/* In SR modes, Prime CPU registers are mapped to memory locations
   0-'37, but only 0-7 are user accessible.  In the post-P300
   architecture, these addresses map to the live register file.

   Locations '40-'57 are reserved for 8 DMC channels, 2 words each.
   Locations '60-'77 are interrupt vectors
   Locations '100-'177 are for external device interrupts 
   see p. A-8 of Sys Arch

   In VI mode, locations 0-'17 are trapped and map to the live
   register file (p 5-17, Sys Arch), though only 0-7 are accessible in
   user mode.
*/

#include "regs.h"
typedef unsigned int ea_t;            /* effective address */
typedef unsigned int pa_t;            /* physical address */

/* the live program counter register is aka microcode scratch register TR7 */

#define RP regs.sym.tr7
#define RPH regs.u16[14]
#define RPL regs.u16[15]

#define SETCC_A \
  crs[KEYS] &= ~0300; \
  if (crs[A] == 0) \
    crs[KEYS] |= 0100; \
  else if (*(short *)(crs+A) < 0) \
    crs[KEYS] |= 0200;

#define SETCC_L \
  crs[KEYS] &= ~0300; \
  if (*(int *)(crs+L) == 0) \
    crs[KEYS] |= 0100; \
  else if (*(int *)(crs+L) < 0) \
    crs[KEYS] |= 0200;

/* NOTE: in previous versions, exponent must be zero for 0.0, but DIAG
   test CPU.FLOAT.V considers a zero fraction and non-zero exponent to
   also be 0.0 (this is a "dirty zero") */
  
#define SETCC_F \
  crs[KEYS] &= ~0300; \
  if (*(short *)(crs+FLTH) < 0) \
    crs[KEYS] |= 0200; \
  else if (*(int *)(crs+FLTH) == 0) \
    crs[KEYS] |= 0100;

#define SETCC_D SETCC_F

/* XEXPC is a dummy to indicate that the C-bit may not be set correctly */

#define XEXPC(onoff) \
  if ((onoff)) crs[KEYS] |= 0100000; \
  else crs[KEYS] &= 077777

#define EXPC(onoff) \
  if ((onoff)) crs[KEYS] |= 0100000; \
  else crs[KEYS] &= 077777

#define SETC crs[KEYS] |= 0100000
#define CLEARC crs[KEYS] &= 077777

/* EXPCL sets both the C and L bits for shift instructions
   NOTE: unlike EXPC, this doesn't clear anything! */

#define EXPCL(onoff) \
  if ((onoff)) crs[KEYS] |= 0120000;

#define SETCL crs[KEYS] |= 0120000;

/* XSETL is a dummy to indicate that the L-bit may not be set correctly */

#define XSETL(onoff) \
  if ((onoff)) crs[KEYS] |= 020000; \
  else crs[KEYS] &= ~020000;

#define SETL(onoff) \
  if ((onoff)) crs[KEYS] |= 020000; \
  else crs[KEYS] &= ~020000;

/* macro for restricted instructions (uses current program counter) */

#define RESTRICT() if (RP & RINGMASK32) fault(RESTRICTFAULT, 0, 0);

/* same macro, but uses a passed program counter */

#define RESTRICTR(rpring) if ((rpring) & RINGMASK32) fault(RESTRICTFAULT, 0, 0);


/* Table for unusual instructions that aren't implemented but we want to
   print something when they're encountered */

unsigned short gen0tab[] = {
  000503,          /* EMCM - enter machine check mode */
  000501,          /* LMCM - leave machine check mode */
  001304,          /* MDEI - memory diagnostic */
  001305,          /* MDII - memory diagnostic */
  001324,          /* MDIW - memory diagnostic */
  001306,          /* MDRS - memory diagnostic */
  001307,          /* MDWC - memory diagnostic */
  000021,          /* RMC - reset machine check */
  000311,          /* VIFY - ucode verify */
  001113};         /* XVFY - extended ucode verify */

#define GEN0TABSIZE sizeof(gen0tab)/sizeof(unsigned short)

char gen0nam[][5] = {
  "EMCM",
  "LMCM",
  "MDEI",
  "MDII",
  "MDIW",
  "MDRS",
  "MDWC",
  "RMC",
  "VIFY",
  "XVFY"};


/* trace flags to control various aspects of emulator tracing:

   T_EAR	trace R-mode effective address calculation
   T_EAV	trace V-mode effective address calculation
   T_EAI	trace I-mode effective address calculation
   T_FLOW       instruction summary
   T_INST	detailed instruction trace
   T_MODE       trace CPU mode changes
   T_EAAP       AP effective address calculation
   T_DIO        disk I/O
   T_TIO        tape I/O
   T_TERM       terminal output (tnou[a])
   T_MAP        segmentation
   T_PCL        PCL instructions
   T_FAULT      Faults
   T_PX         Process exchange
*/

#define TB_EAR   0x00000001
#define TB_EAV   0x00000002
#define TB_EAI   0x00000004
#define TB_INST  0x00000008
#define TB_FLOW  0x00000010
#define TB_MODE  0x00000020
#define TB_EAAP  0x00000040
#define TB_DIO   0x00000080
#define TB_MAP   0x00000100
#define TB_PCL   0x00000200
#define TB_FAULT 0x00000400
#define TB_PX    0x00000800
#define TB_TIO   0x00001000
#define TB_TERM  0x00002000

#define T_EAR TB_EAR
#define T_EAV TB_EAV
#define T_EAI TB_EAI
#define T_INST TB_INST
#define T_FLOW TB_FLOW
#define T_MODE TB_MODE
#define T_EAAP TB_EAAP
#define T_DIO TB_DIO
#define T_MAP TB_MAP
#define T_PCL TB_PCL
#define T_FAULT TB_FAULT
#define T_PX TB_PX
#define T_TIO TB_TIO
#define T_TERM TB_TERM

#if defined(NOTRACE)
  #define TRACE(flags, formatargs...)
#else
  #define TRACE(flags, formatargs...) if (traceflags & (flags)) fprintf(tracefile,formatargs)
  #define TRACEA(formatargs...) fprintf(tracefile,formatargs)
#endif

/* traceflags is the variable used to test tracing of each instruction

   traceuser is the user number to trace, 0 meaning any user

   savetraceflags hold the real traceflags, while "traceflags" switches
   on and off for each instruction

   traceprocs is an array of procedure names we're tracing, with flags
   and various associated data

   numtraceprocs is the number of entries in traceprocs, 0=none

   TRACEUSER is a macro that is true if the current user is being traced
*/

int traceflags=0;                    /* each bit is a trace flag */
int savetraceflags=0;                /* see ITLB */
int traceuser=0;                     /* OWNERL to trace */
int numtraceprocs=0;
#define MAXTRACEPROCS 2
struct {
  char  name[11];                    /* procedure name */
  int   ecb;                         /* ecb ea of proc */
  int   sb;                          /* sb before the call */
  int   oneshot;                     /* disable trace after call? */
} traceprocs[MAXTRACEPROCS];

FILE *tracefile;                     /* trace.log file */
#define TRACEUSER (traceuser == 0 || crs[OWNERL] == traceuser)

int intvec=-1;                       /* currently raised interrupt (if >= zero) */

/* NOTE: Primos II gives "NOT FOUND" on startup 2460 if sense switches
   are set to 014114.  But DIAGS like this sense switch setting. :( */

unsigned short sswitch = 014114;     /* sense switches, set with -ss & -boot*/

/* NOTE: the default cpuid is a 4150: 2 MIPS, 32MB of memory */

unsigned short cpuid = 27;           /* STPM CPU model, set with -cpuid */

unsigned long instcount=0;      /* global instruction count */

unsigned short inhcount = 0;         /* number of instructions to stay inhibited */

unsigned int instpermsec = 2000;     /* initial assumption for inst/msec */

jmp_buf jmpbuf;                      /* for longjumps to the fetch loop */

/* The standard Prime physical memory limit on early machines is 8MB.
   Later machines have higher memory capacities, up to 1GB, using 
   32-bit page tables. 
   NOTE: rev 20 is limited to 32MB on all machines. */

#define MEMSIZE 16*1024*1024   /* 32 MB */
unsigned short mem[MEMSIZE];   /* system's physical memory */

#define MAKEVA(seg,word) ((((int)(seg))<<16) | (word))

/* returns the incremented value of a virtual address, wrapping to word
   zero at the end of a segment (word portion = 0177777) */

#define INCVA(ea,n) (((ea) & 0xFFFF0000) | ((ea)+(n)) & 0xFFFF)

/* STLB cache is defined here.  There are several different styles on
   various Prime models.  This is modeled after the 6350 STLB, but is
   only 1-way associative and doesn't have slots dedicated to I/O
   segments */

#define STLBENTS 512

typedef struct {
  char valid;                 /* 1 if STLB entry is valid, zero otherwise */
  char unmodified;            /* 1 if page hasn't been modified, 0 if modified */
  //  char shared;                /* 1 if page is shared and can't be cached */
  char access[4];             /* ring n access rights */
  unsigned short procid;      /* process id for segments >= '4000 */
  unsigned short seg;         /* segment number */
  unsigned int ppn;           /* physical page number (15 bits = 64MB limit) */
  unsigned short *pmep;       /* pointer to page table flag word */
  unsigned long load_ic;      /* instruction where STLB was loaded (for debug) */
} stlbe_t;

stlbe_t stlb[STLBENTS];

unsigned long long bitmask64[65] = {0,
   1LL<<63, 1LL<<62, 1LL<<61, 1LL<<60, 1LL<<59, 1LL<<58, 1LL<<57, 1LL<<56,
   1LL<<55, 1LL<<54, 1LL<<53, 1LL<<52, 1LL<<51, 1LL<<50, 1LL<<49, 1LL<<48,
   1LL<<47, 1LL<<46, 1LL<<45, 1LL<<44, 1LL<<43, 1LL<<42, 1LL<<41, 1LL<<40,
   1LL<<39, 1LL<<38, 1LL<<37, 1LL<<36, 1LL<<35, 1LL<<34, 1LL<<33, 1LL<<32,
   1LL<<31, 1LL<<30, 1LL<<29, 1LL<<28, 1LL<<27, 1LL<<26, 1LL<<25, 1LL<<24,
   1LL<<23, 1LL<<22, 1LL<<21, 1LL<<20, 1LL<<19, 1LL<<18, 1LL<<17, 1LL<<16,
   1LL<<15, 1LL<<14, 1LL<<13, 1LL<<12, 1LL<<11, 1LL<<10, 1LL<<9, 1LL<<8,
   1LL<<7, 1LL<<6, 1LL<<5, 1LL<<4, 1LL<<3, 1LL<<2, 1LL<<1, 1LL};

unsigned int bitmask32[33] = {0,
			    0x80000000, 0x40000000, 0x20000000, 0x10000000,
			    0x08000000, 0x04000000, 0x02000000, 0x01000000,
			    0x00800000, 0x00400000, 0x00200000, 0x00100000,
			    0x00080000, 0x00040000, 0x00020000, 0x00010000,
			    0x00008000, 0x00004000, 0x00002000, 0x00001000,
			    0x00000800, 0x00000400, 0x00000200, 0x00000100,
			    0x00000080, 0x00000040, 0x00000020, 0x00000010,
			    0x00000008, 0x00000004, 0x00000002, 0x00000001};

unsigned int bitmask16[17] = {0,
			    0x8000, 0x4000, 0x2000, 0x1000,
			    0x0800, 0x0400, 0x0200, 0x0100,
			    0x0080, 0x0040, 0x0020, 0x0010,
			    0x0008, 0x0004, 0x0002, 0x0001};

unsigned int prevpc;                 /* backed program counter */

unsigned short amask;                /* address mask */

#define FAULTMASK32 0x80000000       /* fault bit */
#define RINGMASK32  0x60000000       /* ring bits */
#define EXTMASK32   0x10000000       /* E-bit */
#define SEGMASK32   0x0FFF0000       /* segment number */
#define RINGMASK16  0x6000           /* ring bits */
#define EXTMASK16   0x1000           /* E-bit */

#define DTAR(ea) (((ea)>>26) & 3)
#define SEGNO(ea) (((ea)>>16) & 07777)
#define PAGENO(ea) (((ea)>>10) & 077)


/* Fault/interrupt vectors */

#define RESTRICTFAULT 062
#define PROCESSFAULT  063
#define PAGEFAULT     064
#define SVCFAULT      065
#define UIIFAULT      066
#define PARITYCHECK   067
#define MACHCHECK     070
#define MISSMEMCHECK  071
#define ILLINSTFAULT  072
#define ACCESSFAULT   073
#define ARITHFAULT    074
#define STACKFAULT    075
#define SEGFAULT      076
#define POINTERFAULT  077

ea_t tnoua_ea=0, tnou_ea=0;
int verbose;                         /* -v (not used anymore) */
int domemdump;                       /* -memdump arg */
int pmap32bits;                      /* true if 32-bit page maps */
int csoffset;                        /* concealed stack segment offset */
int tport;                           /* -tport option (incoming terminals) */
int nport;                           /* -nport option (PNC/Ringnet) */

/* load map related data, specified with -map */

#define MAXSYMBOLS 15000
#define MAXSYMLEN 9
int numsyms = 0;
static struct {
  char symname[MAXSYMLEN];
  ea_t address;
  char symtype;                /* o=other, c=common, e=ecb, p=proc, l=linkbase */
} mapsym[MAXSYMBOLS];


/* returns an index to a symbol, based on an address and type
   match; if the address isn't found exactly, the index returned
   will be the address lower than the requested address, or -1
   if the symbol table is empty or the requested address is
   lower than any in the symbol table */

int findsym(ea_t addr, char type) {
  int low, high, mid, saveix;

  addr &= 0xFFFFFFF;      /* strip fault, ring, E bits */

  low = 0;
  high = numsyms-1;
  mid = -1;
  while (low <= high) {
    mid = (low+high)/2;
    if (addr < mapsym[mid].address)
      high = mid-1;
    else if (addr == mapsym[mid].address)
      break;
    else if (addr > mapsym[mid].address && mid != numsyms-1 && addr >= mapsym[mid+1].address)
      low = mid+1;
    else
      break;
  }
  saveix = mid;
  if (type != 'x' && mid >= 0)
    while (addr > mapsym[saveix].address && saveix != numsyms-1 && addr > mapsym[saveix+1].address && mapsym[saveix].symtype != type)
      saveix++;
  return saveix;
}


addsym(char *sym, unsigned short seg, unsigned short word, char type) {
  short symlen,ix,ix2;
  ea_t addr;

  symlen = strlen(sym);
  if (symlen > 0 && symlen < MAXSYMLEN) {
    addr = MAKEVA(seg, word);
    ix = findsym(addr, 'x');
    if (ix+1 < numsyms)          /* make room for the new symbol */
      for (ix2 = numsyms; ix2 > ix; ix2--)
	mapsym[ix2] = mapsym[ix2-1];
    //TRACEA("%s = %o/%o\n", sym, seg, words);
    strcpy(mapsym[ix+1].symname, sym);
    mapsym[ix+1].address = addr;
    mapsym[ix+1].symtype = type;
    numsyms++;
  }
}


readloadmap(char *filename) {
  FILE *mapf;
  char line[100];
  int lc,ix;
  char sym[100];
  unsigned int segno, wordno, ecbseg, ecbword, pbseg, pbword, lbseg, lbword;
  ea_t lastaddr;

  TRACEA("Reading load map from %s... ", filename);
  if ((mapf = fopen(filename, "r")) == NULL) {
    perror("Map file open");
    fatal(NULL);
  }
  lc = 0;
  while (fgets(line, sizeof(line), mapf) != NULL) {
    lc++;
    if (strstr(line, "*START"))
      break;
    if (sscanf(line, "%s %o %o %o %o %*o %*o %o %o", sym, &ecbseg, &ecbword, &pbseg, &pbword, &lbseg, &lbword) == 7) {
      addsym(sym, ecbseg, ecbword, 'e');
      addsym(sym, pbseg, pbword, 'p');
      addsym(sym, lbseg, lbword, 'l');
      //printf("adding proc symbol, line=%s\n", line);
      if (tnou_ea == 0 && strcmp(sym,"TNOU") == 0)
	tnou_ea = MAKEVA(ecbseg, ecbword);
      if (tnoua_ea == 0 && strcmp(sym,"TNOUA") == 0)
	tnoua_ea = MAKEVA(ecbseg, ecbword);
    } else if (sscanf(line, "%s %o %o", sym, &segno, &wordno) == 3) {
      addsym(sym, segno, wordno, 'x');
      //printf("adding symbol, line=%s\n", line);
    } else if (strcspn(line, " \n") == 0)
      continue;
    else
      TRACEA("Can't parse map line #%d: %s\n", lc, line);
    if (numsyms == MAXSYMBOLS) {
      TRACEA("Symbol table limit!");
      break;
    }
  }
  fclose(mapf);
  TRACEA("%d symbols loaded\n", numsyms);

  lastaddr = 0;
  for (ix=0; ix < numsyms; ix++) {
    if (mapsym[ix].address < lastaddr)
      TRACEA("Symbol table out of order: ix=%d, sym=%s, addr=%o/%o, lastaddr=%o/%o\n", ix, mapsym[ix].symname, mapsym[ix].address>>16, mapsym[ix].address&0xffff, lastaddr>>16, lastaddr&0xffff);
    lastaddr = mapsym[ix].address;
  }
}

/* returns a pointer to a static character string like DSKBLK+25, to
   print with the effective address for an instruction.  There is a
   stack of return results so that if this is called twice on a
   function call, different results can be returned */

char *searchloadmap(int addr, char type) {
  short ix, diff;

#define MAXBUFIX 10

  static char blank = 0;
  static char buf[MAXBUFIX][100];
  static int bufix=-1;

  if ((SEGNO(addr) <= 01777 | SEGNO(addr) >= 06000) &&
      (ix = findsym(addr, type)) > 0) {
    diff = addr - mapsym[ix].address;
    if (diff) {
      if (++bufix == MAXBUFIX)
	bufix = 0;
      snprintf(buf[bufix], sizeof(buf[0]), "%s+'%o", mapsym[ix].symname, diff);
      return buf[bufix];
    } else
      return mapsym[ix].symname;
  } else 
    return &blank;
}


/* intended memory access types:
   1 = PCL (PACC)
   2 = read (RACC)
   3 = write (WACC)
   4 = execute (XACC)
*/
#define PACC 0
#define RACC 2
#define WACC 3
#define XACC 4

void fault(unsigned short fvec, unsigned short fcode, ea_t faddr);


/* NOTE: this is the 6350 STLB hash function, giving a 9-bit index 0-511 */

#define STLBIX(ea) ((((((ea) >> 12) ^ (ea)) & 0xc000) >> 7) | (((ea) & 0x70000) >> 12) | ((ea) & 0x3c00) >> 10)

/* maps a Prime 28-bit virtual address to a physical memory
   address, checks access, returns actual access (for PCL)

   May cause:
   - segment fault if segment number is too big
   - segment fault if segment's fault bit is set
   - access fault if intended access isn't permitted
   - page fault if page isn't resident

   Typically, the real program counter is passed in and the ring
   bits from it are used.  For some special cases (gate PCL), a
   fake program counter is passed in with the desired ring bits,
   for example, 0, a R0 program counter, or 020000/0, a R1 program
   counter.  (getr,putr)(16,32,64) allow specifying a PC.
*/

pa_t mapva(ea_t ea, short intacc, unsigned short *access, ea_t rp) {
  short relseg,seg,nsegs,ring;
  unsigned short pte, stlbix;
  stlbe_t *stlbp;
  unsigned int dtar,sdw,staddr,ptaddr,pmaddr,ppn;
  pa_t pa;

  seg = SEGNO(ea);
  ring = ((rp | ea) >> 29) & 3;  /* current ring | ea ring = access ring */

  if ((seg > 0 && (crs[MODALS] & 4)) || (seg == 0 && (crs[MODALS] & 020))) {
    stlbix = STLBIX(ea);
    stlbp = stlb+stlbix;
    if (stlbix >= STLBENTS) {
      printf("STLB index %d is out of range for va %o/%o!\n", stlbix, ea>>16, ea&0xffff);
      fatal(NULL);
    }

    /* if the STLB entry isn't valid, or the segments don't match,
       or the segment is private and the process id doesn't match,
       then the STLB has to be loaded first */

    if (!stlbp->valid || stlbp->seg != seg || (seg >= 04000 && stlbp->procid != crs[OWNERL])) {
      dtar = *(unsigned int *)(crs+DTAR0-2*DTAR(ea));  /* get dtar register */
      nsegs = 1024-(dtar>>22);
      relseg = seg & 0x3FF;     /* segment within segment table */
      TRACE(T_MAP, "   MAP: ea=%o/%o, seg=%o, dtar=%o, nsegs=%d, relseg=%d, page=%d\n", ea>>16, ea&0xFFFF, seg, dtar, nsegs, relseg, PAGENO(ea));
      if (relseg >= nsegs)
	fault(SEGFAULT, 1, ea);   /* fcode = segment too big */
      staddr = (dtar & 0x003F0000) | ((dtar & 0x7FFF)<<1);
      sdw = *(unsigned int *)(mem+staddr+relseg*2);
      TRACE(T_MAP,"        staddr=%o, sdw=%o\n", staddr, sdw);
      if (sdw & 0x8000)
	fault(SEGFAULT, 2, ea);   /* fcode = sdw fault bit set */
      ptaddr = (((sdw & 0x3F)<<10) | (sdw>>22)) << 6;
      if (pmap32bits) {
	pmaddr = ptaddr + 2*PAGENO(ea);
	pte = mem[pmaddr];
	ppn = mem[pmaddr+1];
      } else {
	pmaddr = ptaddr + PAGENO(ea);
	pte = mem[pmaddr];
	ppn = pte & 0xFFF;
      }
      TRACE(T_MAP,"        ptaddr=%o, pmaddr=%o, pte=%o\n", ptaddr, pmaddr, pte);
      if (!(pte & 0x8000))
	fault(PAGEFAULT, 0, ea);
      mem[pmaddr] |= 040000;     /* set referenced bit */
      stlbp->valid = 1;
      stlbp->unmodified = 1;
      stlbp->access[0] = 7;
      stlbp->access[1] = (sdw >> 12) & 7;
      stlbp->access[3] = (sdw >> 6) & 7;
      stlbp->procid = crs[OWNERL];
      stlbp->seg = seg;
      stlbp->ppn = ppn;
      stlbp->pmep = mem+pmaddr;
      stlbp->load_ic = instcount;
    }
    *access = stlbp->access[ring];
    if (((intacc & *access) != intacc) || (intacc == PACC && ((*access & 3) == 0)))
      fault(ACCESSFAULT, 0, ea);
    if (intacc == WACC && stlbp->unmodified) {
      stlbp->unmodified = 0;
      *(stlbp->pmep) &= ~020000;    /* reset unmodified bit in memory */
    }
    pa = (stlbp->ppn << 10) | (ea & 0x3FF);
    TRACE(T_MAP,"        for ea %o/%o, stlbix=%d, pa=%o	loaded at #%d\n", ea>>16, ea&0xffff, stlbix, pa, stlbp->load_ic);
  } else {
    pa = ea & (MEMSIZE-1);
  }
  if (pa <= MEMSIZE)
    return pa;
  printf(" map: Memory address %o (%o/%o) is out of range!\n", ea, ea>>16, ea & 0xffff);
  fatal(NULL);
  /* NOTE: could also take a missing memory check here... */
}


/* these are shorthand macros for get/put that use the current program
   counter - the typical usage */

#define get16(ea) (get16r((ea),RP))
#define get32(ea) (get32r((ea),RP))
#define get64(ea) (get64r((ea),RP))
#define put16(value, ea) (put16r((value),(ea),RP))
#define put32(value, ea) (put32r((value),(ea),RP))
#define put64(value, ea) (put64r((value),(ea),RP))


unsigned short get16r(ea_t ea, ea_t rpring) {
  pa_t pa;
  unsigned short access;

  /* check for live register access */

  if (ea & 0x80000000) {
    ea = ea & 0xFFFF;
    if (ea < 7)
      return crs[memtocrs[ea]];
    if (ea == 7)                   /* PC */
      return RPL;
    RESTRICTR(rpring);
    if (ea < 020)                 /* CRS */
      return crs[memtocrs[ea]];
    if (ea < 040)                 /* DMX */
      return regs.sym.regdmx[((ea & 036) << 1) | (ea & 1)];
    printf(" Live register address %o too big!\n", ea);
    fatal(NULL);
  }
  return mem[mapva(ea, RACC, &access, rpring)];
}

unsigned int get32r(ea_t ea, ea_t rpring) {
  pa_t pa;
  unsigned short access;
  unsigned short m[2];

  /* check for live register access */

  if (ea & 0x80000000)
    warn("address trap in get32");

  pa = mapva(ea, RACC, &access, rpring);

  if ((pa & 01777) <= 01776)
    return *(unsigned int *)(mem+pa);
  else {
    m[0] = mem[pa];
    m[1] = get16r(INCVA(ea,1), rpring);
    return *(unsigned int *)m;
  }
}

double get64r(ea_t ea, ea_t rpring) {
  pa_t pa;
  unsigned short access;
  unsigned short m[4];

  /* check for live register access */

  if (ea & 0x80000000)
    warn("address trap in get64");

  pa = mapva(ea, RACC, &access, rpring);
  if ((pa & 01777) <= 01774)
    return *(double *)(mem+pa);
  else {
    m[0] = mem[pa];
    m[1] = get16r(INCVA(ea,1), rpring);
    m[2] = get16r(INCVA(ea,2), rpring);
    m[3] = get16r(INCVA(ea,3), rpring);
    return *(double *)m;
  }
}

/* Instruction version of get16 (can be replaced by get16 too...)
   This needs to be checked more... not sure it actually improves performance
   all that much, and it doesn't work for self-modifying code in R-mode or
   Ring 0.  I tried using get64 (with appropriate mask changes) and
   performance was much worse than not prefetching at all on a G4 */

#if 0
inline unsigned short iget16(ea_t ea) {
  static ea_t eafirst = -1;             /* ea of instruction buffer */
  static unsigned short insts[2];       /* instruction buffer */

  if (ea & 0x80000000) {                /* check for R-mode inst in register */
    eafirst = -1;
    return get16(ea);
  }
  if ((ea & 0xFFFFFFFE) != eafirst) {   /* load instruction buffer */
    eafirst = ea & 0xFFFFFFFE;
    *(int *)insts = get32(eafirst);
  }
  return insts[ea & 1];
}
#else
#define iget16(ea) get16((ea))
#endif

put16r(unsigned short value, ea_t ea, ea_t rpring) {
  pa_t pa;
  unsigned short access;

  if (ea & 0x80000000) {
    ea = ea & 0xFFFF;
    if (ea < 7)
      crs[memtocrs[ea]] = value;
    else if (ea == 7) {
      RPL = value;
    } else {
      RESTRICTR(rpring);
      if (ea <= 017)                      /* CRS */
	crs[memtocrs[ea]] = value;
      else if (ea <= 037)                 /* DMX */
	regs.sym.regdmx[((ea & 036) << 1) | (ea & 1)] = value;
      else {
	printf(" Live register store address %o too big!\n", ea);
	fatal(NULL);
      }
    }
  } else {
    pa = mapva(ea, WACC, &access, rpring);
    mem[pa] = value;
  }
}

put32r(unsigned int value, ea_t ea, ea_t rpring) {
  pa_t pa;
  unsigned short access;
  unsigned short *m;

  /* check for live register access */

  if (ea & 0x80000000)
    warn("address trap in put32");

  pa = mapva(ea, WACC, &access, rpring);
  if ((pa & 01777) <= 01776)
    *(unsigned int *)(mem+pa) = value;
  else {
    m = (void *)&value;
    mem[pa] = m[0];
    put16r(m[1], INCVA(ea,1), rpring);
  }
}

put64r(double value, ea_t ea, ea_t rpring) {
  pa_t pa;
  unsigned short access;
  unsigned short *m;

  /* check for live register access */

  if (ea & 0x80000000)
    warn("address trap in put32");

  pa = mapva(ea, WACC, &access, rpring);
  if ((pa & 01777) <= 01774)
    *(double *)(mem+pa) = value;
  else {
    m = (void *)&value;
    mem[pa] = m[0];
    put16r(m[1], INCVA(ea,1), rpring);
    put16r(m[2], INCVA(ea,2), rpring);
    put16r(m[3], INCVA(ea,3), rpring);
  }
}


fatal(char *msg) {

  printf("Fatal error: instruction #%d at %o/%o %s: %o %o\nowner=%o %s, keys=%o, modals=%o\n", instcount, prevpc >> 16, prevpc & 0xFFFF, searchloadmap(prevpc,' '), get16(prevpc), get16(prevpc+1), crs[OWNERL], searchloadmap(*(unsigned int *)(crs+OWNER),' '), crs[KEYS], crs[MODALS]);
  if (msg)
    printf("%s\n", msg);
  /* should do a register dump, RL dump, PCB dump, etc. here... */
  exit(1);
}

warn(char *msg) {
  printf("emulator warning:\n  instruction #%d at %o/%o: %o %o keys=%o, modals=%o\n  %s\n", instcount, prevpc >> 16, prevpc & 0xFFFF, get16(prevpc), get16(prevpc+1),crs[KEYS], crs[MODALS], msg);
}
    


/* NOTE: the calf instruction may be running in an outer ring, so
   accesses to protected data need to use get16r */

void calf(ea_t ea) {
  ea_t pcbp, stackfp, csea;
  unsigned short first,next,last,this;
  unsigned short cs[6];

  pcbp = *(ea_t *)(crs+OWNER);    /* my pcb pointer */

  /* get concealed stack entry address */

  first = get16r(pcbp+PCBCSFIRST, 0);
  next = get16r(pcbp+PCBCSNEXT, 0);
  last = get16r(pcbp+PCBCSLAST, 0);
  TRACE(T_FAULT, "CALF: first=%o, next=%o, last=%o\n", first, next, last);
  if (next == first)
    this = last;
  else
    this = next-6;
  csea = MAKEVA(crs[OWNERH]+csoffset, this);
  TRACE(T_FAULT,"CALF: cs frame is at %o/%o\n", csea>>16, csea&0xFFFF);

  /* make sure ecb specifies zero arguments */

  if (get16(ea+5) !=0) {
    printf("CALF ecb at %o/%o has arguments!\n", ea>>16, ea&0xFFFF);
    fatal(NULL);
  }

  pcl(ea);

  /* get the concealed stack entries and adjust the new stack frame */

  *(unsigned int *)(cs+0) = get32r(csea+0, 0);
  *(double *)(cs+2) = get64r(csea+2, 0);

  TRACE(T_FAULT, "CALF: cs entry: retpb=%o/%o, retkeys=%o, fcode=%o, faddr=%o/%o\n", cs[0], cs[1], cs[2], cs[3], cs[4], cs[5]);

  stackfp = *(unsigned int *)(crs+SB);
  put16(1, stackfp+0);                          /* flag it as CALF frame */
  put32(*(unsigned int *)(cs+0), stackfp+2);    /* return PB */
  put16(cs[2], stackfp+8);                      /* return keys */
  put16(cs[3], stackfp+10);                     /* fault code */
  put32(*(unsigned int *)(cs+4), stackfp+11);   /* fault address */

  /* pop the concealed stack */

  put16r(this, pcbp+PCBCSNEXT, 0);
}


/* set new processor keys */

newkeys (unsigned short new) {

  switch ((new & 016000) >> 10) {
  case 0:                     /* 16S */
    TRACE(T_MODE, "Entering 16S mode, keys=%o\n", new);
    amask = 037777;
    break;
  case 1:                     /* 32S */
    TRACE(T_MODE, "Entering 32S mode, keys=%o\n", new);
    amask = 077777;
    break;
  case 2:                     /* 64R */
    TRACE(T_MODE, "Entering 64R mode, keys=%o\n", new);
    amask = 0177777;
    break;
  case 3:                     /* 32R */
    TRACE(T_MODE, "Entering 32R mode, keys=%o\n", new);
    amask = 077777;
    break;
  case 4:                     /* 32I */
    TRACE(T_MODE, "Entering 32I mode, keys=%o\n", new);
    amask = 0177777;
    break;
  case 6:                     /* 64V */
    TRACE(T_MODE, "Entering 64V mode, keys=%o\n", new);
    amask = 0177777;
    break;
  default:                    /* invalid */
    printf("Invalid CPU mode: %o\n", new);
    fatal(NULL);
  }
  crs[KEYS] = new;
}

void fault(unsigned short fvec, unsigned short fcode, ea_t faddr) {
  ea_t pcbp, pxfvec, csea, ea;
  unsigned short first, next, last;
  unsigned short m;
  unsigned short ring;
  int i,namlen;
  unsigned short name[128];
  ea_t faultrp;

  if (fvec == PROCESSFAULT || fvec == SVCFAULT || fvec == ARITHFAULT)
    faultrp = RP;
  else
    faultrp = prevpc;

  /* save RP, keys in regfile, fcode and faddr in crs */

  regs.sym.pswpb = faultrp;
  regs.sym.pswkeys = crs[KEYS];
  crs[FCODE] = fcode;
  *(unsigned int *)(crs+FADDR) = faddr;
  
  TRACE(T_FAULT, "#%d: fault '%o, fcode=%o, faddr=%o/%o, faultrp=%o/%o\n", instcount, fvec, fcode, faddr>>16, faddr&0xFFFF, faultrp>>16, faultrp&0xFFFF);

  if (crs[MODALS] & 010) {   /* process exchange is enabled */
    ring = (RPH>>13) & 3;                     /* save current ring */
    pcbp = *(ea_t *)(crs+OWNER);
    if (fvec == PROCESSFAULT || fvec == ACCESSFAULT || fvec == STACKFAULT || fvec == SEGFAULT)
      pxfvec = get32r(pcbp+PCBFVR0,0);        /* use R0 handler */
    else if (fvec == PAGEFAULT)
      pxfvec = get32r(pcbp+PCBFVPF,0);        /* use page fault handler, also R0 */
    else {
      pxfvec = get32r(pcbp+PCBFVEC+2*ring,0); /* use current ring handler */
      pxfvec |= ((int)ring) << 29;            /* weaken */
    }

    /* push a concealed stack entry */

    first = get16r(pcbp+PCBCSFIRST, 0);
    next = get16r(pcbp+PCBCSNEXT, 0);
    last = get16r(pcbp+PCBCSLAST, 0);
    TRACE(T_FAULT, "fault: PX enabled, pcbp=%o/%o, cs first=%o, next=%o, last=%o\n", pcbp>>16, pcbp&0xFFFF, first, next, last);
    if (next > last) {
      TRACE(T_FAULT, "fault: Concealed stack wraparound to first");
      next = first;
    }
    csea = MAKEVA(crs[OWNERH]+csoffset, next);
    put32r(faultrp, csea, 0);
    put16r(crs[KEYS], csea+2, 0);
    put16r(fcode, csea+3, 0);
    put32r(faddr, csea+4, 0);
    put16r(next+6, pcbp+PCBCSNEXT, 0);
    TRACE(T_FAULT, "fault: updated cs next=%o\n", get16r(pcbp+PCBCSNEXT, 0));

    /* update RP to jump to the fault vector in the fault table */

    RP = pxfvec + (fvec-062)*4;
    newkeys(014000);      /* V-mode */
    inhcount = 1;         /* supposed to do this only for Ring 0, but shouldn't hurt */


#if 0
    if (T_FAULT && fvec == POINTERFAULT) {
      ea = get32(faddr);
      if ((ea & 0xF0000000) == 0x80000000) {
	ea &= 0x0FFFFFFF;
	namlen = get16(ea);
	for (i=0; i<(namlen+1)/2; i++)
	  name[i] = get16(ea+i+1) & 0x7f7f;
	name[i] = 0;
	TRACE(T_FAULT, "fault: DYNT addr=%o/%o, length=%d, name=%s\n", ea>>16, ea&0xffff, namlen, name);
      }
    }
#endif

    TRACE(T_FAULT, "fault: jumping to fault table entry at RP=%o/%o\n", RPH, RPL);

  } else {                   /* process exchange is disabled */
    //TRACE(T_FAULT, "fault '%o occurred at %o/%o, instruction=%o, modals=%o\n", fvec, faultrp>>16, faultrp&0xffff, get16(faultrp), crs[MODALS]);
    /* need to check for standard/vectored interrupt mode here... */
    m = get16(fvec);
    if (m != 0) {
      TRACE(T_FLOW, " fault JST* '%o [%o]\n", fvec, m);
      put16(faultrp & 0xFFFF, m);
      /* NOTE: should this set RP to m (segment 0), or just set RPL? */
#if 0
      RPL = m;
#else
      RP = m;
#endif
      RPL++;
    } else {
      printf("#%d: fault '%o, fcode=%o, faddr=%o/%o, faultrp=%o/%o\n", instcount, fvec, fcode, faddr>>16, faddr&0xFFFF, faultrp>>16, faultrp&0xFFFF);
      fatal("Fault vector is zero, process exchange is disabled.");
    }
  }

  longjmp(jmpbuf, 1);
  fatal("fault: returned after longjmp\n");
}


/* I/O device map table, containing function pointers to handle device I/O */

int devpoll[64] = {0};

#include "emdev.h"

#if 0

/* this is the "full system" controller configuration */

int (*devmap[64])(short, short, short) = {
  /* '0x */ 0,0,0,0,devasr,0,0,devpnc,
  /* '1x */ devnone,devnone,0,devnone,devmt,devamlc, devamlc, devamlc,
  /* '2x */ devcp,0,devdisk,devdisk,devdisk,devdisk,devdisk,devdisk,
  /* '3x */ 0,0,devamlc,0,0,devamlc,devnone,devnone,
  /* '4x */ 0,0,0,0,0,0,0,0,
  /* '5x */ devnone,devnone,devamlc,devamlc,devamlc,0,devnone,0,
  /* '6x */ 0,0,0,0,0,0,0,0,
  /* '7x */ 0,0,0,0,0,devnone,devnone,0};

#else

/* this is the "minimum system" controller configuration */

int (*devmap[64])(int, int, int) = {
  /* '0x */ 0,0,0,0,devasr,0,0,devpnc,
#if 1
  /* '1x */ devnone,devnone,0,devnone,devmt,devnone, devnone, devnone,
#else
  /* '1x */ devnone,devnone,0,devnone,devnone,devnone, devnone, devnone,
#endif
  /* '2x */ devcp,0,devnone,devnone,devnone,devnone,devdisk,devnone,
  /* '3x */ 0,0,devnone,0,0,devnone,devnone,devnone,
  /* '4x */ 0,0,0,0,0,0,0,devnone,
  /* '5x */ devnone,devnone,devnone,devnone,devamlc,0,devnone,0,
  /* '6x */ 0,0,0,0,0,0,0,0,
  /* '7x */ 0,0,0,0,0,devnone,devnone,0};
#endif



/* 16S Addressing Mode */

ea_t ea16s (unsigned short inst, short x) {
  
  unsigned short ea, m, rpl, amask, live, i;
  ea_t va;

  i = inst & 0100000;                            /* indirect */
  if (crs[MODALS] & 4)                           /* segmentation enabled? */
    live = 010;                                  /* yes, limit register traps */
  else
    live = 040;
  amask = 037777;
  rpl = prevpc;
  if (inst & 001000)
    ea = (rpl & 037000) | (inst & 0777);         /* current sector */
  else
    ea = (inst & 0777);                          /* sector 0 */
  while (1) {
    if (x)                                       /* indexed */
      ea += crs[X];
    if (!i)                                      /* not indirect */
      break;
    if (ea < live)
      m = get16(0x80000000|ea);
    else
      m = get16(MAKEVA(RPH,ea));
    i = m & 0100000;
    x = m & 040000;
    ea = m & 037777;                             /* go indirect */
  }
  va = MAKEVA(RPH, ea);
  if (ea < live)                                  /* flag live register ea */
    return va | 0x80000000;
  return va;
}


/* 32S Addressing Mode */

ea_t ea32s (unsigned short inst, short x) {
  
  unsigned short ea, m,rpl, amask, live, i;
  ea_t va;

  i = inst & 0100000;                            /* indirect */
  if (crs[MODALS] & 4)                           /* segmentation enabled? */
    live = 010;                                  /* yes, limit register traps */
  else
    live = 040;
  amask = 077777;
  rpl = prevpc;
  if (inst & 001000)
    ea = (rpl & 077000) | (inst & 0777);         /* current sector */
  else {
    ea = (inst & 0777);                          /* sector 0 */
    if (ea < 0100 && x) {                        /* preindex by X */
      ea += crs[X];
      x = 0;
    }
  }
  while (i) {
    if (ea < 040)
      m = get16(0x80000000|ea);
    else
      m = get16(MAKEVA(RPH,ea));
    i = m & 0100000;
    ea = m & 077777;                             /* go indirect */
  }
  if (x)                                         /* postindex */
    ea += crs[X];
  ea &= amask;
  va = MAKEVA(RPH, ea);
  if (ea < live)                                  /* flag live register ea */
    return va | 0x80000000;
  return va;
}


/* NOTE: the difference between 32R and 64R, besides the extra address
   bit, is that 32R indirect words have an indirect bit for multi-level
   indirects */

ea_t ea32r64r (ea_t earp, unsigned short inst, short x, unsigned short *opcode) {

  unsigned short live, ea, m, rph, rpl, amask, class, i;
  ea_t va;

  i = inst & 0100000;                            /* indirect */
  if (crs[MODALS] & 4)                           /* segmentation enabled? */
    live = 010;                                  /* yes, limit register traps */
  else
    live = 040;
  amask = 0177777;
  if ((crs[KEYS] & 016000) == 06000)             /* 32R mode? */
    amask = 077777;
  rpl = earp;
  rph = (earp >> 16) & 0x7FFF;     /* clear fault (live register) bit from RP */
  TRACE(T_EAR, " ea32r64r: i=%o, x=%o, amask=%o\n", i!= 0, x!=0, amask);
  if (inst & 001000)                             /* sector bit 7 set? */
    if ((inst & 0760) != 0400) {                 /* PC relative? */
      ea = rpl + (((short) (inst << 7)) >> 7);   /* yes, sign extend D */
      TRACE(T_EAR, " PC relative, P=%o, new ea=%o\n", rpl, ea);
    }
    else 
      goto special;                              /* special cases */
  else {
    ea = (inst & 0777);                          /* sector 0 */
    TRACE(T_EAR, " Sector 0, new ea=%o\n", ea);
    if (ea < 0100 && x) {                        /* preindex by X */
      TRACE(T_EAR, " Preindex, ea=%o, X='%o/%d\n", ea, crs[X], *(short *)(crs+X));
      ea += crs[X];
      TRACE(T_EAR, " Preindex, new ea=%o\n", ea);
      x = 0;
    }
  }
  while (i) {
    if (ea < live)
      m = get16(0x80000000|ea);
    else
      m = get16(MAKEVA(rph,ea));
    TRACE(T_EAR, " Indirect, old ea=%o, [ea]=%o\n", ea, m);
    if ((crs[KEYS] & 016000) == 06000)           /* 32R mode? */
      i = m & 0100000;                           /* yes, multiple indirects */
    else
      i = 0;                                     /* no, 64R mode, single indirect */
    ea = m & amask;                              /* go indirect */
    TRACE(T_EAR, " Indirect, new i=%d, new ea=%o\n", i!=0, ea);
  }
  if (x) {
    TRACE(T_EAR, " Postindex, old ea=%o, X='%o/%d\n", ea, crs[X], *(short *)(crs+X));
    ea += crs[X];
    TRACE(T_EAR, " Postindex, new ea=%o\n", ea);
  }
  ea &= amask;
  va = MAKEVA(rph, ea);
  if (ea < live)                                  /* flag live register ea */
    return va | 0x80000000;
  return va;

special:
  class = inst & 3;                              /* class bits = 15 & 16 */
  *opcode = *opcode | ((inst >> 2) & 3);         /* opcode extension */
  TRACE(T_EAR, " special, new opcode=%5#0o, class=%d\n", *opcode, class);

  if (class < 2) {                               /* class 0/1 */
    ea = get16(MAKEVA(RPH,RPL++));               /* get A from next word */
    TRACE(T_EAR, " Class %d, new ea=%o\n", class, ea);
    if (class == 1)
      ea += crs[S];
    if (x) {
      TRACE(T_EAR, " Preindex, ea=%o, X='%o/%d\n", ea, crs[X], *(short *)(crs+X));
      ea += crs[X];
      TRACE(T_EAR, " Preindex, new ea=%o\n", ea);
    }
    while (i) {
      if (ea < live)
	m = get16(0x80000000|ea);
      else
	m = get16(MAKEVA(rph,ea));
      TRACE(T_EAR, " Indirect, old ea=%o, [ea]=%o\n", ea, m);
      if ((crs[KEYS] & 016000) == 06000)
	i = m & 0100000;
      else
	i = 0;
      ea = m & amask;
      TRACE(T_EAR, " Indirect, new i=%d, new ea=%o\n", i!=0, ea);
    }

  } else if (i && x) {                           /* class 2/3, ix=11 */
    TRACE(T_EAR, " class 2/3, ix=11\n");
    ea = get16(MAKEVA(RPH,RPL++));               /* get A from next word */
    TRACE(T_EAR, " ea=%o\n", ea);
    if (class == 3)
      ea += (short) crs[S];
    while (i) {
      if (ea < live)
	m = get16(0x80000000|ea);
      else
	m = get16(MAKEVA(rph,ea));
      TRACE(T_EAR, " Indirect, ea=%o, [ea]=%o\n", ea, m);
      if ((crs[KEYS] & 016000) == 06000)
	i = m & 0100000;
      else
	i = 0;
      ea = m & amask;
      TRACE(T_EAR, " Indirect, new i=%d, new ea=%o\n", i!=0, ea);
    }
    TRACE(T_EAR, " Postindex, old ea=%o, X='%o/%d\n", ea, crs[X], *(short *)(crs+X));
    ea += (short) crs[X];
    TRACE(T_EAR, " Postindex, new ea=%o\n", ea);

  } else {                                       /* class 2/3, ix != 11 */
    if (class == 2)
      ea = crs[S]++;
    else
      ea = --crs[S];
    TRACE(T_EAR, " Class 2/3, new ea=%o, new S=%o\n", ea, crs[S]);
    if (x) {
      if (ea < live)
	m = get16(0x80000000|ea);
      else
	m = get16(MAKEVA(rph,ea));
      if ((crs[KEYS] & 016000) == 06000)
	i = m & 0100000;
      ea = m & amask;
    }
    while (i) {
      if (ea < live)
	m = get16(0x80000000|ea);
      else
	m = get16(MAKEVA(rph,ea));
      if ((crs[KEYS] & 016000) == 06000)
	i = m & 0100000;
      else
	i = 0;
      ea = m & amask;
    }
    if (x)
      ea += crs[X];
  }
  ea &= amask;
  va = MAKEVA(rph, ea);
  if (ea < live)                                  /* flag live register ea */
    return va | 0x80000000;
  return va;
}

#include "ea64v.h"

unsigned int ea32i (ea_t earp, unsigned short inst, short x) {
  TRACE(T_EAI, "Mode 32I not implemented\n");
}


ea_t apea(unsigned short *bitarg) {
  unsigned short ibr, ea_s, ea_w, bit, br, a;
  ea_t ea, ip;
  
  ibr = iget16(RP);
  RPL++;
  a = iget16(RP);
  RPL++;
  bit = (ibr >> 12) & 0xF;
  br = (ibr >> 8) & 3;
  TRACE(T_EAAP, " AP ibr=%o, br=%d, i=%d, bit=%d, a=%o\n", ibr, br, (ibr & 004000) != 0, bit, a);

  /* XXX: should ea ring be weakened with RP ring? */

  ea_s = crs[PBH + 2*br];
  ea_w = crs[PBL + 2*br] + a;
  ea = MAKEVA(ea_s, ea_w);
  TRACE(T_EAAP, " AP ea = %o/%o  %s\n", ea_s, ea_w, searchloadmap(ea,' '));
  if (ibr & 004000) {
    if (ea & 0x80000000)
      fault(POINTERFAULT, ea>>16, 0);
    ip = get32(ea);
    if (ip & EXTMASK32)
      bit = get16(INCVA(ea,2)) >> 12;
    else
      bit = 0;
    ea = ip;
    TRACE(T_EAAP, " After indirect, AP ea = %o/%o, bit=%d  %s\n", ea>>16, ea & 0xFFFF, bit, searchloadmap(ea,' '));
  }
  if (bit)
    ea |= EXTMASK32;
  if (bitarg != NULL)
    *bitarg = bit;
  return ea;
}

/* exception handler types:

  'i' = integer exception
  'd' = decimal exception
  'f' = floating point exception

  Depending on the keys settings, take the appropriate fault.
  Always sets the C-bit.
*/

#define FC_SFP_OFLOW 0400
#define FC_SFP_ZDIV  0401
#define FC_SFP_STORE 0402
#define FC_SFP_INT   0403
#define FC_DFP_OFLOW 01000
#define FC_DFP_ZDIV  01001
#define FC_INT_OFLOW 01400
#define FC_INT_ZDIV  01401
#define FC_DEC_OFLOW 03400
#define FC_DEC_ZDIV  03401
#define FC_DEC_CONV  03402

mathexception(unsigned char extype, unsigned short fcode, ea_t faddr)
{
  crs[KEYS] |= 0x8000;
  switch (extype) {
  case 'i':
    if (crs[KEYS] & 0400) 
      fault(ARITHFAULT, fcode, faddr);
    break;
  case 'd':
    if (crs[KEYS] & 40)
      fault(ARITHFAULT, fcode, faddr);
    break;
  case 'f':
    if (!(crs[KEYS] & 01000))
      fault(ARITHFAULT, fcode, faddr);
    break;
  default:
    printf(" Unrecognized exception type '%c'\n", extype);
    fatal(NULL);
  }
}



memdump(int start, int end) {
  int ea;

  if (domemdump) {

    /* dump sector zero for debugging */

    TRACEA("\nSector 0:\n");
    for (ea=0; ea<01000; ea=ea+8)
      if (mem[ea]|mem[ea+1]|mem[ea+2]|mem[ea+3]|mem[ea+4]|mem[ea+5]|mem[ea+6]|mem[ea+7])
	TRACEA("%3o: %6o %6o %6o %6o %6o %6o %6o %6o\n", ea, mem[ea], mem[ea+1], mem[ea+2], mem[ea+3], mem[ea+4], mem[ea+5], mem[ea+6], mem[ea+7]);

    /* dump main memory for debugging */

    TRACEA("\nMain memory:\n");
    for (ea=start; ea<=end; ea=ea+8)
      if (mem[ea]|mem[ea+1]|mem[ea+2]|mem[ea+3]|mem[ea+4]|mem[ea+5]|mem[ea+6]|mem[ea+7])
	TRACEA("%o: %6o %6o %6o %6o %6o %6o %6o %6o\n", ea, mem[ea], mem[ea+1], mem[ea+2], mem[ea+3], mem[ea+4], mem[ea+5], mem[ea+6], mem[ea+7]);
  }
}


dumpsegs() {
  short seg,nsegs,i,page,segno;
  unsigned short pte,xxx;
  unsigned int dtar,staddr,sdw,ptaddr,pmaddr;

  for (i=0; i<4; i++) {
    dtar = *(unsigned int *)(crs+DTAR0-2*i);  /* get dtar register */
    nsegs = 1024-(dtar>>22);
    staddr = (dtar & 0x003F0000) | ((dtar & 0xFFFF)<<1);
    TRACEA("DTAR %d: register=%o, size=%d, seg table addr=%o\n", i, dtar, nsegs, staddr);
    for (seg=0; seg<nsegs; seg++) {
      segno = (i<<10)+seg;
      sdw = *(unsigned int *)(mem+staddr);
      ptaddr = ((sdw & 0x3F)<<10) | (sdw>>22);
      TRACEA("Segment '%o: F=%d, R1:%o R3:%o PT = %o\n", segno, (sdw>>15)&1, (sdw>>12)&7, (sdw>>6)&7, ptaddr);
      xxx = (sdw>>16)&0x3F;
      if (xxx != 0) TRACEA("WARNING: X=%o\n", xxx);
      if (ptaddr != 0)
	for (page=0; page<64; page++) {
	  pmaddr = (ptaddr<<6) + page;
	  pte = mem[pmaddr];
	  TRACEA(" Seg %o page %d: pmaddr=%o, V=%d R=%d U=%d S=%d PPA=%o\n", segno, page, pmaddr, pte>>15, (pte>>14)&1, (pte>>13)&1, (pte>>12)&1, pte&0xFFF);
	}
      staddr += 2;
    }
  }
}


/* NOTE: this needs get16r */

unsigned short dumppcb(unsigned short pcb) {
  short i;
  unsigned short nextpcb;
  ea_t ea;

  ea = MAKEVA(crs[OWNERH],pcb);
  TRACEA("PCB %06o:\n", pcb);
  TRACEA("  Level: %o\n", get16(ea+0));
  nextpcb = get16(ea+1);
  TRACEA("  Link: %o\n", nextpcb);
  TRACEA("  Wait list: %o/%o\n", get16(ea+2), get16(ea+3));
  TRACEA("  Abort flags: %o\n", get16(ea+4));
  TRACEA("  CPU flags: %o\n", get16(ea+5));
  TRACEA("  6,7 (reserved): %o %o\n", get16(ea+6), get16(ea+7));
  TRACEA("  Elapsed timers: %d %d\n", get16(ea+8), get16(ea+9));
  TRACEA("  DTAR 2 & 3: %o|%o  %o|%o\n", get16(ea+10), get16(ea+11), get16(ea+12), get16(ea+13));
  TRACEA("  Process interval timer: %o\n", get16(ea+14));
  TRACEA("  15 (reserved): %o\n", get16(ea+15));
  TRACEA("  Save mask: %o\n", get16(ea+16));
  TRACEA("  Keys: %o\n", get16(ea+17));
  for (i=0; i<16; i++) {
    TRACEA("  %06o %06o", get16(ea+18+2*i), get16(ea+19+2*i));
    if (i==7 || i==15)
      TRACEA("\n");
  }
  TRACEA("  R0 Fault vec: %o/%o\n", get16(ea+50), get16(ea+51));
  TRACEA("  R1 Fault vec: %o/%o\n", get16(ea+52), get16(ea+53));
  TRACEA("  R2 Fault vec: %o/%o\n", get16(ea+54), get16(ea+55));
  TRACEA("  R3 Fault vec: %o/%o\n", get16(ea+56), get16(ea+57));
  TRACEA("  PG Fault vec: %o/%o\n", get16(ea+58), get16(ea+59));
  TRACEA("  Conc. Stack Hdr: %o %o %o\n", get16(ea+60), get16(ea+61), get16(ea+62));
  TRACEA("\n");
  return nextpcb;
}

/* stack extension, called with size of extension in 16-bit words,
   returns a pointer to the extension */

ea_t stex(unsigned int extsize) {
  short stackrootseg;
  ea_t stackrootp, stackfp;

  if (extsize & 1) extsize++;
  stackrootseg = get16((*(unsigned int *)(crs+SB))+1);
  stackrootp = MAKEVA(stackrootseg,0);
  stackfp = get32(stackrootp);
  if (stackfp == 0)
    fatal("stex: stack free pointer is zero");

  /* find a stack segment where this extension will fit */

  while (stackfp != 0 && (stackfp & 0xFFFF) + extsize > 0xFFFF) {
    stackfp = get32(MAKEVA(stackfp>>16, 2));
    TRACE(T_INST, " no room for frame, extension pointer is %o/%o\n", stackfp>>16, stackfp&0xFFFF);
  }
  if (stackfp == 0)
    fault(STACKFAULT, 0, MAKEVA(stackrootseg,0) | (RP & RINGMASK32));

  /* update the stack free pointer */

  put32((stackfp+extsize) & ~RINGMASK32, stackrootp);
  TRACE(T_INST, " stack extension is at %o/%o\n", stackfp>>16, stackfp&0xffff);
  return stackfp;
}


void prtn() {
  unsigned short stackrootseg;
  ea_t newrp,newsb,newlb;
  unsigned short keys;

  stackrootseg = get16(*(unsigned int *)(crs+SB)+1);
  put32(*(unsigned int *)(crs+SB), MAKEVA(stackrootseg,0));
  newrp = get32(*(unsigned int *)(crs+SB)+2);
  newsb = get32(*(unsigned int *)(crs+SB)+4);
  newlb = get32(*(unsigned int *)(crs+SB)+6);
  keys = get16(*(unsigned int *)(crs+SB)+8);
  RP = newrp | (RP & RINGMASK32);
  *(unsigned int *)(crs+SB) = newsb;
  *(unsigned int *)(crs+LB) = newlb;
  newkeys(keys & 0177770);
  TRACE(T_INST, " Finished PRTN, RP=%o/%o\n", RPH, RPL);
}



/* NOTE: the brsave array contains copies of the PB, SB, and LB base
   registers at the time of the PCL, to compute argument effective
   addresses.  If the PCL faults during argument transfer, the ARGT
   instruction will reload this array from the new stack frame
   header. */

ea_t pclea(unsigned short brsave[6], ea_t rp, unsigned short *bitarg, short *store, short *lastarg) {
  unsigned short ibr, br, ea_s, ea_w, bit, a;
  unsigned int utempl;
  ea_t ea, iwea;

  iwea = 0;
  *store = 0;
  utempl = get32(rp);
  ibr = utempl >> 16;
  a = utempl & 0xFFFF;
  bit = (ibr >> 12) & 0xF;
  *store = ibr & 0100;
  *lastarg = ibr & 0200;
  br = (ibr >> 8) & 3;
  TRACE(T_PCL, " PCLAP @ %o/%o, ibr=%o, br=%d, i=%d, bit=%d, store=%d, lastarg=%d, a=%o\n", rp>>16, rp&0xffff, ibr, br, (ibr & 004000) != 0, bit, (*store != 0), (*lastarg != 0), a);
  if (br != 3) {
    ea_s = brsave[2*br] | (RPH & RINGMASK16);
    ea_w = brsave[2*br + 1];
    ea_w += a;
  } else {
    ea_s = crs[XBH] | (RPH & RINGMASK16);
    ea_w = crs[XBL];
    ea_w += a;
    if (crs[XB] & EXTMASK16) {
      bit += crs[X];
      if (bit > 15) {
	bit -= 16;
	ea_w++;
      }
      if (bit == 0)
	ea_s &= ~EXTMASK16;
    }
  }
  ea = MAKEVA(ea_s, ea_w);
  if (bit)
    ea |= EXTMASK32;
  TRACE(T_PCL, " PCLAP ea = %o/%o, bit=%d\n", ea_s, ea_w, bit);
  if (ibr & 004000) {             /* indirect */
    if (ea & 0x80000000)
      fault(POINTERFAULT, ea>>16, 0);
    iwea = ea;
    ea = get32(iwea) | (RP & RINGMASK32);
    TRACE(T_PCL, " Indirect pointer is %o/%o\n", ea>>16, ea & 0xFFFF);

    /* Case 35 wants a fault when the IP is 120000/0:

	 #28307386 24000/27740: PCL 24000/30045
	  ecb @ 24000/30045, access=7
	  ecb.pb: 4000/30041
	  ecb.framesize: 16
	  ecb.stackroot 4000
	  ecb.argdisp: 12
	  ecb.nargs: 1
	  ecb.lb: 4000/60000
	  ecb.keys: 14000
	  stack free pointer: 4000/70000, current ring=20000
	  before update, stackfp=24000/70000, SB=0/0
	  new SB=24000/70000
	  new RP=24000/30041
	 Entered ARGT
	  Transferring arg, 1 left, Y=12
	  PCLAP ibr=4300, br=0, i=1, bit=0, store=1, lastarg=1, a=27117
	  PCLAP ea = 24000/27117, bit=0
	  Indirect pointer is 120000/0
	  After indirect, PCLAP ea = 120000/0, bit=0
	  Storing arg, 1 left, Y=12
	  Stored
	 #29019968: fault '62, fcode=0, faddr=0/0, faultrp=24000/1356

	 [ Failure Report ]

		address           instruction         scope loop 
	     024000/027740       021410/030045       024000/027547 

	     Actual: NO POINTER FAULT
	     Expected: POINTER FAULT 

       But, Case 37 doesn't want a fault:

	 #28891410 24000/30760: PCL 24000/31065
	  ecb @ 24000/31065, access=7
	  ecb.pb: 4000/31054
	  ecb.framesize: 16
	  ecb.stackroot 4000
	  ecb.argdisp: 12
	  ecb.nargs: 1
	  ecb.lb: 4000/60000
	  ecb.keys: 14000
	  stack free pointer: 4000/70000, current ring=20000
	  before update, stackfp=24000/70000, SB=0/0
	  new SB=24000/70000
	  new RP=24000/31054
	 Entered ARGT
	  Transferring arg, 1 left, Y=12
	  PCLAP ibr=4300, br=0, i=1, bit=0, store=1, lastarg=1, a=27117
	  PCLAP ea = 24000/27117, bit=0
	  Indirect pointer is 120000/0
	 #28891410: fault '77, fcode=120000, faddr=24000/27117, faultrp=24000/30760

	 0003  Unexpected POINTER fault.  Returning to 030760

       Changing <= to < in the ring comparison makes Case 37 work and Case 35 fail.
    */

#if 0
    /* NOTE: this code causes every command to give a pointer fault:


                        #281037430 [SUPPCB 100100] SB: 6003/754 LB: 6/100112 PGMAPA XB: 6/100272
6/100367: 11415         A='201/129 B='11/9 X=2/2 Y=20/16 C=0 L=0 LT=0 EQ=0 K=14000 M=100177
 opcode=00400, i=0, x=0
 2-word format, a=22
 new opcode=00403, y=0, br=1, ixy=0, xok=1
 2-word format, a=1142
 new opcode=01002, y=1, br=2, ixy=3, xok=1
 Long indirect, ea=60013/24350, ea_s=60013, ea_w=24350
 After indirect, ea_s=60013, ea_w=60422, bit=0
 EA: 60013/60422  MISSIN
 PCL MISSIN
 ecb @ 60013/60422, access=6
 ecb.pb: 13/60404
 ecb.framesize: 14
 ecb.stackroot 0
 ecb.argdisp: 12
 ecb.nargs: 1
 ecb.lb: 13/60022
 ecb.keys: 14000
 stack root in ecb was zero, stack root from caller is 6002
 stack free pointer: 6002/3770, current ring=3, new ring=3
 before update, stackfp=66002/3770, SB=66002/1434
 new SB=66002/3770
Entering 64V mode, keys=14000
 new RP=60013/60404
Entered ARGT
 Transferring arg, 1 left, Y=12
 PCLAP ibr=4700, br=1, i=1, bit=0, store=1, lastarg=1, a=143
 PCLAP ea = 66002/1577, bit=0
 Indirect pointer is 160000/0
fault '77, fcode=160000, faddr=66002/1577, faultrp=60013/60404
fault: PX enabled, pcbp=65/100100, cs first=1026, next=1026, last=1064
fault: updated cs next=1034
Entering 64V mode, keys=14000
fault: jumping to fault table entry at RP=60013/61212

    */

    if (ea & 0x80000000)
      if ((ea & 0xFFFF0000) != 0x80000000)
	if ((ea & 0x1FFF0000) || ((RP & RINGMASK32) <= (ea & RINGMASK32)))
	  fault(POINTERFAULT, ea>>16, iwea);
#else
    if ((ea & 0x80000000) && (ea & 0x1FFF0000))
      fault(POINTERFAULT, ea>>16, iwea);
#endif
    bit = 0;
#if 1
    /* CPU.PCL Case 33 shows that the bit field is not stored in the stack frame
       for a 3-word indirect pointer, even though the E bit remains set.

       NOTE: this comment is screwy, and probably reflects a microcode bug on certain
       machines.  If this code is disabled, the SAC command doesn't work:
               SAC X JIMMY:PDALURW  sets permission as  JIMMY:PAUW

       NOTE 2: turns out that this code was fetching the bit offset from ea+2 instead
       of from iwea+2, which could account for lots of weirdness! */

    if (ea & EXTMASK32)
      bit = get16(iwea+2) >> 12;
#endif
    TRACE(T_PCL, " After indirect, PCLAP ea = %o/%o, bit=%d\n", ea>>16, ea & 0xFFFF, bit);
  }

  if (!*store) {
#if 0
    /* Case 36 wants a pointer fault here... See Case 31, 34, 37 also */
    if (ea & 0x80000000)
      if ((ea & 0xFFFF0000) != 0x80000000)
	if ((ea & 0x1FFF0000) || ((RP & RINGMASK32) <= (ea & RINGMASK32)))
	  fault(POINTERFAULT, ea>>16, iwea);
#else
    if (ea & 0x80000000)
      fault(POINTERFAULT, ea>>16, iwea);
#endif
    *(unsigned int *)(crs+XB) = ea;
    crs[X] = bit;
  }
  if (bit) {
    ea |= EXTMASK32;
    *bitarg = bit;
  } else {
    *bitarg = 0;
  }
  return ea;
}


pcl (ea_t ecbea) {
  short i,j;
  unsigned short access;
  unsigned short ecb[9];
  short bit;                  /* bit offset for args */
  ea_t newrp;                 /* start of new proc */
  ea_t ea;
  ea_t rp;                    /* return pointer */
  short stackrootseg;
  short stacksize;
  short store;                /* true if store bit set on AP */
  short storedargs;           /* # of arguments that have been stored */
  short lastarg;              /* true if "last" bit seen in PCL arglist */
  ea_t argp;                  /* where to store next arg in new frame */
  ea_t stackfp;               /* new stack frame pointer */
  pa_t pa;                    /* physical address of ecb */
  unsigned short brsave[6];   /* old PB,SB,LB */
  unsigned short utempa;
  unsigned char tnstring[500];
  unsigned short tnlen, tnword;
  unsigned char tnchar;

#define FATAL$ MAKEVA(06,0164600)
#define INIT$3 MAKEVA(013,0174041)
#define UNWIND_ MAKEVA(013,0106577)

#if 0
  if (ecbea == UNWIND_) {
    printf("pcl: calling unwind_ at %d\n", instcount);
    savetraceflags = ~TB_MAP;
  }
#endif

  /* get segment access; mapva ensures either read or gate */

  pa = mapva(ecbea, PACC, &access, RP);
  TRACE(T_PCL, " ecb @ %o/%o, access=%d\n", ecbea>>16, ecbea&0xFFFF, access);

  /* get a copy of the ecb.  gates must be aligned on a 16-word
     boundary, therefore can't cross a page boundary, and mapva has
     already ensured that the ecb page is resident.  For a non-gate
     ecb, check to see if it crosses a page boundary.  If not, a 
     memcpy is okay; if it does, do fetches */

  if (access == 1) {
    if ((ecbea & 0xF) != 0)
      fault(ACCESSFAULT, 0, ecbea);
    memcpy(ecb,mem+pa,sizeof(ecb));
  } else if ((pa & 01777) <= 01750) {
    memcpy(ecb,mem+pa,sizeof(ecb));
  } else {
    *(double *)(ecb+0) = get64(ecbea+0);
    *(double *)(ecb+4) = get64(ecbea+4);
    ecb[8] = get16(ecbea+8);
  }

  /* XXX: P400 docs say "no ring change takes place if not a
     gate"; does that mean that if R0 calls a R3 ecb, it's
     still in R0, or should it be weakened to the ecb ring?
     (Case 24 of CPU.PCL indicates it should be weakened) */

  TRACE(T_PCL, " ecb.pb: %o/%o\n ecb.framesize: %d\n ecb.stackroot %o\n ecb.argdisp: %o\n ecb.nargs: %d\n ecb.lb: %o/%o\n ecb.keys: %o\n", ecb[0], ecb[1], ecb[2], ecb[3], ecb[4], ecb[5], ecb[6], ecb[7], ecb[8]);

  newrp = *(unsigned int *)(ecb+0);
  if (access != 1)
#if 0
    newrp = (newrp & ~RINGMASK32) | (RP & RINGMASK32);  /* no ring change */
#else
    newrp = newrp | (RP & RINGMASK32);    /* Case 24 indicates to weaken ring */
#endif

  /* setup stack frame
     NOTE: newrp must be used here so that accesses succeed when calling 
     an inner ring procedure. */

  stackrootseg = ecb[3];
  if (stackrootseg == 0) {
    stackrootseg = get16((*(unsigned int *)(crs+SB)) + 1);
    TRACE(T_PCL, " stack root in ecb was zero, stack root from caller is %o\n", stackrootseg);
  }
  if (stackrootseg == 0)
    fatal("Stack base register root segment is zero");
  stackfp = get32r(MAKEVA(stackrootseg,0), newrp);
  if (stackfp == 0)
    fatal("Stack free pointer is zero");
  TRACE(T_PCL, " stack free pointer: %o/%o, current ring=%o, new ring=%o\n", stackfp>>16, stackfp&0xFFFF, (RPH&RINGMASK16)>>13, (newrp&RINGMASK32)>>29);
  stacksize = ecb[2];

  /* if there isn't room for this frame, check the stack extension
     pointer */

  if ((stackfp & 0xFFFF) + stacksize > 0xFFFF) {
    stackfp = get32r(MAKEVA(stackrootseg,2), newrp);
    TRACE(T_PCL, " no room for frame, extension pointer is %o/%o\n", stackfp>>16, stackfp&0xFFFF);

    /* XXX: faddr may need to be the last segment tried when this is changed to loop.
       CPU.PCL Case 26 wants fault address word number to be 3; set EHDB */

    if (stackfp == 0 || (stackfp & 0xFFFF) + stacksize > 0xFFFF)
      fault(STACKFAULT, 0, MAKEVA(stackrootseg,0) | (newrp & RINGMASK32));
  }

  /* setup the new stack frame at stackfp

     NOTE: Ring must be added to stackfp so that any page faults that
     occur while setting up the stack will have the correct ring for
     CPU.PCL tests */

  stackfp |= (newrp & RINGMASK32);
  put16r(0, stackfp, newrp);
  put16r(stackrootseg, stackfp+1, newrp);
  put32r(RP, stackfp+2, newrp);
  put32r(*(unsigned int *)(crs+SB), stackfp+4, newrp);
  put32r(*(unsigned int *)(crs+LB), stackfp+6, newrp);
  put16r(crs[KEYS], stackfp+8, newrp);
  put16r(RPL, stackfp+9, newrp);

#if 0
  /* LATER: save caller's base registers for address calculations, and
     pass to argt */

  if (ecb[5] > 0) {
    brsave[0] = RPH;       brsave[1] = 0;
    brsave[2] = crs[SBH];  brsave[3] = crs[SBL];
    brsave[4] = crs[LBH];  brsave[5] = crs[LBL];
  }
#endif

  /* load new execution state from ecb */

  TRACE(T_PCL, " before update, stackfp=%o/%o, SB=%o/%o\n", stackfp>>16, stackfp&0xFFFF, crs[SBH], crs[SBL]);
  if (access == 1)
    *(unsigned int *)(crs+SB) = stackfp;
  else
    *(unsigned int *)(crs+SB) = (stackfp & ~RINGMASK32) | (RP & RINGMASK32);
  TRACE(T_PCL, " new SB=%o/%o\n", crs[SBH], crs[SBL]);
  *(unsigned int *)(crs+LB) = *(unsigned int *)(ecb+6);
  newkeys(ecb[8] & 0177770);

  /* update the stack free pointer; this has to wait until after all
     memory accesses, in case of stack page faults (PCL restarts).
     Some ucode versions incorrectly store the ring in the free
     pointer if the extension pointer was followed.  Set EHDB to
     suppress this spurious DIAG error. */

  ea = MAKEVA(stackrootseg,0) | (newrp & RINGMASK32);
  put32r((stackfp+stacksize) & ~RINGMASK32, ea, newrp);

  /* transfer arguments if arguments are expected.  There is no
     documentation explaining how the Y register is used during
     argument transfer, so:
     Y(high) = stack frame offset to store next argument
     Y(low) = number of arguments left to transfer (JW hack!) */


  /* if a page fault occurs during argument transfer, we need to 
     make sure to use the current RP, which points to the ARGT
     instruction.  Otherwise, the return from the page fault
     is to the PCL instruction, which has already completed at
     this point */

  RP = newrp;
  prevpc = RP;
  TRACE(T_PCL, " new RP=%o/%o\n", RPH, RPL);

  if (ecb[5] > 0) {
    crs[Y] = ecb[4];
    crs[YL] = ecb[5];
    crs[XL] = 0;
    argt();

    /* if tracing terminal output, display it now.  This has to occur
     after ARGT has setup the argument pointers.  Note that if a fault
     occurs while accessing the arguments here, it will return to ARGT
     in the main emulator loop and nothing will be logged. */

    if (TRACEUSER && ((ecbea & 0xFFFFFFF) == tnou_ea || (ecbea & 0xFFFFFFF) == tnoua_ea)) {
      ea = *(unsigned int *)(crs+SB) + ecb[4];
      utempa = get16(get32(ea));       /* 1st arg: userid */
      if (utempa == ((crs[OWNERL]>>6) & 0xff)) {
	ea = ea + 6;                   /* 3rd arg: length */
	tnlen = get16(get32(ea));
	ea = get32(ea-3);              /* 2nd arg: string */
	j = 0;
	for (i=0; i<tnlen; i++) {
	  if (i & 1)
	    tnchar = tnword & 0x7f;
	  else {
	    tnword = get16(ea+i/2);
	    tnchar = (tnword >> 8) & 0x7f;
	  }
	  if (j > sizeof(tnstring)-5)
	    j = sizeof(tnstring)-5;
	  if (tnchar >= ' ' && tnchar < 0177)
	    tnstring[j++] = tnchar;
	    else {
	      sprintf((char *)(tnstring+j), "%03o ", tnchar);
	      j = j+4;
	    }
	}
	tnstring[j] = 0;
	TRACE(T_TERM, " TNOUx user %d, len %d: %s\n", utempa, tnlen, tnstring);
      }
    }

    RPL++;    /* advance real RP past ARGT after argument transfer */
  }
}

/* for ARGT:
   Registers:
   - RP points to the ARGT instruction
   - SB points to the new stack frame
   - LB is for the called procedure
   - Y is new frame offset of the next argument
   - YL is the number of arguments left to transfer (HACK!)
   - X is used to store the EA bit offset (for unstored AP)
   - XL is used to store the "lastarg seen" flag
   - XB is used to store the EA seg/word (for unstored AP)
   Stack frame:
   - PB points to the next argument template to be evaluated
   - SB is the caller's saved SB
   - LB is the caller's saved LB
*/

argt() {
  unsigned short brsave[6];
  unsigned short argsleft, argdisp, bit;
  short lastarg, store;
  unsigned int utempl;
  unsigned short ecby;          /* last offset where ecb temp ea was stored */
  ea_t ea, stackfp, rp, ecbea;
  unsigned short advancepb, advancey;

  TRACE(T_PCL, "Entered ARGT\n");

  /* stackfp is the new stack frame, rp is in the middle of
     argument templates and is advanced after each transfer */

  stackfp = *(unsigned int *)(crs+SB);
  rp = get32(stackfp+2);

  /* reload the caller's base registers for EA calculations */
  
  brsave[0] = rp >> 16;    brsave[1] = 0;
  *(double *)(brsave+2) = get64(stackfp+4);
  
  argdisp = crs[Y];
  argsleft = crs[YL];
  while (argsleft > 0 || !crs[XL]) {

    TRACE(T_PCL, " Transferring arg, %d left, Y=%o\n", argsleft, crs[Y]);

    advancey = 0;
    if (crs[XL]) {
      ea = 0x80000000;
      store = 1;
      advancepb = 0;
    } else {
      ea = pclea(brsave, rp, &bit, &store, &lastarg) | (RP & RINGMASK32);
      advancepb = 1;
    }
    if (argsleft > 0 && store) {
      TRACE(T_PCL, " Storing arg, %d left, Y=%o\n", argsleft, crs[Y]);

      /* NOTE: some version of ucode only store 16 bits for omitted args.
	 Set EHDB to prevent this error.

	 Case 29 wants ring/E-bits preserved for omitted arguments */

#if 0
  #define OMITTEDARG_MASK 0x8FFFFFFF
#else
  #define OMITTEDARG_MASK 0xEFFFFFFF
#endif

      if ((ea & 0x8FFF0000) == 0x80000000) {
	ea = ea & OMITTEDARG_MASK;      /* strip ring &/or E bits */
	ea = MAKEVA(0100000,0);
	put32(ea, stackfp+crs[Y]);
      } else {
	put32(ea, stackfp+crs[Y]);
	if (ea & EXTMASK32)
	  put16(bit<<12, stackfp+crs[Y]+2);
      }
      TRACE(T_PCL, " Stored arg IP at %o/%o\n\n", stackfp>>16, (stackfp+crs[Y]) & 0xFFFF);
      argsleft--;
      advancey = 1;
    }

    /* advance rp/pb in new stack frame past this template, and
       advance Y to the next arg displacement in the stack.  Y
       has to be advanced last because the PB store may fault.
       If it does, the ARGT starts over, and this argument will
       have to be transferred again. */

    if (advancepb) {
      rp += 2;
      put32(rp, stackfp+2);
      crs[XL] = lastarg;
    }
    if (advancey) {
      crs[Y] += 3;
      crs[YL]--;
    }
  }

  TRACE(T_PCL, " Return RP=%o/%o\n", rp>>16, rp&0xffff);
}


/* process exchange register save:  saves the current register
   set to the process pcb. 
   NOTES:
   - adding "wait" arg and only saving base registers fixed Case 63
*/

pxregsave(unsigned short wait) {
  ea_t pcbp, regp;
  unsigned short i, mask;

  /* if registers aren't owned or are already saved, return */

  if (crs[OWNERL] == 0 || (crs[KEYS] & 1))
    return;

  pcbp = *(unsigned int *)(crs+OWNER);
  regp = pcbp+PCBREGS;
  mask = 0;
  for (i=(wait?014:0); i<020; i++) {
    if (crsl[i] != 0) {
      mask |= bitmask16[i+1];
      put32r(crsl[i], regp, 0);
      regp += 2;
    }
  }
  put16r(mask, pcbp+PCBMASK, 0);
  put32r(*(unsigned int *)(crs+TIMER), pcbp+PCBIT, 0);  /* save interval timer */
  crs[KEYS] |= 1;                     /* set save done bit */
  put16r(crs[KEYS], pcbp+PCBKEYS, 0);
}

/* pxregload: load pcbp's registers from their pcb to the current
   register set, set OWNERL

   NOTE: RP must be set by the caller since this happens whenever
   a process is dispatched - not just when registers are loaded */

pxregload (ea_t pcbp) {
  ea_t regp;
  unsigned short i, mask, modals;

  TRACE(T_PX, "pxregload loading registers for process %o/%o\n", pcbp>>16, pcbp&0xFFFF);
  regp = pcbp+PCBREGS;
  mask = get16r(pcbp+PCBMASK, 0);
  for (i=0; i<020; i++) {
    if (mask & bitmask16[i+1]) {
      crsl[i] = get32r(regp, 0);
      regp += 2;
    } else {
      crsl[i] = 0;
    }
  }
  newkeys(get16r(pcbp+PCBKEYS, 0));
  *(unsigned int *)(crs+DTAR2) = get32r(pcbp+PCBDTAR2, 0);
  *(unsigned int *)(crs+DTAR3) = get32r(pcbp+PCBDTAR3, 0);
  *(unsigned int *)(crs+TIMER) = get32r(pcbp+PCBIT, 0);
  crs[OWNERL] = pcbp & 0xFFFF;

  TRACE(T_PX, "pxregload: registers loaded, ownerl=%o, modals=%o\n", crs[OWNERL], crs[MODALS]);
}


/* switch to the other register set (2 vs 3) */

ors() {
  unsigned short rsnum;
  unsigned short modals;

  /* only bit 11 of crs in modals is important for register set */

  TRACE(T_PX, "ors: current modals = %o, register set = %d\n", crs[MODALS], (crs[MODALS] & 0340)>>5);
  modals = (crs[MODALS] ^ 040) | 0100;
  rsnum = 2+((modals & 040) >> 5);
  TRACE(T_PX, "ors: new modals = %o, register set = %d\n", modals, rsnum);
  crs = regs.rs16[rsnum];
  crsl = (void *)crs;
  crs[MODALS] = modals;
  TRACE(T_PX, "ors: new register set = %d, modals = %o\n", (crs[MODALS] & 0340)>>5, crs[MODALS]);
}


/* the process exchange dispatcher's job is to:
   - determine the highest priority process ready to run
   - find a register set to use
   - save the registers if they are currently owned and not already saved
   - load this process' registers into the register set
   - clear the save done bit in keys
   - cause a process fault if any of this process' pcb abort flags are set

   If no process can be found to run, the dispatcher idles and waits
   for an external interrupt.
 */

dispatcher() {
  ea_t pcbp, rlp;
  unsigned short pcbw;      /* pcb word address */
  unsigned short rsnum;
  unsigned short rlbol;
  unsigned short utempa;


  crs[MODALS] |= 0100000;               /* ISG says dispatcher enables int. */

  if (regs.sym.pcba != 0) {
    pcbp = MAKEVA(crs[OWNERH], regs.sym.pcba);
    TRACE(T_PX, "disp: dispatching PPA, pcba=%o, pla=%o\n", regs.sym.pcba, regs.sym.pla);

  } else if (regs.sym.pcbb != 0) {
    pcbp = MAKEVA(crs[OWNERH], regs.sym.pcbb);
    regs.sym.pcba = regs.sym.pcbb;
    regs.sym.pla = regs.sym.plb;
    regs.sym.pcbb = 0;
    TRACE(T_PX, "disp: dispatching PPB, pcba=%o, pla=%o\n", regs.sym.pcba, regs.sym.pla);

  } else {
    TRACE(T_PX, "disp: scanning RL\n");
    if (regs.sym.pla != 0)
      rlp = MAKEVA(crs[OWNERH], regs.sym.pla);
    else if (regs.sym.plb != 0)
#if 0
      rlp = MAKEVA(crs[OWNERH], regs.sym.plb);
#else
      fatal("disp: pla is invalid, plb is valid?");
#endif
    else
      fatal("dispatch: both pla and plb are zero; can't locate ready list");
    while(1) {
      rlbol = get16r(rlp, 0);
      if (rlbol != 0)
	break;
      rlp += 2;
    }
    if (rlbol == 1)
      goto idle;
    pcbp = MAKEVA(crs[OWNERH], rlbol);
    regs.sym.pcba = rlbol;
    regs.sym.pla = rlp & 0xFFFF;
  }
  pcbw = pcbp & 0xFFFF;
  TRACE(T_PX, "disp: process %o/%o selected\n", pcbp>>16, pcbw);

#if 1
  /* debug tests to verify ready list structure */
  rlp = MAKEVA(crs[OWNERH], regs.sym.pla);
  rlbol = get16r(rlp, 0);
  if (rlbol != pcbw) {
    printf("disp: rl bol=%o, != process dispatched=%o\n", rlbol, pcbw);
    fatal(NULL);
  }
#if 0
  /* NOTE: if a running process has its priority changed (in the pcb), this
     test fails */
  if (get16r(pcbp+PCBLEV, 0) != regs.sym.pla) {
    printf("disp: dispatched process level=%o, != pla=%o\n", get16r(pcbp+PCBLEV, 0), regs.sym.pla);
    fatal(NULL);
  }
#endif
#endif
  
  /* pcbp now points to the process we're going to run.  By
     definition, this process should not be on any wait lists,
     so pcb.waitlist(seg) should be zero.  Check it */

  utempa = get16r(pcbp+PCBWAIT, 0);
  if (utempa != 0) {
    printf("disp: pcb %o/%o selected, but wait segno = %o\n", pcbp>>16, pcbp&0xFFFF, utempa);
    fatal(NULL);
  }

  /* save RP in current register set before possibly switching */

  *(unsigned int *)(crs+PB) = RP;

  /* find a register set for this process */

#if 0
  rsnum = (crs[MODALS] & 0340)>>5;
  if (crs[OWNERL] != pcbw && crs[OWNERL] != 0)
    if (regs.rs16[rsnum ^ 1][OWNERL] == 0 || (regs.rs16[rsnum ^ 1][OWNERL] == pcbw && (regs.rs16[rsnum ^ 1][KEYS] & 1)) || ((regs.rs16[rsnum ^ 1][KEYS] & 1) && !(crs[KEYS] & 1)))
      ors();
#endif

  /* Cases that fail w/o any register switch:
     - 3   err dispatch
     - 5   reg data not saved correctly
     - 7   crs.modals
     - 11  crs.modals
     - 13  crs.modals
     - 17  err dispatch
     - 19  crs.modals
     - 25  crs.modals
     - 27  err dispatch
     - 31  crs.modals
     - 45  err dispatch
     - 47  crs.modals
     - 49  err dispatch
     - 51  crs.modals
     - 61  crs.modals
     - 63  ors.X wrong
     - 81  save mask wrong
     - 83  crs.modals

     Cases that fail with simple register switch below:
     - 9   crs.modals
     - 23  crs.modals
     - 29  crs.modals
  *  - 31  saved X wrong
  *  - 83  crs.modals
     - 85  bad
     - 89  crs.modals  - switches register sets
     - 90  crs.modals

  Adding "&& crs[OWNERL] != 0" and setting this to zero in IRTN if PPA is
  invalid fixes case 81 w/o breaking any other tests.

  */

#if 1
  if (crs[OWNERL] != pcbw && crs[OWNERL] != 0)
    ors();
#endif

  /* If the selected register set is owned and hasn't been saved, save
     it before taking it */

  if (crs[OWNERL] == pcbw) {
    TRACE(T_PX, "disp: register set already owned by %o - no save\n", crs[OWNERL]);
    /* NOTE: call newkeys to make sure amask gets set correctly!  Otherwise, 32R mode programs
       are flaky */
    newkeys(crs[KEYS]);
  } else {
    TRACE(T_PX, "disp: saving registers owned by %o\n", crs[OWNERL]);
    pxregsave(0);
    pxregload(pcbp);
  }

  RP = *(unsigned int *)(crs+PB);
  crs[PBL] = 0;
  crs[KEYS] &= ~3;                           /* erase "in dispatcher" and "save done" */
  TRACE(T_PX, "disp: returning from dispatcher, running process %o/%o at %o/%o, modals=%o, ppa=%o, pla=%o, ppb=%o, plb=%o\n", crs[OWNERH], crs[OWNERL], RPH, RPL, crs[MODALS], regs.sym.pcba, regs.sym.pla, regs.sym.pcbb, regs.sym.plb);

  /* if this process' abort flags are set, clear them and take process fault */

  utempa = get16r(pcbp+PCBABT, 0);
  if (utempa != 0) {
    TRACE(T_PX, "dispatch: abort flags for %o are %o\n", crs[OWNERL], utempa);
    //printf("dispatch: abort flags for %o are %o\n", crs[OWNERL], utempa);
    put16r(0, pcbp+PCBABT, 0);
    fault(PROCESSFAULT, utempa, 0);
    fatal("fault returned after process fault");    
  }

  return;

idle:
  fatal("dispatch idle...");
}

/* take me off the ready list, setting my pcb link pointer to the arg
   passed in.  The dispatcher should always be entered after this
   routine. */

unready (ea_t waitlist, unsigned short newlink) {
  unsigned short level, bol, eol;
  unsigned int rl;
  ea_t rlp, pcbp;

  if (regs.sym.pcba != crs[OWNERL])
    fatal("unready: pcba mismatch");

  pcbp = *(ea_t *)(crs+OWNER);
  rlp = MAKEVA(crs[OWNERH], regs.sym.pla);
  rl = get32r(rlp, 0);
  bol = rl >> 16;
  eol = rl & 0xFFFF;
  if (bol != (pcbp & 0xFFFF)) {
    printf("rlp=%o/%o, bol=%o, eol=%o, pcbp=%o/%o, pla=%o, pcba=%o\n", rlp>>16, rlp&0xFFFF, bol, eol, pcbp>>16, pcbp&0xFFFF, regs.sym.pla, regs.sym.pcba);
    fatal("unready: I'm not first on the ready list");
  }
  if (bol == eol) {
    bol = 0;
    eol = 0;
  } else {
    bol = get16r(pcbp+1, 0);
  }
  rl = (bol<<16) | eol;
  put32r(rl, rlp, 0);          /* update ready list */
  TRACE(T_PX, "unready: new rl bol/eol = %o/%o\n", rl>>16, rl&0xFFFF);
  put16r(newlink, pcbp+1, 0);  /* update my pcb link */
  put32r(waitlist, pcbp+2, 0); /* update my pcb wait address */
  *(unsigned int *)(crs+PB) = RP;
  pxregsave(1);
  regs.sym.pcba = 0;
}


/* pcbp points to the pcb to put on the ready list
   begend is 1 for beginning, 0 for end
   returns true if this process is higher priority than me
*/

unsigned short ready (ea_t pcbp, unsigned short begend) {
  ea_t rlp;
  ea_t xpcbp;
  unsigned short bol,eol,pcbw,level,resched;
  unsigned int rl;

  if ((pcbp & 0xFFFF) == crs[OWNERL])
    fatal("Tried to put myself on the ready list!");
#if 0
  /* NOTE: restore drive b, boot 14314, halts here after login_server */
  if (regs.sym.pcba != crs[OWNERL])
    fatal("I'm running, but not regs.sym.pcba!");
#endif

  level = get16r(pcbp+PCBLEV, 0);
  rlp = MAKEVA(crs[OWNERH],level);
  rl = get32r(rlp, 0);
  TRACE(T_PX, "ready: pcbp=%o/%o\n", pcbp>>16, pcbp&0xFFFF);
  TRACE(T_PX, "ready: old bol/eol for level %o = %o/%o\n", level, rl>>16, rl&0xFFFF);
  pcbw = pcbp;                            /* pcb word number */
  if ((rl>>16) == 0) {                    /* bol=0: this RL level was empty */
    put32r(0, pcbp+1, 0);                 /* set link and wait SN in pcb */
    rl = (pcbw<<16) | pcbw;               /* set beg=end */
  } else if (begend) {                    /* notify to beginning */
    put32r(rl & 0xFFFF0000, pcbp+1, 0);   /* set link and wait SN in pcb */
    rl = (pcbw<<16) | rl&0xFFFF;          /* new is bol, eol is unchanged */
  } else {                                /* notify to end */
    put32r(0, pcbp+1, 0);                 /* set link and wait SN in pcb */
    xpcbp = MAKEVA(crs[OWNERH],rl&0xFFFF); /* get ptr to last pcb at this level */
    put16r(pcbw,xpcbp+1, 0);              /* set last pcb's forward link */
    rl = (rl & 0xFFFF0000) | pcbw;        /* rl bol is unchanged, eol is new */
  }
  put32r(rl, rlp, 0);
  TRACE(T_PX, "ready: new bol/eol for level %o = %o/%o, pcb's link is %o\n", level, rl>>16, rl&0xFFFF, get16r(pcbp+1, 0));

  /* is this new process higher priority than me?  If so, return 1
     so that the dispatcher is entered.  If not, check for new plb/pcbb */

  resched = 0;
  if (level < regs.sym.pla || (level == regs.sym.pla && begend)) {
    regs.sym.plb = regs.sym.pla;
    regs.sym.pcbb = regs.sym.pcba;
    regs.sym.pla = level;
    regs.sym.pcba = pcbw;
    resched = 1;
  } else if (level < regs.sym.plb || (level == regs.sym.plb && begend)) {
    regs.sym.plb = level;
    regs.sym.pcbb = pcbw;
  }
  return resched;
}


pwait() {
  ea_t ea;
  ea_t pcbp, prevpcbp;
  unsigned int utempl;
  unsigned int pcblevnext;      /* pcb level and link */
  unsigned short bol;
  unsigned short pcblev;
  unsigned short pcbnext;
  unsigned short mylev;
  short count;

  ea = apea(NULL);
  TRACE(T_PX, "%o/%o: wait on %o/%o, pcb %o, keys=%o, modals=%o\n", RPH, RPL, ea>>16, ea&0xFFFF, crs[OWNERL], crs[KEYS], crs[MODALS]);
  utempl = get32r(ea, 0);     /* get count and BOL */
  count = utempl>>16;         /* count (signed) */
  bol = utempl & 0xFFFF;      /* beginning of wait list */
  TRACE(T_PX, " wait list count was %d, bol was %o\n", count, bol);
  count++;
  if (count > 0) {      /* I have to wait */
#if 1
    if (count == 1 && bol != 0)
      fatal("WAIT: count == 1 but bol != 0");
#else
    /* NOTE: hack to fix:

CFatal error: instruction #81929908 at 6/54311: 315 4400
WAIT: count == 1 but bol != 0
keys = 14200, modals=137
    */
    if (count == 1)
      bol = 0;
#endif
    if (count > 1 && bol == 0)
      fatal("WAIT: count > 1 but bol == 0");
    if (regs.sym.pcba == 0)
      fatal("WAIT: pcba is zero");
    if (regs.sym.pcba != crs[OWNERL]) {
      printf("WAIT: pcba=%o != ownerl=%o\n", regs.sym.pcba, crs[OWNERL]);
      fatal(NULL);
    }
    mylev = get16r(*(ea_t *)(crs+OWNER),0);

    if (bol != 0) {
      pcbp = MAKEVA(crs[OWNERH],bol);
      pcblevnext = get32r(pcbp, 0);
      pcblev = pcblevnext >> 16;
    }
    TRACE(T_PX, " my level=%o, pcblev=%o\n", mylev, pcblev);

    if (count == 1 || mylev < pcblev) {   /* add me to the beginning */
      utempl = (count<<16) | crs[OWNERL];
      put32r(utempl, ea, 0);    /* update semaphore count/bol */
    } else {
      /* do a priority scan... */
      while (pcblev <= mylev && bol != 0) {
	prevpcbp = pcbp;
	bol = pcblevnext & 0xFFFF;
	if (bol != 0) {
	  pcbp = MAKEVA(crs[OWNERH],bol);
	  pcblevnext = get32r(pcbp, 0);
	  pcblev = pcblevnext >> 16;
	}
      }
      put16r(crs[OWNERL], prevpcbp+PCBLINK, 0);
      put16r(*(unsigned short *)&count, ea, 0);    /* update count */
      TRACE(T_PX, " new count=%d, new link for pcb %o=%o, bol=%o\n", prevpcbp&0xffff, crs[OWNERL], bol);
    }
    unready(ea, bol);
    dispatcher();
  } else
    put16(*(unsigned short *)&count, ea); /* just update count and continue */
}

/* this handles several forms of notify:
   - 001210 = NFYE
   - 001211 = NFYB
   - 001214 = INEN, notify to end, no CAI
   - 001215 = INBN, notify to beg, no CAI
   - 001216 = INEC, notify to end, CAI
   - 001217 = INBC, notify to beg, CAI
*/

nfy(unsigned short inst) {
  unsigned short resched, begend, bol, rsnum;
  ea_t ea, pcbp;
  unsigned int utempl;
  short scount;
  static char *nfyname[] = {"nfye","nfyb"," "," ","inen","inbn","inec","inbc"};

  resched = 0;
  begend = inst & 1;
#if 1
  if (regs.sym.pcba != crs[OWNERL]) {
    printf("NFY: regs.pcba = %o, but crs[OWNERL] = %o\n", regs.sym.pcba, crs[OWNERL]);
    fatal(NULL);
  }
#endif
  ea = apea(NULL);
  utempl = get32r(ea, 0);     /* get count and BOL */
  scount = utempl>>16;        /* count (signed) */
  bol = utempl & 0xFFFF;      /* beginning of wait list */
  TRACE(T_PX, "%o/%o: opcode %o %s, ea=%o/%o, count=%d, bol=%o, I am %o\n", RPH, RPL, inst, nfyname[inst-01210], ea>>16, ea&0xFFFF, scount, bol, crs[OWNERL]);

  /* on later models, semaphore overflow should cause a fault */

  if (scount == -32768) {
    printf("NFY: semaphore overflow at ea %o/%o %s\n", ea>>16, ea&0xFFFF, searchloadmap(ea, 'x'));
    fatal(NULL);
  }

  if (scount > 0) {
    if (bol == 0) {
      printf("NFY: bol is zero, count is %d for semaphore at %o/%o\n", scount, ea>>16, ea&0xFFFF);
      fatal(NULL);
    }
    pcbp = MAKEVA(crs[OWNERH], bol);
    utempl = get32r(pcbp+PCBWAIT, 0);
    if (utempl != ea) {
      printf("NFY: bol=%o, pcb waiting on %o/%o != ea %o/%o\n", utempl>>16, utempl&0xFFFF, ea>>16, ea&0xFFFF);
      fatal(NULL);
    }
    bol = get16r(pcbp+PCBLINK, 0);     /* get new beginning of wait list */
    resched = ready(pcbp, begend);     /* put this pcb on the ready list */
  }

  scount = scount-1;
#if 0
  /* NOTE: shouldn't have to do this if everything is working right, but with
     PRIMOS we get:
CFatal error: instruction #81929908 at 6/54311: 315 4400
WAIT: count == 1 but bol != 0
keys = 14200, modals=137CFatal error: instruction #81929908 at 6/54311: 315 4400
WAIT: count == 1 but bol != 0
keys = 14200, modals=137
  */
  if (scount <= 0)
    bol = 0;
#endif
  utempl = (scount<<16) | bol;
  put32r(utempl, ea, 0);         /* update the semaphore */

  if (inst & 4) {                /* interrupt notify */
    if (inst & 2)                /* clear active interrupt */
      intvec = -1;
    /* not sure about all this... Case 85/87 */
    RP = regs.sym.pswpb;
    crs[PBH] = RPH;
    newkeys(regs.sym.pswkeys);
  }

  if (resched || (inst & 4))
    dispatcher();
}


lpsw() {
  ea_t ea;
  unsigned short m;

  TRACE(T_PX, "\n%o/%o: LPSW issued\n", RPH, RPL);
  TRACE(T_PX, "LPSW: before load, RPH=%o, RPL=%o, keys=%o, modals=%o\n", RPH, RPL, crs[KEYS], crs[MODALS]);
  TRACE(T_PX, "LPSW: crs=%d, ownerl[2]=%o, keys[2]=%o, modals[2]=%o, ownerl[3]=%o, keys[3]=%o, modals[3]=%o\n", crs==regs.rs16[2]? 2:3, regs.rs16[2][OWNERL], regs.rs16[2][KEYS], regs.rs16[2][MODALS], regs.rs16[3][OWNERL], regs.rs16[3][KEYS], regs.rs16[3][MODALS]);

  ea = apea(NULL);
  RPH = get16(ea);
  RPL = get16(INCVA(ea,1));
  newkeys(get16(INCVA(ea,2)));
  m = get16(INCVA(ea,3));
  if ((m & 0340) != (crs[MODALS] & 0340))
    TRACE(T_PX, "LPSW: WARNING: changed current register set: current modals=%o, new modals=%o\n", crs[MODALS], m);
  crs[MODALS] = m;
  inhcount = 1;

  TRACE(T_PX, "LPSW:    NEW RPH=%o, RPL=%o, keys=%o, modals=%o\n", RPH, RPL, crs[KEYS], crs[MODALS]);
  TRACE(T_PX, "LPSW: crs=%d, ownerl[2]=%o, keys[2]=%o, modals[2]=%o, ownerl[3]=%o, keys[3]=%o, modals[3]=%o\n", crs==regs.rs16[2]? 2:3, regs.rs16[2][OWNERL], regs.rs16[2][KEYS], regs.rs16[2][MODALS], regs.rs16[3][OWNERL], regs.rs16[3][KEYS], regs.rs16[3][MODALS]);
  if (crs[MODALS] & 020)
    TRACE(T_PX, "Mapped I/O enabled\n");
  if (crs[MODALS] & 4) {
    TRACE(T_PX, "Segmentation enabled\n");
    if (domemdump) dumpsegs();
    //traceflags = ~TB_MAP;
  }
  if (crs[MODALS] & 010) {
    TRACE(T_PX, "Process exchange enabled:\n");
    TRACE(T_PX, "LPSW: PLA=%o, PCBA=%o, PLB=%o, PCBB=%o\n", regs.sym.pla, regs.sym.pcba, regs.sym.plb, regs.sym.pcbb);
#if 0
    for (i=regs.sym.pla;; i += 2) {
      ea = MAKEVA(crs[OWNERH], i);
      utempa = get16(ea);
      TRACE(T_PX, " Level %o: BOL=%o, EOL=%o\n", i, utempa, get16(ea+1));
      if (utempa == 1)
	break;
      while (utempa > 0)
	utempa = dumppcb(utempa);
    }
#endif
    //traceflags = ~TB_MAP;
    if (crs[KEYS] & 2) {
      TRACE(T_PX, "LPSW: before disp, RPH=%o, RPL=%o, keys=%o, modals=%o\n", RPH, RPL, crs[KEYS], crs[MODALS]);
      dispatcher();
      TRACE(T_PX, "LPSW: after disp, RPH=%o, RPL=%o, keys=%o, modals=%o\n", RPH, RPL, crs[KEYS], crs[MODALS]);
      TRACE(T_PX, "LPSW: crs=%d, ownerl[2]=%o, keys[2]=%o, modals[2]=%o, ownerl[3]=%o, keys[3]=%o, modals[3]=%o\n", crs==regs.rs16[2]? 2:3, regs.rs16[2][OWNERL], regs.rs16[2][KEYS], regs.rs16[2][MODALS], regs.rs16[3][OWNERL], regs.rs16[3][KEYS], regs.rs16[3][MODALS]);
    }
  }
#if 0
  /* XXX: hack to disable serial number checking if E32I is enabled.
     Look for ERA/ANA sequence after illegal shift instruction (SSSN),
     set the ANA operand to zero. */

  ea = MAKEVA(014,040747);
  put16(0,ea);
#endif
}


/* Character instructions */

#define GETFLR(n) (((crsl[FLR0+2*(n)] >> 11) & 0x1FFFE0) | (crsl[FLR0+2*(n)] & 0x1F))
#define PUTFLR(n,v) crsl[FLR0+2*(n)] = (((v) << 11) & 0xFFFF0000) | (crsl[FLR0+2*(n)] & 0xF000) | ((v) & 0x1F);

ldc(n) {
  unsigned int utempl;
  unsigned short m;
  unsigned short far, flr;
  ea_t ea;

  far = FAR0;
  flr = FLR0;
  if (n) {
    far = FAR1;
    flr = FLR1;
  }

  utempl = GETFLR(n);
  if (utempl > 0) {
    ea = crsl[far];
    m = get16(crsl[far]);
    if (crsl[flr] & 0x8000) {
      crs[A] = m & 0xFF;
      crsl[flr] &= 0xFFFF0FFF;
      crsl[far] = (crsl[far] & 0x6FFF0000) | ((crsl[far]+1) & 0xFFFF); \
      TRACE(T_INST, " ldc %d = '%o (%c) from %o/%o right\n", n, crs[A], crs[A]&0x7f, ea>>16, ea&0xffff);
      //printf(" ldc %d = '%o (%c) from %o/%o right\n", n, crs[A], crs[A]&0x7f, ea>>16, ea&0xffff);
    } else {
      crs[A] = m >> 8;
      crsl[flr] |= 0x8000;      /* set bit offset */
      TRACE(T_INST, " ldc %d = '%o (%c) from %o/%o left\n", n, crs[A], crs[A]&0x7f, ea>>16, ea&0xffff);
      //printf(" ldc %d = '%o (%c) from %o/%o left\n", n, crs[A], crs[A]&0x7f, ea>>16, ea&0xffff);
    }
    utempl--;
    PUTFLR(n,utempl);
    crs[KEYS] &= ~0100;     /* reset EQ */
  } else {                  /* utempl == 0 */
    TRACE(T_INST, " LDC %d limit\n", n);
    //printf(" LDC %d limit\n", n);
    crs[A] = 0;
    crs[KEYS] |= 0100;      /* set EQ */
  }
}


stc(n) {
  unsigned int utempl;
  unsigned short m;
  unsigned short far, flr;
  ea_t ea;

  far = FAR0;
  flr = FLR0;
  if (n) {
    far = FAR1;
    flr = FLR1;
  }

  utempl = GETFLR(n);
  if (utempl > 0) {
    ea = crsl[far];
    m = get16(crsl[far]);
    if (crsl[flr] & 0x8000) {
      TRACE(T_INST, " stc %d =  '%o (%c) to %o/%o right\n", n, crs[A], crs[A]&0x7f, ea>>16, ea&0xffff);
      //printf(" stc %d =  '%o (%c) to %o/%o right\n", n, crs[A], crs[A]&0x7f, ea>>16, ea&0xffff);
      m = (m & 0xFF00) | (crs[A] & 0xFF);
      put16(m,crsl[far]);
      crsl[flr] &= 0xFFFF0FFF;
      crsl[far] = (crsl[far] & 0x6FFF0000) | ((crsl[far]+1) & 0xFFFF);
    } else {
      TRACE(T_INST, " stc %d = '%o (%c) to %o/%o left\n", n, crs[A], crs[A]&0x7f, ea>>16, ea&0xffff);
      //printf(" stc %d = '%o (%c) to %o/%o left\n", n, crs[A], crs[A]&0x7f, ea>>16, ea&0xffff);
      m = (crs[A] << 8) | (m & 0xFF);
      put16(m,crsl[far]);
      crsl[flr] |= 0x8000;      /* set bit offset */
    }
    utempl--;
    PUTFLR(n,utempl);
    crs[KEYS] &= ~0100;     /* reset EQ */
  } else {                  /* utempl == 0 */
    TRACE(T_INST, " STC %d limit\n", n);
    //printf(" STC %d limit\n", n);
    crs[KEYS] |= 0100;      /* set EQ */
  }
}


/* queue instructions where physical queues may be involved */

int rtq(ea_t qcbea, unsigned short *qent, ea_t rp) {

  unsigned int qtop, qbot, qtemp;
  unsigned short qseg, qmask;
  ea_t qentea;

  qtop = get16r(qcbea, rp);
  qbot = get16r(qcbea+1, rp);

  if (qtop == qbot)
    return 0;               /* queue is empty */
  qseg = get16r(qcbea+2, rp);
  qmask = get16r(qcbea+3, rp);
  qentea = MAKEVA(qseg & 0xfff, qtop);
  if (qseg & 0x8000)        /* virtual queue */
    *qent = get16r(qentea, rp);
  else {
    RESTRICTR(rp);
    *qent = mem[qentea];
  }
  qtop = (qtop & ~qmask) | ((qtop+1) & qmask);
  put16r(qtop & 0xFFFF, qcbea, rp);
  return 1;
}


int abq(ea_t qcbea, unsigned short qent, ea_t rp) {

  unsigned int qtop, qbot, qtemp;
  unsigned short qseg, qmask;
  ea_t qentea;

  qtop = get16r(qcbea, rp);
  qbot = get16r(qcbea+1, rp);
  qseg = get16r(qcbea+2, rp);
  qmask = get16r(qcbea+3, rp);
  qtemp = (qbot & ~qmask) | ((qbot+1) & qmask);
  if (qtemp == qtop)         /* queue full */
    return 0;
  qentea = MAKEVA(qseg & 0xfff,qbot);
  if (qseg & 0x8000)         /* virtual queue */
    put16r(crs[A], qentea, rp);
  else {
    RESTRICTR(rp);
    mem[qentea] = qent;
  }
  put16r(qtemp, qcbea+1, rp);
  return 1;
}


main (int argc, char **argv) {

  static short bootdiskctrl[4] = {026, 027, 022, 023};

  int boot;                            /* true if reading a boot record */
  char *bootarg;                       /* argument to -boot, if any */
  char bootfile[8];                    /* boot file to load (optional) */
  int bootfd;                          /* initial boot file fd */
  int bootctrl, bootunit;              /* boot device controller and unit */
  int bootskip=0;                      /* skip this many bytes on boot dev */

  short tempa,tempa1,tempa2;
  unsigned short utempa,utempa1,utempa2;
  int templ,templ1,templ2;
  long long templl,templl1, templl2;
  unsigned long long utempll, utempll1, utempll2;
  unsigned int utempl,utempl1,utempl2;
  float tempf,tempf1,tempf2;
  double tempd,tempd1,tempd2;
  unsigned short tempda[4],tempda1[4];
  ea_t tempea;
  unsigned int ea32;                   /* full V/I mode eff address */
  ea_t ea;                             /* final MR effective address */
  ea_t eanext;                         /* ea of lower 16 bits in instl */
  unsigned long instl;
  ea_t earp;                           /* RP to use for eff address calcs */
  unsigned short eabit;
  unsigned short opcode;
  short i,j,x;
  unsigned short savemask;
  unsigned short class;
  int nw,nw2;
  unsigned short rvec[9];    /* SA, EA, P, A, B, X, keys, dummy, dummy */
  unsigned short inst;
  unsigned short m,m1,m2;
  unsigned short qtop,qbot,qseg,qmask,qtemp;
  ea_t qea;
  short scount;                          /* shift count */
  unsigned short trapvalue;
  ea_t trapaddr;
  unsigned short stpm[8];
  unsigned short access;

  unsigned short zresult, zclen1, zclen2, zaccess;
  unsigned int zlen1, zlen2;
  ea_t zea1, zea2;
  unsigned char zch1, zch2, *zcp1, *zcp2, zspace;

  struct timeval boot_tv;
  struct timezone tz;
  float mips;

  /* open trace log */

  if ((tracefile=fopen("trace.log", "w")) == NULL) {
    perror("Unable to open trace.log");
    exit(1);
  }

  /* master clear:
     - clear all registers
     - register set is 2
     - set P to '1000
     - 16S mode, single precision
     - interrupts and machine checks inhibited
     - standard interrupt mode
     - all stlb entries are invalid
     - clear 32K words of memory
  */

  for (i=0; i < 32*REGSETS; i++)
    regs.u32[i] = 0;
  crs = (void *)regs.rs[2];           /* boot w/register set 2 */
  crsl = (void *)crs;
  /* NOTE: interrupts should be disabled (0100000 in modals) */
  crs[MODALS] = 0100;
  newkeys(0);
  RPL = 01000;
  for (i=0; i < STLBENTS; i++)
    stlb[i].valid = 0;
  bzero(mem, 32*1024*2);

  verbose = 0;
  domemdump = 0;
  boot = 0;
  bootarg = NULL;
  bootfile[0] = 0;
  pmap32bits = 0;
  csoffset = 0;
  tport = -1;
  nport = -1;

  /* check args */

  for (i=1; i<argc; i++) {
    if (strcmp(argv[i],"-vv") == 0)
      verbose = 2;
    else if (strcmp(argv[i],"-v") == 0)
      verbose = 1;
    else if (strcmp(argv[i],"-map") == 0) {
      while (i+1 < argc && argv[i+1][0] != '-')
	readloadmap(argv[++i]);
    } else if (strcmp(argv[i],"-memdump") == 0)
      domemdump = 1;
    else if (strcmp(argv[i],"-ss") == 0) {
      if (i+1 < argc && argv[i+1][0] != '-') {
	sscanf(argv[++i],"%o", &templ);
	sswitch = templ;
      } else
	sswitch = 0;
    } else if (strcmp(argv[i],"-cpuid") == 0) {
      if (i+1 < argc && argv[i+1][0] != '-') {
	sscanf(argv[++i],"%d", &templ);
	cpuid = templ;
      } else
	fprintf(stderr,"-cpuid needs an argument\n");
    } else if (strcmp(argv[i],"-tport") == 0) {
      if (i+1 < argc && argv[i+1][0] != '-') {
	sscanf(argv[++i],"%d", &templ);
	tport = templ;
      } else
	fprintf(stderr,"-tport needs an argument\n");
    } else if (strcmp(argv[i],"-nport") == 0) {
      if (i+1 < argc && argv[i+1][0] != '-') {
	sscanf(argv[++i],"%d", &templ);
	nport = templ;
      } else
	fprintf(stderr,"-nport needs an argument\n");
    } else if (strcmp(argv[i],"-trace") == 0)
      while (i+1 < argc && argv[i+1][0] != '-') {
	i++;
	if (strcmp(argv[i],"ear") == 0)
	  traceflags |= TB_EAR;
	else if (strcmp(argv[i],"eav") == 0)
	  traceflags |= TB_EAV;
	else if (strcmp(argv[i],"eai") == 0)
	  traceflags |= TB_EAI;
	else if (strcmp(argv[i],"inst") == 0)
	  traceflags |= TB_INST;
	else if (strcmp(argv[i],"flow") == 0)
	  traceflags |= TB_FLOW;
	else if (strcmp(argv[i],"mode") == 0)
	  traceflags |= TB_MODE;
	else if (strcmp(argv[i],"eaap") == 0)
	  traceflags |= TB_EAAP;
	else if (strcmp(argv[i],"dio") == 0)
	  traceflags |= TB_DIO;
	else if (strcmp(argv[i],"map") == 0)
	  traceflags |= TB_MAP;
	else if (strcmp(argv[i],"pcl") == 0)
	  traceflags |= TB_PCL;
	else if (strcmp(argv[i],"fault") == 0)
	  traceflags |= TB_FAULT;
	else if (strcmp(argv[i],"px") == 0)
	  traceflags |= TB_PX;
	else if (strcmp(argv[i],"tio") == 0)
	  traceflags |= TB_TIO;
	else if (strcmp(argv[i],"term") == 0)
	  traceflags |= TB_TERM;
	else if (strcmp(argv[i],"all") == 0)
	  traceflags = ~0;
	else if (isdigit(argv[i][0]) && strlen(argv[i]) < 2 && sscanf(argv[i],"%d", &templ) == 1)
	  traceuser = 0100000 | (templ<<6);   /* form OWNERL for user # */
	else if (strlen(argv[i]) == 6 && sscanf(argv[i],"%o", &templ) == 1)
	  traceuser = templ;                  /* specify OWNERL directly */
	else if (strlen(argv[i]) <= 8 && argv[i][0] != '-') {
	  if (numtraceprocs >= MAXTRACEPROCS)
	    fprintf(stderr,"Only %d trace procs are allowed\n", MAXTRACEPROCS);
	  else {
	    traceprocs[numtraceprocs].oneshot = 1;
	    for (j=0; argv[i][j]; j++)
	      if (argv[i][j] == '+')
		traceprocs[numtraceprocs].oneshot = 0;
	      else
		traceprocs[numtraceprocs].name[j] = argv[i][j];
	    traceprocs[numtraceprocs].name[j] = 0;
	    traceprocs[numtraceprocs].sb = -1;
	    traceprocs[numtraceprocs].ecb = 0;
	    numtraceprocs++;
	  }
	} else
	  fprintf(stderr,"Unrecognized trace flag: %s\n", argv[i]);
      }
    else if (strcmp(argv[i],"-boot") == 0) {
      boot = 1;
      if (i+1 < argc && argv[i+1][0] != '-') {
	i++;
	if (strlen(argv[i]) <= 6 && sscanf(argv[i],"%o", &templ) == 1)
	  sswitch = templ;
	else
	  bootarg = argv[i];
      }

    } else
      fprintf(stderr,"Unrecognized argument: %s\n", argv[i]);
  }

  /* finish setting up tracing after all options are read, ie, maps */

  if (traceuser != 0)
    TRACEA("Tracing enabled for OWNERL %o\n", traceuser);
  else
    TRACEA("Tracing enabled for all users");
  savetraceflags = traceflags;
  for (i=0; i<numtraceprocs; i++) {
    for (j=0; j<numsyms; j++) {
      if (strcasecmp(mapsym[j].symname, traceprocs[i].name) == 0 && mapsym[j].symtype == 'e') {
	ea = mapsym[j].address;
	traceprocs[i].ecb = ea;
	TRACEA("Tracing procedure %s ecb ea '%o/%o\n", traceprocs[i].name, SEGNO(ea), ea&0xFFFF);
	printf("Tracing procedure %s ecb ea '%o/%o\n", traceprocs[i].name, SEGNO(ea), ea&0xFFFF);
	break;
      }
    }
    if (j == numsyms)
      fprintf(stderr,"Can't find procedure %s in load maps for tracing.\n", traceprocs[i].name);
  }
    
  /* set some vars after the options have been read */

  pmap32bits = (cpuid == 15 || cpuid == 18 || cpuid == 19 || cpuid == 24 || cpuid >= 26);
  if ((26 <= cpuid && cpuid <= 29) || cpuid >= 35)
    csoffset = 1;
  if (tport == -1)
    tport = 8000;
  if (nport == -1)
    nport = 8001;

  /* initialize all devices */

  for (i=0; i<64; i++)
    if (devmap[i])
      if (devmap[i](-1, 0, i)) {   /* if initialization fails, */
	devmap[i] = devnone;       /* remove device */
	printf("emulator: device '%o failed initialization - device removed\n", i);
      }


  os_init();

  /* if a filename follows -boot, load and execute this R-mode runfile
     image.  For example, -boot *DOS64 would load *DOS64 from the Unix
     file system and begin executing Primos II.

     SECURITY: check that boot filename isn't a pathname?
  */

  if (bootarg) {
    if ((bootfd=open(bootarg, O_RDONLY)) == -1) {
      perror("Error opening boot file");
      fatal(NULL);
    }
    if (read(bootfd, rvec, 18) != 18) {
      perror("Error reading boot file's rvec header");
      fatal(NULL);
    }

  } else {

    /* If no filename follows -boot, then the sense switches are used to
       determine whether the boot record should be read from tape or disk
       and select the controller and drive unit.

       Bits 14-16 are 4 for disk boot, 5 for tape boot
       Bit 13 is 1 for disk boot, don't care for tape boot
       Bits 11-12 are the unit number, 0-4
    */

    bootunit = (sswitch>>7) & 3;
    rvec[2] = 01000;                  /* starting address */
    rvec[3] = rvec[4] = rvec[5] = rvec[6] = 0;

    if ((sswitch & 0x7) == 4) {         /* disk boot */
      bootctrl = bootdiskctrl[(sswitch>>4) & 3];
      rvec[0] = 0760;                   /* disk load starts at '760 */
      rvec[1] = rvec[0]+1040-1;         /* read 1 disk block */
      /* setup DMA register '20 (address only) for the next boot record */
      regs.sym.regdmx[041] = 03000;

    } else if ((sswitch & 0x7) == 5) {  /* tape boot */
      bootctrl = 014;
      rvec[0] = 0200;                   /* tape load starts at '200 */
      rvec[1] = rvec[0]+2355-1;         /* read in at most 3 pages (6K) */
      bootskip = 4;                     /* to skip .TAP header */
      /* setup DMA register '20 (address only) for the next boot record */
      regs.sym.regdmx[041] = 0200+2355;;

    } else {
      printf("\
\n\
The -boot option is used to boot from disk, tape, or to load a Prime\n\
runfile directly from the Unix file system.  For example:\n\
\n\
  -boot 14xx4 to boot from disk (see below)\n\
  -boot 10005 to boot from tape.\n\
  -boot *DOS64 to load *DOS64 from the Unix file and execute it\n\
\n\
For disk boots, the last 3 digits can be:\n\
\n\
  114 = dev26u0 ctrl '26 unit 0       154 = dev22u0 ctrl '22 unit 0\n\
  314 = dev26u1 ctrl '26 unit 1       354 = dev22u1 ctrl '22 unit 1\n\
  514 = dev26u2 ctrl '26 unit 2       554 = dev22u2 ctrl '22 unit 2\n\
  714 = dev26u3 ctrl '26 unit 3       754 = dev22u3 ctrl '22 unit 3\n\
\n\
  134 = dev27u0 ctrl '27 unit 0       174 = dev23u0 ctrl '23 unit 0\n\
  334 = dev27u1 ctrl '27 unit 1       374 = dev23u1 ctrl '23 unit 1\n\
  534 = dev27u2 ctrl '27 unit 2       574 = dev23u2 ctrl '23 unit 2\n\
  734 = dev27u3 ctrl '27 unit 3       774 = dev23u3 ctrl '23 unit 3\n\
\n\
  The default option is -boot 14114, to boot from disk dev26u0\n");
      exit(1);
    }

    snprintf(bootfile, sizeof(bootfile), "dev%ou%d", bootctrl, bootunit);
    TRACEA("Boot file is %s\n", bootfile);
    if ((bootfd=open(bootfile, O_RDONLY)) == -1) {
      perror("Error opening boot device file");
      fatal(NULL);
    }
    if (lseek(bootfd, bootskip, SEEK_CUR) == -1) {
      perror("Error skipping on boot device");
      fatal(NULL);
    }
  }
  TRACEA("Sense switches set to %o\n", sswitch);
  TRACE(T_FLOW, "Boot SA=%o, EA=%o, P=%o, A=%o, B=%o, X=%o, K=%o\n\n", rvec[0], rvec[1], rvec[2], rvec[3], rvec[4], rvec[5], rvec[6]);
  if (rvec[2] > rvec[1])
    fatal("Program start > ending: boot image is corrupt");

  /* read memory image from SA to EA inclusive */

  nw = rvec[1]-rvec[0]+1;
  if ((nw2=read(bootfd, mem+rvec[0], nw*2)) == -1) {
    perror("Error reading memory image");
    fatal(NULL);
  }

  /* check we got it all, except for tape boots; the boot program size
     is unpredictable on tape */
  
  if (nw2 != nw*2 && ((sswitch & 0x7) == 4 || bootarg)) {
    printf("rvec[0]=%d, rvec[1]=%d, nw2=%d, nw=%d, nw*2=%d\n", rvec[0], rvec[1], nw2, nw, nw*2);
    fatal("Didn't read entire boot program");
  }

  /* setup execution (registers, keys, address mask, etc.) from rvec */

  crs[A] = rvec[3];
  crs[B] = rvec[4];
  crs[X] = rvec[5];
  newkeys(rvec[6]);
  RPL = rvec[2];

  memdump(rvec[0], rvec[1]);

  /* initialize the timer stuff */

  if (gettimeofday(&boot_tv, &tz) != 0) {
    perror("gettimeofday failed");
    fatal(NULL);
  }

  /* main instruction decode loop
     faults longjmp here: the top of the instruction fetch loop */

  if (setjmp(jmpbuf))
    ;

  while (1) {

#if 0
    /* trace AC$SET call not working

       NOTE: a 2-word range is needed for RPL because a procedure with
       arguments may start executing at the ARGT instruction (listed
       in the load map as procedure start), or at the instruction
       following ARGT (if PCL completes w/o faults) */

    if (TRACEUSER && SEGNO16(RPH) == 041 && 06200 <= RPL && RPL <= 06201) { /* ac$set */
      savetraceflags = ~TB_MAP;
      printf("enable trace, RPH=%o, RPL=%o\n", SEGNO16(RPH), RPL);
    }
    if (TRACEUSER && SEGNO16(RPH) == 013 && 044030 <= RPL && RPL <= 044031) { /* setrc$ */
      savetraceflags = 0;
      printf("disable trace, RPH=%o, RPL=%o\n", SEGNO16(RPH), RPL);
    }
#endif

#if 0
    /* this is for FTN Generic 3 trace */
    if (SEGNO16(RPH) == 4000 && RPL >= 034750 && RPL <= 034760)
      traceflags = ~TB_MAP;
    else
      traceflags = 0;
#endif

#if 0
    if (instcount > 77500000)
      traceflags = ~TB_MAP;
#endif

#if 0
    /* NOTE: this tends to cause a page fault loop if the location
       being monitored isn't wired */

    if (trapaddr != 0 && (crs[OWNERL] & 0100000) && (crs[MODALS] & 010)) {
      traceflags = -1;
      printf("TRAP: at #%d\n", instcount);
      utempa = get16(trapaddr);
      if (utempa != trapvalue) {
	printf("TRAP: at #%d, old value of %o/%o was %o; new value is %o\n", instcount, trapaddr>>16, trapaddr&0xffff, trapvalue, utempa);
	trapvalue = utempa;
	printf("TRAP: new trap value is %o\n", trapvalue);
      }
    }
#endif

    /* is this user being traced? */

    if (TRACEUSER)
      traceflags = savetraceflags;
    else
      traceflags = 0;

    /* poll any devices that requested a poll */

    for (i=0; i<64; i++)
      if (devpoll[i] && (--devpoll[i] <= 0)) {
	if (!devmap[i])
	  fatal("devpoll set but devmap is null");
	devmap[i](4, 0, i);
      }

    /* is an interrupt pending, with interrupts enabled? */

    if (intvec >= 0 && (crs[MODALS] & 0100000) && inhcount == 0) {
      //printf("fetch: taking interrupt vector '%o, modals='%o\n", intvec, crs[MODALS]);
      TRACE(T_INST, "\nfetch: taking interrupt vector '%o, modals='%o\n", intvec, crs[MODALS]);
      regs.sym.pswpb = RP;
      regs.sym.pswkeys = crs[KEYS];

      if (crs[MODALS] & 010) {              /* PX enabled */
	//traceflags = ~TB_MAP;
	newkeys(014000);
	RPH = 4;
	RPL = intvec;

      } else if (crs[MODALS] & 040000) {    /* vectored interrupt mode */
	m = get16(intvec);
	if (m != 0) {
	  put16(RPL, m);
	  RP = m+1;
	} else {
	  printf("fetch: interrupt vector '%o = 0 in vectored interrupt mode\n", intvec);
	  fatal(NULL);
	}

      } else {                              /* standard interrupt mode */
	m = get16(063);
	printf("Standard mode interrupt vector loc = %o\n", m);
	//traceflags = ~TB_MAP;
	if (m != 0) {
	  put16(RPL, m);
	  RP = m+1;
	} else {
	  fatal("fetch: loc '63 = 0 in standard interrupt mode");
	}
      }
      crs[MODALS] &= 077777;   /* inhibit interrupts */
      //intvec = -1;
    }
    if (inhcount)
      inhcount--;

    /* as a speedup later, fetch 32/64 bits (or the rest of the page)
       and maintain a prefetch queue */

    prevpc = RP;
    ea = RP;
#if 0
    /* NOTE: Rev 21 Sys Arch Guide, 2nd Ed, pg 3-32 says:

       "When bits 17 to 32 of the program counter contain a value within
       the ATR (address trap range) and the processor is reading an 
       instruction, an address trap always occurs.  The only exception
       to this is if the machine is operating in 32I mode."

       However, if this code is enabled, the Primos boot fails very
       early, before verifying memory.  */
    
    if ((ea & 0xFFFF) < 040)
      ea = 0x80000000 | (ea & 0xFFFF);
#endif

    inst = iget16(ea);
    RPL++;
    instcount++;

    /* while a process is running, RP is the real program counter, PBH
       is the active procedure segment, and PBL is zero.  When a
       process stops running, RP is copied to PB.  When a process
       starts running again, PB is copied to RP. */

    crs[PBH] = RPH;
    crs[PBL] = 0;
    earp = RP;

    if (crs[MODALS] & 010) {     /* px enabled, bump 1ms process timer */
      if (crs[TIMERL]++ > instpermsec) {
	crs[TIMERL] = 0;

	/* if 1ms resolution process timer overflows, set pcb abort flag */

	crs[TIMER]++;
	if (crs[TIMER] == 0) {
	  TRACE(T_PX,  "#%d: pcb %o timer overflow\n", instcount, crs[OWNERL]);
	  ea = *(ea_t *)(crs+OWNER);
	  m = get16r(ea+4, 0) | 1;       /* set process abort flag */
	  put16r(m, ea+4, 0);
	}
      }
    }

#if 0
    /* NOTE: this is to debug generic instruction 3's in Primos boot */

    if (instcount > 75376100)
      //traceflags = -1;
      traceflags = TB_DIO;

#endif

xec:
    /* NOTE: don't trace JMP * instructions (used to test PX) */

#if 0
    if (inst == 03777)
      traceflags = 0;
#endif

#if 0
    if (crs[OWNERL] == 0100200 && inst == 001114 && savetraceflags)
      traceflags = ~0;
    else
      traceflags = 0;
#endif

    TRACE(T_FLOW, "\n			#%u [%s %o] IT=%d SB: %o/%o LB: %o/%o %s XB: %o/%o\n%o/%o: %o		A='%o/%:0d B='%o/%d L='%o/%d E='%o/%d X=%o/%d Y=%o/%d C=%d L=%d LT=%d EQ=%d K=%o M=%o\n", instcount, searchloadmap(*(unsigned int *)(crs+OWNER),'x'), crs[OWNERL], *(short *)(crs+TIMER), crs[SBH], crs[SBL], crs[LBH], crs[LBL], searchloadmap(*(unsigned int *)(crs+LBH),'l'), crs[XBH], crs[XBL], RPH, RPL-1, inst, crs[A], *(short *)(crs+A), crs[B], *(short *)(crs+B), *(unsigned int *)(crs+L), *(int *)(crs+L), *(unsigned int *)(crs+E), *(int *)(crs+E), crs[X], *(short *)(crs+X), crs[Y], *(short *)(crs+Y), (crs[KEYS]&0100000) != 0, (crs[KEYS]&020000) != 0, (crs[KEYS]&0200) != 0, (crs[KEYS]&0100) != 0, crs[KEYS], crs[MODALS]);

    /* begin instruction decode: generic? */

    if ((inst & 036000) == 0) {
      class = inst>>14;
      if (class == 0) {
	TRACE(T_INST, " generic class 0\n");
	switch (inst) {

	/* V-mode/frequent instructions */

	case 000201:
	  TRACE(T_FLOW, " IAB\n");
	  tempa = crs[B];
	  crs[B] = crs[A];
	  crs[A] = tempa;
	  continue;

	case 001314:
	  TRACE(T_FLOW, " CGT\n");
	  tempa = iget16(RP);              /* get number of words */
	  if (1 <= crs[A] && crs[A] < tempa)
	    RPL = iget16(INCVA(RP,crs[A]));
	  else
	    RPL += tempa;
	  continue;

	case 000115:
	  TRACE(T_FLOW, " PIDA\n");
	  *(int *)(crs+L) = *(short *)(crs+A);
	  continue;

	case 000305:
	  TRACE(T_FLOW, " PIDL\n");
	  *(long long *)(crs+L) = *(int *)(crs+L);
	  continue;

	/* XXX: how does PIMA affect registers when overflow occurs?
	   NOTE: PMA manual says copy B reg to A reg, but DIAG seems
	   to indicate a swap */

	case 000015:
	  TRACE(T_FLOW, " PIMA\n");
	  tempa = crs[B];
	  crs[B] = crs[A];
	  crs[A] = tempa;
	  SETL(0);
	  SETCC_A;
	  if (((crs[A] ^ crs[B]) & 0x8000) || (crs[B] != 0 && crs[B] != 0xFFFF))
	    mathexception('i', FC_INT_OFLOW, 0);
	  else
	    CLEARC;
	  continue;

	case 000301:
	  TRACE(T_FLOW, " PIML\n");
	  templ = *(int *)(crs+L);
	  *(int *)(crs+L) = *(int *)(crs+E);
	  SETL(0);
	  SETCC_L;
	  if (((templ ^ crs[E]) & 0x8000) || (templ != 0 && templ != -1))
	    mathexception('i', FC_INT_OFLOW, 0);
	  else
	    CLEARC;
	  continue;

	/* character/field instructions */

	case 001302:
	  TRACE(T_FLOW, " LDC 0\n");
	  ldc(0);
	  continue;

	case 001312:
	  TRACE(T_FLOW, " LDC 1\n");
	  ldc(1);
	  continue;

	case 001322:
	  TRACE(T_FLOW, " STC 0\n");
	  stc(0);
	  continue;
	    
	case 001332:
	  TRACE(T_FLOW, " STC 1\n");
	  stc(1);
	  continue;

	case 001300:
	  TRACE(T_FLOW, " EAFA 0\n");
	  ea = apea(&eabit);
	  crsl[FAR0] = ea;
	  crsl[FLR0] = (crsl[FLR0] & 0xFFFF0FFF) | (eabit << 12);
	  TRACE(T_INST, " FAR0=%o/%o, eabit=%d, FLR=%x\n", crsl[FAR0]>>16, crsl[FAR0]&0xFFFF, eabit, crsl[FLR0]);
	  continue;

	case 001310:
	  TRACE(T_FLOW, " EAFA 1\n");
	  ea = apea(&eabit);
	  crsl[FAR1] = ea;
	  crsl[FLR1] = (crsl[FLR1] & 0xFFFF0FFF) | (eabit << 12);
	  TRACE(T_INST, " FAR1=%o/%o, eabit=%d, FLR=%x\n", crsl[FAR1]>>16, crsl[FAR1]&0xFFFF, eabit, crsl[FLR1]);
	  continue;

	case 001301:
	  TRACE(T_FLOW, " ALFA 0\n");
	  TRACE(T_INST, " before add, FAR0=%o/%o, FLR=%o\n", crsl[FAR0]>>16, crsl[FAR0]&0xFFFF, crsl[FLR0]);
	  utempl = ((crsl[FAR0] & 0xFFFF) << 4) | ((crsl[FLR0] >> 12) & 0xF);
	  utempl += *(int *)(crs+L);
	  crsl[FAR0] = (crsl[FAR0] & 0xFFFF0000) | ((utempl >> 4) & 0xFFFF);
	  crsl[FLR0] = (crsl[FLR0] & 0xFFFF0FFF) | ((utempl & 0xF) << 12);
	  TRACE(T_INST, " after add, FAR0=%o/%o, FLR=%o\n", crsl[FAR0]>>16, crsl[FAR0]&0xFFFF, crsl[FLR0]);
	  continue;

	case 001311:
	  TRACE(T_FLOW, " ALFA 1\n");
	  TRACE(T_INST, " before add, FAR1=%o/%o, FLR=%o\n", crsl[FAR1]>>16, crsl[FAR1]&0xFFFF, crsl[FLR1]);
	  utempl = ((crsl[FAR1] & 0xFFFF) << 4) | ((crsl[FLR1] >> 12) & 0xF);
	  utempl += *(int *)(crs+L);
	  crsl[FAR1] = (crsl[FAR1] & 0xFFFF0000) | ((utempl >> 4) & 0xFFFF);
	  crsl[FLR1] = (crsl[FLR1] & 0xFFFF0FFF) | ((utempl & 0xF) << 12);
	  TRACE(T_INST, " after add, FAR1=%o/%o, FLR=%o\n", crsl[FAR1]>>16, crsl[FAR1]&0xFFFF, crsl[FLR1]);
	  continue;

	case 001303:
	  TRACE(T_FLOW, " LFLI 0\n");
#if 0
	  for (utempa=0; utempa<256; utempa++) {
	    PUTFLR(0,utempa);
	    crsl[FLR0] |= 0x4000;
	    utempl = GETFLR(0);
	    if (utempa != utempl) {
	      printf(" loaded %d, fetched %d\n", utempa, utempl);
	      fatal(NULL);
	    }
	  }
#endif
	  utempa = iget16(RP);
	  RPL++;
	  PUTFLR(0,utempa);
	  utempl = GETFLR(0);
	  TRACE(T_INST, " Load Field length with %d, FLR=%x, actual = %d\n", utempa, crsl[FLR0], utempl);
	  if (utempa != utempl)
	    fatal("LFLI 0 error");
	  continue;

	case 001313:
	  TRACE(T_FLOW, " LFLI 1\n");
#if 0
	  for (utempa=0; utempa<256; utempa++) {
	    PUTFLR(1,utempa);
	    utempl = GETFLR(1);
	    if (utempa != utempl) {
	      printf(" loaded %d, fetched %d\n", utempa, utempl);
	      fatal(NULL);
	    }
	  }
#endif
	  utempa = iget16(RP);
	  RPL++;
	  PUTFLR(1,utempa);
	  utempl = GETFLR(1);
	  TRACE(T_INST, " Load Field length with %d, FLR=%x, actual = %d\n", utempa, crsl[FLR1], utempl);
	  if (utempa != utempl)
	    fatal("LFLI 1 error");
	  continue;

	case 001320:
	  TRACE(T_FLOW, " STFA 0\n");
	  ea = apea(NULL);
	  utempl = crsl[FAR0] & 0x6FFFFFFF;
	  utempa = crsl[FLR0] & 0xF000;
stfa:
	  if (utempa != 0) {
	    utempl = utempl | EXTMASK32;
	    put16(utempa,INCVA(ea,2));
	    TRACE(T_INST, " stored 3-word pointer %o/%o %o\n", utempl>>16, utempl&0xffff, utempa);
	  } else {
	    TRACE(T_INST, " stored 2-word pointer %o/%o\n", utempl>>16, utempl&0xffff);
	  }
	  put32(utempl,ea);
	  continue;

	case 001330:
	  TRACE(T_FLOW, " STFA 1\n");
	  ea = apea(NULL);
	  utempl = crsl[FAR1] & 0x6FFFFFFF;
	  utempa = crsl[FLR1] & 0xF000;
	  goto stfa;

	case 001321:
	  TRACE(T_FLOW, " TLFL 0\n");
	  PUTFLR(0,*(unsigned int *)(crs+L));
	  continue;

	case 001331:
	  TRACE(T_FLOW, " TLFL 1\n");
	  PUTFLR(1,*(unsigned int *)(crs+L));
	  utempl = GETFLR(1);
	  TRACE(T_INST, " Transfer %d to FLR1, FLR=%x, actual = %d\n", *(unsigned int *)(crs+L), crsl[FLR1], utempl);
	  continue;

	case 001323:
	  TRACE(T_FLOW, " TFLL 0\n");
	  *(unsigned int *)(crs+L) = GETFLR(0);
	  continue;

	case 001333:
	  TRACE(T_FLOW, " TFLL 1\n");
	  *(unsigned int *)(crs+L) = GETFLR(1);
	  continue;
	
	case 000611:
	  TRACE(T_FLOW, " PRTN\n");
	  prtn();

	  /* if this PRTN is for a procedure being traced, disable
	     tracing if one-shot is true */

	  if (numtraceprocs > 0 && TRACEUSER)
	    for (i=0; i<numtraceprocs; i++)
	      if (*(int *)(crs+SB) == traceprocs[i].sb) {
		printf("Disabled trace for %s at sb '%o/%o\n", traceprocs[i].name, crs[SBH], crs[SBL]);
		savetraceflags = 0;
		traceprocs[i].sb = -1;
		fflush(tracefile);
		break;
	      }
	  continue;

	case 001005:
	  TRACE(T_FLOW, " TKA\n");
	  crs[A] = crs[KEYS];
	  continue;

	case 001015:
	  TRACE(T_FLOW, " TAK\n");
	  newkeys(crs[A] & 0177770);
	  continue;

	case 000001:
	  TRACE(T_FLOW, " NOP\n");
	  continue;

	case 000715:
	  TRACE(T_FLOW, " RSAV\n");
	  ea = apea(NULL);
	  j = 1;
	  savemask = 0;
	  for (i = 11; i >= 0; i--) {
	    if (crsl[i] != 0) {
	      TRACE(T_INST, " crsl[%d] saved, value=%o (%o/%o)\n", i, crsl[i], crsl[i]>>16, crsl[i]&0xffff);
	      put32(crsl[i], INCVA(ea,j));
	      savemask |= bitmask16[16-i];
	    }
	    j += 2;
	  }
	  put32(*(int *)(crs+XB), INCVA(ea,25));
	  TRACE(T_INST, " XB saved, value=%o/%o\n", crs[XBH], crs[XBL]);
	  put16(savemask, ea);
	  TRACE(T_INST, " Saved, mask=%o\n", savemask);
	  continue;

	case 000717:
	  TRACE(T_FLOW, " RRST\n");
	  ea = apea(NULL);
	  savemask = get16(ea);
	  TRACE(T_INST, " Save mask=%o\n", savemask);
	  j = 1;
	  for (i = 11; i >= 0; i--) {
	    if (savemask & bitmask16[16-i]) {
	      crsl[i] = get32(INCVA(ea,j));
	      TRACE(T_INST, " crsl[%d] restored, value=%o (%o/%o)\n", i, crsl[i], crsl[i]>>16, crsl[i]&0xffff);
	    } else {
	      crsl[i] = 0;
	    }
	    j += 2;
	  }
	  *(unsigned int *)(crs+XB) = get32(INCVA(ea,25));
	  TRACE(T_INST, " XB restored, value=%o/%o\n", crs[XBH], crs[XBL]);
	  continue;

	case 000400:
	case 000401:
	case 000402:
	  TRACE(T_FLOW, " ENB\n");
	  RESTRICT();
	  crs[MODALS] |= 0100000;
	  inhcount = 1;
	  continue;

	case 001000:
	case 001001:
	case 001002:
	  TRACE(T_FLOW, " INH\n");
	  RESTRICT();
	  crs[MODALS] &= ~0100000;
	  continue;

	case 01200:
	  TRACE(T_FLOW, " STAC\n");
	  ea = apea(NULL);
	  if (get16(ea) == crs[B]) {
	    put16(crs[A], ea);
	    crs[KEYS] |= 0100;       /* set EQ */
	  } else 
	    crs[KEYS] &= ~0100;      /* reset EQ */
	  continue;

	case 01204:
	  TRACE(T_FLOW, " STLC\n");
	  ea = apea(NULL);
	  if (get32(ea) == *(unsigned int *)(crs+E)){
	    put32(*(unsigned int *)(crs+L), ea);
	    crs[KEYS] |= 0100;       /* set EQ */
	  } else 
	    crs[KEYS] &= ~0100;      /* reset EQ */
	  continue;

	/* NOTE: when ARGT is executed as an instruction, it means
	   that a fault occurred during PCL argument processing. */

	case 000605:
	  TRACE(T_FLOW|T_PCL, " ARGT\n");
	  argt();
	  continue;

	case 000705:
	  TRACE(T_FLOW|T_PCL, " CALF\n");
	  ea = apea(NULL);
	  calf(ea);
	  continue;

	/* Decimal and character instructions */

#if 1
 
#define ZGETC(zea, zlen, zcp, zclen, zch) \
  if (zclen == 0) { \
    zcp = (unsigned char *) (mem+mapva(zea, RACC, &zaccess, RP)); \
    zclen = 2048 - (zea & 01777)*2; \
    if (zea & EXTMASK32) { \
      zcp++; \
      zclen--; \
    } \
    if (zclen >= zlen) \
      zclen = zlen; \
    else \
      zea = (zea & 0xEFFF0000) | ((zea+0x400) & 0xFC00); \
  } \
  zch = *zcp; \
  zcp++; \
  zclen--; \
  zlen--

#define ZPUTC(zea, zlen, zcp, zclen, zch) \
  if (zclen == 0) { \
    zcp = (unsigned char *) (mem+mapva(zea, WACC, &zaccess, RP)); \
    zclen = 2048 - (zea & 01777)*2; \
    if (zea & EXTMASK32) { \
      zcp++; \
      zclen--; \
    } \
    if (zclen >= zlen) \
      zclen = zlen; \
    else \
      zea = (zea & 0xEFFF0000) | ((zea+0x400) & 0xFC00); \
  } \
  *zcp = zch; \
  zcp++; \
  zclen--; \
  zlen--

	case 001114:
	  TRACE(T_FLOW, " ZMV\n");
	  if (crs[KEYS] & 020)
	    zspace = 040;
	  else
	    zspace = 0240;
	  //printf("ZMV: source=%o/%o, len=%d, dest=%o/%o, len=%d, keys=%o\n", crsl[FAR0]>>16, crsl[FAR0]&0xffff, GETFLR(0), crsl[FAR1]>>16, crsl[FAR1]&0xffff, GETFLR(1), crs[KEYS]);
	  zlen1 = GETFLR(0);
	  zlen2 = GETFLR(1);
	  zea1 = crsl[FAR0];
	  if (crsl[FLR0] & 0x8000)
	    zea1 |= EXTMASK32;
	  zea2 = crsl[FAR1];
	  if (crsl[FLR1] & 0x8000)
	    zea2 |= EXTMASK32;
	  TRACE(T_INST, " ea1=%o/%o, len1=%d, ea2=%o/%o, len2=%d\n", zea1>>16, zea1&0xffff, zlen1, zea2>>16, zea2&0xffff, zlen2);
	  zclen1 = 0;
	  zclen2 = 0;
	  while (zlen2) {
	    if (zlen1) {
	      ZGETC(zea1, zlen1, zcp1, zclen1, zch1);
	    } else
	      zch1 = zspace;
	    TRACE(T_INST, " zch1=%o (%c)\n", zch1, zch1&0x7f);
	    ZPUTC(zea2, zlen2, zcp2, zclen2, zch1);
	  }
	  crs[KEYS] |= 0100;
	  continue;

	case 001115:
	  TRACE(T_FLOW, " ZMVD\n");
	  zlen1 = GETFLR(1);
	  zlen2 = zlen1;
	  zea1 = crsl[FAR0];
	  if (crsl[FLR0] & 0x8000)
	    zea1 |= EXTMASK32;
	  zea2 = crsl[FAR1];
	  if (crsl[FLR1] & 0x8000)
	    zea2 |= EXTMASK32;
	  TRACE(T_INST, " ea1=%o/%o, ea2=%o/%o, len=%d\n", zea1>>16, zea1&0xffff, zea2>>16, zea2&0xffff, zlen1);
	  zclen1 = 0;
	  zclen2 = 0;
	  while (zlen2) {
	    ZGETC(zea1, zlen1, zcp1, zclen1, zch1);
	    TRACE(T_INST, " zch1=%o (%c)\n", zch1, zch1&0x7f);
	    ZPUTC(zea2, zlen2, zcp2, zclen2, zch1);
	  }
	  crs[KEYS] |= 0100;
	  continue;

	/* NOTE: ZFIL is used early after PX enabled, and can be used to cause
	   a UII fault to debug CALF etc.

	   Could use memset(zcp2, zch2, zclen2) to fill by pieces.
	*/

	case 001116:
	  TRACE(T_FLOW, " ZFIL\n");
	  zlen2 = GETFLR(1);
	  zea2 = crsl[FAR1];
	  if (crsl[FLR1] & 0x8000)
	    zea2 |= EXTMASK32;
	  zch2 = crs[A];
	  TRACE(T_INST, " ea=%o/%o, len=%d, fill=%o (%c)\n", zea2>>16, zea2&0xffff, GETFLR(1), zch2, zch2&0x7f);
	  //printf("ZFIL: ea=%o/%o, len=%d\n", zea2>>16, zea2&0xffff, GETFLR(1));
	  zclen2 = 0;
	  while (zlen2) {
	    ZPUTC(zea2, zlen2, zcp2, zclen2, zch2);
	    //printf(" after put, zlen2=%d, zcp2=%x, zclen2=%d, zch2=%o\n", zlen2, zcp2, zclen2, zch2);
	  }
	  crs[KEYS] |= 0100;
	  continue;

	case 001117:
	  TRACE(T_FLOW, " ZCM\n");
	  if (crs[KEYS] & 020)
	    zspace = 040;
	  else
	    zspace = 0240;
	  zlen1 = GETFLR(0);
	  zlen2 = GETFLR(1);
	  zea1 = crsl[FAR0];
	  if (crsl[FLR0] & 0x8000)
	    zea1 |= EXTMASK32;
	  zea2 = crsl[FAR1];
	  if (crsl[FLR1] & 0x8000)
	    zea2 |= EXTMASK32;
	  TRACE(T_INST, " ea1=%o/%o, len1=%d, ea2=%o/%o, len2=%d\n", zea1>>16, zea1&0xffff, GETFLR(0), zea2>>16, zea2&0xffff, GETFLR(1));
	  zresult = 0100;                /* assume equal */
	  zclen1 = 0;
	  zclen2 = 0;
	  while (zlen1 || zlen2) {
	    if (zlen1) {
	      ZGETC(zea1, zlen1, zcp1, zclen1, zch1);
	    } else
	      zch1 = zspace;
	    if (zlen2) {
	      ZGETC(zea2, zlen2, zcp2, zclen2, zch2);
	    } else
	      zch2 = zspace;
	    TRACE(T_INST, " zch1=%o (%c), zch2=%o (%c)\n", zch1, zch1&0x7f, zch2, zch2&0x7f);
	    if (zch1 < zch2) {
	      zresult = 0200;
	      break;
	    } else if (zch1 > zch2) {
	      zresult = 0;
	      break;
	    }
	  }
	  crs[KEYS] = (crs[KEYS] & ~0300) | zresult;
	  continue;
#endif

	/* 001100 = XAD
	   001101 = XMV
	   001102 = XCM
	   001104 = XMP
	   001107 = XDV
	   001110 = ZTRN
	   001111 = ZED
	   001112 = XED
	   001114 = ZMV
	   001115 = ZMVD
	   001116 = ZFIL
	   001117 = ZCM
	   001145 = XBTD
	   001146 = XDTB
	*/

	/* OS/restricted instructions */

	case 000510:
	  TRACE(T_FLOW, " STTM\n", inst);
	  RESTRICT();
	  fault(UIIFAULT, RPL, RP);
	  continue;

	case 000511:
	  TRACE(T_FLOW, " RTS\n", inst);
	  RESTRICT();
	  //traceflags = ~TB_MAP;
	  fault(UIIFAULT, RPL, RP);
	  continue;

	case 000315:
	  TRACE(T_FLOW, " WAIT\n", inst);
	  RESTRICT();
	  pwait();
	  continue;

	case 001210:
	case 001211:
	case 001214:
	case 001215:
	case 001216:
	case 001217:
	  TRACE(T_FLOW, " NFY\n", inst);
	  RESTRICT();
	  nfy(inst);
	  continue;
	    
	case 001212:
	case 001213:
	  fatal("Unrecognized NFY instruction");

	case 001315:
	  TRACE(T_FLOW, " STEX\n");
	  *(ea_t *)(crs+L) = stex(*(unsigned int *)(crs+L));
	  continue;

	/* NOTE: L contains target virtual address.  How does 
	   LIOT use the target virtual address to "help invalidate
	   the cache"? */

	case 000044:
	  TRACE(T_FLOW, " LIOT\n");
	  RESTRICT();
	  ea = apea(NULL);
	  utempa = STLBIX(ea);
	  stlb[utempa].valid = 0;
	  TRACE(T_INST, " invalidated STLB index %d\n", utempa);
	  mapva(ea, RACC, &access, RP);
	  TRACE(T_INST, " loaded STLB for %o/%o\n", ea>>16, ea&0xffff);
	  continue;

	case 000064:
	  TRACE(T_FLOW, " PTLB\n");
	  RESTRICT();
	  utempl = *(unsigned int *)(crs+L);
	  for (utempa = 0; utempa < STLBENTS; utempa++)
	    if ((utempl & 0x80000000) || stlb[utempa].ppn == utempl)
	      stlb[utempa].valid = 0;
	  continue;

	case 000615:
	  TRACE(T_FLOW, " ITLB\n");
	  RESTRICT();
	  utempl = *(unsigned int *)(crs+L);

	  /* NOTE: Primos substitutes an ITLB loop for PTLB, and the ITLB
	     segno is 1, ie, it looks like using segment 1 invalidates all
	     pages that match, ignoring segment number??  Instead of doing
	     that, we purge the STLB whenever address 1/0 is invalidated. */

	  if (utempl == 0x10000) {
	    for (utempa = 0; utempa < STLBENTS; utempa++)
	      stlb[utempa].valid = 0;
	    TRACE(T_INST, " purged entire STLB\n");
	  } else {
	    utempa = STLBIX(utempl);
	    stlb[utempa].valid = 0;
	    TRACE(T_INST, " invalidated STLB index %d\n", utempa);
	  }
#if 0
	  /* HACK for DIAG to suppress ITLB loop in trace */
	  if (RP == 0106070)
	    if (*(int *)(crs+L) == 0) {
	      TRACEA(" Suppressing DIAG trace\n");
	      savetraceflags = traceflags;
	      traceflags = 0;
	    } else if (crs[A] == 07777 && crs[B] == 0176000) {
	      TRACEA(" Restoring DIAG trace\n");
	      traceflags = savetraceflags;
	    }
#endif
	  continue;

	case 000711:
	  TRACE(T_FLOW, " LPSW\n");
	  RESTRICT();
	  lpsw();
	  continue;

	case 000024:
	  TRACE(T_FLOW, " STPM\n", inst);
	  RESTRICT();
	  for (i=0; i<8; i++)
	    stpm[i] = 0;
	  stpm[1] = cpuid;
	  ea = *(unsigned int *)(crs+XB);
	  put64(*(double *)(stpm+0), ea);
	  put64(*(double *)(stpm+4), INCVA(ea,4));
	  continue;

	/* JW: I think this is an invalid opcode that Prime uses when
	   unexpected things happen, for example:

	   LDA modals        get modals
	   SAS 1             interrupts enabled?
           1702              no, they should be, die
	*/

	case 001702:
	  TRACE(T_FLOW, " 1702?\n", inst);
#if 1
	  fatal("Primos software assertion failure");
#else
	  RESTRICT();
	  //fault(UIIFAULT, RPL, RP);
	  traceflags = ~TB_MAP;
	  dispatcher();
#endif
	  continue;

	case 000601:
	  TRACE(T_FLOW, " IRTN\n", inst);
	  RESTRICT();
	  //fatal("IRTN causes a loop in CPU.CACHE Case 4");
irtn:
	  RP = regs.sym.pswpb;
	  crs[PBH] = RPH;
	  newkeys(regs.sym.pswkeys);
	  crs[MODALS] |= 0100000;
#if 0
	  if (regs.sym.pcba != 0) {
	    RP = regs.sym.pswpb;
	    newkeys(regs.sym.pswkeys);
	  } else
	    crs[OWNERL] = 0;
#endif
	  dispatcher();
	  continue;

	case 000603:
	  TRACE(T_FLOW, " IRTC\n", inst);
	  RESTRICT();
	  intvec = -1;
	  goto irtn;

	case 000411:
	  TRACE(T_FLOW, " CAI\n", inst);
	  RESTRICT();
	  intvec = -1;
	  continue;

	/* R-mode/infrequent gen 0 instructions */

	case 000005:                 /* SGL */
	  TRACE(T_FLOW, " SGL\n");
	  crs[KEYS] &= ~040000;
	  continue;

	case 000011:                 /* E16S */
	  TRACE(T_FLOW, " E16S\n");
	  newkeys(crs[KEYS] & 0161777);
	  continue;

	case 000013:                 /* E32S */
	  TRACE(T_FLOW, " E32S\n");
	  newkeys((crs[KEYS] & 0161777) | 1<<10);
	  continue;

	case 001013:                 /* E32R */
	  TRACE(T_FLOW, " E32R\n");
	  newkeys((crs[KEYS] & 0161777) | 3<<10);
	  continue;

	case 001011:                 /* E64R */
	  TRACE(T_FLOW, " E64R\n");
	  newkeys((crs[KEYS] & 0161777) | 2<<10);
	  continue;

	case 000010:                 /* E64V */
	  TRACE(T_FLOW, " E64V\n");
	  newkeys((crs[KEYS] & 0161777) | 6<<10);
	  continue;

	case 001010:                 /* E32I */
	  TRACE(T_FLOW, " E32I\n");
	  if (cpuid < 4)
	    fault(RESTRICTFAULT, 0, 0);
	  else
	    newkeys((crs[KEYS] & 0161777) | 4<<10);
	  continue;

	case 000505:                 /* SVC */
	  TRACE(T_FLOW, " SVC\n");
	  svc();
	  continue;

	case 000111:                  /* CEA */
	  TRACE(T_FLOW, " CEA\n");
	  switch ((crs[KEYS] & 016000) >> 10) {
	  case 0:                       /* 16S */
	    ea = crs[A];
	    i = ea & 0100000;
	    x = ea & 040000;
	    ea &= 037777;
	    while (1) {
	      if (x)                           /* indexed */
		ea += crs[X];
	      if (!i)                          /* not indirect */
		break;
	      if (ea < 040)
		m = get16(0x80000000|ea);
	      else
		m = get16(MAKEVA(RPH,ea));
	      i = m & 0100000;
	      x = m & 040000;
	      ea = m & 037777;                 /* go indirect */
	    }
	    crs[A] = ea;
	    break;
	  case 1:                       /* 32S */
	  case 3:                       /* 32R */
	    while (crs[A] & 0100000) {
	      ea = crs[A] & 077777;
	      if (ea < 040)
		crs[A] = get16(0x80000000|ea);
	      else
		crs[A] = get16(MAKEVA(RPH,ea));
	    }
	  }
	  continue;

	case 000000:
	  TRACE(T_FLOW, " HLT\n");
	  RESTRICT();
	  memdump(0,0xFFFF);
	  fatal("CPU halt");

	case 000205:                /* PIM (R-mode) */
	  TRACE(T_FLOW, " PIM\n");
#if 0
	  /* NOTE: this fits the description in the Rev 21 ISG, but fails
	     DIAG test CPU.INTEGER, Case 12 */

	  crs[A] = (crs[A] & 0x8000) | (crs[B] & 0x7FFF);
#else
	  crs[A] = (crs[A] & 0x8000) | crs[B];
#endif
	  continue;

	case 000211:                /* PID (R-mode) */
	  TRACE(T_FLOW, " PID\n");
	  *(int *)(crs+L) = *(short *)(crs+A);
	  crs[B] &= 0x7fff;
	  continue;

	/* DBL activates 31-bit mode (R-mode only):

	   LDA -> DLD (double load)
	   STA -> DST (double store)
	   ADD -> DAD (double add)
	   SUB -> DSB (double subtract)

	   Other R-mode, 31-bit instructions include:
	   
	   PID, DIV, MPY, PIM, INT, FLOT
	*/

	case 000007:                 /* DBL */
	  TRACE(T_FLOW, " DBL\n");
	  crs[KEYS] |= 040000;
	  continue;

	case 000041:
	  TRACE(T_FLOW, " SCA\n");
	  crs[A] = crs[VSC] & 0xFF;
	  continue;

	case 000043:
	  TRACE(T_FLOW, " INK\n");
	  crs[A] = (crs[KEYS] & 0xFF00) | (crs[VSC] & 0xFF);
	  continue;

	case 000405:
	  TRACE(T_FLOW, " OTK\n");
	  newkeys((crs[A] & 0xFF00) | (crs[KEYS] & 0xFF));
	  crs[VSC] = (crs[VSC] & 0xFF00) | (crs[A] & 0xFF);
	  if ((RP & RINGMASK32) == 0)
	    inhcount = 1;
	  continue;

	case 000415:
	  TRACE(T_FLOW, " ESIM\n");
	  RESTRICT();
	  crs[MODALS] &= ~040000;
	  continue;

	case 000417:
	  TRACE(T_FLOW, " EVIM\n");
	  RESTRICT();
	  crs[MODALS] |= 040000;
	  continue;

	case 000101:
	  TRACE(T_FLOW, " NRM\n");
	  crs[VSC] = 0;
	  if (crs[A] == 0 && crs[B] == 0)
	    continue;
	  while (!((crs[A] ^ (crs[A] << 1)) & 0x8000)) {
	    TRACE(T_INST,  " step %d: crs[A]=%o, crs[B]=%o\n", crs[VSC], crs[A], crs[B]);
	    crs[B] = crs[B] << 1;
	    crs[A] = (crs[A] & 0x8000) | ((crs[A] << 1) & 0x7FFE) | (crs[B] >> 15);
	    crs[VSC]++;
	  }
	  crs[B] &= 0x7FFF;
	  TRACE(T_INST,  " finished with %d shifts: crs[A]=%o, crs[B]=%o\n", crs[VSC], crs[A], crs[B]);
	  continue;

	case 000105:
	  TRACE(T_FLOW, " RTN\n");
	  m = get16(crs[S]+1);
	  if (m == 0)
	    fatal("RTN stack underflow");
	  crs[S] = get16(crs[S]);
	  continue;

	/* unusual instructions */

	case 000003:
	  TRACE(T_FLOW, " gen 3?\n");
	  TRACEA("#%d: %o/%o: Generic instruction 3?\n", instcount, RPH, RPL);
	  continue;

	default:
	  for (i=0; i<GEN0TABSIZE; i++) {
	    if (inst == gen0tab[i]) {
	      TRACE(T_FLOW, " %s\n", gen0nam[i]);
	      break;
	    }
	  }
	  if (i < GEN0TABSIZE)
	    continue;

	  if (001100 <= inst && inst <= 001146) {
	    //traceflags = -1;
	    TRACE(T_FLOW, " X/Z UII %o\n", inst);
	    fault(UIIFAULT, RPL, RP);
	    continue;
	  }

	  TRACEA(" unrecognized generic class 0 instruction!\n");
	  printf("#%d: %o/%o: Unrecognized generic class 0 instruction '%o!\n", instcount, RPH, RPL, inst);
	  //traceflags = ~TB_MAP;
	  fault(UIIFAULT, RPL, 0);
	  fatal(NULL);
	}
      }

      if (class == 3) {
	TRACE(T_INST, " generic class 3\n");

	switch (inst) {

	case 0141604:
	  TRACE(T_FLOW, " BCLT\n");
bclt:
	  if (crs[KEYS] & 0200)
	    RPL = iget16(RP);
	  else
	    RPL++;
	  continue;

	case 0141600:
	  TRACE(T_FLOW, " BCLE\n");
bcle:
	  if (crs[KEYS] & 0300)
	    RPL = iget16(RP);
	  else
	    RPL++;
	  continue;

	case 0141602:
	  TRACE(T_FLOW, " BCEQ\n");
bceq:
	  if (crs[KEYS] & 0100)
	    RPL = iget16(RP);
	  else
	    RPL++;
	  continue;

	case 0141603:
	  TRACE(T_FLOW, " BCNE\n");
bcne:
	  if (!(crs[KEYS] & 0100))
	    RPL = iget16(RP);
	  else
	    RPL++;
	  continue;

	case 0141605:
	  TRACE(T_FLOW, " BCGE\n");
bcge:
	  if (!(crs[KEYS] & 0200))
	    RPL = iget16(RP);
	  else
	    RPL++;
	  continue;

	case 0141601:
	  TRACE(T_FLOW, " BCGT\n");
bcgt:
	  if (!(crs[KEYS] & 0300))
	    RPL = iget16(RP);
	  else
	    RPL++;
	  continue;

	case 0141705:
	  TRACE(T_FLOW, " BCR\n");
	  if (!(crs[KEYS] & 0100000))
	    RPL = iget16(RP);
	  else
	    RPL++;
	  continue;

	case 0141704:
	  TRACE(T_FLOW, " BCS\n");
	  if (crs[KEYS] & 0100000)
	    RPL = iget16(RP);
	  else
	    RPL++;
	  continue;

	case 0141707:
	  TRACE(T_FLOW, " BMLT/BLR\n");
	  if (!(crs[KEYS] & 020000))
	    RPL = iget16(RP);
	  else
	    RPL++;
	  continue;

	case 0141706:
	  TRACE(T_FLOW, " BLS\n");
bls:
	  if (crs[KEYS] & 020000)
	    RPL = iget16(RP);
	  else
	    RPL++;
	  continue;

	case 0140614:
	  TRACE(T_FLOW, " BLT\n");
	  SETCC_A;
	  goto bclt;

	case 0140610:
	  TRACE(T_FLOW, " BLE\n");
	  SETCC_A;
	  goto bcle;

	case 0140612:
	  TRACE(T_FLOW, " BEQ\n");
	  SETCC_A;
	  goto bceq;

	case 0140613:
	  TRACE(T_FLOW, " BNE\n");
	  SETCC_A;
	  goto bcne;

	case 0140615:
	  TRACE(T_FLOW, " BGE\n");
	  SETCC_A;
	  goto bcge;

	case 0140611:
	  TRACE(T_FLOW, " BGT\n");
	  SETCC_A;
	  goto bcgt;
	  continue;

	case 0140700:
	  TRACE(T_FLOW, " BLLE\n");
	  SETCC_L;
	  goto bcle;

	case 0140702:
	  TRACE(T_FLOW, " BLEQ\n");
	  SETCC_L;
	  goto bceq;

	case 0140703:
	  TRACE(T_FLOW, " BLNE\n");
	  SETCC_L;
	  goto bcne;

	case 0140701:
	  TRACE(T_FLOW, " BLGT\n");
	  SETCC_L;
	  goto bcgt;

	case 0141614:
	  TRACE(T_FLOW, " BFLT\n");
	  SETCC_F;
	  goto bclt;

	case 0141610:
	  TRACE(T_FLOW, " BFLE\n");
	  SETCC_F;
	  goto bcle;

	case 0141612:
	  TRACE(T_FLOW, " BFEQ\n");
	  SETCC_F;
	  goto bceq;

	case 0141613:
	  TRACE(T_FLOW, " BFNE\n");
	  SETCC_F;
	  goto bcne;

	case 0141615:
	  TRACE(T_FLOW, " BFGE\n");
	  SETCC_F;
	  goto bcge;

	case 0141611:
	  TRACE(T_FLOW, " BFGT\n");
	  SETCC_F;
	  goto bcgt;

	case 0141334:
	  TRACE(T_FLOW, " BIX\n");
	  crs[X]++;
bidx:
	  if (crs[X] != 0)
	    RPL = iget16(RP);
	  else
	    RPL++;
	  continue;

	case 0141324:
	  TRACE(T_FLOW, " BIY\n");
	  crs[Y]++;
bidy:
	  if (crs[Y] != 0)
	    RPL = iget16(RP);
	  else
	    RPL++;
	  continue;

	case 0140724:
	  TRACE(T_FLOW, " BDY\n");
	  crs[Y]--;
	  goto bidy;

	case 0140734:
	  TRACE(T_FLOW, " BDX\n");
	  crs[X]--;
#if 1
	  m = iget16(RP);
	  if (crs[X] > 100 && m == RPL-1) {
	    struct timeval tv0,tv1;
	    long delayusec, actualmsec;

	    /* for BDX *-1 loop (backstop process mainly), we want to change
	       this to a 10ms sleep so that the emulation host doesn't peg the
	       CPU.

	       So first, check to see if any device times expire sooner than
	       this, and if so, limit the sleep time to the lowest expiration
	       value (this is stored as number of instructions left until the
	       timer expires).

	       NOTE: In practice, the clock device ticks at 330 times a sec,
	       so we only get to delay about 3ms here.
	    */

	    utempl = instpermsec*10;          /* limit delay to 10 millisecs */
	    for (i=0; i<64; i++)              /* check device timers */
	      if (devpoll[i])                 /* poll set? */
		if (devpoll[i] < 0 || devpoll[i] <= 100) {  /* too fast! */
		utempl = 1;
		break;
	      } else if (devpoll[i] < utempl)
		utempl = devpoll[i];
	    utempl--;                         /* utempl = # instructions */
	    delayusec = utempl*1000/instpermsec;
	    if (delayusec > 1000) {
	      if (gettimeofday(&tv0, NULL) != 0)
		fatal("em: gettimeofday 0 failed");
	      usleep(delayusec);
	      if (gettimeofday(&tv1, NULL) != 0)
		fatal("em: gettimeofday 1 failed");
	      actualmsec = (tv1.tv_sec-tv0.tv_sec-1)*1000 + (tv1.tv_usec+1000000-tv0.tv_usec)/1000;
	      // TRACEA(" BDX loop at %o/%o, remainder=%d, owner=%o, utempl=%d, wanted %d us, got %d ms\n", prevpc>>16, prevpc&0xffff, crs[X], crs[OWNERL], utempl, delayusec, actualusec);

	      /* do timer bookkeeping that would have occurred if we had 
		 actually looped on BDX utempl times */

	      for (i=0; i<64; i++)
		if (devpoll[i] > 0)
		  devpoll[i] -= utempl;
	      crs[X] = 0;
	      utempa = crs[TIMER];
	      if (actualmsec > 0) {
		crs[TIMER] += actualmsec;
		if (crs[TIMER] < utempa) {
		  tempea = *(ea_t *)(crs+OWNER);
		  utempa = get16r(tempea+4, 0) | 1;    /* set process abort flag */
		  put16r(utempa, tempea+4, 0);
		}
	      } else {
		crs[TIMERL] += utempl;
	      }
	      instcount += actualmsec*instpermsec;
	    }
	  }
	  if (crs[X] != 0)
	    RPL = m;
	  else
	    RPL++;
	  continue;
#else
	  goto bidx;
#endif

	case 0141206:
	  TRACE(T_FLOW, " A1A\n");
a1a:
	  crs[KEYS] &= ~0120300;                 /* clear C, L, LT, EQ */
	  utempa = crs[A];
	  utempl = crs[A];
	  utempl += 1;
	  crs[A] = utempl;
	  if (utempl & 0x10000)                  /* set L-bit if carry */
	    crs[KEYS] |= 020000;  
	  if (crs[A] == 0)                       /* set EQ? */
	    crs[KEYS] |= 0100; 
	  else if (*(short *)(crs+A) < 0)        /* set LT? */
	    if (*(short *)&utempa == 32767)      /* overflow? */
	      mathexception('i', FC_INT_OFLOW, 0); /* yes, LT is 0 */
	    else
	      crs[KEYS] |= 0200;
	  continue;

	case 0140304:
	  TRACE(T_FLOW, " A2A\n");
	  crs[KEYS] &= ~0120300;                 /* clear C, L, LT, EQ */
	  utempa = crs[A];
	  utempl = crs[A];
	  utempl += 2;
	  crs[A] = utempl;
	  if (utempl & 0x10000)                  /* set L-bit if carry */
	    crs[KEYS] |= 020000;  
	  if (crs[A] == 0)                       /* set EQ? */
	    crs[KEYS] |= 0100; 
	  else if (*(short *)(crs+A) < 0)        /* set LT? */
	    if (*(short *)&utempa >= 32766)      /* overflow? */
	      mathexception('i', FC_INT_OFLOW, 0);
	    else
	      crs[KEYS] |= 0200;
	  continue;

	case 0141216:
	  TRACE(T_FLOW, " ACA\n");
	  if (crs[KEYS] & 0100000)
	    goto a1a;
	  crs[KEYS] &= ~0120300;                 /* clear C, L, LT, EQ */
	  if (crs[A] == 0)                       /* set EQ? */
	    crs[KEYS] |= 0100; 
	  else if (*(short *)(crs+A) < 0)        /* set LT? */
	    crs[KEYS] |= 0200;
	  continue;

	case 0140110:
	  TRACE(T_FLOW, " S1A\n");
	  crs[KEYS] &= ~0120300;                 /* clear C, L, LT, EQ */
	  utempl = crs[A];
	  utempl += 0xFFFF;
	  crs[A] = utempl;
	  if (utempl & 0x10000)                  /* set L-bit if carry */
	    crs[KEYS] |= 020000;  
	  if (crs[A] == 0)                       /* set EQ? */
	    crs[KEYS] |= 0100; 
	  else if (*(short *)(crs+A) < 0)        /* set LT? */
	    crs[KEYS] |= 0200;
	  else if (crs[A] == 32767) {            /* overflow: set C and LT */
	    crs[KEYS] |= 0200;
	    mathexception('i', FC_INT_OFLOW, 0);
	  }
	  continue;

	case 0140310:
	  TRACE(T_FLOW, " S2A\n");
	  crs[KEYS] &= ~0120300;                 /* clear C, L, LT, EQ */
	  utempa = crs[A];
	  utempl = crs[A];
	  utempl += 0xFFFE;
	  crs[A] = utempl;
	  if (utempl & 0x10000)                  /* set L-bit if carry */
	    crs[KEYS] |= 020000;  
	  if (crs[A] == 0)                       /* set EQ? */
	    crs[KEYS] |= 0100; 
	  else if (*(short *)(crs+A) < 0)        /* set LT? */
	    crs[KEYS] |= 0200;
	  else if (*(short *)&utempa <= -32767) { /* overflow: set C and LT */
	    crs[KEYS] |= 0200;
	    mathexception('i', FC_INT_OFLOW, 0);
	  }
	  continue;

	case 0141050:
	  TRACE(T_FLOW, " CAL\n");
	  crs[A] &= 0xFF;
	  continue;

	case 0141044:
	  TRACE(T_FLOW, " CAR\n");
	  crs[A] &= 0xFF00;
	  continue;

	case 0140040:
	  TRACE(T_FLOW, " CRA\n");
	  crs[A] = 0;
	  continue;

	/* On the P300, the B register is the low-order word of the
	   DP floating pt fraction, so CRB was used to convert SPFP
	   numbers to DPFP.  On the P400 and up, the B register and
	   DPFP accumulator do not overlap.  For compatibility, there
	   are 3 related instructions:

	   '14 clears B and the low-order DPFP register
	   '15 clears only B
	   '16 clears only the low-order DPFP register
	*/

	case 0140014:
	  TRACE(T_FLOW, " P300CRB\n");
	  crs[B] = 0;
	  crs[FLTD] = 0;
	  continue;

	case 0140015:
	  TRACE(T_FLOW, " CRB\n");
	  crs[B] = 0;
	  continue;

	case 0140016:
	  TRACE(T_FLOW, " FDBL\n");
	  crs[FLTD] = 0;
	  continue;

	case 0140010:
	  TRACE(T_FLOW, " CRL\n");
	  *(int *)(crs+L) = 0;
	  continue;

	/* XXX: this should set the L bit like subtract */

	case 0140214:
	  TRACE(T_FLOW, " CAZ\n");
	  crs[KEYS] &= ~020300;               /* clear L, LT, EQ */
	  if (*(short *)(crs+A) < 0) {        /* A < 0? */
	    crs[KEYS] |= 0200;                /* yes, set LT */
	    RPL += 2;                         /* skip */
	  } else if (crs[A] == 0) {           /* A == 0? */
	    crs[KEYS] |= 0100;                /* yes, set EQ */
	    RPL++;                            /* skip */
	  }                                   /* else, A > 0 */
	  XSETL(0);
	  continue;

	/* NOTE: using "if (crs[X]++ == 0)" doesn't work because of
	   unsigned short type promotion! */

	case 0140114:
	  TRACE(T_FLOW, " IRX\n");
	  crs[X]++;
	  if (crs[X] == 0)
	    RPL++;
	  continue;

	case 0140210:
	  TRACE(T_FLOW, " DRX\n");
	  crs[X]--;
	  if (crs[X] == 0)
	    RPL++;
	  continue;

	case 0141240:
	  TRACE(T_FLOW, " ICR\n");
	  crs[A] = crs[A] << 8;
	  continue;

	case 0141140:
	  TRACE(T_FLOW, " ICL\n");
	  crs[A] = crs[A] >> 8;
	  continue;

	case 0141340:
	  TRACE(T_FLOW, " ICA\n");
	  crs[A] = (crs[A] >> 8) | (crs[A] << 8);
	  continue;

	/* NOTE: Rev 21 Inst. Guide says CC are indeterminate, other
	   references say they are set */

	case 0140417:
	  TRACE(T_FLOW, " LT\n");
	  crs[A] = 1;
	  continue;

	case 0140416:
	  TRACE(T_FLOW, " LF\n");
	  crs[A] = 0;
	  continue;

	case 0140314:
	  TRACE(T_FLOW, " TAB\n");
	  crs[B] = crs[A];
	  continue;

	case 0140504:
	  TRACE(T_FLOW, " TAX\n");
	  crs[X] = crs[A];
	  continue;

	case 0140505:
	  TRACE(T_FLOW, " TAY\n");
	  crs[Y] = crs[A];
	  continue;

	case 0140604:
	  TRACE(T_FLOW, " TBA\n");
	  crs[A] = crs[B];
	  continue;

	case 0141034:
	  TRACE(T_FLOW, " TXA\n");
	  crs[A] = crs[X];
	  continue;

	case 0141124:
	  TRACE(T_FLOW, " TYA\n");
	  crs[A] = crs[Y];
	  continue;

	case 0140104:
	  TRACE(T_FLOW, " XCA\n");
	  crs[B] = crs[A];
	  crs[A] = 0;
	  continue;

	case 0140204:
	  TRACE(T_FLOW, " XCB\n");
	  crs[A] = crs[B];
	  crs[B] = 0;
	  continue;

	case 0140407:
	  TRACE(T_FLOW, " TCA\n");
	  *(short *)(crs+A) = - (*(short *)(crs+A));
	  SETCC_A;
	  SETL(crs[A] == 0);
	  if (crs[A] != 0x8000) {
	    CLEARC;
	  } else {
	    crs[KEYS] &= ~0200;
	    mathexception('i', FC_INT_OFLOW, 0);
	  }
	  continue;

	case 0141210:
	  TRACE(T_FLOW, " TCL\n");
	  *(int *)(crs+L) = - (*(int *)(crs+L));
	  SETCC_L;
	  SETL(*(int *)(crs+L) == 0);
	  if (*(unsigned int *)(crs+L) != 0x80000000) {
	    CLEARC;
	  } else {
	    crs[KEYS] &= ~0200;
	    mathexception('i', FC_INT_OFLOW, 0);
	  }
	  continue;

	case 0140600:
	  TRACE(T_FLOW, " SCB\n");
	  crs[KEYS] |= 0100000;
	  continue;

	case 0140200:
	  TRACE(T_FLOW, " RCB\n");
	  crs[KEYS] &= 077777;
	  continue;

	case 0140024:
	  TRACE(T_FLOW, " CHS\n");
	  crs[A] ^= 0x8000;
	  continue;

	case 0140500:
	  TRACE(T_FLOW, " SSM\n");
	  crs[A] |= 0100000;
	  continue;

	case 0140100:
	  TRACE(T_FLOW, " SSP\n");
	  crs[A] &= 077777;
	  continue;

	case 0140401:
	  TRACE(T_FLOW, " CMA\n");
	  crs[A] = ~crs[A];
	  continue;

	case 0140320:
	  TRACE(T_FLOW, " CSA\n");
	  crs[KEYS] = (crs[KEYS] & 077777) | (crs[A] & 0x8000);
	  crs[A] = crs[A] & 077777;
	  continue;

	case 0141500:
	  TRACE(T_FLOW, " LCLT\n");
lclt:
	  crs[A] = ((crs[KEYS] & 0200) != 0);
	  continue;

	case 0141501:
	  TRACE(T_FLOW, " LCLE\n");
lcle:
	  crs[A] = ((crs[KEYS] & 0300) != 0);
	  continue;

	case 0141503:
	  TRACE(T_FLOW, " LCEQ\n");
lceq:
	  crs[A] = ((crs[KEYS] & 0100) != 0);
	  continue;

	case 0141502:
	  TRACE(T_FLOW, " LCNE\n");
lcne:
	  crs[A] = ((crs[KEYS] & 0100) == 0);
	  continue;

	case 0141504:
	  TRACE(T_FLOW, " LCGE\n");
lcge:
	  crs[A] = !(crs[KEYS] & 0200);
	  continue;

	case 0141505:
	  TRACE(T_FLOW, " LCGT\n");
lcgt:
	  crs[A] = ((crs[KEYS] & 0300) == 0);
	  continue;

	case 0140410:
	  TRACE(T_FLOW, " LLT\n");
	  SETCC_A;
	  goto lclt;

	case 0140411:
	  TRACE(T_FLOW, " LLE\n");
	  SETCC_A;
	  goto lcle;

	case 0140412:
	  TRACE(T_FLOW, " LNE\n");
	  SETCC_A;
	  goto lcne;

	case 0140413:
	  TRACE(T_FLOW, " LEQ\n");
	  SETCC_A;
	  goto lceq;

	case 0140414:
	  TRACE(T_FLOW, " LGE\n");
	  SETCC_A;
	  goto lcge;

	case 0140415:
	  TRACE(T_FLOW, " LGT\n");
	  SETCC_A;
	  goto lcgt;

	case 0141511:
	  TRACE(T_FLOW, " LLLE\n");
	  SETCC_L;
	  goto lcle;

	case 0141513:
	  TRACE(T_FLOW, " LLEQ\n");
	  SETCC_L;
	  goto lceq;

	case 0141512:
	  TRACE(T_FLOW, " LLNE\n");
	  SETCC_L;
	  goto lcne;

	case 0141515:
	  TRACE(T_FLOW, " LLGT\n");
	  SETCC_L;
	  goto lcgt;

	case 0141110:
	  TRACE(T_FLOW, " LFLT\n");
	  SETCC_F;
	  goto lclt;

	case 0141111:
	  TRACE(T_FLOW, " LFLE\n");
	  SETCC_F;
	  goto lcle;

	case 0141113:
	  TRACE(T_FLOW, " LFEQ\n");
	  SETCC_F;
	  goto lceq;

	case 0141112:
	  TRACE(T_FLOW, " LFNE\n");
	  SETCC_F;
	  goto lcne;

	case 0141114:
	  TRACE(T_FLOW, " LFGE\n");
	  SETCC_F;
	  goto lcge;

	case 0141115:
	  TRACE(T_FLOW, " LFGT\n");
	  SETCC_F;
	  goto lcgt;

	case 0140550:
	  TRACE(T_FLOW, " FLOT\n");
	  templ = *(short *)(crs+A);
	  templ = crs[B] | (templ<<15);
	  tempf = templ;
	  tempf1 = tempf;
	  ieeepr4(&tempf);
	  crs[FLTH] = (*(unsigned int *)&tempf) >> 16;
	  crs[FLTL] = (*(unsigned int *)&tempf) & 0xFF00;
	  crs[FEXP] = (*(unsigned int *)&tempf) & 0xFF;
	  crs[FLTD] = 0;
	  TRACE(T_INST, " A|B=%d, conv=%f, FEXP=%d (dec), FLTH='%o, FLTL='%o\n", templ, tempf1, crs[FEXP], crs[FLTH], crs[FLTL]);
	  continue;

	case 0140534:
	  TRACE(T_FLOW, " FRN\n");
	  continue;

	case 0140574:
	  TRACE(T_FLOW, " DFCM\n");
	  TRACE(T_INST, " FEXP=%d (dec), FLTH='%o, FLTL='%o\n", crs[FEXP], crs[FLTH], crs[FLTL]);
	  tempda[0] = crs[FLTH];
	  tempda[1] = crs[FLTL];
	  tempda[2] = crs[FLTD];
	  tempda[3] = crs[FEXP];
	  prieee8(tempda); 
	  *(double *)tempda = -(*(double *)tempda);
	  ieeepr8(tempda);
	  crs[FLTH] = tempda[0];
	  crs[FLTL] = tempda[1];
	  crs[FLTD] = tempda[2];
	  crs[FEXP] = tempda[3];
	  XEXPC(0);
	  continue;

	case 0141000:
	  TRACE(T_FLOW, " ADLL\n");
	  if (crs[KEYS] & 020000) {
	    crs[KEYS] &= ~0120300;                 /* clear C, L, LT, EQ */
	    utempll = *(unsigned int *)(crs+L);
	    utempll += 1;
	    *(unsigned int *)(crs+L) = utempll;
	    if (utempll & 0x100000000LL) /* set L-bit if carry */
	      crs[KEYS] |= 020000;  
	    if (*(int *)(crs+L) < 0)
	      if (*(int *)(crs+L) == 0x80000000)
		mathexception('i', FC_INT_OFLOW, 0);
	      else
		crs[KEYS] |= 0200;     /* set LT */
	    else if (*(int *)(crs+L) == 0)
	      crs[KEYS] |= 0100;   /* set EQ */
	  } else {
	    crs[KEYS] &= ~0120300;                 /* clear C, L, LT, EQ */
	    SETCC_L;
	  }
	  continue;

	case 0140530:
	  TRACE(T_FLOW, " FCM\n");
	  TRACE(T_INST, " FEXP=%d (dec), FLTH='%o, FLTL='%o\n", crs[FEXP], crs[FLTH], crs[FLTL]);
	  *(int *)&tempf = (crs[FLTH]<<16) | (crs[FLTL] & 0xFF00) | (crs[FEXP] & 0xFF);
	  prieee4(&tempf);
	  tempf = -tempf;
	  ieeepr4(&tempf);
	  crs[FLTH] = (*(unsigned int *)&tempf) >> 16;
	  crs[FLTL] = (*(unsigned int *)&tempf) & 0xFF00;
	  crs[FEXP] = (*(unsigned int *)&tempf) & 0xFF;
	  XEXPC(0);
	  continue;

	case 0140510:
	  TRACE(T_FLOW, " FSZE\n");
	  if (*(int *)(crs+FLTH) == 0)
	    RPL++;
	  continue;

	case 0140511:
	  TRACE(T_FLOW, " FSNZ\n");
	  if (*(int *)(crs+FLTH) != 0)
	    RPL++;
	  continue;

	case 0140512:
	  TRACE(T_FLOW, " FSMI\n");
	  if (*(int *)(crs+FLTH) < 0)
	    RPL++;
	  continue;

	case 0140513:
	  TRACE(T_FLOW, " FSPL\n");
	  if (*(int *)(crs+FLTH) >= 0)
	    RPL++;
	  continue;

	case 0140514:
	  TRACE(T_FLOW, " FSLE\n");
	  if (*(int *)(crs+FLTH) <= 0)
	    RPL++;
	  continue;

	case 0140515:
	  TRACE(T_FLOW, " FSGT\n");
	  if (*(int *)(crs+FLTH) > 0)
	    RPL++;
	  continue;

	case 0140554:
	  TRACE(T_FLOW, " INT\n");
	  tempda[0] = crs[FLTH];
	  tempda[1] = crs[FLTL];
	  tempda[2] = crs[FLTD];
	  tempda[3] = crs[FEXP];
	  prieee8(tempda);
	  TRACE(T_INST, " DFAC value=%f, FEXP=%d (dec), FLTH='%o, FLTL='%o, fltd='%o\n", *(double *)tempda, crs[FEXP], crs[FLTH], crs[FLTL], crs[FLTD]);
	  templ = *(double *)tempda;
	  TRACE(T_INST, " INT value=%d\n", templ);
	  crs[B] = templ & 0x7FFF;
	  crs[A] = templ >> 15;
	  if (*(double *)tempda > 1073741823.0 || *(double *)tempda < -1073741824.0)
	    mathexception('f', FC_DFP_OFLOW, ea);
	  else
	    CLEARC;
	  continue;

	case 0140531:
	  TRACE(T_FLOW, " INTA\n");
	  tempda[0] = crs[FLTH];
	  tempda[1] = crs[FLTL];
	  tempda[2] = crs[FLTD];
	  tempda[3] = crs[FEXP];
	  prieee8(tempda);
	  if (*(double *)tempda > 32767.0 || *(double *)tempda < -32768.0)
	    mathexception('f', FC_DFP_OFLOW, ea);
	  else
	    CLEARC;
	  *(short *)(crs+A) = *(double *)tempda;
	  continue;

	case 0140532:
	  TRACE(T_FLOW, " FLTA\n");
	  tempf = *(short *)(crs+A);
	  ieeepr4(&tempf);
	  crs[FLTH] = (*(unsigned int *)&tempf) >> 16;
	  crs[FLTL] = (*(unsigned int *)&tempf) & 0xFF00;
	  crs[FEXP] = (*(unsigned int *)&tempf) & 0xFF;
	  crs[FLTD] = 0;
	  continue;

	case 0140533:
	  TRACE(T_FLOW, " INTL\n");
	  tempda[0] = crs[FLTH];
	  tempda[1] = crs[FLTL];
	  tempda[2] = crs[FLTD];
	  tempda[3] = crs[FEXP];
	  prieee8(tempda);
	  if (*(double *)tempda > 2147483647.0 || *(double *)tempda < -2147483648.0)
	    mathexception('f', FC_DFP_OFLOW, ea);
	  else
	    CLEARC;
	  *(int *)(crs+L) = *(double *)tempda;
	  continue;

	case 0140535:
	  TRACE(T_FLOW, " FLTL\n");
	  tempf = *(int *)(crs+L);
	  ieeepr4(&tempf);
	  crs[FLTH] = (*(unsigned int *)&tempf) >> 16;
	  crs[FLTL] = (*(unsigned int *)&tempf) & 0xFF00;
	  crs[FEXP] = (*(unsigned int *)&tempf) & 0xFF;
	  crs[FLTD] = 0;
	  continue;

	case 0141711:
	  TRACE(T_FLOW, " BMLE\n");
	  if (!(crs[KEYS] & 020000))
	    RPL = iget16(RP);
	  else
	    goto bceq;
	  continue;

#if 0
	case 0141602:   /* same opcode as BCEQ */
	  TRACE(T_FLOW, " BMEQ\n");
	  goto bceq;

	case 0141603:   /* same opcode as BCNE */
	  TRACE(T_FLOW, " BMNE\n");
	  goto bcne;

	/* NOTE: BMGE is equivalent to BLS; this opcode doesn't exist
	   in newer manuals */

	case 0141606:
	  TRACE(T_FLOW, " BMGE\n");
	  goto bls;
#endif

	case 0141710:
	  TRACE(T_FLOW, " BMGT\n");
	  if (crs[KEYS] & 020000)
	    goto bcne;
	  RPL++;
	  continue;

	case 0141404:
	  TRACE(T_FLOW, " CRE\n");
	  *(int *)(crs+E) = 0;
	  continue;

	case 0141410:
	  TRACE(T_FLOW, " CRLE\n");
	  *(int *)(crs+L) = 0;
	  *(int *)(crs+E) = 0;
	  continue;

	case 0141414:
	  TRACE(T_FLOW, " ILE\n");
	  templ = *(int *)(crs+L);
	  *(int *)(crs+L) = *(int *)(crs+E);
	  *(int *)(crs+E) = templ;
	  continue;

	/* queue instructions

	   NOTE: ABQ is typically used in software to add an item to a
	   hardware (physical) queue and RTQ is used by DMQ hardware
	   to fetch items from the queue. */

	case 0141714:
	  TRACE(T_FLOW, " RTQ\n");
	  ea = apea(NULL);
	  if (rtq(ea,&utempa,RP)) {
	    crs[A] = utempa;
	    crs[KEYS] &= ~0100;
	  } else {
	    crs[A] = 0;
	    crs[KEYS] |= 0100;
	  }
	  continue;

	case 0141715:
	  TRACE(T_FLOW, " RBQ\n");
	  ea = apea(NULL);
	  qtop = get16(ea);
	  qbot = get16(ea+1);
	  if (qtop == qbot) {  /* queue empty */
	    crs[A] = 0;
	    crs[KEYS] |= 0100;
	  } else {
	    qseg = get16(ea+2) & 0x7FFF;
	    qmask = get16(ea+3);
	    qbot = (qbot & ~qmask) | ((qbot-1) & qmask);
	    qea = MAKEVA(qseg,qbot);
	    crs[A] = get16(qea);
	    put16(qbot, ea+1);
	    crs[KEYS] &= ~0100;
	  }
	  continue;

	case 0141716:
	  TRACE(T_FLOW, " ABQ\n");
	  ea = apea(NULL);
	  if (abq(ea, crs[A], RP))
	    crs[KEYS] &= ~0100;
	  else
	    crs[KEYS] |= 0100;
	  continue;

	case 0141717:
	  TRACE(T_FLOW, " ATQ\n");
	  ea = apea(NULL);
	  qtop = get16(ea);
	  qbot = get16(ea+1);
	  qseg = get16(ea+2) & 0x7FFF;
	  qmask = get16(ea+3);
	  qtemp = (qtop & ~qmask) | ((qtop-1) & qmask);
	  if (qtemp == qbot) {  /* queue full */
	    crs[KEYS] |= 0100;
	  } else {
	    qea = MAKEVA(qseg,qtemp);
	    put16(crs[A],qea);
	    put16(qtemp, ea);
	    crs[KEYS] &= ~0100;
	  }
	  continue;

	case 0141757:
	  TRACE(T_FLOW, " TSTQ\n");
	  ea = apea(NULL);
	  qtop = get16(ea);
	  qbot = get16(ea+1);
	  qmask = get16(ea+3);
	  crs[A] = (qbot-qtop) & qmask;
	  SETCC_A;
	  continue;

	default:
	  TRACE(T_INST, " unrecognized generic class 3 instruction!\n");
	  printf(" unrecognized generic class 3 instruction %o!\n", inst);

	  /* XXX: these are hacks for CPU.FAULT; not sure how to determine whether
	     an instruction is illegal or unimplemented... */
	  if (inst == 0141700)
	    fault(ILLINSTFAULT, RPL, 0);
	  else
	    fault(UIIFAULT, RPL, 0);
	  fatal(NULL);
	}	
      }


      if (class == 1) {
	TRACE(T_INST, " shift group\n");
	scount = -inst & 077;
	if (scount == 0)
	  scount = 0100;
	switch (inst & 01700) {

	case 00000: /* LRL */
	  TRACE(T_FLOW, " LRL %d\n", scount);
	  crs[KEYS] &= ~0120000;             /* clear C,L */
	  if (scount <= 32) {
	    utempl = *(unsigned int *)(crs+L);
	    EXPCL(utempl & bitmask32[33-scount]);
	    *(unsigned int *)(crs+L) = utempl >> scount;
	  } else {
	    *(unsigned int *)(crs+L) = 0;
	  }
	  break;

	case 00100: /* LRS (different in R & V modes) */
	  TRACE(T_FLOW, " LRS %d\n", scount);
	  crs[KEYS] &= ~0120000;             /* clear C,L */
	  if (crs[KEYS] & 010000) {          /* V/I mode */
	    if (scount <= 32) {
	      templ = *(int *)(crs+L);
	      EXPCL(templ & bitmask32[33-scount]);
	      templ = templ >> scount;
	      *(int *)(crs+L) = templ;
	    } else if (crs[A] & 0x8000) {
	      *(int *)(crs+L) = 0xFFFFFFFF;
	      SETCL;
	    } else {
	      *(int *)(crs+L) = 0;
	    }
	  } else {
	    utempa = crs[B] & 0x8000;        /* save B bit 1 */
	    if (scount <= 31) {
	      templ = (crs[A]<<16) | ((crs[B] & 0x7FFF)<<1);
	      EXPCL(templ & bitmask32[32-scount]);
	      templ = templ >> (scount+1);
	      crs[A] = templ >> 15;
	      crs[B] = (templ & 0x7FFF) | utempa;
	    } else if (crs[A] & 0x8000) {
	      *(int *)(crs+A) = 0xFFFF7FFF | utempa;
	      SETCL;
	    } else {
	      *(int *)(crs+A) = utempa;
	    }
	  }
	  break;

	case 00200: /* LRR */
	  TRACE(T_FLOW, " LRR %d\n", scount);
	  crs[KEYS] &= ~0120000;              /* clear C,L */
	  if (scount > 32)
	    scount = scount - 32;
	  utempl = *(unsigned int *)(crs+L);
	  EXPCL(utempl & bitmask32[33-scount]);
	  utempl = (utempl >> scount) | (utempl << (32-scount));
	  *(unsigned int *)(crs+L) = utempl;
	  break;

	case 00300: 
	  if (inst == 040310) {
	    printf("SSSN @ %o/%o\n", RPH, RPL);
	    TRACE(T_FLOW, " SSSN\n", inst);
	    fault(UIIFAULT, RPL, RP);
	    break;
	  }
	  goto badshift;

	case 00400: /* ARL */
	  TRACE(T_FLOW, " ARL %d\n", scount);
	  crs[KEYS] &= ~0120000;              /* clear C,L */
	  if (scount <= 16) {
	    EXPCL(crs[A] & bitmask16[17-scount]);
	    crs[A] = crs[A] >> scount;
	  } else {
	    crs[A] = 0;
	  }
	  break;

	case 00500: /* ARS */
	  TRACE(T_FLOW, " ARS %d\n", scount);
	  crs[KEYS] &= ~0120000;              /* clear C,L */
	  if (scount <= 16) {
	    tempa = *(short *)(crs+A);
	    EXPCL(tempa & bitmask16[17-scount]);
	    tempa = tempa >> scount;
	    *(short *)(crs+A) = tempa;
	  } else if (crs[A] & 0x8000) {
	    *(short *)(crs+A) = 0xFFFF;
	    SETCL;
	  } else {
	    *(short *)(crs+A) = 0;
	  }
	  break;

	case 00600: /* ARR */
	  TRACE(T_FLOW, " ARR %d\n", scount);
	  crs[KEYS] &= ~0120000;              /* clear C,L */
	  scount = ((scount-1)%16)+1;   /* make scount 1-16 */
	  EXPCL(crs[A] & bitmask16[17-scount]);
	  crs[A] = (crs[A] >> scount) | (crs[A] << (16-scount));
	  break;

	case 01000: /* LLL */
	  TRACE(T_FLOW, " LLL %d\n", scount);
	  crs[KEYS] &= ~0120000;              /* clear C,L */
	  if (scount <= 32) {
	    utempl = *(unsigned int *)(crs+A);
	    EXPCL(utempl & bitmask32[scount]);
	    utempl = utempl << scount;
	    *(unsigned int *)(crs+A) = utempl;
	  } else {
	    *(unsigned int *)(crs+A) = 0;
	  }
	  break;

	case 01100: /* LLS (different in R/V modes) */
	  TRACE(T_FLOW, " LLS %d\n", scount);
	  crs[KEYS] &= ~0120000;              /* clear C,L */
	  if (crs[KEYS] & 010000) {          /* V/I mode */
	    if (scount < 32) {
	      templ = 0x80000000;
	      templ = templ >> scount;         /* create mask */
	      templ = templ & *(int *)(crs+A); /* grab bits */
	      templ = templ >> (31-scount);    /* extend them */
	      EXPCL(!(templ == -1 || templ == 0));
	      *(int *)(crs+A) = *(int *)(crs+A) << scount;
	    } else {
	      EXPCL(*(int *)(crs+A) != 0);
	      *(int *)(crs+A) = 0;
	    }
	  } else {
	    utempa = crs[B] & 0x8000;            /* save B bit 1 */
	    if (scount < 31) {
	      utempl = (crs[A]<<16) | ((crs[B] & 0x7FFF)<<1);
	      templ2 = 0x80000000;
	      templ2 = templ2 >> scount;         /* create mask */
	      templ2 = templ2 & utempl;          /* grab bits */
	      templ2 = templ2 >> (31-scount);    /* sign extend them */
	      EXPCL(!(templ2 == -1 || templ2 == 0));
	      //printf(" before: A=%x, B=%x, utempl=%x, ", crs[A], crs[B], utempl);
	      utempl = utempl << scount;
	      crs[A] = utempl >> 16;
	      crs[B] = ((utempl >> 1) & 0x7FFF) | utempa;
	      //printf(" after: A=%x, B=%x, utempl=%x\n", crs[A], crs[B], utempl);
	    } else {
	      EXPCL(*(unsigned int *)(crs+A) != 0);
	      *(unsigned int *)(crs+A) = utempa;
	    }
	  }
	  if (crs[KEYS] & 0100000)
	    mathexception('i', FC_INT_OFLOW, 0);
	  break;

	case 01200: /* LLR */
	  TRACE(T_FLOW, " LLR %d\n", scount);
	  crs[KEYS] &= ~0120000;              /* clear C,L */
	  if (scount > 32)
	    scount = scount - 32;
	  utempl = *(unsigned int *)(crs+A);
	  EXPCL(utempl & bitmask32[scount]);
	  utempl = (utempl << scount) | (utempl >> (32-scount));
	  *(unsigned int *)(crs+A) = utempl;
	  break;

	case 01400: /* ALL */
	  TRACE(T_FLOW, " ALL %d\n", scount);
	  crs[KEYS] &= ~0120000;              /* clear C,L */
	  if (scount <= 16) {
	    EXPCL(crs[A] & bitmask16[scount]);
	    crs[A] = crs[A] << scount;
	  } else {
	    crs[A] = 0;
	  }
	  break;

	case 01500: /* ALS */
	  TRACE(T_FLOW, " ALS %d\n", scount);
	  crs[KEYS] &= ~0120000;              /* clear C,L */
	  if (scount <= 15) {
	    tempa = 0100000;
	    tempa = tempa >> scount;         /* create mask */
	    tempa = tempa & crs[A];          /* grab bits */
	    tempa = tempa >> (15-scount);    /* extend them */
	    crs[A] = crs[A] << scount;
	    EXPCL(!(tempa == -1 || tempa == 0));
	  } else if (crs[A] != 0) {
	    crs[A] = 0;
	    SETCL;
	  }
	  if (crs[KEYS] & 0100000)
	    mathexception('i', FC_INT_OFLOW, 0);
	  break;

	case 01600: /* ALR */
	  TRACE(T_FLOW, " ALR %d\n", scount);
	  crs[KEYS] &= ~0120000;              /* clear C,L */
	  scount = ((scount-1)%16)+1;   /* make scount 1-16 */
	  EXPCL(crs[A] & bitmask16[scount]);
	  crs[A] = (crs[A] << scount) | (crs[A] >> (16-scount));
	  break;

	default:
badshift:
	  printf("emulator warning: unrecognized shift instruction %o at %o/%o\n", inst, RPH, RPL);
	  TRACE(T_INST, " unrecognized shift instruction!: %o\n", inst);
	}
	continue;
      }

      if (class == 2) {
	TRACE(T_INST, " skip group\n");

	if (inst == 0101000) {
	  TRACE(T_FLOW, " NOP\n");
	  continue;
	}

	if (inst == 0100000) {
	  TRACE(T_FLOW, " SKP\n");
	  RPL++;
	  continue;
	}

	if (inst == 0101400) {
	  TRACE(T_FLOW, " SMI/SLT\n");
	  if (*(short *)(crs+A) < 0)
	    RPL++;
	  continue;
	}

	if (inst == 0100400) {
	  TRACE(T_FLOW, " SPL/SGE\n");
	  if (*(short *)(crs+A) >= 0)
	    RPL++;
	  continue;
	}

	if (inst == 0101100) {
	  TRACE(T_FLOW, " SLN\n");
	  if (crs[A] & 1)
	    RPL++;
	  continue;
	}

	if (inst == 0100100) {
	  TRACE(T_FLOW, " SLZ\n");
	  if (!(crs[A] & 1))
	    RPL++;
	  continue;
	}

	if (inst == 0101040) {
	  TRACE(T_FLOW, " SNZ/SNE\n");
	  if (crs[A] != 0)
	    RPL++;
	  continue;
	}

	if (inst == 0100040) {
	  TRACE(T_FLOW, " SZE/SEQ\n");
	  if (crs[A] == 0)
	    RPL++;
	  continue;
	}

	if (inst == 0101220) {
	  TRACE(T_FLOW, " SLE\n");
	  if (*(short *)(crs+A) <= 0)
	    RPL++;
	  continue;
	}

	if (inst == 0100220) {
	  TRACE(T_FLOW, " SGT\n");
	  if (*(short *)(crs+A) > 0)
	    RPL++;
	  continue;
	}

	if (inst == 0101001) {
	  TRACE(T_FLOW, " SSC\n");
	  if (crs[KEYS] & 0100000)
	    RPL++;
	  continue;
	}

	if (inst == 0100001) {
	  TRACE(T_FLOW, " SRC\n");
	  if (!(crs[KEYS] & 0100000))
	    RPL++;
	  continue;
	}

	if ((inst & 0177760) == 0100260) {
	  m = (inst & 017)+1;
	  TRACE(T_FLOW, " SAR %d\n", m);
	  if (!(crs[A] & bitmask16[m]))
	    RPL++;
	  continue;
	}

	if ((inst & 0177760) == 0101260) {
	  m = (inst & 017)+1;
	  TRACE(T_FLOW, " SAS %d\n", m);
	  if (crs[A] & bitmask16[m])
	    RPL++;
	  continue;
	}

	if ((inst & 0177760) == 0100240) {
	  m = (inst & 017)+1;
	  TRACE(T_FLOW, " SNR %d\n", m);
	  RESTRICT();
	  if (!(sswitch & bitmask16[m]))
	    RPL++;
	  continue;
	}

	if ((inst & 0177760) == 0101240) {
	  m = (inst & 017)+1;
	  TRACE(T_FLOW, " SNS %d\n", m);
	  RESTRICT();
	  if (sswitch & bitmask16[m])
	    RPL++;
	  continue;
	}

	if (inst == 0100200) {    /* skip if machine check flop is reset */
	  TRACE(T_FLOW, " SMCR\n");
	  RESTRICT();
	  RPL++;
	  continue;
	}

	if (inst == 0101200) {    /* skip if machine check flop is set */
	  TRACE(T_FLOW, " SMCS\n");
	  RESTRICT();
	  continue;
	}

#if 0
	/* NOTE: this is the clock display instruction skip, but appears
	   goofy to me, and looks more like an I/O instruction:

 unrecognized skip instruction 101704 at 6/3174
Fatal error: instruction #106825755 at 6/3173: 101704 677
keys = 14200, modals=100177

	*/
#endif

	if (inst == 0101704) {    /* skip if machine check flop is set */
	  TRACE(T_FLOW, " clock SKP?\n");
	  RPL++;
	  continue;
	}

	printf(" unrecognized skip instruction %o at %o/%o\n", inst, RPH, RPL);
	fatal(NULL);

      } else
	fatal("Coding error: bad generic class");
    }

    /* here for non-generic instructions: memory references or pio */
    /* pio can only occur in S/R modes */

    if (!(crs[KEYS] & 010000) && (inst & 036000) == 030000) {
      pio(inst);
      continue;
    }

    /* get x bit and adjust opcode so that PMA manual opcode
       references can be used directly, ie, if the PMA manual says the
       opcode is '15 02, then 01502 can be used here.  If the PMA
       manual says the opcode is '11, then use 01100 (the XX extended
       opcode bits are zero) */

    i = inst & 0100000;           /* (for trace) indirect is bit 1 */
    x = inst & 040000;            /* indexed is bit 2 */
    opcode = (inst & 036000) >> 4;  /* isolate opcode bits */

    /* fix ldx/stx (opcode '15): these instructions cannot be indexed
       so if an instruction specifies indexing by X, it acts like an
       opcode extension.  Opcodes listed as '35 02 for example (sty in
       V-mode, jdx in R-mode) have X=1 with the 4 opcode bits 1101
       ('15)

         x=0, opcode='15 -> stx (SRV)
         x=1, opcode='15 -> ldx (SRV) (aka '35)

	 x=0, opcode='15 01 -> flx (RV)
	 x=1, opcode='15 01 -> ldy (V) (aka '35 01)

	 x=0, opcode='15 02 -> dflx (V)
	 x=0, opcode='15 02 -> jdx (R)
         x=1, opcode='15 02 -> sty (V) (aka '35 02)

	 x=0, opcode='15 03 -> jix (R)
	 x=1, opcode='15 03 -> jsx (RV) (aka '35 03)
    */
    
    if (opcode == 01500) {
      opcode = opcode | ((inst & 040000)>>4);   /* if X set, expand opcode */
      x = 0;                        /* clear X bit (these can't be indexed) */
      TRACE(T_INST, " ldx/stx opcode adjusted\n");
    }

    TRACE(T_INST, " opcode=%5#0o, i=%o, x=%o\n", opcode, i != 0, x != 0);

    switch (crs[KEYS] & 016000) {
    case 0:  /* 16S */
      ea = ea16s(inst, x);
      break;
    case 1<<10:  /* 32S */
      ea = ea32s(inst, x);
      break;
    case 2<<10:  /* 64R */
    case 3<<10:  /* 32R */
      ea = ea32r64r(earp, inst, x, &opcode);
      break;
    case 4<<10:  /* 32I */
      ea = ea32i(earp, inst, x);
      warn("32I mode not supported");
      fault(RESTRICTFAULT, 0, 0);
      break;
    case 6<<10:  /* 64V */
      ea = ea64v(earp, inst, x, &opcode, &eabit);
      break;
    default:
      printf("Bad CPU mode in EA calculation, keys = %o\n", crs[KEYS]);
      fatal(NULL);
    }

    TRACE(T_INST, " EA: %o/%o  %s\n",ea>>16, ea & 0xFFFF, searchloadmap(ea,' '));


    /* NOTE: basic and dbasic execute instructions from the register file 
       with TRACE ON */

    switch (opcode) {

    case 00100:
      TRACE(T_FLOW, " JMP\n");
      RP = ea;
      continue;

    /* NOTE: don't use get32 for DLD/DST, because it doesn't handle register
       address traps */

    case 00200:
      crs[A] = get16(ea);
      if ((crs[KEYS] & 050000) == 040000) {  /* R-mode and DP */
	TRACE(T_FLOW, " DLD\n");
	crs[B] = get16(INCVA(ea,1));
      } else {
	TRACE(T_FLOW, " LDA ='%o/%d\n", crs[A], *(short *)(crs+A));
      }
      continue;

    case 00400:
      put16(crs[A],ea);
      if ((crs[KEYS] & 050000) == 040000) {
	TRACE(T_FLOW, " DST\n");
	put16(crs[B],INCVA(ea,1));
      } else {
	TRACE(T_FLOW, " STA\n");
      }
      continue;

    /* NOTE: EQ and LT can be set in the same instruction if overflow
       occurs, for example, '100000+'100000 */

    case 00600:
      crs[KEYS] &= ~0120300;                 /* clear C, L, LT, EQ */
      utempa = crs[A];
      m = get16(ea);
      if ((crs[KEYS] & 050000) != 040000) {     /* V/I mode or SP */
	TRACE(T_FLOW, " ADD ='%o/%d\n", m, *(short *)&m);
	utempl = crs[A];
	utempl += m;
	crs[A] = utempl;
	if (utempl & 0x10000)                  /* set L-bit if carry */
	  crs[KEYS] |= 020000;  
	if (crs[A] == 0)                       /* set EQ? */
	  crs[KEYS] |= 0100; 
	if (((~utempa ^ m) & (utempa ^ crs[A])) & 0x8000) {
	  if (*(short *)(crs+A) >= 0)
	    crs[KEYS] |= 0200;
	  mathexception('i', FC_INT_OFLOW, 0);
	} else if (*(short *)(crs+A) < 0)
	  crs[KEYS] |= 0200;
      } else {                                  /* R-mode and DP */
	TRACE(T_FLOW, " DAD\n");
	crs[B] += get16(INCVA(ea,1));
	utempl = crs[A];
	if (crs[B] & 0x8000) {
	  utempl++;
	  crs[B] &= 0x7fff;
	}
	utempl += m;
	crs[A] = utempl;
	if (utempl & 0x10000)                  /* set L-bit if carry */
	  crs[KEYS] |= 020000;  
	/* NOTE: this EQ test prevents reusing the ADD code :( */
	if (*(int *)(crs+L) == 0)              /* set EQ? */
	  crs[KEYS] |= 0100; 
	if (((~utempa ^ m) & (utempa ^ crs[A])) & 0x8000) {
	  if (*(int *)(crs+L) >= 0)
	    crs[KEYS] |= 0200;
	  mathexception('i', FC_INT_OFLOW, 0);
	} else if (*(int *)(crs+L) < 0)
	  crs[KEYS] |= 0200;
      }
      continue;

    case 00700:
      crs[KEYS] &= ~0120300;   /* clear C, L, and CC */
      utempa = crs[A];
      m = get16(ea);
      if ((crs[KEYS] & 050000) != 040000) {
	TRACE(T_FLOW, " SUB ='%o/%d\n", m, *(short *)&m);
	utempl = crs[A];
	utempl += (unsigned short) ~m;
	utempl += 1;
	crs[A] = utempl;                       /* truncate results */
	if (utempl & 0x10000)                  /* set L-bit if carry */
	  crs[KEYS] |= 020000;  
	if (crs[A] == 0)                       /* set EQ? */
	  crs[KEYS] |= 0100; 
	if (((utempa ^ m) & (utempa ^ crs[A])) & 0x8000) {
	  if (*(short *)(crs+A) >= 0)
	    crs[KEYS] |= 0200;
	  mathexception('i', FC_INT_OFLOW, 0);
	} else if (*(short *)(crs+A) < 0)
	  crs[KEYS] |= 0200;
      } else {
	TRACE(T_FLOW, " DSB\n");
	crs[B] -= get16(INCVA(ea,1));
	utempl = crs[A];
	if (crs[B] & 0x8000) {
	  utempl += 0xFFFF;
	  crs[B] &= 0x7fff;
	}
	utempl += (unsigned short) ~m;
	utempl += 1;
	crs[A] = utempl;                       /* truncate results */
	if (utempl & 0x10000)                  /* set L-bit if carry */
	  crs[KEYS] |= 020000;  
	if (*(int *)(crs+L) == 0)              /* set EQ? */
	  crs[KEYS] |= 0100; 
	if (((utempa ^ m) & (utempa ^ crs[A])) & 0x8000) {
	  if (*(int *)(crs+L) >= 0)
	    crs[KEYS] |= 0200;
	  mathexception('i', FC_INT_OFLOW, 0);
	} else if (*(int *)(crs+L) < 0)
	  crs[KEYS] |= 0200;
      }
      continue;

    case 00300:
      m = get16(ea);
      TRACE(T_FLOW, " ANA ='%o\n",m);
      crs[A] &= m;
      continue;

    case 00500:
      m = get16(ea);
      TRACE(T_FLOW, " ERA ='%o\n", m);
      crs[A] ^= m;
      continue;

    case 00302:
      m = get16(ea);
      TRACE(T_FLOW, " ORA ='%o\n", m);
      crs[A] |= m;
      continue;

    case 01000:
      TRACE(T_FLOW, " JST\n");

      /* NOTE: amask should be recomputed here if in R/S mode, so it
	 can be removed as a global variable.  Flaky errors occur if
	 keys are changed w/o calling newkeys(), because amask would
	 be wrong (see dispatcher comment) */

      if (amask == 0177777)
	m = RPL;
      else
	m = (get16(ea) & ~amask) | RPL;
      put16(m, ea);
      RP = INCVA(ea,1);
      if ((RP & RINGMASK32) == 0)
	inhcount = 1;
      continue;

    case 01100:
      m = get16(ea);
      TRACE(T_FLOW, " CAS ='%o/%d\n", m, *(short *)&m);
#if 1
      crs[KEYS] &= ~020300;   /* clear L, and CC */
      utempa = crs[A];
      utempl = crs[A];
      utempl += (unsigned short) ~m;
      utempl += 1;
      crs[A] = utempl;                       /* truncate results */
      if (utempl & 0x10000)                  /* set L-bit if carry */
	crs[KEYS] |= 020000;  
      if (crs[A] == 0)                       /* set EQ? */
	crs[KEYS] |= 0100; 
      if (((utempa ^ m) & (utempa ^ crs[A])) & 0x8000) {
	if (*(short *)(crs+A) >= 0)
	  crs[KEYS] |= 0200;
      } else if (*(short *)(crs+A) < 0)
	crs[KEYS] |= 0200;
      crs[A] = utempa;                       /* restore A reg */
      if (crs[A] == m)
	RPL++;
      else if (*(short *)(crs+A) < *(short *)&m)
	RPL += 2;
#else
      crs[KEYS] &= ~0300;
      if (crs[A] == m) {
	RPL++;
	crs[KEYS] |= 0100;
      } else if (*(short *)(crs+A) < *(short *)&m) {
	RPL += 2;
	crs[KEYS] |= 0200;
      }
      XSETL(0);
#endif
      continue;

    case 01200:
      TRACE(T_FLOW, " IRS\n");
      m = get16(ea) + 1;
      put16(m,ea);
      if (m == 0)
	RPL++;
      continue;

    case 01300:
      TRACE(T_FLOW, " IMA\n");
      m = get16(ea);
      put16(crs[A],ea);
      crs[A] = m;
      continue;

    case 01400:
      TRACE(T_FLOW, " JSY\n");
      crs[Y] = RPL;
      RP = ea;
      continue;

    case 01402:
      TRACE(T_FLOW, " JSXB\n");
      *(unsigned int *)(crs+XB) = RP;
      RP = ea;
      continue;

    case 01500:
      TRACE(T_FLOW, " STX\n");
      put16(crs[X],ea);
      continue;

    /* MPY can't overflow in V-mode, but in R-mode (31 bits),
       -32768*-32768 can overflow and yields 0x8000/0x0000 */

    case 01600:
      m = get16(ea);
      TRACE(T_FLOW, " MPY ='%o/%d\n", m, *(short *)&m);
      templ = *(short *)(crs+A) * *(short *)&m;
      CLEARC;
      if (crs[KEYS] & 010000) {          /* V/I mode */
	*(int *)(crs+L) = templ;
      } else {                           /* R/S mode */
	utempa = crs[A];
	crs[A] = (templ >> 15);
	crs[B] = templ & 077777;
	if (utempa == 0x8000 && m == 0x8000)
	  mathexception('i', FC_INT_OFLOW, 0);
      }
      continue;

    case 01603:
      templ = get32(ea);
      TRACE(T_FLOW, " MPL ='%o/%d\n", templ, *(int *)&templ);
      templl = (long long)(*(int *)(crs+L)) * (long long)templ;
      *(long long *)(crs+L) = templl;
      CLEARC;
      continue;

    case 01700:
      m = get16(ea);
      //printf("L='%o/%d, DIV ='%o/%d\n", *(unsigned int *)(crs+L), *(int *)(crs+L), m, *(short *)&m);
      TRACE(T_FLOW, " DIV ='%o/%d\n", m, *(short *)&m);
      if (crs[KEYS] & 010000) {          /* V/I mode */
	templ = *(int *)(crs+A);
      } else {                      /* R/S mode */
	templ = *(short *)(crs+A);  /* convert to 32-bit signed */
	templ = (templ<<15) | (crs[B] & 0x7FFF);
	//printf("\nR-mode DIV: A='%o/%d, B='%o/%d, templ='%o/%d\n", crs[A], *(short *)(crs+A), crs[B], *(short *)(crs+B), templ, templ);
      }
      if (m == 0)
	mathexception('i', FC_INT_ZDIV, 0);
      else if (abs(*(short *)(crs+A)) < abs(*(short *)&m)) {
	templ2 = templ / *(short *)&m;
	crs[A] = templ2;
	crs[B] = templ % *(short *)&m;
	//printf("DIV results: m='%o/%d, A='%o/%d, B='%o/%d\n", m, *(short *)&m, crs[A], *(short *)(crs+A), crs[B], *(short *)(crs+B));
	if ((crs[A] & 0x8000) ^ (templ2 & 0x80000000)>>16)
	  mathexception('i', FC_INT_OFLOW, 0);
	else
	  CLEARC;
      } else {
	mathexception('i', FC_INT_OFLOW, 0);
	//printf("DIV overflow\n");
      }
      continue;

    /* NOTE:  RESET QVFY, DVL runs okay with cpuid=5 (P750), but
       fails with default cpuid (P4450) */

    case 01703:
      templ = get32(ea);
      TRACE(T_FLOW, " DVL ='%o/%d\n", templ, *(int *)&templ);
      templl = *(long long *)(crs+L);
      if (templ != 0) {
	*(int *)(crs+L) = templl / templ;
	*(int *)(crs+E) = templl % templ;
	CLEARC;
      } else
	mathexception('i', FC_INT_ZDIV, 0);
      continue;

    case 03500:
      TRACE(T_FLOW, " LDX\n");
      crs[X] = get16(ea);
      continue;

    case 00101:
      if (crs[KEYS] & 010000) {          /* V/I mode */
	TRACE(T_FLOW, " EAL\n");
	*(ea_t *)(crs+L) = ea;
      } else {
	TRACE(T_FLOW, " EAA\n");
	crs[A] = ea;
      }
      continue;

    case 00203:
      if (crs[KEYS] & 010000) {          /* V/I mode */
	*(unsigned int *)(crs+L) = get32(ea);
	TRACE(T_FLOW, " LDL ='%o/%d\n", *(unsigned int *)(crs+A), *(int *)(crs+A));
      } else {
	TRACE(T_FLOW, " JEQ\n");
	if (*(short *)(crs+A) == 0)
	  RPL = ea;
      }
      continue;

    case 00703:
      if (crs[KEYS] & 010000) {          /* V/I mode */
	utempl2 = get32(ea);
	TRACE(T_FLOW, " SBL ='%o/%d\n", utempl2, *(int *)&utempl2);
	crs[KEYS] &= ~0120300;                 /* clear C, L, LT, EQ */
	utempl = *(unsigned int *)(crs+L);     /* save orig L for sign check */
	utempll = utempl;                      /* use bigger register */
	utempll += (unsigned int) ~utempl2;    /* do subtract "by hand" */
	utempll += 1;
	*(unsigned int *)(crs+L) = utempll;    /* truncate results */
	if (utempll & 0x100000000LL)           /* set L-bit if carry */
	  crs[KEYS] |= 020000;  
	if (*(unsigned int *)(crs+L) == 0)     /* set EQ? */
	  crs[KEYS] |= 0100; 
	if ((utempl ^ utempl2) & (utempl ^ *(int *)(crs+L)) & 0x80000000) {
	  if (*(int *)(crs+L) >= 0)
	    crs[KEYS] |= 0200;
	  mathexception('i', FC_INT_OFLOW, 0);
	} else if (*(int *)(crs+L) < 0)
	  crs[KEYS] |= 0200;
      } else {
	TRACE(T_FLOW, " JGE\n");
	if (*(short *)(crs+A) >= 0)
	  RPL = ea;
      }
      continue;

    case 01002:
      if (crs[KEYS] & 010000) {          /* V/I mode */
	//traceflags = ~TB_MAP;
	//TRACE(T_FLOW|T_PCL, "#%d %o/%o: PCL %o/%o\n", instcount, RPH, RPL-2, ea>>16, ea&0xFFFF);
	if (numtraceprocs > 0 && TRACEUSER)
	  for (i=0; i<numtraceprocs; i++)
	    if (traceprocs[i].ecb == (ea & 0xFFFFFFF) && traceprocs[i].sb == -1) {
	      traceflags = ~TB_MAP;
	      savetraceflags = traceflags;
	      traceprocs[i].sb = *(int *)(crs+SB);
	      printf("Enabled trace for %s at sb '%o/%o\n", traceprocs[i].name, crs[SBH], crs[SBL]);
	      break;
	    }
	TRACE(T_FLOW|T_PCL, " PCL %s\n", searchloadmap(ea, 'e'));
	pcl(ea);
      } else {
	TRACE(T_FLOW, " CREP\n");
	put16(RPL,crs[S]++);
	RPL = ea;
      }
      continue;

    case 00503:
      if (crs[KEYS] & 010000) {          /* V/I mode */
	utempl = get32(ea);
	TRACE(T_FLOW, " ERL ='%o/%d  '%o/'%o  %d/%d\n", utempl, *(int *)&utempl, utempl>>16, utempl&0xFFFF, utempl>>16, utempl&0xFFFF);
	*(unsigned int *)(crs+L) ^= utempl;
      } else {
	TRACE(T_FLOW, " JGT\n");
	if (*(short *)(crs+A) > 0)
	  RPL = ea;
      }
      continue;

    case 00403:
      if (crs[KEYS] & 010000) {          /* V/I mode */
	TRACE(T_FLOW, " STL\n");
	put32(*(unsigned int *)(crs+L),ea);
      } else {
	TRACE(T_FLOW, " JLE\n");
	if (*(short *)(crs+A) <= 0)
	  RPL = ea;
      }
      continue;

    case 00603:
      if (crs[KEYS] & 010000) {          /* V/I mode */
	utempl2 = get32(ea);
	TRACE(T_FLOW, " ADL ='%o/%d\n", utempl2, *(int *)&utempl2);
	crs[KEYS] &= ~0120300;           /* clear C, L, LT, EQ */
	utempl = *(unsigned int *)(crs+L);     /* save orig L for sign check */
	utempll = utempl;                      /* expand to 64 bits */
	utempll += utempl2;                    /* 64-bit add */
	*(unsigned int *)(crs+L) = utempll;    /* truncate results */
	if (utempll & 0x100000000LL)           /* set L-bit if carry */
	  crs[KEYS] |= 020000;  
	if (*(unsigned int *)(crs+L) == 0)     /* set EQ? */
	  crs[KEYS] |= 0100; 
	if ((~utempl ^ utempl2) & (utempl ^ *(int *)(crs+L)) & 0x80000000) {
	  if (*(int *)(crs+L) >= 0)
	    crs[KEYS] |= 0200;
	  mathexception('i', FC_INT_OFLOW, 0);
	} else if (*(int *)(crs+L) < 0)
	  crs[KEYS] |= 0200;
      } else {
	TRACE(T_FLOW, " JLT\n");
	if (*(short *)(crs+A) < 0)
	  RPL = ea;
      }
      continue;

    case 00303:
      if (crs[KEYS] & 010000) {          /* V/I mode */
	utempl = get32(ea);
	TRACE(T_FLOW, " ANL ='%o\n", utempl);
	*(unsigned int *)(crs+L) &= utempl;
      } else {
	TRACE(T_FLOW, " JNE\n");
	if (*(short *)(crs+A) != 0)
	  RPL = ea;
      }
      continue;

    case 01202:
      TRACE(T_FLOW, " EAXB\n");
      *(ea_t *)(crs+XB) = ea;
      continue;

    case 01502:
      if (crs[KEYS] & 010000) {          /* V/I mode */
	TRACE(T_FLOW, " DFLX\n");
	crs[X] = get16(ea) * 4;
      } else {
	TRACE(T_FLOW, " JDX\n");
	crs[X]--;
	if (crs[X] != 0)
	  RPL = ea;
      }
      continue;

    case 03502:
      TRACE(T_FLOW, " STY\n");
      put16(crs[Y],ea);
      continue;

    case 01503:
      if (crs[KEYS] & 010000) {          /* V/I mode */
	TRACE(T_FLOW, " QFLX\n");
	crs[X] = get16(ea) * 8;
      } else {
	TRACE(T_FLOW, " JIX\n");
	crs[X]++;
	if (crs[X] != 0)
	  RPL = ea;
      }
      continue;

    case 01501:
      TRACE(T_FLOW, " FLX\n");
      crs[X] = get16(ea) * 2;
      continue;

    case 03501:
      TRACE(T_FLOW, " LDY\n");
      crs[Y] = get16(ea);
      continue;

    case 03503:
      TRACE(T_FLOW, " JSX\n");
      crs[X] = RPL;
      RP = ea;
      continue;

    /* XXX: this should set the L bit like subtract */

    case 01103:
      TRACE(T_FLOW, " CLS\n");
      templ = get32(ea);
      crs[KEYS] &= ~0300;
      if (*(int *)(crs+L) == templ) {
	RPL++;
	crs[KEYS] |= 0100;
      } else if (*(int *)(crs+L) < templ) {
	RPL += 2;
	crs[KEYS] |= 0200;
      }
      XSETL(0);
      continue;

    case 00601:
      TRACE(T_FLOW, " FAD\n");
      *(int *)&tempf = (crs[FLTH]<<16) | (crs[FLTL] & 0xFF00) | (crs[FEXP] & 0xFF);
      prieee4(&tempf);
      TRACE(T_INST, " FAC value=%f, FEXP=%d (dec), FLTH='%o, FLTL='%o\n", tempf, crs[FEXP], crs[FLTH], crs[FLTL]);
      *(int *)&tempf1 = get32(ea);
      prieee4(&tempf1);
      TRACE(T_INST, " ea value=%f, EXP=%d (dec), ea H='%o, ea L='%o\n", tempf1, get16(ea+1) & 0xFF, get16(ea), get16(ea+1) & 0xFF00);
      tempf += tempf1;
      tempf1 = tempf;
      ieeepr4(&tempf);
      crs[FLTH] = (*(unsigned int *)&tempf) >> 16;
      crs[FLTL] = (*(unsigned int *)&tempf) & 0xFF00;
      crs[FEXP] = (*(unsigned int *)&tempf) & 0xFF;
      TRACE(T_INST, " FAC value=%f, FEXP=%d (dec), FLTH='%o, FLTL='%o\n", tempf1, crs[FEXP], crs[FLTH], crs[FLTL]);
      XEXPC(0);
      continue;

    /* this is implemented as a subtract on some models */

    case 01101:
      TRACE(T_FLOW, " FCS\n");
      *(int *)&tempf = (crs[FLTH]<<16) | (crs[FLTL] & 0xFF00) | (crs[FEXP] & 0xFF);
      prieee4(&tempf);
      TRACE(T_INST, " FAC value=%f, FEXP=%d (dec), FLTH='%o, FLTL='%o\n", tempf, crs[FEXP], crs[FLTH], crs[FLTL]);
      *(int *)&tempf1 = get32(ea);
      prieee4(&tempf1);
      TRACE(T_INST, " ea value=%f, EXP=%d (dec), ea H='%o, ea L='%o\n", tempf1, get16(ea+1) & 0xFF, get16(ea), get16(ea+1) & 0xFF00);
      crs[KEYS] &= ~0300;
      if (tempf == tempf1) {
	RPL++;
	crs[KEYS] |= 0100;
      } else if (tempf < tempf1) {
	RPL += 2;
	crs[KEYS] |= 0200;
      }
      continue;

    case 01701:
      TRACE(T_FLOW, " FDV\n");
      TRACE(T_INST, " FEXP=%d (dec), FLTH='%o, FLTL='%o\n", crs[FEXP], crs[FLTH], crs[FLTL]);
      //TRACE(T_INST, " ea EXP=%d (dec), ea H='%o, ea L='%o\n", (mem[ea+1] & 0xFF), mem[ea], (mem[ea+1] & 0xFF00));
      *(int *)&tempf = (crs[FLTH]<<16) | (crs[FLTL] & 0xFF00) | (crs[FEXP] & 0xFF);
      prieee4(&tempf);
      *(int *)&tempf1 = get32(ea);
      prieee4(&tempf1);
      tempf /= tempf1;
      ieeepr4(&tempf);
      crs[FLTH] = (*(unsigned int *)&tempf) >> 16;
      crs[FLTL] = (*(unsigned int *)&tempf) & 0xFF00;
      crs[FEXP] = (*(unsigned int *)&tempf) & 0xFF;
      XEXPC(0);
      continue;

    case 0201:
      TRACE(T_FLOW, " FLD\n");
      utempl = get32(ea);
      crs[FLTH] = utempl >> 16;
      crs[FLTL] = utempl & 0xFF00;
      crs[FEXP] = utempl & 0xFF;
      crs[FLTD] = 0;
      TRACE(T_INST, " FEXP=%d (dec), FLTH='%o, FLTL='%o\n", crs[FEXP], crs[FLTH], crs[FLTL]);
      continue;

    case 01601:
      TRACE(T_FLOW, " FMP\n");
      TRACE(T_INST, " FEXP=%d (dec), FLTH='%o, FLTL='%o\n", crs[FEXP], crs[FLTH], crs[FLTL]);
      //TRACE(T_INST, " ea EXP=%d (dec), ea H='%o, ea L='%o\n", (mem[ea+1] & 0xFF), mem[ea], (mem[ea+1] & 0xFF00));
      *(int *)&tempf = (crs[FLTH]<<16) | (crs[FLTL] & 0xFF00) | (crs[FEXP] & 0xFF);
      prieee4(&tempf);
      *(int *)&tempf1 = get32(ea);
      prieee4(&tempf1);
      tempf *= tempf1;
      ieeepr4(&tempf);
      crs[FLTH] = (*(unsigned int *)&tempf) >> 16;
      crs[FLTL] = (*(unsigned int *)&tempf) & 0xFF00;
      crs[FEXP] = (*(unsigned int *)&tempf) & 0xFF;
      XEXPC(0);
      continue;

    case 00701:
      TRACE(T_FLOW, " FSB\n");
      TRACE(T_INST, " FEXP=%d (dec), FLTH='%o, FLTL='%o\n", crs[FEXP], crs[FLTH], crs[FLTL]);
      //TRACE(T_INST, " ea EXP=%d (dec), ea H='%o, ea L='%o\n", (mem[ea+1] & 0xFF), mem[ea], (mem[ea+1] & 0xFF00));
      *(int *)&tempf = (crs[FLTH]<<16) | (crs[FLTL] & 0xFF00) | (crs[FEXP] & 0xFF);
      prieee4(&tempf);
      *(int *)&tempf1 = get32(ea);
      prieee4(&tempf1);
      tempf -= tempf1;
      ieeepr4(&tempf);
      crs[FLTH] = (*(unsigned int *)&tempf) >> 16;
      crs[FLTL] = (*(unsigned int *)&tempf) & 0xFF00;
      crs[FEXP] = (*(unsigned int *)&tempf) & 0xFF;
      XEXPC(0);
      continue;

    case 0401:
      TRACE(T_FLOW, " FST\n");
      TRACE(T_INST, " FEXP=%d (dec), FLTH='%o, FLTL='%o\n", crs[FEXP], crs[FLTH], crs[FLTL]);
      if (crs[FEXP] & 0xFF00)
	mathexception('f', FC_SFP_STORE, ea);
      put16(crs[FLTH],ea);
      put16((crs[FLTL] & 0xFF00) | crs[FEXP],ea+1);
      CLEARC;
      continue;

    case 0602:
      TRACE(T_FLOW, " DFAD\n");
      TRACE(T_INST, " FEXP=%d (dec), FLTH='%o, FLTL='%o\n", crs[FEXP], crs[FLTH], crs[FLTL]);
      //TRACE(T_INST, " ea EXP=%d (dec), ea H='%o, ea L='%o\n", (mem[ea+1] & 0xFF), mem[ea], (mem[ea+1] & 0xFF00));
      tempda[0] = crs[FLTH];
      tempda[1] = crs[FLTL];
      tempda[2] = crs[FLTD];
      tempda[3] = crs[FEXP];
      prieee8(tempda);
      tempd = get64(ea);
      prieee8(&tempd);
      *(double *)tempda += tempd;
      ieeepr8(tempda);
      crs[FLTH] = tempda[0];
      crs[FLTL] = tempda[1];
      crs[FLTD] = tempda[2];
      crs[FEXP] = tempda[3];
      XEXPC(0);
      continue;

    case 01102:
      TRACE(T_FLOW,  " DFCS\n");
      m = get16(ea);
      if ((crs[FLTH] & 0x8000) == (m & 0x8000)) {
	m1 = get16(INCVA(ea,3));
	if (m1 == crs[FEXP]) {
	  if (m == crs[FLTH]) {
	    utempl = get32(INCVA(ea,1));
	    if ((unsigned int)((crs[FLTL]<<16) | crs[FLTD]) == utempl)
	      RPL += 1;
	    else if ((unsigned int)((crs[FLTL]<<16) | crs[FLTD]) < utempl)
	      RPL += 2;
	  } else if (crs[FLTH] < m)
	    RPL += 2;
	} else if (crs[FEXP] < m1)       /* this line breaks CPU.FLOAT.V */
	  RPL += 2;
      } else if (crs[FLTH] & 0x8000)    /* DAC < mem */
	RPL += 2;

#if 0
      tempda[0] = crs[FLTH];
      tempda[1] = crs[FLTL];
      tempda[2] = crs[FLTD];
      tempda[3] = crs[FEXP];
      prieee8(tempda);
      printf(" FLTH=%x FLTL=%x FLTD=%x FEXP=%x, value=%e\n", crs[FLTH], crs[FLTL], crs[FLTD], crs[FEXP], *(double *)tempda);
      tempd = get64(ea);
      prieee8(&tempd);
      printf(" ea H=%x ea L=%x ea D=%x ea X=%x, value=%e\n", get16(ea), get16(ea+1), get16(ea+2), get16(ea+3), tempd);
      crs[KEYS] &= ~0300;
      if (*(double *)tempda == tempd) {
	RPL++;
	crs[KEYS] |= 0100;
      } else if (*(double *)tempda < tempd) {
	RPL += 2;
	crs[KEYS] |= 0200;
      }
#endif
      continue;

    case 01702:
      TRACE(T_FLOW, " DFDV\n");
      TRACE(T_INST, " FEXP=%d (dec), FLTH='%o, FLTL='%o\n", crs[FEXP], crs[FLTH], crs[FLTL]);
      //TRACE(T_INST, " ea EXP=%d (dec), ea H='%o, ea L='%o\n", (mem[ea+1] & 0xFF), mem[ea], (mem[ea+1] & 0xFF00));
      tempda[0] = crs[FLTH];
      tempda[1] = crs[FLTL];
      tempda[2] = crs[FLTD];
      tempda[3] = crs[FEXP];
      prieee8(tempda);
      tempd = get64(ea);
      prieee8(&tempd);
      *(double *)tempda /= tempd;
      ieeepr8(tempda);
      crs[FLTH] = tempda[0];
      crs[FLTL] = tempda[1];
      crs[FLTD] = tempda[2];
      crs[FEXP] = tempda[3];
      XEXPC(0);
      continue;

    case 0202:
      TRACE(T_FLOW, " DFLD\n");
      *(double *)tempda = get64(ea);
      crs[FLTH] = tempda[0];
      crs[FLTL] = tempda[1];
      crs[FLTD] = tempda[2];
      crs[FEXP] = tempda[3];
      continue;

    case 01602:
      TRACE(T_FLOW, " DFMP\n");
      TRACE(T_INST, " FEXP=%d (dec), FLTH='%o, FLTL='%o\n", crs[FEXP], crs[FLTH], crs[FLTL]);
      //TRACE(T_INST, " ea EXP=%d (dec), ea H='%o, ea L='%o\n", (mem[ea+1] & 0xFF), mem[ea], (mem[ea+1] & 0xFF00));
      tempda[0] = crs[FLTH];
      tempda[1] = crs[FLTL];
      tempda[2] = crs[FLTD];
      tempda[3] = crs[FEXP];
      prieee8(tempda);
      tempd = get64(ea);
      prieee8(&tempd);
      *(double *)tempda *= tempd;
      ieeepr8(tempda);
      crs[FLTH] = tempda[0];
      crs[FLTL] = tempda[1];
      crs[FLTD] = tempda[2];
      crs[FEXP] = tempda[3];
      XEXPC(0);
      continue;

    case 0702:
      TRACE(T_FLOW, " DFSB\n");
      TRACE(T_INST, " FEXP=%d (dec), FLTH='%o, FLTL='%o\n", crs[FEXP], crs[FLTH], crs[FLTL]);
      //TRACE(T_INST, " ea EXP=%d (dec), ea H='%o, ea L='%o\n", (mem[ea+1] & 0xFF), mem[ea], (mem[ea+1] & 0xFF00));
      tempda[0] = crs[FLTH];
      tempda[1] = crs[FLTL];
      tempda[2] = crs[FLTD];
      tempda[3] = crs[FEXP];
      prieee8(tempda);
      tempd = get64(ea);
      prieee8(&tempd);
      *(double *)tempda -= tempd;
      ieeepr8(tempda);
      crs[FLTH] = tempda[0];
      crs[FLTL] = tempda[1];
      crs[FLTD] = tempda[2];
      crs[FEXP] = tempda[3];
      XEXPC(0);
      continue;

    case 0402:
      TRACE(T_FLOW, " DFST\n");
      tempda[0] = crs[FLTH];
      tempda[1] = crs[FLTL];
      tempda[2] = crs[FLTD];
      tempda[3] = crs[FEXP];
      put64(*(double *)tempda, ea);
      continue;

    case 01302:
      TRACE(T_FLOW, " EALB\n");
      *(ea_t *)(crs+LB) = ea;
      continue;

    /* NOTE: during Primos coldstart, when setting PPA:

14/35052: 7404          A='100/64 B='0/0 X=1010/520 Y=1235/669 C=0 L=1 LT=0 EQ=1        #75327960 [SEG0 0]
 opcode=00300, i=0, x=0
 2-word format, a=40032
 new opcode=00301, y=0, br=0, ixy=0, xok=1
 EA: 14/40032  COLDS+'32
 STLR

14/35054: 141704                A='100/64 B='0/0 X=1010/520 Y=1235/669 C=0 L=1 LT=0 EQ=1        #75327961 [SEG0 0]
 generic class 3
 BCS

       Not sure what the C-bit means here... maybe multi-processor stuff?
    */

    case 0301:
      TRACE(T_FLOW, " STLR\n");
      if (MAKEVA(012,037122) <= ea && ea <= MAKEVA(012,042664))
	printf("STLR in PNCDIM at %o/%o, ea=%o/%o, L=%o/%o\n", RPH, RPL-2, ea>>16, ea&0xffff, crs[A], crs[B]);
      utempa = ea;                 /* word number portion only */
      if (utempa & 040000) {       /* absolute RF addressing */
	RESTRICT();
	regs.s32[utempa & 0377] = *(int *)(crs+L);
      } else {
	utempa &= 037;
	if (utempa > 017) RESTRICT();
	*(((int *)crs)+utempa) = *(int *)(crs+L);
      }
      continue;

    case 0501:
      TRACE(T_FLOW, " LDLR\n");
      utempa = ea;                 /* word number portion only */
      if (utempa & 040000) {       /* absolute RF addressing */
	RESTRICT();
	*(int *)(crs+L) = regs.s32[utempa & 0377];
      } else {
	utempa &= 037;
	if (utempa > 017) RESTRICT();
	*(int *)(crs+L) = *(((int *)crs)+utempa);
      }
      continue;

    case 01401:
      TRACE(T_FLOW, " EIO\n");
      crs[KEYS] &= ~0100;      /* reset EQ */
      pio(ea & 0xFFFF);
      continue;

    case 00102:
      TRACE(T_FLOW, " XEC\n");
      utempa = get16(ea);
      //utempl = RP-2;
      //printf("RPL %o/%o: XEC instruction %o|%o, ea is %o/%o, new inst = %o \n", utempl>>16, utempl&0xFFFF, inst, get16(utempl+1), ea>>16, ea&0xFFFF, utempa);
      inst = utempa;
      earp = INCVA(ea,1);
      goto xec;

    case 00103:
      TRACE(T_FLOW, " ENTR\n");
      utempa = crs[S];
      crs[S] -= ea;
      put16(utempa,crs[S]);
      continue;

    default:
      printf("Unknown memory reference opcode: %o\n", opcode);
      fatal(NULL);
    }
  }
}
    
  
/* Handle SVC instruction.  For real hardware emulation on an R-mode
   such as the P300, SVC would interrupt (JST*) through location '75
   (in vectored mode) or would fault on the P400.  

   Since we may be running programs without an OS underneath us,
   handle the SVC's here if vectors aren't set.

   Typical usage:

     CALL TNOUA ('MESSAGE', 7)
         JST TNOUA
         DAC =C'MESSAGE'
         DAC =7
         OCT 0
         ... 

     The library would resolve the TNOUA reference like this:
 
     TNOUA DAC  **        RETURN ADDRESS
           SVC            ENTER THE OS
           OCT  140703
           JMP# BADCALL   HERE FOR BAD SVC
           ...

   The SVC code word follows the SVC instruction:

     '100000 = this is an interlude; the actual arguments are found
     at the address preceeding the SVC instruction (ie, the caller)

     '040000 = there is an instruction following the SVC code, before
     the argument list.  If there is an SVC error, like a bad function
     code, the SVC handler will return to the location following the
     code word.

     Bits 3-4 are ignored

     Bits 5-10 are the SVC group, 1-15.
     Bits 11-16 are the SVC function within the group.

  Interludes are used because at the time a program is compiled, the
  compiler doesn't know whether a call is going to the OS or not.  So the
  call is compiled like any other.  It's dumb for the library TNOUA to
  muck around with all the arguments, so bit 1 is set as a shortcut and
  the address of the argument list is just before the SVC itself.

  If a program knows it's going to call the OS, a direct SVC can be used,
  with bit 1 clear:

	 SVC
	 OCT  '040703
         JMP# BADSVC
         DAC  =C'MESSAGE'
         DAC  =7
         OCT  0
  GOOD   ...

  BADSVC EQU  *

*/


svc() {

#define MAXCLASS 015
#define MAXFUNC 027
#define MAXSVCARGS 10

  unsigned short code;             /* code word following SVC instruction */
  unsigned short argl;             /* address of argument list */
  void *arg[MAXSVCARGS];           /* arg address array */
  short actargs;                   /* number of actual arguments */
  short class;                     /* SVC class, from bits 5-10 */
  short func;                      /* SVC func within class, bits 11-16 */
  short temp16;
  short dolocal;

  static struct {
    char name[8];                  /* svc routine name */
    char numargs;                  /* number of arguments for this svc */
    short locargs;                 /* bit mask for LOC(X) arguments */
  } svcinfo[MAXCLASS+1][MAXFUNC+1] = {

    /* class 0 is a dummy: it's mapped to class 1 below */

    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,

    /* class 1 */

    "ATTACH", -5, 0, /* func 0 */
    "SEARCH", -4, 0, /* func 1 */
    "SAVE  ",  2, 0, /* func 2 */
    "RESTOR", -3, 0, /* func 3 */
    "RESUME",  1, 0, /* func 4 */
    "EXIT  ",  0, 0, /* func 5 */
    "ERRRTN",  4, 0, /* func 6 */
    "UPDATE", 99, 0, /* func 7 */
    "GETERR",  2, 0, /* func 010 */
    "PRERR ",  0, 0, /* func 011 */
    "GINFO ",  2, 0, /* func 012 */
    "CNAME$", -3, 0, /* func 013 */
    "ERRSET",  5, 0, /* func 014 */
    "FORCEW",  2, 0, /* func 015 */
    "",99,0, "",99,0,
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,

    /* class 2 */

    "READ  ", 99, 0, /* func 0 */
    "WRITE ", 99, 0, /* func 1 */
    "RDLIN ", -4, 0, /* func 2 */
    "WTLIN ", -4, 0, /* func 3 */
    "",99,0, "",99,0, "",99,0, "",99,0,
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,

    /* class 3 */

    "PRWFIL", -6, 020000, /* func 0 */
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,

    /* class 4 */
    
    "FAMSVC", 99, 0, /* func 0 */
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,

    /* class 5 */

    "RREC  ", -6, 0, /* func 0 */
    "WREC  ", -6, 0, /* func 1 */
    "TIMDAT",  2, 0, /* func 2 */
    "DIGIN ", -4, 0, /* func 3 */
    "DIGINW", -4, 0, /* func 4 */
    "RCYCL ",  0, 0, /* func 5 */
    "D$INIT",  1, 0, /* func 6 */
    "BREAK$",  1, 0, /* func 7 */
    "T$MT  ",  6, 040000, /* func 010 */
    "T$LMPC",  5, 040000, /* func 011 */
    "T$CMPC",  5, 040000, /* func 012 */
    "T$AMLC",  5, 040000, /* func 013 */
    "T$VG  ",  5, 040000, /* func 014 */
    "T$PMPC",  5, 040000, /* func 015 */
    "RRECL ", -6, 0, /* func 016 */
    "WRECL ", -6, 0, /* func 017 */
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,

    /* class 6 */

    "COMANL",  0, 0, /* func 0 */
    "C1IN  ",  1, 0, /* func 1 */
    "CMREAD",  1, 0, /* func 2 */
    "COMINP", -3, 0, /* func 3 */
    "CNIN$ ",  3, 0, /* func 4 */
    "PHANT$",  5, 0, /* func 5 */
    "",99,0, "",99,0,
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,

    /* class 7 */

    "T1IN  ",  1, 0, /* func 0 */
    "T1OU  ",  1, 0, /* func 1 */
    "TNOU  ",  2, 0, /* func 2 */
    "TNOUA ",  2, 0, /* func 3 */
    "TOOCT ",  1, 0, /* func 4 */
    "DUPLX$",  1, 0, /* func 5 */
    "",99,0, "",99,0,
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,

    /* class 010 */

    "T$MT  ",  6, 040000, /* func 0 */
    "T$SMLC",  4, 020000, /* func 1 */
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,

    /* class 011 */

    "T$LMPC",  6, 040000, /* func 0 */
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,

    /* class 012 */

    "T$CMPC",  6, 040000, /* func 0 */
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,

    /* class 013 (user defined) */

    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,

    /* class 014 */

    "ATTAC$",  6, 002000, /* func 0 */
    "CREAT$",  5, 004000, /* func 1 */
    "ERRPR$",  6, 0, /* func 2 */
    "ERRST$", 99, 0, /* func 3 */
    "GPASS$",  5, 004000, /* func 4 */
    "GQUOT$", 99, 0, /* func 5 */
    "PRWFL$",  7, 021000, /* func 6 */
    "RDENT$",  8, 000400, /* func 7 */
    "SATTR$",  5, 004000, /* func 010 */
    "SEARC$",  6, 002000, /* func 011 */
    "SEGDR$",  5, 004000, /* func 012 */
    "SPASS$",  3, 020000, /* func 013 */
    "SQUOT$", 99, 0, /* func 014 */
    "CNGNM$",  5, 004000, /* func 015 */
    "COMIN$",  4, 010000, /* func 016 */
    "RDTKN$",  5, 004000, /* func 017 */
    "RESTO$",  4, 010000, /* func 020 */
    "RESUM$",  2, 0,  /* func 021 */
    "SAVE$ ",  4, 010000,  /* func 022 */
    "",99,0, "",99,0, "",99,0, "",99,0, "",99,0,

    /* class 015 */

    "ATCH$$",  6, 0,  /* func 0 */
    "CREA$$",  5, 0,  /* func 1 */
    "",99,0,           /* func 2 */
    "",99,0,           /* func 3 */
    "GPAS$$",  5, 0,  /* func 4 */
    "",99,0,           /* func 5 */
    "PRWF$$",  7, 020000,  /* func 6 */
    "RDEN$$",  8, 0,  /* func 7 */
    "SATR$$",  5, 0,  /* func 010 */
    "SRCH$$",  6, 0,  /* func 011 */
    "SGDR$$",  5, 0,  /* func 012 */
    "SPAS$$",  3, 0,  /* func 013 */
    "",99,0,  /* func 014 */
    "CNAM$$",  5, 0,  /* func 015 */
    "COMI$$",  4, 0,  /* func 016 */
    "RDTK$$",  5, 0,  /* func 017 */
    "REST$$",  4, 0,  /* func 020 */
    "RESU$$",  2, 0,  /* func 021 */
    "SAVE$$",  4, 0,  /* func 022 */
    "COMO$$",  5, 0,  /* func 023 */
    "ERKL$$",  4, 0,  /* func 024 */
    "RDLIN$",  4, 0,  /* func 025 */
    "WTLIN$",  4, 0,  /* func 026 */
    "",99,0
  };

#if 0
  for (class=0; class<=MAXCLASS; class++)
    for (func=0; func<=MAXFUNC; func++)
      printf("Class %o, func %o: %s %d args %o\n", class,func, svcinfo[class][func].name, svcinfo[class][func].numargs, svcinfo[class][func].locargs);
#endif

  /* if location '65 is set, do indirect JST to handle svc */

  /* if the svc fault vector is zero, interpret the svc here.  This
     allows the emulator to run R-mode programs directly */

  dolocal = 1;
  if ((crs[MODALS] & 010) || get16(065) != 0)
    dolocal = 0;

  if (dolocal || (traceflags & (T_INST|T_FLOW))) {

    /* get svc code word, break into class and function */

    code = iget16(RP);
    class = (code >> 6) & 077;
    if (class == 0)
      class = 1;
    func = code & 077;

    /* determine argument list location and create arg list vector */

    if (code & 0100000)
      argl = get16(MAKEVA(RPH,RPL-2));
    else if (code & 040000)
      argl = RPL+2;
    else
      argl = RPL+1;

    TRACE(T_INST, " code=%o, class=%o, func=%o, argl=%o\n", code, class, func, argl);
    if (class > MAXCLASS || func > MAXFUNC)
      goto badsvc;

    TRACE(T_FLOW, " name=%s, #args=%d, LOC args=%o\n", svcinfo[class][func].name, svcinfo[class][func].numargs, svcinfo[class][func].locargs);
  }

  if (!dolocal) {
    fault(SVCFAULT, 0, 0);
    fatal("Returned from SVC fault");
  }

  if (svcinfo[class][func].numargs == 99)
    goto badsvc;

  if (svcinfo[class][func].locargs != 0)
    fatal("Can't handle LOC() args");

  actargs = 0;
  if (svcinfo[class][func].numargs == 1) {
    actargs = 1;
    arg[0] = mem+mem[argl++];
    if (mem[argl] == 0)     /* single arg: terminating zero is optional */
      argl++;
  } else if (svcinfo[class][func].numargs > 0) {
    while (mem[argl] != 0) {
      if (actargs < MAXSVCARGS)
	arg[actargs++] = mem+mem[argl];
      argl++;
    }
    argl++;                 /* skip terminating zero */
    while (actargs < svcinfo[class][func].numargs)
      arg[actargs++] == NULL;
  }

  TRACE(T_INST, " return=%o, actargs=%d\n", argl, actargs);

  switch (class) {
  case 0: /* same as class 1 */
  case 1: /* funcs 0-'15 */
    switch (func) {
    case 0:  /* ATTACH (NAME,DEV,PASSWD,HOMESET,ALTRTN) */
      goto unimp;
    case 1:  /* SEARCH (KEY,NAME,UNIT,ALTRTN) */
      goto unimp;
    case 2:  /* SAVE (RVEC,NAME) */
      goto unimp;
    case 3:  /* RESTOR (RVEC,NAME,ALTRTN) */
      goto unimp;
    case 4:  /* RESUME (NAME) */
      goto unimp;
    case 5:  /* EXIT () */
      os_exit();
    case 6:  /* ERRRTN (ALTRTN,'XXXXXX',MSG,MSGLEN) */
      goto unimp;
    case 7:  /* UPDATE */
      goto unimp;
    case 010:  /* GETERR (BUFF,N) */
      goto unimp;
    case 011:  /* PRERR () */  
      goto unimp;
    case 012:  /* GINFO (BUFF,N) */
      os_ginfo(arg[0], arg[1]);
      break;
    case 013:  /* CNAME$ (OLDNAM,NEWNAM,ALTRTN) */
      goto unimp;
    case 014:  /* ERRSET (ALTVAL,ALTRTN,'XXXXXX','MSG',MSGLEN) */
      goto unimp;
    case 015:  /* FORCEW (KEY,UNIT) */
      goto unimp;
    default:
      goto unimp;
    }
    break;

  case 2: /* funcs 0-3 */
    goto unimp;
    break;

  case 3: /* func 0 = PRWFIL */
    switch (func) {
    case 0:  /* PRWFIL (KEY,UNIT,LOC(BUF),N,POS,ALTRTN) */
      goto unimp;
    default:
      goto unimp;
    }
    break;

  case 4: /* func 0 = FAMSVC (obsolete) */
    goto unimp;
    break;

  case 5: /* funcs 0-'17 */
    switch (func) {
    case 0:  /* RREC (PBAV,NWV,N,RA16,PDEV,ALTRTN) */
      goto unimp;
    case 1:  /* WREC (PBAV,NWV,N,RA16,PDEV,ALTRTN) */
      goto unimp;
    case 2:  /* TIMDAT (BUFF,N) */
      os_timdat(arg[0], arg[1]);
      break;
    case 3:  /* DIGIN (N,ADRVEC,DATVEC,ALTRTN) */
      goto unimp;
    case 4:  /* DIGINW (same, but initial arg of 1 instead of 0 to DIGIN) */
      goto unimp;
    case 5:  /* RECYCL */
      goto unimp;
    case 6:  /* D$INIT (PDEV) */
      goto unimp;
    case 7:  /* BREAK$ (ONOFF) */
      os_break$(arg[0]);
      break;
    case 010:  /* T$MT (UNIT,LOC(BUF),NW,INST,STATV,CODE) */
      goto unimp;
    case 011:  /* T$LMPC (UNIT,LOC(BUF),NW,INST,STATV) */
      goto unimp;
    case 012:  /* T$CMPC (UNIT,LOC(BUF),NW,INST,STATV) */
      goto unimp;
    case 013:  /* T$AMLC (LINE,LOC(BUF),NW,INST,STATV) */
      goto unimp;
    case 014:  /* T$VG (UNIT,LOC(BUF),NW,INST,STATV) */
      goto unimp;
    case 015:  /* T$PMPC (UNIT,LOC(BUF),NW,INST,STATV) */
      goto unimp;
    case 016:  /* RRECL (PBAV,NWV,N,RA32,PDEV,ALTRTN) */
      goto unimp;
    case 017:  /* WRECL (PBAV,NWV,N,RA32,PDEV,ALTRTN) */
      goto unimp;
    default:
      goto unimp;
    }
    break;

  case 6: /* funcs 0-5 */
    switch (func) {
    case 0:  /* COMANL () */
      os_comanl();
      break;
    case 1:  /* C1IN (CHAR) */
      os_c1in(arg[0]);
      break;
    case 2:  /* CMREAD (BUFF) */
      strncpy(arg[0], "      ", 6);
      break;
    case 3:  /* COMINP (FILNAM,UNIT,ALTRTN) */
      goto unimp;
    case 4:  /* CNIN$ (BUF,MAXCHARS,ACTCHARS) */
      os_cnin$(arg[0], arg[1], arg[2]);
      break;
    case 5:  /* PHANT$ (FILNAM,NAMLEN,UNIT,USER,CODE) */
      goto unimp;
    default:
      goto unimp;
    }
    break;

  case 7:
    switch (func) {
    case 0:  /* T1IN (CHAR) */
      os_c1in(arg[0]);
      break;
    case 1:  /* T1OU (CHAR) */
      os_t1ou(arg[0]);
      break;
    case 2: /* TNOU */                       
    case 3: /* TNOUA */
      os_tnoua(arg[0],arg[1]);
      if (func == 2) {
	temp16 = 1;
	os_tnoua("\n", &temp16);
      }
      break;
    case 4:  /* TOOCT (NUMBER) */
      goto unimp;
    case 5:  /* DUPLX$ (KEY) */
      goto unimp;
    default:
      goto unimp;
    }
    break;

  case 010:
    switch (func) {
    case 0:  /* T$MT (UNIT,LOC(BUF),NW,INST,STATV,CODE) */
      goto unimp;
    case 1:  /* T$SLC1 (KEY,LINE,LOC(BUF),NWORDS) */
      goto unimp;
    default:
      goto unimp;
    }
    break;

  case 011:
    switch (func) {
    case 0:  /* T$LMPC (UNIT,LOC(BUF),NW,INST,STATV,CODE) */
      goto unimp;
    default:
      goto unimp;
    }
    break;

  case 012:
    switch (func) {
    case 0:  /* T$CMPC (UNIT,LOC(BUF),NW,INST,STATV,CODE) */
      goto unimp;
    default:
      goto unimp;
    }
    break;

  case 013:
    switch (func) {  /* func is 0-2 */
    default:
      goto unimp;
    }
    break;

  case 014:
    switch (func) {
    case 0:  /* ATTAC$ (NAME,NAMLEN,LDEV,PASSWD,KEY,LOC(CODE)) */
      goto unimp;
    case 1:  /* CREAT$ (NAME,NAMLEN,OWNER,NONOWNER,LOC(CODE)) */
      goto unimp;
    case 2:  /* ERRPR$ (KEY,CODE,TEXT,TEXTLEN,PROGNAME,NAMLEN) */
      os_errpr$ (arg[0], arg[1], arg[2], arg[3], arg[4], arg[5]);
      break;
    case 4:  /* GPASS$ (NAME,NAMLEN,OWNER,NONOWNER,LOC(CODE) */
      goto unimp;
    case 6:  /* PRWFL$ (KEY,UNIT,LOC(BUF),NW,POS,RNW,LOC(CODE)) */
      goto unimp;
    case 7:  /* RDENT$ (KEY,UNIT,BUF,BUFSIZ,RNW,NAME,NAMLEN,LOC(CODE)) */
      goto unimp;
    case 010:  /* SATTR$ (KEY,NAME,NAMLEN,ARRAY,LOC(CODE)) */
      goto unimp;
    case 011:  /* SEARC$ (KEY,NAME,NAMLEN,UNIT,TYPE,LOC(CODE) */
      goto unimp;
    case 012:  /* SEGDR$ (KEY,UNIT,ENTRYA,ENTRYB,LOC(CODE)) */
      goto unimp;
    case 013:  /* SPASS$ (OWNER,NONOWNER,LOC(CODE)) */
      goto unimp;
    case 015:  /* CNGNM$ (OLDNAM,OLDLEN,NEWNAM,NEWLEN,LOC(CODE)) */
      goto unimp;
    case 016:  /* COMIN$ (FILNAM,NAMLEN,UNIT,LOC(CODE)) */
      goto unimp;
    case 017:  /* RDTKN$ (KEY,INFO(5),BUF,BUFLEN,LOC(CODE)) */
      goto unimp;
    case 020:  /* RESTO$ (RVEC(9),FILNAM,NAMLEN,LOC(CODE)) */
      goto unimp;
    case 021:  /* RESUM$ (NAME,NAMLEN) */
      goto unimp;
    case 022:  /* SAVE$ (RVEC,FILNAM,NAMLEN,LOC(CODE)) */
      goto unimp;
    default:
      goto unimp;
    }
    break;

  case 015:
    switch (func) {
    case 0:  /* ATCH$$ (UFDNAM,NAMLEN,LDEV,PASSWD,KEY,CODE) */
      goto unimp;
    case 1:  /* CREA$$ (NAME,NAMLEN,OPASS,NPASS,CODE) */
      goto unimp;
    case 4:  /* GPAS$$ (UFDNAM,NAMLEN,OPASS,NPASS,CODE) */
      goto unimp;
    case 6:  /* PRWF$$ (KEY,FUNIT,LOC(BUF),BUFLEN,POS32,RNW,CODE) */
      goto unimp;
    case 7:  /* RDEN$$ (KEY,FUNIT,BUF,BUFLEN,RNW,NAME32,NAMLEN,CODE) */
      goto unimp;
    case 010:  /* SATR$$ (KEY,NAME,NAMLEN,ARRAY,CODE) */
      goto unimp;
    case 011:  /* SRCH$$ (KEY,NAME,NAMLEN,FUNIT,TYPE,CODE) */
      goto unimp;
    case 012:  /* SGDR$$ (KEY,FUNIT,ENTRYA,ENTRYB,CODE) */
      goto unimp;
    case 013:  /* SPAS$$ (OWNER,NON-OWNER,CODE) */
      goto unimp;
    case 015:  /* CNAM$$ (OLDNAM,OLDLEN,NEWNAM,NEWLEN,CODE) */
      goto unimp;
    case 016:  /* COMI$$ (FILNAM,NAMLEN,UNIT,CODE) */
      goto unimp;
    case 017:  /* RDTK$$ (KEY,INFO(5),BUF,BUFLEN,CODE) */
      os_rdtk$$ (arg[0], arg[1], arg[2], arg[3], arg[4]);
      break;
    case 020:  /* REST$$ (RVEC,NAME,NAMLEN,CODE) */
      goto unimp;
    case 021:  /* RESU$$ (NAME,NAMLEN) */
      goto unimp;
    case 022:  /* SAVE$$ (RVEC,NAME,NAMLEN,CODE) */
      goto unimp;
    case 023:  /* COMO$$ (KEY,NAME,NAMLEN,XXXXXX,CODE) */
      goto unimp;
    case 024:  /* ERKL$$ (KEY,ERASECH,KILLCH,CODE) */
      os_erkl$$(arg[0], arg[1], arg[2], arg[3]);
      break;
    case 025:  /* RDLIN$ (UNIT,LINE,NWDS,CODE) */
      goto unimp;
    case 026:  /* WTLIN$ (UNIT,LINE,NWDS,CODE) */
      goto unimp;
    default:
      goto unimp;
    }
    break;

  default:
    goto unimp;                   /* bad class */
  }

  /* after the SVC, argl is the return address */

  TRACE(T_INST, " returning from SVC to %o\n", argl);
  RPL = argl;
  return;


unimp:

  printf(" svc not implemented, class=%o, func=%o\n", class, func);
  fatal(NULL);

  /* here on a bad svc; if the bounce bit (bit 2) is set in the code word,
     jump to the location following the code word (which is typically a
     JMP instruction).  If the bounce bit isn't set, we have to halt */

badsvc:

  if (code & 040000) {
    RPL++;
    TRACE(T_INST, " bouncing svc error to address %o\n", RPL);
    return;
  }
  
  printf(" halting on bad svc, class=%o, func=%o\n", class, func);
  fatal(NULL);
}



/* here for PIO instructions: OCP, SKS, INA, OTA.  The instruction
   word is passed in as an argument to handle EIO (Execute I/O) in
   V-mode.
*/

pio(unsigned int inst) {
  int class;
  int func;
  int device;

  RESTRICT();
  class = inst >> 14;
  func = (inst >> 6) & 017;
  device = inst & 077;
  TRACE(T_INST, " pio, class=%d, func='%o, device='%o\n", class, func, device);
  if (devmap[device])
    devmap[device](class, func, device);
  else {
#if 1
    printf("pio: no handler, class=%d, func='%o, device='%o, A='%o\n", class, func, device, crs[A]);
    fatal(NULL);
#else
    TRACEA("pio: no handler, class=%d, func='%o, device='%o, A='%o\n", class, func, device, crs[A]);
#endif
  }
}
