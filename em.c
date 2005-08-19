/* Pr1me Computer emulator, Jim Wilcoxson (jim@meritnet.com), April 4, 2005
   Copyright (C) 2005, Jim Wilcoxson (jim@meritnet.com).  All Rights Reserved.

   Restores a Prime R-mode .save image from stdin to memory and
   emulates execution, or boots from a Prime disk image.

   This is a project in development, so please don't publish it.
   Comments, suggestions, corrections, and general notes that you're
   interested in a Prime emulation project are welcome and
   appreciated.

   Usage:

   $ ./em <smad.save 2>/dev/null

   Lots of instruction details are spewed to stderr.


   NOTE: this only runs on a big-endian machine, like the Prime.
*/

#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <setjmp.h>
#include <sys/time.h>
#include "syscom/keys.ins.cc"
#include "syscom/errd.ins.cc"

/* In SR modes, Prime CPU registers are mapped to memory locations 0-'37, but 
   only 0-7 are user accessible.  In the post-P300 architecture, these addresses
   map to the live register file.
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

#define SETCC_LE \
  crs[KEYS] &= ~0300; \
  if (*(int *)(crs+L) == 0 && *(int *)(crs+E) == 0) \
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

/* macro for restricted instructions */

#define RESTRICT() if (RP & RINGMASK32) fault(RESTRICTFAULT, 0, 0);


/* Table for unusual instructions that aren't implemented but we want to
   print something when they're encountered */

unsigned short gen0tab[] = {
  000503,          /* EMCM */
  000501,          /* LMCM */
  001304,          /* MDEI */
  001305,          /* MDII */
  001324,          /* MDIW */
  001306,          /* MDRS */
  001307,          /* MDWC */
  000021,          /* RMC */
  000311,          /* VIFY */
  001113};         /* XVFY */

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
   T_MAP        segmentation
   T_PCL        PCL instructions
   T_FAULT      Faults
   T_PX         Process exchange
*/

#define TB_EAR 0x00000001
#define TB_EAV 0x00000002
#define TB_EAI 0x00000004
#define TB_INST 0x00000008
#define TB_FLOW 0x00000010
#define TB_MODE 0x00000020
#define TB_EAAP 0x00000040
#define TB_DIO 0x00000080
#define TB_MAP 0x00000100
#define TB_PCL 0x00000200
#define TB_FAULT 0x00000400
#define TB_PX 0x00000800

#define T_EAR  (traceflags & TB_EAR)
#define T_EAV  (traceflags & TB_EAV)
#define T_EAI  (traceflags & TB_EAI)
#define T_INST (traceflags & TB_INST)
#define T_FLOW (traceflags & TB_FLOW)
#define T_MODE (traceflags & TB_MODE)
#define T_EAAP (traceflags & TB_EAAP)
#define T_DIO (traceflags & TB_DIO)
#define T_MAP (traceflags & TB_MAP)
#define T_PCL (traceflags & TB_PCL)
#define T_FAULT (traceflags & TB_FAULT)
#define T_PX (traceflags & TB_PX)

int traceflags=0;                    /* each bit is a trace flag */
int savetraceflags=0;                /* see ITLB, BDX */

int intvec=-1;                       /* currently raised interrupt (if >= zero) */

/* NOTE: Primos II gives "NOT FOUND" on startup 2460 if sense switches
   are set to 014114.  But DIAGS like this sense switch setting. :( */

unsigned short sswitch = 0;          /* sense switches, set with --ss */

unsigned short cpuid = 0;            /* STPM CPU model, set with --cpuid */

unsigned long instcount=0;           /* global instruction count */

unsigned short inhcount = 0;         /* number of instructions to stay inhibited */

unsigned int instpermsec = 2048;     /* initially assume 2048 inst/msec */

jmp_buf jmpbuf;                      /* for longjumps to the fetch loop */

/* define a 4 million 16-bit word system memory; later systems have 
   more main memory capacity, using a different page map setup */

#define MEMSIZE 4*1024*1024
unsigned short mem[MEMSIZE];   /* system's physical memory */

#define MAKEVA(seg,word) (((int)(seg))<<16) | (word)

/* returns the incremented value of a virtual address, wrapping to word
   zero at the end of a segment (word portion = 0177777) */

#define INCVA(ea,n) (((ea) & 0xFFFF0000) | ((ea)+(n)) & 0xFFFF)

/* STLB cache is defined here.  There are several different styles on
   various Prime models.  This is modeled after the 6350 STLB, but is
   only 1-way associative */

#define STLBENTS 512

typedef struct {
  char valid;                 /* 1 if this STLB entry is valid, zero otherwise */
  char unmodified;            /* 1 if this page hasn't been modified, 0 if modified */
  //  char shared;                /* 1 if page is shared and can't be cached */
  char access[4];             /* ring n access rights */
  unsigned short procid;      /* process id for segments >= '4000 */
  unsigned short seg;         /* segment number */
  unsigned short vpage;       /* virtual page address */
  unsigned int ppn;           /* physical page number (128MB limit) */
  unsigned short *pmep;       /* pointer to page table flag word */
  unsigned long load_ic;      /* instruction where STLB was loaded */
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
#define RINGMASK32 0x60000000        /* ring bits */
#define EXTMASK32 0x10000000         /* E-bit */
#define SEGMASK32 0x0FFF0000         /* segment number */
#define RINGMASK16 0x6000            /* ring bits */
#define EXTMASK16 0x1000             /* E-bit */

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

int verbose;                         /* --v (not used anymore) */
int domemdump;                       /* --memdump arg */
int boot;                            /* true if reading a boot record */

/* load map related data, specified with --map */

#define MAXSYMBOLS 5000
#define MAXSYMLEN 9
int numsyms = 0;
static struct {
  char symname[MAXSYMLEN];
  int  address;
} mapsym[MAXSYMBOLS];


readloadmap(char *filename) {
  FILE *mapf;
  char line[100];
  int lc;
  char sym[100];
  int symlen;
  int segno, wordno;
  int thisaddr, lastaddr;
  int wordlen;

  printf("Reading load map from %s... ", filename);
  if ((mapf = fopen(filename, "r")) == NULL) {
    perror("Map file open");
    fatal(NULL);
  }
  lc = 0;
  lastaddr = -1;
  while (fgets(line, sizeof(line), mapf) != NULL) {
    lc++;
    if (sscanf(line, " %s %o %o ", sym, &segno, &wordno) > 0) {
      if (strcmp(sym,"*START") == 0)
	break;
      symlen = strlen(sym);
      if (symlen > 0 && symlen < MAXSYMLEN) {
	//printf("%s = %o/%o\n", sym, segno, wordno);
	strcpy(mapsym[numsyms].symname, sym);
	thisaddr = segno<<16 | wordno;
	if (thisaddr < lastaddr) {
	  printf("Load map sequence error at line %d\n", lc);
	  fatal(NULL);
	}
	mapsym[numsyms].address = thisaddr;
	lastaddr = thisaddr;
	numsyms++;
	if (numsyms == MAXSYMBOLS) {
	  printf("TABLE LIMIT! ");
	  break;
	}
      }
    }
  }
  fclose(mapf);
  printf("%d symbols loaded\n", numsyms);
}

/* returns a pointer to a static character string like DSKBLK+25, to print
   with the effective address for an instruction */

char *searchloadmap(int addr) {
  int low, high, mid, diff;
  static char buf[100] = "";

  if (numsyms == 0)
    return buf;

  addr &= 0xFFFFFFF;      /* strip fault, ring, E bits */

  low = 0;
  high = numsyms-1;
  while (low <= high) {
    mid = (low+high)/2;
    if (addr < mapsym[mid].address)
      high = mid-1;
    else if (addr > mapsym[mid].address && mid != numsyms-1 && addr >= mapsym[mid+1].address)
      low = mid+1;
    else
      break;
  }
  diff = addr - mapsym[mid].address;
  if (diff) {
    sprintf(buf, "%s+'%o", mapsym[mid].symname, diff);
    return buf;
  } else
    return mapsym[mid].symname;
}


#define DTAR(ea) (((ea)>>26) & 3)
#define SEGNO(ea) (((ea)>>16) & 07777)
#define PAGENO(ea) (((ea)>>10) & 077)

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


/* maps a Prime 28-bit virtual address to a physical memory
   address, checks access, returns actual access (for PCL)
   May cause:
   - segment fault if segment number is too big
   - segment fault if segment's fault bit is set
   - access fault if intended access isn't permitted
   - page fault if page isn't resident
*/

/* NOTE: this is the 6350 STLB hash function, giving a 9-bit index 0-511 */

#define STLBIX(ea) ((((((ea) >> 12) ^ (ea)) & 0xc000) >> 7) | (((ea) & 0x70000) >> 12) | ((ea) & 0x3c00) >> 10)

pa_t mapva(ea_t ea, short intacc, short *access) {
  short relseg,seg,nsegs,ring;
  unsigned short pte, stlbix;
  stlbe_t *stlbp;
  unsigned int dtar,sdw,staddr,ptaddr,pmaddr;
  pa_t pa;

  seg = SEGNO(ea);
  ring = ((RP | ea) >> 29) & 3;  /* current ring | ea ring = access ring */

  if ((seg > 0 && (crs[MODALS] & 4)) || (seg == 0 && (crs[MODALS] & 020))) {
    stlbix = STLBIX(ea);
    stlbp = stlb+stlbix;
    if (stlbix >= STLBENTS) {
      printf("STLB index %d is out of range for va %o/%o!\n", stlbix, ea>>16, ea&0xffff);
      exit(1);
    }

    /* if the STLB entry isn't valid, or the segments don't match,
       or the segment is private and the process id doesn't match,
       then the STLB has to be loaded first */

    if (/* 1 || instcount == 20445443 || */ !stlbp->valid || stlbp->seg != seg || (seg >= 04000 && stlbp->procid != crs[OWNERL])) {
      dtar = *(unsigned int *)(crs+DTAR0-2*DTAR(ea));  /* get dtar register */
      nsegs = 1024-(dtar>>22);
      relseg = seg & 0x3FF;     /* segment within segment table */
      if (T_MAP) fprintf(stderr,"   MAP: ea=%o/%o, seg=%o, dtar=%o, nsegs=%d, relseg=%d, page=%d\n", ea>>16, ea&0xFFFF, seg, dtar, nsegs, relseg, PAGENO(ea));
      if (relseg >= nsegs)
	fault(SEGFAULT, 1, ea);   /* fcode = segment too big */
      staddr = (dtar & 0x003F0000) | ((dtar & 0x7FFF)<<1);
      sdw = *(unsigned int *)(mem+staddr+relseg*2);
      if (T_MAP) fprintf(stderr,"        staddr=%o, sdw=%o\n", staddr, sdw);
      if (sdw & 0x8000)
	fault(SEGFAULT, 2, ea);   /* fcode = sdw fault bit set */
      ptaddr = (((sdw & 0x3F)<<10) | (sdw>>22)) << 6;
      pmaddr = ptaddr + PAGENO(ea);
      pte = mem[pmaddr];
      if (T_MAP) fprintf(stderr,"        ptaddr=%o, pmaddr=%o, pte=%o\n", ptaddr, pmaddr, pte);
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
      stlbp->vpage = ea & 0xfc00;
      stlbp->ppn = (pte & 0xFFF);
      stlbp->pmep = mem+pmaddr;
      stlbp->load_ic = instcount;
    }
    if (stlbp->vpage != (ea & 0xfc00))
      fatal("STLB page number is wrong!");
    *access = stlbp->access[ring];
    if (((intacc & *access) != intacc) || (intacc == PACC && ((*access & 3) == 0)))
      fault(ACCESSFAULT, 0, ea);
    if (intacc == WACC && stlbp->unmodified) {
      stlbp->unmodified = 0;
      //*(stlbp->pmep) &= ~020000;    /* reset unmodified bit in memory */
    }
    pa = (stlbp->ppn << 10) | (ea & 0x3FF);
  } else {
    pa = ea & 0x3FFFFF;
  }
  if (T_MAP) fprintf(stderr,"        for ea %o/%o, stlbix=%d, pa=%o	loaded at #%d\n", ea>>16, ea&0xffff, stlbix, pa, stlbp->load_ic);
  if (pa <= MEMSIZE)
    return pa;
  printf(" map: Memory address %o (%o/%o) is out of range!\n", ea, ea>>16, ea & 0xffff);
  /* NOTE: could also take a missing memory check here... */
}


/* Can V-mode do JMP# 4 to start executing from the register file?
   Might need to "or" 0x80000000 with RPH to indicate this?? */ 

unsigned short get16(ea_t ea) {
  pa_t pa;
  unsigned short access;

  /* check for live register access */

  if (ea & 0x80000000 || (!(crs[KEYS] & 010000) && (ea & 0xFFFF) < 040)) {
    ea = ea & 0xFFFF;
    if (ea < 7)
      return crs[memtocrs[ea]];
    if (ea == 7)                   /* PC */
      return RPL;
    RESTRICT();
    if (ea < 020)                 /* CRS */
      return crs[memtocrs[ea]];
    if (ea < 040)                 /* DMX */
      return regs.sym.regdmx[((ea & 036) << 1) | (ea & 1)];
    printf(" Live register address %o too big!\n", ea);
    fatal(NULL);
  }
  return mem[mapva(ea, RACC, &access)];
}

unsigned int get32(ea_t ea) {
  pa_t pa;
  unsigned short access;
  unsigned short m[2];

  pa = mapva(ea, RACC, &access);

  if ((pa & 01777) <= 01776)
    return *(unsigned int *)(mem+pa);
  else {
    m[0] = mem[pa];
    m[1] = get16(INCVA(ea,1));
    return *(unsigned int *)m;
  }
}

double get64(ea_t ea) {
  pa_t pa;
  unsigned short access;
  unsigned short m[4];

  pa = mapva(ea, RACC, &access);
  if ((pa & 01777) <= 01774)
    return *(double *)(mem+pa);
  else {
    m[0] = mem[pa];
    m[1] = get16(INCVA(ea,1));
    m[2] = get16(INCVA(ea,2));
    m[3] = get16(INCVA(ea,3));
    return *(double *)m;
  }
}


put16(unsigned short value, ea_t ea) {
  pa_t pa;
  unsigned short access;

  if (ea & 0x80000000) {         /* flag for live register address */
    ea = ea & 0x7FFFFFFF;
    if (ea < 7)
      crs[memtocrs[ea]] = value;
    else if (ea == 7) {
      RPL = value;
    } else {
      RESTRICT();
      if (ea <= 017)                      /* CRS */
	crs[memtocrs[ea]] = value;
      else if (ea <= 037)                 /* DMX */
	regs.sym.regdmx[((ea & 036) << 1) | (ea & 1)] = value;
      else {
	printf(" Live register store address %o too big!\n", ea);
	exit(1);
      }
    }
  } else {
    pa = mapva(ea, WACC, &access);
    mem[pa] = value;
  }
}

put32(unsigned int value, ea_t ea) {
  pa_t pa;
  unsigned short access;
  unsigned short *m;

  pa = mapva(ea, WACC, &access);
  if ((pa & 01777) <= 01776)
    *(unsigned int *)(mem+pa) = value;
  else {
    m = (void *)&value;
    mem[pa] = m[0];
    put16(m[1], INCVA(ea,1));
  }
}

put64(double value, ea_t ea) {
  pa_t pa;
  unsigned short access;
  unsigned short *m;

  pa = mapva(ea, WACC, &access);
  if ((pa & 01777) <= 01774)
    *(double *)(mem+pa) = value;
  else {
    m = (void *)&value;
    mem[pa] = m[0];
    put16(m[1], INCVA(ea,1));
    put16(m[2], INCVA(ea,2));
    put16(m[3], INCVA(ea,3));
  }
}


void calf(ea_t ea) {
  ea_t pcbp, stackfp, csea;
  unsigned short first,next,last,this;
  unsigned short cs[6];

  pcbp = *(ea_t *)(crs+OWNER);    /* my pcb pointer */

  /* get concealed stack entry address */

  first = get16(pcbp+PCBCSFIRST);
  next = get16(pcbp+PCBCSNEXT);
  last = get16(pcbp+PCBCSLAST);
  if (T_FAULT) fprintf(stderr,"CALF: first=%o, next=%o, last=%o\n", first, next, last);
  if (next == first)
    this = last;
  else
    this = next-6;
  csea = MAKEVA(crs[OWNERH], this);
  if (T_FAULT) fprintf(stderr,"CALF: cs frame is at %o/%o\n", csea>>16, csea&0xFFFF);

  /* make sure ecb specifies zero arguments */

  if (get16(ea+5) !=0) {
    printf("CALF ecb at %o/%o has arguments!\n", ea>>16, ea&0xFFFF);
    fatal(NULL);
  }

  pcl(ea);

  /* get the concealed stack entries and adjust the new stack frame */

  *(unsigned int *)(cs+0) = get32(csea+0);
  *(double *)(cs+2) = get64(csea+2);

  if (T_FAULT) fprintf(stderr,"CALF: cs entry: retpb=%o/%o, retkeys=%o, fcode=%o, faddr=%o/%o\n", cs[0], cs[1], cs[2], cs[3], cs[4], cs[5]);

  stackfp = *(unsigned int *)(crs+SB);
  put16(1, stackfp+0);                          /* flag it as CALF frame */
  put32(*(unsigned int *)(cs+0), stackfp+2);    /* return PB */
  put16(cs[2], stackfp+8);                      /* return keys */
  put16(cs[3], stackfp+10);                     /* fault code */
  put32(*(unsigned int *)(cs+4), stackfp+11);   /* fault address */

  /* pop the concealed stack */

  put16(this, pcbp+PCBCSNEXT);
}


/* set new processor keys */

newkeys (unsigned short new) {

  switch ((new & 016000) >> 10) {
  case 0:                     /* 16S */
    if (T_MODE) fprintf(stderr,"Entering 16S mode, keys=%o\n", new);
    amask = 037777;
    break;
  case 1:                     /* 32S */
    if (T_MODE) fprintf(stderr,"Entering 32S mode, keys=%o\n", new);
    amask = 077777;
    break;
  case 2:                     /* 64R */
    if (T_MODE) fprintf(stderr,"Entering 64R mode, keys=%o\n", new);
    amask = 0177777;
    break;
  case 3:                     /* 32R */
    if (T_MODE) fprintf(stderr,"Entering 32R mode, keys=%o\n", new);
    amask = 077777;
    break;
  case 4:                     /* 32I */
    if (T_MODE) fprintf(stderr,"Entering 32I mode, keys=%o\n", new);
    amask = 0177777;
    break;
  case 6:                     /* 64V */
    if (T_MODE) fprintf(stderr,"Entering 64V mode, keys=%o\n", new);
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
  
#if 0
  if (T_FAULT) printf("#%d: fault '%o, fcode=%o, faddr=%o/%o, faultrp=%o/%o\n", instcount, fvec, fcode, faddr>>16, faddr&0xFFFF, faultrp>>16, faultrp&0xFFFF);
#endif
  if (T_FAULT) fprintf(stderr,"fault '%o, fcode=%o, faddr=%o/%o, faultrp=%o/%o\n", fvec, fcode, faddr>>16, faddr&0xFFFF, faultrp>>16, faultrp&0xFFFF);

  if (crs[MODALS] & 010) {   /* process exchange is enabled */
    ring = (RPH>>13) & 3;                     /* save current ring */
    RP = 0;                                   /* force us to ring 0 for access */
    pcbp = *(ea_t *)(crs+OWNER);
    if (fvec == PROCESSFAULT || fvec == ACCESSFAULT || fvec == STACKFAULT || fvec == SEGFAULT)
      pxfvec = get32(pcbp+PCBFVR0);           /* use R0 handler */
    else if (fvec == PAGEFAULT)
      pxfvec = get32(pcbp+PCBFVPF);
    else
      /* NOTE: probably need to weaken ring field on pxfvec in this case */
      pxfvec = get32(pcbp+PCBFVEC+2*ring);    /* use current ring handler */

    /* push a concealed stack entry */

    first = get16(pcbp+PCBCSFIRST);
    next = get16(pcbp+PCBCSNEXT);
    last = get16(pcbp+PCBCSLAST);
    if (T_FAULT) fprintf(stderr,"fault: PX enabled, pcbp=%o/%o, cs first=%o, next=%o, last=%o\n", pcbp>>16, pcbp&0xFFFF, first, next, last);
    if (next > last) {
      if (T_FAULT) fprintf(stderr, "CALF: Concealed stack wraparound to first");
      next = first;
    }
    csea = MAKEVA(crs[OWNERH], next);
    put32(faultrp, csea);
    put16(crs[KEYS], csea+2);
    put16(fcode, csea+3);
    put32(faddr, csea+4);
    put16(next+6, pcbp+PCBCSNEXT);
    if (T_FAULT) fprintf(stderr,"fault: updated cs next=%o\n", get16(pcbp+PCBCSNEXT));

    /* update RP to jump to the fault vector in the fault table */

    RP = pxfvec + (fvec-062)*4;
    newkeys(014000);      /* V-mode */
    inhcount = 1;         /* supposed to do this only for Ring 0, but shouldn't hurt */

    if (T_FAULT && fvec == POINTERFAULT) {
      ea = get32(faddr);
      if ((ea & 0xF0000000) == 0x80000000) {
	ea &= 0x0FFFFFFF;
	namlen = get16(ea);
	for (i=0; i<(namlen+1)/2; i++)
	  name[i] = get16(ea+i+1) & 0x7f7f;
	name[i] = 0;
	fprintf(stderr, "fault: DYNT addr=%o/%o, length=%d, name=%s\n", ea>>16, ea&0xffff, namlen, name);
      }
    }

    if (T_FAULT) fprintf(stderr, "fault: jumping to fault table entry at RP=%o/%o\n", RPH, RPL);

  } else {                   /* process exchange is disabled */
    //printf("fault '%o occurred at %o/%o, instruction=%o, modals=%o\n", fvec, faultrp>>16, faultrp&0xffff, get16(faultrp), crs[MODALS]);
    /* need to check for standard/vectored interrupt mode here... */
    m = get16(fvec);
    if (m != 0) {
      if (1 || T_FLOW) fprintf(stderr," fault JST* '%o [%o]\n", fvec, m);
      put16(faultrp & 0xFFFF, m);
      RP = m;
      RPL++;
    } else {
      printf("#%d: fault '%o, fcode=%o, faddr=%o/%o, faultrp=%o/%o\n", instcount, fvec, fcode, faddr>>16, faddr&0xFFFF, faultrp>>16, faultrp&0xFFFF);
      fatal("Fault vector is zero, process exchange is disabled.");
    }
  }

  longjmp(jmpbuf, 1);
  fatal("fault: returned after longjmp\n");
}


fatal(char *msg) {
  printf("Fatal error: instruction #%d at %o/%o: %o %o\n", instcount, prevpc >> 16, prevpc & 0xFFFF, get16(prevpc), get16(prevpc+1));
  if (msg)
    printf("%s\n", msg);
  printf("keys = %o, modals=%o\n", crs[KEYS], crs[MODALS]);
  /* should do a register dump, RL dump, PCB dump, etc. here... */
  exit(1);
}
    
/* I/O device map table, containing function pointers to handle device I/O */

long devpoll[64] = {0};

#include "emdev.h"

void (*devmap[64])(short, short, short) = {
  0,0,0,0,devasr,0,0,0,
  0,0,0,devmt,devmt,0,0,0,
  devcp,0,0,0,0,0,devdisk,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0};

/* read a short (16-bit) integer in big-endian from stdin */

unsigned short readshort () {

  return getchar()<<8 | getchar();
}




/* NOTE: This code is untested! */

ea_t ea16s (unsigned short inst, short i, short x) {
  
  unsigned short ea,m,rpl;
  ea_t va;

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
    /* NOTE: this test is already in get16... */
    if (ea < 040)
      m = get16(0x80000000|ea);
    else
      m = get16(ea);
    i = m & 0100000;
    x = m & 040000;
    ea = m & 037777;                             /* go indirect */
  }
  va = MAKEVA(RPH, ea);
  if (ea < 040)                                  /* flag live register ea */
    return va | 0x80000000;
  return va;
}


/* NOTE: This code is untested! */

ea_t ea32s (unsigned short inst, short i, short x) {
  
  unsigned short ea,m,rpl;
  ea_t va;

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
      m = get16(ea);
    i = m & 0100000;
    ea = m & 077777;                             /* go indirect */
  }
  if (x)                                         /* postindex */
    ea += crs[X];
  ea &= amask;
  va = MAKEVA(RPH, ea);
  if (ea < 040)                                  /* flag live register ea */
    return va | 0x80000000;
  return va;
}


/* NOTE: the difference between 32R and 64R, besides the extra address
   bit, is that 32R indirect words have an indirect bit for multi-level
   indirects */

ea_t ea32r64r (ea_t earp, unsigned short inst, short i, short x, short *opcode) {

  short class;
  unsigned short ea,m,rpl;
  ea_t va;

  rpl = earp;
  if (T_EAR) fprintf(stderr," ea32r64r: i=%o, x=%o\n", i!= 0, x!=0);
  if (inst & 001000)                             /* sector bit 7 set? */
    if ((inst & 0760) != 0400) {                 /* PC relative? */
      ea = rpl + (((short) (inst << 7)) >> 7);   /* yes, sign extend D */
      if (T_EAR) fprintf(stderr," PC relative, P=%o, new ea=%o\n", rpl, ea);
    }
    else 
      goto special;                              /* special cases */
  else {
    ea = (inst & 0777);                          /* sector 0 */
    if (T_EAR) fprintf(stderr," Sector 0, new ea=%o\n", ea);
    if (ea < 0100 && x) {                        /* preindex by X */
      if (T_EAR) fprintf(stderr," Preindex, ea=%o, X='%o/%d\n", ea, crs[X], *(short *)(crs+X));
      ea += crs[X];
      if (T_EAR) fprintf(stderr," Preindex, new ea=%o\n", ea);
      x = 0;
    }
  }
  while (i) {
    if (ea < 040)
      m = get16(0x80000000|ea);
    else
      m = get16(ea);
    if (T_EAR) fprintf(stderr," Indirect, old ea=%o, [ea]=%o\n", ea, m);
    if ((crs[KEYS] & 016000) == 06000)           /* 32R mode? */
      i = m & 0100000;                           /* yes, multiple indirects */
    else
      i = 0;                                     /* no, 64R mode, single indirect */
    ea = m & amask;                              /* go indirect */
    if (T_EAR) fprintf(stderr," Indirect, new i=%d, new ea=%o\n", i!=0, ea);
  }
  if (x) {
    if (T_EAR) fprintf(stderr," Postindex, old ea=%o, X='%o/%d\n", ea, crs[X], *(short *)(crs+X));
    ea += crs[X];
    if (T_EAR) fprintf(stderr," Postindex, new ea=%o\n", ea);
  }
  ea &= amask;
  va = MAKEVA(RPH, ea);
  if (ea < 040)                                  /* flag live register ea */
    return va | 0x80000000;
  return va;

special:
  class = inst & 3;                              /* class bits = 15 & 16 */
  *opcode = *opcode | ((inst >> 2) & 3);         /* opcode extension */
  if (T_EAR) fprintf(stderr," special, new opcode=%5#0o, class=%d\n", *opcode, class);

  if (class < 2) {                               /* class 0/1 */
    ea = get16(RPL++);                           /* get A from next word */
    if (T_EAR) fprintf(stderr," Class %d, new ea=%o\n", class, ea);
    if (class == 1)
      ea += crs[S];
    if (x) {
      if (T_EAR) fprintf(stderr," Preindex, ea=%o, X='%o/%d\n", ea, crs[X], *(short *)(crs+X));
      ea += crs[X];
      if (T_EAR) fprintf(stderr," Preindex, new ea=%o\n", ea);
    }
    while (i) {
      if (ea < 040)
	m = get16(0x80000000|ea);
      else
	m = get16(ea);
      if (T_EAR) fprintf(stderr," Indirect, old ea=%o, [ea]=%o\n", ea, m);
      if ((crs[KEYS] & 016000) == 06000)
	i = m & 0100000;
      else
	i = 0;
      ea = m & amask;
      if (T_EAR) fprintf(stderr," Indirect, new i=%d, new ea=%o\n", i!=0, ea);
    }

  } else if (i && x) {                           /* class 2/3, ix=11 */
    if (T_EAR) fprintf(stderr," class 2/3, ix=11\n");
    ea = get16(RPL++);                           /* get A from next word */
    if (T_EAR) fprintf(stderr," ea=%o\n", ea);
    if (class == 3)
      ea += (short) crs[S];
    while (i) {
      if (ea < 040)
	m = get16(0x80000000|ea);
      else
	m = get16(ea);
      if (T_EAR) fprintf(stderr," Indirect, ea=%o, [ea]=%o\n", ea, m);
      if ((crs[KEYS] & 016000) == 06000)
	i = m & 0100000;
      else
	i = 0;
      ea = m & amask;
      if (T_EAR) fprintf(stderr," Indirect, new i=%d, new ea=%o\n", i!=0, ea);
    }
    if (T_EAR) fprintf(stderr," Postindex, old ea=%o, X='%o/%d\n", ea, crs[X], *(short *)(crs+X));
    ea += (short) crs[X];
    if (T_EAR) fprintf(stderr," Postindex, new ea=%o\n", ea);

  } else {                                       /* class 2/3, ix != 11 */
    if (class == 2)
      ea = crs[S]++;
    else
      ea = --crs[S];
    if (T_EAR) fprintf(stderr," Class 2/3, new ea=%o, new S=%o\n", ea, crs[S]);
    if (x) {
      if (ea < 040)
	m = get16(0x80000000|ea);
      else
	m = get16(ea);
      if ((crs[KEYS] & 016000) == 06000)
	i = m & 0100000;
      ea = m & amask;
    }
    while (i) {
      if (ea < 040)
	m = get16(0x80000000|ea);
      else
	m = get16(ea);
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
  va = MAKEVA(RPH, ea);
  if (ea < 040)                                  /* flag live register ea */
    return va | 0x80000000;
  return va;
}

#include "ea64v.h"

unsigned int ea32i (ea_t earp, unsigned short inst, short i, short x) {
  if (T_EAI) fprintf(stderr,"Mode 32I not implemented\n");
}


ea_t apea(unsigned short *bitarg) {
  unsigned short ibr, ea_s, ea_w, bit, br;
  ea_t ea;
  
  ibr = get16(RP);
  RPL++;
  bit = (ibr >> 12) & 0xF;
  br = (ibr >> 8) & 3;
  if (T_EAAP) fprintf(stderr," AP ibr=%o, br=%d, i=%d, bit=%d\n", ibr, br, (ibr & 004000) != 0, bit);

  /* XXX: should ea ring be weakened with RP ring? */

  ea_s = crs[PBH + 2*br];
  ea_w = crs[PBL + 2*br];
  ea_w += get16(RP);
  RPL++;
  ea = MAKEVA(ea_s, ea_w);
  if (T_EAAP) fprintf(stderr," AP ea = %o/%o  %s\n", ea_s, ea_w, searchloadmap(ea));
  if (ibr & 004000) {
    if (ea & 0x80000000)
      fault(POINTERFAULT, ea>>16, 0);
    ea = get32(ea);
    if (ea & EXTMASK32)
      bit = get16(INCVA(ea,2)) >> 12;
    else
      bit = 0;
    if (T_EAAP) fprintf(stderr," After indirect, AP ea = %o/%o, bit=%d  %s\n", ea>>16, ea & 0xFFFF, bit, searchloadmap(ea));
  }
  if (bit)
    ea |= EXTMASK32;
  if (bitarg != NULL)
    *bitarg = bit;
  return ea;
}

#define GETFLR(n) (((crsl[FLR0+2*(n)] >> 11) & 0x1FFFE0) | (crsl[FLR0+2*(n)] & 0x1F))
#define PUTFLR(n,v) crsl[FLR0+2*(n)] = (((v) << 11) & 0xFFFF0000) | (crsl[FLR0+2*(n)] & 0xF000) | ((v) & 0x1F);

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

  /* dump sector zero for debugging */

  fprintf(stderr,"\nSector 0:\n");
  for (ea=0; ea<01000; ea=ea+8)
    if (mem[ea]|mem[ea+1]|mem[ea+2]|mem[ea+3]|mem[ea+4]|mem[ea+5]|mem[ea+6]|mem[ea+7])
      fprintf(stderr,"%3o: %6o %6o %6o %6o %6o %6o %6o %6o\n", ea, mem[ea], mem[ea+1], mem[ea+2], mem[ea+3], mem[ea+4], mem[ea+5], mem[ea+6], mem[ea+7]);
    
  /* dump main memory for debugging */

  fprintf(stderr,"\nMain memory:\n");
  for (ea=start; ea<=end; ea=ea+8)
    if (mem[ea]|mem[ea+1]|mem[ea+2]|mem[ea+3]|mem[ea+4]|mem[ea+5]|mem[ea+6]|mem[ea+7])
      fprintf(stderr,"%o: %6o %6o %6o %6o %6o %6o %6o %6o\n", ea, mem[ea], mem[ea+1], mem[ea+2], mem[ea+3], mem[ea+4], mem[ea+5], mem[ea+6], mem[ea+7]);
}


dumpsegs() {
  short seg,nsegs,i,page,segno;
  unsigned short pte,xxx;
  unsigned int dtar,staddr,sdw,ptaddr,pmaddr;

  for (i=0; i<4; i++) {
    dtar = *(unsigned int *)(crs+DTAR0-2*i);  /* get dtar register */
    nsegs = 1024-(dtar>>22);
    staddr = (dtar & 0x003F0000) | ((dtar & 0xFFFF)<<1);
    fprintf(stderr,"DTAR %d: register=%o, size=%d, seg table addr=%o\n", i, dtar, nsegs, staddr);
    for (seg=0; seg<nsegs; seg++) {
      segno = (i<<10)+seg;
      sdw = *(unsigned int *)(mem+staddr);
      ptaddr = ((sdw & 0x3F)<<10) | (sdw>>22);
      fprintf(stderr,"Segment '%o: F=%d, R1:%o R3:%o PT = %o\n", segno, (sdw>>15)&1, (sdw>>12)&7, (sdw>>6)&7, ptaddr);
      xxx = (sdw>>16)&0x3F;
      if (xxx != 0) fprintf(stderr,"WARNING: X=%o\n", xxx);
      if (ptaddr != 0)
	for (page=0; page<64; page++) {
	  pmaddr = (ptaddr<<6) + page;
	  pte = mem[pmaddr];
	  fprintf(stderr," Seg %o page %d: pmaddr=%o, V=%d R=%d U=%d S=%d PPA=%o\n", segno, page, pmaddr, pte>>15, (pte>>14)&1, (pte>>13)&1, (pte>>12)&1, pte&0xFFF);
	}
      staddr += 2;
    }
  }
}


unsigned short dumppcb(unsigned short pcb) {
  short i;
  unsigned short nextpcb;
  ea_t ea;

  ea = MAKEVA(crs[OWNERH],pcb);
  fprintf(stderr,"PCB %06o:\n", pcb);
  fprintf(stderr,"  Level: %o\n", get16(ea+0));
  nextpcb = get16(ea+1);
  fprintf(stderr,"  Link: %o\n", nextpcb);
  fprintf(stderr,"  Wait list: %o/%o\n", get16(ea+2), get16(ea+3));
  fprintf(stderr,"  Abort flags: %o\n", get16(ea+4));
  fprintf(stderr,"  CPU flags: %o\n", get16(ea+5));
  fprintf(stderr,"  6,7 (reserved): %o %o\n", get16(ea+6), get16(ea+7));
  fprintf(stderr,"  Elapsed timers: %d %d\n", get16(ea+8), get16(ea+9));
  fprintf(stderr,"  DTAR 2 & 3: %o|%o  %o|%o\n", get16(ea+10), get16(ea+11), get16(ea+12), get16(ea+13));
  fprintf(stderr,"  Process interval timer: %o\n", get16(ea+14));
  fprintf(stderr,"  15 (reserved): %o\n", get16(ea+15));
  fprintf(stderr,"  Save mask: %o\n", get16(ea+16));
  fprintf(stderr,"  Keys: %o\n", get16(ea+17));
  for (i=0; i<16; i++) {
    fprintf(stderr,"  %06o %06o", get16(ea+18+2*i), get16(ea+19+2*i));
    if (i==7 || i==15)
      fprintf(stderr,"\n");
  }
  fprintf(stderr,"  R0 Fault vec: %o/%o\n", get16(ea+50), get16(ea+51));
  fprintf(stderr,"  R1 Fault vec: %o/%o\n", get16(ea+52), get16(ea+53));
  fprintf(stderr,"  R2 Fault vec: %o/%o\n", get16(ea+54), get16(ea+55));
  fprintf(stderr,"  R3 Fault vec: %o/%o\n", get16(ea+56), get16(ea+57));
  fprintf(stderr,"  PG Fault vec: %o/%o\n", get16(ea+58), get16(ea+59));
  fprintf(stderr,"  Conc. Stack Hdr: %o %o %o\n", get16(ea+60), get16(ea+61), get16(ea+62));
  fprintf(stderr,"\n");
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
    if (T_INST) fprintf(stderr," no room for frame, extension pointer is %o/%o\n", stackfp>>16, stackfp&0xFFFF);
  }
  if (stackfp == 0)
    fault(STACKFAULT, 0, MAKEVA(stackrootseg,0) | (RP & RINGMASK32));

  /* update the stack free pointer */

  put32((stackfp+extsize) & ~RINGMASK32, stackrootp);
  if (T_INST) fprintf(stderr," stack extension is at %o/%o\n", stackfp>>16, stackfp&0xffff);
  return stackfp;
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
  if (T_PCL) fprintf(stderr," PCLAP ibr=%o, br=%d, i=%d, bit=%d, store=%d, lastarg=%d, a=%o\n", ibr, br, (ibr & 004000) != 0, bit, (*store != 0), (*lastarg != 0), a);
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
  if (T_PCL) fprintf(stderr," PCLAP ea = %o/%o, bit=%d\n", ea_s, ea_w, bit);
  if (ibr & 004000) {             /* indirect */
    if (ea & 0x80000000)
      fault(POINTERFAULT, ea>>16, 0);
    iwea = ea;
    ea = get32(iwea) | (RP & RINGMASK32);
    if (T_PCL) fprintf(stderr," Indirect pointer is %o/%o\n", ea>>16, ea & 0xFFFF);

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

#if 1
    if (ea & 0x80000000)
      if ((ea & 0xFFFF0000) != 0x80000000)
	if ((ea & 0x1FFF0000) || ((RP & RINGMASK32) <= (ea & RINGMASK32)))
	  fault(POINTERFAULT, ea>>16, iwea);
#endif
    bit = 0;
#if 0
    /* CPU.PCL Case 33 shows that the bit field is not stored in the stack frame
       for a 3-word indirect pointer, even though the E bit remains set */
    if (ea & EXTMASK32)
      bit = get16(ea+2) >> 12;
#endif
    if (T_PCL) fprintf(stderr," After indirect, PCLAP ea = %o/%o, bit=%d\n", ea>>16, ea & 0xFFFF, bit);
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
  short i;
  short access;
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

  /* get segment access; mapva ensures either read or gate */

  pa = mapva(ecbea, PACC, &access);
  if (T_PCL) fprintf(stderr," ecb @ %o/%o, access=%d\n", ecbea>>16, ecbea&0xFFFF, access);

#if 0
  if (strstr(searchloadmap(ea),"TNOU")) {
    fprintf(stderr," TNOUx called!\n");
  }
#endif

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

  if (T_PCL) fprintf(stderr," ecb.pb: %o/%o\n ecb.framesize: %d\n ecb.stackroot %o\n ecb.argdisp: %o\n ecb.nargs: %d\n ecb.lb: %o/%o\n ecb.keys: %o\n", ecb[0], ecb[1], ecb[2], ecb[3], ecb[4], ecb[5], ecb[6], ecb[7], ecb[8]);

  newrp = *(unsigned int *)(ecb+0);
  if (access != 1)
#if 0
    newrp = (newrp & ~RINGMASK32) | (RP & RINGMASK32);  /* no ring change */
#else
    newrp = newrp | (RP & RINGMASK32);    /* Case 24 indicates to weaken ring */
#endif

  /* setup stack frame */

  stackrootseg = ecb[3];
  if (stackrootseg == 0) {
    stackrootseg = get16((*(unsigned int *)(crs+SB)) + 1);
    if (T_PCL) fprintf(stderr," stack root in ecb was zero, stack root from caller is %o\n", stackrootseg);
  }
  if (stackrootseg == 0)
    fatal("Stack base register root segment is zero");
  stackfp = get32(MAKEVA(stackrootseg,0));
  if (stackfp == 0)
    fatal("Stack free pointer is zero");
  if (T_PCL) fprintf(stderr," stack free pointer: %o/%o, current ring=%o\n", stackfp>>16, stackfp&0xFFFF, RPH&RINGMASK16);
  stacksize = ecb[2];

  /* if there isn't room for this frame, check the stack extension
     pointer */

  if ((stackfp & 0xFFFF) + stacksize > 0xFFFF) {
    stackfp = get32(MAKEVA(stackrootseg,2));
    if (T_PCL) fprintf(stderr," no room for frame, extension pointer is %o/%o\n", stackfp>>16, stackfp&0xFFFF);

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
  put16(0, stackfp);
  put16(stackrootseg, stackfp+1);
  put32(RP, stackfp+2);
  put32(*(unsigned int *)(crs+SB), stackfp+4);
  put32(*(unsigned int *)(crs+LB), stackfp+6);
  put16(crs[KEYS], stackfp+8);
  put16(RPL, stackfp+9);

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

  if (T_PCL) fprintf(stderr," before update, stackfp=%o/%o, SB=%o/%o\n", stackfp>>16, stackfp&0xFFFF, crs[SBH], crs[SBL]);
  if (access == 1)
    *(unsigned int *)(crs+SB) = stackfp;
  else
    *(unsigned int *)(crs+SB) = (stackfp & ~RINGMASK32) | (RP & RINGMASK32);
  if (T_PCL) fprintf(stderr," new SB=%o/%o\n", crs[SBH], crs[SBL]);
  *(unsigned int *)(crs+LB) = *(unsigned int *)(ecb+6);
  newkeys(ecb[8] & 0177760);
  RP = newrp;
  if (T_PCL) fprintf(stderr," new RP=%o/%o\n", RPH, RPL);

  /* update the stack free pointer; this has to wait until after all
     memory accesses, in case of stack page faults (PCL restarts).
     Some ucode versions incorrectly store the ring in the free
     pointer if the extension pointer was followed.  Set EHDB to
     suppress this spurious DIAG error. */

  ea = MAKEVA(stackrootseg,0) | (newrp & RINGMASK32);
  put32((stackfp+stacksize) & ~RINGMASK32, ea);

  /* transfer arguments if arguments are expected.  There is no
     documentation explaining how the Y register is used during
     argument transfer, so:
     Y(high) = stack frame offset to store next argument
     Y(low) = number of arguments left to transfer (JW hack!) */

  if (ecb[5] > 0) {
    crs[Y] = ecb[4];
    crs[YL] = ecb[5];
    argt();
  }
}

/* for ARGT:
   Registers:
   - RP points to the ARGT instruction
   - SB points to the new stack frame
   - LB is for the called procedure
   - Y is new frame offset of the next argument
   - YL is the number of arguments left to transfer (HACK!)
   Stack frame:
   - PB points to the next argument template to be evaluated
   - SB is the caller's saved SB
   - LB is the caller's saved LB
*/

argt() {
  unsigned short brsave[6];
  unsigned short argsleft, argdisp, lastarg, store,bit;
  unsigned int utempl;
  unsigned short ecby;          /* last offset where ecb temp ea was stored */
  ea_t ea, stackfp, rp, ecbea;
  unsigned short advancepb, advancey;

  if (T_PCL) fprintf(stderr,"Entered ARGT\n");

  /* stackfp is the new stack frame, rp is in the middle of
     argument templates and is advanced after each transfer */

  stackfp = *(unsigned int *)(crs+SB);
  rp = get32(stackfp+2);

  /* reload the caller's base registers for EA calculations */
  
  brsave[0] = rp >> 16;    brsave[1] = 0;
  *(double *)(brsave+2) = get64(stackfp+4);
  
  argdisp = crs[Y];
  argsleft = crs[YL];
  lastarg = 0;         /* true when AP template with L bit is seen */
  while (argsleft > 0 || !lastarg) {

    if (T_PCL) fprintf(stderr," Transferring arg, %d left, Y=%o\n", argsleft, crs[Y]);

    advancey = 0;
    if (lastarg) {
      ea = 0x80000000;
      store = 1;
      advancepb = 0;
    } else {
      ea = pclea(brsave, rp, &bit, &store, &lastarg) | (RP & RINGMASK32);
      advancepb = 1;
    }
    if (argsleft > 0 && store) {
      if (T_PCL) fprintf(stderr," Storing arg, %d left, Y=%o\n", argsleft, crs[Y]);

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
	put32(ea, stackfp+crs[Y]);
      } else {
	put32(ea, stackfp+crs[Y]);
	if (ea & EXTMASK32)
	  put16(bit<<12, stackfp+crs[Y]+2);
      }
      if (T_PCL) fprintf(stderr," Stored\n");
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
    }
    if (advancey) {
      crs[Y] += 3;
      crs[YL]--;
    }
  }

  /* after argument transfer, advance real RP past ARGT */

  RPL++;
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
  newkeys(keys & 0177760);
  if (T_INST) fprintf(stderr," Finished PRTN, RP=%o/%o\n", RPH, RPL);
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
  for (i=(wait?12:0); i<16; i++) {
    if (crsl[i] != 0) {
      mask |= bitmask16[i+1];
      put32(crsl[i], regp);
      regp += 2;
    }
  }
  put16(mask, pcbp+PCBMASK);
  crs[KEYS] |= 1;
  put16(crs[KEYS], pcbp+PCBKEYS);
}

/* pxregload: load pcbp's registers from their pcb to the current
   register set, set OWNERL

   NOTE: RP must be set by the caller since this happens whenever
   a process is dispatched - not just when registers are loaded */

pxregload (ea_t pcbp) {
  ea_t regp;
  unsigned short i, mask;

  if (T_PX) fprintf(stderr,"pxregload loading registers for process %o/%o\n", pcbp>>16, pcbp&0xFFFF);
  regp = pcbp+PCBREGS;
  mask = get16(pcbp+PCBMASK);
  for (i=0; i<16; i++) {
    if (mask & bitmask16[i+1]) {
      crsl[i] = get32(regp);
      regp += 2;
    } else {
      crsl[i] = 0;
    }
  }
  newkeys(get16(pcbp+PCBKEYS));
  *(unsigned int *)(crs+DTAR2) = get32(pcbp+PCBDTAR2);
  *(unsigned int *)(crs+DTAR3) = get32(pcbp+PCBDTAR3);
  crs[OWNERL] = pcbp & 0xFFFF;

  if (T_PX) fprintf(stderr,"pxregload: registers loaded, ownerl=%o\n", crs[OWNERL]);
}


/* switch to the other register set (2 vs 3) */

ors() {
  unsigned short rsnum;
  unsigned short modals;

  /* only bit 11 of crs in modals is important for register set */

  if (T_PX) fprintf(stderr,"ors: current modals = %o, register set = %d\n", crs[MODALS], (crs[MODALS] & 0340)>>5);
  modals = (crs[MODALS] ^ 040) | 0100;
  rsnum = 2+((modals & 040) >> 5);
  if (T_PX) fprintf(stderr,"ors: new modals = %o, register set = %d\n", modals, rsnum);
  crs = regs.rs16[rsnum];
  crsl = (void *)crs;
  crs[MODALS] = modals;
  if (T_PX) fprintf(stderr,"ors: new register set = %d\n", (crs[MODALS] & 0340)>>5);
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


  if (regs.sym.pcba != 0) {
    pcbp = MAKEVA(crs[OWNERH], regs.sym.pcba);
    regs.sym.pla = get16(pcbp+PCBLEV);
    if (T_PX) fprintf(stderr,"disp: dispatching PPA, pcba=%o, pla=%o\n", regs.sym.pcba, regs.sym.pla);

  } else if (regs.sym.pcbb != 0) {
    pcbp = MAKEVA(crs[OWNERH], regs.sym.pcbb);
    regs.sym.pcba = regs.sym.pcbb;
    regs.sym.pla = get16(pcbp+PCBLEV);
    regs.sym.pcbb = 0;
    if (T_PX) fprintf(stderr,"disp: dispatching PPB, pcba=%o, pla=%o\n", regs.sym.pcba, regs.sym.pla);

  } else {
    if (T_PX) fprintf(stderr,"disp: scanning RL\n");
    if (regs.sym.pla != 0)
      rlp = MAKEVA(crs[OWNERH], regs.sym.pla);
    else if (regs.sym.plb != 0)
      rlp = MAKEVA(crs[OWNERH], regs.sym.plb);
    else
      fatal("dispatch: both pla and plb are zero; can't locate ready list");
    while(1) {
      rlbol = get16(rlp);
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
  if (T_PX) fprintf(stderr,"disp: process %o/%o selected\n", pcbp>>16, pcbw);
  
  /* pcbp now points to the process we're going to run.  By
     definition, this process should not be on any wait lists,
     so pcb.waitlist(seg) should be zero.  Check it */

  utempa = get16(pcbp+PCBWAIT);
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
    if (T_PX) fprintf(stderr,"disp: register set already owned by %o - no save\n", crs[OWNERL]);
  } else {
    if (T_PX) fprintf(stderr,"disp: saving registers owned by %o\n", crs[OWNERL]);
    pxregsave(0);
    pxregload(pcbp);
  }

  crs[TIMER] = get16(pcbp+PCBIT);
  RP = *(unsigned int *)(crs+PB);
  crs[PBL] = 0;
  crs[KEYS] &= ~3;                           /* erase "in dispatcher" and "save done" */
  if (T_PX) fprintf(stderr,"disp: returning from dispatcher, running process %o/%o at %o/%o\n", crs[OWNERH], crs[OWNERL], RPH, RPL);

  /* if this process' abort flags are set, process fault */

  utempa = get16(pcbp+PCBABT);
  if (utempa != 0) {
    if (T_PX) fprintf(stderr,"dispatch: abort flags for %o are %o\n", crs[OWNERL], utempa);
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
  rl = get32(rlp);
  bol = rl >> 16;
  eol = rl & 0xFFFF;
  if (bol != (pcbp & 0xFFFF)) {
    printf("rlp=%o/%o, bol=%o, pcbp=%o/%o, pla=%o, pcba=%o\n", rlp>>16, rlp&0xFFFF, bol, pcbp>>16, pcbp&0xFFFF, regs.sym.pla, regs.sym.pcba);
    fatal("unready: I'm not first on the ready list");
  }
  if (bol == eol) {
    bol = 0;
    eol = 0;
  } else {
    bol = get16(pcbp+1);
  }
  rl = (bol<<16) | eol;
  put32(rl, rlp);         /* update ready list */
  if (T_PX) fprintf(stderr,"unready: new rl bol/eol = %o/%o\n", rl>>16, rl&0xFFFF);
  put16(newlink, pcbp+1);     /* update my pcb link */
  put32(waitlist, pcbp+2);    /* update my pcb wait address */
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
  if (regs.sym.pcba != crs[OWNERL])
    fatal("I'm running, but not regs.sym.pcba!");

  level = get16(pcbp+PCBLEV);
  rlp = MAKEVA(crs[OWNERH],level);
  rl = get32(rlp);
  if (T_PX) fprintf(stderr,"ready: pcbp=%o/%o\n", pcbp>>16, pcbp&0xFFFF);
  if (T_PX) fprintf(stderr,"ready: old bol/eol for level %o = %o/%o\n", level, rl>>16, rl&0xFFFF);
  pcbw = pcbp;                            /* pcb word number */
  if ((rl>>16) == 0) {                    /* bol=0: this RL level was empty */
    put32(0, pcbp+1);                     /* set link and wait SN in pcb */
    rl = (pcbw<<16) | pcbw;               /* set beg=end */
  } else if (begend) {                    /* notify to beginning */
    put32(rl & 0xFFFF0000, pcbp+1);       /* set link and wait SN in pcb */
    rl = (pcbw<<16) | rl&0xFFFF;          /* new is bol, eol is unchanged */
  } else {                                /* notify to end */
    put32(0, pcbp+1);                     /* set link and wait SN in pcb */
    xpcbp = MAKEVA(crs[OWNERH],rl&0xFFFF); /* get ptr to last pcb at this level */
    put16(pcbw,xpcbp+1);                  /* set last pcb's forward link */
    rl = (rl & 0xFFFF0000) | pcbw;        /* rl bol is unchanged, eol is new */
  }
  put32(rl, rlp);
  if (T_PX) fprintf(stderr,"ready: new bol/eol for level %o = %o/%o, pcb's link is %o\n", level, rl>>16, rl&0xFFFF, get16(pcbp+1));

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


wait() {
  ea_t ea;
  ea_t pcbp, prevpcbp;
  unsigned int utempl;
  unsigned int pcblevnext;      /* pcb level and link */
  unsigned short bol;
  unsigned short pcblev;
  unsigned short pcbnext;
  short count;

  ea = apea(NULL);
  if (T_PX) fprintf(stderr,"%o/%o: WAIT on %o/%o, pcb %o, keys=%o, modals=%o\n", RPH, RPL, ea>>16, ea&0xFFFF, crs[OWNERL], crs[KEYS], crs[MODALS]);
  utempl = get32(ea);     /* get count and BOL */
  count = utempl>>16;    /* count (signed) */
  bol = utempl & 0xFFFF;  /* beginning of wait list */
  if (T_PX) fprintf(stderr," wait list count was %d, bol was %o\n", count, bol);
  count++;
  if (count > 0) {      /* I have to wait */
#if 0
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
    if (bol != 0) {
      pcbp = MAKEVA(crs[OWNERH],bol);
      pcblevnext = get32(pcbp);
      pcblev = pcblevnext >> 16;
    }
    if (count == 1 || regs.sym.pla < pcblev) {   /* add me to the beginning */
      utempl = (count<<16) | crs[OWNERL];
      put32(utempl, ea);    /* update semaphore count/bol */
    } else {
      /* do a priority scan... */
      while (pcblev <= regs.sym.pla && bol != 0) {
	prevpcbp = pcbp;
	bol = pcblevnext & 0xFFFF;
	if (bol != 0) {
	  pcbp = MAKEVA(crs[OWNERH],bol);
	  pcblevnext = get32(pcbp);
	  pcblev = pcblevnext >> 16;
	}
      }
      put16(crs[OWNERL], prevpcbp+PCBLINK);
      put16(*(unsigned short *)&count, ea);    /* update count */
    }
    unready(ea, bol);
    crs[MODALS] |= 0100000;               /* ISG says dispatcher enables int. */
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

  resched = 0;
  begend = inst & 1;
#if 0
  if (regs.sym.pcba != crs[OWNERL]) {
    printf("NFY: regs.pcba = %o, but crs[OWNERL] = %o\n", regs.sym.pcba, crs[OWNERL]);
    fatal(NULL);
  }
#endif
  ea = apea(NULL);
  utempl = get32(ea);     /* get count and BOL */
  scount = utempl>>16;    /* count (signed) */
  bol = utempl & 0xFFFF;  /* beginning of wait list */
  if (T_PX) fprintf(stderr,"%o/%o: NFYB/E opcode %o, ea=%o/%o, count=%d, bol=%o, I am %o\n", RPH, RPL, inst, ea>>16, ea&0xFFFF, scount, bol, crs[OWNERL]);
  if (scount > 0) {
    if (bol == 0) {
      printf("NFYB: bol is zero, count is %d for semaphore at %o/%o\n", scount, ea>>16, ea&0xFFFF);
      fatal(NULL);
    }
    pcbp = MAKEVA(crs[OWNERH], bol);
    utempl = get32(pcbp+PCBWAIT);
    if (utempl != ea) {
      printf("NFYB: bol=%o, pcb waiting on %o/%o != ea %o/%o\n", utempl>>16, utempl&0xFFFF, ea>>16, ea&0xFFFF);
      fatal(NULL);
    }
    bol = get16(pcbp+PCBLINK);     /* get new beginning of wait list */
    resched = ready(pcbp, begend); /* put this pcb on the ready list */
  }
  scount = scount-1;
#if 1
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
  put32(utempl, ea);             /* update the semaphore */

  if (inst & 4) {                /* interrupt notify */
    if (inst & 2) {              /* clear active interrupt */
      intvec = -1;
      crs[MODALS] |= 0100000;    /* enable interrupts */
    }
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

  if (T_PX) printf("\n%o/%o: LPSW issued\n", RPH, RPL);
  if (T_PX) printf("LPSW: before load, RPH=%o, RPL=%o, keys=%o, modals=%o\n", RPH, RPL, crs[KEYS], crs[MODALS]);
  if (T_PX) printf("LPSW: crs=%d, ownerl[2]=%o, keys[2]=%o, modals[2]=%o, ownerl[3]=%o, keys[3]=%o, modals[3]=%o\n", crs==regs.rs16[2]? 2:3, regs.rs16[2][OWNERL], regs.rs16[2][KEYS], regs.rs16[2][MODALS], regs.rs16[3][OWNERL], regs.rs16[3][KEYS], regs.rs16[3][MODALS]);

  ea = apea(NULL);
  RPH = get16(ea);
  RPL = get16(INCVA(ea,1));
  newkeys(get16(INCVA(ea,2)));
  m = get16(INCVA(ea,3));
  if ((m & 0340) != (crs[MODALS] & 0340))
    if (T_PX) printf("LPSW: WARNING: changed current register set: current modals=%o, new modals=%o\n", crs[MODALS], m);
  crs[MODALS] = m;
  inhcount = 1;

  if (T_PX) printf("LPSW:    NEW RPH=%o, RPL=%o, keys=%o, modals=%o\n", RPH, RPL, crs[KEYS], crs[MODALS]);
  if (T_PX) printf("LPSW: crs=%d, ownerl[2]=%o, keys[2]=%o, modals[2]=%o, ownerl[3]=%o, keys[3]=%o, modals[3]=%o\n", crs==regs.rs16[2]? 2:3, regs.rs16[2][OWNERL], regs.rs16[2][KEYS], regs.rs16[2][MODALS], regs.rs16[3][OWNERL], regs.rs16[3][KEYS], regs.rs16[3][MODALS]);
  if (crs[MODALS] & 020)
    if (T_PX) fprintf(stderr,"Mapped I/O enabled\n");
  if (crs[MODALS] & 4) {
    if (T_PX) fprintf(stderr,"Segmentation enabled\n");
    if (domemdump) dumpsegs();
    //traceflags = ~TB_MAP;
  }
  if (crs[MODALS] & 010) {
    if (T_PX) fprintf(stderr,"Process exchange enabled:\n");
    if (T_PX) printf("LPSW: PLA=%o, PCBA=%o, PLB=%o, PCBB=%o\n", regs.sym.pla, regs.sym.pcba, regs.sym.plb, regs.sym.pcbb);
#if 0
    for (i=regs.sym.pla;; i += 2) {
      ea = MAKEVA(crs[OWNERH], i);
      utempa = get16(ea);
      if (T_PX) fprintf(stderr," Level %o: BOL=%o, EOL=%o\n", i, utempa, get16(ea+1));
      if (utempa == 1)
	break;
      while (utempa > 0)
	utempa = dumppcb(utempa);
    }
#endif
    //traceflags = ~TB_MAP;
    if (crs[KEYS] & 2) {
      if (T_PX) printf("LPSW: before disp, RPH=%o, RPL=%o, keys=%o, modals=%o\n", RPH, RPL, crs[KEYS], crs[MODALS]);
      dispatcher();
      if (T_PX) printf("LPSW: after disp, RPH=%o, RPL=%o, keys=%o, modals=%o\n", RPH, RPL, crs[KEYS], crs[MODALS]);
      if (T_PX) printf("LPSW: crs=%d, ownerl[2]=%o, keys[2]=%o, modals[2]=%o, ownerl[3]=%o, keys[3]=%o, modals[3]=%o\n", crs==regs.rs16[2]? 2:3, regs.rs16[2][OWNERL], regs.rs16[2][KEYS], regs.rs16[2][MODALS], regs.rs16[3][OWNERL], regs.rs16[3][KEYS], regs.rs16[3][MODALS]);
    }
  }
#if 0
  /* XXX: hack to disable serial number checking if E32I is enabled.
     Look for ERA/ANA sequence after illegal shift instruction, set
     the ANA operand to zero. */

  ea = MAKEVA(014,040747);
  put16(0,ea);
#endif
}


/* Character instructions */

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
      crsl[far] = (crsl[far]+1) & 0x6FFFFFFF;
      if (T_INST) fprintf(stderr," loaded character '%o (%c) from %o/%o left\n", crs[A], crs[A]&0x7f, ea>>16, ea&0xffff);
    } else {
      crs[A] = m >> 8;
      crsl[flr] |= 0x8000;
      if (T_INST) fprintf(stderr," loaded character '%o (%c) from %o/%o right\n", crs[A], crs[A]&0x7f, ea>>16, ea&0xffff);
    }
    utempl--;
    PUTFLR(n,utempl);
    crs[KEYS] &= ~0100;     /* reset EQ */
  } else {                  /* utempl == 0 */
    if (T_INST) fprintf(stderr," LDC limit\n");
    crs[A] = 0;
    crs[KEYS] |= 0100;      /* set EQ */
  }
}


stc(n) {
  unsigned int utempl;
  unsigned short m;
  unsigned short far, flr;

  far = FAR0;
  flr = FLR0;
  if (n) {
    far = FAR1;
    flr = FLR1;
  }

  utempl = GETFLR(n);
  if (utempl > 0) {
    m = get16(crsl[far]);
    if (crsl[flr] & 0x8000) {
      m = (m & 0xFF00) | (crs[A] & 0xFF);
      put16(m,crsl[far]);
      crsl[flr] &= 0xFFFF0FFF;
      crsl[far] = (crsl[far]+1) & 0x6FFFFFFF;
    } else {
      m = (crs[A] << 8) | (m & 0xFF);
      put16(m,crsl[far]);
      crsl[flr] |= 0x8000;
    }
    utempl--;
    PUTFLR(n,utempl);
    crs[KEYS] &= ~0100;     /* reset EQ */
  } else {                  /* utempl == 0 */
    if (T_INST) fprintf(stderr," STC limit\n");
    crs[KEYS] |= 0100;      /* set EQ */
  }
}


main (int argc, char **argv) {

  short tempa,tempa1,tempa2;
  unsigned short utempa;
  int templ,templ1,templ2;
  long long templl,templl1, templl2;
  unsigned long long utempll, utempll1, utempll2;
  unsigned int utempl,utempl1,utempl2;
  float tempf,tempf1,tempf2;
  double tempd,tempd1,tempd2;
  unsigned short tempda[4],tempda1[4];
  unsigned int ea32;                   /* full V/I mode eff address */
  ea_t ea;                             /* final MR effective address */
  ea_t earp;                           /* RP to use for eff address calcs */
  unsigned short eabit;
  unsigned short opcode;
  short i,j,x;
  unsigned short savemask;
  unsigned short class;
  int nw;
  unsigned short rvec[9];    /* SA, EA, P, A, B, X, keys, dummy, dummy */
  unsigned short inst;
  unsigned short m,m1,m2;
  unsigned short qtop,qbot,qseg,qmask,qtemp;
  ea_t qea;
  short scount;                          /* shift count */
  unsigned short trapvalue;
  ea_t trapaddr;
  unsigned short stpm[8];

  unsigned int instpermsecmask = 03777;   /* nearest mask of above */
  unsigned long long bootmsec;            /* time we booted */
  unsigned long long curmsec;             /* current time in milliseconds */

  struct timeval boot_tv;
  struct timeval tv;
  struct timezone tz;
  float mips;

  /* master clear:
     - clear all registers
     - register set is 2
     - set P to '1000
     - 16S mode, single precision
     - interrupts and machine checks inhibited
     - standard interrupt mode
     - all stlb entries are invalid
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

  verbose = 0;
  domemdump = 0;
  boot = 0;

  /* check args */

  for (i=1; i<argc; i++) {
    if (strcmp(argv[i],"--vv") == 0)
      verbose = 2;
    else if (strcmp(argv[i],"--v") == 0)
      verbose = 1;
    else if (strcmp(argv[i],"--map") == 0) {
      if (i+1 < argc && argv[i+1][0] != '-')
	readloadmap(argv[++i]);
      else
	fatal("--map option needs a filename\n");
    } else if (strcmp(argv[i],"--memdump") == 0)
      domemdump = 1;
    else if (strcmp(argv[i],"--ss") == 0) {
      if (i+1 < argc && argv[i+1][0] != '-') {
	sscanf(argv[i+1],"%o", &templ);
	sswitch = templ;
      } else
	sswitch = 0;
    } else if (strcmp(argv[i],"--cpuid") == 0) {
      if (i+1 < argc && argv[i+1][0] != '-') {
	sscanf(argv[i+1],"%d", &templ);
	cpuid = templ;
      } else
	cpuid = 0;
    } else if (strcmp(argv[i],"--trace") == 0)
      while (i+1 < argc && argv[i+1][0] != '-') {
	if (strcmp(argv[i+1],"ear") == 0)
	  traceflags |= TB_EAR;
	else if (strcmp(argv[i+1],"eav") == 0)
	  traceflags |= TB_EAV;
	else if (strcmp(argv[i+1],"eai") == 0)
	  traceflags |= TB_EAI;
	else if (strcmp(argv[i+1],"inst") == 0)
	  traceflags |= TB_INST;
	else if (strcmp(argv[i+1],"flow") == 0)
	  traceflags |= TB_FLOW;
	else if (strcmp(argv[i+1],"mode") == 0)
	  traceflags |= TB_MODE;
	else if (strcmp(argv[i+1],"eaap") == 0)
	  traceflags |= TB_EAAP;
	else if (strcmp(argv[i+1],"dio") == 0)
	  traceflags |= TB_DIO;
	else if (strcmp(argv[i+1],"map") == 0)
	  traceflags |= TB_MAP;
	else if (strcmp(argv[i+1],"pcl") == 0)
	  traceflags |= TB_PCL;
	else if (strcmp(argv[i+1],"fault") == 0)
	  traceflags |= TB_FAULT;
	else if (strcmp(argv[i+1],"px") == 0)
	  traceflags |= TB_PX;
	else if (strcmp(argv[i+1],"all") == 0)
	  traceflags = -1;
	else
	  fprintf(stderr,"Unrecognized trace flag: %s\n", argv[i+1]);
	i++;
      }
    else if (strcmp(argv[i],"--boot") == 0)
      boot = 1;
    else if (argv[i][0] == '-' && argv[i][1] == '-')
      fprintf(stderr,"Unrecognized argument: %s\n", argv[i]);
  }

  fprintf(stderr,"Sense switches set to %o\n", sswitch);

  /* initialize all devices */

  for (i=0; i<64; i++)
    if (devmap[i])
      devmap[i](-1, 0, i);

  os_init();

  if (boot) {
    rvec[0] = 0760;
    rvec[1] = 0760+1040-1;
    rvec[2] = 01000;
    rvec[3] = rvec[4] = rvec[5] = 0;
    rvec[6] = 0;
    /* setup DMA register '21 for the next boot record */
    regs.sym.regdmx[041] = 03000;

  } else {

    /* read 9-word rvec header */

    for (i=0; i<9; i++)
      rvec[i] = readshort();
    if (T_FLOW) fprintf(stderr,"SA=%o, EA=%o, P=%o, A=%o, B=%o, X=%o, K=%o\n\n", rvec[0], rvec[1],
	    rvec[2], rvec[3], rvec[4], rvec[5], rvec[6]);
    if (rvec[2] > rvec[1])
      fatal("Program start > EA: runfile is trashed");
  }

  /* read memory image from SA to EA inclusive */

  nw = rvec[1]-rvec[0]+1;
  if (fread(mem+rvec[0], sizeof(short), nw, stdin) != nw) {
    perror("Error reading memory image");
    fatal(NULL);
  }

  /* setup execution (registers, keys, address mask, etc.) from rvec */

  crs[A] = rvec[3];
  crs[B] = rvec[4];
  crs[X] = rvec[5];
  newkeys(rvec[6]);
  RPL = rvec[2];

  if (RPL == 0161000)      /* hack for *DOS64; P is off by 3?? */
    RPL = 0161003;

  if (domemdump)
    memdump(rvec[0], rvec[1]);

  /* initialize the timer stuff */

  if (gettimeofday(&boot_tv, &tz) != 0) {
    perror("gettimeofday failed");
    fatal(NULL);
  }

  /* main instruction decode loop */

  trapaddr = 0144003;
  trapvalue = -12345;
  trapaddr = 0;

  /* faults longjmp here: the top of the instruction fetch loop */

  if (setjmp(jmpbuf))
    ;

  while (1) {

    if (0 && instcount > 46800000)   /* rev 20 crash after STLB installed */
      traceflags = -1;
    if (0 && instcount > 18255000)   /* this is rev 20 mem test requiring STLB */
      traceflags = -1;
#if 0
    if (instcount > 20445443)
      exit(1);
#endif

#if 0
    if (trapaddr != 0 && mem[trapaddr] != trapvalue) {
      printf("TRAP: at #%d, old value of '%o was %o; new value is %o\n", instcount, trapaddr, trapvalue, mem[trapaddr]);
      trapvalue = mem[trapaddr];
    }
    if (*(int *)(crs+XB) != 0) {
      fprintf(stderr, "TRAP: at #%d, XB% changed to %o/%o\n", instcount, crs[XBH], crs[XBL]);
    }
#endif

    /* poll any devices that requested a poll */

    for (i=0; i<64; i++)
      if (devpoll[i] && (--devpoll[i] == 0)) {
	if (!devmap[i])
	  fatal("devpoll set but devmap is null");
	if (RPL == 070511 && savetraceflags != 0)
	  traceflags = savetraceflags;
	devmap[i](4, 0, i);
      }

    /* is an interrupt pending, with interrupts enabled? */

    if (intvec >= 0 && (crs[MODALS] & 0100000) && inhcount == 0) {
      //printf("fetch: taking interrupt vector '%o, modals='%o\n", intvec, crs[MODALS]);
      if (RPL == 070511 && savetraceflags != 0)
	traceflags = savetraceflags;
      if (T_INST) fprintf(stderr, "\nfetch: taking interrupt vector '%o, modals='%o\n", intvec, crs[MODALS]);
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

      } else {
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
    inst = get16(ea);
    RPL++;
    instcount++;

    /* while a process is running, RP is the real program counter, PBH
       is the active procedure segment, and PBL is zero.  When a
       process stops running, RP is copied to PB.  When a process
       starts running again, PB is copied to RP.  See seg14.pma,
       WRMSAV. */

    crs[PBH] = RPH;
    crs[PBL] = 0;
    earp = RP;

    if (crs[MODALS] & 010) {     /* px enabled, bump 1ms process timer */
      if ((instcount & instpermsecmask) == 0) {

#if 0
	/* bump all timers, applying corrections based on actual time
	   if necessary */

	if (!gettimeofday(&tv, &tz))
	  fatal("em: gettimeofday failed");
#endif

	/* if 1ms resolution process timer overflows, set pcb abort flag */

	if (++crs[TIMER] == 0) {
	  if (T_PX) printf("pcb %o timer overflow\n", crs[OWNERL]);
	  ea = *(ea_t *)(crs+OWNER);
	  m = get16(ea+4) | 1;       /* set process abort flag */
	  put16(m, ea+4);
#if 0
	  fault(PROCESSFAULT, utempa, 0);
	  fatal("fault returned after process fault");    
#endif
	}
	//printf("incremented timer to %d\n", *(short *)(crs+TIMER));
      }
    }

#if 0
    /* NOTE: this is to debug generic instruction 3's in Primos boot */

    if (instcount > 75376100)
      //traceflags = -1;
      traceflags = TB_DIO;

#endif

xec:
    if (T_FLOW) fprintf(stderr,"\n%o/%o: %o		A='%o/%:0d B='%o/%d X=%o/%d Y=%o/%d C=%d L=%d LT=%d EQ=%d K=%o M=%o	#%d [%s %o]\n", RPH, RPL-1, inst, crs[A], *(short *)(crs+A), crs[B], *(short *)(crs+B), crs[X], *(short *)(crs+X), crs[Y], *(short *)(crs+Y), (crs[KEYS]&0100000) != 0, (crs[KEYS]&020000) != 0, (crs[KEYS]&0200) != 0, (crs[KEYS]&0100) != 0, crs[KEYS], crs[MODALS], instcount, searchloadmap(*(unsigned int *)(crs+OWNER)), crs[OWNERL]);

    /* begin instruction decode: generic? */

    if ((inst & 036000) == 0) {
      class = inst>>14;
      if (class == 0) {
	if (T_INST) fprintf(stderr," generic class 0\n");

	/* V-mode/frequent instructions */

	if (inst == 000201) {
	  if (T_FLOW) fprintf(stderr," IAB\n");
	  tempa = crs[B];
	  crs[B] = crs[A];
	  crs[A] = tempa;
	  continue;
	}

	if (inst == 001314) {
	  if (T_FLOW) fprintf(stderr," CGT\n");
	  tempa = get16(RP);              /* get number of words */
	  if (1 <= crs[A] && crs[A] < tempa)
	    RPL = get16(INCVA(RP,crs[A]));
	  else
	    RPL += tempa;
	  continue;
	}

	if (inst == 000115) {
	  if (T_FLOW) fprintf(stderr," PIDA\n");
	  *(int *)(crs+L) = *(short *)(crs+A);
	  continue;
	}

	if (inst == 000305) {
	  if (T_FLOW) fprintf(stderr," PIDL\n");
	  *(long long *)(crs+L) = *(int *)(crs+L);
	  continue;
	}

	/* XXX: how does PIMA affect registers when overflow occurs?
	   NOTE: PMA manual says copy B reg to A reg, but DIAG seems
	   to indicate a swap */

	if (inst == 000015) {
	  if (T_FLOW) fprintf(stderr," PIMA\n");
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
	}

	if (inst == 000301) {
	  if (T_FLOW) fprintf(stderr," PIML\n");
	  templ = *(int *)(crs+L);
	  *(int *)(crs+L) = *(int *)(crs+E);
	  SETL(0);
	  SETCC_L;
	  if (((templ ^ crs[E]) & 0x8000) || (templ != 0 && templ != -1))
	    mathexception('i', FC_INT_OFLOW, 0);
	  else
	    CLEARC;
	  continue;
	}

	/* character/field instructions */

	if (inst == 001302) {
	  if (T_FLOW) fprintf(stderr," LDC 0\n");
	  ldc(0);
	  continue;
	}

	if (inst == 001312) {
	  if (T_FLOW) fprintf(stderr," LDC 1\n");
	  ldc(1);
	  continue;
	}

	if (inst == 001322) {
	  if (T_FLOW) fprintf(stderr," STC 0\n");
	  stc(0);
	  continue;
	}
	    
	if (inst == 001332) {
	  if (T_FLOW) fprintf(stderr," STC 1\n");
	  stc(1);
	  continue;
	}

	if (inst == 001300) {
	  if (T_FLOW) fprintf(stderr," EAFA 0\n");
	  ea = apea(&eabit);
	  crsl[FAR0] = ea;
	  crsl[FLR0] = (crsl[FLR0] & 0xFFFF0FFF) | (eabit << 12);
	  if (T_INST) fprintf(stderr," FAR0=%o, eabit=%d, FLR=%x\n", crsl[FAR0], eabit, crsl[FLR0]);
	  continue;
	}

	if (inst == 001310) {
	  if (T_FLOW) fprintf(stderr," EAFA 1\n");
	  ea = apea(&eabit);
	  crsl[FAR1] = ea;
	  crsl[FLR1] = (crsl[FLR1] & 0xFFFF0FFF) | (eabit << 12);
	  if (T_INST) fprintf(stderr," FAR1=%o, eabit=%d, FLR=%x\n", crsl[FAR1], eabit, crsl[FLR1]);
	  continue;
	}

	if (inst == 001301) {
	  if (T_FLOW) fprintf(stderr," ALFA 0\n");
	  if (T_INST) fprintf(stderr," before add, FAR0=%o, FLR=%o\n", crsl[FAR0], crsl[FLR0]);
	  utempl = ((crsl[FAR0] & 0xFFFF) << 4) | ((crsl[FLR0] >> 12) & 0xF);
	  utempl += *(int *)(crs+L);
	  crsl[FAR0] = (crsl[FAR0] & 0xFFFF0000) | ((utempl >> 4) & 0xFFFF);
	  crsl[FLR0] = (crsl[FLR0] & 0xFFFF0FFF) | ((utempl & 0xF) << 12);
	  if (T_INST) fprintf(stderr," after add, FAR0=%o, FLR=%o\n", crsl[FAR0], crsl[FLR0]);
	  continue;
	}

	if (inst == 001311) {
	  if (T_FLOW) fprintf(stderr," ALFA 1\n");
	  utempl = ((crsl[FAR1] & 0xFFFF) << 4) | ((crsl[FLR1] >> 12) & 0xF);
	  utempl += *(int *)(crs+L);
	  crsl[FAR1] = (crsl[FAR1] & 0xFFFF0000) | ((utempl >> 4) & 0xFFFF);
	  crsl[FLR1] = (crsl[FLR1] & 0xFFFF0FFF) | ((utempl & 0xF) << 12);
	  continue;
	}

	if (inst == 001303) {
	  if (T_FLOW) fprintf(stderr," LFLI 0\n");
#if 0
	  for (utempa=0; utempa<256; utempa++) {
	    PUTFLR(0,utempa);
	    crsl[FLR0] |= 0x4000;
	    utempl = GETFLR(0);
	    if (utempa != utempl) {
	      fprintf(stderr," loaded %d, fetched %d\n", utempa, utempl);
	      exit(1);
	    }
	  }
#endif
	  utempa = get16(RP);
	  RPL++;
	  PUTFLR(0,utempa);
	  utempl = GETFLR(0);
	  if (T_INST) fprintf(stderr," Load Field length with %d, FLR=%x, actual = %d\n", utempa, crsl[FLR0], utempl);
	  if (utempa != utempl)
	    fatal("LFLI 0 error");
	  continue;
	}

	if (inst == 001313) {
	  if (T_FLOW) fprintf(stderr," LFLI 1\n");
#if 0
	  for (utempa=0; utempa<256; utempa++) {
	    PUTFLR(1,utempa);
	    utempl = GETFLR(1);
	    if (utempa != utempl) {
	      fprintf(stderr," loaded %d, fetched %d\n", utempa, utempl);
	      exit(1);
	    }
	  }
#endif
	  utempa = get16(RP);
	  RPL++;
	  PUTFLR(1,utempa);
	  utempl = GETFLR(1);
	  if (T_INST) fprintf(stderr," Load Field length with %d, FLR=%x, actual = %d\n", utempa, crsl[FLR1], utempl);
	  if (utempa != utempl)
	    fatal("LFLI 1 error");
	  continue;
	}

	if (inst == 001320) {
	  if (T_FLOW) fprintf(stderr," STFA 0\n");
	  ea = apea(NULL);
	  utempl = crsl[FAR0] & 0x6FFFFFFF;
	  utempa = crsl[FLR0] & 0xF000;
stfa:
	  if (utempa != 0) {
	    utempl = utempl | EXTMASK32;
	    put16(utempa,INCVA(ea,2));
	  }
	  put32(utempl,ea);
	  continue;
	}

	if (inst == 001330) {
	  if (T_FLOW) fprintf(stderr," STFA 1\n");
	  ea = apea(NULL);
	  utempl = crsl[FAR1] & 0x6FFFFFFF;
	  utempa = crsl[FLR1] & 0xF000;
	  goto stfa;
	}

	if (inst == 001321) {
	  if (T_FLOW) fprintf(stderr," TLFL 0\n");
	  PUTFLR(0,*(unsigned int *)(crs+L));
	  continue;
	}

	if (inst == 001331) {
	  if (T_FLOW) fprintf(stderr," TLFL 1\n");
	  PUTFLR(1,*(unsigned int *)(crs+L));
	  utempl = GETFLR(1);
	  if (T_INST) fprintf(stderr," Transfer %d to FLR1, FLR=%x, actual = %d\n", *(unsigned int *)(crs+L), crsl[FLR1], utempl);
	  continue;
	}


	if (inst == 001323) {
	  if (T_FLOW) fprintf(stderr," TFLL 0\n");
	  *(unsigned int *)(crs+L) = GETFLR(0);
	  continue;
	}

	if (inst == 001333) {
	  if (T_FLOW) fprintf(stderr," TFLL 1\n");
	  *(unsigned int *)(crs+L) = GETFLR(1);
	  continue;
	}
	
	if (inst == 000611) {
	  if (T_FLOW) fprintf(stderr," PRTN\n");
	  prtn();
	  continue;
	}

	if (inst == 001005) {
	  if (T_FLOW) fprintf(stderr," TKA\n");
	  crs[A] = crs[KEYS];
	  continue;
	}

	if (inst == 001015) {
	  if (T_FLOW) fprintf(stderr," TAK\n");
	  newkeys(crs[A] & 0177760);
	  continue;
	}

	if (inst == 000001) {
	  if (T_FLOW) fprintf(stderr," NOP\n");
	  continue;
	}

	if (inst == 000715) {
	  if (T_FLOW) fprintf(stderr," RSAV\n");
	  ea = apea(NULL);
	  j = 1;
	  savemask = 0;
	  for (i = 11; i >= 0; i--) {
	    if (crsl[i] != 0) {
	      if (T_INST) fprintf(stderr," crsl[%d] saved, value=%o\n", i, crsl[i]);
	      put32(crsl[i], INCVA(ea,j));
	      savemask |= bitmask16[16-i];
	    }
	    j += 2;
	  }
	  put32(*(int *)(crs+XB), INCVA(ea,25));
	  put16(savemask, ea);
	  if (T_INST) fprintf(stderr," Saved, mask=%o\n", savemask);
	  continue;
	}

	if (inst == 000717) {
	  if (T_FLOW) fprintf(stderr," RRST\n");
	  ea = apea(NULL);
	  savemask = get16(ea);
	  //if (T_INST) fprintf(stderr," %o/%o: RRST %o/%o, mask=%o, modals=%o\n", RPH,RPL,ea>>16,ea&0xffff,savemask,crs[MODALS]);
	  if (T_INST) fprintf(stderr," Save mask=%o\n", savemask);
	  j = 1;
	  for (i = 11; i >= 0; i--) {
	    if (savemask & bitmask16[16-i]) {
	      crsl[i] = get32(INCVA(ea,j));
	      if (T_INST) fprintf(stderr," crsl[%d] restored, value=%o\n", i, crsl[i]);
	    } else {
	      crsl[i] = 0;
	    }
	    j += 2;
	  }
	  *(unsigned int *)(crs+XB) = get32(INCVA(ea,25));
	  if (T_INST) fprintf(stderr," XB restored, value=%o/%o\n", crs[XBH], crs[XBL]);
	  continue;
	}

	if (000400 <= inst && inst <= 000402) {
	  if (T_FLOW) fprintf(stderr," ENB\n");
	  RESTRICT();
	  crs[MODALS] |= 0100000;
	  inhcount = 1;
	  continue;
	}

	if (001000 <= inst && inst <= 001002) {
	  if (T_FLOW) fprintf(stderr," INH\n");
	  RESTRICT();
	  crs[MODALS] &= ~0100000;
	  continue;
	}

	if (inst == 01200) {
	  if (T_FLOW) fprintf(stderr," STAC\n");
	  ea = apea(NULL);
	  if (get16(ea) == crs[B]) {
	    put16(crs[A], ea);
	    crs[KEYS] |= 0100;       /* set EQ */
	  } else 
	    crs[KEYS] &= ~0100;      /* reset EQ */
	  continue;
	}

	if (inst == 01204) {
	  if (T_FLOW) fprintf(stderr," STLC\n");
	  ea = apea(NULL);
	  if (get32(ea) == *(unsigned int *)(crs+E)){
	    put32(*(unsigned int *)(crs+L), ea);
	    crs[KEYS] |= 0100;       /* set EQ */
	  } else 
	    crs[KEYS] &= ~0100;      /* reset EQ */
	  continue;
	}

	if (inst == 000605) {
	  if (T_FLOW || T_PCL) fprintf(stderr," ARGT\n");
	  argt();
	  continue;
	}

	if (inst == 000705) {
	  if (T_FLOW || T_PCL) fprintf(stderr," CALF\n");
	  ea = apea(NULL);
	  calf(ea);
	  continue;
	}

	/* Decimal and character instructions */
	/* NOTE: ZFIL is used early after PX enabled, and can be used to cause
	   a UII fault to debug CALF etc. */

#if 1
	if (inst == 001114) {
	  if (T_FLOW) fprintf(stderr," ZMV\n", inst);
	  utempa = crs[A];
	  do {
	    ldc(0);
	    if (crs[KEYS] & 0100)
	      crs[A] = 0240;
	    stc(1);
	  } while (!(crs[KEYS] & 0100));
	  crs[A] = utempa;
	  continue;
	}

	if (inst == 001115) {
	  if (T_FLOW) fprintf(stderr," ZMVD\n", inst);
	  utempa = crs[A];
	  utempl = GETFLR(0);
	  PUTFLR(0, 65535);
	  if (T_INST) fprintf(stderr," source=%o/%o, len=%d, dest=%o/%o, len=%d\n", crsl[FAR0]>>16, crsl[FAR0]&0xffff, GETFLR(0), crsl[FAR1]>>16, crsl[FAR0]&0xffff, GETFLR(1));
	  do {
	    ldc(0);
	    stc(1);
	  } while (!(crs[KEYS] & 0100));
	  crs[A] = utempa;
	  PUTFLR(0, utempl);
	  continue;
	}

	if (inst == 001116) {
	  if (T_FLOW) fprintf(stderr," ZFIL\n", inst);
	  do {
	    stc(1);
	  } while (!(crs[KEYS] & 0100));
	  continue;
	}
#endif

	/* 001100 = XAD
	   001101 = XMV
	   001102 = XCM
	   001104 = XMP
	   001107 = XDV
	   001110 = ZTRN
	   001111 = ZED
	   001116 = ZFIL
	   001117 = ZCM
	   001112 = XED
	   001145 = XBTD
	   001146 = XDTB
	*/

	if (001100 <= inst && inst <= 001146) {
	  //traceflags = -1;
	  if (T_FLOW) fprintf(stderr," X/Z UII %o\n", inst);
	  fault(UIIFAULT, RPL, RP);
	  continue;
	}

	/* OS/restricted instructions */

	if (inst == 000511) {
	  if (T_FLOW) fprintf(stderr," RTS\n", inst);
	  RESTRICT();
	  //traceflags = ~TB_MAP;
	  fault(UIIFAULT, RPL, RP);
	  continue;
	}

	if (inst == 000315) {
	  if (T_FLOW) fprintf(stderr," WAIT\n", inst);
	  RESTRICT();
	  wait();
	  continue;
	}

	if (001210 <= inst && inst <= 001217) {
	  if (001212 <= inst && inst <= 001213)
	    fatal("Unrecognized NFY instruction");
	  if (T_FLOW) fprintf(stderr," NFY(%o)\n", inst);
	  RESTRICT();
	  nfy(inst);
	  continue;
	}
	    
	if (inst == 001315) {
	  if (T_FLOW) fprintf(stderr," STEX\n");
	  *(ea_t *)(crs+L) = stex(*(unsigned int *)(crs+L));
	  continue;
	}

	if (inst == 000711) {
	  if (T_FLOW) fprintf(stderr," LPSW\n");
	  RESTRICT();
	  lpsw();
	  continue;
	}

	if (inst == 000615) {
	  if (T_FLOW) fprintf(stderr," ITLB\n");
	  RESTRICT();
	  utempl = *(int *)(crs+L);
	  utempa = STLBIX(utempl);
	  stlb[utempa].valid = 0;
	  if (T_INST) fprintf(stderr," invalidated STLB index %d\n", utempa);
	  /* HACK for DIAG to suppress ITLB loop in trace */
	  if (RP == 0106070)
	    if (*(int *)(crs+L) == 0) {
	      fprintf(stderr," Suppressing DIAG trace\n");
	      savetraceflags = traceflags;
	      traceflags = 0;
	    } else if (crs[A] == 07777 && crs[B] == 0176000) {
	      fprintf(stderr," Restoring DIAG trace\n");
	      traceflags = savetraceflags;
	    }
	  continue;
	}

	if (inst == 000024) {
	  if (T_FLOW) fprintf(stderr," STPM\n", inst);
	  RESTRICT();
	  for (i=0; i<8; i++)
	    stpm[i] = 0;
	  stpm[1] = cpuid;
	  ea = *(unsigned int *)(crs+XB);
	  put64(*(double *)(stpm+0), ea);
	  put64(*(double *)(stpm+4), INCVA(ea,4));
	  continue;
	}

	if (inst == 040310) {
	  if (T_FLOW) fprintf(stderr," SSSN\n", inst);
	  fault(UIIFAULT, RPL, RP);
	  continue;
	}

	if (inst == 001702) {
	  if (T_FLOW) fprintf(stderr," IDLE?\n", inst);
	  RESTRICT();
	  //traceflags = ~TB_MAP;
	  dispatcher();
	  continue;
	}

	if (inst == 000601) {
	  if (T_FLOW) fprintf(stderr," IRTN\n", inst);
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
	}

	if (inst == 000603) {
	  if (T_FLOW) fprintf(stderr," IRTC\n", inst);
	  RESTRICT();
	  intvec = -1;
	  goto irtn;
	}

	/* R-mode/infrequent gen 0 instructions */

	if (inst == 000005) {                 /* SGL */
	  if (T_FLOW) fprintf(stderr," SGL\n");
	  crs[KEYS] &= ~040000;
	  continue;
	}

	if (inst == 000011) {                 /* E16S */
	  if (T_FLOW) fprintf(stderr," E16S\n");
	  newkeys(crs[KEYS] & 0161777);
	  continue;
	}

	if (inst == 000013) {                 /* E32S */
	  if (T_FLOW) fprintf(stderr," E32S\n");
	  newkeys((crs[KEYS] & 0161777) | 1<<10);
	  continue;
	}

	if (inst == 001013) {                 /* E32R */
	  if (T_FLOW) fprintf(stderr," E32R\n");
	  newkeys((crs[KEYS] & 0161777) | 3<<10);
	  continue;
	}

	if (inst == 001011) {                 /* E64R */
	  if (T_FLOW) fprintf(stderr," E64R\n");
	  newkeys((crs[KEYS] & 0161777) | 2<<10);
	  continue;
	}

	if (inst == 000010) {                 /* E64V */
	  if (T_FLOW) fprintf(stderr," E64V\n");
	  newkeys((crs[KEYS] & 0161777) | 6<<10);
	  continue;
	}

	if (inst == 001010) {                 /* E32I */
	  if (T_FLOW) fprintf(stderr," E32I\n");
	  fault(RESTRICTFAULT, 0, 0);
	  newkeys((crs[KEYS] & 0161777) | 4<<10);
	  continue;
	}

	if (inst == 000505) {                 /* SVC */
	  if (T_FLOW) fprintf(stderr," SVC\n");
	  svc();
	  continue;
	}

	if (inst == 000111) {                  /* CEA */
	  if (T_FLOW) fprintf(stderr," CEA\n");
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
		m = get16(ea);
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
		crs[A] = get16(ea);
	    }
	  }
	  continue;
	}

	if (inst == 000000) {
	  if (T_FLOW) fprintf(stderr," HLT\n");
	  RESTRICT();
	  memdump(0,0xFFFF);
	  fatal("CPU halt");
	}

	if (inst == 000205) {                /* PIM (R-mode) */
	  if (T_FLOW) fprintf(stderr," PIM\n");
#if 0
	  /* NOTE: this fits the description in the Rev 21 ISG, but fails
	     DIAG test CPU.INTEGER, Case 12 */

	  crs[A] = (crs[A] & 0x8000) | (crs[B] & 0x7FFF);
#else
	  crs[A] = (crs[A] & 0x8000) | crs[B];
#endif
	  continue;
	}

	if (inst == 000211) {                /* PID (R-mode) */
	  if (T_FLOW) fprintf(stderr," PID\n");
	  *(int *)(crs+L) = *(short *)(crs+A);
	  crs[B] &= 0x7fff;
	  continue;
	}

	/* DBL activates 31-bit mode (R-mode only):

	   LDA -> DLD (double load)
	   STA -> DST (double store)
	   ADD -> DAD (double add)
	   SUB -> DSB (double subtract)

	   Other R-mode, 31-bit instructions include:
	   
	   PID, DIV, MPY, PIM, INT, FLOT
	*/

	if (inst == 000007) {                 /* DBL */
	  if (T_FLOW) fprintf(stderr," DBL\n");
	  crs[KEYS] |= 040000;
	  continue;
	}

	if (inst == 000041) {
	  if (T_FLOW) fprintf(stderr," SCA\n");
	  crs[A] = crs[VSC] & 0xFF;
	  continue;
	}

	if (inst == 000043) {
	  if (T_FLOW) fprintf(stderr," INK\n");
	  crs[A] = (crs[KEYS] & 0xFF00) | (crs[VSC] & 0xFF);
	  continue;
	}

	if (inst == 000405) {
	  if (T_FLOW) fprintf(stderr," OTK\n");
	  newkeys((crs[A] & 0xFF00) | (crs[KEYS] & 0xFF));
	  crs[VSC] = (crs[VSC] & 0xFF00) | (crs[A] & 0xFF);
	  if ((RP & RINGMASK32) == 0)
	    inhcount = 1;
	  continue;
	}

	if (inst == 000415) {
	  if (T_FLOW) fprintf(stderr," ESIM\n");
	  RESTRICT();
	  crs[MODALS] &= ~040000;
	  continue;
	}

	if (inst == 000417) {
	  if (T_FLOW) fprintf(stderr," EVIM\n");
	  RESTRICT();
	  crs[MODALS] |= 040000;
	  continue;
	}

	if (inst == 000101) {
	  if (T_FLOW) fprintf(stderr," NRM\n");
	  crs[VSC] = 0;
	  if (crs[A] == 0 && crs[B] == 0)
	    continue;
	  while (!((crs[A] ^ (crs[A] << 1)) & 0x8000)) {
	    if (T_INST) fprintf(stderr, " step %d: crs[A]=%o, crs[B]=%o\n", crs[VSC], crs[A], crs[B]);
	    crs[B] = crs[B] << 1;
	    crs[A] = (crs[A] & 0x8000) | ((crs[A] << 1) & 0x7FFE) | (crs[B] >> 15);
	    crs[VSC]++;
	  }
	  crs[B] &= 0x7FFF;
	  if (T_INST) fprintf(stderr, " finished with %d shifts: crs[A]=%o, crs[B]=%o\n", crs[VSC], crs[A], crs[B]);
	  continue;
	}

	if (inst == 000105) {
	  if (T_FLOW) fprintf(stderr," RTN\n");
	  m = get16(crs[S]+1);
	  if (m == 0)
	    fatal("RTN stack underflow");
	  crs[S] = get16(crs[S]);
	  continue;
	}

	/* unusual instructions */

	if (inst == 3) {
	  if (T_FLOW) fprintf(stderr," gen 3?\n");
	  //printf("#%d: %o/%o: Generic instruction 3?\n", instcount, RPH, RPL);
	  continue;
	}

	for (i=0; i<GEN0TABSIZE; i++) {
	  if (inst == gen0tab[i]) {
	    if (T_FLOW) fprintf(stderr," %s\n", gen0nam[i]);
	    break;
	  }
	}
	if (i < GEN0TABSIZE)
	  continue;

	if (T_INST) fprintf(stderr," unrecognized generic class 0 instruction!\n");
	printf("#%d: %o/%o: Unrecognized generic class 0 instruction '%o!\n", instcount, RPH, RPL, inst);
	fault(UIIFAULT, RPL, 0);
	fatal(NULL);
      }

      if (class == 3) {
	if (T_INST) fprintf(stderr," generic class 3\n");

	if (inst == 0141604) {
	  if (T_FLOW) fprintf(stderr," BCLT\n");
bclt:
	  if (crs[KEYS] & 0200)
	    RPL = get16(RP);
	  else
	    RPL++;
	  continue;
	}

	if (inst == 0141600) {
	  if (T_FLOW) fprintf(stderr," BCLE\n");
bcle:
	  if (crs[KEYS] & 0300)
	    RPL = get16(RP);
	  else
	    RPL++;
	  continue;
	}

	if (inst == 0141602) {
	  if (T_FLOW) fprintf(stderr," BCEQ\n");
bceq:
	  if (crs[KEYS] & 0100)
	    RPL = get16(RP);
	  else
	    RPL++;
	  continue;
	}

	if (inst == 0141603) {
	  if (T_FLOW) fprintf(stderr," BCNE\n");
bcne:
	  if (!(crs[KEYS] & 0100))
	    RPL = get16(RP);
	  else
	    RPL++;
	  continue;
	}

	if (inst == 0141605) {
	  if (T_FLOW) fprintf(stderr," BCGE\n");
bcge:
	  if (!(crs[KEYS] & 0200))
	    RPL = get16(RP);
	  else
	    RPL++;
	  continue;
	}

	if (inst == 0141601) {
	  if (T_FLOW) fprintf(stderr," BCGT\n");
bcgt:
	  if (!(crs[KEYS] & 0300))
	    RPL = get16(RP);
	  else
	    RPL++;
	  continue;
	}

	if (inst == 0141705) {
	  if (T_FLOW) fprintf(stderr," BCR\n");
	  if (!(crs[KEYS] & 0100000))
	    RPL = get16(RP);
	  else
	    RPL++;
	  continue;
	}

	if (inst == 0141704) {
	  if (T_FLOW) fprintf(stderr," BCS\n");
	  if (crs[KEYS] & 0100000)
	    RPL = get16(RP);
	  else
	    RPL++;
	  continue;
	}

	if (inst == 0141707) {
	  if (T_FLOW) fprintf(stderr," BLR\n");
blr:
	  if (!(crs[KEYS] & 020000))
	    RPL = get16(RP);
	  else
	    RPL++;
	  continue;
	}

	if (inst == 0141706) {
	  if (T_FLOW) fprintf(stderr," BLS\n");
bls:
	  if (crs[KEYS] & 020000)
	    RPL = get16(RP);
	  else
	    RPL++;
	  continue;
	}

	if (inst == 0140614) {
	  if (T_FLOW) fprintf(stderr," BLT\n");
	  SETCC_A;
	  goto bclt;
	}

	if (inst == 0140610) {
	  if (T_FLOW) fprintf(stderr," BLE\n");
	  SETCC_A;
	  goto bcle;
	}

	if (inst == 0140612) {
	  if (T_FLOW) fprintf(stderr," BEQ\n");
	  SETCC_A;
	  goto bceq;
	}

	if (inst == 0140613) {
	  if (T_FLOW) fprintf(stderr," BNE\n");
	  SETCC_A;
	  goto bcne;
	}

	if (inst == 0140615) {
	  if (T_FLOW) fprintf(stderr," BGE\n");
	  SETCC_A;
	  goto bcge;
	}

	if (inst == 0140611) {
	  if (T_FLOW) fprintf(stderr," BGT\n");
	  SETCC_A;
	  goto bcgt;
	  continue;
	}

	if (inst == 0140700) {
	  if (T_FLOW) fprintf(stderr," BLLE\n");
	  SETCC_L;
	  goto bcle;
	}

	if (inst == 0140702) {
	  if (T_FLOW) fprintf(stderr," BLEQ\n");
	  SETCC_L;
	  goto bceq;
	}

	if (inst == 0140703) {
	  if (T_FLOW) fprintf(stderr," BLNE\n");
	  SETCC_L;
	  goto bcne;
	}

	if (inst == 0140701) {
	  if (T_FLOW) fprintf(stderr," BLGT\n");
	  SETCC_L;
	  goto bcgt;
	}

	if (inst == 0141614) {
	  if (T_FLOW) fprintf(stderr," BFLT\n");
	  SETCC_F;
	  goto bclt;
	}

	if (inst == 0141610) {
	  if (T_FLOW) fprintf(stderr," BFLE\n");
	  SETCC_F;
	  goto bcle;
	}

	if (inst == 0141612) {
	  if (T_FLOW) fprintf(stderr," BFEQ\n");
	  SETCC_F;
	  goto bceq;
	}

	if (inst == 0141613) {
	  if (T_FLOW) fprintf(stderr," BFNE\n");
	  SETCC_F;
	  goto bcne;
	}

	if (inst == 0141615) {
	  if (T_FLOW) fprintf(stderr," BFGE\n");
	  SETCC_F;
	  goto bcge;
	}

	if (inst == 0141611) {
	  if (T_FLOW) fprintf(stderr," BFGT\n");
	  SETCC_F;
	  goto bcgt;
	}

	if (inst == 0141334) {
	  if (T_FLOW) fprintf(stderr," BIX\n");
	  crs[X]++;
bidx:
	  if (crs[X] != 0)
	    RPL = get16(RP);
	  else
	    RPL++;
	  continue;
	}

	if (inst == 0141324) {
	  if (T_FLOW) fprintf(stderr," BIY\n");
	  crs[Y]++;
bidy:
	  if (crs[Y] != 0)
	    RPL = get16(RP);
	  else
	    RPL++;
	  continue;
	}

	if (inst == 0140724) {
	  if (T_FLOW) fprintf(stderr," BDY\n");
	  crs[Y]--;
	  goto bidy;
	}

	if (inst == 0140734) {
	  if (T_FLOW) fprintf(stderr," BDX\n");
	  crs[X]--;
#if 1
	  if (RPL == 070512 && traceflags != 0) {   /* backstop loop */
	    fprintf(stderr," Suppressing backstop trace\n");
	    savetraceflags = traceflags;
	    traceflags = 0;
	  } else if (RPL == 070512 && savetraceflags != 0 && crs[X] == 0) {
	    fprintf(stderr," Restoring backstop trace\n");
	    traceflags = savetraceflags;
	  }
#endif
	  goto bidx;
	}

	if (inst == 0141206) {
	  if (T_FLOW) fprintf(stderr," A1A\n");
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
	}

	if (inst == 0140304) {
	  if (T_FLOW) fprintf(stderr," A2A\n");
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
	}

	if (inst == 0141216) {
	  if (T_FLOW) fprintf(stderr," ACA\n");
	  if (crs[KEYS] & 0100000)
	    goto a1a;
	  crs[KEYS] &= ~0120300;                 /* clear C, L, LT, EQ */
	  if (crs[A] == 0)                       /* set EQ? */
	    crs[KEYS] |= 0100; 
	  else if (*(short *)(crs+A) < 0)        /* set LT? */
	    crs[KEYS] |= 0200;
	  continue;
	}

	if (inst == 0140110) {
	  if (T_FLOW) fprintf(stderr," S1A\n");
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
	}

	if (inst == 0140310) {
	  if (T_FLOW) fprintf(stderr," S2A\n");
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
	}

	if (inst == 0141050) {
	  if (T_FLOW) fprintf(stderr," CAL\n");
	  crs[A] &= 0xFF;
	  continue;
	}

	if (inst == 0141044) {
	  if (T_FLOW) fprintf(stderr," CAR\n");
	  crs[A] &= 0xFF00;
	  continue;
	}

	if (inst == 0140040) {
	  if (T_FLOW) fprintf(stderr," CRA\n");
	  crs[A] = 0;
	  continue;
	}

	/* On the P300, the B register is the low-order word of the
	   DP floating pt fraction, so CRB was used to convert SPFP
	   numbers to DPFP.  On the P400 and up, the B register and
	   DPFP accumulator do not overlap.  For compatibility, there
	   are 3 related instructions:

	   '14 clears B and the low-order DPFP register
	   '15 clears only B
	   '16 clears only the low-order DPFP register
	*/

	if (inst == 0140014) {
	  if (T_FLOW) fprintf(stderr," P300CRB\n");
	  crs[B] = 0;
	  crs[FLTD] = 0;
	  continue;
	}

	if (inst == 0140015) {
	  if (T_FLOW) fprintf(stderr," CRB\n");
	  crs[B] = 0;
	  continue;
	}

	if (inst == 0140016) {
	  if (T_FLOW) fprintf(stderr," FDBL\n");
	  crs[FLTD] = 0;
	  continue;
	}

	if (inst == 0140010) {
	  if (T_FLOW) fprintf(stderr," CRL\n");
	  *(int *)(crs+L) = 0;
	  continue;
	}

	/* XXX: this should set the L bit like subtract */

	if (inst == 0140214) {
	  if (T_FLOW) fprintf(stderr," CAZ\n");
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
	}

	/* NOTE: using "if (crs[X]++ == 0)" doesn't work because of
	   unsigned short type promotion! */

	if (inst == 0140114) {
	  if (T_FLOW) fprintf(stderr," IRX\n");
	  crs[X]++;
	  if (crs[X] == 0)
	    RPL++;
	  continue;
	}

	if (inst == 0140210) {
	  if (T_FLOW) fprintf(stderr," DRX\n");
	  crs[X]--;
	  if (crs[X] == 0)
	    RPL++;
	  continue;
	}

	if (inst == 0141240) {
	  if (T_FLOW) fprintf(stderr," ICR\n");
	  crs[A] = crs[A] << 8;
	  continue;
	}

	if (inst == 0141140) {
	  if (T_FLOW) fprintf(stderr," ICL\n");
	  crs[A] = crs[A] >> 8;
	  continue;
	}

	if (inst == 0141340) {
	  if (T_FLOW) fprintf(stderr," ICA\n");
	  crs[A] = (crs[A] >> 8) | (crs[A] << 8);
	  continue;
	}

	/* NOTE: Rev 21 Inst. Guide says CC are indeterminate, other
	   references say they are set */

	if (inst == 0140417) {
	  if (T_FLOW) fprintf(stderr," LT\n");
	  crs[A] = 1;
	  continue;
	}

	if (inst == 0140416) {
	  if (T_FLOW) fprintf(stderr," LF\n");
	  crs[A] = 0;
	  continue;
	}

	if (inst == 0140314) {
	  if (T_FLOW) fprintf(stderr," TAB\n");
	  crs[B] = crs[A];
	  continue;
	}

	if (inst == 0140504) {
	  if (T_FLOW) fprintf(stderr," TAX\n");
	  crs[X] = crs[A];
	  continue;
	}

	if (inst == 0140505) {
	  if (T_FLOW) fprintf(stderr," TAY\n");
	  crs[Y] = crs[A];
	  continue;
	}

	if (inst == 0140604) {
	  if (T_FLOW) fprintf(stderr," TBA\n");
	  crs[A] = crs[B];
	  continue;
	}

	if (inst == 0141034) {
	  if (T_FLOW) fprintf(stderr," TXA\n");
	  crs[A] = crs[X];
	  continue;
	}

	if (inst == 0141124) {
	  if (T_FLOW) fprintf(stderr," TYA\n");
	  crs[A] = crs[Y];
	  continue;
	}

	if (inst == 0140104) {
	  if (T_FLOW) fprintf(stderr," XCA\n");
	  crs[B] = crs[A];
	  crs[A] = 0;
	  continue;
	}

	if (inst == 0140204) {
	  if (T_FLOW) fprintf(stderr," XCB\n");
	  crs[A] = crs[B];
	  crs[B] = 0;
	  continue;
	}

	if (inst == 0140407) {
	  if (T_FLOW) fprintf(stderr," TCA\n");
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
	}

	if (inst == 0141210) {
	  if (T_FLOW) fprintf(stderr," TCL\n");
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
	}

	if (inst == 0140600) {
	  if (T_FLOW) fprintf(stderr," SCB\n");
	  crs[KEYS] |= 0100000;
	  continue;
	}

	if (inst == 0140200) {
	  if (T_FLOW) fprintf(stderr," RCB\n");
	  crs[KEYS] &= 077777;
	  continue;
	}

	if (inst == 0140024) {
	  if (T_FLOW) fprintf(stderr," CHS\n");
	  crs[A] ^= 0x8000;
	  continue;
	}

	if (inst == 0140500) {
	  if (T_FLOW) fprintf(stderr," SSM\n");
	  crs[A] |= 0100000;
	  continue;
	}

	if (inst == 0140100) {
	  if (T_FLOW) fprintf(stderr," SSP\n");
	  crs[A] &= 077777;
	  continue;
	}

	if (inst == 0140401) {
	  if (T_FLOW) fprintf(stderr," CMA\n");
	  crs[A] = ~crs[A];
	  continue;
	}

	if (inst == 0140320) {
	  if (T_FLOW) fprintf(stderr," CSA\n");
	  crs[KEYS] = (crs[KEYS] & 077777) | (crs[A] & 0x8000);
	  crs[A] = crs[A] & 077777;
	  continue;
	}

	if (inst == 0141500) {
	  if (T_FLOW) fprintf(stderr," LCLT\n");
lclt:
	  crs[A] = ((crs[KEYS] & 0200) != 0);
	  continue;
	}

	if (inst == 0141501) {
	  if (T_FLOW) fprintf(stderr," LCLE\n");
lcle:
	  crs[A] = ((crs[KEYS] & 0300) != 0);
	  continue;
	}

	if (inst == 0141503) {
	  if (T_FLOW) fprintf(stderr," LCEQ\n");
lceq:
	  crs[A] = ((crs[KEYS] & 0100) != 0);
	  continue;
	}

	if (inst == 0141502) {
	  if (T_FLOW) fprintf(stderr," LCNE\n");
lcne:
	  crs[A] = ((crs[KEYS] & 0100) == 0);
	  continue;
	}

	if (inst == 0141504) {
	  if (T_FLOW) fprintf(stderr," LCGE\n");
lcge:
	  crs[A] = !(crs[KEYS] & 0200);
	  continue;
	}

	if (inst == 0141505) {
	  if (T_FLOW) fprintf(stderr," LCGT\n");
lcgt:
	  crs[A] = ((crs[KEYS] & 0300) == 0);
	  continue;
	}

	if (inst == 0140410) {
	  if (T_FLOW) fprintf(stderr," LLT\n");
	  SETCC_A;
	  goto lclt;
	}

	if (inst == 0140411) {
	  if (T_FLOW) fprintf(stderr," LLE\n");
	  SETCC_A;
	  goto lcle;
	}

	if (inst == 0140412) {
	  if (T_FLOW) fprintf(stderr," LNE\n");
	  SETCC_A;
	  goto lcne;
	}

	if (inst == 0140413) {
	  if (T_FLOW) fprintf(stderr," LEQ\n");
	  SETCC_A;
	  goto lceq;
	}

	if (inst == 0140414) {
	  if (T_FLOW) fprintf(stderr," LGE\n");
	  SETCC_A;
	  goto lcge;
	}

	if (inst == 0140415) {
	  if (T_FLOW) fprintf(stderr," LGT\n");
	  SETCC_A;
	  goto lcgt;
	}

	if (inst == 0141511) {
	  if (T_FLOW) fprintf(stderr," LLLE\n");
	  SETCC_L;
	  goto lcle;
	}

	if (inst == 0141513) {
	  if (T_FLOW) fprintf(stderr," LLEQ\n");
	  SETCC_L;
	  goto lceq;
	}

	if (inst == 0141512) {
	  if (T_FLOW) fprintf(stderr," LLNE\n");
	  SETCC_L;
	  goto lcne;
	}

	if (inst == 0141515) {
	  if (T_FLOW) fprintf(stderr," LLGT\n");
	  SETCC_L;
	  goto lcgt;
	}

	if (inst == 0141110) {
	  if (T_FLOW) fprintf(stderr," LFLT\n");
	  SETCC_F;
	  goto lclt;
	}

	if (inst == 0141111) {
	  if (T_FLOW) fprintf(stderr," LFLE\n");
	  SETCC_F;
	  goto lcle;
	}

	if (inst == 0141113) {
	  if (T_FLOW) fprintf(stderr," LFEQ\n");
	  SETCC_F;
	  goto lceq;
	}

	if (inst == 0141112) {
	  if (T_FLOW) fprintf(stderr," LFNE\n");
	  SETCC_F;
	  goto lcne;
	}

	if (inst == 0141114) {
	  if (T_FLOW) fprintf(stderr," LFGE\n");
	  SETCC_F;
	  goto lcge;
	}

	if (inst == 0141115) {
	  if (T_FLOW) fprintf(stderr," LFGT\n");
	  SETCC_F;
	  goto lcgt;
	}

	if (inst == 0140550) {
	  if (T_FLOW) fprintf(stderr," FLOT\n");
	  templ = crs[A];
	  templ = crs[B] | (templ<<15);
	  tempf = templ;
	  ieeepr4(&tempf);
	  crs[FLTH] = (*(unsigned int *)&tempf) >> 16;
	  crs[FLTL] = (*(unsigned int *)&tempf) & 0xFF00;
	  crs[FEXP] = (*(unsigned int *)&tempf) & 0xFF;
	  continue;
	}

	if (inst == 0140534) {
	  if (T_FLOW) fprintf(stderr," FRN\n");
	  continue;
	}

	if (inst == 0140574) {
	  if (T_FLOW) fprintf(stderr," DFCM\n");
	  if (T_INST) fprintf(stderr," FEXP=%d (dec), FLTH='%o, FLTL='%o\n", crs[FEXP], crs[FLTH], crs[FLTL]);
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
	}

	if (inst == 0141000) {
	  if (T_FLOW) fprintf(stderr," ADLL\n");
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
	}

	if (inst == 0140530) {
	  if (T_FLOW) fprintf(stderr," FCM\n");
	  if (T_INST) fprintf(stderr," FEXP=%d (dec), FLTH='%o, FLTL='%o\n", crs[FEXP], crs[FLTH], crs[FLTL]);
	  *(int *)&tempf = (crs[FLTH]<<16) | (crs[FLTL] & 0xFF00) | (crs[FEXP] & 0xFF);
	  prieee4(&tempf);
	  tempf = -tempf;
	  ieeepr4(&tempf);
	  crs[FLTH] = (*(unsigned int *)&tempf) >> 16;
	  crs[FLTL] = (*(unsigned int *)&tempf) & 0xFF00;
	  crs[FEXP] = (*(unsigned int *)&tempf) & 0xFF;
	  XEXPC(0);
	  continue;
	}

	if (inst == 0140510) {
	  if (T_FLOW) fprintf(stderr," FSZE\n");
	  if (*(int *)(crs+FLTH) == 0)
	    RPL++;
	  continue;
	}

	if (inst == 0140511) {
	  if (T_FLOW) fprintf(stderr," FSNZ\n");
	  if (*(int *)(crs+FLTH) != 0)
	    RPL++;
	  continue;
	}

	if (inst == 0140512) {
	  if (T_FLOW) fprintf(stderr," FSMI\n");
	  if (*(int *)(crs+FLTH) < 0)
	    RPL++;
	  continue;
	}

	if (inst == 0140513) {
	  if (T_FLOW) fprintf(stderr," FSPL\n");
	  if (*(int *)(crs+FLTH) >= 0)
	    RPL++;
	  continue;
	}

	if (inst == 0140514) {
	  if (T_FLOW) fprintf(stderr," FSLE\n");
	  if (*(int *)(crs+FLTH) <= 0)
	    RPL++;
	  continue;
	}

	if (inst == 0140515) {
	  if (T_FLOW) fprintf(stderr," FSGT\n");
	  if (*(int *)(crs+FLTH) > 0)
	    RPL++;
	  continue;
	}

	if (inst == 0140554) {
	  if (T_FLOW) fprintf(stderr," INT\n");
	  tempda[0] = crs[FLTH];
	  tempda[1] = crs[FLTL];
	  tempda[2] = crs[FLTD];
	  tempda[3] = crs[FEXP];
	  prieee8(tempda);
	  if (*(double *)tempda > 1073741823.0 || *(double *)tempda < -1073741824.0)
	    mathexception('f', FC_DFP_OFLOW, ea);
	  else
	    CLEARC;
	  templ = *(double *)tempda;
	  crs[B] = templ & 0x7FFF;
	  crs[A] = templ >> 15;
	  continue;
	}

	if (inst == 0140531) {
	  if (T_FLOW) fprintf(stderr," INTA\n");
	  tempda[0] = crs[FLTH];
	  tempda[1] = crs[FLTL];
	  tempda[2] = crs[FLTD];
	  tempda[3] = crs[FEXP];
	  prieee8(tempda);
	  if (*(double *)tempda > 32767.0 || *(double *)tempda < -32768.0)
	    mathexception('f', FC_DFP_OFLOW, ea);
	  else
	    CLEARC;
	  tempa = *(double *)tempda;
	  continue;
	}

	if (inst == 0140532) {
	  if (T_FLOW) fprintf(stderr," FLTA\n");
	  tempf = *(short *)(crs+A);
	  ieeepr4(&tempf);
	  crs[FLTH] = (*(unsigned int *)&tempf) >> 16;
	  crs[FLTL] = (*(unsigned int *)&tempf) & 0xFF00;
	  crs[FEXP] = (*(unsigned int *)&tempf) & 0xFF;
	  continue;
	}

	if (inst == 0140533) {
	  if (T_FLOW) fprintf(stderr," INTL\n");
	  tempda[0] = crs[FLTH];
	  tempda[1] = crs[FLTL];
	  tempda[2] = crs[FLTD];
	  tempda[3] = crs[FEXP];
	  prieee8(tempda);
	  if (*(double *)tempda > 2147483647.0 || *(double *)tempda < -2147483648.0)
	    mathexception('f', FC_DFP_OFLOW, ea);
	  else
	    CLEARC;
	  templ = *(double *)tempda;
	  continue;
	}

	if (inst == 0140535) {
	  if (T_FLOW) fprintf(stderr," FLTL\n");
	  tempf = *(int *)(crs+L);
	  ieeepr4(&tempf);
	  crs[FLTH] = (*(unsigned int *)&tempf) >> 16;
	  crs[FLTL] = (*(unsigned int *)&tempf) & 0xFF00;
	  crs[FEXP] = (*(unsigned int *)&tempf) & 0xFF;
	  continue;
	}

	if (inst == 0141707) {
	  if (T_FLOW) fprintf(stderr," BMLT\n");
	  goto blr;
	}

	if (inst == 0141711) {
	  if (T_FLOW) fprintf(stderr," BMLE\n");
	  if (!(crs[KEYS] & 020000))
	    RPL = get16(RP);
	  else
	    goto bceq;
	  continue;
	}

	if (inst == 0141602) {
	  if (T_FLOW) fprintf(stderr," BMEQ\n");
	  goto bceq;
	}

#if 0
	if (inst == 0141603) {   /* same opcode as BCNE */
	  if (T_FLOW) fprintf(stderr," BMNE\n");
	  goto bcne;
	}

	/* NOTE: BMGE is equivalent to BLS; this opcode doesn't exist
	   in newer manuals */

	if (inst == 0141606) {
	  if (T_FLOW) fprintf(stderr," BMGE\n");
	  printf("WARNING: BMGE instruction '141606 at #%d\n", instcount);
	  goto bls;
	}
#endif

	if (inst == 0141710) {
#if 0
	  /* this is good for tracing the failure if cpuid is set to 15 in
	     Primos boot.  Weird memory maps for 9950 16MB? */
	  traceflags = -1;
#endif
	  if (T_FLOW) fprintf(stderr," BMGT\n");
	  if (crs[KEYS] & 020000)
	    goto bcne;
	  RPL++;
	  continue;
	}

	if (inst == 0141404) {
	  if (T_FLOW) fprintf(stderr," CRE\n");
	  *(int *)(crs+E) = 0;
	  continue;
	}

	if (inst == 0141410) {
	  if (T_FLOW) fprintf(stderr," CRLE\n");
	  *(int *)(crs+L) = 0;
	  *(int *)(crs+E) = 0;
	  continue;
	}

	if (inst == 0141414) {
	  if (T_FLOW) fprintf(stderr," ILE\n");
	  templ = *(int *)(crs+L);
	  *(int *)(crs+L) = *(int *)(crs+E);
	  *(int *)(crs+E) = templ;
	  continue;
	}

	/* queue instructions */

	if (inst == 0141714) {
	  if (T_FLOW) fprintf(stderr," RTQ\n");
	  ea = apea(NULL);
	  qtop = get16(ea);
	  qbot = get16(ea+1);
	  if (qtop == qbot) {
	    crs[A] = 0;
	    crs[KEYS] |= 0100;
	  } else {
	    qseg = get16(ea+2) & 0x7FFF;
	    qmask = get16(ea+3);
	    qea = MAKEVA(qseg,qtop);
	    crs[A] = get16(qea);
	    qtop = (qtop & ~qmask) | ((qtop+1) & qmask);
	    put16(qtop, ea);
	    crs[KEYS] &= ~0100;
	  }
	  continue;
	}

	if (inst == 0141715) {
	  if (T_FLOW) fprintf(stderr," RBQ\n");
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
	}

	if (inst == 0141716) {
	  if (T_FLOW) fprintf(stderr," ABQ\n");
	  ea = apea(NULL);
	  qtop = get16(ea);
	  qbot = get16(ea+1);
	  qseg = get16(ea+2) & 0x7FFF;
	  qmask = get16(ea+3);
	  qtemp = (qbot & ~qmask) | ((qbot+1) & qmask);
	  if (qtemp == qtop) {  /* queue full */
	    crs[KEYS] |= 0100;
	  } else {
	    qea = MAKEVA(qseg,qbot);
	    put16(crs[A],qea);
	    put16(qtemp, ea+1);
	    crs[KEYS] &= ~0100;
	  }
	  continue;
	}

	if (inst == 0141717) {
	  if (T_FLOW) fprintf(stderr," ATQ\n");
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
	}

	if (inst == 0141757) {
	  if (T_FLOW) fprintf(stderr," TSTQ\n");
	  ea = apea(NULL);
	  qtop = get16(ea);
	  qbot = get16(ea+1);
	  qmask = get16(ea+3);
	  crs[A] = (qbot-qtop) & qmask;
	  SETCC_A;
	  continue;
	}

	if (T_INST) fprintf(stderr," unrecognized generic class 3 instruction!\n");
	printf(" unrecognized generic class 3 instruction %o!\n", inst);

	/* XXX: these are hacks for CPU.FAULT; not sure how to determine whether
	   an instruction is illegal or unimplemented... */
	if (inst == 0141700)
	  fault(ILLINSTFAULT, RPL, 0);
	else
	  fault(UIIFAULT, RPL, 0);
	fatal(NULL);
	
      }


      if (class == 1) {
	if (T_INST) fprintf(stderr," shift group\n");
	scount = -inst & 077;
	if (scount == 0)
	  scount = 0100;
	switch (inst & 01700) {

	case 00000: /* LRL */
	  if (T_FLOW) fprintf(stderr," LRL %d\n", scount);
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
	  if (T_FLOW) fprintf(stderr," LRS %d\n", scount);
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
	  if (T_FLOW) fprintf(stderr," LRR %d\n", scount);
	  crs[KEYS] &= ~0120000;              /* clear C,L */
	  if (scount > 32)
	    scount = scount - 32;
	  utempl = *(unsigned int *)(crs+L);
	  EXPCL(utempl & bitmask32[33-scount]);
	  utempl = (utempl >> scount) | (utempl << (32-scount));
	  *(unsigned int *)(crs+L) = utempl;
	  break;

	case 00400: /* ARL */
	  if (T_FLOW) fprintf(stderr," ARL %d\n", scount);
	  crs[KEYS] &= ~0120000;              /* clear C,L */
	  if (scount <= 16) {
	    EXPCL(crs[A] & bitmask16[17-scount]);
	    crs[A] = crs[A] >> scount;
	  } else {
	    crs[A] = 0;
	  }
	  break;

	case 00500: /* ARS */
	  if (T_FLOW) fprintf(stderr," ARS %d\n", scount);
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
	  if (T_FLOW) fprintf(stderr," ARR %d\n", scount);
	  crs[KEYS] &= ~0120000;              /* clear C,L */
	  scount = ((scount-1)%16)+1;   /* make scount 1-16 */
	  EXPCL(crs[A] & bitmask16[17-scount]);
	  crs[A] = (crs[A] >> scount) | (crs[A] << (16-scount));
	  break;

	case 01000: /* LLL */
	  if (T_FLOW) fprintf(stderr," LLL %d\n", scount);
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
	  if (T_FLOW) fprintf(stderr," LLS %d\n", scount);
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
	  if (T_FLOW) fprintf(stderr," LLR %d\n", scount);
	  crs[KEYS] &= ~0120000;              /* clear C,L */
	  if (scount > 32)
	    scount = scount - 32;
	  utempl = *(unsigned int *)(crs+A);
	  EXPCL(utempl & bitmask32[scount]);
	  utempl = (utempl << scount) | (utempl >> (32-scount));
	  *(unsigned int *)(crs+A) = utempl;
	  break;

	case 01400: /* ALL */
	  if (T_FLOW) fprintf(stderr," ALL %d\n", scount);
	  crs[KEYS] &= ~0120000;              /* clear C,L */
	  if (scount <= 16) {
	    EXPCL(crs[A] & bitmask16[scount]);
	    crs[A] = crs[A] << scount;
	  } else {
	    crs[A] = 0;
	  }
	  break;

	case 01500: /* ALS */
	  if (T_FLOW) fprintf(stderr," ALS %d\n", scount);
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
	  if (T_FLOW) fprintf(stderr," ALR %d\n", scount);
	  crs[KEYS] &= ~0120000;              /* clear C,L */
	  scount = ((scount-1)%16)+1;   /* make scount 1-16 */
	  EXPCL(crs[A] & bitmask16[scount]);
	  crs[A] = (crs[A] << scount) | (crs[A] >> (16-scount));
	  break;

	default:
	  printf("WARNING: unrecognized shift instruction %o at %o/%o\n", inst, RPH, RPL);
	  if (T_INST) fprintf(stderr," unrecognized shift instruction!: %o\n", inst);
	}
	continue;
      }

      if (class == 2) {
	if (T_INST) fprintf(stderr," skip group\n");

	if (inst == 0101000) {
	  if (T_FLOW) fprintf(stderr," NOP\n");
	  continue;
	}

	if (inst == 0100000) {
	  if (T_FLOW) fprintf(stderr," SKP\n");
	  RPL++;
	  continue;
	}

	if (inst == 0101400) {
	  if (T_FLOW) fprintf(stderr," SMI/SLT\n");
	  if (*(short *)(crs+A) < 0)
	    RPL++;
	  continue;
	}

	if (inst == 0100400) {
	  if (T_FLOW) fprintf(stderr," SPL/SGE\n");
	  if (*(short *)(crs+A) >= 0)
	    RPL++;
	  continue;
	}

	if (inst == 0101100) {
	  if (T_FLOW) fprintf(stderr," SLN\n");
	  if (crs[A] & 1)
	    RPL++;
	  continue;
	}

	if (inst == 0100100) {
	  if (T_FLOW) fprintf(stderr," SLZ\n");
	  if (!(crs[A] & 1))
	    RPL++;
	  continue;
	}

	if (inst == 0101040) {
	  if (T_FLOW) fprintf(stderr," SNZ/SNE\n");
	  if (crs[A] != 0)
	    RPL++;
	  continue;
	}

	if (inst == 0100040) {
	  if (T_FLOW) fprintf(stderr," SZE/SEQ\n");
	  if (crs[A] == 0)
	    RPL++;
	  continue;
	}

	if (inst == 0101220) {
	  if (T_FLOW) fprintf(stderr," SLE\n");
	  if (*(short *)(crs+A) <= 0)
	    RPL++;
	  continue;
	}

	if (inst == 0100220) {
	  if (T_FLOW) fprintf(stderr," SGT\n");
	  if (*(short *)(crs+A) > 0)
	    RPL++;
	  continue;
	}

	if (inst == 0101001) {
	  if (T_FLOW) fprintf(stderr," SSC\n");
	  if (crs[KEYS] & 0100000)
	    RPL++;
	  continue;
	}

	if (inst == 0100001) {
	  if (T_FLOW) fprintf(stderr," SRC\n");
	  if (!(crs[KEYS] & 0100000))
	    RPL++;
	  continue;
	}

	if ((inst & 0177760) == 0100260) {
	  m = (inst & 017)+1;
	  if (T_FLOW) fprintf(stderr," SAR %d\n", m);
	  if (!(crs[A] & bitmask16[m]))
	    RPL++;
	  continue;
	}

	if ((inst & 0177760) == 0101260) {
	  m = (inst & 017)+1;
	  if (T_FLOW) fprintf(stderr," SAS %d\n", m);
	  if (crs[A] & bitmask16[m])
	    RPL++;
	  continue;
	}

	if ((inst & 0177760) == 0100240) {
	  m = (inst & 017)+1;
	  if (T_FLOW) fprintf(stderr," SNR %d\n", m);
	  RESTRICT();
	  if (!(sswitch & bitmask16[m]))
	    RPL++;
	  continue;
	}

	if ((inst & 0177760) == 0101240) {
	  m = (inst & 017)+1;
	  if (T_FLOW) fprintf(stderr," SNS %d\n", m);
	  RESTRICT();
	  if (sswitch & bitmask16[m])
	    RPL++;
	  continue;
	}

	if (inst == 0100200) {    /* skip if machine check flop is reset */
	  if (T_FLOW) fprintf(stderr," SMCR\n");
	  RESTRICT();
	  RPL++;
	  continue;
	}

	if (inst == 0101200) {    /* skip if machine check flop is set */
	  if (T_FLOW) fprintf(stderr," SMCS\n");
	  RESTRICT();
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

    /* get ix bits and adjust opcode so that PMA manual opcode
       references can be used directly, ie, if the PMA manual says the
       opcode is '15 02, then 01502 can be used here.  If the PMA
       manual says the opcode is '11, then use 01100 (the XX extended
       opcode bits are zero) */

    i = inst & 0100000;           /* indirect is bit 1 (left/MS bit) */
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
      if (T_INST) fprintf(stderr," ldx/stx opcode adjusted\n");
    }

    if (T_INST) fprintf(stderr," opcode=%5#0o, i=%o, x=%o\n", opcode, i != 0, x != 0);

    switch ((crs[KEYS] & 016000) >> 10) {
    case 0:  /* 16S */
      ea = ea16s(inst, i, x);
      break;
    case 1:  /* 32S */
      ea = ea32s(inst, i, x);
      break;
    case 2:  /* 64R */
    case 3:  /* 32R */
      ea = ea32r64r(earp, inst, i, x, &opcode);
      break;
    case 4:  /* 32I */
      ea = ea32i(earp, inst, i, x);
      fatal("32I not supported");
      break;
    case 6:  /* 64V */
      ea = ea64v(earp, inst, i, x, &opcode, &eabit);
      break;
    default:
      printf("Bad CPU mode in EA calculation, keys = %o\n", crs[KEYS]);
      fatal(NULL);
    }

    if (T_INST) fprintf(stderr," EA: %o/%o  %s\n",ea>>16, ea & 0xFFFF, searchloadmap(ea));


    /* NOTE: basic and dbasic execute instructions from the register file 
       with TRACE ON */

    if (opcode == 00100) {
      if (T_FLOW) fprintf(stderr," JMP\n");
      RP = ea;

#if 1
      /* it seems that on some DIAG tests (see JST), the monitor is
	 incorrectly entered in R-mode, which causes LDL to be
	 interpreted as JEQ, and the test incorrectly halts.  This
	 helps with that, though the wrong tests appear to run, ie, it
	 says it's running test 1 but runs test 2, and some tests fail
	 that should work.  (This latter problem is fixed with the JST
	 change to use the segment number on a long JST) */

      if (prevpc < 0100000 && RP >= 0100000 && (crs[KEYS] & 010000) == 0) {
	printf("Switching to V mode for DIAGS, keys=%o!\n", crs[KEYS]);
	newkeys((crs[KEYS] & ~016000) | 014000);
      }
#endif
      continue;
    }

    if (opcode == 00200) {
      if ((crs[KEYS] & 050000) != 040000) {     /* V/I mode or SP */
	crs[A] = get16(ea);
	if (T_FLOW) fprintf(stderr," LDA ='%o/%d\n", crs[A], *(short *)(crs+A));
      } else {                                  /* R-mode and DP */
	if (T_FLOW) fprintf(stderr," DLD\n");
	*(unsigned int *)(crs+L) = get32(ea);
      }
      continue;
    }

    if (opcode == 00400) {
      if ((crs[KEYS] & 050000) != 040000) {     /* V/I mode or SP */
	if (T_FLOW) fprintf(stderr," STA\n");
	put16(crs[A],ea);
      } else {                                  /* R-mode and DP */
	if (T_FLOW) fprintf(stderr," DST\n");
	put32(*(unsigned int *)(crs+L),ea);
      }
      continue;
    }

    /* NOTE: EQ and LT can be set in the same instruction if overflow
       occurs, for example, '100000+'100000 */

    if (opcode == 00600) {
      crs[KEYS] &= ~0120300;                 /* clear C, L, LT, EQ */
      utempa = crs[A];
      m = get16(ea);
      if ((crs[KEYS] & 050000) != 040000) {     /* V/I mode or SP */
	if (T_FLOW) fprintf(stderr," ADD ='%o/%d\n", m, *(short *)&m);
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
	if (T_FLOW) fprintf(stderr," DAD\n");
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
	/* NOTE: the EQ test prevents use from reusing the ADD code :( */
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
    }

    if (opcode == 00700) {
      crs[KEYS] &= ~0120300;   /* clear C, L, and CC */
      utempa = crs[A];
      m = get16(ea);
      if ((crs[KEYS] & 050000) != 040000) {
	if (T_FLOW) fprintf(stderr," SUB ='%o/%d\n", m, *(short *)&m);
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
	if (T_FLOW) fprintf(stderr," DSB\n");
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
    }

    if (opcode == 00300) {
      m = get16(ea);
      if (T_FLOW) fprintf(stderr," ANA ='%o\n",m);
      crs[A] &= m;
      continue;
    }

    if (opcode == 00500) {
      m = get16(ea);
      if (T_FLOW) fprintf(stderr," ERA ='%o\n", m);
      crs[A] ^= m;
      continue;
    }

    if (opcode == 00302) {
      m = get16(ea);
      if (T_FLOW) fprintf(stderr," ORA ='%o\n", m);
      crs[A] |= m;
      continue;
    }

    if (opcode == 01000) {
      if (T_FLOW) fprintf(stderr," JST\n");

#if 1
      /* NOTE: CPU.KEYS.SR, CPU.LOGICAL.VI, CPU.CLEAR.VI, CPU.MOVE.VI all
	 fail with unexpected seg faults on a 2-word JST instruction that
	 references a weird segment.

	 Ignoring the ea segment number gets past the segfault and seems
	 to begin the actual test, but then it fails with program halts
	 (and for CPU.CLEAR.VI, executes the Case 2 instruction CRLE when
	 it says it's executing Case 1)

	 Using the ea segment number as the JST word address seems to
	 work better, and executes CRE like it should.  However, the
	 program still halts in the monitor portion (see below) and
	 this hack causes the Primos rev 22 boot to fail, even before
	 the memory test... :(

0/140561: 43400         A='0/0 B='177777/-1 X=147635/-12387 Y=177777/-1 C=0 L=0 LT=0 EQ=1       #25131753 [ 0]
 opcode=00100, i=0, x=40000
 ea32r64r: i=0, x=1
 special, new opcode=00100, class=0
 Class 0, new ea=3
 Preindex, ea=3, X='147635/-12387
 Preindex, new ea=147640
 EA: 0/147640
 JMP

0/147640: 5414          A='0/0 B='177777/-1 X=147635/-12387 Y=177777/-1 C=0 L=0 LT=0 EQ=1       #25131754 [ 0]
 opcode=00200, i=0, x=0
 ea32r64r: i=0, x=0
 special, new opcode=00203, class=0
 Class 0, new ea=115561
 EA: 0/115561
 JEQ

0/115561: 0             A='0/0 B='177777/-1 X=147635/-12387 Y=177777/-1 C=0 L=0 LT=0 EQ=1       #25131755 [ 0]
 generic class 0
 HLT
      */

      ea = MAKEVA(RPH, ea & 0xFFFF);
#endif
      if (amask == 0177777)
	m = RPL;
      else
	m = (get16(ea) & ~amask) | RPL;
      put16(m, ea);
      RP = INCVA(ea,1);
      if ((RP & RINGMASK32) == 0)
	inhcount = 1;
      continue;
    }

    /* this should set the C and L bits like subtract */

    if (opcode == 01100) {
      m = get16(ea);
      if (T_FLOW) fprintf(stderr," CAS ='%o/%d\n", m, *(short *)&m);
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
    }

    if (opcode == 01200) {
      if (T_FLOW) fprintf(stderr," IRS\n");
      m = get16(ea) + 1;
      put16(m,ea);
      if (m == 0)
	RPL++;
      continue;
    }

    if (opcode == 01300) {
      if (T_FLOW) fprintf(stderr," IMA\n");
      m = get16(ea);
      put16(crs[A],ea);
      crs[A] = m;
      continue;
    }

    if (opcode == 01400) {
      if (T_FLOW) fprintf(stderr," JSY\n");
      crs[Y] = RPL;
      RP = ea;
      continue;
    }

    if (opcode == 01402) {
      if (T_FLOW) fprintf(stderr," JSXB\n");
      *(unsigned int *)(crs+XB) = RP;
      RP = ea;
      continue;
    }

    if (opcode == 01500) {
      if (T_FLOW) fprintf(stderr," STX\n");
      put16(crs[X],ea);
      continue;
    }

    if (opcode == 01600) {
      m = get16(ea);
      if (T_FLOW) fprintf(stderr," MPY ='%o/%d\n", m, *(short *)&m);
      templ = *(short *)(crs+A) * *(short *)&m;
      CLEARC;
      if (crs[KEYS] & 010000) {          /* V/I mode */
	*(int *)(crs+L) = templ;
      } else {                           /* R/S mode */
	if (crs[A] == 0x8000 && m == 0x8000)
	  mathexception('i', FC_INT_OFLOW, 0);
	else {
	  crs[A] = (templ >> 15);
	  crs[B] = templ & 077777;
	}
      }
      continue;
    }

    if (opcode == 01603) {
      templ = get32(ea);
      if (T_FLOW) fprintf(stderr," MPL ='%o/%d\n", templ, *(int *)&templ);
      templl = (long long)(*(int *)(crs+L)) * (long long)templ;
      *(long long *)(crs+L) = templl;
      CLEARC;
      continue;
    }

    if (opcode == 01700) {
      m = get16(ea);
      if (T_FLOW) fprintf(stderr," DIV ='%o/%d\n", m, *(short *)&m);
      if (crs[KEYS] & 010000) {          /* V/I mode */
	templ = *(int *)(crs+A);
      } else {                      /* R/S mode */
	templ = *(short *)(crs+A);  /* convert to 32-bit signed */
	templ = (templ<<15) | (crs[B] & 0x7FFF);
	//printf("\nR-mode DIV: A='%o/%d, B='%o/%d, templ='%o/%d\n", crs[A], *(short *)(crs+A), crs[B], *(short *)(crs+B), templ, templ);
      }
      if (m != 0 && abs(*(short *)(crs+A)) < abs(*(short *)&m)) {
	crs[A] = templ / *(short *)&m;
	crs[B] = templ % *(short *)&m;
	CLEARC;
	//printf("DIV results: m='%o/%d, A='%o/%d, B='%o/%d\n", m, *(short *)&m, crs[A], *(short *)(crs+A), crs[B], *(short *)(crs+B));
      } else {
	SETC;
	//printf("DIV overflow\n");
      }
      continue;
    }

    if (opcode == 01703) {
      templ = get32(ea);
      if (T_FLOW) fprintf(stderr," DVL ='%o/%d\n", templ, *(int *)&templ);
      templl = *(long long *)(crs+L);
      if (templ != 0) {
	*(int *)(crs+L) = templl / templ;
	*(int *)(crs+E) = templl % templ;
	CLEARC;
      } else
	SETC;
      continue;
    }

    if (opcode == 03500) {
      if (T_FLOW) fprintf(stderr," LDX\n");
      crs[X] = get16(ea);
      continue;
    }

    if (opcode == 00101) {
      if (crs[KEYS] & 010000) {          /* V/I mode */
	if (T_FLOW) fprintf(stderr," EAL\n");
	*(unsigned int *)(crs+L) = ea;
      } else {
	if (T_FLOW) fprintf(stderr," EAA\n");
	crs[A] = ea;
      }
      continue;
    }

    if (opcode == 00203) {
      if (crs[KEYS] & 010000) {          /* V/I mode */
	if (T_FLOW) fprintf(stderr," LDL\n");
	*(unsigned int *)(crs+L) = get32(ea);
      } else {
	if (T_FLOW) fprintf(stderr," JEQ\n");
	if (*(short *)(crs+A) == 0)
	  RPL = ea;
      }
      continue;
    }

    if (opcode == 00703) {
      if (crs[KEYS] & 010000) {          /* V/I mode */
	utempl2 = get32(ea);
	if (T_FLOW) fprintf(stderr," SBL ='%o/%d\n", utempl2, *(int *)&utempl2);
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
	if (T_FLOW) fprintf(stderr," JGE\n");
	if (*(short *)(crs+A) >= 0)
	  RPL = ea;
      }
      continue;
    }

    if (opcode == 01002) {
      if (crs[KEYS] & 010000) {          /* V/I mode */
	//traceflags = ~TB_MAP;
	if (T_FLOW || T_PCL) fprintf(stderr,"#%d %o/%o: PCL %o/%o\n", instcount, RPH, RPL-2, ea>>16, ea&0xFFFF);
	pcl(ea);
      } else {
	if (T_FLOW) fprintf(stderr," CREP\n");
	put16(RPL,crs[S]++);
	RPL = ea;
      }
      continue;
    }

    if (opcode == 00503) {
      if (crs[KEYS] & 010000) {          /* V/I mode */
	utempl = get32(ea);
	if (T_FLOW) fprintf(stderr," ERL ='%o\n", utempl);
	*(unsigned int *)(crs+L) ^= utempl;
      } else {
	if (T_FLOW) fprintf(stderr," JGT\n");
	if (*(short *)(crs+A) > 0)
	  RPL = ea;
      }
      continue;
    }

    if (opcode == 00403) {
      if (crs[KEYS] & 010000) {          /* V/I mode */
	if (T_FLOW) fprintf(stderr," STL\n");
	put32(*(unsigned int *)(crs+L),ea);
      } else {
	if (T_FLOW) fprintf(stderr," JLE\n");
	if (*(short *)(crs+A) <= 0)
	  RPL = ea;
      }
      continue;
    }

    if (opcode == 00603) {
      if (crs[KEYS] & 010000) {          /* V/I mode */
	utempl2 = get32(ea);
	if (T_FLOW) fprintf(stderr," ADL ='%o/%d\n", utempl2, *(int *)&utempl2);
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
	if (T_FLOW) fprintf(stderr," JLT\n");
	if (*(short *)(crs+A) < 0)
	  RPL = ea;
      }
      continue;
    }

    if (opcode == 00303) {
      if (crs[KEYS] & 010000) {          /* V/I mode */
	utempl = get32(ea);
	if (T_FLOW) fprintf(stderr," ANL ='%o\n", utempl);
	*(unsigned int *)(crs+L) &= utempl;
      } else {
	if (T_FLOW) fprintf(stderr," JNE\n");
	if (*(short *)(crs+A) != 0)
	  RPL = ea;
      }
      continue;
    }

    if (opcode == 01202) {
      if (T_FLOW) fprintf(stderr," EAXB\n");
      *(ea_t *)(crs+XB) = ea;
      continue;
    }

    if (opcode == 01502) {
      if (crs[KEYS] & 010000) {          /* V/I mode */
	if (T_FLOW) fprintf(stderr," DFLX\n");
	crs[X] = get16(ea) * 4;
      } else {
	if (T_FLOW) fprintf(stderr," JDX\n");
	crs[X]--;
	if (crs[X] != 0)
	  RPL = ea;
      }
      continue;
    }

    if (opcode == 03502) {
      if (T_FLOW) fprintf(stderr," STY\n");
      put16(crs[Y],ea);
      continue;
    }

    if (opcode == 01503) {
      if (crs[KEYS] & 010000) {          /* V/I mode */
	if (T_FLOW) fprintf(stderr," QFLX\n");
	crs[X] = get16(ea) * 8;
      } else {
	if (T_FLOW) fprintf(stderr," JIX\n");
	crs[X]++;
	if (crs[X] != 0)
	  RPL = ea;
      }
      continue;
    }

    if (opcode == 01501) {
      if (T_FLOW) fprintf(stderr," FLX\n");
      crs[X] = get16(ea) * 2;
      continue;
    }

    if (opcode == 03501) {
      if (T_FLOW) fprintf(stderr," LDY\n");
      crs[Y] = get16(ea);
      continue;
    }

    if (opcode == 0101) {
      if (T_FLOW) fprintf(stderr," EAL\n");
      *(ea_t *)(crs+L) = ea;
      continue;
    }

    if (opcode == 03503) {
      if (T_FLOW) fprintf(stderr," JSX\n");
      crs[X] = RPL;
      RP = ea;
      continue;
    }

    /* XXX: this should set the L bit like subtract */

    if (opcode == 01103) {
      if (T_FLOW) fprintf(stderr," CLS\n");
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
    }

    if (opcode == 00601) {
      if (T_FLOW) fprintf(stderr," FAD\n");
      if (T_INST) fprintf(stderr," FEXP=%d (dec), FLTH='%o, FLTL='%o\n", crs[FEXP], crs[FLTH], crs[FLTL]);
      //if (T_INST) fprintf(stderr," ea EXP=%d (dec), ea H='%o, ea L='%o\n", (mem[ea+1] & 0xFF), mem[ea], (mem[ea+1] & 0xFF00));
      *(int *)&tempf = (crs[FLTH]<<16) | (crs[FLTL] & 0xFF00) | (crs[FEXP] & 0xFF);
      prieee4(&tempf);
      *(int *)&tempf1 = get32(ea);
      prieee4(&tempf1);
      tempf += tempf1;
      ieeepr4(&tempf);
      crs[FLTH] = (*(unsigned int *)&tempf) >> 16;
      crs[FLTL] = (*(unsigned int *)&tempf) & 0xFF00;
      crs[FEXP] = (*(unsigned int *)&tempf) & 0xFF;
      XEXPC(0);
      continue;
    }

    /* this is implemented as a subtract on some models */

    if (opcode == 01101) {
      if (T_FLOW) fprintf(stderr," FCS\n");
      if (T_INST) fprintf(stderr," FEXP=%d (dec), FLTH='%o, FLTL='%o\n", crs[FEXP], crs[FLTH], crs[FLTL]);
      //if (T_INST) fprintf(stderr," ea EXP=%d (dec), ea H='%o, ea L='%o\n", (mem[ea+1] & 0xFF), mem[ea], (mem[ea+1] & 0xFF00));
      *(int *)&tempf = (crs[FLTH]<<16) | (crs[FLTL] & 0xFF00) | (crs[FEXP] & 0xFF);
      prieee4(&tempf);
      *(int *)&tempf1 = get32(ea);
      prieee4(&tempf1);
      crs[KEYS] &= ~0300;
      if (tempf == tempf1) {
	RPL++;
	crs[KEYS] |= 0100;
      } else if (tempf < tempf1) {
	RPL += 2;
	crs[KEYS] |= 0200;
      }
      continue;
    }

    if (opcode == 01701) {
      if (T_FLOW) fprintf(stderr," FDV\n");
      if (T_INST) fprintf(stderr," FEXP=%d (dec), FLTH='%o, FLTL='%o\n", crs[FEXP], crs[FLTH], crs[FLTL]);
      //if (T_INST) fprintf(stderr," ea EXP=%d (dec), ea H='%o, ea L='%o\n", (mem[ea+1] & 0xFF), mem[ea], (mem[ea+1] & 0xFF00));
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
    }

    if (opcode == 0201) {
      if (T_FLOW) fprintf(stderr," FLD\n");
      crs[FLTH] = get16(ea);
      m = get16(ea+1);
      crs[FLTL] = m & 0xFF00;
      crs[FEXP] = m & 0xFF;
      continue;
    }

    if (opcode == 01601) {
      if (T_FLOW) fprintf(stderr," FMP\n");
      if (T_INST) fprintf(stderr," FEXP=%d (dec), FLTH='%o, FLTL='%o\n", crs[FEXP], crs[FLTH], crs[FLTL]);
      //if (T_INST) fprintf(stderr," ea EXP=%d (dec), ea H='%o, ea L='%o\n", (mem[ea+1] & 0xFF), mem[ea], (mem[ea+1] & 0xFF00));
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
    }

    if (opcode == 00701) {
      if (T_FLOW) fprintf(stderr," FSB\n");
      if (T_INST) fprintf(stderr," FEXP=%d (dec), FLTH='%o, FLTL='%o\n", crs[FEXP], crs[FLTH], crs[FLTL]);
      //if (T_INST) fprintf(stderr," ea EXP=%d (dec), ea H='%o, ea L='%o\n", (mem[ea+1] & 0xFF), mem[ea], (mem[ea+1] & 0xFF00));
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
    }

    if (opcode == 0401) {
      if (T_FLOW) fprintf(stderr," FST\n");
      if (crs[FEXP] & 0xFF00)
	mathexception('f', FC_SFP_STORE, ea);
      put16(crs[FLTH],ea);
      put16((crs[FLTL] & 0xFF00) | crs[FEXP],ea+1);
      CLEARC;
      continue;
    }

    if (opcode == 0602) {
      if (T_FLOW) fprintf(stderr," DFAD\n");
      if (T_INST) fprintf(stderr," FEXP=%d (dec), FLTH='%o, FLTL='%o\n", crs[FEXP], crs[FLTH], crs[FLTL]);
      //if (T_INST) fprintf(stderr," ea EXP=%d (dec), ea H='%o, ea L='%o\n", (mem[ea+1] & 0xFF), mem[ea], (mem[ea+1] & 0xFF00));
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
    }

    if (opcode == 01102) {
      if (T_FLOW) fprintf(stderr, " DFCS\n");
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
    }

    if (opcode == 01702) {
      if (T_FLOW) fprintf(stderr," DFDV\n");
      if (T_INST) fprintf(stderr," FEXP=%d (dec), FLTH='%o, FLTL='%o\n", crs[FEXP], crs[FLTH], crs[FLTL]);
      //if (T_INST) fprintf(stderr," ea EXP=%d (dec), ea H='%o, ea L='%o\n", (mem[ea+1] & 0xFF), mem[ea], (mem[ea+1] & 0xFF00));
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
    }

    if (opcode == 0202) {
      if (T_FLOW) fprintf(stderr," DFLD\n");
      *(double *)tempda = get64(ea);
      crs[FLTH] = tempda[0];
      crs[FLTL] = tempda[1];
      crs[FLTD] = tempda[2];
      crs[FEXP] = tempda[3];
      continue;
    }

    if (opcode == 01602) {
      if (T_FLOW) fprintf(stderr," DFMP\n");
      if (T_INST) fprintf(stderr," FEXP=%d (dec), FLTH='%o, FLTL='%o\n", crs[FEXP], crs[FLTH], crs[FLTL]);
      //if (T_INST) fprintf(stderr," ea EXP=%d (dec), ea H='%o, ea L='%o\n", (mem[ea+1] & 0xFF), mem[ea], (mem[ea+1] & 0xFF00));
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
    }

    if (opcode == 0702) {
      if (T_FLOW) fprintf(stderr," DFSB\n");
      if (T_INST) fprintf(stderr," FEXP=%d (dec), FLTH='%o, FLTL='%o\n", crs[FEXP], crs[FLTH], crs[FLTL]);
      //if (T_INST) fprintf(stderr," ea EXP=%d (dec), ea H='%o, ea L='%o\n", (mem[ea+1] & 0xFF), mem[ea], (mem[ea+1] & 0xFF00));
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
    }

    if (opcode == 0402) {
      if (T_FLOW) fprintf(stderr," DFST\n");
      tempda[0] = crs[FLTH];
      tempda[1] = crs[FLTL];
      tempda[2] = crs[FLTD];
      tempda[3] = crs[FEXP];
      put64(*(double *)tempda, ea);
      continue;
    }

    if (opcode == 01302) {
      if (T_FLOW) fprintf(stderr," EALB\n");
      *(ea_t *)(crs+LB) = ea;
      continue;
    }

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

    if (opcode == 0301) {
      if (T_FLOW) fprintf(stderr," STLR\n");
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
    }

    if (opcode == 0501) {
      if (T_FLOW) fprintf(stderr," LDLR\n");
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
    }

    if (opcode == 01401) {
      if (T_FLOW) fprintf(stderr," EIO\n");
      crs[KEYS] &= ~0100;      /* reset EQ */
      pio(ea & 0xFFFF);
      continue;
    }

    if (opcode == 00102) {
      if (T_FLOW) fprintf(stderr," XEC\n");
      utempa = get16(ea);
      //utempl = RP-2;
      //printf("RPL %o/%o: XEC instruction %o|%o, ea is %o/%o, new inst = %o \n", utempl>>16, utempl&0xFFFF, inst, get16(utempl+1), ea>>16, ea&0xFFFF, utempa);
      inst = utempa;
      earp = INCVA(ea,1);
      goto xec;
    }

    if (opcode == 00103) {
      if (T_FLOW) fprintf(stderr," ENTR\n");
      utempa = crs[S];
      crs[S] -= ea;
      put16(utempa,crs[S]);
      continue;
    }

#if 0
    if (mem[066] != 0) {
      if (T_FLOW) fprintf(stderr," JST* '66 [%o]\n", mem[066]);
      mem[mem[066]] = RPL;
      RPL = mem[066]+1;
      continue;
    }
#endif
    printf("Unknown memory reference opcode: %o\n", opcode);
    fatal(NULL);
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

  /* if the svc fault vector is zero, interpret the svc here.  This
     allows the emulator to run r-mode programs directly */

  if ((crs[KEYS] & 010) || get16(065) != 0) {
    fault(SVCFAULT, 0, 0);
    fatal("Returned from SVC fault");
  }

  /* get svc code word, break into class and function */

  code = mem[RPL];
  class = (code >> 6) & 077;
  if (class == 0)
    class = 1;
  func = code & 077;

  /* determine argument list location and create arg list vector */

  if (code & 0100000)
    argl = mem[RPL-2];
  else if (code & 040000)
    argl = RPL+2;
  else
    argl = RPL+1;

  if (T_INST) fprintf(stderr," code=%o, class=%o, func=%o, argl=%o\n", code, class, func, argl);
  if (class > MAXCLASS || func > MAXFUNC)
    goto badsvc;

  if (T_FLOW) fprintf(stderr," name=%s, #args=%d, LOC args=%o\n", svcinfo[class][func].name, svcinfo[class][func].numargs, svcinfo[class][func].locargs);

  /* if location '65 is set, do indirect JST to handle svc */

  if (mem[065] != 0) {
    if (T_INST) fprintf(stderr," JST* '65 [%o]\n", mem[065]);
    mem[mem[065]] = RPL;
    RPL = mem[065]+1;
    return;
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

  if (T_INST) fprintf(stderr," return=%o, actargs=%d\n", argl, actargs);

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

  if (T_INST) fprintf(stderr," returning from SVC to %o\n", argl);
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
    if (T_INST) fprintf(stderr," bouncing svc error to address %o\n", RPL);
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
  short class;
  short func;
  short device;

  RESTRICT();
  class = inst >> 14;
  func = (inst >> 6) & 017;
  device = inst & 077;
  if (T_INST) fprintf(stderr," pio, class=%d, func='%o, device='%o\n", class, func, device);
  if (devmap[device])
    devmap[device](class, func, device);
  else {
    fprintf(stderr,"pio: no handler for device '%o\n", device);
    return;
    fatal(NULL);
  }
}
