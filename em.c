/* Pr1me Computer emulator, Jim Wilcoxson (prirun@gmail.com), April 4, 2005
   Copyright (C) 2005-2019, Jim Wilcoxson.  All Rights Reserved.

   Emulates a Prime Computer system by:
   - booting from a Prime disk image (normal usage)
   - booting from a Prime MAGSAV tape
   - restoring a Prime R-mode .save image from the host file system

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
   real-time clock automatically for older models (cpuid < 15).  If
   maps are not available, use the Primos SE command to set the clock
   manually after the system boots: 
        SE -MMDDYY HHMM

   -------------
   Usage:  (to load and start an R-mode runfile directly from the Unix FS)

   $ ./em -ss 114 -boot *DOS64 2>/dev/null   (-ss optional)


   -------------
   Usage:  to load SAM.SAVE from Unix FS and run diagnostics from pdev 2466

$ time ./em  -cpuid 5 -boot SAM.SAVE 14114 2>err
[SAM Rev. 16.2, DTS Release: 0004.A, Copyright (c) 1990, Prime Computer, Inc.]

Enter physical device = 2466

QUICK VERIFY MODE Enabled; Enter 'RESET QVFY' for normal operation. 
Enter 'SET DCM' to display CASE messages. 
Enter 'LOAD;RUN' for Default Execution

SAM> 

   --------------
   Usage:  to load initial boot from tape, then prompt for disk pdev

$ time ./em -boot 1005 -tport 8000 -cpuid 5
Boot file is mt0                 <--- note tape drive boot
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

TRACING:

The trace makefile target includes tracing code.  Instruction details
are spewed to trace.log depending on the trace flags, set with -trace.
If -trace is not used, only TRACEA (trace all) traces are enabled.

To activate more tracing, use -trace followed by flags to indicate
which parts you want to trace, for example -trace map will activate
all T_MAP traces.

The "flush" trace flag turns on line-by-line trace file flushing.
This is useful if the emulator itself is crashing, eg, with a (Unix)
seg fault.

The "off" trace flag can be added to initially disable tracing.
Ctrl-t at the console will toggle tracing on or off.  To trace only a
particular instruction, use "off", then do gv.traceflags =
gv.savedtraceflags at the beginning of the instruction emulation.
To trace from an instruction forward, do the previous and add
gv.tracetriggered = 1; this simulates the Ctrl-t.

-DNOTRACE disables tracing altogether for increased performance.

*/

/* force GCC to honor inline for about a 25% speed-up */

#define inline inline __attribute__((always_inline))

#ifdef __APPLE__
  #define OSX 1
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <setjmp.h>
#include <sys/time.h>
#include <signal.h>
#include <unistd.h>
#include <sys/select.h>
#include <termios.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <netinet/tcp.h>
#include <time.h>
#include <sys/file.h>
#include <glob.h>

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

/* procs needing forward declarations */

static void fault(unsigned short fvec, unsigned short fcode, ea_t faddr) __attribute__ ((noreturn));
static void fatal(char *msg) __attribute__ ((noreturn));
static void warn(char *msg);
static void macheck (unsigned short p300vec, unsigned short chkvec, unsigned int dswstat, unsigned int dswrma) __attribute__ ((noreturn));

/* condition code macros */

#define CLEARCC putcrs16(KEYS, getcrs16(KEYS) & ~0300)
#define CLEAREQ putcrs16(KEYS, getcrs16(KEYS) & ~0100)
#define CLEARLT putcrs16(KEYS, getcrs16(KEYS) & ~0200)
#define SETEQ putcrs16(KEYS, getcrs16(KEYS) | 0100)
#define SETLT putcrs16(KEYS, getcrs16(KEYS) | 0200)

/* set condition codes based on a 16-bit signed value */

#define SETCC_16(val16) \
  if ((val16) != 0) \
    putcrs16(KEYS, (getcrs16(KEYS) & ~0300) | ((unsigned short)((val16) & 0x8000) >> 8)); \
  else \
    putcrs16(KEYS, (getcrs16(KEYS) & ~0300) | 0100);

/* set condition codes based on A register (16-bit signed) */

#define SETCC_A SETCC_16(getcrs16(A))

/* set condition codes based on a 32-bit signed value */

#define SETCC_32(val32) \
  if ((val32) != 0) \
    putcrs16(KEYS, (getcrs16(KEYS) & ~0300) | ((unsigned int)((val32) & 0x80000000) >> 24)); \
  else \
    putcrs16(KEYS, (getcrs16(KEYS) & ~0300) | 0100);

/* set condition codes based on L register (32-bit signed) */

#define SETCC_L SETCC_32(getgr32(GR2))

/* set condition codes based on V-mode FP accumulator

   NOTES: 

   -Prime considers anything with a zero fraction to be zero,
   even if the exponent is non-zero (this is a "dirty zero")

   - Prime only tested 32 bits of the fraction, even for double
   precision.  It expected DP floats to be normalized, or mostly
   normalized.

   XXX: #define SETCC_F SETCC_32(getcrs32(FLTH))
*/
  
#define SETCC_F \
  CLEARCC; \
  if (getcrs32s(FLTH) < 0) \
    SETLT; \
  else if (getcrs32s(FLTH) == 0) \
    SETEQ;

#define SETCC_D SETCC_F


/* macros for handling the C-bit (overflow) and L-bit (carry out) */

#define EXPC(onoff) \
  if ((onoff)) putcrs16(KEYS, getcrs16(KEYS) | 0100000);	\
  else putcrs16(KEYS, getcrs16(KEYS) & 077777)

#define SETC putcrs16(KEYS, getcrs16(KEYS) | 0100000)
#define CLEARC putcrs16(KEYS, getcrs16(KEYS) & 077777)

/* EXPCL sets both the C and L bits for shift instructions */

#define EXPCL(onoff) \
  if ((onoff)) putcrs16(KEYS, getcrs16(KEYS) | 0120000);	\
  else putcrs16(KEYS, getcrs16(KEYS) & ~0120000)

#define SETCL putcrs16(KEYS, getcrs16(KEYS) | 0120000)
#define CLEARCL putcrs16(KEYS, getcrs16(KEYS) & ~0120000)

#define SETL(onoff) \
  if ((onoff)) putcrs16(KEYS, getcrs16(KEYS) | 020000);	\
  else putcrs16(KEYS, getcrs16(KEYS) & ~020000)

/* XSETL is a dummy to indicate that the L-bit may not be set correctly */

#define XSETL(onoff) SETL(onoff)

/* these macros are for the VI-mode branch insructions */

#define BCLT if   (getcrs16(KEYS) & 0200)  RPL = iget16(RP); else INCRP
#define BCLE if   (getcrs16(KEYS) & 0300)  RPL = iget16(RP); else INCRP
#define BCEQ if   (getcrs16(KEYS) & 0100)  RPL = iget16(RP); else INCRP
#define BCNE if (!(getcrs16(KEYS) & 0100)) RPL = iget16(RP); else INCRP
#define BCGE if (!(getcrs16(KEYS) & 0200)) RPL = iget16(RP); else INCRP
#define BCGT if (!(getcrs16(KEYS) & 0300)) RPL = iget16(RP); else INCRP
#define BLS  if  (getcrs16(KEYS) & 020000) RPL = iget16(RP); else INCRP
#define BHNE(r) if (getgr16((r)) != 0) RPL = iget16(RP); else INCRP
#define BRNE(r) if (getgr32((r)) != 0) RPL = iget16(RP); else INCRP

/* expressions for logicize instructions */

#define LCLT ((getcrs16(KEYS) & 0200) != 0)
#define LCLE ((getcrs16(KEYS) & 0300) != 0)
#define LCEQ ((getcrs16(KEYS) & 0100) != 0)
#define LCNE ((getcrs16(KEYS) & 0100) == 0)
#define LCGE !(getcrs16(KEYS) & 0200)
#define LCGT ((getcrs16(KEYS) & 0300) == 0)

/* macro for restricted instructions (uses current program counter) */

#define RESTRICT() if (RP & RINGMASK32) fault(RESTRICTFAULT, 0, 0);

/* same macro, but uses a passed program counter */

#define RESTRICTR(rpring) if ((rpring) & RINGMASK32) fault(RESTRICTFAULT, 0, 0);

/* trace flags to control aspects of emulator tracing:

   T_EAR	trace R-mode effective address calculation
   T_EAV	trace V-mode effective address calculation
   T_EAI	trace I-mode effective address calculation
   T_FLOW       instruction summary
   T_INST	detailed instruction trace
   T_MODE       trace CPU mode changes
   T_EAAP       AP effective address calculation
   T_DIO        disk I/O
   T_TIO        tape I/O
   T_RIO        ring network I/O
   T_TERM       terminal output (tnou[a])
   T_MAP        segmentation
   T_PCL        PCL instructions
   T_FAULT      Faults
   T_PX         Process exchange
   T_LM         License manager
   T_TLB        STLB and IOTLB changes
*/

#define T_EAR   0x00000001
#define T_EAV   0x00000002
#define T_EAI   0x00000004
#define T_INST  0x00000008
#define T_FLOW  0x00000010
#define T_MODE  0x00000020
#define T_EAAP  0x00000040
#define T_DIO   0x00000080
#define T_MAP   0x00000100
#define T_PCL   0x00000200
#define T_FAULT 0x00000400
#define T_PX    0x00000800
#define T_TIO   0x00001000
#define T_TERM  0x00002000
#define T_RIO   0x00004000
#define T_LM    0x00008000
#define T_PUT   0x00010000
#define T_GET   0x00020000
#define T_EAS   0x00040000
#define T_TLB   0x00080000

#define BITMASK16(b) (0x8000 >> ((b)-1))
#define BITMASK32(b) ((unsigned int)(0x80000000) >> ((b)-1))

#ifdef NOTRACE
  #define TRACE(flags, formatargs...)
  #define TRACEA(formatargs...)
#else
  #define TRACE(flags, formatargs...) if (gv.traceflags & (flags)) fprintf(gv.tracefile,formatargs)
  #define TRACEA(formatargs...) fprintf(gv.tracefile,formatargs)
#endif

/* traceprocs is an array of (operating system) procedure names we're
   tracing, with flags and associated data

   numtraceprocs is the number of entries in traceprocs, 0=none */

#define MAXTRACEPROCS 2
static struct {
  char  name[11];                    /* procedure name */
  int   ecb;                         /* ecb ea of proc */
  int   sb;                          /* sb before the call */
  int   oneshot;                     /* disable trace after call? */
} traceprocs[MAXTRACEPROCS];

#define TRACEUSER (gv.traceuser == 0 || getcrs16(OWNERL) == gv.traceuser)

/* NOTE: Primos II gives "NOT FOUND" on STARTUP 2460 command if sense
   switches are set to 014114.  But DIAGS like this setting. :( */

static unsigned short sswitch = 014114;     /* sense switches, set with -ss & -boot */
static unsigned short dswitch = 0;          /* data (down) switches, set with -sd */
static unsigned short lights = 0;           /* control panel lights */
static unsigned short bootregs = 0;         /* load regs and keys from rvec */
static unsigned short sensorabort = 0;      /* if 1, causes a sensor check */
static unsigned short firstbdx = 0;         /* backstop sleep flag */

/* NOTE: the default cpuid is a P750: 1 MIPS, 8MB of memory

   9/21/2007: changed the default to a P9950 so that the date gets
   automagically set via special VCP commands.  The 9950 was a 2 MIP
   CPU w/max of 16MB physical memory via the extended page map format */

static unsigned short cpuid = 15;      /* STPM CPU model, set with -cpuid */

/* STLB cache structure is defined here; the actual stlb is in gv.
   There are several different styles on Prime models.  This is
   modeled after the 6350 STLB, but is only 1-way associative.
   The hit rate has been measured to be over 99.8% with a single
   user.

   Instead of using a valid/invalid bit, a segment number of 0xFFFF
   (note that the fault, ring, and E bits are set) means "invalid",
   since it won't match any 12-bit segment number.  This eliminates
   testing the valid bit in mapva.

   As a speed-up, the unmodified bit is stored in access[2], where
   Ring 2 access should be kept.  This makes the structure exactly 16
   bytes, so indexing into the table can use a left shift 4
   instruction rather than a multiply by 20.  It saves a multiply in
   the fast path of a _very_ speed-critical routine (mapva).
*/

#define STLBENTS 512
#define STLB_UNMODIFIED_BIT 2 /* stored in access[2] */

typedef struct {
  unsigned int pmaddr;        /* Prime phys addr of page table flag word */
  unsigned int ppa;           /* physical page address (PPN << 10) */
  unsigned short procid;      /* process id for segments >= '4000 */
  short seg;                  /* segment number (0-0xFFF), 0xFFFF = invalid */
  char access[4];             /* ring n access rights */
  //char unmodified;            /* 1 = page not modified, 0 = modified */
  //char shared;                /* 1 if page is shared and can't be cached */
  //char valid;                 /* 1 if STLB entry is valid, zero otherwise */
#ifndef NOTRACE
  unsigned int load_ic;      /* instruction where STLB was loaded, (debug) */
#endif
} stlbe_t;

/* The IOTLB stores translations for each page of the I/O segments 0-3 */

#define IOTLBENTS 64*4

typedef struct {
  unsigned int ppa;           /* physical page address (PPN << 10) */
  char valid;                 /* 1 if IOTLB entry is valid, zero otherwise */
} iotlbe_t;

/* maximum indirect levels is 8 according to T&M CPUT4 */

#define INDLEVELS 8

/* the emulator uses a special, very small address translation cache
   to hold the virtual page address and corresponding MEM[] pointer
   for a few special pages:

   - the 4 base registers (PBBR, SBBR, LBBR, XBBR)
   - the current instruction page (RPBR)
   - sector zero (S0BR) of the current procedure
   - field address registers 0 and 1 (F0BR, F1BR)
   - "unknown", used for indirect references (UNBR)

   When an effective address is calculated, eap is set to point to one
   of these cache entries.  If the EA, which is a virtual address,
   references the same page as the cache entry pointed to by eap, then
   the mapva call can be bypassed and the physical memory address
   calculated using the cache.  If the cache can't be used, either
   because the entry is invalid, points to a different virtual page,
   or the access isn't allowed (only for writes), mapva is called to
   do the mapping and (assuming it doesn't fault) the eap cache entry
   is updated.

   This special cache is invalidated when the STLB is changed, a ring
   change occurs, on process exchange, and on LPSW.  For reads, no
   access checking is needed when the cache is used: the cache entry
   is either valid, meaning that the program has at least read access
   to the page, or the cache entry is invalid (special value of
   0x000000FF, which is not a virtual page address - the right 10 bits
   must be zero for the start of a page), in which case mapva is
   called.

   The special cache can also be used for write references.  Bits 2-4
   of the cache entry vpn are the access bits from mapva, so bit 4
   being 1 means that writes are allowed.  

   IMPORTANT NOTE: bit 4 MUST BE CHECKED before all write references!
*/

#define PBBR 0
#define SBBR 1
#define LBBR 2
#define XBBR 3
#define RPBR 4
#define S0BR 5
#define F0BR 6
#define F1BR 7
#define UNBR 8
#define BRP_SIZE 9

/* NOTE: vpn is a segment number/word offset Prime virtual address
   corresponding to the physical page of memory in memp.  The high-
   order fault bit of vpn is never set.  The ring and extension bits
   are used to store the 3-bit access field from the STLB for the
   current ring.

   All brp entries are invalidated by PCL and PRTN when the ring
   changes, ITLB, PTLB, and LIOT (STLB changes), when OWNERL changes
   (process exchange), and on LPSW.

   Storing the access bits in the vpn allows use of of the brp cache
   for write accesses as well as read accesses.  Only bit 4 (write
   allowed) is used, but since a 3-bit value comes back from mapva,
   it's easier just to put it all in vpn.

   IMPORTANT NOTE: the "get" calls do not store the access field in
   the brp cache entry; the first write to a page must clear the "page
   unmodified" bit in the Primos page maps, and mapva is the only way
   to do this.  So the first "put" has to go through mapva.  An
   alternative would be to store the page map pointer in brp and use
   bit 3 of vpn as a flag for whether the page map has to be modified,
   but this is more work for probably little gain.
 */

typedef struct {
  unsigned short *memp;       /* MEM[] physical page address */
  ea_t vpn;                   /* corresponding virtual page address */
} brp_t;

/* the dispatch table for generic instructions:
   - bits 1-2 are the class (0-3)
   - bits 3-6 are always zero
   - bits 7-16 are the opcode
   the index into the table is bits 1-2_7-16, for a 12-bit index */

void *disp_gen[4096];         /* generic dispatch table */

/* "gv" is a static structure used to hold "hot" global variables.  These
   are pointed to by a dedicated register, so that the usual PowerPC global
   variable instructions aren't needed: these variables can be directly
   referenced by instructions as long as the structure is < 16K bytes. */

typedef struct {

  int memlimit;                 /* user's desired memory limit (-mem) */

  int intvec;                   /* currently raised interrupt (if >= zero) */

  unsigned int instcount;       /* global instruction count */

  brp_t brp[BRP_SIZE];          /* PB, SB, LB, XB, RP, S0, F0, F1, UN */

  unsigned short inhcount;      /* number of instructions to stay inhibited */

  unsigned int instpermsec;     /* instructions executed per millisecond */

  stlbe_t stlb[STLBENTS];       /* the STLB: Segmentation Translation Lookaside Buffer */

  iotlbe_t iotlb[IOTLBENTS];    /* the IOTLB: I/O Translation Lookaside Buffer */

  unsigned int prevpc;          /* backed program counter */

  unsigned short amask;         /* address mask */

  int pmap32bits;               /* true if 32-bit page maps */

  int pmap32mask;               /* mask for 32-bit page maps */

  int csoffset;                 /* concealed stack segment offset */

  unsigned int livereglim;      /* 010 if seg enabled, 040 if disabled */

  int mapvacalls;               /* # of mapva calls */
  int mapvamisses;              /* STLB misses */
  int supercalls;               /* brp supercache hits */
  int supermisses;              /* brp supercache misses */

  void *disp_rmr[128];           /* R-mode memory ref dispatch table */
  void *disp_vmr[128];           /* V-mode memory ref dispatch table */

#ifndef NOTRACE

/* traceflags is the variable used to test tracing of each instruction
   traceuser is the user number to trace, 0 meaning any user
   traceseg is the procedure segment number to trace, 0 meaning any
   savetraceflags hold the real traceflags, while "traceflags" switches
   on and off for each instruction

   TRACEUSER is a macro that is true if the current user is being traced
*/

#define MAXRPQ 8

  FILE *tracefile;              /* trace.log file */
  int traceflags;               /* each bit is a trace flag */
  int savetraceflags;
  unsigned short traceuser;     /* OWNERL to trace */
  short traceseg;               /* RPH segment # to trace */
  short numtraceprocs;          /* # of procedures we're tracing */
  unsigned int traceinstcount;  /* only trace if instcount > this */
  short tracetriggered;         /* Ctrl-T on console toggles tracing */
  short tracerpqx;              /* rpq index to store next RP */
  unsigned int tracerpq[MAXRPQ];/* last 16 locations executed */
#endif

} gv_t;

static gv_t gv;

brp_t *eap;

static  jmp_buf jmpbuf;               /* for longjumps to the fetch loop */
static  jmp_buf bootjmp;               /* for longjumps to the fetch loop */

/* The standard Prime physical memory limit on early machines is 8MB.
   Later machines have higher memory capacities, up to 1024MB, using 
   32-bit page tables. 

   NOTE: 
   - rev 20 is limited to a max of 32MB
   - rev 23.4 is limited to a max of 512MB

   "memlimit" is set with the -mem argument, taking an argument which is
   the desired memory limit in MB.  Setting a memory limit is useful to
   speed up system boots and diagnostics during emulator testing.
*/

#define MAXMB   32     /* must be a power of 2 */
#define MEMSIZE MAXMB/2*1024*1024
#define MEMMASK MEMSIZE-1
#define MEM physmem

static unsigned short *physmem = NULL; /* system's physical memory */

#define get16mem(phyaddr) swap16(MEM[(phyaddr)])
#define put16mem(phyaddr, val) MEM[(phyaddr)] = swap16((val))
#define get32mem(phyaddr) swap32(*(unsigned int *)(MEM+phyaddr))
#define put32mem(phyaddr, val) *(unsigned int *)(MEM+phyaddr) = swap32((val))
#define get64mem(phyaddr) swap64(*(unsigned long long *)(MEM+phyaddr))
#define put64mem(phyaddr, val) *(unsigned long long *)(MEM+phyaddr) = swap64((val))

#define MAKEVA(seg,word) ((((int)(seg))<<16) | (word))

/* returns the incremented value of a virtual address, wrapping to word
   zero at the end of a segment (word portion = 0177777) */

#define INCVA(ea,n) (((ea) & 0xFFFF0000) | ((ea)+(n)) & 0xFFFF)

/* RPADD returns an incremented program counter (does NOT increment
   the counter!)  Technically, it should wrap like INCVA, but for this
   special case, we let it wrap.  Executing the last word in a segment
   should wrap to location zero in the segment, but that causes a trap
   and executes code in the X register - extremely unlikely.

   INCRP bumps program counter.  Again, should only increment the
   16-bit word number, but it's faster (fewer instructions) to
   increment the whole thing.

   DIAG cpu.pcl test 42 does check for segment wraparound, so -DFAST
   will cause this test to fail (but, see hack in pcl which fixes it).
*/

#ifdef FAST
#define RPADD(n) (RP+(n))
#define INCRP RP++
#else
#define RPADD(n) MAKEVA(RPH,RPL+(n))
#define INCRP RPL++
#endif

#define FAULTMASK32 0x80000000       /* fault bit */
#define RINGMASK32  0x60000000       /* ring bits */
#define EXTMASK32   0x10000000       /* E-bit */
#define SEGMASK32   0x0FFF0000       /* segment number */
#define RINGMASK16  0x6000           /* ring bits */
#define EXTMASK16   0x1000           /* E-bit */

#define DTAR32(ea) (((ea)>>26) & 3)
#define SEGNO32(ea) (((ea)>>16) & 07777)
#define SEGNO16(ea) ((ea) & 07777)
#define PAGENO(ea) (((ea)>>10) & 077)


/* Fault/interrupt vectors */

#define FIRSTFAULT    062
#define RESTRICTFAULT 062
#define PROCESSFAULT  063
#define PAGEFAULT     064
#define SVCFAULT      065
#define UIIFAULT      066
#define SEMFAULT      067   /* note duplicate w/parity */
#define PARITYCHECK   067
#define MACHCHECK     070
#define MISSMEMCHECK  071
#define ILLINSTFAULT  072
#define ACCESSFAULT   073
#define ARITHFAULT    074
#define STACKFAULT    075
#define SEGFAULT      076
#define POINTERFAULT  077
#define LASTFAULT     077

//static ea_t tnoua_ea=0, tnou_ea=0, tsrc_ea=0;
static int domemdump;                       /* -memdump arg */

static int tport;                           /* -tport option (incoming terminals) */
static int nport;                           /* -nport option (PNC/Ringnet) */
static in_addr_t bindaddr = INADDR_ANY;     /* -naddr option (PnC/Ringnet) */

/* load map related data, specified with -map */

#define MAXSYMBOLS 15000
#define MAXSYMLEN 11
static int numsyms = 0;
static struct {
  char symname[MAXSYMLEN+1];
  ea_t address;
  char symtype;                /* o=other, c=common, e=ecb, p=proc, l=linkbase */
} mapsym[MAXSYMBOLS];


/* invalidates all entries in the mapva supercache */

void inline invalidate_brp() {
  int i;

  for (i=0; i < BRP_SIZE; i++)
    gv.brp[i].vpn = 0x000000FF;
}

#ifndef NOTRACE
char *brp_name() {
  if (eap == &gv.brp[PBBR])
    return "PBBR";
  if (eap == &gv.brp[SBBR])
    return "SBBR";
  if (eap == &gv.brp[LBBR])
    return "LBBR";
  if (eap == &gv.brp[XBBR])
    return "XBBR";
  if (eap == &gv.brp[RPBR])
    return "RPBR";
  if (eap == &gv.brp[S0BR])
    return "S0BR";
  if (eap == &gv.brp[F0BR])
    return "F0BR";
  if (eap == &gv.brp[F1BR])
    return "F1BR";
  if (eap == &gv.brp[UNBR])
    return "UNBR";
  return "??BR";
}
#endif


/* returns an index to a symbol, based on an address and type
   match; if the address isn't found exactly, the index returned
   will be the address lower than the requested address, or -1
   if the symbol table is empty or the requested address is
   lower than any in the symbol table 

   Symbol types:
     e = address of ecb
     p = address of proc start
     l = address of linkage start - '400 (LB% is loaded w/this)
     x = other symbol (ENT in PMA, FTN COMMON)
*/

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


void addsym(char *sym, unsigned short seg, unsigned short word, char type) {
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
    strncpy(mapsym[ix+1].symname, sym, MAXSYMLEN);
    mapsym[ix+1].address = addr;
    mapsym[ix+1].symtype = type;
    numsyms++;
  }
}


void readloadmap(char *filename, int showerr) {
  FILE *mapf;
  char line[100];
  int lc,ix;
  char sym[100];
  unsigned int segno, wordno, ecbseg, ecbword, pbseg, pbword, lbseg, lbword;
  ea_t lastaddr;

  TRACEA("Reading load map from %s... ", filename);
  if ((mapf = fopen(filename, "r")) == NULL)
    if (showerr) {
      perror("Map file open");
      fatal("Error reading map file");
    } else
      return;

  lc = 0;
  while (fgets(line, sizeof(line), mapf) != NULL) {
    lc++;
    if (strstr(line, "*START"))
      break;
    if (sscanf(line, "%s %o %o %o %o %*o %*o %o %o", sym, &ecbseg, &ecbword, &pbseg, &pbword, &lbseg, &lbword) == 7) {
      addsym(sym, ecbseg, ecbword, 'e');
      addsym(sym, pbseg, pbword, 'p');
      addsym(sym, lbseg, lbword, 'l');
#if 0
      printf("adding proc symbol, line=%s\n", line);
      if (tnou_ea == 0 && strcmp(sym,"TNOU") == 0)
	tnou_ea = MAKEVA(ecbseg, ecbword);
      if (tnoua_ea == 0 && strcmp(sym,"TNOUA") == 0)
	tnoua_ea = MAKEVA(ecbseg, ecbword);
      if (tsrc_ea == 0 && strcmp(sym,"TSRC$$") == 0)
	tsrc_ea = MAKEVA(ecbseg, ecbword);
#endif
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

  if ((SEGNO32(addr) <= 01777 | SEGNO32(addr) >= 06000) &&
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
   0 = PCL (PACC)
   2 = read (RACC)
   3 = write (WACC) = read+write
*/
#define PACC 0
#define RACC 2
#define WACC 3


/* NOTE: this is the 6350 STLB hash function, giving a 9-bit index 0-511.
   The hit rate is over 99% for booting and compiling Primos */

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

static pa_t mapva(ea_t ea, ea_t rp, short intacc, unsigned short *access) {
  short relseg,seg,nsegs,ring;
  unsigned short pte, stlbix, iotlbix;
  stlbe_t *stlbp;
  unsigned int dtar,sdw,staddr,ptaddr,pmaddr,ppa;
  pa_t pa;

#if 0
  /* fault bit set on EA means an address trap, which should be
     handled at a higher level and never make it this far.

     basic and dbasic execute R-mode instructions from the Prime
     register file with the basic statement TRACE ON.

     NOTE: this is commented out because if a stack frame is trashed
     and stackroot gets its fault bit set, PRTN will cause this error,
     halting the emulator.  Without this check, an illegal segno or
     access fault occurs.  Thanks Dennis! */

  if (ea & 0x80000000)
    fatal("mapva: fault bit set on EA");
#endif

  /* map virtual address if segmentation is enabled */

  if (getcrs16(MODALS) & 4) {
#ifndef NOTRACE
    gv.mapvacalls++;
#endif
    seg = SEGNO32(ea);
    stlbix = STLBIX(ea);
    stlbp = gv.stlb+stlbix;
#ifdef DBG
    if (stlbix >= STLBENTS) {
      printf("STLB index %d is out of range for va %o/%o!\n", stlbix, ea>>16, ea&0xffff);
      fatal(NULL);
    }
#endif

    /* if the segments don't match, or the segment is private and the
       process id doesn't match, then the STLB has to be loaded
       first (invalid entries have a segment of 0xFFFF and won't match) */

    if (stlbp->seg != seg || ((seg & 04000) && stlbp->procid != getcrs16(OWNERL))) {
#ifndef NOTRACE
      gv.mapvamisses++;
#endif
      dtar = getcrs32(DTAR0-2*DTAR32(ea));  /* get dtar register */
      nsegs = 1024-(dtar>>22);
      relseg = seg & 0x3FF;     /* segment within segment table */
      TRACE(T_MAP, "   MAP: ea=%o/%o, seg=%o, dtar=%o, nsegs=%d, relseg=%d, page=%d\n", ea>>16, ea&0xFFFF, seg, dtar, nsegs, relseg, PAGENO(ea));
      if (relseg >= nsegs)
	fault(SEGFAULT, 1, ea);   /* fcode = segment too big */
      staddr = (dtar & 0x003F0000) | ((dtar & 0x7FFF)<<1);
      sdw = get32mem(staddr+relseg*2);
      TRACE(T_MAP,"        staddr=%o, sdw=%o\n", staddr, sdw);
      if (sdw & 0x8000)
	fault(SEGFAULT, 2, ea);   /* fcode = sdw fault bit set */
      ptaddr = (((sdw & 0x3F)<<10) | (sdw>>22)) << 6;
      if (gv.pmap32bits) {
	pmaddr = ptaddr + 2*PAGENO(ea);
	pte = get16mem(pmaddr);

	/* this line enables 512MB on the 53xx and later machines that
	   support more than 128MB of physical memory.  The pmap32mask
	   is cpu-dependant: older machines like the 9950 looked at
	   fewer bits, and the older software like Primos rev 19 had
	   other stuff in the higher-order bits that the hardware
	   ignored.  So on older machines, we need to ignore them
	   too, so older software still works. */

	ppa = (get32mem(pmaddr) & gv.pmap32mask) << 10;
      } else {
	pmaddr = ptaddr + PAGENO(ea);
	pte = get16mem(pmaddr);
	ppa = (pte & 0xFFF) << 10;
      }
      TRACE(T_MAP,"        ptaddr=%o, pmaddr=%o, pte=%o\n", ptaddr, pmaddr, pte);
      if (!(pte & 0x8000))
	fault(PAGEFAULT, 0, ea);
      put16mem(pmaddr, get16mem(pmaddr) | 040000);     /* set referenced bit */
      stlbp->access[0] = 7;
      stlbp->access[1] = (sdw >> 12) & 7;
      stlbp->access[STLB_UNMODIFIED_BIT] = 1;
      stlbp->access[3] = (sdw >> 6) & 7;
      stlbp->procid = getcrs16(OWNERL);
      stlbp->seg = seg;
      stlbp->ppa = ppa;
      stlbp->pmaddr = pmaddr;
#ifndef NOTRACE
      TRACE(T_TLB, "stlb[%d] loaded at %o/%o for %o/%o, ppn=%d\n", stlbix, RPH, RPL, seg, ea&0xFFFF, ppa>>10);
      stlbp->load_ic = gv.instcount;
#endif

      /* if this is an I/O segment reference, load the I/O TLB too.
	 This is done because earlier machines didn't have the LIOT
	 instruction to load the IOTLB; instead, they loaded it with
	 regular memory reference instructions like LDA (they also
	 only had a 64-entry IOTLB, so the emulation is not exact) */

      if (seg < 4) {
	iotlbix = (ea & 0x3FFFF) >> 10;
	gv.iotlb[iotlbix].valid = 1;
	gv.iotlb[iotlbix].ppa = ppa;
	TRACE(T_TLB, "iotlb[%d] loaded at %o/%o for %o/%o, ppn=%d\n", stlbix, RPH, RPL, seg, ea&0xFFFF, ppa>>10);
      }
    }
#if 0
    /* seems like ea ring should always be = rp ring, but not true */
    if ((rp & RINGMASK32) != (ea & RINGMASK32))
      warn("rp ring <> ea ring!");
#endif
    ring = ((rp | ea) >> 29) & 3;  /* current ring | ea ring = access ring */
    *access = stlbp->access[ring];
    if (((intacc & *access) != intacc) || (intacc == PACC && ((*access & 3) == 0)))
      fault(ACCESSFAULT, 0, ea);
    if (stlbp->access[STLB_UNMODIFIED_BIT] && intacc == WACC) {
      stlbp->access[STLB_UNMODIFIED_BIT] = 0;
      put16mem(stlbp->pmaddr, get16mem(stlbp->pmaddr) & ~020000);    /* reset unmodified bit in memory */
    }
    pa = stlbp->ppa | (ea & 0x3FF);
    TRACE(T_MAP,"        for ea %o/%o, iacc=%d, stlbix=%d, pa=%o	loaded at #%u\n", ea>>16, ea&0xffff, intacc, stlbix, pa, stlbp->load_ic);
  } else {
    pa = ea & MEMMASK;
  }
  if (pa < gv.memlimit)
    return pa;
#ifdef DBG
  printf(" map: Memory address '%o (%o/%o) is out of range 0-'%o (%o/%o) at #%d!\n", pa, pa>>16, pa & 0xffff, gv.memlimit-1, (gv.memlimit-1)>>16, (gv.memlimit-1) & 0xffff, gv.instcount);
#endif

  /* take a missing memory check
     XXX: not sure if dswstat and dswrma are right, but Primos doesn't
     seem to look at them for this check */

  macheck(071, 0310, 0xd000, pa);
  fatal("Return from macheck");
}


/* for I/O, ea is either an 18-bit physical address (which is just
   returned if not in mapped I/O mode), or a 2-bit segment number and
   16-bit word number for mapped I/O.  A physical address is returned. */

static unsigned int mapio(ea_t ea) {
  int iotlbix;

  ea &= 0x3FFFF;
  if (getcrs16(MODALS) & 020) {           /* mapped I/O mode? */
    iotlbix = (ea >> 10) & 0xFF;     /* TLB range is 0-255 */
    if (gv.iotlb[iotlbix].valid)
      return gv.iotlb[iotlbix].ppa | (ea & 0x3FF);
    else {
      printf("Mapped I/O request to %o/%o, but IOTLB is invalid!\n", ea>>16, ea&0xFFFF);
      fatal(NULL);
    }
  }
  return ea;
}


/* these are I/O versions of get/put that use the IOTLB rather than
   the STLB */

#define get16io(ea) get16mem(mapio((ea)))
#define get32io(ea) get32mem(mapio((ea)))
#define put16io(word,ea) put16mem(mapio((ea)), word)

/* these are shorthand macros for get/put that use the current program
   counter - the typical usage - or Ring 0, the other typical case.
   The other places the ring field is important are PCL (ring may be
   changing) and queue instructions (user process uses current ring,
   while device controllers use Ring 0 (physical queues) */

#define get16r0(ea) (get16r((ea),0))
#define get32r0(ea) (get32r((ea),0))
#define get64(ea) (get64r((ea),RP))
#define get64r0(ea) (get64r((ea),0))
#define put16r0(value, ea) (put16r((value),(ea),0))
#define put32r0(value, ea) (put32r((value),(ea),0))
#define put64(value, ea) (put64r((value),(ea),RP))
#define put64r0(value, ea) (put64r((value),(ea),0))

/* 
   get16 handles 16-bit fetches that CANNOT cause address traps.

   get16t handles 16-bit fetches that MIGHT cause address traps,
   indicated by the sign bit set in EA. 
   Address traps can occur:
   - fetching S/R mode instructions
   - fetching V-mode instructions when RPL < 010 or 040 (seg enabled/not)
   - in any S/R mode memory reference or address calculation
   - in V-mode address calculations (16-bit indirects)
   - in V-mode short instruction execution (LDA# 0 for example)
   Address traps CANNOT occur:
   - in I-mode
   - in V-mode long instruction address calculation or execution

   get16trap handles 16-bit fetches that are KNOWN to be address traps

   get16r handles 16-bit fetches that used a passed-in virtual address;
   (only the ring part is used from this address).

   VERY IMPORTANT: get16r _cannot_ use the supercache!  You don't want to 
   cache Ring 0 accesses to data, then let Ring 3 use the cache!  It could
   be changed to use a special R0 brp cache entry, but probably not much
   gain performance-wise.  Might speed up process exchange...
*/


static inline unsigned short get16(ea_t ea) {
  unsigned short access;

#ifdef DBG
  if (ea & 0x80000000) {
    printf("address trap in get16, ea=%o/%o\n", ea>>16, ea&0xffff);
    fatal(NULL);
  }
#endif

#ifdef FAST
#ifndef NOTRACE
  gv.supercalls++;
#endif
  if ((ea & 0x0FFFFC00) == (eap->vpn & 0x0FFFFFFF)) {
    TRACE(T_MAP, "    get16: cached %o/%o [%s]\n", ea>>16, ea&0xFFFF, brp_name());
    return swap16(eap->memp[ea & 0x3FF]);
  } else {
#ifndef NOTRACE
    gv.supermisses++;
#endif
    eap->memp = MEM + (mapva(ea, RP, RACC, &access) & 0xFFFFFC00);
    eap->vpn = ea & 0x0FFFFC00;
    return swap16(eap->memp[ea & 0x3FF]);
  }
#else
  return get16mem(mapva(ea, RP, RACC, &access));
#endif
}

static unsigned short get16trap(ea_t ea) {

  ea = ea & 0xFFFF;
  if (ea < 7)
    return getcrs16(memtocrs[ea]);
  if (ea == 7)                   /* PC */
    return RPL;
  RESTRICTR(RP);
  if (ea < 020)                 /* CRS */
    return getcrs16(memtocrs[ea]);
  if (ea < 040)                 /* DMX */
    return getar16(REGDMX16 + (((ea & 036) << 1) | (ea & 1)));
  printf("get16trap: live register address %o too big!\n", ea);
  fatal(NULL);
}

static inline unsigned short get16t(ea_t ea) {

  /* sign bit is set for live register access */

  if (*(int *)&ea >= 0)
    return get16(ea);
  return get16trap(ea);
}

static unsigned short get16r(ea_t ea, ea_t rpring) {
  unsigned short access;

#ifdef DBG
  if (ea & 0x80000000) {
    printf("address trap in get16r, ea=%o/%o\n", ea>>16, ea&0xffff);
    fatal(NULL);
  }
#endif

  /* if passed-in ring == RP ring, use get16 and maybe avoid mapva;
     PCL uses "r" versions of get/put because it may be crossing
     rings.  However, if PCL isn't crossing rings (user calls his own
     subroutine for example), then the if below will try to use the
     brp cache. */

  if (((rpring ^ RP) & RINGMASK32) == 0)
    return get16(ea);
  return get16mem(mapva(ea, rpring, RACC, &access));
}

/* get32m always uses the map and isn't inlined */

static unsigned int get32m(ea_t ea) {
  unsigned short access;

#ifdef DBG
  if (ea & 0x80000000) {
    printf("address trap in get32m, ea=%o/%o\n", ea>>16, ea&0xffff);
    fatal(NULL);
  }
#endif

#ifndef NOTRACE
  gv.supermisses++;
#endif
  if ((ea & 01777) <= 01776) {
    eap->memp = MEM + (mapva(ea, RP, RACC, &access) & 0xFFFFFC00);
    eap->vpn = ea & 0x0FFFFC00;
    return swap32(*(unsigned int *)&eap->memp[ea & 0x3FF]);
  }
  return (get16(ea) << 16) | get16(INCVA(ea,1));
}

/* get32 tries to use the cache and is inlined */

static inline unsigned int get32(ea_t ea) {

#ifdef DBG
  if (ea & 0x80000000) {
    printf("address trap in get32, ea=%o/%o\n", ea>>16, ea&0xffff);
    fatal(NULL);
  }
#endif

#ifdef FAST
#ifndef NOTRACE
  gv.supercalls++;
#endif
  if ((ea & 01777) <= 01776)
    if ((ea & 0x0FFFFC00) == (eap->vpn & 0x0FFFFFFF)) {
      TRACE(T_MAP, "    get32: cached %o/%o [%s]\n", ea>>16, ea&0xFFFF, brp_name());
      return swap32(*(unsigned int *)&eap->memp[ea & 0x3FF]);
    }
#endif
  return get32m(ea);
}

static unsigned int get32r(ea_t ea, ea_t rpring) {
  pa_t pa;
  unsigned short access;

#ifdef DBG
  if (ea & 0x80000000) {
    printf("address trap in get32r, ea=%o/%o\n", ea>>16, ea&0xffff);
    fatal(NULL);
  }
#endif

  /* if passed-in ring == RP ring, use get32 and maybe avoid mapva */

  if (((rpring ^ RP) & RINGMASK32) == 0)
    return get32(ea);

  pa = mapva(ea, rpring, RACC, &access);
  if ((pa & 01777) <= 01776)
    return get32mem(pa);
  return (swap16(MEM[pa]) << 16) | get16r(INCVA(ea,1), rpring);
}

static long long get64r(ea_t ea, ea_t rpring) {
  pa_t pa, pa2;
  unsigned short access;
  long long m;

  /* check for live register access */

#ifdef DBG
  if (ea & 0x80000000) {
    printf("address trap in get64r, ea=%o/%o\n", ea>>16, ea&0xffff);
    fatal(NULL);
  }
#endif

  pa = mapva(ea, rpring, RACC, &access);
  if ((ea & 01777) <= 01774)          /* no page wrap */
    return get64mem(pa);
  switch (ea & 3) {                  /* wraps page (maybe seg too) */
  case 1:
    pa2 = mapva(INCVA(ea,3), rpring, RACC, &access);
    m = (((long long) get16mem(pa)) << 48) +
      (((long long) get32mem(pa+1)) << 16) +
      get16mem(pa2);
    break;
  case 2:
    pa2 = mapva(INCVA(ea,2), rpring, RACC, &access);
    m = (((long long) get32mem(pa)) << 32) +
      get32mem(pa2);
    break;
  case 3:
    pa2 = mapva(INCVA(ea,1), rpring, RACC, &access);
    m = (((long long) get16mem(pa)) << 48) +
      (((long long) get32mem(pa2)) << 16) +
      get16mem(pa2+2);
    break;
  default:
    fatal("Page cross error in get64r");
  }
  return m;
}

/* Instruction version of get16 (can be replaced by get16 too...)

   To avoid mapva on every instruction word fetch, two pointers are kept
   in the hot global variable structure:  brp[RPBR].vpn and .memp.  These
   variables track the virtual page address we're currently executing
   (.vpn) and the corresponding pointer in the Prime physical memory
   array mem (.memp).  Whenever a process exchange occurs or the STLB
   is changed, these are invalidated and will be reloaded with the long
   version of iget16: iget16t.

   Bit 1 of the EA is checked below so that if we're executing code from
   registers, iget16t will be called, and eventually, get16t, to handle
   the address trap.  Since bit 1 is never stored in brp.vpn, EA < 0
   will always end up in get16t to load the instruction from a register.

   The effect of this mechanism is that virtual->physical address
   translation and access protection checks are only done once per
   page instead of on every 16-bit fetch from the instruction stream.
*/

#ifdef FAST

unsigned short iget16t(ea_t ea) {
  unsigned short access;

  if (*(int *)&ea >= 0) {
    gv.brp[RPBR].memp = MEM + (mapva(ea, RP, RACC, &access) & 0xFFFFFC00);
    gv.brp[RPBR].vpn = ea & 0x0FFFFC00;
    return swap16(gv.brp[RPBR].memp[ea & 0x3FF]);
  }
  return get16trap(ea);
}

static inline unsigned short iget16(ea_t ea) {

  if ((ea & 0x8FFFFC00) == (gv.brp[RPBR].vpn & 0x0FFFFFFF))
    return swap16(gv.brp[RPBR].memp[ea & 0x3FF]);
  else
    return iget16t(ea);
}

#else
#define iget16(ea) get16((ea))
#define iget16t(ea) get16t((ea))
#endif

static inline void put16(unsigned short value, ea_t ea) {
  unsigned short access;

#ifdef DBG
  if (ea & 0x80000000) {
    printf("address trap in put16, ea=%o/%o\n", ea>>16, ea&0xffff);
    fatal(NULL);
  }
#endif

#ifdef FAST

#ifndef NOTRACE
  gv.supercalls++;
#endif

  /* access bits are stored in bits 2-4 of the cache vpn.
     write access is indicated by bit 4 (MSB = bit 1) */

  if ((ea & 0x0FFFFC00) == (eap->vpn & 0x0FFFFFFF) && (eap->vpn & 0x10000000)) {
    TRACE(T_MAP, "    put16: cached %o/%o [%s]\n", ea>>16, ea&0xFFFF, brp_name());
    eap->memp[ea & 0x3FF] = swap16(value);
  } else {
#ifndef NOTRACE
    gv.supermisses++;
#endif
    eap->memp = MEM + (mapva(ea, RP, WACC, &access) & 0xFFFFFC00);
    eap->vpn = (ea & 0x0FFFFC00) | (access << 28);
    eap->memp[ea & 0x3FF] = swap16(value);
  }
#else
  put16mem(mapva(ea, RP, WACC, &access), value);
#endif
}

static void put16r(unsigned short value, ea_t ea, ea_t rpring) {
  unsigned short access;

#ifdef DBG
  if (ea & 0x80000000) {
    printf("address trap in put16r, ea=%o/%o\n", ea>>16, ea&0xffff);
    fatal(NULL);
  }
#endif

  /* if passed-in ring == RP ring, use put16 and maybe avoid mapva */

  if (((rpring ^ RP) & RINGMASK32) == 0) {
    put16(value, ea);
    return;
  }

  put16mem(mapva(ea, rpring, WACC, &access), value);
}

/* put16trap handles stores that ARE address traps */

static void put16trap(unsigned short value, ea_t ea) {

  ea = ea & 0xFFFF;
  if (ea < 7)
    putcrs16(memtocrs[ea], value);
  else if (ea == 7) {
    RPL = value;
  } else {
    RESTRICTR(RP);
    if (ea <= 017)                      /* CRS */
      putcrs16(memtocrs[ea], value);
    else if (ea <= 037)                 /* DMX */
      putar16(REGDMX16+(((ea & 036) << 1) | (ea & 1)), value);
    else {
      printf(" Live register store address %o too big!\n", ea);
      fatal(NULL);
    }
  }
}

/* put16t handles stores that MIGHT address trap */

static inline void put16t(unsigned short value, ea_t ea) {

  if (*(int *)&ea >= 0)
    put16(value, ea);
  else
    put16trap(value, ea);
}


static void put32(unsigned int value, ea_t ea) {
  unsigned short access;

#ifdef DBG
  if (ea & 0x80000000) {
    printf("address trap in put32, ea=%o/%o\n", ea>>16, ea&0xffff);
    fatal(NULL);
  }
#endif

#ifndef NOTRACE
  gv.supercalls++;
#endif
  if ((ea & 01777) <= 01776) {
    if ((ea & 0x0FFFFC00) == (eap->vpn & 0x0FFFFFFF) && (eap->vpn & 0x10000000)) {
      TRACE(T_MAP, "    put32: cached %o/%o [%s]\n", ea>>16, ea&0xFFFF, brp_name());
      *(unsigned int *)&eap->memp[ea & 0x3FF] = swap32(value);
    } else {
#ifndef NOTRACE
      gv.supermisses++;
#endif
      eap->memp = MEM + (mapva(ea, RP, WACC, &access) & 0xFFFFFC00);
      eap->vpn = (ea & 0x0FFFFC00) | (access << 28);
      *(unsigned int *)&eap->memp[ea & 0x3FF] = swap32(value);
    }
  } else {
    put16(value >> 16, ea);
    put16(value & 0xFFFF, INCVA(ea,1));
  }
}

static void put32r(unsigned int value, ea_t ea, ea_t rpring) {
  pa_t pa;
  unsigned short access;

#ifdef DBG
  if (ea & 0x80000000) {
    printf("address trap in put32r, ea=%o/%o\n", ea>>16, ea&0xffff);
    fatal(NULL);
  }
#endif

  /* if passed-in ring == RP ring, use put32 and maybe avoid mapva */

  if (((rpring ^ RP) & RINGMASK32) == 0) {
    put32(value, ea);
    return;
  }

  pa = mapva(ea, rpring, WACC, &access);
  if ((pa & 01777) <= 01776)
    put32mem(pa, value);
  else {
    put16mem(pa, value>>16);
    put16r(value & 0xFFFF, INCVA(ea,1), rpring);
  }
}

static void put64r(long long value, ea_t ea, ea_t rpring) {
  pa_t pa;
  unsigned short access;

  /* check for live register access */

#ifdef DBG
  if (ea & 0x80000000) {
    printf("address trap in put64r, ea=%o/%o\n", ea>>16, ea&0xffff);
    fatal(NULL);
  }
#endif

  pa = mapva(ea, rpring, WACC, &access);
  if ((pa & 01777) <= 01774)
    put64mem(pa, value);
  else {
    put16mem(pa, value >> 48);
    put16r((value >> 32) & 0xFFFF, INCVA(ea,1), rpring);
    put16r((value >> 16) & 0xFFFF, INCVA(ea,2), rpring);
    put16r(value & 0xFFFF, INCVA(ea,3), rpring);
  }
}

/* flag a hardware sensor check if process exchange is enabled and
   SIGTERM occurs.  The sensorabort flag is checked when a new process
   is dispatched, sets the process abort flags for the hardware sensor
   check if the new process is user 1, causing Primos to flush all
   disk buffers and shutdown gracefully.

   NOTE: it may take up to 1 minute for Primos to see the sensor
   check.  User 1 will usually be at the OK, prompt, waiting on 
   input, and the check will occur when the 1-minute abort occurs.

   NOTE: these are inactive if the emulator is using dedicated
   registers, because signals trash the registers and it's not
   possible to save & restore them.
 */

void sensorcheck () {
  if (getcrs16(MODALS) & 010) {     /* PX enabled */
    printf("\nem: sigterm received, Primos may shutdown soon\n");
    sensorabort = 1;
  }
}

void sigquit() {
  if (sensorabort || ((getcrs16(MODALS) & 010) == 0))
    fatal("Quit");
  else {
    printf("\nem: sigquit received, Primos may shutdown soon\n");
    sensorabort = 1;
  }
}

/* machine check handler, called with check vector locations
   The first arg is used when PX is disabled, the 2nd when PX is enabled. */

void macheck (unsigned short p300vec, unsigned short chkvec, unsigned int dswstat, unsigned int dswrma) {
  unsigned short m;

  /* set check registers in the register file */

  putar32(DSWPB32, RP);
  putar32(DSWSTAT32, dswstat);
  putar32(DSWRMA32, dswrma);

  /* if process exchange is enabled, follow the standard check protocol;
     if PX not enabled, simulate JST p300vec,* to invoke the check.
     Then longjmp back to the fetch loop */

  if (getcrs16(MODALS) & 010)
    fatal(" macheck: machine check (missing memory) with PX enabled\n");
  m = get16(p300vec);
  put16(RPL, m);
  RP = m+1;

  /* similar code in the fault handler */

  grp = RP;
  gcrsl = crsl;
  longjmp(jmpbuf, 1);
  fatal("macheck: returned after longjmp\n");
}


/* queue instructions 

   NOTE: ABQ is typically used in software to add an item to a
   hardware (physical) queue and RTQ is used by DMQ hardware to fetch
   items from the queue.  All of the queue instructions _should_
   support physical queues, but only ABQ and RTQ currently support
   them (they're needed for AMLC boards).  If ICS support is added,
   the other queue instructions will probably need to support physical
   queues. 

   The CPU KEYS are not set here because this would not happen on a
   DMQ request - only when the instruction is executed by software.
*/

static int rtq(ea_t qcbea, unsigned short *qent, ea_t rp) {

  unsigned short qtop, qbot;
  unsigned short qseg, qmask;
  ea_t qentea;

  qtop = get16r(qcbea, rp);
  qbot = get16r(qcbea+1, rp);
  if (qtop == qbot) {
    *qent = 0;
    return 0;               /* queue is empty */
  }
  qseg = get16r(qcbea+2, rp);
  qmask = get16r(qcbea+3, rp);
  qentea = MAKEVA(qseg & 0xfff, qtop);
  if (qseg & 0x8000)        /* virtual queue */
    *qent = swap16(get16r(qentea, rp));
  else {
    RESTRICTR(rp);
    /* XXX: this should probably go through mapio */
    *qent = swap16(get16mem(qentea));
  }
  qtop = (qtop & ~qmask) | ((qtop+1) & qmask);
  put16r(qtop, qcbea, rp);
  return 1;
}


static int abq(ea_t qcbea, unsigned short qent, ea_t rp) {

  unsigned short qtop, qbot, qtemp;
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
    put16r(qent, qentea, rp);
  else {
    RESTRICTR(rp);
    /* XXX: this should probably go through mapio */
    put16mem(qentea, qent);
  }
  put16r(qtemp, qcbea+1, rp);
  return 1;
}


static int rbq(ea_t qcbea, unsigned short *qent, ea_t rp) {

  unsigned short qtop, qbot;
  unsigned short qseg, qmask;
  ea_t qentea;

  qtop = get16(qcbea);
  qbot = get16(qcbea+1);
  if (qtop == qbot) {  /* queue empty */
    *qent = 0;
    return 0;
  }
  qseg = get16(qcbea+2) & 0x7FFF;
  qmask = get16(qcbea+3);
  qbot = (qbot & ~qmask) | ((qbot-1) & qmask);
  qentea = MAKEVA(qseg,qbot);
  *qent = swap16(get16(qentea));
  put16(qbot, qcbea+1);
  return 1;
}

static int atq(ea_t qcbea, unsigned short qent, ea_t rp) {

  unsigned short qtop, qbot, qtemp;
  unsigned short qseg, qmask;
  ea_t qentea;

  qtop = get16(qcbea);
  qbot = get16(qcbea+1);
  qseg = get16(qcbea+2) & 0x7FFF;
  qmask = get16(qcbea+3);
  qtemp = (qtop & ~qmask) | ((qtop-1) & qmask);
  if (qtemp == qbot)   /* queue full */
    return 0;
  qentea = MAKEVA(qseg,qtemp);
  put16(qent,qentea);
  put16(qtemp, qcbea);
  return 1;
}

static unsigned short tstq(ea_t qcbea) {

  unsigned short qtop, qbot, qmask;

  qtop = get16(qcbea);
  qbot = get16(qcbea+1);
  qmask = get16(qcbea+3);
  return (qbot-qtop) & qmask;
}

/* devpoll: number of instructions until device poll */

static int devpoll[64] = {0};

#include "emdev.h"

/* I/O device map table, containing function pointers to handle device I/O */

#if 0

/* this is the "full system" configuration supported by the emulator

   '04 = devasr: system console
   '07 = devpnc: Primenet Node Controller aka PNC (Ringnet)
   '14 = devmt: mag tape controller (4 drives)
   '15 = devamlc: 5th AMLC (16 lines)
   '16 = devamlc: 6th AMLC (16 lines)
   '17 = devamlc: 7th AMLC (16 lines)
   '20 = devcp: clock / VCP / SOC
   '22 = devdisk: 3rd disk controller (8 drives)
   '23 = devdisk: 4th disk controller (8 drives)
   '24 = devdisk: 5th disk controller (8 drives)
   '25 = devdisk: 6th disk controller (8 drives)
   '26 = devdisk: 1st disk controller (8 drives)
   '27 = devdisk: 2nd disk controller (8 drives)
   '32 = devamlc: 8th AMLC (16 lines)
   '35 = devamlc: 4th AMLC (16 lines)
   '45 = devdisk: 7th disk controller (8 drives)
   '46 = devdisk: 8th disk controller (8 drives)
   '52 = devamlc: 3rd AMLC (16 lines)
   '53 = devamlc: 2nd AMLC (16 lines)
   '54 = devamlc: 1st AMLC (16 lines)
*/

static int (*devmap[64])(int, int, int) = {
  /* '0x */ devnone,devnone,devnone,devnone,devasr,devnone,devnone,devpnc,
  /* '1x */ devnone,devnone,devnone,devnone,devmt,devamlc, devamlc, devamlc,
  /* '2x */ devcp,devnone,devdisk,devdisk,devdisk,devdisk,devdisk,devdisk,
  /* '3x */ devnone,devnone,devamlc,devnone,devnone,devamlc,devnone,devnone,
  /* '4x */ devnone,devnone,devnone,devnone,devnone,devdisk,devdisk,devnone,
  /* '5x */ devnone,devnone,devamlc,devamlc,devamlc,devnone,devnone,devnone,
  /* '6x */ devnone,devnone,devnone,devnone,devnone,devnone,devnone,devnone,
  /* '7x */ devnone,devnone,devnone,devnone,devnone,devnone,devnone,devnone};

#else

/* this is a "typical system" controller configuration:

   '04 = devasr: system console
   '07 = devpnc: Primenet Node Controller aka PNC (Ringnet)
   '14 = devmt: mag tape controller (4 drives)
   '20 = devcp: clock / VCP / SOC
   '26 = devdisk: 1st disk controller (8 drives)
   '27 = devdisk: 2nd disk controller (8 drives)
   '54 = 1st amlc (terminal) controller (16 lines)
   '53 = devamlc: 2nd AMLC (16 lines)
*/

static int (*devmap[64])(int, int, int) = {
  /* '0x */ devnone,devnone,devnone,devnone,devasr,devnone,devnone,devpnc,
  /* '1x */ devnone,devnone,devnone,devnone,devmt,devnone, devnone, devnone,
  /* '2x */ devcp,devnone,devnone,devnone,devnone,devnone,devdisk,devdisk,
  /* '3x */ devnone,devnone,devnone,devnone,devnone,devnone,devnone,devnone,
  /* '4x */ devnone,devnone,devnone,devnone,devnone,devnone,devnone,devnone,
  /* '5x */ devnone,devnone,devnone,devamlc,devamlc,devnone,devnone,devnone,
  /* '6x */ devnone,devnone,devnone,devnone,devnone,devnone,devnone,devnone,
  /* '7x */ devnone,devnone,devnone,devnone,devnone,devnone,devnone,devnone};
#endif


static void warn(char *msg) {
  printf("emulator warning:\n  instruction #%u at %o/%o: %o %o keys=%o, modals=%o\n  %s\n", gv.instcount, gv.prevpc >> 16, gv.prevpc & 0xFFFF, get16t(gv.prevpc), get16t(gv.prevpc+1),getcrs16(KEYS), getcrs16(MODALS), msg);
}
    

/* return a string for the keys
   C = C bit set
   D = DBL bit set (S/R)
   L = L bit set
   16S, 32S, 32R, 64R, 64V, 32I = addressing mode
   < = CC "less than" set
   = = CC "equal" set
   F = float exceptions enabled
   I = integer exceptions enabled
   D = decimal exceptions enabled (V/I)
   OOO/DD = visible shift count (S/R)
*/

char *keystring(unsigned short keys) {
  int i;
  char *sp;
  static char s[50];
  static char modes[9][4] = {"16S", "32S", "64R", "32R", "32I", "M5?", "64V", "M7?"};

  sp = s;
  if (keys & 0100000)   /* C-bit */
    *sp++ = 'C';
  if (keys & 040000)    /* DBL */
    *sp++ = 'D';
  if (keys & 020000)    /* L-bit */
    *sp++ = 'L';
  memcpy(sp, modes[(keys>>10) & 7], 3);
  sp += 3;
  if (keys & 01000 == 0)  /* float exception enabled */
    *sp++ = 'F';
  if (keys & 0400)        /* int exception enabled */
    *sp++ = 'I';
  if (keys & 010000) {    /* V/I mode */
    if (keys & 0200)      /* CC LT bit */
      *sp++ = '<';
    if (keys & 0100)      /* CC EQ bit */
      *sp++ = '=';
    if (keys & 040)       /* dec exception enabled */
      *sp++ = 'D';
  } else {                /* R-mode visible shift count */
    i = keys & 0377;
    i = sprintf(sp, "%03o/%d ", i, i);
    sp += i;
  }
  *sp = 0;
  return s;
}

/* return a string for the modals
   I = interrupts inhibited, bit 1 = 0
   E = interrupts enabled, bit 1 = 1
   
   V = vectored interrupt mode, bit 2 = 1

   0-7 = CRS, bits 9-11

   M = mapped I/O, bit 12 = 1

   P = process exchange enabled, bit 13 = 1

   S = segmentation enabled, bit 14 = 1

   Machine check mode, bits 15-16:
   N = no reporting (00)
   C = report uncorreced mem parity errors
   R = report unrecovered mem parity errors
   A = report all errors
*/

char *modstring(unsigned short m) {
  char *sp;
  static char s[50];
  static char modes[4] = {'N', 'C', 'R', 'A'};

  sp = s;
  if (m & 0100000)  /* interrupts enabled */
    *sp++ = 'E';
  else
    *sp++ = 'I';
  if (m & 040000)   /* vectored interrupts */
    *sp++ = 'V';
  *sp++ = ((m >> 5) & 7) + '0';  /* CRS */
  if (m & 020)      /* mapped I/O */
    *sp++ = 'M';
  if (m & 010)      /* process exchange */
    *sp++ = 'P';
  if (m & 4)        /* segmentation */
    *sp++ = 'S';
  *sp++ = modes[m & 3];
  *sp = 0;
  return s;
}

static void fatal(char *msg) {
  static int fatal_called = 0;
  ea_t pcbp, csea;
  unsigned short first,next,last,this;
  unsigned short cs[6];
  int i;

  if (fatal_called) {
    printf("Nested call to fatal()\n");
    if (msg)
      printf("%s\n", msg);
    exit(1);
  }

  fatal_called = 1;
  printf("\n");

  /* XXX: need to log this stuff... */

  printf("Fatal error");
  if (msg)
    printf(": %s", msg);
  printf("\n");
  
  if (physmem != NULL) {
    printf("instruction #%u at %o/%o %s ^%06o^\nA='%o/%d  B='%o/%d  L='%o/%d  X='%o/%d K=%o [%s]\nowner=%o %s, modals=%o [%s]\n", gv.instcount, gv.prevpc >> 16, gv.prevpc & 0xFFFF, searchloadmap(gv.prevpc,' '), lights, getcrs16(A), getcrs16s(A), getcrs16(B), getcrs16s(B), getcrs32(A), getcrs32s(A), getcrs16(X), getcrs16s(X), getcrs16(KEYS), keystring(getcrs16(KEYS)), getcrs16(OWNERL), searchloadmap(getcrs32(OWNER),' '), getcrs16(MODALS), modstring(getcrs16(MODALS)));

    /* dump concealed stack entries */

    if (getcrs16(MODALS) & 010) {   /* process exchange is enabled */
      pcbp = getcrs32ea(OWNER);     /* my pcb pointer */
      first = get16r0(pcbp+PCBCSFIRST);
      next = get16r0(pcbp+PCBCSNEXT);
      last = get16r0(pcbp+PCBCSLAST);
      while (next != first) {
	this = next-6;
	csea = MAKEVA(getcrs16(OWNERH)+gv.csoffset, this);
	for (i=0; i<6; i++)
	  cs[i] = get16r0(csea+i);
	printf("Fault: RP=%o/%o, keys=%06o, fcode=%o, faddr=%o/%o\n", cs[0], cs[1], cs[2], cs[3], cs[4], cs[5]);
	next = this;
      }
    }

#ifndef NOTRACE
    printf("RP queue:");
    i = gv.tracerpqx;
    while(1) {
      printf(" %o/%o", gv.tracerpq[i]>>16, gv.tracerpq[i]&0xFFFF);
      i = (i+1) & (MAXRPQ-1);
      if (i == gv.tracerpqx)
	break;
    }
    printf("\n");
    printf("STLB calls: %d  misses: %d  hitrate: %5.2f%%\n", gv.mapvacalls, gv.mapvamisses, (double)(gv.mapvacalls-gv.mapvamisses)/gv.mapvacalls*100.0);
    printf("Supercache calls: %d  misses: %d  hitrate: %5.2f%%\n", gv.supercalls, gv.supermisses, (double)(gv.supercalls-gv.supermisses)/gv.supercalls*100.0);
#endif

    /* should do a register dump, RL dump, PCB dump, etc. here... */

    /* call all devices with a request to terminate */

    for (i=0; i<64; i++)
      devmap[i](-2, 0, i);
  }
  
#ifndef NOTRACE
  fclose(gv.tracefile);
#endif
  if (lseek(2, 0, SEEK_END) > 0)
    printf("Check error.log for more information\n");
  printf("\n");
  exit(1);
}



/* set new processor keys */

static void newkeys (unsigned short new) {

  switch ((new & 016000) >> 10) {
  case 0:                     /* 16S */
    TRACE(T_MODE, "Entering 16S mode, keys=%o\n", new);
    gv.amask = 037777;
    break;
  case 1:                     /* 32S */
    TRACE(T_MODE, "Entering 32S mode, keys=%o\n", new);
    gv.amask = 077777;
    break;
  case 2:                     /* 64R */
    TRACE(T_MODE, "Entering 64R mode, keys=%o\n", new);
    gv.amask = 0177777;
    break;
  case 3:                     /* 32R */
    TRACE(T_MODE, "Entering 32R mode, keys=%o\n", new);
    gv.amask = 077777;
    break;
  case 4:                     /* 32I */
    TRACE(T_MODE, "Entering 32I mode, keys=%o\n", new);
    gv.amask = 0177777;
    break;
  case 6:                     /* 64V */
    TRACE(T_MODE, "Entering 64V mode, keys=%o\n", new);
    gv.amask = 0177777;
    break;
  default:                    /* invalid mode */
    warn("Invalid CPU mode");
    fault(ILLINSTFAULT, RPL, RP);
  }
  putcrs16(KEYS, new);
}

static void fault(unsigned short fvec, unsigned short fcode, ea_t faddr) {
  static unsigned char faultname[LASTFAULT-FIRSTFAULT+2][4] = 
    {"RXM", "PRC", "PAG", "SVC", "UII", "SEM", "MCK", "MM", "ILL", "ACC", "ARI", "STK", "SEG", "PTR", "-?-"};
  unsigned char *faultnamep;
  ea_t pcbp, pxfvec, csea, ea;
  unsigned short first, next, last;
  unsigned short m;
  unsigned short ring;
  int i,namlen;
  unsigned short name[128];
  ea_t faultrp;

  /* NOTE: Prime Hackers Guide says RP is backed for SVC fault, other
     docs say it is current */

  if (fvec == PROCESSFAULT || fvec == SVCFAULT || fvec == ARITHFAULT)
    faultrp = RP;
  else
    faultrp = gv.prevpc;

  /* NOTE: RP may have the fault bit set if executing from registers.
     This is an emulator-ism to let us know we're executing registers
     without having to do tests on every instruction fetch.  But this
     can confuse Primos (dmstk for example), so don't ever store it
     this way. */

  faultrp &= 0x7FFFFFFF;

  /* save RP, keys in regfile, fcode and faddr in crs */

  putar32(PSWPB32, faultrp);
  regs.sym.pswkeys = crs[KEYS];   /* Prime->Prime: no swap! */
  putcrs16(FCODE, fcode);
  putcrs32(FADDR, faddr);
  
  if (FIRSTFAULT <= fvec && fvec <= LASTFAULT)
    faultnamep = faultname[fvec-FIRSTFAULT];
  else
    faultnamep = faultname[LASTFAULT-FIRSTFAULT+1];
  TRACE(T_FAULT, "#%u: fault '%o (%s), fcode=%o, faddr=%o/%o, faultrp=%o/%o\n", gv.instcount, fvec, faultnamep, fcode, faddr>>16, faddr&0xFFFF, faultrp>>16, faultrp&0xFFFF);

  if (getcrs16(MODALS) & 010) {   /* process exchange is enabled */
    ring = (RPH>>13) & 3;                     /* save current ring */
    pcbp = getcrs32ea(OWNER);
    if (fvec == PROCESSFAULT || fvec == SEMFAULT || fvec == ACCESSFAULT || fvec == STACKFAULT || fvec == SEGFAULT)
      pxfvec = get32r0(pcbp+PCBFVR0);         /* use R0 handler */
    else if (fvec == PAGEFAULT)
      pxfvec = get32r0(pcbp+PCBFVPF);         /* use page fault handler, also R0 */
    else {
      pxfvec = get32r0(pcbp+PCBFVEC+2*ring);  /* use current ring handler */
      pxfvec |= ((int)ring) << 29;            /* weaken */
    }

    /* push a concealed stack entry */

    first = get16r0(pcbp+PCBCSFIRST);
    next = get16r0(pcbp+PCBCSNEXT);
    last = get16r0(pcbp+PCBCSLAST);
    TRACE(T_FAULT, "fault: PX enabled, pcbp=%o/%o, cs first=%o, next=%o, last=%o\n", pcbp>>16, pcbp&0xFFFF, first, next, last);
    if (next > last) {
#ifdef DBG
      /* this is better for debugging */
      TRACE(T_FAULT, "fault: Concealed stack wraparound to first");
      fatal("fault: Concealed stack wraparound to first");
#else
      /* this is the normal mode of operation & necessary for DIAG */
      TRACE(T_FAULT, "fault: Concealed stack wraparound to first");
      next = first;
#endif
    }
    csea = MAKEVA(getcrs16(OWNERH)+gv.csoffset, next);
    put32r0(faultrp, csea);
    put16r0(getcrs16(KEYS), csea+2);
    put16r0(fcode, csea+3);
    put32r0(faddr, csea+4);
    put16r0(next+6, pcbp+PCBCSNEXT);
    TRACE(T_FAULT, "fault: updated cs next=%o\n", get16r0(pcbp+PCBCSNEXT));

    /* update RP to jump to the fault vector in the fault table */

    RP = pxfvec + (fvec-062)*4;
    newkeys(014000);      /* V-mode */
    gv.inhcount = 1;         /* supposed to do this only for Ring 0, but shouldn't hurt */

#if 0
    /* this was useful, but also can cause page faults - careful! */
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
    //TRACE(T_FAULT, "fault '%o occurred at %o/%o, instruction=%o, modals=%o\n", fvec, faultrp>>16, faultrp&0xffff, get16t(faultrp), getcrs16(MODALS));
    /* XXX: need to check for standard/vectored interrupt mode here... */
    m = get16(fvec);
    if (m != 0) {
      TRACE(T_FLOW, " fault JST* '%o [%o]\n", fvec, m);
      put16(faultrp & 0xFFFF, m);
      RP = m;                /* NOTE: changes RP(segno) to segment 0 */
      INCRP;
    } else {
      printf("#%u: fault '%o, fcode=%o, faddr=%o/%o, faultrp=%o/%o\n", gv.instcount, fvec, fcode, faddr>>16, faddr&0xFFFF, faultrp>>16, faultrp&0xFFFF);
      fatal("Fault vector is zero, process exchange is disabled.");
    }
  }

  /* on longjmp, register globals are reset (PPC); save them before jumping
     See also macheck */

  grp = RP;
  gcrsl = crsl;
  longjmp(jmpbuf, 1);
  fatal("fault: returned after longjmp\n");
}


/* 16S Addressing Mode 

   NOTE: when fetching indirect words via address traps, ie, the word
   is in a register, the segment and ring bits are unimportant.  But
   when returning the final EA, seg and ring are important: if a JMP
   instruction is executing, and the target of the jump is a register,
   the seg and ring fields of RP must be maintained.  Otherwise, a
   restricted instruction like HLT executed from a register would
   actually halt the machine.  Yes, this actually happened to me! :-)
*/

static ea_t ea16s (unsigned short inst) {
  
  unsigned short rpl, amask, i, x;
  int indlevel;
  ea_t ea, va;

  i = inst & 0100000;                            /* indirect */
  x = ((inst & 036000) != 032000) ? (inst & 040000) : 0;
  amask = 037777;
  rpl = gv.prevpc;                             /* S-mode uses backed PC! */
  if (inst & 001000)
    ea = (rpl & 037000) | (inst & 0777);         /* current sector */
  else
    ea = (inst & 0777);                          /* sector 0 */
  indlevel = 0;
  while (1) {
    if (x)                                       /* indexed */
      ea += getcrs16(X);
    if (!i)                                      /* not indirect */
      break;
    if (indlevel++ == INDLEVELS)
      fault(RESTRICTFAULT, 0, 0);
    ea &= amask;
    if (ea < gv.livereglim)                    /* flag live register ea */
      ea |= 0x80000000;
    ea = get16t(ea);                             /* get indirect word */
    i = ea & 0100000;
    x = ea & 040000;
  }
  ea &= amask;
  va = MAKEVA(RPH, ea);
  if (ea < gv.livereglim)                      /* flag live register ea */
    va |= 0x80000000;
  return va;
}


/* 32S Addressing Mode */

static ea_t ea32s (unsigned short inst) {
  
  unsigned short rpl, amask, i, x;
  int indlevel;
  ea_t ea, va;

  i = inst & 0100000;                            /* indirect */
  x = ((inst & 036000) != 032000) ? (inst & 040000) : 0;
  TRACE(T_EAS, " ea32s: i=%o, x=%o, s=%o\n", i!= 0, x!=0, (inst & 01000)!=0);
  amask = 077777;
  rpl = gv.prevpc;
  if (inst & 001000) {
    ea = (rpl & 077000) | (inst & 0777);         /* current sector */
    TRACE(T_EAS, " current sector, P=%o, ea=%o\n", rpl, ea);
  } else {
    ea = (inst & 0777);                          /* sector 0 */
    TRACE(T_EAS, " sector 0, ea=%o\n", ea);
    if (ea < 0100 && x) {                        /* preindex by X */
      ea += getcrs16(X);
      TRACE(T_EAS, " preindex, ea=%o\n", ea);
      x = 0;
    }
  }
  for (indlevel=0; i; indlevel++) {
    if (indlevel == INDLEVELS)
      fault(RESTRICTFAULT, 0, 0);
    ea &= amask;
    if (ea < gv.livereglim)                    /* flag live register ea */
      ea |= 0x80000000;
    ea = get16t(ea);                             /* go indirect */
    TRACE(T_EAS, " indirect, ea=%o\n", ea);
    i = ea & 0100000;
  }
  if (x) {                                       /* postindex */
    ea += getcrs16(X);
    TRACE(T_EAS, " postindex, ea=%o\n", ea);
  }
  ea &= amask;
  va = MAKEVA(RPH, ea);
  if (ea < gv.livereglim)                      /* flag live register ea */
    va |= 0x80000000;
  return va;
}

/* NOTE: the difference between 32R and 64R, besides the extra address
   bit, is that 32R indirect words have an indirect bit for multi-level
   indirects */

static inline ea_t ea32r64r (ea_t earp, unsigned short inst) {

  unsigned short ea, m, rph, rpl, amask, class, i, x;
  int indlevel;
  ea_t va;

  x = ((inst & 036000) != 032000) ? (inst & 040000) : 0;
  i = inst & 0100000;                            /* indirect */
  amask = 0177777;
  if ((getcrs16(KEYS) & 016000) == 06000)             /* 32R mode? */
    amask = 077777;
  rpl = earp;
  rph = (earp >> 16) & 0x7FFF;     /* clear fault (live register) bit from RP */
  TRACE(T_EAR, " ea32r64r: i=%o, x=%o, amask=%o\n", i!= 0, x!=0, amask);
  if (inst & 001000)                             /* sector bit 7 set? */
    if ((inst & 0760) != 0400) {                 /* PC relative? */
      ea = rpl + (((short) (inst << 7)) >> 7);   /* yes, sign extend D */
      TRACE(T_EAR, " PC relative, P=%o, new ea=%o\n", rpl, ea);
      eap = &gv.brp[RPBR];
    }
    else 
      goto special;                              /* special cases */
  else {
    eap = &gv.brp[S0BR];
    ea = (inst & 0777);                          /* sector 0 */
    TRACE(T_EAR, " Sector 0, new ea=%o\n", ea);
    if (ea < 0100 && x) {                        /* preindex by X */
      TRACE(T_EAR, " Preindex, ea=%o, X='%o/%d\n", ea, getcrs16(X), getcrs16s(X));
      ea += getcrs16(X);
      TRACE(T_EAR, " Preindex, new ea=%o\n", ea);
      x = 0;
    }
  }
  ea &= amask;
  for (indlevel=0; i; indlevel++) {
    if (indlevel == INDLEVELS)
      fault(RESTRICTFAULT, 0, 0);
    if (ea >= gv.livereglim)
      m = get16(MAKEVA(rph,ea));
    else
      m = get16trap(ea);
    TRACE(T_EAR, " Indirect, old ea=%o, [ea]=%o\n", ea, m);
    if ((getcrs16(KEYS) & 016000) == 04000)           /* 64R mode? */
      i = 0;                                     /* yes, single indirect */
    else
      i = m & 0100000;                           /* 32R - multiple indirects */
    ea = m & amask;                              /* go indirect */
    TRACE(T_EAR, " Indirect, new i=%d, new ea=%o\n", i!=0, ea);
    eap = &gv.brp[UNBR];
  }
  if (x) {
    eap = &gv.brp[XBBR];
    TRACE(T_EAR, " Postindex, old ea=%o, X='%o/%d\n", ea, getcrs16(X), getcrs16s(X));
    ea += getcrs16(X);
    TRACE(T_EAR, " Postindex, new ea=%o\n", ea);
  }
  ea &= amask;
  va = MAKEVA(rph, ea);
  if (ea < gv.livereglim)                      /* flag live register ea */
    va |= 0x80000000;
  return va;

special:
  class = inst & 3;                              /* class bits = 15 & 16 */

#ifndef NOTRACE
  int opcode;

  opcode = ((inst & 036000) != 032000) ? ((inst & 036000) >> 4) : ((inst & 076000) >> 4);
  opcode |= ((inst >> 2) & 3);         /* opcode extension */
  TRACE(T_EAR, " special, new opcode=%#05o, class=%d\n", opcode, class);
#endif

  if (class < 2) {                               /* class 0/1 */
    ea = iget16(RP);                             /* get A from next word */
    INCRP;
    TRACE(T_EAR, " Class %d, new ea=%o\n", class, ea);
    if (class == 1)
      ea += getcrs16(S);
    if (x) {
      TRACE(T_EAR, " Preindex, ea=%o, X='%o/%d\n", ea, getcrs16(X), getcrs16s(X));
      ea += getcrs16(X);
      TRACE(T_EAR, " Preindex, new ea=%o\n", ea);
    }
    for (indlevel=0; i; indlevel++) {
      if (indlevel == INDLEVELS)
	fault(RESTRICTFAULT, 0, 0);
      if (ea >= gv.livereglim)
	m = get16(MAKEVA(rph,ea));
      else
	m = get16trap(ea);
      TRACE(T_EAR, " Indirect, old ea=%o, [ea]=%o\n", ea, m);
      if ((getcrs16(KEYS) & 016000) == 04000)
	i = 0;
      else
	i = m & 0100000;
      ea = m & amask;
      TRACE(T_EAR, " Indirect, new i=%d, new ea=%o\n", i!=0, ea);
      eap = &gv.brp[UNBR];
    }

  } else if (i && x) {                           /* class 2/3, ix=11 */
    TRACE(T_EAR, " class 2/3, ix=11\n");
    ea = iget16(RP);                             /* get A from next word */
    INCRP;
    TRACE(T_EAR, " ea=%o\n", ea);
    if (class == 3) {
      ea += (short) getcrs16(S);
      eap = &gv.brp[SBBR];
    }
    for (indlevel=0; i; indlevel++) {
      if (indlevel == INDLEVELS)
	fault(RESTRICTFAULT, 0, 0);
      if (ea >= gv.livereglim)
	m = get16(MAKEVA(rph,ea));
      else
	m = get16trap(ea);
      TRACE(T_EAR, " Indirect, ea=%o, [ea]=%o\n", ea, m);
      if ((getcrs16(KEYS) & 016000) == 04000)
	i = 0;
      else
	i = m & 0100000;
      ea = m & amask;
      TRACE(T_EAR, " Indirect, new i=%d, new ea=%o\n", i!=0, ea);
    }
    TRACE(T_EAR, " Postindex, old ea=%o, X='%o/%d\n", ea, getcrs16(X), getcrs16s(X));
    ea += (short) getcrs16(X);
    TRACE(T_EAR, " Postindex, new ea=%o\n", ea);
    eap = &gv.brp[XBBR];

  } else {                                       /* class 2/3, ix != 11 */
    eap = &gv.brp[SBBR];
    if (class == 2) {
      ea = getcrs16(S);
      putcrs16(S, getcrs16(S) + 1);
    } else {
      putcrs16(S, getcrs16(S) - 1);
      ea = getcrs16(S);
    }
    TRACE(T_EAR, " Class 2/3, new ea=%o, new S=%o\n", ea, getcrs16(S));
    if (x) {
      if (ea >= gv.livereglim)
	m = get16(MAKEVA(rph,ea));
      else
	m = get16trap(ea);
      if ((getcrs16(KEYS) & 016000) == 06000)
	i = m & 0100000;
      ea = m & amask;
    }
    for (indlevel=0; i; indlevel++) {
      if (indlevel == INDLEVELS)
	fault(RESTRICTFAULT, 0, 0);
      if (ea >= gv.livereglim)
	m = get16(MAKEVA(rph,ea));
      else
	m = get16trap(ea);
      if ((getcrs16(KEYS) & 016000) == 04000)
	i = 0;
      else
	i = m & 0100000;
      ea = m & amask;
      eap = &gv.brp[UNBR];
    }
    if (x) {
      ea += getcrs16(X);
      eap = &gv.brp[XBBR];
    }
  }
  ea &= amask;
  va = MAKEVA(rph, ea);
  if (ea < gv.livereglim)      /* flag live register ea */
    va |= 0x80000000;
  return va;
}

#include "ea64v.h"
#include "ea32i.h"

static ea_t apea(unsigned short *bitarg) {
  unsigned short ibr, ea_s, ea_w, bit, br, a;
  unsigned int utempl;
  ea_t ea, ip;

  eap = &gv.brp[RPBR];
  utempl = get32(RP);
  INCRP; INCRP;
  ibr = utempl >> 16;
  a = utempl & 0xffff;
  bit = (ibr >> 12) & 0xF;
  br = (ibr >> 8) & 3;
  TRACE(T_EAAP, " AP ibr=%o, br=%d, i=%d, bit=%d, a=%o\n", ibr, br, (ibr & 004000) != 0, bit, a);

  /* XXX: should ea ring be weakened with RP ring? */

  ea_s = getcrs16(PBH + 2*br);
  ea_w = getcrs16(PBL + 2*br) + a;
  ea = MAKEVA(ea_s, ea_w);
  TRACE(T_EAAP, " AP ea = %o/%o  %s\n", ea_s, ea_w, searchloadmap(ea,' '));
  if (ibr & 004000) {
    if (ea & 0x80000000)
      fault(POINTERFAULT, ea>>16, 0);    /* XXX: faddr=0? */
    eap = &gv.brp[UNBR];
    ip = get32(ea);
    if (ip & EXTMASK32)
      bit = get16(INCVA(ea,2)) >> 12;
    else
      bit = 0;
    ea = ip;
    TRACE(T_EAAP, " After indirect, AP ea = %o/%o, bit=%d  %s\n", ea>>16, ea & 0xFFFF, bit, searchloadmap(ea,' '));
  }
  if (bitarg != NULL)
    *bitarg = bit;
  TRACE(T_FLOW|T_EAAP," APEA: %o/%o-%d %s\n", ea>>16, ea&0xFFFF, bit, searchloadmap(ea,' '));
  return ea;
}

/* exception handler types:

  'i' = integer exception
  'd' = decimal exception
  'f' = floating point exception

  Depending on the keys settings, take the appropriate fault.
  Always sets the C-bit.
*/

#define FC_SFP_OFLOW 0400    /* 0x100 */
#define FC_SFP_ZDIV  0401    /* 0x101 */
#define FC_SFP_STORE 0402    /* 0x102 */
#define FC_INT_CONV  0403    /* 0x103 */
#define FC_DFP_OFLOW 01000   /* 0x200 */
#define FC_DFP_ZDIV  01001   /* 0x201 */
#define FC_INT_OFLOW 01400   /* 0x300 */
#define FC_INT_ZDIV  01401   /* 0x301 */
#define FC_FUNC_EXC  03000   /* 0x600 */
#define FC_DEC_OFLOW 03400   /* 0x700 */
#define FC_DEC_ZDIV  03401   /* 0x701 */
#define FC_DEC_CONV  03402   /* 0x702 */
#define FC_QFP_OFLOW 04000   /* 0x800 */
#define FC_QFP_ZDIV  04001   /* 0x801 */
#define FC_QFP_QINQ  04003   /* 0x803 */


static inline void mathexception(unsigned char extype, unsigned short fcode, ea_t faddr)
{
  putcrs16(KEYS, getcrs16(KEYS) | 0x8000);
  switch (extype) {
  case 'i':
    if (getcrs16(KEYS) & 0400) 
      fault(ARITHFAULT, fcode, faddr);
    break;
  case 'd':
    if (getcrs16(KEYS) & 040)
      fault(ARITHFAULT, fcode, faddr);
    break;
  case 'f':
    if (!(getcrs16(KEYS) & 01000))
      fault(ARITHFAULT, fcode, faddr);
    break;
  default:
    printf(" Unrecognized exception type '%c'\n", extype);
    fatal(NULL);
  }
}

#include "fp.h"

static void dumpregs() {
  int rs, i;
  unsigned int val;
  unsigned short v1, v2;

  TRACEA("\nREGISTER DUMP:\n");
  for (rs=0; rs<REGSETS; rs++) {
    TRACEA("Register set %d:\n", rs);
    for (i=0; i<32; i++) {
      val = swap32(regs.rs[rs][i]);
      v1 = val >> 16;
      v2 = val & 0xFFFF;
      TRACEA("'%02o: %06o %06o  %04x %04x\n", i, v1, v2, v1, v2);
    }
  }
}

/* XXX: doesn't handle both page map formats */

static void dumpsegs() {
  short seg,nsegs,i,page,segno;
  unsigned short pte,xxx;
  unsigned int dtar,staddr,sdw,ptaddr,pmaddr;

  TRACEA("\nSEGMENT TABLE DUMP:\n");
  for (i=0; i<4; i++) {
    dtar = getcrs32(DTAR0-2*i);  /* get dtar register */
    nsegs = 1024-(dtar>>22);
    staddr = (dtar & 0x003F0000) | ((dtar & 0xFFFF)<<1);
    TRACEA("DTAR %d: register=%o, size=%d, seg table addr=%o\n", i, dtar, nsegs, staddr);
    for (seg=0; seg<nsegs; seg++) {
      segno = (i<<10)+seg;
      sdw = get32mem(staddr);
      ptaddr = ((sdw & 0x3F)<<10) | (sdw>>22);
      TRACEA("Segment '%o: F=%d, R1:%o R3:%o PT = %o\n", segno, (sdw>>15)&1, (sdw>>12)&7, (sdw>>6)&7, ptaddr);
      xxx = (sdw>>16)&0x3F;
      if (xxx != 0) TRACEA("WARNING: X=%o\n", xxx);
      if (ptaddr != 0)
	for (page=0; page<64; page++) {
	  pmaddr = (ptaddr<<6) + page;
	  pte = get16mem(pmaddr);
	  TRACEA(" Seg %o page %d: pmaddr=%o, V=%d R=%d U=%d S=%d PPA=%o\n", segno, page, pmaddr, pte>>15, (pte>>14)&1, (pte>>13)&1, (pte>>12)&1, pte&0xFFF);
	}
      staddr += 2;
    }
  }
}


/* NOTE: this needs get16r0 */

static unsigned short dumppcb(unsigned short pcb) {
  short i;
  unsigned short nextpcb;
  ea_t ea;

  ea = MAKEVA(getcrs16(OWNERH),pcb);
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
   the stackroot segment.

   According to the ISG, STEX can create a multi-segment extension,
   but the emulator doesn't support this and will fault.

   NOTE: see related code in PCL and PRTN.
*/

static ea_t stex(unsigned int framesize) {
  short stackrootseg, stackseg;
  ea_t stackfp, fpva;

  if (framesize > 0xFFFF)
    fault(RESTRICTFAULT, 0, 0);

  framesize = (framesize + 1) & 0xFFFE;    /* round up to even */
  eap = &gv.brp[SBBR];
  stackrootseg = get16((getcrs32(SB))+1);
  stackseg = stackrootseg;
  stackfp = get32(MAKEVA(stackseg,0));

  /* see if there is room for this frame */

  if ((stackfp & 0xFFFF) + framesize > 0xFFFF) {

    /* no, follow the extension pointer in the last stack segment to
       the next stack segment */

    stackseg = SEGNO32(stackfp);
    fpva = MAKEVA(stackseg, 2);
    stackfp = get32(fpva);
    TRACE(T_PCL, " no room for frame, extension pointer at %o/%o is %o/%o\n", fpva>>16, fpva&0xffff, stackfp>>16, stackfp&0xFFFF);
    if (stackfp == 0 || (stackfp & 0xFFFF) + framesize > 0xFFFF)
      fault(STACKFAULT, 0, MAKEVA(stackseg,0) | (RP & RINGMASK32));
  }

  /* update the stack free pointer */

  put32((stackfp+framesize) & ~RINGMASK32, MAKEVA(stackrootseg, 0));
  TRACE(T_FLOW, " stack extension is at %o/%o\n", stackfp>>16, stackfp&0xffff);
  return stackfp;
}

/* for PRTN, load values into temps first so that if any faults occur,
   PRTN can be restarted.  After all the temps are loaded, the stack
   free pointer can be updated since no further faults can occur.

   If changing rings, make sure to invalidate the brp cache. */

static inline void prtn() {
  unsigned short stackroot;
  ea_t newrp,newsb,newlb;
  unsigned short keys;

  eap = &gv.brp[SBBR];
  stackroot = get16(getcrs32(SB)+1);
  newrp = get32(getcrs32(SB)+2);
  newsb = get32(getcrs32(SB)+4);
  newlb = get32(getcrs32(SB)+6);
  keys = get16(getcrs32(SB)+8);

  /* update the stack free pointer */

  put32(getcrs32(SB), MAKEVA(stackroot, 0));
  if ((newrp ^ RP) & RINGMASK32)
    invalidate_brp();
  RP = newrp | (RP & RINGMASK32);
  putcrs32(SB, newsb);
  putcrs32(LB, newlb);
  newkeys(keys & 0177770);
  //TRACE(T_FLOW, " Finished PRTN, RP=%o/%o\n", RPH, RPL);
}



/* NOTE: the brsave array contains copies of the PB, SB, and LB base
   registers at the time of the PCL, to compute argument effective
   addresses.  If the PCL faults during argument transfer, the ARGT
   instruction will reload this array from the new stack frame
   header. */

static inline ea_t pclea(unsigned short brsave[6], ea_t rp, unsigned short *bitarg, short *store, short *lastarg) {
  unsigned short ibr, br, ea_s, ea_w, bit, a;
  unsigned int utempl;
  ea_t ea, iwea;

  iwea = 0;
  *store = 0;
  eap = &gv.brp[RPBR];
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
    ea_s = getcrs16(XBH) | (RPH & RINGMASK16);
    ea_w = getcrs16(XBL);
    ea_w += a;
    if (getcrs16(XBH) & EXTMASK16) {
      bit += getcrs16(X);
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
      fault(POINTERFAULT, ea>>16, 0);    /* XXX: faddr=0? */
    iwea = ea;
    eap = &gv.brp[br];
    ea = get32(iwea);
    TRACE(T_PCL, " Indirect pointer is %o/%o\n", ea>>16, ea & 0xFFFF);
#if 1
    if (ea & 0x80000000) {
      if (!*store || (ea & 0x8FFF0000) != 0x80000000)
	fault(POINTERFAULT, ea>>16, iwea);
    } else
      ea |= (RP & RINGMASK32);             /* weaken */
#endif
    bit = 0;
    if (ea & EXTMASK32)
      bit = get16(iwea+2) >> 12;
    TRACE(T_PCL, " After indirect, PCLAP ea = %o/%o, bit=%d\n", ea>>16, ea & 0xFFFF, bit);
  }

  if (bit) {
    ea |= EXTMASK32;
    *bitarg = bit;
  } else
    *bitarg = 0;

  if (!*store) {
    putcrs32(XB, ea | (RP & RINGMASK32));
    putcrs16(X, bit);
  }

  return ea;
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

static void argt() {
  unsigned short brsave[6];
  unsigned short argsleft, argdisp, bit;
  short lastarg, store;
  unsigned int utempl;
  ea_t ea, stackfp, rp;
  unsigned short advancepb, advancey;

  TRACE(T_PCL, "Entered ARGT\n");

  /* stackfp is the new stack frame, rp is in the middle of
     argument templates and is advanced after each transfer */

  eap = &gv.brp[SBBR];
  stackfp = getcrs32(SB);
  rp = get32(stackfp+2);

  /* reload the caller's base registers for EA calculations */
  
  brsave[0] = rp >> 16;    brsave[1] = 0;
  /* set ring bits on LB and SB to get correct ring on args */
  brsave[2] = get16(stackfp+4) | ((rp >> 16) & RINGMASK16);
  brsave[3] = get16(stackfp+5);
  brsave[4] = get16(stackfp+6) | ((rp >> 16) & RINGMASK16);
  brsave[5] = get16(stackfp+7);

  argdisp = getcrs16(Y);
  argsleft = getcrs16(YL);
  while (argsleft > 0 || !getcrs16(XL)) {

    TRACE(T_PCL, " Transferring arg, %d left, Y=%o\n", argsleft, getcrs16(Y));

    advancey = 0;
    if (getcrs16(XL)) {
      ea = 0x80000000;
      store = 1;
      advancepb = 0;
    } else {
      ea = pclea(brsave, rp, &bit, &store, &lastarg);
      advancepb = 1;
    }
    if (argsleft > 0 && store) {
      TRACE(T_PCL, " Storing arg, %d left, Y=%o\n", argsleft, getcrs16(Y));
      eap = &gv.brp[SBBR];

      /* NOTE: some version of ucode only store 16 bits for omitted args.
	 Set EHDB to prevent this error.

	 Case 29 wants ring/E-bits preserved for omitted arguments */

      /* 6650 (cpuid 40) strips ring bits on omitted arguments in
	 V-mode, but leaves them in I-mode :-( */

#define OMITTEDARG_MASK1 0x8FFFFFFF
#define OMITTEDARG_MASK2 0xEFFFFFFF

      if ((ea & 0x8FFF0000) == 0x80000000) {
	ea = ea & OMITTEDARG_MASK2;      /* keep ring bits */
#if 0
	if ((getcrs16(KEYS) & 0016000) == 0010000)
	  ea = ea & OMITTEDARG_MASK2;      /* I-mode keeps ring bits */
	else
	  ea = ea & OMITTEDARG_MASK1;      /* V-mode strips ring bits */
#endif
#if 0
	ea = MAKEVA(0100000,0);            /* some machines do this instead */
#endif
	put32(ea, stackfp+getcrs16(Y));
      } else {
	put32(ea, stackfp+getcrs16(Y));
	if (ea & EXTMASK32)
	  put16(bit<<12, stackfp+getcrs16(Y)+2);
      }
      TRACE(T_PCL, " Stored arg IP at %o/%o\n\n", stackfp>>16, (stackfp+getcrs16(Y)) & 0xFFFF);
      argsleft--;
      advancey = 1;
    }

    /* advance rp/pb in new stack frame past this template, and
       advance Y to the next arg displacement in the stack.  Y
       has to be advanced last because the PB store may fault.
       If it does, the ARGT starts over, and this argument will
       have to be transferred again.

       The full 32-bit rp is incremented, which is technically
       wrong but faster, but because PCL DIAG 42 specifically
       checks for segment wraparound, only update the 16-bit
       word offset in the stack frame header. */

    if (advancepb) {
      rp += 2;
      put16(rp & 0xffff, stackfp+3);
      putcrs16(XL, lastarg);
    }
    if (advancey) {
      putcrs16(Y, getcrs16(Y) + 3);
      putcrs16(YL, getcrs16(YL) - 1);
    }
  }

  TRACE(T_PCL, " Return RP=%o/%o\n", rp>>16, rp&0xffff);
}


static void pcl (ea_t ecbea) {
  short i,j;
  unsigned short access;
  unsigned short ecb[9];
  ea_t newrp;                 /* start of new proc */
  ea_t ea;
  short stackrootseg, stackseg;
  unsigned short framesize;
  ea_t stackfp;               /* new stack frame pointer */
  ea_t fpva;                  /* virtual address of the free pointer */
  pa_t pa;                    /* physical address of ecb */
  unsigned short brsave[6];   /* old PB,SB,LB */
  unsigned short utempa;

#define UNWIND_ MAKEVA(013,0106577)

#if 0
  if (ecbea == UNWIND_) {
    printf("pcl: calling unwind_ at %d\n", gv.instcount);
    gv.savetraceflags = ~T_MAP;
  }
#endif

  /* this hack makes DIAG cpu.pcl happy: RP is only supposed to
     have the 16-bit word offset increment, but we do 32-bits
     for speed.  If RPL == 0 during PCL, it means it used to be
     segno/177776 and was incremented to segno+1/0.  So fiddle
     RP here to pass diags.  In practice, RP wraparound is
     just stupid and slow. */

  if (RPL == 0)      /* did RP wrap? */
    RP -= (1<<16);   /* yes, subtract 1 from seg # */

  /* get segment access; mapva ensures either read or gate */

  pa = mapva(ecbea, RP, PACC, &access);
  TRACE(T_PCL, " ecb @ %o/%o, access=%d\n", ecbea>>16, ecbea&0xFFFF, access);

  /* get a copy of the ecb.  gates must be aligned on a 16-word
     boundary, therefore can't cross a page boundary, and mapva has
     already ensured that the ecb page is resident.  For a non-gate
     ecb, check to see if it crosses a page boundary.  If not, a 
     memcpy is okay; if it does, do fetches */

  if (access == 1 && (ecbea & 0xF) != 0)
    fault(ACCESSFAULT, 0, ecbea);
  if ((pa & 01777) <= 02000 - sizeof(ecb)/sizeof(ecb[0])) {
    memcpy(ecb, MEM+pa, sizeof(ecb));
    for (i=0; i<9; i++)
      ecb[i] = swap16(ecb[i]);
  } else {
    for (i=0; i<9; i++)
      ecb[i] = get16(ecbea+i);
  }

  TRACE(T_PCL, " ecb.pb: %o/%o\n ecb.framesize: %d\n ecb.stackroot %o\n ecb.argdisp: %o\n ecb.nargs: %d\n ecb.lb: %o/%o\n ecb.keys: %o\n", ecb[0], ecb[1], ecb[2], ecb[3], ecb[4], ecb[5], ecb[6], ecb[7], ecb[8]);

  newrp = ecb[0]<<16 | ecb[1];
  if (access != 1)    /* not a gate, so weaken ring (outward calls) */
    newrp = newrp | (RP & RINGMASK32);

  /* setup stack frame

     NOTE: newrp must be used here so that accesses succeed when
     calling an inner ring procedure.

     NOTE: see related code in STEX and PRTN.
  */

  framesize = (ecb[2] + 1) & 0xFFFE;   /* round up frame size to even */
  stackrootseg = ecb[3];
  eap = &gv.brp[SBBR];
  if (stackrootseg == 0) {
    stackrootseg = get16((getcrs32(SB)) + 1);
    TRACE(T_PCL, " stack root in ecb was zero, stack root from caller is %o\n", stackrootseg);
  }
  stackseg = stackrootseg;

  stackfp = get32r(MAKEVA(stackrootseg, 0), newrp);
  TRACE(T_PCL, " stack free pointer is %o/%o, current ring=%o, new ring=%o\n", stackfp>>16, stackfp&0xFFFF, (RPH&RINGMASK16)>>13, (newrp&RINGMASK32)>>29);

  /* see if there is room for this frame */

  if ((stackfp & 0xFFFF) + framesize > 0xFFFF) {

    /* no, follow the extension pointer in the last stack segment to
       the next stack segment */

    stackseg = SEGNO32(stackfp);
    fpva = MAKEVA(stackseg, 2);
    stackfp = get32r(fpva, newrp);
    TRACE(T_PCL, " no room for frame, extension pointer at %o/%o is %o/%o\n", fpva>>16, fpva&0xffff, stackfp>>16, stackfp&0xFFFF);

    /* CPU.PCL Case 26 wants fault address word number to be 3 for some
       CPU models; set EHDB switch in CPU.PCL to avoid this error */

    if (stackfp == 0 || (stackfp & 0xFFFF) + framesize > 0xFFFF)
      fault(STACKFAULT, 0, MAKEVA(stackseg,0) | (newrp & RINGMASK32));
  }

  /* setup the new stack frame at stackfp

     NOTE: Ring must be added to stackfp so that any page faults that
     occur while setting up the stack will have the correct ring for
     CPU.PCL tests */

  stackfp |= (newrp & RINGMASK32);
  put16r(0, stackfp, newrp);
  put16r(stackrootseg, stackfp+1, newrp);
  put32r(RP, stackfp+2, newrp);
  put32r(getcrs32(SB), stackfp+4, newrp);
  put32r(getcrs32(LB), stackfp+6, newrp);
  put16r(getcrs16(KEYS), stackfp+8, newrp);
  put16r(RPL, stackfp+9, newrp);

#if 0
  /* LATER: save caller's base registers for address calculations, and
     pass to argt */

  if (ecb[5] > 0) {
    brsave[0] = RPH;       brsave[1] = 0;
    brsave[2] = getcrs16(SBH);  brsave[3] = getcrs16(SBL);
    brsave[4] = getcrs16(LBH);  brsave[5] = getcrs16(LBL);
  }
#endif

  /* load new execution state from ecb */

  TRACE(T_PCL, " before update, stackfp=%o/%o, SB=%o/%o\n", stackfp>>16, stackfp&0xFFFF, getcrs16(SBH), getcrs16(SBL));
  if (access == 1)                 /* for gate access, don't weaken ring */
    putcrs32(SB, stackfp);
  else
#if 1
    putcrs32(SB, (stackfp & ~RINGMASK32) | (RP & RINGMASK32));
#else
  /* XXX: use newrp ring, right?  What about setting ring on LB, below? */
    putcrs32(SB, (stackfp & ~RINGMASK32) | (newrp & RINGMASK32));
#endif
  TRACE(T_PCL, " new SB=%o/%o\n", getcrs16(SBH), getcrs16(SBL));
  putcrs32(LB, (ecb[6]<<16) | ecb[7]);
  newkeys(ecb[8] & 0177770);

  /* update the stack free pointer; this has to wait until after all
     memory accesses, in case of stack page faults (PCL restarts).

     Some ucode versions incorrectly store the ring in the free
     pointer if the extension pointer was followed.  Try setting EHDB
     to suppress this spurious DIAG error. */

  ea = MAKEVA(stackrootseg, 0) | (newrp & RINGMASK32);
  if (cpuid == 15)
    put32r(stackfp+framesize, ea, newrp);
  else
    put32r((stackfp+framesize) & ~RINGMASK32, ea, newrp);

  /* transfer arguments if arguments are expected.  There is no
     documentation explaining how the Y register is used during
     argument transfer, so:
     Y(high) = stack frame offset to store next argument
     Y(low) = number of arguments left to transfer (JW hack!) */


  /* if a page fault occurs during argument transfer, we need to 
     make sure to use the current RP, which points to the ARGT
     instruction.  Otherwise, the return from the page fault
     is to the PCL instruction, which has already completed at
     this point

     RP changes here, which may change the ring.  The brp cache needs
     to be invalidated if the ring changes. (Although, it seems odd to
     invalidate the brp here, because we could use it for argument
     transfers...  Maybe this isn't quite the right place for RP and
     live base registers to change.  See failing PCL DIAG cases 35 &
     36.)
 */

  if ((newrp ^ RP) & RINGMASK32)
    invalidate_brp();
  RP = newrp;
  gv.prevpc = RP;
  TRACE(T_PCL, " new RP=%o/%o\n", RPH, RPL);

  if (ecb[5] > 0) {
    putcrs16(Y, ecb[4]);
    putcrs16(YL, ecb[5]);
#if 0
    putcrs16(X, 0);
#endif
    putcrs16(XL, 0);
    argt();

    /* if tracing terminal output, display it now.  This has to occur
     after ARGT has setup the argument pointers.  Note that if a fault
     occurs while accessing the arguments here, it will return to ARGT
     in the main emulator loop and nothing will be logged. */

#if 0
    if (TRACEUSER && ((ecbea & 0xFFFFFFF) == tnou_ea || (ecbea & 0xFFFFFFF) == tnoua_ea)) {
      unsigned char tnstring[500];
      unsigned short tnlen, tnword;
      unsigned char tnchar;
      ea = getcrs32(SB) + ecb[4];
      utempa = get16(get32(ea));       /* 1st arg: userid */
      if (utempa == ((getcrs16(OWNERL)>>6) & 0xff)) {
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
#endif

    /* this crap was to trace tsrc$$ failing in Prime Information.
       Information uses FP math on pathname lengths, and the rounding
       was causing problems */

#if 0
    if (((ecbea & 0xFFFFFFF) == tsrc_ea)) {
      ea_t eatemp;
      int utempl;
      unsigned short utempa,utempa1,utempa2;

      ea = getcrs32(SB) + ecb[4];
      utempa = get16(get32(ea));       /* 1st arg: key */
      TRACEA(" TSRC$$: key = %d\n", utempa);
      eatemp = get32(ea+9);       /* 4th arg: CP(1..2) */
      utempl = get32(eatemp);   
      utempa1 = utempl>>16;       /* starting cp */
      utempa2 = utempl&0xffff;    /* # chars */
      TRACEA("   cp(0)=%d, cp(1)=%d, loc(cp)=%o/%o\n", utempa1, utempa2, eatemp>>16, eatemp&0xffff);
      ea = get32(ea+3);              /* 2nd arg: string */
      j = 0;
      for (i=utempa1; i<utempa1+utempa2; i++) {
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
      TRACEA(" TSRC$$ path = %s\n", tnstring);
    }
#endif

    INCRP;    /* advance real RP past ARGT after argument transfer */
  }
}

/* NOTE: the calf instruction may be running in an outer ring, so
   accesses to protected data need to use get16r0 */

static void calf(ea_t ea) {
  ea_t pcbp, stackfp, csea;
  unsigned short first,next,last,this;
  unsigned short cs[6];
  int i;

  pcbp = getcrs32ea(OWNER);    /* my pcb pointer */

  /* get concealed stack entry address */

  eap = &gv.brp[UNBR];
  first = get16r0(pcbp+PCBCSFIRST);
  next = get16r0(pcbp+PCBCSNEXT);
  last = get16r0(pcbp+PCBCSLAST);
  TRACE(T_FAULT, "CALF: first=%o, next=%o, last=%o\n", first, next, last);
  if (next == first)
    this = last;
  else
    this = next-6;
  csea = MAKEVA(getcrs16(OWNERH)+gv.csoffset, this);
  TRACE(T_FAULT,"CALF: cs frame is at %o/%o\n", csea>>16, csea&0xFFFF);

#ifdef DBG
  /* make sure ecb specifies zero args (not part of the architecture)

     NOTE: this check needs get16r0 too because in Rev 19, segment 5
     only has gate access and this read caused an access fault when an
     R-mode I/O instruction occurs under Primos (causing a restricted
     inst fault that is handled in the outer ring). */

  if (get16r0(ea+5) != 0) {
    printf("CALF ecb at %o/%o has arguments!\n", ea>>16, ea&0xFFFF);
    fatal(NULL);
  }
#endif

  pcl(ea);

  /* get the concealed stack entries and adjust the new stack frame */

  eap = &gv.brp[UNBR];
  for (i=0; i<6; i++)
    cs[i] = get16r0(csea+i);

  /* pop the concealed stack 
     XXX: this was after code below.  Does it matter? */

  put16r0(this, pcbp+PCBCSNEXT);

  TRACE(T_FAULT, "CALF: cs entry: retpb=%o/%o, retkeys=%o, fcode=%o, faddr=%o/%o\n", cs[0], cs[1], cs[2], cs[3], cs[4], cs[5]);

  eap = &gv.brp[SBBR];
  stackfp = getcrs32(SB);
  put16(1, stackfp+0);                          /* flag it as CALF frame */
  put16(cs[0], stackfp+2);                      /* return PBH */
  put16(cs[1], stackfp+3);                      /* return PBL */
  put16(cs[2], stackfp+8);                      /* return keys */
  put16(cs[3], stackfp+10);                     /* fault code */
  put16(cs[4], stackfp+11);                     /* fault addr segno */
  put16(cs[5], stackfp+12);                     /* fault addr word */
}


/* process exchange register save:  saves the current register
   set to the process pcb. 
   NOTES:
   - adding "wait" arg and only saving base registers fixed Case 63
*/

static void pxregsave(unsigned short wait) {
  ea_t pcbp, regp;
  int i;
  unsigned short mask;

  /* if registers aren't owned or are already saved, return */

  if (getcrs16(OWNERL) == 0) {
    TRACE(T_PX, "pxregsave: OWNERL is zero: no save\n");
    return;
  }
  if (getcrs16(KEYS) & 1) {
    TRACE(T_PX, "pxregsave: SD=1: no save\n");
    return;
  }

  TRACE(T_PX, "pxregsave: saving registers owned by %o (wait=%d)\n", getcrs16(OWNERL), wait);
    
  /* NB: I think hardware might save the base registers in a predictable
     location in the PCB register save area, rather than compressed in a
     random order, because IIRC, Primos sometimes looks at a waiting
     process' PB to see where it is waiting */

  pcbp = getcrs32(OWNER);
  regp = pcbp+PCBREGS;
  mask = 0;
  for (i=(wait?014:0); i<020; i++) {
    if (getgr32(i) != 0) {
      mask |= BITMASK16(i+1);
      put32r0(getgr32(i), regp);
      regp += 2;
    }
  }
  put16r0(mask, pcbp+PCBMASK);
  put32r0(getcrs32(TIMER), pcbp+PCBIT);  /* save interval timer */
  putcrs16(KEYS, getcrs16(KEYS) | 1);                 /* set save done bit */
  put16r0(getcrs16(KEYS), pcbp+PCBKEYS);
}

/* pxregload: load pcbp's registers from their pcb to the current
   register set, set OWNERL

   NOTE: RP must be set by the caller since this happens whenever
   a process is dispatched - not just when registers are loaded */

static void pxregload (ea_t pcbp) {
  ea_t regp;
  unsigned short mask;
  int i;

  TRACE(T_PX, "pxregload loading registers for process %o/%o\n", pcbp>>16, pcbp&0xFFFF);
  regp = pcbp+PCBREGS;
  mask = get16r0(pcbp+PCBMASK);
  for (i=0; i<020; i++) {
    if (mask & BITMASK16(i+1)) {
      putgr32(i, get32r0(regp));
      regp += 2;
    } else {
      putgr32(i, 0);
    }
  }
  newkeys(get16r0(pcbp+PCBKEYS));
  putcrs32(DTAR2, get32r0(pcbp+PCBDTAR2));
  putcrs32(DTAR3, get32r0(pcbp+PCBDTAR3));
  putcrs32(TIMER, get32r0(pcbp+PCBIT));
  putcrs16(OWNERL, pcbp & 0xFFFF);

  TRACE(T_PX, "pxregload: registers loaded, ownerl=%o, modals=%o\n", getcrs16(OWNERL), getcrs16(MODALS));
}


/* selects a register set and sets modals and crs/crsl to that register set.
   pcbw is OWNERL of the process that will use the register set. */

static void ors(unsigned short pcbw) {
  static short regq[] = {0,1,2,3,4,5,6,7};
  short i,rx;
  unsigned short ownerl, currs, rs;
  short ownedx, freex, savedx;
  unsigned short modals;


  currs = (getcrs16(MODALS) & 0340) >> 5;
  TRACE(T_PX, "ors: currs = %d, modals = %o\n", currs, getcrs16(MODALS));

#if 1
  /* this is the code for handling more than 2 register sets.  It is
     "smarter" than the Prime u-code, so probably doesn't pass DIAG
     tests.  I haven't tested whether the extra overhead of keeping a
     LRU queue for register sets is worth it vs. the simpler Prime
     way. */

  ownedx = freex = savedx = -1;
  for (rx = regsets[cpuid]-1; rx >= 0; rx--) {   /* search LRU first */
    rs = regq[rx];
    TRACE(T_PX, "ors: check rs %d: owner=%o/%o, saved=%d\n", rs, swap32(regs.sym.userregs[rs][21])>>16, swap32(regs.sym.userregs[rs][21]) & 0xFFFF, swap32(regs.sym.userregs[rs][20]) & 1);
    ownerl = swap32(regs.sym.userregs[rs][21]) & 0xFFFF;      /* OWNERH/OWNERL */

    /* NOTE: could stick breaks after a rs is found, except that for
       debug, I wanted to make sure a process never owns 2 register sets */

    if (ownerl == pcbw) {
      if (ownedx >= 0) 
	fatal("Process owns more than 1 register set!");
      ownedx = rx;
    } else if (ownerl == 0)
      freex = rx;
    else if (savedx < 0 && (swap32(regs.sym.userregs[rs][20]) & 1)) /* KEYS/MODALS */
      savedx = rx;
  }
  if (ownedx >= 0) {
    rx = ownedx;
    TRACE(T_PX, "ors: using owned reg set %d\n", regq[rx]);
  } else if (freex >= 0) {
    rx = freex;
    TRACE(T_PX, "ors: using free reg set %d\n", regq[rx]);
  } else if (savedx >= 0) {
    rx = savedx;
    TRACE(T_PX, "ors: using saved reg set %d\n", regq[rx]);
  } else {
    rx = regsets[cpuid]-1;                  /* least recently used */
    TRACE(T_PX, "ors: no reg set found; using %d\n", regq[rx]);
  }
  rs = regq[rx];
  if (rs >= regsets[cpuid])
    fatal("ors: rs chosen is too big");
  modals = (getcrs16(MODALS) & ~0340) | (rs << 5);

  /* put the register set selected at the front of the queue */

  for (i=rx; i>0; i--)
    regq[i] = regq[i-1];
  regq[0] = rs;
#else
  modals = getcrs16(MODALS) ^ 040;
  rs = (modals & 0340) >> 5;
#endif
  crsl = regs.sym.userregs[rs];  /* pointer: no swap */
  putcrs16(MODALS, modals);
  TRACE(T_PX, "ors: rs = %d, reg set in modals = %d, modals = %o\n", rs, (getcrs16(MODALS) & 0340)>>5, getcrs16(MODALS));

  /* invalidate the mapva translation cache */

  invalidate_brp();
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

static void dispatcher() {
  ea_t pcbp, rlp;
  unsigned short pcbw;      /* pcb word address */
  unsigned short rsnum;
  unsigned short rlbol;
  unsigned short utempa;


  putcrs16(MODALS, getcrs16(MODALS) | 0100000);     /* ISG says dispatcher enables int. */

  if (regs.sym.pcba != 0) {         /* zero: no byte swap */
    pcbp = MAKEVA(getcrs16(OWNERH), getar16(PCBA16));
    TRACE(T_PX, "disp: dispatching PPA, pcba=%o, pla=%o\n", getar16(PCBA16), getar16(PLA16));

  } else if (regs.sym.pcbb != 0) {  /* zero: no byte swap */
    pcbp = MAKEVA(getcrs16(OWNERH), getar16(PCBB16));
    regs.sym.pcba = regs.sym.pcbb;  /* Prime->Prime: no byte swap */
    regs.sym.pla = regs.sym.plb;    /* Prime->Prime: no byte swap */
    regs.sym.pcbb = 0;              /* zero: no byte swap */
    TRACE(T_PX, "disp: dispatching PPB, pcba=%o, pla=%o\n", getar16(PCBA16), getar16(PLA16));

  } else {
    TRACE(T_PX, "disp: scanning RL\n");
    if (regs.sym.pla != 0)          /* zero: no byte swap */
      rlp = MAKEVA(getcrs16(OWNERH), getar16(PLA16));
    else if (regs.sym.plb != 0)     /* zero: no byte swap */
      fatal("disp: pla is invalid, plb is valid?");
    else
      fatal("dispatch: both pla and plb are zero; can't locate ready list");
    while(1) {
      rlbol = get16r0(rlp);
      if (rlbol != 0)
	break;
      rlp += 2;
    }
    if (rlbol == 1)
      goto idle;
    pcbp = MAKEVA(getcrs16(OWNERH), rlbol);
    putar16(PCBA16, rlbol);
    putar16(PLA16, rlp & 0xFFFF);
  }
  pcbw = pcbp & 0xFFFF;
  TRACE(T_PX, "disp: process %o/%o selected\n", pcbp>>16, pcbw);

#if 0
  /* debug tests to verify ready list structure
     NOTE: this test causes some DIAGS to fail, so has been disabled */

  rlp = MAKEVA(getcrs16(OWNERH), getar16(PLA16));
  rlbol = get16r0(rlp);
  if (rlbol != pcbw) {
    printf("disp: rl bol=%o, != process dispatched=%o\n", rlbol, pcbw);
    fatal(NULL);
  }
#endif
#if 0
  /* NOTE: if a running process has its priority changed (in the pcb), this
     test fails, so it has been disabled  */

  if (get16r0(pcbp+PCBLEV) != getar16(PLA16)) {
    printf("disp: dispatched process level=%o, != pla=%o\n", get16r0(pcbp+PCBLEV), getar16(PLA16));
    fatal(NULL);
  }
#endif
  
  /* pcbp now points to the process we're going to run (pcbw is the
     16-bit word number that will go in OWNERL).  By definition, this
     process should not be on any wait lists, so pcb.waitlist(seg)
     should be zero.  Check it */

#if 1
  /* NOTE: CPU.PXT1 can fail with this enabled */

  utempa = get16r0(pcbp+PCBWAIT);
  if (utempa != 0) {
    printf("disp: pcb %o/%o selected, but wait segno = %o\n", pcbp>>16, pcbp&0xFFFF, utempa);
    fatal(NULL);
  }
#endif

  /* save RP in current register set before possibly switching */

  putcrs32(PB, (RP & 0x7FFFFFFF));

  /* find a register set for this process */

#if 0
  rsnum = (getcrs16(MODALS) & 0340)>>5;
  if (getcrs16(OWNERL) != pcbw && getcrs16(OWNERL) != 0)
    if (regs.rs16[rsnum ^ 1][OWNERL] == 0 || (regs.rs16[rsnum ^ 1][OWNERL] == pcbw && (regs.rs16[rsnum ^ 1][KEYS] & 1)) || ((regs.rs16[rsnum ^ 1][KEYS] & 1) && !(getcrs16(KEYS) & 1)))
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

  Adding "&& getcrs16(OWNERL) != 0" and setting this to zero in IRTN if PPA is
  invalid fixes case 81 w/o breaking any other tests.

  */

#if 1
  if (getcrs16(OWNERL) != pcbw && getcrs16(OWNERL) != 0)
    ors(pcbw);
#endif

  /* If the selected register set is owned and hasn't been saved, save
     it before taking it */

  if (getcrs16(OWNERL) == pcbw) {
    TRACE(T_PX, "disp: reg set already owned by %o: no save or load\n", getcrs16(OWNERL));
    /* NOTE: call newkeys to make sure amask gets set correctly!
       Otherwise, 32R mode programs are flaky */
    newkeys(getcrs16(KEYS));
  } else {
    pxregsave(0);
    pxregload(pcbp);
  }

  RP = getcrs32(PB);

  /* if we were executing from registers and not in I-mode, make sure
     the fault bit is set again on RP.  We try to hide this hack from
     Primos. */

  if (RPL < gv.livereglim && ((getcrs16(KEYS) & 0016000) != 010000))
    RP |= 0x80000000;
  putcrs16(PBL, 0);
  putcrs16(KEYS, getcrs16(KEYS) & ~3);      /* erase "in dispatcher" and "save done" */
  TRACE(T_PX, "disp: returning from dispatcher, running process %o/%o at %o/%o, modals=%o, ppa=%o, pla=%o, ppb=%o, plb=%o\n", getcrs16(OWNERH), getcrs16(OWNERL), RPH, RPL, getcrs16(MODALS), getar16(PCBA16), getar16(PLA16), getar16(PCBB16), getar16(PLB16));

  /* if this process' abort flags are set, clear them and take process fault */

  utempa = get16r0(pcbp+PCBABT);
  if (pcbw == 0100100 && sensorabort == 1) {
    utempa |= 01000;    /* set user 1's sensor check abort flag */
    sensorabort = 2;
  }
  if (utempa != 0) {
    TRACE(T_PX, "dispatch: abort flags for %o are %o\n", getcrs16(OWNERL), utempa);
    put16r0(0, pcbp+PCBABT);
    fault(PROCESSFAULT, utempa, 0);
    fatal("fault returned after process fault");    
  }

  firstbdx = 1;
  return;

idle:
  fatal("dispatch idle...");
}

/* take me off the ready list, setting my pcb link pointer to the arg
   passed in.  The dispatcher should always be entered after this
   routine. */

static void unready (ea_t waitlist, unsigned short newlink) {
  unsigned short bol, eol;
  unsigned int rl;
  ea_t rlp, pcbp;

#if 0
  /* this fails with rev 23.4:
Fatal error: instruction #86286965 at 6/15274 UNLOAD+'120: 315 1400
owner=71600 DUMPCB, keys=14000, modals=37
unready: pcba mismatch
  */
  if (getar16(PCBA16) != getcrs16(OWNERL))
    fatal("unready: pcba mismatch");
#endif

  pcbp = getcrs32ea(OWNER);
  rlp = MAKEVA(getcrs16(OWNERH), getar16(PLA16));
  rl = get32r0(rlp);
  bol = rl >> 16;
  eol = rl & 0xFFFF;
#if 0
  /* this fails with rev 23.4:
rlp=240/136, bol=100500, eol=100500, pcbp=240/71600, pla=136, pcba=100500
Fatal error: instruction #86212270 at 6/15274 UNLOAD+'120: 315 1400
owner=71600 DUMPCB, keys=14000, modals=77
unready: I'm not first on the ready list
  */
  if (bol != (pcbp & 0xFFFF)) {
    printf("rlp=%o/%o, bol=%o, eol=%o, pcbp=%o/%o, pla=%o, pcba=%o\n", rlp>>16, rlp&0xFFFF, bol, eol, pcbp>>16, pcbp&0xFFFF, getar16(PLA16), getar16(PCBA16));
    fatal("unready: I'm not first on the ready list");
  }
#endif
  if (bol == eol) {
    bol = 0;
    eol = 0;
  } else {
    bol = get16r0(pcbp+1);
  }
  rl = (bol<<16) | eol;
  put32r0(rl, rlp);           /* update ready list */
  TRACE(T_PX, "unready: new rl bol/eol = %o/%o\n", rl>>16, rl&0xFFFF);
  put16r0(newlink, pcbp+1);   /* update my pcb link */
  put32r0(waitlist, pcbp+2);  /* update my pcb wait address */
  putcrs32(PB, RP & 0x7FFFFFFF);
  pxregsave(1);
  regs.sym.pcba = 0;          /* zero: no byte swap */
}


/* pcbp points to the pcb to put on the ready list
   begend is 1 for beginning, 0 for end
   returns true if this process is higher priority than me
*/

static unsigned short ready (ea_t pcbp, unsigned short begend) {
  ea_t rlp;
  ea_t xpcbp;
  unsigned short pcbw,level,resched;
  unsigned int rl;

  if ((pcbp & 0xFFFF) == getcrs16(OWNERL))
    fatal("Tried to put myself on the ready list!");
#if 0
  /* NOTE: restore drive b, boot 14314, halts here after login_server */
  if (getar16(PCBA16) != getcrs16(OWNERL))
    fatal("I'm running, but not regs.sym.pcba!");
#endif

  level = get16r0(pcbp+PCBLEV);
  rlp = MAKEVA(getcrs16(OWNERH),level);
  rl = get32r0(rlp);
  TRACE(T_PX, "ready: pcbp=%o/%o\n", pcbp>>16, pcbp&0xFFFF);
  TRACE(T_PX, "ready: old bol/eol for level %o = %o/%o\n", level, rl>>16, rl&0xFFFF);
  pcbw = pcbp;                            /* pcb word number */
  if ((rl>>16) == 0) {                    /* bol=0: this RL level was empty */
    put32r0(0, pcbp+1);                   /* set link and wait SN in pcb */
    rl = (pcbw<<16) | pcbw;               /* set beg=end */
  } else if (begend) {                    /* notify to beginning */
    put32r0(rl & 0xFFFF0000, pcbp+1);     /* set link and wait SN in pcb */
    rl = (pcbw<<16) | rl&0xFFFF;          /* new is bol, eol is unchanged */
  } else {                                /* notify to end */
    put32r0(0, pcbp+1);                   /* set link and wait SN in pcb */
    xpcbp = MAKEVA(getcrs16(OWNERH),rl&0xFFFF); /* get ptr to last pcb at this level */
    put16r0(pcbw,xpcbp+1);                /* set last pcb's forward link */
    rl = (rl & 0xFFFF0000) | pcbw;        /* rl bol is unchanged, eol is new */
  }
  put32r0(rl, rlp);
  TRACE(T_PX, "ready: new bol/eol for level %o = %o/%o, pcb's link is %o\n", level, rl>>16, rl&0xFFFF, get16r0(pcbp+1));

  /* is this new process higher priority than me?  If so, return 1
     so that the dispatcher is entered.  If not, check for new plb/pcbb */

  resched = 0;
  if (level < getar16(PLA16) || (level == getar16(PLA16) && begend)) {
    regs.sym.plb = regs.sym.pla;          /* Prime->Prime: no byte swap */ 
    regs.sym.pcbb = regs.sym.pcba;        /* Prime->Prime: no byte swap */ 
    putar16(PLA16, level);
    putar16(PCBA16, pcbw);
    resched = 1;
  } else if (level < getar16(PLB16) || (level == getar16(PLB16) && begend)) {
    putar16(PLB16, level);
    putar16(PCBB16, pcbw);
  }
  return resched;
}


static void pwait() {
  ea_t ea;
  ea_t pcbp, prevpcbp;
  unsigned int utempl;
  unsigned int pcblevnext;      /* pcb level and link */
  unsigned short bol;
  unsigned short pcblev;
  unsigned short mylev;
  short count;

  ea = apea(NULL);
  TRACE(T_PX, "%o/%o: wait on %o/%o, pcb %o, keys=%o, modals=%o\n", RPH, RPL, ea>>16, ea&0xFFFF, getcrs16(OWNERL), getcrs16(KEYS), getcrs16(MODALS));
  utempl = get32r0(ea);       /* get count and BOL */
  count = utempl>>16;         /* count (signed) */
  bol = utempl & 0xFFFF;      /* beginning of wait list */
  TRACE(T_PX, " wait list count was %d, bol was %o\n", count, bol);
  count++;
  if (count <= 0) {           /* no wait is needed (mutex locks) */
    put16(*(unsigned short *)&count, ea); /* so update count and continue */
    return;
  }

  /* I have to wait */

  if (count == 1 && bol != 0)
    fatal("WAIT: count == 1 but bol != 0");
  if (count > 1 && bol == 0)
    fatal("WAIT: count > 1 but bol == 0");
  if (regs.sym.pcba == 0)              /* zero: no byte swap */
    fatal("WAIT: pcba is zero");
#if 0
  /* enabling this causes rev 23.4 to fail:
WAIT: pcba=100500 != ownerl=71600
Fatal error: instruction #86137885 at 6/15274 UNLOAD+'120: 315 1400
owner=71600 DUMPCB, keys=14000, modals=77           */

  if (getar16(PCBA16) != getcrs16(OWNERL)) {
    printf("WAIT: pcba=%o != ownerl=%o\n", getar16(PCBA16), getcrs16(OWNERL));
    fatal(NULL);
  }
#endif
  mylev = get16r0(getcrs32ea(OWNER));

  if (bol != 0) {
    pcbp = MAKEVA(getcrs16(OWNERH),bol);
    pcblevnext = get32r0(pcbp);
    pcblev = pcblevnext >> 16;
  }
  TRACE(T_PX, " my level=%o, pcblev=%o\n", mylev, pcblev);

  if (count == 1 || mylev < pcblev) {   /* add me to the beginning */
    utempl = (count<<16) | getcrs16(OWNERL);
    put32r0(utempl, ea);    /* update semaphore count/bol */
  } else {
    /* do a priority scan... */
    while (pcblev <= mylev && bol != 0) {
      prevpcbp = pcbp;
      bol = pcblevnext & 0xFFFF;
      if (bol != 0) {
	pcbp = MAKEVA(getcrs16(OWNERH),bol);
	pcblevnext = get32r0(pcbp);
	pcblev = pcblevnext >> 16;
      }
    }
    put16r0(getcrs16(OWNERL), prevpcbp+PCBLINK);
    put16r0(*(unsigned short *)&count, ea);    /* update count */
    TRACE(T_PX, " new count=%d, new link for pcb %o=%o, bol=%o\n", count, prevpcbp&0xffff, getcrs16(OWNERL), bol);
  }
  unready(ea, bol);
  dispatcher();
}

/* this handles several forms of notify:
   - 001210 = NFYE
   - 001211 = NFYB
   - 001214 = INEN, notify to end, no CAI
   - 001215 = INBN, notify to beg, no CAI
   - 001216 = INEC, notify to end, CAI
   - 001217 = INBC, notify to beg, CAI
*/

static void nfy(unsigned short inst) {
  unsigned short resched, begend, bol;
  ea_t ea, pcbp;
  unsigned int utempl;
  short scount;
  static char *nfyname[] = {"nfye","nfyb"," "," ","inen","inbn","inec","inbc"};

  resched = 0;
  begend = inst & 1;
  if (getar16(PCBA16) != getcrs16(OWNERL)) {
    printf("NFY: regs.pcba = %o, but OWNERL = %o\n", getar16(PCBA16), getcrs16(OWNERL));
    fatal(NULL);
  }
  ea = apea(NULL);
  utempl = get32r0(ea);       /* get count and BOL */
  scount = utempl>>16;        /* count (signed) */
  bol = utempl & 0xFFFF;      /* beginning of wait list */
  TRACE(T_PX, "%o/%o: opcode %o %s, ea=%o/%o, count=%d, bol=%o, I am %o\n", RPH, RPL, inst, nfyname[inst-01210], ea>>16, ea&0xFFFF, scount, bol, getcrs16(OWNERL));

  /* XXX: on later models, semaphore overflow should cause a fault */

  if (scount == -32768) {
    printf("NFY: semaphore overflow at ea %o/%o %s\n", ea>>16, ea&0xFFFF, searchloadmap(ea, 'x'));
    fatal(NULL);
  }

  if (scount > 0) {
    if (bol == 0) {
      printf("NFY: bol is zero, count is %d for semaphore at %o/%o\n", scount, ea>>16, ea&0xFFFF);
      fatal(NULL);
    }
    pcbp = MAKEVA(getcrs16(OWNERH), bol);
    utempl = get32r0(pcbp+PCBWAIT);
    if (utempl != ea) {
      printf("NFY: bol=%o, pcb waiting on %o/%o != ea %o/%o\n", bol, utempl>>16, utempl&0xFFFF, ea>>16, ea&0xFFFF);
      fatal(NULL);
    }
    bol = get16r0(pcbp+PCBLINK);     /* get new beginning of wait list */
    resched = ready(pcbp, begend);     /* put this pcb on the ready list */
  }

  scount = scount-1;
  utempl = (scount<<16) | bol;
  put32r0(utempl, ea);           /* update the semaphore */

  if (inst & 4) {                /* interrupt notify */
    if (inst & 2)                /* clear active interrupt */
      gv.intvec = -1;
    /* not sure about all this... Case 85/87 */
    newkeys(getar16(PSWKEYS16));
    RP = getar32(PSWPB32);
    putcrs16(PBH, RPH);          /* NOTE: won't have fault bit */
    if (RPL < gv.livereglim && ((getcrs16(KEYS) & 0016000) != 010000))
      RP |= 0x80000000;
  }

  if (resched || (inst & 4))
    dispatcher();
}


static void lpsw() {
  ea_t ea;
  unsigned short m;

  TRACE(T_PX, "\n%o/%o: LPSW issued\n", RPH, RPL);
  TRACE(T_PX, "LPSW: before load, RPH=%o, RPL=%o, keys=%o, modals=%o, owner=%o/%o\n", RPH, RPL, getcrs16(KEYS), getcrs16(MODALS), getcrs16(OWNERH), getcrs16(OWNERL));
  TRACE(T_PX, "LPSW: crs=%d, ownerl[2]=%o, keys[2]=%o, modals[2]=%o, ownerl[3]=%o, keys[3]=%o, modals[3]=%o\n", crs==regs.rs16[2]? 2:3, swap16(regs.rs16[2][OWNERL]), swap16(regs.rs16[2][KEYS]), swap16(regs.rs16[2][MODALS]), swap16(regs.rs16[3][OWNERL]), swap16(regs.rs16[3][KEYS]), swap16(regs.rs16[3][MODALS]));

  ea = apea(NULL);
  RPH = get16(ea);
  RPL = get16(INCVA(ea,1));
  newkeys(get16(INCVA(ea,2)));
  m = get16(INCVA(ea,3));
#if 0

  /* NOTE: rev 18 changes the register set to 010.  On old Primes,
     there are 2 user register sets, numbered 000 (0) and 010 (2).  I
     think 010 is the initial register set, so it works okay, but in
     the emulator, register sets are numbered 0-7, and register set 2
     is the 3rd register set.  On old Primes, there are only 2
     register sets initialized, so using RS 3 in the emulator bombs
     because the dtar registers aren't initialized by Primos.  So I
     commented the register switch code out.  This might cause a diag
     to fail - not sure.  But the Prime instruction set guide says not
     to change the current register set field in the modals; only the
     dispatcher (microcode / emulator) is supposed to fiddle this. */

  if ((m & 0340) != (getcrs16(MODALS) & 0340)) {
    TRACE(T_PX, "LPSW: WARNING: changed current register set: current modals=%o, new modals=%o\n", getcrs16(MODALS), m);
    /* not sure about doing this... */
    printf("WARNING: LPSW changed current register set: current modals=%o, new modals=%o\n", getcrs16(MODALS), m);
    crsl = regs.sym.userregs[(m & 0340) >> 5];   /* pointers: no byte swap */
  }
#endif
  invalidate_brp();

  putcrs16(MODALS, m);
  gv.inhcount = 1;

  TRACE(T_PX, "LPSW:    NEW RPH=%o, RPL=%o, keys=%o, modals=%o, owner=%o/%o\n", RPH, RPL, getcrs16(KEYS), getcrs16(MODALS), getcrs16(OWNERH), getcrs16(OWNERL));
  TRACE(T_PX, "LPSW: crs=%d, ownerl[2]=%o, keys[2]=%o, modals[2]=%o, ownerl[3]=%o, keys[3]=%o, modals[3]=%o\n", crs==regs.rs16[2]? 2:3, swap16(regs.rs16[2][OWNERL]), swap16(regs.rs16[2][KEYS]), swap16(regs.rs16[2][MODALS]), swap16(regs.rs16[3][OWNERL]), swap16(regs.rs16[3][KEYS]), swap16(regs.rs16[3][MODALS]));
  if (getcrs16(MODALS) & 020)
    TRACE(T_PX, "Mapped I/O enabled\n");
  if (getcrs16(MODALS) & 4) {
    TRACE(T_PX, "Segmentation enabled\n");
    gv.livereglim = 010;
  } else 
    gv.livereglim = 040;
  if (getcrs16(MODALS) & 010) {
    TRACE(T_PX, "Process exchange enabled:\n");
    TRACE(T_PX, "LPSW: PLA=%o, PCBA=%o, PLB=%o, PCBB=%o\n", getar16(PLA16), getar16(PCBA16), getar16(PLB16), getar16(PCBB16));
#if 0
    for (i=getar16(PLA16);; i += 2) {
      ea = MAKEVA(getcrs16(OWNERH), i);
      utempa = get16(ea);
      TRACE(T_PX, " Level %o: BOL=%o, EOL=%o\n", i, utempa, get16(ea+1));
      if (utempa == 1)
	break;
      while (utempa > 0)
	utempa = dumppcb(utempa);
    }
#endif
    if (getcrs16(KEYS) & 2) {
      TRACE(T_PX, "LPSW: before disp, RPH=%o, RPL=%o, keys=%o, modals=%o\n", RPH, RPL, getcrs16(KEYS), getcrs16(MODALS));
      dispatcher();
      TRACE(T_PX, "LPSW: after disp, RPH=%o, RPL=%o, keys=%o, modals=%o\n", RPH, RPL, getcrs16(KEYS), getcrs16(MODALS));
      TRACE(T_PX, "LPSW: crs=%d, ownerl[2]=%o, keys[2]=%o, modals[2]=%o, ownerl[3]=%o, keys[3]=%o, modals[3]=%o\n", crs==regs.rs16[2]? 2:3, swap16(regs.rs16[2][OWNERL]), swap16(regs.rs16[2][KEYS]), swap16(regs.rs16[2][MODALS]), swap16(regs.rs16[3][OWNERL]), swap16(regs.rs16[3][KEYS]), swap16(regs.rs16[3][MODALS]));
    }
  }
}


static void sssn() {
#if 1
  static char snbuf[] = "FN      123456  ";  /* dummy serial number for DIAG */
#else
  static char snbuf[] = "FN      018554  ";  /* Doug's serial number */
#endif
  ea_t ea;
  int i;

  TRACE(T_FLOW, " SSSN\n");
  ea = getcrs32(XB);
  for (i=0; i<8; i++)
    put16(swap16(*(((unsigned short *)snbuf)+i)), ea+i);
  for (i=8; i<16; i++) {
    put16(0, ea+i);
  }

  /* if SSSN is from segment 14, it's probably the rev 23.4.Y2K 
     system serial number check.  Bypass the check loop. */

  if (RPH == 014)
    RP += 12;
}


/* C-Pointer conversion macros

   EACP: convert Prime effective address to C-Pointer value (the bit offset
   moves to the right bit so that byte math can be performed)

   CPEA: convert a C-Pointer back to an effective address.  The fault and
   ring bits from the ea argument are used (can pass in zero)
*/

#define EACP(v) (((v) & 0x0FFFFFFF)<<1) | (((v)>>28) & 1)
#define CPEA(ea,v) ((ea) & 0xE0000000) | (((v) & 1)<<28) | (((v)>>1) & 0x0FFFFFFF)

/* Character instructions */

#define GETFLR(n) ((getgr32(FLR0+2*(n)) >> 16) | ((getgr32(FLR0+2*(n)) & 0x1F) << 16))
#define PUTFLR(n,v) putgr32(FLR0+2*(n), (((v) << 16) | (getgr32(FLR0+2*(n)) & 0xF000) | (((v) >> 16) & 0x1F)))

static inline unsigned short ldc(int n, unsigned short result) {
  unsigned int utempl;
  unsigned short m;
  unsigned int far, flr;
  ea_t ea;

  if (n) {
    far = FAR1;
    flr = FLR1;
    eap = &gv.brp[F1BR];
  } else {
    far = FAR0;
    flr = FLR0;
    eap = &gv.brp[F0BR];
  }

  utempl = GETFLR(n);
  if (utempl > 0) {
    ea = getgr32(far);
    m = get16(getgr32(far));
    if (getgr32(flr) & 0x8000) {
      result = m & 0xFF;
      putgr32(flr, getgr32(flr) & 0xFFFF0FFF);
      putgr32(far, (getgr32(far) & 0x6FFF0000) | ((getgr32(far)+1) & 0xFFFF)); \
      TRACE(T_FLOW, " ldc %d = '%o (%c) from %o/%o right\n", n, result, result&0x7f, ea>>16, ea&0xffff);
    } else {
      result = m >> 8;
      putgr32(flr, getgr32(flr) | 0x8000);  /* set bit offset */
      TRACE(T_FLOW, " ldc %d = '%o (%c) from %o/%o left\n", n, result, result&0x7f, ea>>16, ea&0xffff);
    }
    utempl--;
    PUTFLR(n,utempl);
    CLEAREQ;
  } else {                  /* utempl == 0 */
    TRACE(T_FLOW, " LDC %d limit\n", n);
    SETEQ;
  }
  return result;
}


static void inline stc(int n, unsigned short ch) {
  unsigned int utempl;
  unsigned short m;
  unsigned int far, flr;
  ea_t ea;

  if (n) {
    far = FAR1;
    flr = FLR1;
    eap = &gv.brp[F1BR];
  } else {
    far = FAR0;
    flr = FLR0;
    eap = &gv.brp[F0BR];
  }

  utempl = GETFLR(n);
  if (utempl > 0) {
    ea = getgr32(far);
    m = get16(getgr32(far));
    if (getgr32(flr) & 0x8000) {
      TRACE(T_FLOW, " stc %d =  '%o (%c) to %o/%o right\n", n, ch, ch&0x7f, ea>>16, ea&0xffff);
      m = (m & 0xFF00) | (ch & 0xFF);
      put16(m,getgr32(far));
      putgr32(flr, getgr32(flr) & 0xFFFF0FFF);
      putgr32(far, (getgr32(far) & 0x6FFF0000) | ((getgr32(far)+1) & 0xFFFF));
    } else {
      TRACE(T_FLOW, " stc %d = '%o (%c) to %o/%o left\n", n, ch, ch&0x7f, ea>>16, ea&0xffff);
      m = (ch << 8) | (m & 0xFF);
      put16(m,getgr32(far));
      putgr32(flr, getgr32(flr) | 0x8000);    /* set bit offset */
    }
    utempl--;
    PUTFLR(n,utempl);
    CLEAREQ;
  } else {                  /* utempl == 0 */
    TRACE(T_FLOW, " STC %d limit\n", n);
    SETEQ;
  }
}

/* add a bit offset, passed in "val", to field address register n */

static void inline arfa(int n, int val) {
  int utempl;

  TRACE(T_FLOW, " before add, FAR=%o/%o, FLR=%o\n", getgr32(FAR0+2*n)>>16, getgr32(FAR0+2*n)&0xFFFF, getgr32(FLR0+2*n));
  utempl = ((getgr32(FAR0+2*n) & 0xFFFF) << 4) | ((getgr32(FLR0+2*n) >> 12) & 0xF);
  utempl += val;
  putgr32(FAR0+2*n, (getgr32(FAR0+2*n) & 0xFFFF0000) | ((utempl >> 4) & 0xFFFF));
  putgr32(FLR0+2*n, (getgr32(FLR0+2*n) & 0xFFFF0FFF) | ((utempl & 0xF) << 12));
  TRACE(T_FLOW, " after add, FAR=%o/%o, FLR=%o\n", getgr32(FAR0+2*n)>>16, getgr32(FAR0+2*n)&0xFFFF, getgr32(FLR0+2*n));
}

/* inline function to extract shift count from an instruction */

static inline unsigned short shiftcount (short inst) {
  unsigned short scount;
  scount = -inst & 077;
  if (scount == 0)
    scount = 0100;
  return scount;
}

/* 32-bit shifts */

static inline unsigned int lrs(unsigned int val, unsigned short inst) {
  unsigned short scount;

  scount = shiftcount(inst);
  if (scount < 32) {
    EXPCL(val & (((unsigned int)0x80000000) >> (32-scount)));
    return (*(int *)&val) >> scount;
  } else if (val & 0x80000000) {
    SETCL;
    return 0xFFFFFFFF;
  } else
    CLEARCL;
    return 0;
}

static inline unsigned int lls(unsigned int val, unsigned short inst) {
  unsigned short scount;
  int templ;

  scount = shiftcount(inst);
  if (scount < 32) {
    templ = 0x80000000;
    templ = templ >> scount;         /* create mask */
    templ = templ & val;             /* grab bits */
    templ = templ >> (31-scount);    /* extend them */
    EXPCL(!(templ == -1 || templ == 0));
    return *(int *)&val << scount;
  } else {
    EXPCL(val != 0);
    return 0;
  }
}

static inline unsigned int lll(unsigned int val, unsigned short inst) {
  unsigned short scount;

  scount = shiftcount(inst);
  if (scount <= 32) {
    EXPCL(val & (((unsigned int)0x80000000) >> (scount-1)));
    return val << scount;
  } else
    CLEARCL;
    return 0;
}

static inline unsigned int lrl(unsigned int val, unsigned short inst) {
  unsigned short scount;

  scount = shiftcount(inst);
  if (scount <= 32) {
    EXPCL(val & (((unsigned int)0x80000000) >> (32-scount)));
    return val >> scount;
  } else
    CLEARCL;
    return 0;
}

/* 16-bit shifts */

static inline unsigned short arl (unsigned short val, unsigned short inst) {
  unsigned short scount;

  scount = shiftcount(inst);
  if (scount <= 16) {
    EXPCL(val & (((unsigned short)0x8000) >> (16-scount)));
    return val >> scount;
  } else {
    CLEARCL;
    return 0;
  }
}

static inline unsigned short all (unsigned short val, unsigned short inst) {
  unsigned short scount;

  scount = shiftcount(inst);
  if (scount <= 16) {
    EXPCL(val & (((unsigned short)0x8000) >> (scount-1)));
    return val << scount;
  } else {
    CLEARCL;
    return 0;
  }
}

static inline unsigned short als (unsigned short val, unsigned short inst) {
  unsigned short scount;
  short tempa;

  scount = shiftcount(inst);
  if (scount <= 15) {
    tempa = 0100000;
    tempa = tempa >> scount;         /* create mask */
    tempa = tempa & val;             /* grab bits */
    tempa = tempa >> (15-scount);    /* extend them */
    EXPCL(!(tempa == -1 || tempa == 0));
    return val << scount;
  }
  EXPCL(val != 0);
  return 0;
}

static inline unsigned short ars (unsigned short val, unsigned short inst) {
  unsigned short scount;

  scount = shiftcount(inst);
  if (scount <= 16) {
    EXPCL(val & (((unsigned short)0x8000) >> (16-scount)));
    return (*(short *)&val) >> scount;
  } else if (val & 0x8000) {
    SETCL;
    return 0xFFFF;
  } else
    CLEARCL;
    return 0;
}

/* 32-bit rotates */

static inline unsigned int lrr(unsigned int val, unsigned short inst) {
  unsigned short scount;

  scount = shiftcount(inst);
  scount = ((scount-1)%32)+1;         /* make scount 1-32 */
  EXPCL(val & (((unsigned int)0x80000000) >> (32-scount)));
  return (val >> scount) | (val << (32-scount));
}

static inline unsigned int llr(unsigned int val, unsigned short inst) {
  unsigned short scount;

  scount = shiftcount(inst);
  scount = ((scount-1)%32)+1;         /* make scount 1-32 */
  EXPCL(val & (((unsigned int)0x80000000) >> (scount-1)));
  return (val << scount) | (val >> (32-scount));
}

/* 16-bit rotates */

static inline unsigned int alr(unsigned short val, unsigned short inst) {
  unsigned short scount;

  scount = shiftcount(inst);
  scount = ((scount-1)%16)+1;         /* make scount 1-16 */
  EXPCL(val & (((unsigned short)0x8000) >> (scount-1)));
  return (val << scount) | (val >> (16-scount));
}

static inline unsigned int arr(unsigned short val, unsigned short inst) {
  unsigned short scount;

  scount = shiftcount(inst);
  scount = ((scount-1)%16)+1;         /* make scount 1-16 */
  EXPCL(val & (((unsigned short)0x8000) >> (16-scount)));
  return (val >> scount) | (val << (16-scount));
}

/* math functions */

static inline unsigned int tcr(unsigned int un) {

  unsigned int utempl;

  utempl = -(int)un;
  if (utempl != 0) {    /* clear C, L, EQ, set LT from bit 1 */
    putcrs16(KEYS, (getcrs16(KEYS) & ~0120300) | ((utempl & 0x80000000) >> 24));
    if (utempl == 0x80000000) {
      CLEARLT;
      mathexception('i', FC_INT_OFLOW, 0);
    }
  } else
    putcrs16(KEYS, (getcrs16(KEYS) & ~0120300) | 020100);  /* set L, EQ */
  return utempl;
}

static inline unsigned short tch (unsigned short un) {

  unsigned short utemp;

  utemp = -(short)un;
  if (utemp != 0) {    /* clear C, L, EQ, set LT from bit 1 */
    putcrs16(KEYS, (getcrs16(KEYS) & ~0120300) | ((utemp & 0x8000) >> 8));
    if (utemp == 0x8000) {
      CLEARLT;
      mathexception('i', FC_INT_OFLOW, 0);
    }
  } else
    putcrs16(KEYS, (getcrs16(KEYS) & ~0120300) | 020100);  /* set L, EQ */
  return utemp;
}

/* NOTE: ea is only used to set faddr should an arithmetic exception occur */

static int add32(unsigned int a1, unsigned int a2, unsigned int a3, ea_t ea) {

  unsigned int retval;
  unsigned int uresult;
  unsigned long long utemp;
  short link, eq, lt;

  putcrs16(KEYS, getcrs16(KEYS) & ~0120300);
  link = eq = lt = 0;
  utemp = a1;                              /* expand to higher precision */
  utemp += a2;                             /* double-precision add */
  utemp += a3;                             /* again, for subtract */
  uresult = utemp;                         /* truncate result to result size */
  retval = uresult;
  if (utemp & 0x100000000LL)               /* set L-bit if carry occurred */
    link = 020000;  
  if (uresult == 0)                        /* set EQ? */
    eq = 0100; 
  if (((~a1 ^ a2) & (a1 ^ uresult) & 0x80000000) == 0) {
    if (*(int *)&uresult < 0)
      lt = 0200;
    putcrs16(KEYS, getcrs16(KEYS) | link | eq | lt);
  } else {
    if (*(int *)&uresult >= 0)
      lt = 0200;
    putcrs16(KEYS, getcrs16(KEYS) | link | eq | lt);
    mathexception('i', FC_INT_OFLOW, 0);
  }
  return retval;
}

static int add16(unsigned short a1, unsigned short a2, unsigned short a3, ea_t ea) {

  unsigned short retval;
  unsigned int uresult;
  int keybits, oflow;

  uresult = a1;                            /* expand to higher precision */
  uresult += a2;                           /* double-precision add */
  uresult += a3;                           /* again, for subtract */
  keybits = (uresult & 0x10000) >> 3;      /* set L-bit if carry occurred */  
  uresult &= 0xFFFF;                       /* truncate result */
  retval = uresult;                        /* save result */
  if (uresult == 0)                        /* set EQ? */
    keybits |= 0100; 
  oflow = (((~a1 ^ a2) & (a1 ^ uresult) & 0x8000) != 0); /* overflow! */
  if (oflow)
    uresult = ~uresult;
  keybits |= (uresult & 0x8000) >> 8;      /* set LT if result negative */
  putcrs16(KEYS, getcrs16(KEYS) & ~0120300 | keybits);
  if (oflow)
    mathexception('i', FC_INT_OFLOW, ea);
  return retval;
}


static void inline adlr(int dr) {

  if (getcrs16(KEYS) & 020000)
    putgr32(dr, add32(getgr32(dr), 1, 0, 0));
  else {
    putcrs16(KEYS, getcrs16(KEYS) & ~0120300);    /* clear C, L, LT, EQ */
    SETCC_32(getgr32(dr));
  }
}


static void inline cgt(unsigned short n) {
  unsigned short utempa;

  utempa = iget16(RP);              /* get number of words */
  if (1 <= n && n < utempa)
    RPL = iget16(RPADD(n));
  else
    RP = RPADD(utempa);
}


static void inline pimh(int dr) {
  int templ, templ2;

  templ = getgr32(dr);
  /* NOTE: PIMH could be implemented as a left shift, but Prime DIAG
     tests require a swap - hence the "or" below */
  putgr32(dr, (getgr32(dr) << 16) | (getgr32(dr) >> 16));
  /* check that bits 1-16 were equal to bit 17 before PIMH */
  templ2 = (templ << 16) >> 16;
  if (templ2 == templ)
    CLEARC;
  else
    mathexception('i', FC_INT_OFLOW, 0);
}

/* NOTE: PMA manuals say the range for absolute RF addressing is
   0-'377, but this does not allow addressing a machine with 8 user
   register sets.  The range should probably be an emulator config
   variable, based on the cpuid */

static int ldar(ea_t ea) {
  unsigned int result;

  if (ea & 040000) {       /* absolute RF addressing */
    RESTRICT();

    if ((ea & 0777) > 0477) {
      printf("em: LDLR ea '%o is out of range for this CPU model\n", ea);
      fatal(NULL);
    }
    ea &= 0777;
    if (ea == 020)
      result = 1;
    else if (ea == 024)
      result = -1;
    else if (ea == 7)
      result = RP;
    else
      result = getar32(ea);
  } else {
    ea &= 037;
    if (ea > 017) RESTRICT();
    result = getgr32(ea);
  }
  return result;
}


static void star(unsigned int val32, ea_t ea) {

  if (ea & 040000) {       /* absolute RF addressing */
    RESTRICT();
    if ((ea & 0777) > 0477) {
      printf("em: STLR ea '%o is out of range for this cpu model.\nThis -cpuid may not be supported by this version of software\nTry a lower -cpuid", ea);
      fatal(NULL);
    }
    ea &= 0777;
    putar32(ea, val32);
    if (ea == 7)
      RP = val32;
  } else {
    ea &= 037;
    if (ea > 017) RESTRICT();
    putgr32(ea, val32);
  }
}


/* here for PIO instructions: OCP, SKS, INA, OTA.  The instruction
   word is passed in as an argument to handle EIO (Execute I/O) in
   V/I modes. */

static void pio(unsigned int inst) {
  int class;
  int func;
  int device;

  RESTRICT();
  class = inst >> 14;
  func = (inst >> 6) & 017;
  device = inst & 077;
  TRACE(T_FLOW, " pio, class=%d, func='%o, device='%o\n", class, func, device);
  devmap[device](class, func, device);
}

int main (int argc, char **argv) {

  static short bootdiskctrl[4] = {026, 027, 022, 023};

  char *bootarg;                       /* argument to -boot, if any */
  char bootfile[16];                   /* boot file to load (optional) */
  int bootfd=-1;                       /* initial boot file fd */
  int lockfd=-1;                       /* fd of /tmp/em.lck lock file */
  int bootctrl, bootunit;              /* boot device controller and unit */
  int bootskip=0;                      /* skip this many bytes on boot dev */
  unsigned char boottaphdr[4];

  short tempa,tempa1,tempa2;
  unsigned short utempa;
  int templ,templ1,templ2;
  long long templl,templl1,templl2;
  unsigned long long utempll;
  unsigned int utempl,utempl1,utempl2;
  double tempd,tempd1,tempd2;
  ea_t tempea;
  ea_t ea;                             /* final MR effective address */
  ea_t earp;                           /* RP to use for eff address calcs */
  pa_t pa;
  int brop;
  int dr;
  unsigned short eabit;
  unsigned short opcode, opix;
  short i,j,x;
  unsigned short savemask;
  int nw,nw2;
  unsigned short rvec[9];    /* SA, EA, P, A, B, X, keys, dummy, dummy */
  unsigned short inst;
  unsigned short m;
  short scount;                          /* shift count */
  unsigned short trapvalue;
  ea_t trapaddr;
  unsigned short access;
  unsigned int immu32;
  unsigned long long immu64;
  unsigned short zresult, zclen1, zclen2, zaccess;
  unsigned int zlen1, zlen2;
  ea_t zea1, zea2;
  unsigned char zch1, zch2, *zcp1, *zcp2, zspace;
  unsigned char xsc, xfc, xsign, xsig;

  /* Prime ASCII constants for decimal instructions */

#define XPLUS 0253
#define XMINUS 0255
#define XZERO 0260
#define XONE 0261
#define XJ 0312
#define XRBRACE 0375

  struct timeval boot_tv;
  struct timezone tz;

  printf("[Prime Emulator ver %s %s]\n", REV, __DATE__);
  printf("[Copyright (C) 2005-2019 Jim Wilcoxson prirun@gmail.com]\n");
  if (argc > 1 && (strcmp(argv[1],"--version") == 0)) {
    exit(0);
  }

  /* don't let 2 emulators run in the same directory */

  lockfd = open(".lock", O_WRONLY+O_CREAT, 0644);
  if (lockfd == -1) {
    fprintf(stderr, "Error opening .lock: %s\n", strerror(errno));
    exit(1);
  }
  if (flock(lockfd, LOCK_EX+LOCK_NB) == -1) {
    fprintf(stderr, "Already running\n");
    exit(1);
  }

  /* re-open stderr as error.log */

  close(2);
  if ((templ = open("error.log", O_WRONLY+O_CREAT+O_TRUNC, 0644)) != 2) {
    if (templ == -1)
      printf("Error redirecting stderr to error.log: %s\n", strerror(errno));
    else
      printf("Open returned %d redirecting stderr\n", templ);
    exit(1);
  }
  setvbuf(stderr, NULL, _IONBF, 0);

  /* initialize global variables */

  gv.intvec = -1;
  gv.instcount = 0;
  gv.inhcount = 0;
  gv.instpermsec = 2000;
  gv.livereglim = 040;
  gv.mapvacalls = 0;
  gv.mapvamisses = 0;
  gv.supercalls = 0;
  gv.supermisses = 0;
#ifndef NOTRACE
  gv.traceflags = 0;
  gv.savetraceflags = 0;
  gv.traceuser = 0;
  gv.traceseg = 0;
  gv.numtraceprocs = 0;
  gv.traceinstcount = 0;
  gv.tracetriggered = 1;
  gv.tracerpqx = 0;
  for (i=0; i<MAXRPQ; i++)
    gv.tracerpq[i] = 0;
#endif
  invalidate_brp();
  eap = &gv.brp[0];

  /* ignore SIGPIPE signals (sockets) or they'll kill the emulator */

  signal (SIGPIPE, SIG_IGN);

  /* on SIGTERM, shutdown the emulator with a Prime sensor check */

  signal (SIGTERM, sensorcheck);
  signal (SIGQUIT, sigquit);

#ifndef NOTRACE

  /* open trace log */

  if ((gv.tracefile=fopen("trace.log", "w")) == NULL) {
    perror("Unable to open trace.log");
    exit(1);
  }
#endif

  /* initialize dispatch tables */

#include "dispatch.h"

  domemdump = 0;
  bootarg = NULL;
  bootfile[0] = 0;
  gv.pmap32bits = 0;
  gv.csoffset = 0;
  gv.memlimit = MEMSIZE;
  tport = 0;
  nport = 0;

  /* check args */

  for (i=1; i<argc; i++) {

    if ((strcmp(argv[i],"-map") == 0) || (strcmp(argv[i],"-maps") == 0)) {
      while (i+1 < argc && argv[i+1][0] != '-')
	readloadmap(argv[++i], 1);

    } else if (strcmp(argv[i],"-memdump") == 0) {
      domemdump = 1;

    } else if (strcmp(argv[i],"-ss") == 0) {
      if (i+1 < argc && argv[i+1][0] != '-') {
	sscanf(argv[++i],"%o", &templ);
	sswitch = templ;
      } else
	fatal("-ss needs an octal sense switch setting\n");

    } else if (strcmp(argv[i],"-ds") == 0) {
      if (i+1 < argc && argv[i+1][0] != '-') {
	sscanf(argv[++i],"%o", &templ);
	dswitch = templ;
      } else
	fatal("-ds needs an octal data switch setting\n");

    } else if (strcmp(argv[i],"-boot") == 0) {
      if (i+1 < argc && argv[i+1][0] != '-') {
	if (strcmp(argv[i+1],"help") == 0) {
	  sswitch = 0;
	  i++;
	} else {
	  if (sscanf(argv[i+1],"%o", &templ) == 0) {
	    bootarg = argv[++i];
	    sswitch = 0;
	  }
	  if (i+1 < argc && argv[i+1][0] != '-' && sscanf(argv[i+1],"%o", &templ) == 1) {
	    i++;
	    sswitch = templ;
	  }
	  if (i+1 < argc && argv[i+1][0] != '-' && sscanf(argv[i+1],"%o", &templ) == 1) {
	    i++;
	    dswitch = templ;
	  }
	  if (i+1 < argc && strcmp(argv[i+1],"regs") == 0) {
	    i++;
	    bootregs = 1;
	  }
	}
      }

    } else if (strcmp(argv[i],"-cpuid") == 0) {
      if (i+1 < argc && argv[i+1][0] != '-') {
	sscanf(argv[++i],"%d", &templ);
	if (0 <= templ && templ <= 44)
	  cpuid = templ;
	else {
	  cpuid = 9999;
	  for (j=0; cputab[j].cpumodel; j++) {
	    if (templ == cputab[j].cpumodel) {
	      cpuid = cputab[j].cputype;
	      break;
	    }
	  }
	  if (cpuid > 44)
	    fatal("-cpuid arg range is 0 to 44 or model name: 400, 550, 9950, etc\n");
	}
      } else
	fatal("-cpuid needs an argument\n");

    } else if (strcmp(argv[i],"-mem") == 0) {
      if (i+1 < argc && argv[i+1][0] != '-') {
	sscanf(argv[++i],"%d", &templ);
	if (1 <= templ && templ <= 512)
	  gv.memlimit = templ*1024/2*1024;
	else
	  fatal("-mem arg range is 1 to 512 (megabytes)\n");
      } else
	fatal("-mem needs an argument\n");

    } else if (strcmp(argv[i],"-nport") == 0) {
      if (i+1 < argc && argv[i+1][0] != '-') {
	sscanf(argv[++i],"%d", &templ);
	nport = templ;
      } else
	fatal("-nport needs an argument\n");

    } else if (strcmp(argv[i],"-naddr") == 0) {
      if (i+1 < argc && argv[i+1][0] != '-') {
	inet_aton(argv[++i], &bindaddr);
      } else
	fatal("-naddr needs an argument\n");

    } else if (strcmp(argv[i],"-tport") == 0) {
      if (i+1 < argc && argv[i+1][0] != '-') {
	sscanf(argv[++i],"%d", &templ);
	tport = templ;
      } else
	fatal("-tport needs an argument\n");

#ifndef NOTRACE
    } else if (strcmp(argv[i],"-trace") == 0) {
      while (i+1 < argc && argv[i+1][0] != '-') {
	i++;
	if (strcmp(argv[i],"ear") == 0)
	  gv.traceflags |= T_EAR;
	else if (strcmp(argv[i],"eav") == 0)
	  gv.traceflags |= T_EAV;
	else if (strcmp(argv[i],"eai") == 0)
	  gv.traceflags |= T_EAI;
	else if (strcmp(argv[i],"eas") == 0)
	  gv.traceflags |= T_EAS;
	else if (strcmp(argv[i],"inst") == 0)
	  gv.traceflags |= T_INST;
	else if (strcmp(argv[i],"flow") == 0)
	  gv.traceflags |= T_FLOW;
	else if (strcmp(argv[i],"mode") == 0)
	  gv.traceflags |= T_MODE;
	else if (strcmp(argv[i],"eaap") == 0)
	  gv.traceflags |= T_EAAP;
	else if (strcmp(argv[i],"dio") == 0)
	  gv.traceflags |= T_DIO;
	else if (strcmp(argv[i],"map") == 0)
	  gv.traceflags |= T_MAP;
	else if (strcmp(argv[i],"pcl") == 0)
	  gv.traceflags |= T_PCL;
	else if (strcmp(argv[i],"fault") == 0)
	  gv.traceflags |= T_FAULT;
	else if (strcmp(argv[i],"px") == 0)
	  gv.traceflags |= T_PX;
	else if (strcmp(argv[i],"tio") == 0)
	  gv.traceflags |= T_TIO;
	else if (strcmp(argv[i],"term") == 0)
	  gv.traceflags |= T_TERM;
	else if (strcmp(argv[i],"rio") == 0)
	  gv.traceflags |= T_RIO;
	else if (strcmp(argv[i],"off") == 0)
	  gv.tracetriggered = 0;
	else if (strcmp(argv[i],"lm") == 0)
	  gv.traceflags |= T_LM;
	else if (strcmp(argv[i],"put") == 0)
	  gv.traceflags |= T_PUT;
	else if (strcmp(argv[i],"get") == 0)
	  gv.traceflags |= T_GET;
	else if (strcmp(argv[i],"all") == 0)
	  gv.traceflags = ~0;
	else if (strcmp(argv[i],"flush") == 0)
	  setlinebuf(gv.tracefile);
	else if (strcmp(argv[i],"tlb") == 0)
	  gv.traceflags |= T_TLB;
	else if (isdigit(argv[i][0]) && strlen(argv[i]) <= 3 && sscanf(argv[i],"%d", &templ) == 1)
	  gv.traceuser = 0100000 | (templ<<6);   /* form OWNERL for user # */
	else if (isdigit(argv[i][0]) && sscanf(argv[i],"%d", &templ) == 1)
	  gv.traceinstcount = templ;
	else if (strlen(argv[i]) == 6 && sscanf(argv[i],"%o", &templ) == 1)
	  gv.traceuser = templ;                  /* specify OWNERL directly */
	else if (strlen(argv[i]) == 4 && sscanf(argv[i],"%o", &templ) == 1)
	  gv.traceseg = templ;                   /* specify RPH segno */
	else if (strlen(argv[i]) <= 8 && argv[i][0] != '-') {
	  if (gv.numtraceprocs >= MAXTRACEPROCS)
	    fprintf(stderr,"Only %d trace procs are allowed\n", MAXTRACEPROCS);
	  else {
	    printf("Request to trace proc %s\n", argv[i]);
	    traceprocs[gv.numtraceprocs].oneshot = 1;
	    for (j=0; argv[i][j]; j++)
	      if (argv[i][j] == '+')
		traceprocs[gv.numtraceprocs].oneshot = 0;
	      else
		traceprocs[gv.numtraceprocs].name[j] = argv[i][j];
	    traceprocs[gv.numtraceprocs].name[j] = 0;
	    traceprocs[gv.numtraceprocs].sb = -1;
	    traceprocs[gv.numtraceprocs].ecb = 0;
	    gv.numtraceprocs++;
	  }
	} else {
	  fprintf(stderr,"Unrecognized trace flag: %s\n", argv[i]);
	  printf("Unrecognized trace flag: %s\n", argv[i]);
	}
      }
#endif

    } else {
      printf("Unrecognized argument: %s\n", argv[i]);
      fatal(NULL);
    }
  }

  TRACEA("Boot sense switches=%06o, data switches=%06o\n", sswitch, dswitch);

  /* master clear:
     - clear all registers
     - user register set is 0
     - modals:
     -- interrupts inhibited
     -- standard interrupt mode
     -- user register set is 0
     -- non-mapped I/O
     -- process exchange disabled
     -- segmentation disabled
     -- machine checks disabled
     - keys:
     -- C, L, LT, EQ clear
     -- single precision
     -- 16S mode
     -- take fault on FP exception
     -- no fault on integer or decimal exception
     -- characters have high bit on
     -- FP rounding disabled
     -- not in dispatcher
     -- register set is not saved
     - set P to '1000
     - all stlb entries are invalid
     - all iotlb entries are invalid
     - clear 64K words of memory
  */

  for (i=0; i < 32*REGSETS; i++)
    regs.u32[i] = 0;

  crsl = (void *)regs.sym.userregs[0]; /* first user register set */
  
  putcrs16(MODALS, 0);                /* interrupts inhibited */
  newkeys(0);
  RP = 01000;
  for (i=0; i < STLBENTS; i++)
    gv.stlb[i].seg = 0xFFFF;        /* marker for invalid STLB entry */
  for (i=0; i < IOTLBENTS; i++)
    gv.iotlb[i].valid = 0;
  physmem = malloc(gv.memlimit * sizeof(*physmem));
  if (physmem == NULL) {
    perror("Error allocating Prime memory block");
    fatal("Can't allocate Prime memory block");
  }
  bzero(MEM, 64*1024*2);              /* zero first 64K words */
  
  /* if no maps were specified on the command line, look for ring0.map and 
     ring3.map in the current directory and read them */

  if (numsyms == 0) {
    readloadmap("ring0.map", 0);
    readloadmap("ring3.map", 0);
  }

  /* finish setting up tracing after all options are read, ie, maps */

#ifndef NOTRACE
  TRACEA("Trace flags = 0x%x\n", gv.traceflags);
  gv.savetraceflags = gv.traceflags;
  gv.traceflags = 0;
  if (gv.traceuser != 0)
    TRACEA("Tracing enabled for OWNERL %o\n", gv.traceuser);
  else
    TRACEA("Tracing enabled for all users\n");
  if (gv.traceinstcount != 0)
    TRACEA("Tracing enabled after instruction %u\n", gv.traceinstcount);
  for (i=0; i<gv.numtraceprocs; i++) {
    for (j=0; j<numsyms; j++) {
      if (strcasecmp(mapsym[j].symname, traceprocs[i].name) == 0 && mapsym[j].symtype == 'e') {
	ea = mapsym[j].address;
	traceprocs[i].ecb = ea;
	TRACEA("Tracing procedure %s ecb ea '%o/%o\n", traceprocs[i].name, SEGNO32(ea), ea&0xFFFF);
	printf("Tracing procedure %s ecb ea '%o/%o\n", traceprocs[i].name, SEGNO32(ea), ea&0xFFFF);
	break;
      }
    }
    if (j == numsyms) {
      fprintf(stderr,"Can't find procedure %s in load maps for tracing.\n", traceprocs[i].name);
      printf("Can't find procedure %s in load maps for tracing.\n", traceprocs[i].name);
    }
  }
#endif

  /* set some vars after the options have been read */

  if (cpuid == 15 || cpuid == 18 || cpuid == 19 || cpuid == 24 || cpuid >= 26) {
    gv.pmap32bits = 1;
    if (cpuid == 33 || cpuid == 37 || cpuid == 39 || cpuid >= 43)
      gv.pmap32mask = 0x3FFFF;    /* this is for 512MB physical memory */
    else
      gv.pmap32mask = 0xFFFF;     /* this is for 128MB physical memory */
  }
  if ((26 <= cpuid && cpuid <= 29) || cpuid >= 35)
    gv.csoffset = 1;

  /* initialize all devices */

  for (i=0; i<64; i++) {
    if (devmap[i](-1, 0, i)) {   /* if initialization fails, */
      devmap[i] = devnone;       /* remove device */
      fprintf(stderr, "emulator: device '%o failed initialization - device removed\n", i);
    }
  }


  /* if a filename follows -boot, load and execute this R-mode runfile
     image.  For example, -boot *DOS64 would load *DOS64 from the Unix
     file system and begin executing Primos II.

     SECURITY: check that boot filename isn't a pathname?
  */

  setjmp(bootjmp);

  if (bootarg) {
    if ((bootfd=open(bootarg, O_RDONLY)) == -1) {
      perror("Error opening boot file");
      fatal(NULL);
    }
    if (read(bootfd, rvec, 18) != 18) {
      perror("Error reading boot file's rvec header");
      fatal(NULL);
    }
    for (i=0; i<9; i++)
      rvec[i] = swap16(rvec[i]);

  } else {

    /* If no filename follows -boot, then the sense switches are used to
       determine whether the boot record should be read from tape or disk
       and select the controller and drive unit.

       Bits 14-16 are 4 for disk boot, 5 for tape boot
       Bit 13 is 1 for disk boot, don't care for tape boot
       Bits 11-12 are the unit number, 0-4
    */

    bootunit = (sswitch>>7) & 3;
    rvec[2] = 01000;                  /* program start address */
    rvec[3] = rvec[4] = rvec[5] = rvec[6] = 0;

    if ((sswitch & 0x7) == 4) {         /* disk boot */
      bootctrl = bootdiskctrl[(sswitch>>4) & 3];
      rvec[0] = 0760;                   /* disk load starts at '760 */
      rvec[1] = rvec[0]+1040-1;         /* read 1 disk block */
      /* setup DMA register '20 (address only) for the next boot record */
      putar16(REGDMX16 + 041, 03000);
      if (globdisk(bootfile, sizeof(bootfile), bootctrl, bootunit) != 0) {
	printf("Can't find disk boot device file %s, or multiple files match this device.\nTo boot from tape (mt0), use -boot 10005", bootfile);
	fatal(NULL);
      }

    } else if ((sswitch & 0x7) == 5) {  /* tape boot */
      bootctrl = 014;
      rvec[0] = 0200;                   /* tape load starts at '200 */
      bootskip = 4;                     /* to skip trailing .TAP header */
      snprintf(bootfile, sizeof(bootfile), "mt%d", bootunit);

    } else {
      printf("\
\n\
The -boot option is used to boot from disk, tape, or to load a Prime\n\
runfile directly from the Unix file system.  For example:\n\
\n\
  -boot  14xx4 to boot from disk (see below)\n\
  -boot  10005 to boot from tape.\n\
  -boot *DOS64 to load *DOS64 from the Unix file and execute it\n\
  -boot 1xxxxx to force Primos to prompt for COMDEV, PAGDEV, & NTUSR\n\
\n\
For disk boots, the last 3 digits can be:\n\
\n\
  114 = disk26u0 ctrl '26 unit 0       154 = disk22u0 ctrl '22 unit 0\n\
  314 = disk26u1 ctrl '26 unit 1       354 = disk22u1 ctrl '22 unit 1\n\
  514 = disk26u2 ctrl '26 unit 2       554 = disk22u2 ctrl '22 unit 2\n\
  714 = disk26u3 ctrl '26 unit 3       754 = disk22u3 ctrl '22 unit 3\n\
\n\
  134 = disk27u0 ctrl '27 unit 0       174 = disk23u0 ctrl '23 unit 0\n\
  334 = disk27u1 ctrl '27 unit 1       374 = disk23u1 ctrl '23 unit 1\n\
  534 = disk27u2 ctrl '27 unit 2       574 = disk23u2 ctrl '23 unit 2\n\
  734 = disk27u3 ctrl '27 unit 3       774 = disk23u3 ctrl '23 unit 3\n\
\n\
  The default option is -boot 14114, to boot from disk disk26u0\n\
\n\
If two octal values follow -boot, the first is the sense switch\n\
setting for switches flipped up, and the second is the data switch\n\
setting for switches pushed down.  These two values may follow the\n\
filename if booting from a file.  If the \"regs\" option is used with\n\
a filename, CPU registers and keys are loaded from the runfile header.\n\
");
      exit(1);
    }

    printf("Booting from file %s\n", bootfile);
    if ((bootfd=open(bootfile, O_RDONLY)) == -1) {
      perror("Error opening boot device file");
      fatal(NULL);
    }

    /* for tape boots, read the .tap header and setup the DMA registers
       as they would be following a tape device read */

    if ((sswitch & 0x7) == 5) {  /* tape boot */
      if ((read(bootfd, boottaphdr, 4)) != 4) {
	perror("Error reading boot tape header");
	fatal(NULL);
      }
      nw = (boottaphdr[0] | boottaphdr[1]<<8 | boottaphdr[2]<<16 | boottaphdr[3]<<24)/2;
      rvec[1] = rvec[0]+nw-1;
      /* setup DMA register '20 (address only) for the next boot record */
      putar16(REGDMX16 + 041, 0200+nw);
    }
  }
  TRACEA("Sense switches set to %o\n", sswitch);
  TRACEA("Boot SA=%o, EA=%o, P=%o, A=%o, B=%o, X=%o, K=%o\n\n", rvec[0], rvec[1], rvec[2], rvec[3], rvec[4], rvec[5], rvec[6]);
  if (rvec[2] > rvec[1])
    fatal("Program start > ending: boot image is corrupt");

  /* read memory image from SA to EA inclusive */

  nw = rvec[1]-rvec[0]+1;
  if ((nw2=read(bootfd, MEM+rvec[0], nw*2)) == -1) {
    perror("Error reading memory image");
    fatal(NULL);
  }
  if (lseek(bootfd, bootskip, SEEK_CUR) == -1) {
    perror("Error skipping on boot device");
    fatal(NULL);
  }

  close(bootfd);

  /* check we got it all */
  
  if (nw2 != nw*2) {
    printf("rvec[0]=%d, rvec[1]=%d, nw2=%d, nw=%d, nw*2=%d\n", rvec[0], rvec[1], nw2, nw, nw*2);
    fatal("Didn't read entire boot program");
  }

  /* setup execution (registers, keys, address mask, etc.) from rvec */

#if 1
  /* do a partial sysclr... move it here later if it fixes rev 19
     reboot from tape (ctrl-b) */
  
  putcrs16(MODALS, 0);                /* interrupts inhibited */
  for (i=0; i < STLBENTS; i++)
    gv.stlb[i].seg = 0xFFFF;        /* marker for invalid STLB entry */
  for (i=0; i < IOTLBENTS; i++)
    gv.iotlb[i].valid = 0;
#endif

  if (bootregs) {
    putcrs16(A, rvec[3]);
    putcrs16(B, rvec[4]);
    putcrs16(X, rvec[5]);
    newkeys(rvec[6]);
  }
  RPL = rvec[2];

  /* initialize the timer stuff */

  if (gettimeofday(&boot_tv, &tz) != 0) {
    perror("gettimeofday failed");
    fatal(NULL);
  }

  /* main instruction decode loop */

  grp = RP;             /* see similar assignments in fault, before longjmp */
  gcrsl = crsl;

  /* faults longjmp back here: the top of the instruction fetch loop.
     Stop the fault timer and restore dedicated registers trashed by longjmp */

  if (setjmp(jmpbuf)) {              /* returns 1 on longjmp */
    crsl = gcrsl;
    RP = grp;
  }


fetch:
  gv.instcount++;

#if 0
  /* check for memory overwrite in prmnt1 T&M: OTA '1507 w/packet length 3712*2 :( */
  utempa = get16(0124);
  if (utempa != 014011) {
    printf("[] = %o\n", utempa);
    fatal("Mem overwrite!");
  }
#endif

#ifndef NOTRACE
  gv.traceflags = 0;
  if (gv.tracetriggered &&
      (gv.instcount >= gv.traceinstcount) &&
      (TRACEUSER && ((gv.traceseg == 0) || (gv.traceseg == (RPH & 0xFFF))))
      )
    gv.traceflags = gv.savetraceflags;
  gv.tracerpq[gv.tracerpqx] = RP;
  gv.tracerpqx = (gv.tracerpqx+1) & (MAXRPQ-1);
#endif

  /* hack to activate trace in 32I mode */

#if 0
  if ((getcrs16(KEYS) & 0016000) == 0010000)
    gv.traceflags = gv.savetraceflags;
  else
    gv.traceflags = 0;
#endif

#if 0
  /* NOTE: rev 23.4 halts at inst #75379065 with the error:
     "System Serial Number does not agree with this version of Primos."
     To track this down, turn on tracing just before this instruction. */

  if (75370000 < gv.instcount && gv.instcount < 75380000)
    gv.traceflags = ~T_MAP;
  else
    gv.traceflags = 0;
#endif

#if 0
  /* turn on tracing  near instruction #47704931 to debug I/O TLB error
     in rev 22.1 */

  if (gv.instcount > 47700000)
    gv.traceflags = ~0;
  else
    gv.traceflags = 0;
#endif

#if 0
  /* NOTE: doing something like this sometimes causes Primos to do a
	 controlled shutdown, flushing disk buffers, etc. */
  RPH = 07777;
#endif

#if 0
    /* trace AC$SET call not working

     NOTE: a 2-word range is needed for RPL because a procedure with
     arguments may start executing at the ARGT instruction (listed
     in the load map as procedure start), or at the instruction
     following ARGT (if PCL completes w/o faults) */

  if (TRACEUSER && SEGNO16(RPH) == 041 && 06200 <= RPL && RPL <= 06201) { /* ac$set */
    gv.savetraceflags = ~T_MAP;
    printf("enable trace, RPH=%o, RPL=%o\n", SEGNO16(RPH), RPL);
  }
  if (TRACEUSER && SEGNO16(RPH) == 013 && 044030 <= RPL && RPL <= 044031) { /* setrc$ */
    gv.savetraceflags = 0;
    printf("disable trace, RPH=%o, RPL=%o\n", SEGNO16(RPH), RPL);
  }
#endif

#if 0
  /* this is for FTN Generic 3 trace */
  if (SEGNO16(RPH) == 04000 && RPL >= 034750 && RPL <= 034760)
    gv.savetraceflags = ~T_MAP;
  else
    gv.savetraceflags = 0;
#endif

#if 0
  /* NOTE: this tends to cause a page fault loop if the location
     being monitored isn't wired */

  if (trapaddr != 0 && (getcrs16(OWNERL) & 0100000) && (getcrs16(MODALS) & 010)) {
    gv.traceflags = -1;
    printf("TRAP: at #%d\n", gv.instcount);
    utempa = get16(trapaddr);
    if (utempa != trapvalue) {
      printf("TRAP: at #%d, old value of %o/%o was %o; new value is %o\n", gv.instcount, trapaddr>>16, trapaddr&0xffff, trapvalue, utempa);
      trapvalue = utempa;
      printf("TRAP: new trap value is %o\n", trapvalue);
    }
  }
#endif

  /* bump instruction count & periodically do time-related activities.
     TIMERMASK affects the granularity of these events, and in general
     should get bigger as the emulator gets faster.  There's also the
     potential of TIMERL overflowing because the emulator is running
     too fast.  Prime used more bits in later machines.  The timermask
     should probably be dynamic, or we should use real timers to avoid
     instruction timing issues. */

#define TIMERMASK 0777   /* must be power of 2 - 1 */

  if ((gv.instcount & TIMERMASK) == 0) {
    for (i=0; i<64; i++)
      if (devpoll[i] && ((devpoll[i] -= (TIMERMASK+1)) <= 0)) {
	devpoll[i] = 0;
	devmap[i](4, 0, i);
      }

    /* bump the 1ms process timer; docs say to only bump this if px is
       enabled, but since it is nearly all the time in practice, it
       could be bumped all the time w/o checking the modals.  But this
       makes CPU.REGISTER case 4 fail. :( */

    if (getcrs16(MODALS) & 010) {
      putcrs16(TIMERL, getcrs16(TIMERL) + TIMERMASK + 1);
      if (getcrs16(TIMERL) > gv.instpermsec) {
	putcrs16(TIMERL, getcrs16(TIMERL) - gv.instpermsec);

	/* if 1ms resolution process timer overflows, set pcb abort flag */

	putcrs16(TIMERH, getcrs16(TIMERH) + 1);
	if (getcrs16(TIMERH) == 0) {
	  TRACE(T_PX,  "#%u: pcb %o timer overflow\n", gv.instcount, getcrs16(OWNERL));
	  ea = getcrs32ea(OWNER);
	  m = get16r0(ea+4) | 1;       /* set process abort flag */
	  put16r0(m, ea+4);
	}
      }
    }
  }

  /* is an interrupt pending, with interrupts enabled? */

  if (gv.intvec >= 0 && (getcrs16(MODALS) & 0100000)) {
    if (gv.inhcount == 0) {
      //printf("fetch: taking interrupt vector '%o, modals='%o\n", gv.intvec, getcrs16(MODALS));
      TRACE(T_FLOW, "\nfetch: taking interrupt vector '%o, modals='%o\n", gv.intvec, getcrs16(MODALS));
      putar32(PSWPB32, RP & 0x7FFFFFFF);
      regs.sym.pswkeys = crs[KEYS];              /* Prime->Prime: no byte swap */

      /* NOTE: this code doesn't match the description on page B-21 of
	 the Sys Arch Guide 2nd Ed. for Standard Interrupt Mode */

      if (getcrs16(MODALS) & 010) {              /* PX enabled */
	//gv.traceflags = ~T_MAP;
	newkeys(014000);
	RPH = 4;
	RPL = gv.intvec;

      } else if (getcrs16(MODALS) & 040000) {    /* vectored interrupt mode */
	m = get16(gv.intvec);
	if (m != 0) {
	  put16(RPL, m);
	  RP = m+1;
	} else {
	  printf("fetch: interrupt vector '%o = 0 in vectored interrupt mode\n", gv.intvec);
	  fatal(NULL);
	}

      } else {                              /* standard interrupt mode */
	m = get16(063);
	//printf("Standard mode interrupt vector loc = %o\n", m);
	//gv.traceflags = ~T_MAP;
	if (m != 0) {
	  put16(RPL, m);
	  RP = m+1;
	} else {
	  fatal("em: loc '63 = 0 when standard mode interrupt occurred");
	}
      }
      putcrs16(MODALS, getcrs16(MODALS) & 077777);   /* inhibit interrupts */
    } else
      gv.inhcount--;
  }

  gv.prevpc = RP;

#if 0
  /* NOTE: Rev 21 Sys Arch Guide, 2nd Ed, pg 3-32 says:

     "When bits 17 to 32 of the program counter contain a value within
     the ATR (address trap range) and the processor is reading an 
     instruction, an address trap always occurs.  The only exception
     to this is if the machine is operating in 32I mode."

     The line below is equivalent to:

     ea = RP
     if !i-mode
       if segmented and ealow < 010 or !segmented and ealow < 040
         set ea to trap

     NOTE 10/9/2007 (JW):
     iget16 has checks for RP < 0 to indicate an address trap, and all
     of the EA calculations can generate traps (and set the high-order
     bit) except 32I.  

     If it's a bad idea for RP to have the high-order bit set, then
     this code can be enabled, along with special handling in JMP,
     JSXB, JSY, JSX, and JST that is currently disabled.  I couldn't
     see that one way worked better than the other, except enabling
     this check on every instruction fetch is more overhead.  Setting
     the HO bit of RP does expose this to Primos when a fault occurs
     while executing code in registers.

     NOTE 2: clear A, JMP# 1, and instead of getting a program halt
     or restricted instruction signal raised, a pointer fault occurs
     because Primos can't handle 160000/000001 as the fault address.
     So maybe this code should be enabled (?), along with the 5 jump
     instructions.
  */

  inst = iget16(RP | ((RPL >= gv.livereglim || (getcrs16(KEYS) & 0016000) == 010000) ? 0 : 0x80000000));
#else
  inst = iget16(RP);
#endif

  INCRP;

  /* while a process is running, RP is the real program counter, PBH
     is the active procedure segment, and PBL is zero.  When a
     process stops running, RP is copied to PB.  When a process
     starts running again, PB is copied to RP.
     XXX: it'd be nice to remove this, if possible. */

  putcrs32(PB, RP & 0x7FFF0000);

  /* earp is used for RP-relative address calculations.  It is the
     same as RP, except during an xec instruction.  See d_xec */

  earp = RP;


xec:
  /* NOTE: don't trace JMP * instructions (used to test PX) */

#if 0
  if (inst == 03777)
    gv.traceflags = 0;
#endif

#if 0
  if (getcrs16(OWNERL) == 0100200 && inst == 001114 && gv.savetraceflags)
    gv.traceflags = ~0;
  else
    gv.traceflags = 0;
#endif

#if 1
  TRACE(T_FLOW, "\n			#%u [%s %o] IT=%d SB: %o/%o LB: %o/%o %s XB: %o/%o\n%o/%o: %o		A='%o/%u B='%o/%d L='%o/%d E='%o/%d X='%o/%d Y='%o/%d%s%s%s%s K=%o M=%o\n", gv.instcount, searchloadmap(getcrs32(OWNER),'x'), getcrs16(OWNERL), getcrs16s(TIMERH), getcrs16(SBH), getcrs16(SBL), getcrs16(LBH), getcrs16(LBL), searchloadmap(getcrs32(LBH),'l'), getcrs16(XBH), getcrs16(XBL), RPH, RPL-1, inst, getcrs16(A), getcrs16s(A), getcrs16(B), getcrs16s(B), getcrs32(L), getcrs32s(L), getcrs32(E), getcrs32s(E), getcrs16(X), getcrs16s(X), getcrs16(Y), getcrs16s(Y), (getcrs16(KEYS)&0100000)?" C":"", (getcrs16(KEYS)&020000)?" L":"", (getcrs16(KEYS)&0200)?" LT":"", (getcrs16(KEYS)&0100)?" EQ":"", getcrs16(KEYS), getcrs16(MODALS));
#else
  TRACE(T_FLOW, "\n			[%s %o] SB: %o/%o LB: %o/%o %s XB: %o/%o\n%o/%o: %o		A='%o/%u B='%o/%d L='%o/%d E='%o/%d X='%o/%d Y='%o/%d%s%s%s%s K=%o M=%o\n", searchloadmap(getcrs32(OWNER),'x'), getcrs16(OWNERL), getcrs16(SBH), getcrs16(SBL), getcrs16(LBH), getcrs16(LBL), searchloadmap(getcrs32(LBH),'l'), getcrs16(XBH), getcrs16(XBL), RPH, RPL-1, inst, getcrs16(A), getcrs16s(A), getcrs16(B), getcrs16s(B), getcrs32(L), getcrs32s(L), getcrs32(E), getcrs32s(E), getcrs16(X), getcrs16s(X), getcrs16(Y), getcrs16s(Y), (getcrs16(KEYS)&0100000)?" C":"", (getcrs16(KEYS)&020000)?" L":"", (getcrs16(KEYS)&0200)?" LT":"", (getcrs16(KEYS)&0100)?" EQ":"", getcrs16(KEYS), getcrs16(MODALS) & 0177437);
#endif

  /* begin instruction decode: generic? */

  if ((inst & 036000) == 0)
    goto *disp_gen[GENIX(inst)];

  /* get x bit and adjust opcode so that PMA manual opcode
     references can be used directly, ie, if the PMA manual says the
     opcode is '15 02, then 01502 can be used here.  If the PMA
     manual says the opcode is '11, then use 01100 (the XX extended
     opcode bits are zero) */

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

  TRACE(T_EAR|T_EAV, " op=%#05o, i=%o, x=%o, mode=%d\n", ((inst & 036000) != 032000) ? ((inst & 036000) >> 4) : ((inst & 076000) >> 4), inst & 0100000, ((inst & 036000) != 032000) ? (inst & 040000) : 0, (getcrs16(KEYS) & 016000) >> 10);

  if ((getcrs16(KEYS) & 016000) == 014000) {   /* 64V mode */
    ea = ea64v(inst, earp);
    TRACE(T_FLOW, " EA: %o/%o  %s\n",ea>>16, ea & 0xFFFF, searchloadmap(ea,' '));
    goto *(gv.disp_vmr[VMRINSTIX(inst)]);

  } else if ((getcrs16(KEYS) & 0016000) == 010000) {  /* E32I */
      goto imode;
    
  } else if ((inst & 036000) == 030000) { /* check for pio in S/R modes, */
    pio(inst);                            /* before calculating EA */
    goto fetch;

  } else if (getcrs16(KEYS) & 004000) {        /* 32R/64R */
    ea = ea32r64r(earp, inst);
    TRACE(T_FLOW, " EA: %o/%o  %s\n",ea>>16, ea & 0xFFFF, searchloadmap(ea,' '));
    goto *(gv.disp_rmr[RMRINSTIX(inst)]);

  } else if (getcrs16(KEYS) & 002000) {
    ea = ea32s(inst);
    TRACE(T_FLOW, " EA: %o/%o  %s\n",ea>>16, ea & 0xFFFF, searchloadmap(ea,' '));
    goto *(gv.disp_rmr[SMRINSTIX(inst)]);

  } else if ((getcrs16(KEYS) & 016000) == 0) {
    ea = ea16s(inst);
    TRACE(T_FLOW, " EA: %o/%o  %s\n",ea>>16, ea & 0xFFFF, searchloadmap(ea,' '));
    goto *(gv.disp_rmr[SMRINSTIX(inst)]);

  } else {
    printf("Bad CPU mode in EA calculation, keys = %o\n", getcrs16(KEYS));
    fatal(NULL);
  }


  /* generic instructions */

d_iab:  /* 000201 */
  TRACE(T_FLOW, " IAB\n");
  putgr32(GR2, (getgr32(GR2) << 16) | (getgr32(GR2) >> 16));
  goto fetch;

d_cgt:  /* 001314 */
  TRACE(T_FLOW, " CGT\n");
  cgt(getcrs16(A));
  goto fetch;

d_pida:  /* 000115 */
  TRACE(T_FLOW, " PIDA\n");
  putcrs32s(L, getcrs16s(A));
  goto fetch;

d_pidl:  /* 000305 */
  TRACE(T_FLOW, " PIDL\n");
  putcrs64s(L, getcrs32s(L));
  goto fetch;

  /* NOTE: PMA manual says copy B reg to A reg, but DIAG seems
     to indicate a swap */

d_pima: /* 000015 */
  TRACE(T_FLOW, " PIMA\n");
  pimh(GR2);
  goto fetch;

d_piml:  /* 000301 */
  TRACE(T_FLOW, " PIML\n");
  templ = getcrs32s(L);
  putcrs32s(L, getcrs32s(E));
  if (((templ ^ getcrs32s(E)) & 0x80000000) || (templ != 0 && templ != -1))
    mathexception('i', FC_INT_OFLOW, 0);
  else
    CLEARC;
  goto fetch;

  /* character/field instructions */

d_ldc0:  /* 001302 */
  TRACE(T_FLOW, " LDC 0\n");
  putcrs16(A, ldc(0, getcrs16(A)));
  goto fetch;

d_ldc1:  /* 001312 */
  TRACE(T_FLOW, " LDC 1\n");
  putcrs16(A, ldc(1, getcrs16(A)));
  goto fetch;

d_stc0: /* 001322 */
  TRACE(T_FLOW, " STC 0\n");
  stc(0, getcrs16(A));
  goto fetch;

d_stc1:  /* 001332 */
  TRACE(T_FLOW, " STC 1\n");
  stc(1, getcrs16(A));
  goto fetch;

d_eafa0:  /* 001300 */
  TRACE(T_FLOW, " EAFA 0\n");
  ea = apea(&eabit);
  putgr32(FAR0, ea & 0x6FFFFFFF);
  putgr32(FLR0, (getgr32(FLR0) & 0xFFFF0FFF) | (eabit << 12));
  TRACE(T_INST, " FAR0=%o/%o, eabit=%d, FLR=%x\n", getgr32(FAR0)>>16, getgr32(FAR0)&0xFFFF, eabit, getgr32(FLR0));
  goto fetch;

d_eafa1:  /* 001310 */
  TRACE(T_FLOW, " EAFA 1\n");
  ea = apea(&eabit);
  putgr32(FAR1, ea & 0x6FFFFFFF);
  putgr32(FLR1, (getgr32(FLR1) & 0xFFFF0FFF) | (eabit << 12));
  TRACE(T_INST, " FAR1=%o/%o, eabit=%d, FLR=%x\n", getgr32(FAR1)>>16, getgr32(FAR1)&0xFFFF, eabit, getgr32(FLR1));
  goto fetch;

d_alfa0:  /* 001301 */
  TRACE(T_FLOW, " ALFA 0\n");
  arfa(0, getcrs32s(L));
  goto fetch;

d_alfa1:  /* 001311 */
  TRACE(T_FLOW, " ALFA 1\n");
  arfa(1, getcrs32s(L));
  goto fetch;

d_lfli0:  /* 001303 */
  TRACE(T_FLOW, " LFLI 0\n");
  utempa = iget16(RP);
  PUTFLR(0,utempa);
  INCRP;
  TRACE(T_FLOW, " Load Field length with %d, FLR=%x, actual = %d\n", utempa, getgr32(FLR0), GETFLR(0));
  goto fetch;

d_lfli1:  /* 001313 */
  TRACE(T_FLOW, " LFLI 1\n");
  utempa = iget16(RP);
  INCRP;
  PUTFLR(1,utempa);
  TRACE(T_FLOW, " Load Field length with %d, FLR=%x, actual = %d\n", utempa, getgr32(FLR1), GETFLR(1));
  goto fetch;

d_stfa0:  /* 001320 */
  /* XXX change to inline proc */
  TRACE(T_FLOW, " STFA 0\n");
  ea = apea(NULL);
  utempl = getgr32(FAR0) & 0x6FFFFFFF;
  utempa = getgr32(FLR0) & 0xF000;
stfa:
  if (utempa != 0) {
    utempl = utempl | EXTMASK32;
    put16(utempa,INCVA(ea,2));
    TRACE(T_FLOW, " stored 3-word pointer %o/%o %o\n", utempl>>16, utempl&0xffff, utempa);
  } else {
    TRACE(T_FLOW, " stored 2-word pointer %o/%o\n", utempl>>16, utempl&0xffff);
  }
  put32(utempl,ea);
  goto fetch;

d_stfa1:  /* 001330 */
  TRACE(T_FLOW, " STFA 1\n");
  ea = apea(NULL);
  utempl = getgr32(FAR1) & 0x6FFFFFFF;
  utempa = getgr32(FLR1) & 0xF000;
  goto stfa;

d_tlfl0:  /* 001321 */
  TRACE(T_FLOW, " TLFL 0\n");
  PUTFLR(0,getcrs32(L));
  TRACE(T_FLOW, " Transfer %d to FLR0, FLR=%x, actual=%d\n", getcrs32(L), getgr32(FLR0), GETFLR(0));
  goto fetch;

d_tlfl1:  /* 001331 */
  TRACE(T_FLOW, " TLFL 1\n");
  PUTFLR(1,getcrs32(L));
  TRACE(T_FLOW, " Transfer %d to FLR1, FLR=%x, actual=%d\n", getcrs32(L), getgr32(FLR1), GETFLR(1));
  goto fetch;

d_tfll0:  /* 001323 */
  TRACE(T_FLOW, " TFLL 0\n");
  putcrs32(L, GETFLR(0));
  goto fetch;

d_tfll1:  /* 001333 */
  TRACE(T_FLOW, " TFLL 1\n");
  putcrs32(L, GETFLR(1));
  goto fetch;

d_prtn:  /* 000611 */
  TRACE(T_FLOW, " PRTN\n");
  prtn();

#ifndef NOTRACE

  /* if this PRTN is for a procedure being traced, disable
     tracing if one-shot is true */

  if (gv.numtraceprocs > 0 && TRACEUSER)
    for (i=0; i<gv.numtraceprocs; i++)
      if (getcrs32s(SB) == traceprocs[i].sb) {
	traceprocs[i].sb = -1;
	fflush(gv.tracefile);
	if (traceprocs[i].oneshot) {
	  printf("Disabled trace for %s at sb '%o/%o\n", traceprocs[i].name, getcrs16(SBH), getcrs16(SBL));
	  gv.savetraceflags = 0;
	}
	break;
      }
#endif
  goto fetch;

d_tka:  /* 001005 */
  TRACE(T_FLOW, " TKA\n");
  putcrs16(A, getcrs16(KEYS));
  goto fetch;

d_tak:  /* 001015 */
  TRACE(T_FLOW, " TAK\n");
  newkeys(getcrs16(A) & 0177774);
  goto fetch;

d_nop:  /* 000001 */
  TRACE(T_FLOW, " NOP\n");
  goto fetch;

d_rsav:  /* 000715 */
  TRACE(T_FLOW, " RSAV\n");
  ea = apea(NULL);
  eap = &gv.brp[SBBR];   /* registers almost always saved on the stack */
  j = 1;
  savemask = 0;
  for (i = 11; i >= 0; i--) {
    if (getgr32(i) != 0) {
      TRACE(T_INST, " gr%d saved, value=%o (%o/%o)\n", i, getgr32(i), getgr32(i)>>16, getgr32(i)&0xffff);
      put32(getgr32(i), INCVA(ea,j));
      savemask |= BITMASK16(16-i);
    }
    j += 2;
  }
  put32(getcrs32s(XB), INCVA(ea,25));
  TRACE(T_INST, " XB saved, value=%o/%o\n", getcrs16(XBH), getcrs16(XBL));
  put16(savemask, ea);
  TRACE(T_INST, " Saved, mask=%o\n", savemask);
  goto fetch;

d_rrst:  /* 000717 */
  TRACE(T_FLOW, " RRST\n");
  ea = apea(NULL);
  eap = &gv.brp[SBBR];   /* registers almost always saved on the stack */
  savemask = get16(ea);
  TRACE(T_INST, " Save mask=%o\n", savemask);
  j = 1;
  for (i = 11; i >= 0; i--) {
    if (savemask & BITMASK16(16-i)) {
      putgr32(i, get32(INCVA(ea,j)));
      TRACE(T_INST, " gr%d restored, value=%o (%o/%o)\n", i, getgr32(i), getgr32(i)>>16, getgr32(i)&0xffff);
    } else {
      putgr32(i, 0);
    }
    j += 2;
  }
  putcrs32(XB, get32(INCVA(ea,25)));
  TRACE(T_INST, " XB restored, value=%o/%o\n", getcrs16(XBH), getcrs16(XBL));
  goto fetch;

d_enb:  /* 000400 (enbm), 000401 (enbl), 000402 (enbp) */
  TRACE(T_FLOW, " ENB\n");
  RESTRICT();
  putcrs16(MODALS, getcrs16(MODALS) | 0100000);
  gv.inhcount = 1;
  goto fetch;

d_inh:  /* 001000 (inhm), 001001 (inhl), 001002 (inhp) */
  TRACE(T_FLOW, " INH\n");
  RESTRICT();
  putcrs16(MODALS, getcrs16(MODALS) & ~0100000);
  goto fetch;

d_stac:  /* 001200 */
  TRACE(T_FLOW, " STAC\n");
  ea = apea(NULL);
  if (get16(ea) == getcrs16(B)) {
    put16(getcrs16(A), ea);
    SETEQ;
  } else 
    CLEAREQ;
  goto fetch;

d_stlc:  /* 001204 */
  TRACE(T_FLOW, " STLC\n");
  ea = apea(NULL);
  if (get32(ea) == getcrs32(E)){
    put32(getcrs32(L), ea);
    SETEQ;
  } else 
    CLEAREQ;
  goto fetch;

  /* NOTE: when ARGT is executed as an instruction, it means
     that a fault occurred during PCL argument processing. */

d_argt:  /* 000605 */
  TRACE(T_FLOW|T_PCL, " ARGT\n");
  argt();
  goto fetch;

d_calf:  /* 000705 */
  TRACE(T_FLOW|T_PCL, " CALF\n");
  ea = apea(NULL);
  calf(ea);
  goto fetch;

  /* Decimal and character instructions

    IMPORTANT NOTE: when using the Z macros, be sure to use curly
    braces, ie,

	 Instead of:

	   if (cond)
	     ZPUTC ...

	 use:

	   if (cond) {
	     ZPUTC ...
	   }
  */

#define ZSTEP(zea, zlen, zcp, zclen, zacc) \
  zcp = (unsigned char *) (MEM+mapva(zea, RP, zacc, &zaccess)); \
  zclen = 2048 - (zea & 01777)*2; \
  if (zea & EXTMASK32) { \
    zcp++; \
    zclen--; \
  } \
  if (zclen >= zlen) \
    zclen = zlen; \
  else \
    zea = (zea & 0xEFFF0000) | ((zea+0x400) & 0xFC00) \

#define ZGETC(zea, zlen, zcp, zclen, zch) \
  if (zclen == 0) { \
    ZSTEP(zea, zlen, zcp, zclen, RACC); \
  } \
  zch = *zcp; \
  zcp++; \
  zclen--; \
  zlen--

#define ZPUTC(zea, zlen, zcp, zclen, zch) \
  if (zclen == 0) { \
    ZSTEP(zea, zlen, zcp, zclen, WACC); \
  } \
  *zcp = (zch); \
  zcp++; \
  zclen--; \
  zlen--

d_zmv:  /* 001114 */
  TRACE(T_FLOW, " ZMV\n");
  zspace = 0240;
  if (getcrs16(KEYS) & 020)
    zspace = 040;
  TRACE(T_FLOW, "ZMV: source=%o/%o, len=%d, dest=%o/%o, len=%d, keys=%o\n", getgr32(FAR0)>>16, getgr32(FAR0)&0xffff, GETFLR(0), getgr32(FAR1)>>16, getgr32(FAR1)&0xffff, GETFLR(1), getcrs16(KEYS));

  zlen1 = GETFLR(0);
  zlen2 = GETFLR(1);
  zea1 = getgr32(FAR0);
  if (getgr32(FLR0) & 0x8000)
    zea1 |= EXTMASK32;
  zea2 = getgr32(FAR1);
  if (getgr32(FLR1) & 0x8000)
    zea2 |= EXTMASK32;
  TRACE(T_FLOW, "     ea1=%o/%o, len1=%d, ea2=%o/%o, len2=%d\n", zea1>>16, zea1&0xffff, zlen1, zea2>>16, zea2&0xffff, zlen2);
  zclen1 = 0;
  zclen2 = 0;
  while (zlen2) {
    if (zlen1) {
      ZGETC(zea1, zlen1, zcp1, zclen1, zch1);
    } else
      zch1 = zspace;
    TRACE(T_FLOW, " zch1=%o (%c)\n", zch1, zch1&0x7f);
    ZPUTC(zea2, zlen2, zcp2, zclen2, zch1);
  }
  goto fetch;

d_zmvd:  /* 001115 */
  TRACE(T_FLOW, " ZMVD\n");
  zlen1 = GETFLR(1);
  zlen2 = zlen1;
  zea1 = getgr32(FAR0);
  if (getgr32(FLR0) & 0x8000)
    zea1 |= EXTMASK32;
  zea2 = getgr32(FAR1);
  if (getgr32(FLR1) & 0x8000)
    zea2 |= EXTMASK32;
  TRACE(T_FLOW, " ea1=%o/%o, ea2=%o/%o, len=%d\n", zea1>>16, zea1&0xffff, zea2>>16, zea2&0xffff, zlen1);
  zclen1 = 0;
  zclen2 = 0;
  while (zlen2) {
#if 0
    /* this code doesn't run much faster than the 3-line ZGETC/PUTC loop,
       and sometimes slower.  Suspect there are cache differences. */

    if (zclen1 == 0) {
      ZSTEP(zea1, zlen1, zcp1, zclen1, RACC);
    }
    if (zclen2 == 0) {
      ZSTEP(zea2, zlen2, zcp2, zclen2, WACC);
    }
    if (zclen1 < zclen2)
      utempa = zclen1;
    else
      utempa = zclen2;
    zclen1 -= utempa;
    zclen2 -= utempa;
    zlen1 -= utempa;
    zlen2 -= utempa;
#if 1
    /* this works */
    while (utempa--)
      *zcp2++ = *zcp1++;
#else
    /* this causes error: 
          Coldstarting PRIMOS, Please wait...
	  Unable to initialize gate segment.  (GATE_INIT) */
    memcpy(zcp2, zcp1, utempa);
#endif
#else
    ZGETC(zea1, zlen1, zcp1, zclen1, zch1);
    TRACE(T_FLOW, " zch1=%o (%c)\n", zch1, zch1&0x7f);
    ZPUTC(zea2, zlen2, zcp2, zclen2, zch1);
#endif
  }
  goto fetch;

  /* NOTE: ZFIL is used early after PX enabled, and can be used to cause
     a UII fault to debug CALF etc.

     I tried using memset, but it was much slower than a manual loop.

     It may be worthwhile to special case a 2048-byte fill that is
     page-aligned, since ZFIL is often used this way by Primos, but
     this isn't a significant overall emulator performance issue. */

d_zfil:  /* 001116 */
  TRACE(T_FLOW, " ZFIL\n");
  zlen2 = GETFLR(1);
  zea2 = getgr32(FAR1);
  if (getgr32(FLR1) & 0x8000)
    zea2 |= EXTMASK32;
  zch2 = getcrs16(A);
  TRACE(T_FLOW, " ea=%o/%o, len=%d, fill=%o (%c)\n", zea2>>16, zea2&0xffff, zlen2, zch2, zch2&0x7f);
  zclen2 = 0;
  while (zlen2) {
#if 1
    ZSTEP(zea2, zlen2, zcp2, zclen2, WACC);
    zlen2 -= zclen2;
    while (zclen2--) {
      *zcp2++ = zch2;
    }
#else
    ZPUTC(zea2, zlen2, zcp2, zclen2, zch2);
#endif
  }
  goto fetch;

d_zcm:  /* 001117 */
  TRACE(T_FLOW, " ZCM\n");
  if (getcrs16(KEYS) & 020)
    zspace = 040;
  else
    zspace = 0240;
  zlen1 = GETFLR(0);
  zlen2 = GETFLR(1);
  zea1 = getgr32(FAR0);
  if (getgr32(FLR0) & 0x8000)
    zea1 |= EXTMASK32;
  zea2 = getgr32(FAR1);
  if (getgr32(FLR1) & 0x8000)
    zea2 |= EXTMASK32;
  TRACE(T_FLOW, " ea1=%o/%o, len1=%d, ea2=%o/%o, len2=%d\n", zea1>>16, zea1&0xffff, zlen1, zea2>>16, zea2&0xffff, zlen2);
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
    TRACE(T_FLOW, " zch1=%o (%c), zch2=%o (%c)\n", zch1, zch1&0x7f, zch2, zch2&0x7f);
    if (zch1 < zch2) {
      zresult = 0200;
      break;
    } else if (zch1 > zch2) {
      zresult = 0;
      break;
    }
  }
  putcrs16(KEYS, (getcrs16(KEYS) & ~0300) | zresult);
  goto fetch;

d_ztrn:  /* 001110 */
  TRACE(T_FLOW, " ZTRN\n");
  zlen1 = GETFLR(1);
  zlen2 = zlen1;
  utempl = zlen1;
  zea1 = getgr32(FAR0);
  if (getgr32(FLR0) & 0x8000)
    zea1 |= EXTMASK32;
  zea2 = getgr32(FAR1);
  if (getgr32(FLR1) & 0x8000)
    zea2 |= EXTMASK32;
  TRACE(T_FLOW, " ea1=%o/%o, len1=%d, ea2=%o/%o, len2=%d\n", zea1>>16, zea1&0xffff, zlen1, zea2>>16, zea2&0xffff, zlen2);
  zclen1 = 0;
  zclen2 = 0;
  ea = getcrs32ea(XB);
  while (zlen2) {
    ZGETC(zea1, zlen1, zcp1, zclen1, zch1);
    utempa = get16(INCVA(ea,zch1/2));
    if (zch1 & 1)
      zch2 = utempa & 0xFF;
    else
      zch2 = utempa >> 8;
    TRACE(T_FLOW, " zch1=%o (%c), zch2=%o (%c)\n", zch1, zch1&0x7f, zch2, zch2&0x7f);
    ZPUTC(zea2, zlen2, zcp2, zclen2, zch2);
  }
  PUTFLR(1, 0);
  arfa(0, utempl);
  arfa(1, utempl);
  goto fetch;

d_zed:  /* 001111 */
  TRACE(T_FLOW, " ZED\n");
  zlen1 = GETFLR(0);
  zlen2 = 128*1024;      /* XXX: not sure about max length of result */
  zea1 = getgr32(FAR0);
  if (getgr32(FLR0) & 0x8000)
    zea1 |= EXTMASK32;
  zea2 = getgr32(FAR1);
  if (getgr32(FLR1) & 0x8000)
    zea2 |= EXTMASK32;
  TRACE(T_FLOW, " ea1=%o/%o, len1=%d, ea2=%o/%o, len2=%d\n", zea1>>16, zea1&0xffff, zlen1, zea2>>16, zea2&0xffff, zlen2);
  if (getcrs16(KEYS) & 020)
    zspace = 040;
  else
    zspace = 0240;
  zclen1 = 0;
  zclen2 = 0;
  ea = getcrs32ea(XB);
  for (i=0; i < 32767; i++) {     /* do edit pgms have a size limit? */
    utempa = get16(INCVA(ea, i));
    m = utempa & 0xFF;
    switch ((utempa >> 8) & 3) {
    case 0:  /* copy M chars */
      while (m && zlen1) {
	ZGETC(zea1, zlen1, zcp1, zclen1, zch1);
	ZPUTC(zea2, zlen2, zcp2, zclen2, zch1);
	m--;
      }
      while (m) {
	ZPUTC(zea2, zlen2, zcp2, zclen2, zspace);
	m--;
      }
      break;

    case 1:  /* insert character M */
      ZPUTC(zea2, zlen2, zcp2, zclen2, m);
      break;

    case 2:  /* skip M characters */
      if (m >= zlen1)
	zlen1 = 0;
      else while (m) {
	ZGETC(zea1, zlen1, zcp1, zclen1, zch1);
	m--;
      }
      break;

    case 3:  /* insert M blanks */
      while (m) {
	ZPUTC(zea2, zlen2, zcp2, zclen2, zspace);
	m--;
      }
      break;

    default:
      fatal("ZED em bug");
    }
    if (utempa & 0x8000)
      break;
  }
  goto fetch;

d_xed:  /* 001112 */

  /* XED has some support for chars w/o parity by checking the
     keys before setting the zero suppress character, but it's
     not clear if it should ignore all character parity */

  TRACE(T_FLOW, " XED\n");
  zlen1 = zlen2 = 128*1024;
  zea1 = getgr32(FAR0);
  if (getgr32(FLR0) & 0x8000)
    zea1 |= EXTMASK32;
  zea2 = getgr32(FAR1);
  if (getgr32(FLR1) & 0x8000)
    zea2 |= EXTMASK32;
  zclen1 = 0;
  zclen2 = 0;
  TRACE(T_FLOW, " ea1=%o/%o, len1=%d, ea2=%o/%o, len2=%d\n", zea1>>16, zea1&0xffff, zlen1, zea2>>16, zea2&0xffff, zlen2);
  if (getcrs16(KEYS) & 020)
    xsc = 040;
  else
    xsc = 0240;
  xfc = 0;
  ZGETC(zea1, zlen1, zcp1, zclen1, zch1);
  //printf("xed: first char = '%o\n", zch1);
  xsign = (zch1 == XMINUS);
  xsig = 0;
  ea = getcrs32ea(XB);
  for (i=0; i < 32767; i++) {     /* do edit pgms have a size limit? */
    utempa = get16(INCVA(ea, i));
    m = utempa & 0xFF;
    //printf("\nxed: %d: opcode = %o, m=%o\n", i, (utempa>>8) & 037, m);
    switch ((utempa >> 8) & 037) {
    case 0:  /* Zero Suppress */
      while (m) {
	ZGETC(zea1, zlen1, zcp1, zclen1, zch1);
	if (!xsig)
	  if (zch1 == XZERO)
	    zch1 = xsc;
	  else {
	    xsig = 1;
	    if (xfc) {
	      ZPUTC(zea2, zlen2, zcp2, zclen2, xfc);
	    }
	  }
	ZPUTC(zea2, zlen2, zcp2, zclen2, zch1);
	m--;
      }
      break;

    case 1:  /* insert character M */
      ZPUTC(zea2, zlen2, zcp2, zclen2, m);
      break;

    case 2:  /* set supression character */
      xsc = m;
      break;

    case 3:  /* insert character */
      if (xsig)
	zch1 = m;
      else
	zch1 = xsc;
      ZPUTC(zea2, zlen2, zcp2, zclen2, zch1);
      break;

    case 4:  /* insert digits */
      if (!xsig && xfc) {
	ZPUTC(zea2, zlen2, zcp2, zclen2, xfc);
      }
      while (m) {
	ZGETC(zea1, zlen1, zcp1, zclen1, zch1);
	ZPUTC(zea2, zlen2, zcp2, zclen2, zch1);
	m--;
      }
      xsig = 1;
      break;

    case 5:  /* insert char if minus */
      if (xsign)
	zch1 = m;
      else
	zch1 = xsc;
      ZPUTC(zea2, zlen2, zcp2, zclen2, zch1);
      break;

    case 6:  /* insert char if plus */
      if (!xsign)
	zch1 = m;
      else
	zch1 = xsc;
      ZPUTC(zea2, zlen2, zcp2, zclen2, zch1);
      break;

    case 7:  /* set floating char */
      xfc = m;
      break;

    case 010:  /* set floating if plus */
      if (!xsign)
	xfc = m;
      else
	xfc = xsc;
      break;

    case 011:  /* set floating if minus */
      if (xsign)
	xfc = m;
      else
	xfc = xsc;
      break;

    case 012:  /* set floating to sign */
      if (xsign)
	xfc = XMINUS;
      else
	xfc = XPLUS;
      break;

    case 013:  /* jump if zero */
      if (getcrs16(A))
	i += m;
      break;

    case 014:  /* fill with suppress */
      while (m) {
	ZPUTC(zea2, zlen2, zcp2, zclen2, xsc);
	m--;
      }
      break;

    case 015:  /* set significance */
      if (!xsig && xfc) {
	ZPUTC(zea2, zlen2, zcp2, zclen2, xfc);
      }
      xsig = 1;
      break;

    case 016:  /* insert sign */
      if (xsign)
	zch1 = XMINUS;
      else
	zch1 = XPLUS;
      ZPUTC(zea2, zlen2, zcp2, zclen2, zch1);
      break;

    case 017:  /* suppress digits */
      while (m) {
	ZGETC(zea1, zlen1, zcp1, zclen1, zch1);
	if (zch1 == XZERO)
	  zch1 = xsc;
	ZPUTC(zea2, zlen2, zcp2, zclen2, zch1);
	m--;
      }
      break;

    case 020:  /* embed sign */
      while (m) {
	ZGETC(zea1, zlen1, zcp1, zclen1, zch1);
	if (xsign)
	  if (zch1 == XZERO)
	    zch1 = XRBRACE;
	  else
	    zch1 = zch1-XONE+XJ;
	ZPUTC(zea2, zlen2, zcp2, zclen2, zch1);
	m--;
      }
      break;

    default:
      warn("xed: unrecognized subprogram opcode ignored");
    }
    if (utempa & 0x8000)
      break;
  }
  goto fetch;

  /* unimplemented decimal instructions:

     - 001100 : XAD
     - 001101 : XMV
     - 001102 : XCM
     - 001104 : XMP
     - 001107 : XDV
     - 001145 : XBTD
     - 001146 : XDTB
  */

d_xuii:
  TRACE(T_FLOW, " XUII: %s\n", inst==01100?"XAD":inst==01101?"XMV":inst==01102?"XMV":inst==01104?"XMP":inst==01107?"XDV":inst==01145?"XBTD":inst==01146?"XDTB":"UNKN");
  fault(UIIFAULT, RPL, RP);
  fatal("Return from XZUII fault");

d_sttm:  /* 000510 */
  TRACE(T_FLOW, " STTM\n", inst);
  ea = getcrs32ea(OWNER);
  utempl = get32r0(ea+PCBPET);      /* get PCB elapsed timer */
  utempl += getcrs16s(TIMERH); /* add live timer (signed) */
  ea = getcrs32ea(XB);
  put32(utempl, ea);                /* store process time */
  put16(getcrs16(TIMERL), INCVA(ea,2));  /* and live timer residue */
  goto fetch;

  /* OS/restricted instructions */

d_rts:  /* 000511 */
  TRACE(T_FLOW, " RTS / P300ISI\n", inst);
  RESTRICT();
  if (((getcrs16(KEYS) & 016000) >> 10) <= 3)
    goto d_uii;
  tempa = getcrs16(TIMERH);
  templ = tempa - getcrs16s(A);
  ea = getcrs32ea(OWNER);
  templ += get32r0(ea+PCBPET);
  put32r0(templ, ea+PCBPET);
  putcrs16(TIMERH, getcrs16(A));
  goto fetch;

d_wait:  /* 000315 */
  TRACE(T_FLOW, " WAIT\n", inst);
  RESTRICT();
  pwait();
  goto fetch;

d_nfy:  /* 1210 (nfye), 1211 (nfyb), 
	   1214 (inen), 1215 (inbn), 1216 (inec), 1217 (inbc) */
  TRACE(T_FLOW, " NFY\n", inst);
  RESTRICT();
  nfy(inst);
  goto fetch;

d_stex:  /* 001315 */
  TRACE(T_FLOW, " STEX\n");
  putcrs32ea(L, stex(getcrs32(L)));
  goto fetch;

  /* NOTE: L contains target virtual address, which is used to
     determine which pages of cache to invalidate.  Since this
     emulator does not have a memory cache, L is unused. */

d_liot:  /* 000044 */
  TRACE(T_FLOW, " LIOT\n");
  RESTRICT();
  ea = apea(NULL);
  utempa = STLBIX(ea);
  gv.stlb[utempa].seg = 0xFFFF;
  TRACE(T_TLB, "stlb[%d] invalidated at %o/%o for liot\n", utempa, RPH, RPL);
  mapva(ea, RP, RACC, &access);
  TRACE(T_INST, " loaded STLB for %o/%o\n", ea>>16, ea&0xffff);
  invalidate_brp();
  goto fetch;

d_ptlb:  /* 000064 */
  /* XXX: What about the IOTLB?  Should it be purged too? */
  TRACE(T_FLOW, " PTLB\n");
  RESTRICT();
  utempl = getcrs32(L);
  for (utempa = 0; utempa < STLBENTS; utempa++)
    if ((utempl & 0x80000000) || gv.stlb[utempa].ppa == (utempl << 10)) {
      TRACE(T_TLB, "stlb[%d] invalidated at %o/%o for ptlb\n", utempa, RPH, RPL);
      gv.stlb[utempa].seg = 0xFFFF;
    }
  invalidate_brp();
  goto fetch;

d_itlb:  /* 000615 */
  TRACE(T_FLOW, " ITLB\n");
  RESTRICT();
  utempl = getcrs32(L);

  /* NOTE: on older systems w/o PTLB, Primos substitutes an ITLB loop
     for PTLB and the ITLB segno is 1, ie, it looks like using segment
     1 invalidates all pages that match, ignoring segment number??
     Instead of doing that, we purge the STLB whenever address 1/0 is
     invalidated.
  */

  if (utempl == 0x10000) {
    for (utempa = 0; utempa < STLBENTS; utempa++)
      gv.stlb[utempa].seg = 0xFFFF;
    TRACE(T_TLB, "stlb purged at %o/%o by ITLB\n", RPH, RPL);
  } else {
    utempa = STLBIX(utempl);
    gv.stlb[utempa].seg = 0xFFFF;
    TRACE(T_TLB, "stlb[%d] invalidated at %o/%o by ITLB for %o/%o\n", utempa, RPH, RPL, utempl>>16, utempl&0xFFFF);
    if (((utempl >> 16) & 07777) < 4) {
      gv.iotlb[(utempl >> 10) & 0xFF].valid = 0;
      TRACE(T_TLB, "iotlb[%d] invalidated at %o/%o by ITLB for %o/%o\n", utempa, RPH, RPL, utempl>>16, utempl&0xFFFF);
    }
  }
  invalidate_brp();
  goto fetch;

d_lpsw:  /* 000711 */
  TRACE(T_FLOW, " LPSW\n");
  RESTRICT();
  lpsw();
  goto fetch;

d_stpm:  /* 000024 */
  {
    ea_t ea;
    int i;

    TRACE(T_FLOW, " STPM\n", inst);
    RESTRICT();
    ea = getcrs32(XB);
    for (i=0; i<8; i++)
      put16(0, ea+i);
    put16(cpuid, ea+1);
    if (sscanf(REV, "%d", &templ))
      put16((short)templ, ea+3);
    goto fetch;
  }

d_dbgill:  /*  001700, 001701 */
  TRACE(T_FLOW, " DBGILL\n", inst);
  fault(ILLINSTFAULT, RPL, RP);
  fatal(NULL);

      /* JW: I think 1702 is an invalid opcode that Prime uses as
	 an assertion when unexpected things happen, for example:

	 LDA modals        get modals
	 SAS 1             interrupts enabled?
	 1702              no, they should be, die

	 Update: this is the "slow halt" instruction.  Ewan Milne
	 explained that if a HLT instruction is encountered on some
	 machines,, any DMX in progress will stop, possibly leading to
	 partial disk records being written.  Using this illegal
	 instruction instead of HLT lets Primos delay the halt until
	 DMX has completed.
      */

d_pbug:  /* 001702 */
  TRACE(T_FLOW, " 1702?\n", inst);
  fault(ILLINSTFAULT, RPL, RP);
  fatal(NULL);  /* just in case of a bogus return (coding error) */

d_irtn:  /* 000601 */
  TRACE(T_FLOW, " IRTN\n", inst);
  RESTRICT();
  //fatal("IRTN causes a loop in CPU.CACHE Case 4");
irtn:
  newkeys(getar16(PSWKEYS16));
  RP = getar32(PSWPB32);
  putcrs16(PBH, RPH);
  if (RPL < gv.livereglim && ((getcrs16(KEYS) & 0016000) != 010000))
    RP |= 0x80000000;
  putcrs16(MODALS, getcrs16(MODALS) | 0100000);
#if 0
  if (regs.sym.pcba != 0) {
    RP = getar32(PSWPB32);
    newkeys(getar16(PSWKEYS16));
  } else
    putcrs16(OWNERL, 0);
#endif
  dispatcher();
  goto fetch;

d_irtc:  /* 000603 */
  TRACE(T_FLOW, " IRTC\n", inst);
  RESTRICT();
  gv.intvec = -1;
  goto irtn;

d_cai:  /* 000411 */
  TRACE(T_FLOW, " CAI\n", inst);
  RESTRICT();
  gv.intvec = -1;
  goto fetch;

  /* R-mode/infrequent gen 0 instructions */

d_sgl:  /* 000005 */
  TRACE(T_FLOW, " SGL\n");
  putcrs16(KEYS, getcrs16(KEYS) & ~040000);
  goto fetch;

d_e16s:  /* 000011 */
  TRACE(T_FLOW, " E16S\n");
  newkeys(getcrs16(KEYS) & 0161777);
  goto fetch;

d_e32s:  /* 000013 */
  TRACE(T_FLOW, " E32S\n");
  newkeys((getcrs16(KEYS) & 0161777) | 1<<10);
  goto fetch;

d_e32r:  /* 001013 */
  TRACE(T_FLOW, " E32R\n");
  newkeys((getcrs16(KEYS) & 0161777) | 3<<10);
  goto fetch;

d_e64r:  /* 001011 */
  TRACE(T_FLOW, " E64R\n");
  newkeys((getcrs16(KEYS) & 0161777) | 2<<10);
  goto fetch;

d_e64v:  /* 000010 */
  TRACE(T_FLOW, " E64V\n");
  newkeys((getcrs16(KEYS) & 0161777) | 6<<10);
  goto fetch;

d_e32i:  /* 001010 */
  TRACE(T_FLOW, " E32I\n");

  /* NOTE: this fault needs to occur on older models even in
     Ring 0, so the RESTRICT() macro can't be used here.

     XXX: for a P500, cpuid=0 is shared with a P400, but E32I
     doesn't fault! */

  if (cpuid < 4)
    fault(RESTRICTFAULT, 0, 0);
  else
    newkeys((getcrs16(KEYS) & 0161777) | 4<<10);
  goto fetch;

d_svc:  /* 000505 */
  TRACE(T_FLOW, " SVC\n");
  fault(SVCFAULT, 0, 0);
  fatal("Returned from SVC fault");
  
d_cea:  /* 000111 */
  TRACE(T_FLOW, " CEA\n");
  switch ((getcrs16(KEYS) & 016000) >> 10) {
  case 0:                       /* 16S */
    ea = getcrs16(A);
    while (1) {
      i = ea & 0100000;
      x = ea & 040000;
      ea &= 037777;
      if (x)                           /* indexed */
	ea = (ea + getcrs16(X)) & 037777;
      if (!i)                          /* not indirect */
	break;
      if (ea < gv.livereglim)
	ea = get16trap(ea);
      else
	ea = get16(MAKEVA(RPH,ea));
    }
    putcrs16(A, ea);
    break;
  case 1:                       /* 32S */
  case 3:                       /* 32R */
    while (getcrs16(A) & 0100000) {
      ea = getcrs16(A) & 077777;
      if (ea < gv.livereglim)
	putcrs16(A, get16trap(ea));
      else
	putcrs16(A, get16(MAKEVA(RPH,ea)));
    }
  }
  goto fetch;

d_hlt:  /* 000000 */
  TRACE(T_FLOW, " HLT\n");
  RESTRICT();
  if (bootarg) {
    printf("\nCPU halt, instruction #%u at %o/%o %s: %o %o ^%06o^\nA='%o/%d  B='%o/%d  L='%o/%d  X=%o/%d", gv.instcount, RPH, RPL, searchloadmap(gv.prevpc,' '), get16t(gv.prevpc), get16t(gv.prevpc+1), lights, getcrs16(A), getcrs16s(A), getcrs16(B), getcrs16s(B), getcrs32(A), getcrs32s(A), getcrs16(X), getcrs16s(X));
    while (1) {
      printf("\nPress Enter to continue, h to halt... ");
      utempa = getchar();
      printf("\n");
      if (utempa == '\r' || utempa == '\n')
	goto fetch;
      if (utempa == 'h')
	break;
    }
  }
  fatal("CPU halt");

d_pim:  /* 000205 (R-mode) */
  TRACE(T_FLOW, " PIM\n");
#if 0
  /* NOTE: this fits the description in the Rev 21 ISG, but fails
     DIAG test CPU.INTEGER, Case 12 */

  putcrs16(A, (getcrs16(A) & 0x8000) | (getcrs16(B) & 0x7FFF));
#else
  putcrs16(A, (getcrs16(A) & 0x8000) | getcrs16(B));
#endif
  goto fetch;

d_pid:  /* 000211 (R-mode) */
  TRACE(T_FLOW, " PID\n");
  putcrs32s(L, getcrs16s(A));
  putcrs16(B, getcrs16(B) & 0x7fff);
  goto fetch;

d_dbl:  /* 000007 (R-mode) */

  /* DBL activates 31-bit mode (R-mode only):

      LDA -> DLD (double load)
      STA -> DST (double store)
      ADD -> DAD (double add)
      SUB -> DSB (double subtract)

      Other R-mode, 31-bit instructions include:

      PID, DIV, MPY, PIM, INT, FLOT
  */

  TRACE(T_FLOW, " DBL\n");
  putcrs16(KEYS, getcrs16(KEYS) | 040000);
  goto fetch;

d_sca:  /* 000041 */
  TRACE(T_FLOW, " SCA\n");
  putcrs16(A, getcrs16(VSC) & 0xFF);
  goto fetch;

d_inkr:  /* 000043 */
  TRACE(T_FLOW, " INKr\n");
  putcrs16(A, (getcrs16(KEYS) & 0xFF00) | (getcrs16(VSC) & 0xFF));
  goto fetch;

d_otkr:  /* 000405 */
  TRACE(T_FLOW, " OTKr\n");
  newkeys((getcrs16(A) & 0xFF00) | (getcrs16(KEYS) & 0xFF));
  putcrs16(VSC, (getcrs16(VSC) & 0xFF00) | (getcrs16(A) & 0xFF));
  if ((RP & RINGMASK32) == 0)
    gv.inhcount = 1;
  goto fetch;

d_esim:  /* 000415 */
  TRACE(T_FLOW, " ESIM\n");
  RESTRICT();
  putcrs16(MODALS, getcrs16(MODALS) & ~040000);
  goto fetch;

d_evim:  /* 000417 */
  TRACE(T_FLOW, " EVIM\n");
  RESTRICT();
  putcrs16(MODALS, getcrs16(MODALS) | 040000);
  goto fetch;

d_nrm:  /* 000101 */
  TRACE(T_FLOW, " NRM\n");
  putcrs16(VSC, 0);
  if (getgr32(GR2) != 0) {
    while (!((getcrs16(A) ^ (getcrs16(A) << 1)) & 0x8000)) {
      TRACE(T_INST,  " step %d: A=%o, B=%o\n", getcrs16(VSC), getcrs16(A), getcrs16(B));
      putcrs16(B, getcrs16(B) << 1);
      putcrs16(A, (getcrs16(A) & 0x8000) | ((getcrs16(A) << 1) & 0x7FFE) | (getcrs16(B) >> 15));
      putcrs16(VSC, getcrs16(VSC) + 1);
    }
    putcrs16(B, getcrs16(B) & 0x7FFF);
    TRACE(T_INST,  " finished with %d shifts: A=%o, B=%o\n", getcrs16(VSC), getcrs16(A), getcrs16(B));
  }
  goto fetch;

d_rtn:  /* 000105 */
  TRACE(T_FLOW, " RTN\n");
  utempa = get16(getcrs16(S));
  RPL = get16(utempa+1);
  if (RPL == 0)
    fault(STACKFAULT, 0, 0);
  putcrs16(S, utempa);
  goto fetch;

  /* unusual instructions */

d_sync:  /* 000003 */
  TRACE(T_FLOW, " SYNC\n");

  /* After looking at the simh Honeywell 315/516 simulator, I
     decided that instruction 3 must be some kind of no-op on
     the Prime.  I did verify that it is a legal instruction and
     doesn't generate a UII fault, even though it is not
     documented anywhere that I could find.

     FTN executes this instruction (by JMP to wrong relative
     address I think), and newer versions of Primos execute it
     in the disk driver code, to sync the cache on multi-processor
     systems (something like that).
  */

  goto fetch;

d_bclt:  /* 0141604 */
  TRACE(T_FLOW, " BCLT\n");
  BCLT;
  goto fetch;

d_bcle:  /* 0141600 */
  TRACE(T_FLOW, " BCLE\n");
  BCLE;
  goto fetch;

d_bceq:  /* 0141602 */
  TRACE(T_FLOW, " BCEQ\n");
  BCEQ;
  goto fetch;

d_bcne:  /* 0141603 */
  TRACE(T_FLOW, " BCNE\n");
  BCNE;
  goto fetch;

d_bcge:  /* 0141605 */
  TRACE(T_FLOW, " BCGE\n");
  BCGE;
  goto fetch;

d_bcgt:  /* 0141601 */
  TRACE(T_FLOW, " BCGT\n");
  BCGT;
  goto fetch;

d_bcr:  /* 0141705 */
  TRACE(T_FLOW, " BCR\n");
  if (!(getcrs16(KEYS) & 0100000))
    RPL = iget16(RP);
  else
    INCRP;
  goto fetch;

d_bcs:  /* 0141704 */
  TRACE(T_FLOW, " BCS\n");
  if (getcrs16(KEYS) & 0100000)
    RPL = iget16(RP);
  else
    INCRP;
  goto fetch;

d_blr:  /* 0141707 */
  TRACE(T_FLOW, " BMLT/BLR\n");
  if (!(getcrs16(KEYS) & 020000))
    RPL = iget16(RP);
  else
    INCRP;
  goto fetch;

d_bls:  /* 0141706 */
  TRACE(T_FLOW, " BLS\n");
  BLS;
  goto fetch;

d_blt:  /* 0140614 */
  TRACE(T_FLOW, " BLT\n");
  SETCC_A;
  BCLT;
  goto fetch;

d_ble:  /* 0140610 */
  TRACE(T_FLOW, " BLE\n");
  SETCC_A;
  BCLE;
  goto fetch;

d_beq:  /* 0140612 */
  TRACE(T_FLOW, " BEQ\n");
  SETCC_A;
  BCEQ;
  goto fetch;

d_bne:  /* 0140613 */
  TRACE(T_FLOW, " BNE\n");
  SETCC_A;
  BCNE;
  goto fetch;

d_bge:  /* 0140615 */
  TRACE(T_FLOW, " BGE\n");
  SETCC_A;
  BCGE;
  goto fetch;

d_bgt:  /* 0140611 */
  TRACE(T_FLOW, " BGT\n");
  SETCC_A;
  BCGT;
  goto fetch;

d_blle:  /* 0140700 */
  TRACE(T_FLOW, " BLLE\n");
  SETCC_L;
  BCLE;
  goto fetch;

d_bleq:  /* 0140702 */
  TRACE(T_FLOW, " BLEQ\n");
  SETCC_L;
  BCEQ;
  goto fetch;

d_blne:  /* 0140703 */
  TRACE(T_FLOW, " BLNE\n");
  SETCC_L;
  BCNE;
  goto fetch;

d_blgt:  /* 0140701 */
  TRACE(T_FLOW, " BLGT\n");
  SETCC_L;
  BCGT;
  goto fetch;

d_bflt:  /* 0141614 */
  TRACE(T_FLOW, " BFLT\n");
  SETCC_F;
  BCLT;
  goto fetch;

d_bfle:  /* 0141610 */
  TRACE(T_FLOW, " BFLE\n");
  SETCC_F;
  BCLE;
  goto fetch;

d_bfeq:  /* 0141612 */
  TRACE(T_FLOW, " BFEQ\n");
  SETCC_F;
  BCEQ;
  goto fetch;

d_bfne:  /* 0141613 */
  TRACE(T_FLOW, " BFNE\n");
  SETCC_F;
  BCNE;
  goto fetch;

d_bfge:  /* 0141615 */
  TRACE(T_FLOW, " BFGE\n");
  SETCC_F;
  BCGE;
  goto fetch;

d_bfgt:  /* 0141611 */
  TRACE(T_FLOW, " BFGT\n");
  SETCC_F;
  BCGT;
  goto fetch;

d_bix:  /* 0141334 */
  TRACE(T_FLOW, " BIX\n");
  putcrs16(X, getcrs16(X) + 1);
  if (getcrs16(X) != 0) 
    RPL = iget16(RP);
  else
    INCRP;
  goto fetch;

d_biy:  /* 0141324 */
  TRACE(T_FLOW, " BIY\n");
  putcrs16(Y, getcrs16(Y) + 1);
  if (getcrs16(Y) != 0) 
    RPL = iget16(RP);
  else
    INCRP;
  goto fetch;

d_bdy:  /* 0140724 */
  TRACE(T_FLOW, " BDY\n");
  putcrs16(Y, getcrs16(Y) - 1);
  if (getcrs16(Y) != 0) 
    RPL = iget16(RP);
  else
    INCRP;
  goto fetch;

d_bdx:  /* 0140734 */
  TRACE(T_FLOW, " BDX\n");
  putcrs16(X, getcrs16(X) - 1);
  if (getcrs16(X) == 0) 
    INCRP;
  else {
    m = iget16(RP);
#ifndef NOIDLE
    if (getcrs16(X) > 100 && m == RPL-1) {
      struct timeval tv0,tv1;
      long delayusec, actualmsec;

      /* for BDX * loop (backstop process mainly), we want to change
	 this to a long sleep so that the emulation host's CPU isn't 
	 pegged the whole time the emulator is running.

	 So first, check to see if any device times expire sooner than
	 this, and if so, limit the sleep time to the lowest expiration
	 value (this is stored as number of instructions left until the
	 timer expires).

	 NOTE: In practice, the clock device ticks at 330 times a sec
	 under standard Primos so we only get to delay about 3ms here,
	 but it still keeps CPU usage to 4-5% on a 1.5GHz Mac.  Primos
	 mods to make the clock tick 20 times per second allows for
	 much longer sleeps here, ie, CPU overhead is 0.7% while idle.
      */

      putcrs16(X, 1);                     /* exit on next loop */
      if (!firstbdx) {
	//printf("%o ", getcrs16(OWNERL)); fflush(stdout);
	utempl = gv.instpermsec*100;    /* limit delay to 100 msecs */
	for (i=0; i<64; i++)              /* check device timers */
	  if (devpoll[i])                 /* poll set? */
	    if (devpoll[i] <= 100) {      /* too fast! */
	      utempl = 1;
	      break;
	    } else if (devpoll[i] < utempl)
	      utempl = devpoll[i];
      } else {
	firstbdx = 0;
	utempl = 1;
      }

      /* this decrement ensures that if a device had a poll pending,
	 we won't decrement it to zero below, ie, it'll still fire
	 in the main loop */

      utempl--;                         /* utempl = # instructions */

      delayusec = utempl*1000/gv.instpermsec;
      if (delayusec > 1000) {
	if (gettimeofday(&tv0, NULL) != 0)
	  fatal("em: gettimeofday 0 failed");
	
	/* for some reason, the SIGTERM signal handler gets reset
	   during emulator initialization; this re-installs it */

	{
	  static int firsttime=1;
	  if (firsttime) {
	    signal (SIGTERM, sensorcheck);
	    firsttime = 0;
	  }
	}

	/* NOTE: on OSX, a signal (sigio for pnc) will interrupt usleep */

	usleep(delayusec);
	if (gettimeofday(&tv1, NULL) != 0)
	  fatal("em: gettimeofday 1 failed");
	actualmsec = (tv1.tv_sec-tv0.tv_sec-1)*1000 + (tv1.tv_usec+1000000-tv0.tv_usec)/1000;
#if 0
	if (actualmsec > delayusec*1.2/1000) {
	  TRACEA(" BDX loop at %o/%o, owner=%o, utempl=%d, wanted %d ms, got %d ms\n", gv.prevpc>>16, gv.prevpc&0xffff, getcrs16(OWNERL), utempl, delayusec/1000, actualmsec);
	}
#endif
	/* do timer bookkeeping that would have occurred if we had 
	   actually looped on BDX utempl times */

	for (i=0; i<64; i++)
	  if (devpoll[i] > 0)
	    devpoll[i] -= utempl;
	if (actualmsec > 0) {
	  utempa = getcrs16(TIMERH);
	  putcrs16(TIMERH, getcrs16(TIMERH) + actualmsec);
	  if (getcrs16(TIMERH) < utempa) {     /* timer overflowed */
	    tempea = getcrs32ea(OWNER);
	    utempa = get16r0(tempea+4) | 1;    /* set process abort flag */
	    put16r0(utempa, tempea+4);
	  }
	} else {
	  putcrs16(TIMERL, getcrs16(TIMERL) + utempl);
	}
	gv.instcount += actualmsec*gv.instpermsec;
      }
    }
#endif
    RPL = m;
  }
  goto fetch;

d_a1a:  /* 0141206 */
  TRACE(T_FLOW, " A1A\n");
a1a:
  putcrs16(A, add16(getcrs16(A), 1, 0, 0));
  goto fetch;

d_a2a:  /* 0140304 */
  TRACE(T_FLOW, " A2A\n");
  putcrs16(A, add16(getcrs16(A), 2, 0, 0));
  goto fetch;

d_aca:  /* 0141216 */
  TRACE(T_FLOW, " ACA\n");
  if (getcrs16(KEYS) & 0100000)
    goto a1a;
  putcrs16(KEYS, getcrs16(KEYS) & ~0120300);     /* clear C, L, LT, EQ */
  SETCC_A;
  goto fetch;

d_s1a:  /* 0140110 */
  TRACE(T_FLOW, " S1A\n");
  putcrs16(A, add16(getcrs16(A), 0xFFFF, 0, 0));
  goto fetch;

d_s2a:  /* 0140310 */
  TRACE(T_FLOW, " S2A\n");
  putcrs16(A, add16(getcrs16(A), 0xFFFE, 0, 0));
  goto fetch;

d_cal:  /* 0141050 */
  TRACE(T_FLOW, " CAL\n");
  putcrs16(A, getcrs16(A) & 0xFF);
  goto fetch;

d_car:  /* 0141044 */
  TRACE(T_FLOW, " CAR\n");
  putcrs16(A, getcrs16(A) & 0xFF00);
  goto fetch;

d_cra:  /* 0140040 */
  TRACE(T_FLOW, " CRA\n");
  putcrs16(A, 0);
  goto fetch;

  /* On the P300, the B register is the low-order word of the
     DP floating pt fraction, so CRB was used to convert SPFP
     numbers to DPFP.  On the P400 and up, the B register and
     DPFP accumulator do not overlap.  For compatibility, there
     are 3 related instructions:

     '14 clears B and the low-order DPFP register
     '15 clears only B
     '16 clears only the low-order DPFP register
  */

d_crb300:  /* 0140014 */
  TRACE(T_FLOW, " P300CRB\n");
  putcrs16(B, 0);
  putcrs16(FLTD, 0);
  goto fetch;

d_crb:  /* 0140015 */
  TRACE(T_FLOW, " CRB\n");
  putcrs16(B, 0);
  goto fetch;

d_fdbl:  /* 0140016 */
  TRACE(T_FLOW, " FDBL\n");
  putcrs16(FLTD, 0);
  goto fetch;

d_crl:  /* 0140010 */
  TRACE(T_FLOW, " CRL\n");
  putcrs32(L, 0);
  goto fetch;

d_caz:  /* 0140214 */
  TRACE(T_FLOW, " CAZ\n");
  /* set keys like CAS =0 would (subtract) */
  putcrs16(KEYS, (getcrs16(KEYS) & ~0100) | 020200);   /* clear EQ, set L, LT */
  if (getcrs16(A) == 0) {                  /* if zero, set EQ */
    SETEQ;
    INCRP;
  } else if (getcrs16s(A) < 0)
    RPL += 2;
  goto fetch;


d_irx:  /* 0140114 */
  TRACE(T_FLOW, " IRX\n");
  putcrs16(X, getcrs16(X) + 1);
  if (getcrs16(X) == 0) 
    INCRP;
  goto fetch;

d_drx:  /* 0140210 */
  TRACE(T_FLOW, " DRX\n");
  putcrs16(X, getcrs16(X) - 1);
  if (getcrs16(X) == 0) 
    INCRP;
  goto fetch;

d_icr:  /* 0141240 */
  TRACE(T_FLOW, " ICR\n");
  putcrs16(A, getcrs16(A) << 8);
  goto fetch;

d_icl:  /* 0141140 */
  TRACE(T_FLOW, " ICL\n");
  putcrs16(A, getcrs16(A) >> 8);
  goto fetch;

d_ica:  /* 0141340 */
  TRACE(T_FLOW, " ICA\n");
  putcrs16(A, (getcrs16(A) >> 8) | (getcrs16(A) << 8));
  goto fetch;

  /* NOTE: Rev 21 Inst. Guide says CC are indeterminate, other
     references say they are set */

d_lt:  /* 0140417 */
  TRACE(T_FLOW, " LT\n");
  putcrs16(A, 1);
  goto fetch;

d_lf:  /* 0140416 */
  TRACE(T_FLOW, " LF\n");
  putcrs16(A, 0);
  goto fetch;

d_tab:  /* 0140314 */
  TRACE(T_FLOW, " TAB\n");
  putcrs16(B, getcrs16(A));
  goto fetch;

d_tax:  /* 0140504 */
  TRACE(T_FLOW, " TAX\n");
  putcrs16(X, getcrs16(A));
  goto fetch;

d_tay:  /* 0140505 */
  TRACE(T_FLOW, " TAY\n");
  putcrs16(Y, getcrs16(A));
  goto fetch;

d_tba:  /* 0140604 */
  TRACE(T_FLOW, " TBA\n");
  putcrs16(A, getcrs16(B));
  goto fetch;

d_txa:  /* 0141034 */
  TRACE(T_FLOW, " TXA\n");
  putcrs16(A, getcrs16(X));
  goto fetch;

d_tya:  /* 0141124 */
  TRACE(T_FLOW, " TYA\n");
  putcrs16(A, getcrs16(Y));
  goto fetch;

d_xca:  /* 0140104 */
  TRACE(T_FLOW, " XCA\n");
  putgr32(GR2, getgr32(GR2) >> 16);
  goto fetch;

d_xcb:  /* 0140204 */
  TRACE(T_FLOW, " XCB\n");
  putgr32(GR2, getgr32(GR2) << 16);
  goto fetch;

d_tca:  /* 0140407 */
  TRACE(T_FLOW, " TCA\n");
  putcrs16(A, tch(getcrs16(A)));
  goto fetch;

d_tcl:  /* 0141210 */
  TRACE(T_FLOW, " TCL\n");
  putcrs32(L, tcr(getcrs32(L)));
  goto fetch;

d_scb:  /* 0140600 */
  TRACE(T_FLOW, " SCB\n");
  putcrs16(KEYS, getcrs16(KEYS) | 0100000);
  goto fetch;

d_rcb:  /* 0140200 */
  TRACE(T_FLOW, " RCB\n");
  putcrs16(KEYS, getcrs16(KEYS) & 077777);
  goto fetch;

d_chs:  /* 0140024 */
  TRACE(T_FLOW, " CHS\n");
  putcrs16(A, getcrs16(A) ^ 0x8000);
  goto fetch;

d_ssm:  /* 0140500 */
  TRACE(T_FLOW, " SSM\n");
  putcrs16(A, getcrs16(A) | 0100000);
  goto fetch;

d_ssp:  /* 0140100 */
  TRACE(T_FLOW, " SSP\n");
  putcrs16(A, getcrs16(A) & 077777);
  goto fetch;

d_cma:  /* 0140401 */
  TRACE(T_FLOW, " CMA\n");
  putcrs16(A, ~getcrs16(A));
  goto fetch;

d_csa:  /* 0140320 */
  TRACE(T_FLOW, " CSA\n");
  putcrs16(KEYS, (getcrs16(KEYS) & 077777) | (getcrs16(A) & 0x8000));
  putcrs16(A, getcrs16(A) & 077777);
  goto fetch;

d_lclt:  /* 0141500 */
  TRACE(T_FLOW, " LCLT\n");
  putcrs16(A, LCLT);
  goto fetch;

d_lcle:  /* 0141501 */
  TRACE(T_FLOW, " LCLE\n");
  putcrs16(A, LCLE);
  goto fetch;

d_lceq:  /* 0141503 */
  TRACE(T_FLOW, " LCEQ\n");
  putcrs16(A, LCEQ);
  goto fetch;

d_lcne:  /* 0141502 */
  TRACE(T_FLOW, " LCNE\n");
  putcrs16(A, LCNE);
  goto fetch;

d_lcge:  /* 0141504 */
  TRACE(T_FLOW, " LCGE\n");
  putcrs16(A, LCGE);
  goto fetch;

d_lcgt:  /* 0141505 */
  TRACE(T_FLOW, " LCGT\n");
  putcrs16(A, LCGT);
  goto fetch;

d_llt:  /* 0140410 */
  TRACE(T_FLOW, " LLT\n");
  SETCC_A;
  putcrs16(A, LCLT);
  goto fetch;

d_lle:  /* 0140411 */
  TRACE(T_FLOW, " LLE\n");
  SETCC_A;
  putcrs16(A, LCLE);
  goto fetch;

d_lne:  /* 0140412 */
  TRACE(T_FLOW, " LNE\n");
  SETCC_A;
  putcrs16(A, LCNE);
  goto fetch;

d_leq:  /* 0140413 */
  TRACE(T_FLOW, " LEQ\n");
  SETCC_A;
  putcrs16(A, LCEQ);
  goto fetch;

d_lge:  /* 0140414 */
  TRACE(T_FLOW, " LGE\n");
  SETCC_A;
  putcrs16(A, LCGE);
  goto fetch;

d_lgt:  /* 0140415 */
  TRACE(T_FLOW, " LGT\n");
  SETCC_A;
  putcrs16(A, LCGT);
  goto fetch;

d_llle:  /* 0141511 */
  TRACE(T_FLOW, " LLLE\n");
  SETCC_L;
  putcrs16(A, LCLE);
  goto fetch;

d_lleq:  /* 0141513 */
  TRACE(T_FLOW, " LLEQ\n");
  SETCC_L;
  putcrs16(A, LCEQ);
  goto fetch;

d_llne:  /* 0141512 */
  TRACE(T_FLOW, " LLNE\n");
  SETCC_L;
  putcrs16(A, LCNE);
  goto fetch;

d_llgt:  /* 0141515 */
  TRACE(T_FLOW, " LLGT\n");
  SETCC_L;
  putcrs16(A, LCGT);
  goto fetch;

d_lflt:  /* 0141110 */
  TRACE(T_FLOW, " LFLT\n");
  SETCC_F;
  putcrs16(A, LCLT);
  goto fetch;

d_lfle:  /* 0141111 */
  TRACE(T_FLOW, " LFLE\n");
  SETCC_F;
  putcrs16(A, LCLE);
  goto fetch;

d_lfeq:  /* 0141113 */
  TRACE(T_FLOW, " LFEQ\n");
  SETCC_F;
  putcrs16(A, LCEQ);
  goto fetch;

d_lfne:  /* 0141112 */
  TRACE(T_FLOW, " LFNE\n");
  SETCC_F;
  putcrs16(A, LCNE);
  goto fetch;

d_lfge:  /* 0141114 */
  TRACE(T_FLOW, " LFGE\n");
  SETCC_F;
  putcrs16(A, LCGE);
  goto fetch;

d_lfgt:  /* 0141115 */
  TRACE(T_FLOW, " LFGT\n");
  SETCC_F;
  putcrs16(A, LCGT);
  goto fetch;

d_flot:  /* 0140550 */
  TRACE(T_FLOW, " FLOT\n");
  templ = getcrs16s(A);
  templ = (templ<<15) | getcrs16(B);
  putcrs64d(FLTH, fltl(templ));
  goto fetch;

d_frn:  /* 0140534 */
  {
    int oflow;
    TRACE(T_FLOW, " FRN\n");
    CLEARC;
    putfr64(2, frn(getfr64(2), &oflow));
    if (oflow)
      mathexception('f', FC_DFP_OFLOW, 0);
  }
  goto fetch;

d_dfcm:  /* 0140574 */
  {
    int oflow;
    TRACE(T_FLOW, " DFCM\n");
dfcmfac1:
    putfr64(2, dfcm(getfr64(2), &oflow));
    if (oflow)
      mathexception('f', FC_DFP_OFLOW, 0);
    goto fetch;
  }

d_adll:  /* 0141000 */
  TRACE(T_FLOW, " ADLL\n");
  adlr(2);
  goto fetch;

d_fcmv:  /* 0140530 */
  TRACE(T_FLOW, " FCMv\n");
  goto dfcmfac1;

d_fsze:  /* 0140510 */
  TRACE(T_FLOW, " FSZE\n");
  if (getcrs32s(FLTH) == 0)
    INCRP;
  goto fetch;

d_fsnz:  /* 0140511 */
  TRACE(T_FLOW, " FSNZ\n");
  if (getcrs32s(FLTH) != 0)
    INCRP;
  goto fetch;

d_fsmi:  /* 0140512 */
  TRACE(T_FLOW, " FSMI\n");
  if (getcrs32s(FLTH) < 0)
    INCRP;
  goto fetch;

d_fspl:  /* 0140513 */
  TRACE(T_FLOW, " FSPL\n");
  if (getcrs32s(FLTH) >= 0)
    INCRP;
  goto fetch;

d_fsle:  /* 0140514 */
  TRACE(T_FLOW, " FSLE\n");
  if (getcrs32s(FLTH) <= 0)
    INCRP;
  goto fetch;

d_fsgt:  /* 0140515 */
  TRACE(T_FLOW, " FSGT\n");
  if (getcrs32s(FLTH) > 0)
    INCRP;
  goto fetch;

d_int:  /* 0140554 */
  TRACE(T_FLOW, " INTr\n");
  /* XXX: do -1073741824.5 and 1073741823.5 work on Prime, or overflow? */
  if (prieee8(getfr64(2), &tempd) && -1073741824.0 <= tempd && tempd <= 1073741823.0) {
    templ = tempd;
    putcrs16(B, templ & 0x7FFF);
    putcrs16(A, templ >> 15);
    CLEARC;
  } else
    mathexception('f', FC_INT_CONV, ea);
  goto fetch;

d_inta:  /* 0140531 */
  TRACE(T_FLOW, " INTA\n");
  /* XXX: do 32767.5 and -32768.5 work on Prime, or overflow? */
  if (prieee8(getfr64(2), &tempd) && -32768.0 <= tempd && tempd <= 32767.0) {
    putcrs16(A, (short)tempd);
    CLEARC;
  } else
    mathexception('f', FC_INT_CONV, ea);
  goto fetch;

d_flta:  /* 0140532 */
  TRACE(T_FLOW, " FLTA\n");
  tempd = getcrs16s(A);
  ieeepr8(tempd, (long long *)(crs+FLTH), 0);
  goto fetch;

d_intl:  /* 0140533 */
  TRACE(T_FLOW, " INTL\n");
  if (prieee8(getfr64(2), &tempd) && -2147483648.0 <= tempd && tempd <= 2147483647.0) {
    putcrs32s(L, (int)tempd);
    CLEARC;
  } else
    mathexception('f', FC_INT_CONV, ea);
  goto fetch;

d_fltl:  /* 0140535 */
  TRACE(T_FLOW, " FLTL\n");
  putcrs64d(FLTH, fltl(getgr32(GR2)));
  goto fetch;

d_bmle:  /* 0141711 */
  TRACE(T_FLOW, " BMLE\n");
  if (!(getcrs16(KEYS) & 020000))
    RPL = iget16(RP);
  else
    BCEQ;
  goto fetch;

#if 0
d_bmeq:  /* 0141602 */   /* same opcode as BCEQ */
  TRACE(T_FLOW, " BMEQ\n");
  goto bceq;

d_bmne:  /* 0141603 */   /* same opcode as BCNE */
  TRACE(T_FLOW, " BMNE\n");
  goto bcne;

  /* NOTE: BMGE is equivalent to BLS; this opcode doesn't exist
     in newer manuals */

d_bmge:  /* 0141606 */
  TRACE(T_FLOW, " BMGE\n");
  goto bls;
#endif

d_bmgt:  /* 0141710 */
  TRACE(T_FLOW, " BMGT\n");
  if (getcrs16(KEYS) & 020000)
    BCNE;
  else
    INCRP;
  goto fetch;

d_cre:  /* 0141404 */
  TRACE(T_FLOW, " CRE\n");
  putcrs32(E, 0);
  goto fetch;

d_crle:  /* 0141410 */
  TRACE(T_FLOW, " CRLE\n");
  putcrs64s(L, 0);
  goto fetch;

d_ile:  /* 0141414 */
  TRACE(T_FLOW, " ILE\n");
  templ = getcrs32s(L);
  putcrs32s(L, getcrs32s(E));
  putcrs32s(E, templ);
  goto fetch;

  /* these next 4 are V/I-mode quad mode:

      0140570:  QFCM - quad complement
      0140571:  DRNM - round minus Q to D
      0140572:  QINQ - trucate Q fraction
      0140573:  QIQR - round and remove Q fraction
  */
d_quii:
  TRACE(T_FLOW, " QFCM DRNM QINQ QIQR UII\n");
  fault(UIIFAULT, RPL, RP);
  fatal("Return from d_quii");

  /* queue instructions */

d_rtq:  /* 0141714 */
  TRACE(T_FLOW, " RTQ\n");
  ea = apea(NULL);
  if (rtq(ea, crs+A, RP))   /* does swap on store */
    CLEAREQ;
  else
    SETEQ;
  goto fetch;

d_rbq:  /* 0141715 */
  TRACE(T_FLOW, " RBQ\n");
  ea = apea(NULL);
  if (rbq(ea, crs+A, RP))  /* does swap on store */
    CLEAREQ;
  else
    SETEQ;
  goto fetch;

d_abq:  /* 0141716 */
  TRACE(T_FLOW, " ABQ\n");
  ea = apea(NULL);
  if (abq(ea, getcrs16(A), RP))
    CLEAREQ;
  else
    SETEQ;
  goto fetch;

d_atq:  /* 0141717 */
  TRACE(T_FLOW, " ATQ\n");
  ea = apea(NULL);
  if (atq(ea, getcrs16(A), RP))
    CLEAREQ;
  else
    SETEQ;
  goto fetch;

d_tstq:  /* 0141757 */
  TRACE(T_FLOW, " TSTQ\n");
  ea = apea(NULL);
  putcrs16(A, tstq(ea));
  SETCC_A;
  goto fetch;

 d_diagill:  /* 0141700 */

  /* XXX: hack for CPU.FAULT; not sure how to determine
     whether an instruction is illegal or unimplemented */
  
  fault(ILLINSTFAULT, RPL, RP);
  fatal("Return from 0141700 fault");

d_emcm:  /* 000503 - enter machine check mode */
  TRACE(T_FLOW, " EMCM\n");
  RESTRICT();
  goto fetch;

d_lmcm:  /* 000501 - leave machine check mode */
  TRACE(T_FLOW, " LMCM\n");
  RESTRICT();
  goto fetch;

d_rmc:  /* 000021 - reset machine check FF */
  TRACE(T_FLOW, " RMC\n");
  RESTRICT();
  goto fetch;

d_viry:  /* 000311 - ucode verify */
  TRACE(T_FLOW, " VIRY\n");
  RESTRICT();
  goto fetch;

d_xvfy:  /* 001113 - extended ucode verify */
  TRACE(T_FLOW, " XVFY\n");
  goto fetch;

  /* memory diagnostic opcodes:
      001304:  MDEI - enable interleave
      001305:  MDII - inhibit interleave
      001306:  MDRS - reset syndrome bits
      001307:  MDWC - write cache
      001324:  MDIW - inhibit wide-word mode?
  */

d_mdxx:  /* 01304-01307, 01324 */
  TRACE(T_FLOW, " MDxx\n");
  RESTRICT();
  goto fetch;


  /* this is a bit weird here: the shift group is really only for
     V-mode instructions, but Prime put some I-mode generics in
     the same instruction space.  Not sure, but I think a real
     Prime would probably take an illegal instruction fault on
     something like LRL executed in I-mode, but the emulator will
     just do it.  */

d_lrl:  /* 00000 - LRL */
  TRACE(T_FLOW, " LRL %d\n", shiftcount(inst));
  putgr32(GR2, lrl(getgr32(GR2), inst));
  goto fetch;

d_lrs:  /* 00100 - LRS (different in R & V modes) */
  TRACE(T_FLOW, " LRS %d\n", shiftcount(inst));
  if (getcrs16(KEYS) & 010000) {          /* V/I mode */
    putgr32(GR2, lrs(getgr32(GR2), inst));
  } else {
    scount = shiftcount(inst);
    utempa = getcrs16(B) & 0x8000;        /* save B bit 1 */
    if (scount <= 31) {
      templ = (getcrs16(A)<<16) | ((getcrs16(B) & 0x7FFF)<<1);
      EXPCL(templ & BITMASK32(32-scount));
      templ = templ >> (scount+1);
      putcrs16(A, templ >> 15);
      putcrs16(B, (templ & 0x7FFF) | utempa);
    } else if (getcrs16(A) & 0x8000) {
      putcrs32(A, 0xFFFF7FFF | utempa);
      SETCL;
    } else {
      CLEARCL;
      putcrs32(A, utempa);
    }
  }
  goto fetch;

d_lrr:  /* 00200 - LRR */
  TRACE(T_FLOW, " LRR %d\n", shiftcount(inst));
  putgr32(GR2, lrr(getgr32(GR2), inst));
  goto fetch;

d_300shift:  /* 00300 - generic extension */
  switch (inst) {

  case 0040310: /* SSSN */
    sssn();
    break;

  case 0040300: /* DRN */
  case 0040301: /* DRNP */
  case 0040302: /* DRNZ */
  case 0040303: /* FRNP */
  case 0040320: /* FRNM */
  case 0040321: /* FRNZ */
    TRACE(T_FLOW, " DRNx/FRNx(V) UII\n");
    fault(UIIFAULT, RPL, RP);
    break;

  default:
    goto d_badshift;
  }
  goto fetch;

d_arl:  /* 00400 - ARL */
  TRACE(T_FLOW, " ARL %d\n", shiftcount(inst));
  putcrs16(A, arl(getcrs16(A), inst));
  goto fetch;

d_ars:  /* 00500 - ARS */
  TRACE(T_FLOW, " ARS %d\n", shiftcount(inst));
  putcrs16(A, ars(getcrs16(A), inst));
  goto fetch;

d_arr:  /* 00600 - ARR */
  TRACE(T_FLOW, " ARR %d\n", shiftcount(inst));
  putcrs16(A, arr(getcrs16(A), inst));
  goto fetch;

d_lll:  /* 01000 - LLL */
  TRACE(T_FLOW, " LLL %d\n", shiftcount(inst));
  putgr32(GR2, lll(getgr32(GR2), inst));
  goto fetch;

d_lls:  /* 01100 - LLS (different in R/V modes) */
  TRACE(T_FLOW, " LLS %d\n", shiftcount(inst));
  if (getcrs16(KEYS) & 010000)                /* V/I mode */
    putgr32(GR2, lls(getgr32(GR2), inst));
  else {
    scount = shiftcount(inst);
    utempa = getcrs16(B) & 0x8000;            /* save B bit 1 */
    if (scount < 31) {
      utempl = (getcrs16(A)<<16) | ((getcrs16(B) & 0x7FFF)<<1);
      templ2 = 0x80000000;
      templ2 = templ2 >> scount;         /* create mask */
      templ2 = templ2 & utempl;          /* grab bits */
      templ2 = templ2 >> (31-scount);    /* sign extend them */
      EXPCL(!(templ2 == -1 || templ2 == 0));
      //printf(" before: A=%x, B=%x, utempl=%x, ", getcrs16(A), getcrs16(B), utempl);
      utempl = utempl << scount;
      putcrs16(A, utempl >> 16);
      putcrs16(B, ((utempl >> 1) & 0x7FFF) | utempa);
      //printf(" after: A=%x, B=%x, utempl=%x\n", getcrs16(A), getcrs16(B), utempl);
    } else {
      EXPCL(getcrs32(A) != 0);
      putcrs32(A, utempa);   /* XXX: this looks wrong - JW 10/14/2011 */
    }
  }
  if ((getcrs16(KEYS) & 0100400) == 0100400)
    mathexception('i', FC_INT_OFLOW, 0);
  goto fetch;

d_llr:  /* 01200 - LLR */
  TRACE(T_FLOW, " LLR %d\n", shiftcount(inst));
  putgr32(GR2, llr(getgr32(GR2), inst));
  goto fetch;

d_all:  /* 01400 - ALL */
  TRACE(T_FLOW, " ALL %d\n", shiftcount(inst));
  putcrs16(A, all(getcrs16(A), inst));
  goto fetch;

d_als:  /* 01500 - ALS */
  TRACE(T_FLOW, " ALS %d\n", shiftcount(inst));
  putcrs16(A, als(getcrs16(A), inst));
  if ((getcrs16(KEYS) & 0100400) == 0100400)
    mathexception('i', FC_INT_OFLOW, 0);
  goto fetch;

d_alr:  /* 01600 - ALR */
  TRACE(T_FLOW, " ALR %d\n", shiftcount(inst));
  putcrs16(A, alr(getcrs16(A), inst));
  goto fetch;

d_badshift:
  printf("emulator warning: unrecognized class 1 (shift) generic instruction %06o at %o/%o\n", inst, RPH, RPL);
  TRACE(T_FLOW, " unrecognized shift instruction!: %o\n", inst);
  goto fetch;


  /* class 2 generic instructions (skip group) */

d_nopskp:  /* 0101000 */
  TRACE(T_FLOW, " NOP-SKP\n");
  goto fetch;

d_skp:  /* 0100000 */
  TRACE(T_FLOW, " SKP\n");
  INCRP;
  goto fetch;

d_smi:  /* 0101400 */
  TRACE(T_FLOW, " SMI/SLT\n");
  if (getcrs16s(A) < 0)
    INCRP;
  goto fetch;

d_spl:  /* 0100400 */
  TRACE(T_FLOW, " SPL/SGE\n");
  if (getcrs16s(A) >= 0)
    INCRP;
  goto fetch;

d_sln:  /* 0101100 */
  TRACE(T_FLOW, " SLN\n");
  if (getcrs16(A) & 1)
    INCRP;
  goto fetch;

d_slz:  /* 0100100 */
  TRACE(T_FLOW, " SLZ\n");
  if (!(getcrs16(A) & 1))
    INCRP;
  goto fetch;

d_snz:  /* 0101040 */
  TRACE(T_FLOW, " SNZ/SNE\n");
  if (getcrs16(A) != 0)
    INCRP;
  goto fetch;

d_sze:  /* 0100040 */
  TRACE(T_FLOW, " SZE/SEQ\n");
  if (getcrs16(A) == 0)
    INCRP;
  goto fetch;

d_sle:  /* 0101220 */
  TRACE(T_FLOW, " SLE\n");
  if (getcrs16s(A) <= 0)
    INCRP;
  goto fetch;

d_sgt:  /* 0100220 */
  TRACE(T_FLOW, " SGT\n");
  if (getcrs16s(A) > 0)
    INCRP;
  goto fetch;

d_ssc:  /* 0101001 */
  TRACE(T_FLOW, " SSC\n");
  if (getcrs16(KEYS) & 0100000)
    INCRP;
  goto fetch;

d_src:  /* 0100001 */
  TRACE(T_FLOW, " SRC\n");
  if (!(getcrs16(KEYS) & 0100000))
    INCRP;
  goto fetch;

d_sar:  /* 0100260 - 0100277 */
  m = (inst & 017)+1;
  TRACE(T_FLOW, " SAR %d\n", m);
  if (!(getcrs16(A) & BITMASK16(m)))
    INCRP;
  goto fetch;

d_sas:  /* 0101260 - 0101277 */
  m = (inst & 017)+1;
  TRACE(T_FLOW, " SAS %d\n", m);
  if (getcrs16(A) & BITMASK16(m))
    INCRP;
  goto fetch;

d_snr:  /* 0100240 - 0100257 */
  m = (inst & 017)+1;
  TRACE(T_FLOW, " SNR %d\n", m);
  RESTRICT();
  if (!(sswitch & BITMASK16(m)))
    INCRP;
  goto fetch;

d_sns:  /* 0101240 - 0101257 */
  m = (inst & 017)+1;
  TRACE(T_FLOW, " SNS %d\n", m);
  RESTRICT();
  if (sswitch & BITMASK16(m))
    INCRP;
  goto fetch;

d_ssx:   /* 0101020, 0101010, 0101004, 0101002: SS1-4, SSS */
  m = (inst & 036) << 11;
  TRACE(T_FLOW, " SSx\n");
  RESTRICT();
  if (sswitch & m)
    INCRP;
  goto fetch;

d_srx:   /* 0100020, 0100010, 0100004, 0100002: SR1-4, SRS */
  m = (inst & 036) << 11;
  TRACE(T_FLOW, " SRx\n");
  RESTRICT();
  if (!(sswitch & m))
    INCRP;
  goto fetch;

d_smcr:  /* 0100200 */
  TRACE(T_FLOW, " SMCR\n");
  RESTRICT();
  INCRP;
  goto fetch;

d_smcs:  /* 0101200 */
  TRACE(T_FLOW, " SMCS\n");
  RESTRICT();
  goto fetch;

d_badgen:
  TRACEA(" unrecognized generic instruction!\n");
  printf("em: #%u %o/%o: Unrecognized generic instruction '%o!\n", gv.instcount, RPH, RPL, inst);
  fault(UIIFAULT, RPL, RP);
  fatal(NULL);


imode:

    /* branch and register generic instructions don't have ea, so they
       are tested outside the main switch, before an ea is computed */

  opcode = inst >> 10;
  dr = (inst >> 7) & 7;

  if (opcode == 010) {               /* register branch */
    brop = inst & 0177;
    switch (brop) {
    case 0100:
      TRACE(T_FLOW, " BRLE\n");
      SETCC_32(getgr32(dr));
      BCLE;
      break;

    case 0101:
      TRACE(T_FLOW, " BRGT\n");
      SETCC_32(getgr32(dr));
      BCGT;
      break;

    case 0102:
      TRACE(T_FLOW, " BREQ\n");
      SETCC_32(getgr32(dr));
      BCEQ;
      break;

    case 0103:
      TRACE(T_FLOW, " BRNE\n");
      SETCC_32(getgr32(dr));
      BCNE;
      break;

    case 0104:
      TRACE(T_FLOW, " BRLT\n");
      SETCC_32(getgr32(dr));
      BCLT;
      break;

    case 0105:
      TRACE(T_FLOW, " BRGE\n");
      SETCC_32(getgr32(dr));
      BCGE;
      break;

    case 0110:
      TRACE(T_FLOW, " BHLE\n");
      SETCC_16(getgr16(dr));
      BCLE;
      break;

    case 0111:
      TRACE(T_FLOW, " BHGT\n");
      SETCC_16(getgr16(dr));
      BCGT;
      break;

    case 0112:
      TRACE(T_FLOW, " BHEQ\n");
      SETCC_16(getgr16(dr));
      BCEQ;
      break;

    case 0113:
      TRACE(T_FLOW, " BHNE\n");
      SETCC_16(getgr16(dr));
      BCNE;
      break;

    case 0114:
      TRACE(T_FLOW, " BHLT\n");
      SETCC_16(getgr16(dr));
      BCLT;
      break;

    case 0115:
      TRACE(T_FLOW, " BHGE\n");
      SETCC_16(getgr16(dr));
      BCGE;
      break;

    case 0120:
      TRACE(T_FLOW, " BFLE\n");
      SETCC_32(getfr32(dr));
      BCLE;
      break;

    case 0121:
      TRACE(T_FLOW, " BFGT\n");
      SETCC_32(getfr32(dr));
      BCGT;
      break;

    case 0122:
      TRACE(T_FLOW, " BFEQ\n");
      SETCC_32(getfr32(dr));
      BCEQ;
      break;

    case 0123:
      TRACE(T_FLOW, " BFNE\n");
      SETCC_32(getfr32(dr));
      BCNE;
      break;

    case 0124:
      TRACE(T_FLOW, " BFLT\n");
      SETCC_32(getfr32(dr));
      BCLT;
      break;

    case 0125:
      TRACE(T_FLOW, " BFGE\n");
      SETCC_32(getfr32(dr));
      BCGE;
      break;

    case 0130:
      TRACE(T_FLOW, " BRI1\n");
      putgr32(dr, getgr32(dr) + 1);
      BRNE(dr);
      break;

    case 0131:
      TRACE(T_FLOW, " BRI2\n");
      putgr32(dr, getgr32(dr) + 2);
      BRNE(dr);
      break;

    case 0132:
      TRACE(T_FLOW, " BRI4\n");
      putgr32(dr, getgr32(dr) + 4);
      BRNE(dr);
      break;

    case 0134:
      TRACE(T_FLOW, " BRD1\n");
      putgr32(dr, getgr32(dr) - 1);
      BRNE(dr);
      break;

    case 0135:
      TRACE(T_FLOW, " BRD2\n");
      putgr32(dr, getgr32(dr) - 2);
      BRNE(dr);
      break;

    case 0136:
      TRACE(T_FLOW, " BRD4\n");
      putgr32(dr, getgr32(dr) - 4);
      BRNE(dr);
      break;

    case 0140:
      TRACE(T_FLOW, " BHI1\n");
      putgr16(dr, getgr16(dr) + 1);
      BHNE(dr);
      break;

    case 0141:
      TRACE(T_FLOW, " BHI2\n");
      putgr16(dr, getgr16(dr) + 2);
      BHNE(dr);
      break;

    case 0142:
      TRACE(T_FLOW, " BHI4\n");
      putgr16(dr, getgr16(dr) + 4);
      BHNE(dr);
      break;

    case 0144:
      TRACE(T_FLOW, " BHD1\n");
      putgr16(dr, getgr16(dr) - 1);
      BHNE(dr);
      break;

    case 0145:
      TRACE(T_FLOW, " BHD2\n");
      putgr16(dr, getgr16(dr) - 2);
      BHNE(dr);
      break;

    case 0146:
      TRACE(T_FLOW, " BHD4\n");
      putgr16(dr, getgr16(dr) - 4);
      BHNE(dr);
      break;

    default:
      if (brop <= 037) {
	TRACE(T_FLOW, " BRBS\n");
	if (getgr32(dr) & BITMASK32(brop+1))
	  RPL = iget16(RP);
	else
	  INCRP;
      } else if (brop <= 077) {
	TRACE(T_FLOW, " BRBR\n");
	if (getgr32(dr) & BITMASK32(brop-040+1))
	  INCRP;
	else
	  RPL = iget16(RP);
      } else
	fault(UIIFAULT, RPL, RP);
    }
    goto fetch;
  }

  if (opcode == 030) {       /* register generic */
    switch (inst & 0177) {
    case 0134:
      TRACE(T_FLOW, " ABQ\n");
      ea = apea(NULL);
      if (abq(ea, getgr16(dr), RP))
	CLEAREQ;
      else
	SETEQ;
      break;

    case 0014:
      TRACE(T_FLOW, " ADLR\n");
      adlr(dr);
      break;

    case 0161:
      TRACE(T_FLOW, " ARFA 0\n");
      arfa(0, getgr32(dr));
      break;

    case 0171:
      TRACE(T_FLOW, " ARFA 1\n");
      arfa(1, getgr32(dr));
      break;

    case 0135:
      TRACE(T_FLOW, " ATQ\n");
      ea = apea(NULL);
      if (atq(ea, getgr16(dr), RP))
	CLEAREQ;
      else
	SETEQ;
      break;

    case 0026:
      TRACE(T_FLOW, " CGT\n");
      cgt(getgr16(dr));
      break;

    case 0040:
      TRACE(T_FLOW, " CHS\n");
      putgr32(dr, getgr32(dr) ^ 0x80000000);
      break;

    case 045:
      TRACE(T_FLOW, " CMH\n");
      putgr16(dr, ~getgr16(dr));
      break;

    case 044:
      TRACE(T_FLOW, " CMR\n");
      putgr32(dr, ~getgr32(dr));
      break;

    case 0056:
      TRACE(T_FLOW, " CR\n");
      putgr32(dr, 0);
      break;

    case 0062:
      TRACE(T_FLOW, " CRBL\n");
      putgr32(dr, getgr32(dr) & 0x00FFFFFF);
      break;

    case 0063:
      TRACE(T_FLOW, " CRBR\n");
      putgr32(dr, getgr32(dr) & 0xFF00FFFF);
      break;

    case 0054:
      TRACE(T_FLOW, " CRHL\n");
      putgr32(dr, getgr32(dr) & 0x0000FFFF);
      break;

    case 0055:
      TRACE(T_FLOW, " CRHR\n");
      putgr32(dr, getgr32(dr) & 0xFFFF0000);
      break;

    case 0041:
      TRACE(T_FLOW, " CSR\n");
      putcrs16(KEYS, (getcrs16(KEYS) & 0x7FFF) | (getgr16(dr) & 0x8000));
      putgr32(dr, getgr32(dr) & 0x7FFFFFFF);
      break;

    case 0106:
      TRACE(T_FLOW, " DBLE\n");
      putgr32(FAC0+dr+1, getgr32(FAC0+dr+1) & 0x0000FFFF);
      break;

    case 0160:
      TRACE(T_FLOW, " DCP\n");
#if 0
      utempl = EACP(getgr32(dr));
      utempl--;
      putgr32(dr, CPEA(getgr32(dr), utempl));
#else
      putgr32(dr, getgr32(dr) ^ EXTMASK32);
      if (getgr32(dr) & EXTMASK32)
	putgr32(dr, getgr32(dr) - 1);
#endif
      break;

    case 0144:
      {
	int oflow;
	TRACE(T_FLOW, " DFCM\n");
dfcmdr:
	putfr64(dr, dfcm(getfr64(dr), &oflow));
	if (oflow)
	  mathexception('f', FC_DFP_OFLOW, 0);
      }
      break;

    case 0130:
      TRACE(T_FLOW, " DH1\n");
      putgr16(dr, add16(getgr16(dr), 0xFFFF, 0, 0));
      break;

    case 0131:
      TRACE(T_FLOW, " DH2\n");
      putgr16(dr, add16(getgr16(dr), 0xFFFE, 0, 0));
      break;

    case 0124:
      TRACE(T_FLOW, " DR1\n");
      putgr32(dr, add32(getgr32(dr), 0xFFFFFFFF, 0, 0));
      break;

    case 0125:
      TRACE(T_FLOW, " DR2\n");
      putgr32(dr, add32(getgr32(dr), 0xFFFFFFFE, 0, 0));
      break;

    case 0100:
      TRACE(T_FLOW, " FCM\n");
      goto dfcmdr;
      break;

    case 0105:
      TRACE(T_FLOW, " FLT 0\n");
      putfr64d(0, fltl(getgr32(dr)));
      break;

    case 0115:
      TRACE(T_FLOW, " FLT 1\n");
      putfr64d(2, fltl(getgr32(dr)));   //XXX: 2=FPR 1 ! */
      break;

    case 0102:
      TRACE(T_FLOW, " FLTH 0\n");
      putfr64d(0, fltl(getgr16s(dr)));
      break;

    case 0112:
      TRACE(T_FLOW, " FLTH 1\n");
      putfr64d(2, fltl(getgr16s(dr)));
      break;

    case 0107:
      {
	int oflow;
	TRACE(T_FLOW, " FRN\n");
	CLEARC;
	putfr64(dr, frn(getfr64(dr), &oflow));
	if (oflow)
	  mathexception('f', FC_DFP_OFLOW, 0);
      }
      break;

    case 0146:  /* I-mode FRNM */
    case 0145:  /* I-mode FRNP */
    case 0147:  /* I-mode FRNZ */
      TRACE(T_FLOW, " FRNx(I) UII\n");
      fault(UIIFAULT, RPL, RP);
      break;

    case 0065:
      TRACE(T_FLOW, " ICBL\n");
      putgr16(dr, getgr16(dr)>>8);
      break;

    case 0066:
      TRACE(T_FLOW, " ICBR\n");
      putgr16(dr, getgr16(dr)<<8);
      break;

    case 0060:
      TRACE(T_FLOW, " ICHL\n");
      putgr32(dr, getgr32(dr)>>16);
      break;

    case 0061:
      TRACE(T_FLOW, " ICHR\n");
      putgr32(dr, getgr32(dr)<<16);
      break;

    case 0167:
      TRACE(T_FLOW, " ICP\n");
#if 0
      utempl = EACP(getgr32(dr));
      utempl++;
      putgr32(dr, CPEA(getgr32(dr), utempl));
#else
      putgr32(dr, getgr32(dr) ^ EXTMASK32);
      if ((getgr32(dr) & EXTMASK32) == 0)
	putgr32(dr, getgr32(dr) + 1);
#endif
      break;

    case 0126:
      TRACE(T_FLOW, " IH1\n");
      putgr16(dr, add16(getgr16(dr), 1, 0, 0));
      break;

    case 0127:
      TRACE(T_FLOW, " IH2\n");
      putgr16(dr, add16(getgr16(dr), 2, 0, 0));
      break;

    case 0070:
      TRACE(T_FLOW, " INK\n");
      putgr16(dr, getcrs16(KEYS));    /* IXX: says to read S register? */
      break;

    case 0103:
      TRACE(T_FLOW, " INT 0\n");
      if (prieee8(getfr64(0), &tempd) && -2147483648.0 <= tempd && tempd <= 2147483647.0) {
	putgr32s(dr, (int)tempd);
	CLEARC;
      } else
	mathexception('f', FC_INT_CONV, ea);
      break;

    case 0113:
      TRACE(T_FLOW, " INT 1\n");
      if (prieee8(getfr64(2), &tempd) && -2147483648.0 <= tempd && tempd <= 2147483647.0) {
	putgr32s(dr, (int)tempd);
	CLEARC;
      } else
	mathexception('f', FC_INT_CONV, ea);
      break;

    case 0101:
      TRACE(T_FLOW, " INTH 0\n");
      if (prieee8(getfr64(0), &tempd) && -32768.0 <= tempd && tempd <= 32767.0) {
	putgr16s(dr, (short)tempd);
	CLEARC;
      } else
	mathexception('f', FC_INT_CONV, ea);
      break;

    case 0111:
      TRACE(T_FLOW, " INTH 1\n");
      if (prieee8(getfr64(2), &tempd) && -32768.0 <= tempd && tempd <= 32767.0) {
	putgr16s(dr, (short)tempd);
	CLEARC;
      } else
	mathexception('f', FC_INT_CONV, ea);
      break;

    case 0122:
      TRACE(T_FLOW, " IR1\n");
      putgr32(dr, add32(getgr32(dr), 1, 0, 0));
      break;

    case 0123:
      TRACE(T_FLOW, " IR2\n");
      putgr32(dr, add32(getgr32(dr), 2, 0, 0));
      break;

    case 0064:
      TRACE(T_FLOW, " IRB\n");
      putgr16(dr, (getgr16(dr)>>8) | (getgr16(dr)<<8));
      break;

    case 0057:
      TRACE(T_FLOW, " IRH\n");
      putgr32(dr, (getgr32(dr)>>16) | (getgr32(dr)<<16));
      break;

    case 0153:
      TRACE(T_FLOW, " LCEQ\n");
      putgr16(dr, LCEQ);
      break;

    case 0154:
      TRACE(T_FLOW, " LCGE\n");
      putgr16(dr, LCGE);
      break;

    case 0155:
      TRACE(T_FLOW, " LCGT\n");
      putgr16(dr, LCGT);
      break;

    case 0151:
      TRACE(T_FLOW, " LCLE\n");
      putgr16(dr, LCLE);
      break;

    case 0150:
      TRACE(T_FLOW, " LCLT\n");
      putgr16(dr, LCLT);
      break;

    case 0152:
      TRACE(T_FLOW, " LCNE\n");
      putgr16(dr, LCNE);
      break;

    case 0162:
      TRACE(T_FLOW, " LDC 0\n");
      putgr16(dr, ldc(0, getgr16(dr)));
      break;

    case 0172:
      TRACE(T_FLOW, " LDC 1\n");
      putgr16(dr, ldc(1, getgr16(dr)));
      break;

    case 0003:
      TRACE(T_FLOW, " LEQ\n");
      SETCC_32(getgr32(dr));
      putgr16(dr, LCEQ);
      break;

    case 0016:
      TRACE(T_FLOW, " LF\n");
      putgr16(dr, 0);
      break;

    case 0023:
      TRACE(T_FLOW, " LFEQ 0\n");
      SETCC_32(getgr32(FAC0));
      putgr16(dr, LCEQ);
      break;

    case 0033:
      TRACE(T_FLOW, " LFEQ 1\n");
      SETCC_32(getgr32(FAC1));
      putgr16(dr, LCEQ);
      break;

    case 0024:
      TRACE(T_FLOW, " LFGE 0\n");
      SETCC_32(getgr32(FAC0));
      putgr16(dr, LCGE);
      break;

    case 0034:
      TRACE(T_FLOW, " LFGE 1\n");
      SETCC_32(getgr32(FAC1));
      putgr16(dr, LCGE);
      break;

    case 0025:
      TRACE(T_FLOW, " LFGT 0\n");
      SETCC_32(getgr32(FAC0));
      putgr16(dr, LCGT);
      break;

    case 0035:
      TRACE(T_FLOW, " LFGT 1\n");
      SETCC_32(getgr32(FAC1));
      putgr16(dr, LCGT);
      break;

    case 0021:
      TRACE(T_FLOW, " LFLE 0\n");
      SETCC_32(getgr32(FAC0));
      putgr16(dr, LCLE);
      break;

    case 0031:
      TRACE(T_FLOW, " LFLE 1\n");
      SETCC_32(getgr32(FAC1));
      putgr16(dr, LCLE);
      break;

    case 0020:
      TRACE(T_FLOW, " LFLT 0\n");
      SETCC_32(getgr32(FAC0));
      putgr16(dr, LCLT);
      break;

    case 0030:
      TRACE(T_FLOW, " LFLT 1\n");
      SETCC_32(getgr32(FAC1));
      putgr16(dr, LCLT);
      break;

    case 0022:
      TRACE(T_FLOW, " LFNE 0\n");
      SETCC_32(getgr32(FAC0));
      putgr16(dr, LCNE);
      break;

    case 0032:
      TRACE(T_FLOW, " LFNE 1\n");
      SETCC_32(getgr32(FAC1));
      putgr16(dr, LCNE);
      break;

    case 0004:
      TRACE(T_FLOW, " LGE/LHGE\n");
      SETCC_32(getgr32(dr));
      putgr16(dr, LCGE);
      break;

    case 0005:
      TRACE(T_FLOW, " LGT\n");
      SETCC_32(getgr32(dr));
      putgr16(dr, LCGT);
      break;

    case 0013:
      TRACE(T_FLOW, " LHEQ\n");
      SETCC_16(getgr16(dr));
      putgr16(dr, LCEQ);
      break;

    case 0015:
      TRACE(T_FLOW, " LHGT\n");
      SETCC_16(getgr16(dr));
      putgr16(dr, LCGT);
      break;

    case 0011:
      TRACE(T_FLOW, " LHLE\n");
      SETCC_16(getgr16(dr));
      putgr16(dr, LCLE);
      break;

    case 0000:
      TRACE(T_FLOW, " LLT/LHLT\n");
      SETCC_32(getgr32(dr));
      putgr16(dr, LCLT);
      break;

    case 0012:
      TRACE(T_FLOW, " LHNE\n");
      SETCC_16(getgr16(dr));
      putgr16(dr, LCNE);
      break;

    case 0001:
      TRACE(T_FLOW, " LLE\n");
      SETCC_32(getgr32(dr));
      putgr16(dr, LCLE);
      break;

    case 0002:
      TRACE(T_FLOW, " LNE\n");
      SETCC_32(getgr32(dr));
      putgr16(dr, LCNE);
      break;

    case 0017:
      TRACE(T_FLOW, " LT\n");
      putgr16(dr, 1);
      break;

    case 0071:
      TRACE(T_FLOW, " OTK\n");
      newkeys(getgr16(dr) & 0177770);
      gv.inhcount = 1;
      break;

    case 0052:
      TRACE(T_FLOW, " PID\n");
      putgr64s(dr, getgr32s(dr));
      break;

    case 0053:
      TRACE(T_FLOW, " PIDH\n");
      putgr32s(dr, ((int)getgr32(dr)) >> 16);
      break;

    case 0050:
      TRACE(T_FLOW, " PIM\n");
      dr &= 6;                    /* force dr to be even */
      utempl = getgr32(dr);
      putgr32(dr, getgr32(dr+1));
      if (((utempl ^ getgr32(dr+1)) & 0x80000000) || (utempl != 0 && ~utempl != 0))
	mathexception('i', FC_INT_OFLOW, 0);
      else
	CLEARC;
      break;

    case 0051:
      TRACE(T_FLOW, " PIMH\n");
      pimh(dr);
      break;

    case 0133:
      TRACE(T_FLOW, " RBQ\n");
      ea = apea(NULL);
      if (rbq(ea, crs+dr*2, RP))
	CLEAREQ;
      else
	SETEQ;
      break;

    case 0132:
      TRACE(T_FLOW, " RTQ\n");
      ea = apea(NULL);
      if (rtq(ea,crs+dr*2,RP))
	CLEAREQ;
      else
	SETEQ;
      break;

    case 0076:
      TRACE(T_FLOW, " SHL1\n");
      if (getgr16(dr) & 0x8000)
	SETCL;
      else
	CLEARCL;
      putgr16(dr, getgr16(dr) << 1);
      break;

    case 0077:
      TRACE(T_FLOW, " SHL2\n");
      if (getgr16(dr) & 0x4000)
	SETCL;
      else
	CLEARCL;
      putgr16(dr, getgr16(dr) << 2);
      break;

    case 0120:
      TRACE(T_FLOW, " SHR1\n");
      if (getgr16(dr) & 0x0001)
	SETCL;
      else
	CLEARCL;
      putgr16(dr, getgr16(dr) >> 1);
      break;

    case 0121:
      TRACE(T_FLOW, " SHR2\n");
      if (getgr16(dr) & 0x0002)
	SETCL;
      else
	CLEARCL;
      putgr16(dr, getgr16(dr) >> 2);
      break;

    case 0072:
      TRACE(T_FLOW, " SL1\n");
      if (getgr32(dr) & 0x80000000)
	SETCL;
      else
	CLEARCL;
      putgr32(dr, getgr32(dr) << 1);
      break;

    case 0073:
      TRACE(T_FLOW, " SL2\n");
      if (getgr32(dr) & 0x40000000)
	SETCL;
      else
	CLEARCL;
      putgr32(dr, getgr32(dr) << 2);
      break;

    case 0074:
      TRACE(T_FLOW, " SR1\n");
      if (getgr32(dr) & 0x00000001)
	SETCL;
      else
	CLEARCL;
      putgr32(dr, getgr32(dr) >> 1);
      break;

    case 0075:
      TRACE(T_FLOW, " SR2\n");
      if (getgr32(dr) & 0x00000002)
	SETCL;
      else
	CLEARCL;
      putgr32(dr, getgr32(dr) >> 2);
      break;

    case 0042:
      TRACE(T_FLOW, " SSM\n");
      putgr32(dr, getgr32(dr) | 0x80000000);
      break;

    case 0043:
      TRACE(T_FLOW, " SSP\n");
      putgr32(dr, getgr32(dr) & 0x7FFFFFFF);
      break;

    case 0166:
      TRACE(T_FLOW, " STC 0\n");
      stc(0, getgr16(dr));
      break;

    case 0176:
      TRACE(T_FLOW, " STC 1\n");
      stc(1, getgr16(dr));
      break;

    case 0137:
      TRACE(T_FLOW, " STCD\n");
      ea = apea(NULL);
      if (get32(ea) == getgr32(dr+1)) {
	put32(getgr32(dr), ea);
	SETEQ;
      } else 
	CLEAREQ;
      break;

    case 0136:
      TRACE(T_FLOW, " STCH\n");
      ea = apea(NULL);
      if (get16(ea) == (getcrs16(dr*2+1))) {
	put16(getgr16(dr), ea);
	SETEQ;
      } else 
	CLEAREQ;
      break;

    case 0027:
      TRACE(T_FLOW, " STEX\n");
      putgr32(dr, stex(getgr32(dr)));
      break;

    case 0046:
      TRACE(T_FLOW, " TC\n");
      putgr32(dr, tcr(getgr32(dr)));
      break;

    case 0047:
      TRACE(T_FLOW, " TCH\n");
      putgr16(dr, tch(getgr16(dr)));
      break;

    case 0170:
      TRACE(T_FLOW, " TCNP\n");
      if ((getgr32(dr) & 0x1FFFFFFF) == 0)
	SETEQ;
      else
	CLEAREQ;
      break;

    case 0163:
      TRACE(T_FLOW, " TFLR 0\n");
      putgr32(dr, GETFLR(0));
      break;

    case 0173:
      TRACE(T_FLOW, " TFLR 1\n");
      putgr32(dr, GETFLR(1));
      break;

    case 0165:
      TRACE(T_FLOW, " TRFL 0\n");
      PUTFLR(0, getgr32(dr));
      break;

    case 0175:
      TRACE(T_FLOW, " TRFL 1\n");
      PUTFLR(1, getgr32(dr));
      break;


    case 0104:
      TRACE(T_FLOW, " TSTQ\n");
      ea = apea(NULL);
      putgr16(dr, tstq(ea));
      SETCC_16(getgr16(dr));
      break;

    default:
      warn("IXX 030");
      fault(ILLINSTFAULT, RPL, RP);
    }
    goto fetch;
  }

  ea = ea32i(earp, inst, &immu32, &immu64);

  switch (opcode) {

  case 000:
    /* this should have been handled already! */
    fatal("I-mode generic class 0?");

  case 001:
    TRACE(T_FLOW, " L\n");
    if (*(int *)&ea < 0)
      putgr32(dr, immu32);
    else
      putgr32(dr, get32(ea));
    goto fetch;

  case 002:
    TRACE(T_FLOW, " A\n");
    if (*(int *)&ea < 0)
      utempl = immu32;
    else
      utempl = get32(ea);
    putgr32(dr, add32(getgr32(dr), utempl, 0, ea));
    goto fetch;

  case 003:
    TRACE(T_FLOW, " N\n");
    if (*(int *)&ea < 0)
      putgr32(dr, getgr32(dr) & immu32);
    else
      putgr32(dr, getgr32(dr) & get32(ea));
    goto fetch;

  case 004:
    TRACE(T_FLOW, " LHL1\n");
    if (*(int *)&ea < 0)
      putgr16(dr, (immu32 >> 16) << 1);
    else
      putgr16(dr, get16(ea) << 1);
    goto fetch;

  case 005:
    TRACE(T_FLOW, " SHL\n");
    switch ((ea >> 14) & 3) {
    case 0:
      putgr32(dr, lll(getgr32(dr), ea));
      break;
    case 1:
      putgr16(dr, all(getgr16(dr), ea));
      break;
    case 2:
      putgr32(dr, lrl(getgr32(dr), ea));
      break;
    case 3:
      putgr16(dr, arl(getgr16(dr), ea));
      break;
    default:
      warn("I-mode SHL switch?");
      fault(ILLINSTFAULT, RPL, RP);
    }
    goto fetch;

  case 006:  /* Special MR FP format */
    /* FL, DFL, FC, DFC */
    switch (dr) {
    case 0:
    case 2:
      dr &= 2;
      TRACE(T_FLOW, " FL\n");
      if (*(int *)&ea < 0)
	putgr64s(FAC0+dr, immu64);
      else {
	utempl = get32(ea);
	putgr32(FAC0+dr,   utempl & 0xFFFFFF00);
	putgr32(FAC0+dr+1, utempl & 0x000000FF);
      }
      break;

    case 1:
    case 3:
      dr &= 2;
      TRACE(T_FLOW, " DFL\n");
      if (*(int *)&ea < 0)
	putgr64s(FAC0+dr, immu64);
      else
	putgr64s(FAC0+dr, get64(ea));
      break;

    case 4:
    case 6:
      dr &= 2;
      TRACE(T_FLOW, " FC\n");
      if (*(int *)&ea < 0)
	utempl = ((immu64 >> 32) & 0xffffff00) | (immu64 & 0xff);
      else
	utempl = get32(ea);
      fcs(getfr64(dr), utempl);
      break;

    case 5:
    case 7:
      dr &= 2;
      TRACE(T_FLOW, " DFC\n");
      if (*(int *)&ea >= 0)
	immu64 = get64(ea);
      dfcs(getfr64(dr), immu64);
    break;

    default:
      warn("I-mode 006 switch?");
      fault(ILLINSTFAULT, RPL, RP);
    }
    goto fetch;

  case 007:
    warn("I-mode opcode 007?");
    fault(ILLINSTFAULT, RPL, RP);

  case 010:                            /* register generic branch */
    /* this should have been handled already! */
    fatal("I-mode RGBR?");

  case 011:
    TRACE(T_FLOW, " LH\n");
    if (*(int *)&ea < 0) {
      TRACE(T_FLOW, " ea=%x, immu32=%x, gr%d=%x\n", ea, immu32, dr, getgr32(dr));
      putgr16(dr, immu32 >> 16);
    } else
      putgr16(dr, get16(ea));
    goto fetch;

  case 012:
    TRACE(T_FLOW, " AH\n");
    if (*(int *)&ea < 0)
      utempa = (immu32 >> 16);
    else
      utempa = get16(ea);
    putgr16(dr, add16(getgr16(dr), utempa, 0, ea));
    goto fetch;

  case 013:
    TRACE(T_FLOW, " NH\n");
    if (*(int *)&ea < 0)
      putgr16(dr, getgr16(dr) & (immu32 >> 16));
    else
      putgr16(dr, getgr16(dr) & get16(ea));
    goto fetch;

  case 014:
    TRACE(T_FLOW, " LHL2\n");
    if (*(int *)&ea < 0)
      putgr16(dr, (immu32 >> 16) << 2);
    else
      putgr16(dr, get16(ea) << 2);
    goto fetch;

  case 015:
    TRACE(T_FLOW, " SHA\n");
    switch ((ea >> 14) & 3) {
    case 0:
      putgr32(dr, lls(getgr32(dr), ea));
      if ((getcrs16(KEYS) & 0100400) == 0100400)
	mathexception('i', FC_INT_OFLOW, 0);
      break;
    case 1:
      putgr16(dr, als(getgr16(dr), ea));
      if ((getcrs16(KEYS) & 0100400) == 0100400)
	mathexception('i', FC_INT_OFLOW, 0);
      break;
    case 2:
      putgr32(dr, lrs(getgr32(dr), ea));
      break;
    case 3:
      putgr16(dr, ars(getgr16(dr), ea));
      break;
    default:
      fatal("SHA?");
    }
    goto fetch;

  case 016:  /* Special MR FP format */
    /* FST, DFST, FA, DFA */
    switch (dr) {
      int oflow;
    case 0:
    case 2:
      dr &= 2;
      TRACE(T_FLOW, " FST\n");
      CLEARC;
      if (*(int *)&ea >= 0) {
	if (getcrs16(KEYS) & 010)
	  putfr64(dr, frn(getfr64(dr), &oflow));  /* sing prec can't overflow */
	if ((getgr32(FAC0+dr+1) & 0xFF00) == 0)
	  put32((getfr32(dr) & 0xFFFFFF00) | (getgr32(FAC0+dr+1) & 0xFF), ea);
	else
	  mathexception('f', FC_SFP_STORE, ea);
      } else {
	warn("I-mode immediate FST?");
	fault(ILLINSTFAULT, RPL, RP);
      }
      break;

    case 1:
    case 3:
      dr &= 2;
      TRACE(T_FLOW, " DFST\n");
      if (*(int *)&ea >= 0)
	put64(getgr64s(FAC0+dr), ea);
      else {
	warn("I-mode immediate DFST?");
	fault(ILLINSTFAULT, RPL, RP);
      }
      break;

    case 4:
    case 6:
      dr &= 2;
      TRACE(T_FLOW, " FA\n");
      if (*(int *)&ea >= 0) {
	immu64 = get32(ea);
	immu64 = ((immu64 << 32) & 0xffffff0000000000LL) | (immu64 & 0xff);
      }
      if (immu64 & 0xFFFFFFFF00000000LL)
	if (getgr32s(FAC0+dr)) {
	  tempa1 = getgr32(FAC0+dr+1) & 0xffff;
	  tempa2 = immu64 & 0xffff;
	  if (abs(tempa1-tempa2) < 48)
	    if (prieee8(getfr64(dr), &tempd1) 
		&& prieee8(immu64, &tempd2)
		&& ieeepr8(tempd1+tempd2, (long long *)(crsl+FAC0+dr), 0))
	      CLEARC;
	    else
	      mathexception('f', FC_SFP_OFLOW, ea);
	  else if (tempa1 < tempa2)
	    putgr64s(FAC0+dr, immu64);
	} else
	  putgr64s(FAC0+dr, immu64);
      else if (getgr32s(FAC0+dr) == 0)
	putgr64s(FAC0+dr, 0);
      break;

    case 5:
    case 7:
      dr &= 2;
      TRACE(T_FLOW, " DFA\n");
      if (*(int *)&ea >= 0)
	immu64 = get64(ea);
      if (immu64 & 0xFFFFFFFF00000000LL)
	if (getgr32s(FAC0+dr))
	  if (prieee8(getfr64(dr), &tempd1) 
	      && prieee8(immu64, &tempd2)
	      && ieeepr8(tempd1+tempd2, (long long *)(crsl+FAC0+dr), 0))
	    CLEARC;
	  else
	    mathexception('f', FC_DFP_OFLOW, ea);
	else
	  putgr64s(FAC0+dr, immu64);
      else if (getgr32s(FAC0+dr) == 0)
	putgr64s(FAC0+dr, 0);
      break;

    default:
      warn("I-mode 016 switch?");
      fault(ILLINSTFAULT, RPL, RP);
    }
    goto fetch;

  case 017:
    warn("I-mode opcode 017?");
    fault(ILLINSTFAULT, RPL, RP);

  case 020:
    /* this should have been handled already! */
    fatal("I-mode generic class 1?");

  case 021:
    TRACE(T_FLOW, " ST\n");
    if (*(int *)&ea < 0)
      putgr32((inst >> 2) & 7, getgr32(dr));
    else
      put32(getgr32(dr),ea);
    goto fetch;

  case 022:
    TRACE(T_FLOW, " S\n");
    if (*(int *)&ea < 0)
      utempl = immu32;
    else
      utempl = get32(ea);
    putgr32(dr, add32(getgr32(dr), ~utempl, 1, ea));
    goto fetch;

  case 023:
    TRACE(T_FLOW, " O\n");
    if (*(int *)&ea < 0)
      putgr32(dr, getgr32(dr) | immu32);
    else
      putgr32(dr, getgr32(dr) | get32(ea));
    goto fetch;

  case 024:
    TRACE(T_FLOW, " ROT\n");
    switch ((ea >> 14) & 3) {
    case 0:
      putgr32(dr, llr(getgr32(dr), ea));
      break;
    case 1:
      putgr16(dr, alr(getgr16(dr), ea));
      break;
    case 2:
      putgr32(dr, lrr(getgr32(dr), ea));
      break;
    case 3:
      putgr16(dr, arr(getgr16(dr), ea));
      break;
    default:
      warn("I-mode ROT switch?");
      fault(ILLINSTFAULT, RPL, RP);
    }
    goto fetch;

  case 025:
    warn("I-mode opcode 025?");
    fault(ILLINSTFAULT, RPL, RP);

  case 026:  /* Special MR FP format */
    /* FS, DFS, FM, DFM */
    switch (dr) {
    case 0:
    case 2:
      dr &= 2;
      TRACE(T_FLOW, " FS\n");
      if (*(int *)&ea >= 0) {
	immu64 = get32(ea);
	immu64 = ((immu64 << 32) & 0xffffff0000000000LL) | (immu64 & 0xff);
      }
      if (immu64 & 0xFFFFFFFF00000000LL)
	if (getgr32s(FAC0+dr)) {
	  tempa1 = getgr32(FAC0+dr+1) & 0xffff;
	  tempa2 = immu64 & 0xffff;
	  if (abs(tempa1-tempa2) < 48)
	    if (prieee8(getfr64(dr), &tempd1) 
		&& prieee8(immu64, &tempd2)
	        && ieeepr8(tempd1-tempd2, (long long *)(crsl+FAC0+dr), 0))
	      CLEARC;
	    else
	      mathexception('f', FC_SFP_OFLOW, ea);
	  else if (tempa1 < tempa2) {
	    putgr64s(FAC0+dr, immu64);
	    goto dfcmdr;
	  }
	} else {
	  putgr64s(FAC0+dr, immu64);
	  goto dfcmdr;
	}
      else if (getgr32s(FAC0+dr) == 0)
	putgr64s(FAC0+dr, 0);
      break;

    case 1:
    case 3:
      dr &= 2;
      TRACE(T_FLOW, " DFS\n");
      if (*(int *)&ea >= 0)
	immu64 = get64(ea);
      if (immu64 & 0xFFFFFFFF00000000LL)
	if (getgr32s(FAC0+dr))
	  if (prieee8(getfr64(dr), &tempd1) 
	      && prieee8(immu64, &tempd2)
	      && ieeepr8(tempd1-tempd2, (long long *)(crsl+FAC0+dr), 0))
	    CLEARC;
	  else
	    mathexception('f', FC_DFP_OFLOW, ea);
	else {
	  putgr64s(FAC0+dr, immu64);
	  goto dfcmdr;
	}
      else if (getgr32s(FAC0+dr) == 0)
	putgr64s(FAC0+dr, 0);
      break;

    case 4:
    case 6:
      dr &= 2;
      TRACE(T_FLOW, " FM\n");
      if (getgr32s(FAC0+dr)) {
	if (*(int *)&ea >= 0) {
	  immu64 = get32(ea);
	  immu64 = ((immu64 << 32) & 0xffffff0000000000LL) | (immu64 & 0xff);
	}
	if (immu64 & 0xFFFFFFFF00000000LL)
	  if (prieee8(immu64, &tempd2) 
	      && prieee8(getfr64(dr), &tempd1)
	      && ieeepr8(tempd1*tempd2, (long long *)(crsl+FAC0+dr), 0))
	    CLEARC;
	  else
	    mathexception('f', FC_SFP_OFLOW, ea);
	else            /* operand = 0.0: no multiply */
	  putgr64s(FAC0+dr, 0);
      } else            /* clean up (maybe) dirty zero */
	putgr64s(FAC0+dr, 0);
      break;

    case 5:
    case 7:
      dr &= 2;
      TRACE(T_FLOW, " DFM\n");
      if (getgr32s(FAC0+dr)) {
	if (*(int *)&ea >= 0)
	  immu64 = get64(ea);
	if (immu64 & 0xFFFFFFFF00000000LL)
	  if (prieee8(immu64, &tempd2) 
	      && prieee8(getfr64(dr), &tempd1)
	      && ieeepr8(tempd1*tempd2, (long long *)(crsl+FAC0+dr), 0))
	    CLEARC;
	  else
	    mathexception('f', FC_DFP_OFLOW, ea);
	else             /* operand = 0.0: no multiply */
	  putgr64s(FAC0+dr, 0);
      } else
	putgr64s(FAC0+dr, 0);
      break;

    default:
      warn("I-mode 026 switch?");
      fault(ILLINSTFAULT, RPL, RP);
    }
    goto fetch;

  case 027:
    warn("I-mode opcode 027?");
    fault(ILLINSTFAULT, RPL, RP);

  case 030:  /* register generic */
    /* this should have been handled already! */
    fatal("I-mode RGEN?");

  case 031:
    TRACE(T_FLOW, " STH\n");
    if (*(int *)&ea < 0)
      putgr16(((inst >> 2) & 7), getgr16(dr));
    else
      put16(getgr16(dr), ea);
    goto fetch;

  case 032:
    TRACE(T_FLOW, " SH\n");
    if (*(int *)&ea < 0)
      utempa = (immu32 >> 16);
    else
      utempa = get16(ea);
    putgr16(dr, add16(getgr16(dr), ~utempa, 1, ea));
    goto fetch;

  case 033:
    TRACE(T_FLOW, " OH\n");
    if (*(int *)&ea < 0)
      putgr16(dr, getgr16(dr) | (immu32 >> 16));
    else
      putgr16(dr, getgr16(dr) | get16(ea));
    goto fetch;

  case 034:
    TRACE(T_FLOW, " EIO\n");
    CLEAREQ;
    pio(ea & 0xFFFF);
    goto fetch;

  case 035:
    TRACE(T_FLOW, " LHL3\n");
    if (*(int *)&ea < 0)
      putgr16(dr, (immu32 >> 16) << 3);
    else
      putgr16(dr, get16(ea) << 3);
    goto fetch;

  case 036:  /* Special MR FP format */
    /* FD, DFD, QFLD, QFST, QFSB, QFAD */
    switch (dr) {
    case 0:
    case 2:
      dr &= 2;
      TRACE(T_FLOW, " FD\n");
      if (*(int *)&ea >= 0) {
	immu64 = get32(ea);
	immu64 = ((immu64 << 32) & 0xffffff0000000000LL) | (immu64 & 0xff);
      }
      if (immu64 & 0xFFFFFFFF00000000LL)
	if (getgr32s(FAC0+dr))
	  if (prieee8(immu64, &tempd2) 
	      && prieee8(getfr64(dr), &tempd1)
	      && ieeepr8(tempd1/tempd2, (long long *)(crsl+FAC0+dr), 1))
	    CLEARC;
	  else
	    mathexception('f', FC_SFP_OFLOW, ea);
	else            /* operand = 0.0 */
	  putgr64s(FAC0+dr, 0);
      else            /* clean up (maybe) dirty zero */
	mathexception('f', FC_SFP_ZDIV, ea);
      break;

    case 1:
    case 3:
      dr &= 2;
      TRACE(T_FLOW, " DFD\n");
      if (*(int *)&ea >= 0)
	immu64 = get64(ea);
      if (immu64 & 0xFFFFFFFF00000000LL)
	if (getgr32s(FAC0+dr))
	  if (prieee8(immu64, &tempd2) 
	      && prieee8(getfr64(dr), &tempd1)
	      && ieeepr8(tempd1/tempd2, (long long *)(crsl+FAC0+dr), 1))
	    CLEARC;
	  else
	    mathexception('f', FC_DFP_OFLOW, ea);
	else
	  putgr64s(FAC0+dr, 0);
      else
	mathexception('f', FC_DFP_ZDIV, ea);
      break;

    case 4:  /* QFLD */
    case 5:  /* QFST */
    case 6:  /* QFSB */
    case 7:  /* QFAD */
      fault(UIIFAULT, RPL, RP);

    default:
      warn("I-mode 036 switch?");
      fault(ILLINSTFAULT, RPL, RP);
    }
    goto fetch;

  case 037:
    warn("I-mode opcode 037?");
    fault(ILLINSTFAULT, RPL, RP);

  case 040:  /* generic class 2, overlays skip group */
    /* this should have been handled already! */
    fatal("I-mode generic class 2?");

  case 041:
    TRACE(T_FLOW, " I\n");
    utempl = getgr32(dr);
    if (*(int *)&ea < 0) {   /* register-to-register form */
      putgr32(dr, immu32);
      putgr32((inst >> 2) & 7, utempl);
    } else {
      putgr32(dr, get32(ea));
      put32(utempl, ea);
    }
    goto fetch;

  case 042:
    TRACE(T_FLOW, " M\n");
    dr &= 6;                  /* force dr even */
    if (*(int *)&ea < 0)
      templ = immu32;
    else
      templ = get32(ea);
    putgr64s(dr, (long long)templ * (long long)(getgr32s(dr)));
    CLEARC;
    goto fetch;

  case 043:
    TRACE(T_FLOW, " X\n");
    if (*(int *)&ea < 0)
      putgr32(dr, getgr32(dr) ^ immu32);
    else
      putgr32(dr, getgr32(dr) ^ get32(ea));
    goto fetch;

  case 044:
    TRACE(T_FLOW, " LDAR\n");
    putgr32(dr, ldar(ea));
    goto fetch;

  case 045:
    if (*(int *)&ea < 0) {
      TRACE(T_FLOW, " CCP\n");
      utempl1 = EACP(getgr32(dr));
      utempl2 = EACP(immu32);
      if (utempl1 < utempl2)
	putcrs16(KEYS, getcrs16(KEYS) & ~0300 | 0200);
      else if (utempl1 == utempl2)
	putcrs16(KEYS, getcrs16(KEYS) & ~0300 | 0100);
      else
	putcrs16(KEYS, getcrs16(KEYS) & ~0300);
    } else {
      TRACE(T_FLOW, " LCC\n");
      utempa = get16(ea);
      TRACE(T_INST, " before load, keys=%o, ea=%o/%o, [ea]=0x%x, dr=%d, [dr]=0x%x\n", getcrs16(KEYS), ea>>16, ea&0xFFFF, utempa, dr, getgr32(dr));
      if (ea & EXTMASK32)
	utempa &= 0xFF;
      else
	utempa >>= 8;
      putgr16(dr, utempa);
      if (utempa == 0)
	SETEQ;
      else
	CLEAREQ;
      TRACE(T_INST, " after load, keys=%o, ea=%o/%o, utempa=0x%x, dr=%d, [dr]=0x%x\n", getcrs16(KEYS), ea>>16, ea&0xFFFF, utempa, dr, getgr32(dr));
    }
    goto fetch;

  case 046:  /* I special MR, GR format: IM, PCL, EALB, ZM, TM, QFMP, QFDV, QFC */
    switch (dr) {
    case 0:
      TRACE(T_FLOW, " IM\n");
      templ = get32(ea);
      put32(templ+1, ea);
      CLEARCC;
      /* NOTE: test pre-incremented values to get true LT (overflow) */
      if (templ == -1)
	SETEQ;
      else if (templ < 0)
	SETLT;
      break;

    /* NOTE: V-mode PCL may jump here! */

    case 1:
imodepcl:
#if 0
      TRACE(T_FLOW|T_PCL, "#%u %o/%0o: PCL %o/%o %s\n", gv.instcount, RPH, RPL-2, ea>>16, ea&0xFFFF, searchloadmap(ea, 'e'));
#else
      TRACE(T_FLOW|T_PCL, "%o/%0o: PCL %o/%o %s\n", RPH, RPL-2, ea>>16, ea&0xFFFF, searchloadmap(ea, 'e'));
#endif
#ifndef NOTRACE
      if (gv.numtraceprocs > 0 && TRACEUSER)
	for (i=0; i<gv.numtraceprocs; i++)
	  if (traceprocs[i].ecb == (ea & 0xFFFFFFF) && traceprocs[i].sb == -1) {
	    gv.traceflags = ~T_MAP;
	    gv.savetraceflags = gv.traceflags;
	    traceprocs[i].sb = getcrs32s(SB);
	    printf("Enabled trace for %s at sb '%o/%o\n", traceprocs[i].name, getcrs16(SBH), getcrs16(SBL));
	    break;
	  }
#endif
      pcl(ea);
      break;

    case 2:
      TRACE(T_FLOW, " EALB\n");
      putcrs32ea(LB, ea);
      break;

    case 3:
      TRACE(T_FLOW, " ZM\n");
      put32(0, ea);
      break;

    case 4:
      TRACE(T_FLOW, " TM\n");
      utempl = get32(ea);
      SETCC_32(utempl);
      break;

    case 5:
      TRACE(T_FLOW, " QFMP\n");
      fault(UIIFAULT, RPL, RP);
      //warn("IXX QFMP");
      break;

    case 6:
      TRACE(T_FLOW, " QFDV\n");
      fault(UIIFAULT, RPL, RP);
      //warn("IXX QFDV");
      break;

    case 7:
      TRACE(T_FLOW, " QFC\n");
      fault(UIIFAULT, RPL, RP);
      //warn("IXX QFC");
      break;

    default:
      warn("I-mode 006 switch?");
      fault(ILLINSTFAULT, RPL, RP);
    }
    goto fetch;

  case 047:
    warn("I-mode opcode 047?");
    fault(ILLINSTFAULT, RPL, RP);

  case 050:
    warn("I-mode opcode 050?");
    fault(ILLINSTFAULT, RPL, RP);

  case 051:
    TRACE(T_FLOW, " IH\n");
    utempa = getgr16(dr);
    if (*(int *)&ea < 0) {
      putgr16(dr, immu32 >> 16);
      putgr16(((inst >> 2) & 7), utempa);
    } else {
      putgr16(dr, get16(ea));
      put16(utempa, ea);
    }
    goto fetch;

  case 052:
    TRACE(T_FLOW, " MH\n");
    if (*(int *)&ea < 0)
      tempa = (immu32 >> 16);
    else
      tempa = get16(ea);
    putgr32(dr, getgr16s(dr) * tempa);
    CLEARC;
    goto fetch;

  case 053:
    TRACE(T_FLOW, " XH\n");
    if (*(int *)&ea < 0)
      putgr16(dr, getgr16(dr) ^ (immu32 >> 16));
    else
      putgr16(dr, getgr16(dr) ^ get16(ea));
    goto fetch;

  case 054:
    TRACE(T_FLOW, " STAR\n");
    star(getgr32(dr), ea);
    goto fetch;

  case 055:
    if (*(int *)&ea < 0) {
      TRACE(T_FLOW, " ACP\n");
      TRACE(T_INST, " before acp, gr%d=%o/%o, immu32=%u, CP(dr)=%d\n", dr, getgr32(dr)>>16, getgr32(dr)&0xFFFF, immu32, EACP(getgr32(dr)));
      utempl = EACP(getgr32(dr));
      utempl += *(int *)&immu32;
      putgr32(dr, CPEA(getgr32(dr), utempl));
      TRACE(T_INST, " after acp, utempl=%d, gr[dr]=%o/%o\n", utempl, getgr32(dr)>>16, getgr32(dr)&0xFFFF);
    } else {
      TRACE(T_FLOW, " SCC\n");
      utempa = get16(ea);
      if (ea & EXTMASK32)
	utempa = (utempa & 0xFF00) | (getgr16(dr) & 0xFF);
      else
	utempa = (getgr16(dr) << 8) | (utempa & 0xFF);
      put16(utempa, ea);
    }
    goto fetch;

  case 056:  /* I special MR, GR format: IMH, JMP, EAXB, ZMH, TMH */
    switch (dr) {
    case 0:
      TRACE(T_FLOW, " IMH\n");
      tempa = get16(ea);
      put16(tempa+1, ea);
      CLEARCC;
      if (tempa == -1)
	SETEQ;
      else if (tempa < 0)
	SETLT;
      break;

    case 1:
      TRACE(T_FLOW, " JMP\n");
      RP = ea;
      break;

    case 2:
      TRACE(T_FLOW, " EAXB\n");
      putcrs32ea(XB, ea);
      break;

    case 3:
      TRACE(T_FLOW, " ZMH\n");
      put16(0, ea);
      break;

    case 4:
      TRACE(T_FLOW, " TMH\n");
      utempa = get16(ea);
      SETCC_16(utempa);
      break;

    default:
      fault(ILLINSTFAULT, RPL, RP);
    }
    goto fetch;

  case 057:
    warn("I-mode opcode 057?");
    fault(ILLINSTFAULT, RPL, RP);

  case 060:
    /* this should have been handled already! */
    fatal("I-mode generic class 3?");

  case 061:
    TRACE(T_FLOW, " C\n");
    if (*(int *)&ea < 0)
      utempl = immu32;
    else
      utempl = get32(ea);
    putcrs16(KEYS, getcrs16(KEYS) & ~020300);     /* clear L, EQ LT */
    utempll = getgr32(dr);
    if ((utempll + (~utempl & 0xFFFFFFFF) + 1) & 0x100000000LL)
      putcrs16(KEYS, getcrs16(KEYS) | 020000);
    if (getgr32(dr) == utempl)
      SETEQ;
    else if (getgr32s(dr) < *(int *)&utempl)
      SETLT;
    goto fetch;

  case 062:
    TRACE(T_FLOW, " D\n");
    if (*(int *)&ea < 0)
      templ = immu32;
    else
      templ = get32(ea);
    dr &= 6;                  /* force dr even */
    if (templ != 0) {
      templl1 = getgr64s(dr);
      templl2 = templl1 / templ;
      putgr32(dr, templl2);
      putgr32(dr+1, templl1 % templ);
      if (-2147483648LL <= templl2 && templl2 <= 2147483647LL)
	CLEARC;
      else
	mathexception('i', FC_INT_OFLOW, 0);
    } else
      mathexception('i', FC_INT_ZDIV, 0);
    goto fetch;

  case 063:
    TRACE(T_FLOW, " EAR\n");
    if (*(int *)&ea >= 0)
      putgr32(dr, ea);
    else {
      warn("Immediate mode EAR?");
      fault(ILLINSTFAULT, RPL, RP);
    }
    goto fetch;

  case 064:
    TRACE(T_FLOW, " MIA\n");
    fault(UIIFAULT, RPL, RP);
    goto fetch;

  case 065:
    TRACE(T_FLOW, " LIP\n");
    utempl = get32(ea);
    if (utempl & 0x80000000)
      fault(POINTERFAULT, utempl>>16, ea);
    putgr32(dr, utempl | (RP & RINGMASK32));  /* CPU.AMGRR, cpuid=26+ */
    goto fetch;

  case 066:  /* I-mode special MR: DM, JSXB */
    switch (dr) {
    case 0:
      TRACE(T_FLOW, " DM\n");
      templ = get32(ea);
      put32(templ-1, ea);
      /* NOTE: test pre-decremented values to get true LT (overflow) */
      CLEARCC;
      if (templ == 1)
	SETEQ;
      else if (templ <= 0)
	SETLT;
      break;

    case 1:
      TRACE(T_FLOW, " JSXB\n");
      putcrs32(XB, RP);
      RP = ea;
      break;

    default:
      fault(ILLINSTFAULT, RPL, RP);
    }
    goto fetch;

  case 067:
    warn("I-mode opcode 067?");
    fault(ILLINSTFAULT, RPL, RP);

  case 070:
    warn("I-mode opcode 070?");
    fault(ILLINSTFAULT, RPL, RP);

  case 071:
    TRACE(T_FLOW, " CH\n");
    if (*(int *)&ea < 0)
      utempa = (immu32 >> 16);
    else
      utempa = get16(ea);
    putcrs16(KEYS, getcrs16(KEYS) & ~020300);
    utempl = getgr16(dr);
    if ((utempl + (~utempa & 0xFFFF) + 1) & 0x10000)
      putcrs16(KEYS, getcrs16(KEYS) | 020000);
    if (getgr16(dr) == utempa)
      SETEQ;
    else if (getgr16s(dr) < *(short *)&utempa)
      SETLT;
    goto fetch;

  case 072:
    TRACE(T_FLOW, " DH\n");
    if (*(int *)&ea < 0)
      tempa = (immu32 >> 16);
    else
      tempa = get16(ea);
    if (tempa != 0) {
      templ1 = getgr32s(dr);
      templ2 = templ1 / tempa;
      putgr32(dr, (templ2 << 16) | (templ1 % tempa));
      if (-32768 <= templ2 && templ2 <= 32767)
	CLEARC;
      else
	mathexception('i', FC_INT_OFLOW, 0);
    } else
      mathexception('i', FC_INT_ZDIV, 0);
    goto fetch;

  case 073:
    TRACE(T_FLOW, " JSR\n");
    putgr16(dr, RPL);
    RP = ea;
    goto fetch;

  case 074:
    TRACE(T_FLOW, " MIB\n");
    fault(UIIFAULT, RPL, RP);
    goto fetch;

  case 075:
    TRACE(T_FLOW, " AIP\n");
    if (*(int *)&ea < 0)
      utempl = immu32;
    else
      utempl = get32(ea);
    utempl += getgr32(dr);
    if (utempl & 0x80000000)
      fault(POINTERFAULT, utempl>>16, ea);
    /* IXX: ISG says C & L are set, ring needs to be weakened */
    putgr32(dr, utempl);
    goto fetch;

  case 076:  /* I-mode special MR: DMH, TCNP */
    switch (dr) {
    case 0:
      TRACE(T_FLOW, " DMH\n");
      tempa = get16(ea);
      put16(tempa-1, ea);
      CLEARCC;
      /* NOTE: test pre-decremented values to get true LT (overflow) */
      if (tempa == 1)
	SETEQ;
      else if (tempa <= 0)
	SETLT;
      break;

    case 6:
      TRACE(T_FLOW, " TCNP\n");
      if (*(int *)&ea > 0)
	if ((get32(ea) & 0x1FFFFFFF) == 0)
	  SETEQ;
	else
	  CLEAREQ;
      else
	fault(UIIFAULT, RPL, RP);
      break;

    default:
      fault(ILLINSTFAULT, RPL, RP);
    }
    goto fetch;

  case 077:
    warn("I-mode opcode 077");
    fault(ILLINSTFAULT, RPL, RP);
  }
  fatal("I-mode fall-through?");


d_lda:  /* 00200 (V-mode) */
  putcrs16(A, get16t(ea));
  TRACE(T_FLOW, " LDA ='%o/%d %03o %03o\n", getcrs16(A), getcrs16s(A), getcrs16(A)>>8, getcrs16(A) & 0xFF);
  goto fetch;

  /* NOTE: don't use get32 for DLD/DST, because it doesn't handle register
     address traps */

d_ldadld:  /* 00200 (R-mode) */
  putcrs16(A, get16t(ea));
  if (!(getcrs16(KEYS) & 040000)) {  /* not DP */
    TRACE(T_FLOW, " LDA ='%o/%d %03o %03o\n", getcrs16(A), getcrs16s(A), getcrs16(A)>>8, getcrs16(A) & 0xFF);
  } else {
    TRACE(T_FLOW, " DLD\n");
    putcrs16(B, get16t(INCVA(ea,1)));
  }
  goto fetch;

d_sta:  /* 00400 (V-mode) */
  TRACE(T_FLOW, " STA\n");
  put16t(getcrs16(A),ea);
  goto fetch;

d_stadst:  /* 00400 (R-mode) */
  put16t(getcrs16(A),ea);
  if ((getcrs16(KEYS) & 050000) != 040000) {
    TRACE(T_FLOW, " STA\n");
  } else {
    TRACE(T_FLOW, " DST\n");
    put16t(getcrs16(B),INCVA(ea,1));
  }
  goto fetch;

d_jmp:  /* 00100 */
  TRACE(T_FLOW, " JMP\n");
  RP = ea;
  goto fetch;

d_ana:  /* 00300 */
  m = get16t(ea);
  TRACE(T_FLOW, " ANA ='%o\n",m);
  putcrs16(A, getcrs16(A) & m);
  goto fetch;

d_era:  /* 00500 */
  m = get16t(ea);
  TRACE(T_FLOW, " ERA ='%o\n", m);
  putcrs16(A, getcrs16(A) ^ m);
  goto fetch;

d_add:  /* 00600 (V-mode) */
  m = get16t(ea);
  TRACE(T_FLOW, " ADD ='%o/%d\n", m, *(short *)&m);
  putcrs16(A, add16(getcrs16(A), m, 0, ea));
  goto fetch;

d_adddad:  /* 00600 (R-mode) */
  if (!(getcrs16(KEYS) & 040000))      /* dbl mode? */
    goto d_add;
  TRACE(T_FLOW, " DAD\n");
  putcrs16(KEYS, getcrs16(KEYS) & ~0120300);   /* clear C, L, LT, EQ */
  utempa = getcrs16(A);
  m = get16t(ea);
  putcrs16(B, getcrs16(B) + get16t(INCVA(ea,1)));
  utempl = getcrs16(A);
  if (getcrs16(B) & 0x8000) {
    utempl++;
    putcrs16(B, getcrs16(B) & 0x7fff);
  }
  utempl += m;
  putcrs16(A, utempl);
  if (utempl & 0x10000)                  /* set L-bit if carry */
    putcrs16(KEYS, getcrs16(KEYS) | 020000);
  /* NOTE: this EQ test prevents reusing the ADD code :( */
  if (getcrs32s(L) == 0)              /* set EQ? */
    SETEQ; 
  if (((~utempa ^ m) & (utempa ^ getcrs16(A))) & 0x8000) {
    if (getcrs32s(L) >= 0)
      SETLT;
    mathexception('i', FC_INT_OFLOW, 0);
  } else if (getcrs32s(L) < 0)
    SETLT;
  goto fetch;

d_sub:  /* 00700 (V-mode) */
  m = get16t(ea);
  utempa = getcrs16(A);
  TRACE(T_FLOW, " SUB ='%o/%d\n", m, *(short *)&m);
  putcrs16(A, add16(getcrs16(A), ~m, 1, ea));
  goto fetch;

d_subdsb:  /* 00700 */
  if (!(getcrs16(KEYS) & 040000))
    goto d_sub;
  TRACE(T_FLOW, " DSB\n");
  putcrs16(KEYS, getcrs16(KEYS) & ~0120300);   /* clear C, L, and CC */
  utempa = getcrs16(A);
  m = get16t(ea);
  putcrs16(B, getcrs16(B) - get16t(INCVA(ea,1)));
  utempl = getcrs16(A);
  if (getcrs16(B) & 0x8000) {
    utempl += 0xFFFF;
    putcrs16(B, getcrs16(B) & 0x7fff);
  }
  utempl += (unsigned short) ~m;
  utempl += 1;
  putcrs16(A, utempl);                   /* truncate results */
  if (utempl & 0x10000)                  /* set L-bit if carry */
    putcrs16(KEYS, getcrs16(KEYS) | 020000);
  if (getcrs32s(L) == 0)              /* set EQ? */
    SETEQ; 
  if (((utempa ^ m) & (utempa ^ getcrs16(A))) & 0x8000) {
    if (getcrs32s(L) >= 0)
      SETLT;
    mathexception('i', FC_INT_OFLOW, 0);
  } else if (getcrs32s(L) < 0)
    SETLT;
  goto fetch;

d_ora:  /* 00302 */
  m = get16t(ea);
  TRACE(T_FLOW, " ORA ='%o\n", m);
  putcrs16(A, getcrs16(A) | m);
  goto fetch;

d_jst:  /* 01000 */
  TRACE(T_FLOW, " JST\n");

  /* NOTE: amask should be recomputed here if in R/S mode, so it
     can be removed as a global variable.  Flaky errors occur if
     keys are changed w/o calling newkeys(), because amask would
     be wrong (see dispatcher comment) */

  if (gv.amask == 0177777)
    m = RPL;
  else
    m = (get16t(ea) & ~gv.amask) | RPL;
  put16t(m, ea);
  RP = ea;
  INCRP;
  gv.brp[RPBR] = *eap;
  if ((RP & RINGMASK32) == 0)
    gv.inhcount = 1;
  goto fetch;

d_cas:  /* 01100 */
  m = get16t(ea);
  TRACE(T_FLOW, " CAS ='%o/%d\n", m, *(short *)&m);

#if 1

  /* this crap is to set L & CC like subtract would, even though
     most programs never tested these after CAS.  add16 can't be
     used because docs say CAS leaves C-bit unchanged... :( */

  putcrs16(KEYS, getcrs16(KEYS) & ~020300);   /* clear L, and CC */
  utempa = getcrs16(A);
  utempl = getcrs16(A);
  utempl += (unsigned short) ~m;
  utempl += 1;
  putcrs16(A, utempl);                   /* truncate results */
  if (utempl & 0x10000)                  /* set L-bit if carry */
    putcrs16(KEYS, getcrs16(KEYS) | 020000);  
  if (getcrs16(A) == 0)                  /* set EQ? */
    SETEQ; 
  if (((utempa ^ m) & (utempa ^ getcrs16(A))) & 0x8000) {
    if (getcrs16s(A) >= 0)
      SETLT;
  } else if (getcrs16s(A) < 0)
    SETLT;
  putcrs16(A, utempa);                   /* restore A reg */
#endif

  if (getcrs16(A) == m) {
    INCRP;
  } else if (getcrs16s(A) < *(short *)&m) {
    RPL += 2;
  }
  goto fetch;

d_irs:  /* 01200 */
  TRACE(T_FLOW, " IRS\n");
  utempa = get16t(ea) + 1;
  put16t(utempa, ea);
  TRACE(T_FLOW, " New: '%o/%d\n", utempa, *(short *)&utempa);
  if (utempa == 0)
    INCRP;
  goto fetch;

d_ima:  /* 01300 */
  TRACE(T_FLOW, " IMA\n");
  utempa = get16t(ea);
  put16t(getcrs16(A),ea);
  putcrs16(A, utempa);
  goto fetch;

d_jsy:  /* 01400 */
  TRACE(T_FLOW, " JSY\n");
  putcrs16(Y, RPL);
  RP = ea;
  goto fetch;

d_jsxb:  /* 01402 */
  TRACE(T_FLOW, " JSXB\n");
  putcrs32(XB, RP);
  RP = ea;
  goto fetch;

d_stx:  /* 01500 */
  TRACE(T_FLOW, " STX\n");
  put16t(getcrs16(X),ea);
  goto fetch;

  /* MPY can't overflow in V-mode */

d_mpy:  /* 01600 (V-mode) */
  m = get16t(ea);
  TRACE(T_FLOW, " MPY ='%o/%d\n", m, *(short *)&m);
  putcrs32s(L, getcrs16s(A) * *(short *)&m);
  CLEARC;
  goto fetch;

  /* in R-mode (31 bits), -32768*-32768 can overflow and yields
   0x8000/0x0000 */

d_mpy_r:  /* 01600 (R-mode) */
  m = get16t(ea);
  TRACE(T_FLOW, " MPY ='%o/%d\n", m, *(short *)&m);
  templ = getcrs16s(A) * *(short *)&m;
  CLEARC;
  utempa = getcrs16(A);
  putcrs16(A, (templ >> 15));
  putcrs16(B, templ & 077777);
  if (utempa == 0x8000 && m == 0x8000)
    mathexception('i', FC_INT_OFLOW, 0);
  goto fetch;

d_mpl:  /* 01603 (V-mode) */
  templ = get32(ea);
  TRACE(T_FLOW, " MPL ='%o/%d\n", templ, *(int *)&templ);
  putcrs64s(L, (long long)(getcrs32s(L)) * (long long)templ);
  CLEARC;
  goto fetch;

d_uii:
  fault(UIIFAULT, RPL, ea);
  goto fetch;

d_div:  /* 01700 */
  tempa = get16t(ea);
  TRACE(T_FLOW, " DIV ='%o/%d\n", *(unsigned short *)&tempa, tempa);
  if (getcrs16(KEYS) & 010000) {          /* V/I mode */
    templ = getcrs32s(A);
  } else {                           /* R/S mode */
    templ = getcrs16s(A);       /* convert to 32-bit signed */
    templ = (templ<<15) | (getcrs16(B) & 0x7FFF);
  }
  if (tempa != 0) {
    templ2 = templ / tempa;
    putcrs16(A, templ2);
    putcrs16(B, templ % tempa);
    if (-32768 <= templ2 && templ2 <= 32767)
      CLEARC;
    else
      mathexception('i', FC_INT_OFLOW, 0);
  } else
    mathexception('i', FC_INT_ZDIV, 0);
  goto fetch;

/* NOTE:  RESET QVFY, DVL runs okay with cpuid=5 (P750), but
   fails with default cpuid (P4450) */

d_dvl:  /* 01703 */
  templ = get32(ea);
  TRACE(T_FLOW, " DVL ='%o/%d\n", templ, templ);
  if (templ != 0) {
    templl1 = getcrs64s(L);
    templl2 = templl1 / templ;
    putcrs32s(L, templl2);
    putcrs32s(E, templl1 % templ);
    if (-2147483648LL <= templl2 && templl2 <= 2147483647LL)
      CLEARC;
    else
      mathexception('i', FC_INT_OFLOW, 0);
  } else
    mathexception('i', FC_INT_ZDIV, 0);
  goto fetch;

d_ldx:  /* 03500 */
  TRACE(T_FLOW, " LDX\n");
  putcrs16(X, get16t(ea));
  goto fetch;

d_eal:  /* 00101 (V-mode) */
  TRACE(T_FLOW, " EAL\n");
  putcrs32ea(L, ea);
  goto fetch;

d_eaa:  /* 00101 (R-mode) */
  TRACE(T_FLOW, " EAA\n");
  putcrs16(A, ea);
  goto fetch;

d_ldl:  /* 00203 (V-mode) */
  putcrs32(L, get32(ea));
  TRACE(T_FLOW, " LDL ='%o/%d\n", getcrs32(A), getcrs32s(A));
  goto fetch;

d_jeq:  /* 00203 (R-mode) */
  TRACE(T_FLOW, " JEQ\n");
  if (getcrs16s(A) == 0)
    RPL = ea;
  goto fetch;

d_sbl:  /* 00703 (V-mode) */
  utempl = get32(ea);
  TRACE(T_FLOW, " SBL ='%o/%d\n", utempl, *(int *)&utempl);
  putgr32(GR2, add32(getgr32(GR2), ~utempl, 1, ea));
  goto fetch;

d_jge:  /* 00703 (R-mode) */
  TRACE(T_FLOW, " JGE\n");
  if (getcrs16s(A) >= 0)
    RPL = ea;
  goto fetch;

d_pcl:  /* 01002 (V-mode) */

  /* NOTE: real PCL code is in I-mode section! */

  goto imodepcl;

d_crep:  /* 01002 (R-mode) */
  TRACE(T_FLOW, " CREP\n");
  put16t(RPL,getcrs16(S)+1);
  RPL = ea;
  goto fetch;

d_erl:  /* 00503 (V-mode) */
  utempl = get32(ea);
  TRACE(T_FLOW, " ERL ='%o/%d  '%o/'%o  %d/%d\n", utempl, *(int *)&utempl, utempl>>16, utempl&0xFFFF, utempl>>16, utempl&0xFFFF);
  putcrs32(L, getcrs32(L) ^ utempl);
  goto fetch;

d_jgt:  /* 00503 (R-mode) */
  TRACE(T_FLOW, " JGT\n");
  if (getcrs16s(A) > 0)
    RPL = ea;
  goto fetch;

d_stl:  /* 00403 (V-mode) */
  TRACE(T_FLOW, " STL\n");
  put32(getcrs32(L),ea);
  goto fetch;

d_jle:  /* 00403 (R-mode) */
  TRACE(T_FLOW, " JLE\n");
  if (getcrs16s(A) <= 0)
    RPL = ea;
  goto fetch;

d_adl:  /* 00603 (V-mode) */
  utempl = get32(ea);
  TRACE(T_FLOW, " ADL ='%o/%d\n", utempl, *(int *)&utempl);
  putgr32(GR2, add32(getgr32(GR2), utempl, 0, ea));
  goto fetch;

d_jlt:  /* 00603 (R-mode) */
  TRACE(T_FLOW, " JLT\n");
  if (getcrs16s(A) < 0)
    RPL = ea;
  goto fetch;

d_anl:  /* 00303 (V-mode) */
  utempl = get32(ea);
  TRACE(T_FLOW, " ANL ='%o\n", utempl);
  putcrs32(L, getcrs32(L) & utempl);
  goto fetch;

d_jne:  /* 00303 (R-mode) */
  TRACE(T_FLOW, " JNE\n");
  if (getcrs16s(A) != 0)
    RPL = ea;
  goto fetch;

d_eaxb:  /* 01202 */
  TRACE(T_FLOW, " EAXB\n");
  putcrs32ea(XB, ea);
  goto fetch;

d_dflx:  /* 01502 (V-mode) */
  TRACE(T_FLOW, " DFLX\n");
  putcrs16(X, get16(ea) * 4);
  goto fetch;

d_jdx:  /* 01502 (R-mode) */
  TRACE(T_FLOW, " JDX\n");
  putcrs16(X, getcrs16(X) - 1);
  if (getcrs16(X) != 0)
    RPL = ea;
  goto fetch;

d_sty:  /* 03502 */
  TRACE(T_FLOW, " STY\n");
  put16(getcrs16(Y),ea);
  goto fetch;

d_qflx:  /* 01503 (V-mode) */
  TRACE(T_FLOW, " QFLX\n");
  putcrs16(X, get16(ea) * 8);
  goto fetch;

d_jix:  /* 01503 (R-mode) */
  TRACE(T_FLOW, " JIX\n");
  putcrs16(X, getcrs16(X) + 1);
  if (getcrs16(X) != 0)
    RPL = ea;
  goto fetch;

d_flx:  /* 01501 */
  TRACE(T_FLOW, " FLX\n");
  putcrs16(X, get16(ea) * 2);
  goto fetch;

d_ldy:  /* 03501 */
  TRACE(T_FLOW, " LDY\n");
  putcrs16(Y, get16(ea));
  goto fetch;

d_jsx:  /* 03503 */
  TRACE(T_FLOW, " JSX\n");
  putcrs16(X, RPL);
  RP = ea;
  goto fetch;

/* XXX: this should set the L bit like subtract */

d_cls:  /* 01103 */
  TRACE(T_FLOW, " CLS\n");
  templ = get32(ea);
  TRACE(T_INST, " [ea]='%o/%+d/%u '%o/'%o\n", templ, templ, templ, *(unsigned int *)&templ >> 16, templ & 0xFFFF);
  CLEARCC;
  if (getcrs32s(L) == templ) {
    INCRP;
    SETEQ;
  } else if (getcrs32s(L) < templ) {
    RPL += 2;
    SETLT;
  }
  XSETL(0);
  goto fetch;

d_fad:  /* 00601 */
  TRACE(T_FLOW, " FAD\n");
  immu64 = get32(ea);
  immu64 = ((immu64 << 32) & 0xffffff0000000000LL) | (immu64 & 0xff);
  if (immu64 & 0xFFFFFFFF00000000LL)
    if (getgr32s(FAC1)) {
      tempa1 = getcrs16(FEXP);
      tempa2 = immu64 & 0xffff;
      if (abs(tempa1-tempa2) < 48)
	if (prieee8(getfr64(2), &tempd1) 
	    && prieee8(immu64, &tempd2)
	    && ieeepr8(tempd1+tempd2, (long long *)(crsl+FAC1), 0))
	  CLEARC;
	else
	  mathexception('f', FC_SFP_OFLOW, ea);
      else if (tempa1 < tempa2)
	putgr64s(FAC1, immu64);
    } else
      putgr64s(FAC1, immu64);
  else if (getgr32s(FAC1) == 0)
    putgr64s(FAC1, 0);
  goto fetch;

/* this is implemented as a subtract on some models */

d_fcs:  /* 01101 */
  TRACE(T_FLOW, " FCS\n");
  templ = get32(ea);
  RPL += fcs(getfr64(2), templ);
  goto fetch;

d_fdv:  /* 01701 */
  TRACE(T_FLOW, " FDV\n");
  immu64 = get32(ea);
  immu64 = ((immu64 << 32) & 0xffffff0000000000LL) | (immu64 & 0xff);
  if (immu64 & 0xFFFFFFFF00000000LL)
    if (getgr32s(FAC1))
      if (prieee8(immu64, &tempd2) 
	  && prieee8(getfr64(2), &tempd1)
	  && ieeepr8(tempd1/tempd2, (long long *)(crsl+FAC1), 1))
	CLEARC;
      else
	mathexception('f', FC_SFP_OFLOW, ea);
    else            /* operand = 0.0 */
      putgr64s(FAC1, 0);
  else            /* clean up (maybe) dirty zero */
    mathexception('f', FC_SFP_ZDIV, ea);
  goto fetch;

d_fld:  /* 0201 */
  TRACE(T_FLOW, " FLD\n");
  utempl = get32(ea);
  putgr32(FAC1, utempl & 0xFFFFFF00);
  putgr32(FAC1+1, utempl & 0x00FF);
  goto fetch;

d_fmp:  /* 01601 */
  TRACE(T_FLOW, " FMP\n");
  if (getgr32s(FAC1)) {
    immu64 = get32(ea);
    immu64 = ((immu64 << 32) & 0xffffff0000000000LL) | (immu64 & 0xff);
    if (immu64 & 0xFFFFFFFF00000000LL)
      if (prieee8(immu64, &tempd2) 
	  && prieee8(getfr64(2), &tempd1)
	  && ieeepr8(tempd1*tempd2, (long long *)(crsl+FAC1), 0))
	CLEARC;
      else
	mathexception('f', FC_SFP_OFLOW, ea);
    else            /* operand = 0.0: no multiply */
      putgr64s(FAC1, 0);
  } else            /* clean up (maybe) dirty zero */
    putgr64s(FAC1, 0);
  goto fetch;

d_fsb:  /* 00701 */
  TRACE(T_FLOW, " FSB\n");
  immu64 = get32(ea);
  immu64 = ((immu64 << 32) & 0xffffff0000000000LL) | (immu64 & 0xff);
  if (immu64 & 0xFFFFFFFF00000000LL)
    if (getgr32s(FAC1)) {
      tempa1 = getcrs16(FEXP);
      tempa2 = immu64 & 0xffff;
      if (abs(tempa1-tempa2) < 48)
	if (prieee8(getfr64(2), &tempd1) 
	    && prieee8(immu64, &tempd2)
	    && ieeepr8(tempd1-tempd2, (long long *)(crsl+FAC1), 0))
	  CLEARC;
	else
	  mathexception('f', FC_SFP_OFLOW, ea);
      else if (tempa1 < tempa2) {
	putgr64s(FAC1, immu64);
	goto dfcmfac1;
      }
    } else {
      putgr64s(FAC1, immu64);
      goto dfcmfac1;
    }
  else if (getgr32s(FAC1) == 0)
    putgr64s(FAC1, 0);
  goto fetch;

d_fst:  /* 0401 */
  {
    int oflow;
    unsigned long long dfp;
    TRACE(T_FLOW, " FST\n");
    CLEARC;
    if (getcrs16(KEYS) & 010)
      putfr64(2, frn(getfr64(2), &oflow));  /* sing prec can't overflow */
    dfp = getfr64(2);
    if ((dfp & 0xFF00) == 0)
      put32(((dfp & 0xFFFFFF0000000000LL) >> 32) | (dfp & 0xFF), ea);
    else
      mathexception('f', FC_SFP_STORE, ea);
  }
  goto fetch;

d_dfad:  /* 0602 */
  TRACE(T_FLOW, " DFAD\n");
  immu64 = get64(ea);
  if (immu64 & 0xFFFFFFFF00000000LL)
    if (getgr32s(FAC1))
      if (prieee8(getfr64(2), &tempd1) 
	  && prieee8(immu64, &tempd2)
	  && ieeepr8(tempd1+tempd2, (long long *)(crsl+FAC1), 0)) {
	CLEARC;
	TRACE(T_FLOW, " %f ('%o %o %o %o) + %f (%o %o %o %o)\n", tempd1, getcrs16(FLTH), getcrs16(FLTL), getcrs16(FLTD), getcrs16(FEXP), tempd2, (unsigned short)(immu64>>48), (unsigned short)((immu64>>32)&0xffff), (unsigned short)((immu64>>16)&0xffff), (unsigned short)(immu64&0xffff));
	TRACE(T_FLOW, " = %f ('%o %o %o %o)\n", tempd1+tempd2, getcrs16(FLTH), getcrs16(FLTL), getcrs16(FLTD), getcrs16(FEXP));
      } else
	mathexception('f', FC_DFP_OFLOW, ea);
    else
      putgr64s(FAC1, immu64);
  else if (getgr32s(FAC1) == 0)
    putgr64s(FAC1, 0);
  goto fetch;

d_dfcs:  /* 01102 */
  TRACE(T_FLOW,  " DFCS\n");
  templl = get64(ea);
  RPL += dfcs(getfr64(2), templl);
  goto fetch;

d_dfdv:  /* 01702 */
  TRACE(T_FLOW, " DFDV\n");
  if (*(int *)&ea >= 0)
    immu64 = get64(ea);
  if (immu64 & 0xFFFFFFFF00000000LL)
    if (getgr32s(FAC1))
      if (prieee8(immu64, &tempd2) 
	  && prieee8(getfr64(2), &tempd1)
	  && ieeepr8(tempd1/tempd2, (long long *)(crsl+FAC1), 1)) {
	CLEARC;
	TRACE(T_FLOW, " %f ('%o %o %o %o) / %f (%o %o %o %o)\n", tempd1, getcrs16(FLTH), getcrs16(FLTL), getcrs16(FLTD), getcrs16(FEXP), tempd2, (unsigned short)(immu64>>48), (unsigned short)((immu64>>32)&0xffff), (unsigned short)((immu64>>16)&0xffff), (unsigned short)(immu64&0xffff));
	TRACE(T_FLOW, " = %f ('%o %o %o %o)\n", tempd1/tempd2, getcrs16(FLTH), getcrs16(FLTL), getcrs16(FLTD), getcrs16(FEXP));
      } else
	mathexception('f', FC_DFP_OFLOW, ea);
    else
      putgr64s(FAC1, 0);
  else
    mathexception('f', FC_DFP_ZDIV, ea);
  goto fetch;

d_dfld:  /* 0202 */
  TRACE(T_FLOW, " DFLD\n");
  putcrs64s(FLTH, get64(ea));
#ifndef NOTRACE
  if (!prieee8(getfr64(2), &tempd1))
    tempd1 = -0.0;
  TRACE(T_FLOW, " Loaded %f  '%o %o %o %o\n", tempd1, getcrs16(FLTH), getcrs16(FLTL), getcrs16(FLTD), getcrs16(FEXP));
#endif
  goto fetch;

d_dfmp:  /* 01602 */
  TRACE(T_FLOW, " DFMP\n");
  if (getgr32s(FAC1)) {
    immu64 = get64(ea);
    if (immu64 & 0xFFFFFFFF00000000LL)
      if (prieee8(immu64, &tempd2) 
	  && prieee8(getfr64(2), &tempd1)
	  && ieeepr8(tempd1*tempd2, (long long *)(crsl+FAC1), 0)) {
	CLEARC;
	TRACE(T_FLOW, " %f ('%o %o %o %o) * %f (%o %o %o %o)\n", tempd1, getcrs16(FLTH), getcrs16(FLTL), getcrs16(FLTD), getcrs16(FEXP), tempd2, (unsigned short)(immu64>>48), (unsigned short)((immu64>>32)&0xffff), (unsigned short)((immu64>>16)&0xffff), (unsigned short)(immu64&0xffff));
	TRACE(T_FLOW, " = %f ('%o %o %o %o)\n", tempd1*tempd2, getcrs16(FLTH), getcrs16(FLTL), getcrs16(FLTD), getcrs16(FEXP));
      } else
	mathexception('f', FC_DFP_OFLOW, ea);
    else             /* operand = 0.0: no multiply */
      putgr64s(FAC1, 0);
  } else
    putgr64s(FAC1, 0);
  goto fetch;

d_dfsb:  /* 0702 */
  TRACE(T_FLOW, " DFSB\n");
  immu64 = get64(ea);
  if (immu64 & 0xFFFFFFFF00000000LL)
    if (getgr32s(FAC1))
      if (prieee8(getfr64(2), &tempd1) 
	  && prieee8(immu64, &tempd2)
	  && ieeepr8(tempd1-tempd2, (long long *)(crsl+FAC1), 0)) {
	CLEARC;
	TRACE(T_FLOW, " %f ('%o %o %o %o) - %f (%o %o %o %o)\n", tempd1, getcrs16(FLTH), getcrs16(FLTL), getcrs16(FLTD), getcrs16(FEXP), tempd2, (unsigned short)(immu64>>48), (unsigned short)((immu64>>32)&0xffff), (unsigned short)((immu64>>16)&0xffff), (unsigned short)(immu64&0xffff));
	TRACE(T_FLOW, " = %f ('%o %o %o %o)\n", tempd1-tempd2, getcrs16(FLTH), getcrs16(FLTL), getcrs16(FLTD), getcrs16(FEXP));
      } else
	mathexception('f', FC_DFP_OFLOW, ea);
    else {
      putgr64s(FAC1, immu64);
      goto dfcmfac1;
    }
  else if (getgr32s(FAC1) == 0)
    putgr64s(FAC1, 0);
  goto fetch;

d_dfst:  /* 0402 */
  TRACE(T_FLOW, " DFST\n");
  put64(getcrs64s(FLTH), ea);
#ifndef NOTRACE
  if (!prieee8(getfr64(2), &tempd1))
    tempd1 = -0.0;
  TRACE(T_FLOW, " Stored %f  '%o %o %o %o\n", tempd1, getcrs16(FLTH), getcrs16(FLTL), getcrs16(FLTD), getcrs16(FEXP));
#endif
  goto fetch;

d_ealb:  /* 01302 */
  TRACE(T_FLOW, " EALB\n");
  putcrs32ea(LB, ea);
  goto fetch;

d_stlr:  /* 0301 */
  TRACE(T_FLOW, " STLR '%06o\n", ea & 0xFFFF);
  star(getcrs32(L), ea);
  goto fetch;

d_ldlr:  /* 0501 */
  TRACE(T_FLOW, " LDLR '%06o\n", ea & 0xFFFF);
  putcrs32(L, ldar(ea));
  goto fetch;

d_qfxxuii:  /* 0502 */
  TRACE(T_FLOW, " QFxx '%06o\n", ea & 0xFFFF);
  fault(UIIFAULT, RPL, RP);

d_eio:  /* 01401 */
  TRACE(T_FLOW, " EIO\n");
  CLEAREQ;
  pio(ea & 0xFFFF);
  goto fetch;

d_xec:  /* 00102 */
  TRACE(T_FLOW, " XEC\n");
  utempa = get16t(ea);
  //utempl = RP-2;
  //printf("RPL %o/%o: XEC instruction %o|%o, ea is %o/%o, new inst = %o \n", utempl>>16, utempl&0xFFFF, inst, get16t(utempl+1), ea>>16, ea&0xFFFF, utempa);
  inst = utempa;
  earp = INCVA(ea,1);
  goto xec;

d_entr:  /* 00103 */
  TRACE(T_FLOW, " ENTR\n");
  utempa = getcrs16(S);
  putcrs16(S, getcrs16(S) - ea);
  put16t(utempa,getcrs16(S));
  goto fetch;

d_lpid:  /* LPID 00617 */
  TRACE(T_FLOW, " LPID\n");
  RESTRICT();
  goto d_uii;

d_badmr:
  printf("em: unknown memory reference, inst='%06o, opcode '%o, op index=%d\n", inst, opcode, opix);
  fault(UIIFAULT, RPL, RP);
  fatal(NULL);
}
