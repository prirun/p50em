#define REGSETS 10

/* these are 16-bit offsets into crs (current register set) */

#define A 4
#define B 5
#define L 4
#define E 6

#define S 10
#define Y 10
#define YH 10
#define YL 11

#define X 14
#define XH 14
#define XL 15

#define FLTH 20
#define FLTL 21
#define FLTD 22
#define FEXP 23
#define VSC 23

#define PB 24
#define PBH 24
#define PBL 25

#define SB 26
#define SBH 26
#define SBL 27

#define LB 28
#define LBH 28
#define LBL 29

#define XB 30
#define XBH 30
#define XBL 31

#define DTAR3 32
#define DTAR2 34
#define DTAR1 36
#define DTAR0 38

#define KEYS 40
#define MODALS 41

#define OWNER 42
#define OWNERH 42
#define OWNERL 43
#define FCODE 44
#define FADDR 46
#define TIMER 48
#define TIMERH 48
#define TIMERL 49

/* I-mode offsets for 16-bit access to registers, eg, crs[GR0H] */

#define GR0H 0
#define GR1H 2
#define GR2H 4
#define GR3H 6
#define GR4H 8
#define GR5H 10
#define GR6H 12
#define GR7H 14

/* these are 32-bit offsets into crsl (current register set long) */

#define GR0 0
#define GR1 1
#define GR2 2
#define GR3 3
#define GR4 4
#define GR5 5
#define GR6 6
#define GR7 7
#define FAR0 8
#define FLR0 9
#define FAR1 10
#define FLR1 11
#define FAC0 8
#define FAC1 10
#define BR 12
#define OWNER32 (OWNERH/2)

static union {
    int rs[REGSETS][32];

    unsigned short rs16[REGSETS][64];

    /* locs '0-'177 as signed 32-bit integers */
    int s32[32*REGSETS];

    /* locs '0-'177 as unsigned 32-bit integers */
    unsigned int u32[32*REGSETS];

    /* locs '0-'377 as signed 16-bit integers */
    short s16[64*REGSETS];

    /* locs '0-'377 as signed 16-bit integers */
    unsigned short u16[64*REGSETS];

    /* symbolic register file locations */
    struct {
      unsigned int tr0,tr1,tr2,tr3,tr4,tr5,tr6,tr7;     /*  '0-7  */
      unsigned int rdmx1,rdmx2;                         /* '10-11 */
      unsigned short rdum1[1],ratmpl;                   /* '12    */
      unsigned int rsgt1,rsgt2,recc1,recc2;             /* '13-16 */
      unsigned short rdum2[1],reoiv,zero,one;           /* '17-20 */
      unsigned int pbsave,rdmx3,rdmx4,c377,rdum3[3];    /* '21-27 */
      unsigned int pswpb;                               /* '30    */
      unsigned short pswkeys,rdum4[1];                  /* '31    */
      unsigned short pla,pcba,plb,pcbb;                 /* '32-33 */
      unsigned int dswrma;                              /* '34    */
      unsigned int dswstat;                             /* '35    */
      unsigned int dswpb,rsavptr;                       /* '36-37 */
      unsigned short regdmx[64];                        /* '40-77 */
      unsigned int userregs[REGSETS-2][32];             /* '100-  */
    } sym;
  } regs;

/* the Prime program counter (RP) and pointer to current register set (crsl)
   can be either in a dedicated register or regular global variables.
   Putting them in dedicated registers gives about an 11% performance boost
   and reduces the code size from 146K to 136K */

#if 1

/* store RP and crsl in dedicated registers 29-30 (Power PC) */

#define RP rpreg.ul
#define RPH rpreg.s.rph
#define RPL rpreg.s.rpl

static unsigned int grp;      /* global RP for restore after longjmp */
register union {
  struct {
    unsigned short rph;
    unsigned short rpl;
  } s;
  unsigned int ul;
} rpreg asm ("r29");

static unsigned int *gcrsl;   /* global crs pointer for restore after longjmp */
register union {
  short *i16;
  unsigned short *u16;
  int *i32;
  unsigned int *u32;
  long long *i64;
  unsigned long long *u64;
} cr asm ("r30");

#else

/* the live program counter register is aka microcode scratch register TR7 */

#define RP regs.sym.tr7
#define RPH regs.u16[14]
#define RPL regs.u16[15]
#define grp RP              /* turns grp assignments into dummies */
#define gcrsl crsl          /* turns gcrsl assignments into dummies */

static union {
  short *i16;
  unsigned short *u16;
  int *i32;
  unsigned int *u32;
  long long *i64;
  unsigned long long *u64;
} cr;
#endif

#define crs  cr.u16
#define crsl cr.u32

#define PCBLEV 0
#define PCBLINK 1
#define PCBWAIT 2
#define PCBABT 4
#define PCBCPU 5
#define PCBPET 8
#define PCBDTAR2 10
#define PCBDTAR3 12
#define PCBIT 14
#define PCBMASK 16
#define PCBKEYS 17
#define PCBREGS 18
#define PCBFVEC 50
#define PCBFVR0 50
#define PCBFVR1 52
#define PCBFVR2 54
#define PCBFVR3 56
#define PCBFVPF 58
#define PCBCSFIRST 60
#define PCBCSNEXT 61
#define PCBCSLAST 62

/* define mapping between memory addresses and the current register set */

static unsigned short memtocrs[] = {
  X,      /* 0 = X */
  A,      /* 1 = A */
  B,      /* 2 = B */
  Y,      /* 3 = Y */
  FLTH,   /* 4 = FAC1/FLTH */
  FLTL,   /* 5 = FAC1/FLTL */
  FEXP,   /* 6 = FAC1/FEXP */
  -1,     /* 7 = PC (this is in the microcode scratch register set - TR7) */
  32,     /* 10 = unnamed */
  FCODE,  /* 11 = FCODE */
  FADDR+1,/* 12 = FADDR (word) */
  16,     /* 13 = unnamed */
  SBH,    /* 14 = unnamed (SB seg) */
  SBL,    /* 15 = unnamed (SB word) */
  LBH,    /* 16 = unnamed (LB seg) */
  LBL};    /* 17 = unnamed (LB word) */

