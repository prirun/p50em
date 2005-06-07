#define REGSETS 8

/* these are 16-bit offsets into crs (current register set) */

#define A 4
#define B 5
#define L 4
#define E 6

#define S 10
#define Y 10
#define X 14

#define FLTH 20
#define FLTL 21
#define FEXP 22
#define FLTD 23
#define VSC 22

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
#define FAC0H 8
#define FAC0L 9
#define FAC1H 10
#define FAC1L 11

  union {
    int rs[REGSETS][32];

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
      unsigned int tr0,tr1,tr2,tr3,tr4,tr5,tr6,tr7;
      unsigned int rdmx1,rdmx2;
      unsigned short rdum1[1],ratmpl;
      unsigned int rsgt1,rsgt2,recc1,recc2;
      unsigned short rdum2[1],reoiv,zero,one;
      unsigned int pbsave,rdmx3,rdmx4,c377,rdum3[3];
      unsigned int pswpb;
      unsigned short pswkeys,rdum4[1];
      unsigned short pla,pcba,plb,pcbb;
      unsigned int dswrma;
      unsigned int dswstat;
      unsigned int dswpb,rsavptr;
      unsigned short regdmx[64];
      unsigned int userregs[32*(REGSETS-2)];
    } sym;
  } regs;

unsigned short *crs;
unsigned int *crsl;

/* define mapping between memory addresses and the current register set */

unsigned short memtocrs[] = {
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
