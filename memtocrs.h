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

