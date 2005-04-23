/* Pr1me Computer emulator, Jim Wilcoxson (jim@meritnet.com), April 4, 2005
   Copyright (C) 2005, Jim Wilcoxson (jim@meritnet.com).  All Rights Reserved.

   Restores a Prime R-mode .save image from stdin to memory and
   emulates execution.

   PLEASE NOTE: this is a very rough prototype still in development.
   The main goal for the prototype is to get an understanding of the
   kinds of design issues faced by an emulator, before doing a "real"
   Prime 50-series emulator.

   You are welcome to pass this along to friends for fun and
   amusement, but please don't publish it.  Comments, suggestions,
   corrections, and general notes that you're interested in a Prime
   emulation project are also welcome and appreciated.

   Usage:

   $ ./em <smad.save 2>/dev/null

   Lots of instruction details are spewed to stderr.


   Left to do (way more to do ... this just gives an idea):

   - only runs on a big-endian machine, like the Prime

   - nothing has been systematically verified/checked

   - many instructions are left to implement

   - svc doesn't handle alternate returns, LOC(blah) args might not be
   handled correctly, and it doesn't handle missing or extra arguments
   correctly (by looking for the terminating zero)

   - instructions don't update the C-bit, L-bit, and condition codes

   - restricted-mode instruction checking is missing, as well as all
   restricted-mode instructions and environment (for example, no page
   mapping)

*/

#include <stdio.h>
#include <errno.h>
#include "syscom/keys.ins.cc"
#include "syscom/errd.ins.cc"

/* Prime CPU registers are mapped to memory locations 0-'37, but only
   0-7 are accessible in SR user mode.  In VI user mode, locations
   0-'17 are trapped and map to the live register file (p 5-17, Sys Arch)
*/

#define X 0
#define A 1
#define B 2
#define S 3
#define Y 3
#define FLTH 4
#define FLTL 5
#define FLTD 07776<<16
#define FEXP 6
#define VSC 6
#define P 7
/* XXX: this is a huge hack! */
#define E 010
#define PMAR 010
#define FCODE 011
#define PFAR 012
#define DMA0 020
#define DMA1 022
#define DMA2 024
#define DMA3 026
#define DMA4 030
#define DMA5 032
#define DMA6 034
#define DMA7 036

/* Locations '40-'57 are reserved for 8 DMC channels, 2 words each.
   Locations '60-'77 are interrupt vectors
   Locations '100-'177 are for external device interrupts 
   see p. A-8 of Sys Arch
*/


#define SETCC_A \
  keys &= ~0300; \
  if (mem[A] == 0) \
    keys |= 0100; \
  else if (*(short *)(mem+A) < 0) \
    keys |= 0200;

#define SETCC_X \
  keys &= ~0300; \
  if (mem[X] == 0) \
    keys |= 0100; \
  else if (*(short *)(mem+X) < 0) \
    keys |= 0200;

#define SETCC_L \
  keys & ~0300; \
  if (*(int *)(mem+A) == 0) \
    keys |= 0100; \
  else if (*(int *)(mem+A) < 0) \
    keys |= 0200;
  
#define SETCC_F \
  keys & ~0300; \
  if (*(short *)(mem+FLTH) < 0) \
    keys |= 0200; \
  else if (*(int *)(mem+FLTH) == 0 && *(short *)(mem+FEXP) == 0) \
    keys |= 0100;

#define SETCC_D SETCC_F

#define SETC(onoff) \
  if ((onoff)) keys |= 0100000; \
  else keys &= 077777;

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
  0100200,         /* SMCR */
  0101200,         /* SMCS */
  000311,          /* VIFY */
  001113,          /* XVFY */
  000411};          /* CAI */

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
  "SMCR",
  "SMCS",
  "VIFY",
  "XVFY",
  "CAI"};


/* NOTES: 
   - mem is 1 user's virtual memory address space in this version,
   but eventually should be the system's physical memory
   - where to put the V/I mode live register file to make mapped
   memory access to registers, easy?
*/

unsigned short mem[07776*64*1024];   /* 4095 segments of 64K words each */

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

unsigned short prevpc,keys;          /* program counter, prev pc, keys */
unsigned short modals;

unsigned short amask;                /* address mask */
#define faultamask 0100000           /* fault bit */
#define ringamask 060000             /* ring bits */
#define extamask 010000              /* E-bit */
#define segamask 07777               /* segment number */

int verbose;
int domemdump;                         /* -memdump arg */
int boot;                            /* true if reading a boot record */

/* I/O device map table, containing function pointers to handle device I/O */

#include "emdev.h"

void (*devmap[64])(short, short, short) = {
  0,0,0,0,devasr,0,0,0,
  0,0,0,devmt,devmt,0,0,0,
  devcp,0,0,0,0,0,devdisk,devdisk,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0};

/* read a short (16-bit) integer in big-endian from stdin */

unsigned short readshort () {

  return getchar()<<8 | getchar();
}


/* set new processor keys */

newkeys (unsigned short new) {

  keys = new;
  switch ((keys & 016000) >> 10) {
  case 0:                     /* 16S */
    fprintf(stderr,"Entering 16S mode, keys=%o\n", keys);
    amask = 037777;
    break;
  case 1:                     /* 32S */
    fprintf(stderr,"Entering 32S mode, keys=%o\n", keys);
    amask = 077777;
    break;
  case 2:                     /* 64R */
    fprintf(stderr,"Entering 64R mode, keys=%o\n", keys);
    amask = 0177777;
    break;
  case 3:                     /* 32R */
    fprintf(stderr,"Entering 32R mode, keys=%o\n", keys);
    amask = 077777;
    break;
  case 4:                     /* 32I */
    fprintf(stderr,"Entering 32I mode, keys=%o\n", keys);
    amask = 0177777;
    exit(1);
    break;
  case 6:                     /* 64V */
    fprintf(stderr,"Entering 64V mode, keys=%o\n", keys);
    amask = 0177777;
    memdump(0,0177777);
    exit(1);
    break;
  default:                    /* invalid */
    fprintf(stderr,"Invalid CPU mode: %o\n", keys);
    exit(1);
  }
}



/* NOTE: This code is untested! */

unsigned short ea16s (unsigned short inst, short i, short x) {
  
  unsigned short ea;

  if (inst & 001000)
    ea = (prevpc & 037000) | (inst & 0777);      /* current sector */
  else
    ea = (inst & 0777);                          /* sector 0 */
  while (1) {
    if (x)                                       /* indexed */
      ea += mem[X];
    if (!i)                                      /* not indirect */
      break;
    i = mem[ea] & 0100000;
    x = mem[ea] & 040000;
    ea = mem[ea] & 037777;                       /* go indirect */
  }
  return ea;
}


/* NOTE: This code is untested! */

unsigned short ea32s (unsigned short inst, short i, short x) {
  
  unsigned short ea;

  if (inst & 001000)
    ea = (prevpc & 077000) | (inst & 0777);      /* current sector */
  else {
    ea = (inst & 0777);                          /* sector 0 */
    if (ea < 0100 && x) {                        /* preindex by X */
      ea += mem[X];
      x = 0;
    }
  }
  while (i) {
    i = mem[ea] & 0100000;
    ea = mem[ea] & 077777;                       /* go indirect */
  }
  if (x)                                         /* postindex */
    ea += mem[X];
  return ea & amask;
}


/* NOTE: the difference between 32R and 64R, besides the extra address
   bit, is that 32R indirect words have an indirect bit for multi-level
   indirects */

unsigned short ea32r64r (unsigned short inst, short i, short x, short *opcode) {

  short class;
  unsigned short ea;

  fprintf(stderr," ea32r64r: i=%o, x=%o\n", i!= 0, x!=0);
  if (inst & 001000)                             /* sector bit 7 set? */
    if ((inst & 0760) != 0400) {                 /* PC relative? */
      ea = mem[P] + (((short) (inst << 7)) >> 7);   /* yes, sign extend D */
      fprintf(stderr," PC relative, P=%o, new ea=%o\n", mem[P], ea);
    }
    else 
      goto special;                              /* special cases */
  else {
    ea = (inst & 0777);                          /* sector 0 */
    fprintf(stderr," Sector 0, new ea=%o\n", ea);
    if (ea < 0100 && x) {                        /* preindex by X */
      fprintf(stderr," Preindex, ea=%o, X='%o/%d\n", ea, mem[X], *(short *)(mem+X));
      ea += mem[X];
      fprintf(stderr," Preindex, new ea=%o\n", ea);
      x = 0;
    }
  }
  while (i) {
    fprintf(stderr," Indirect, old ea=%o, [ea]=%o\n", ea, mem[ea]);
    if ((keys & 016000) == 06000)
      i = mem[ea] & 0100000;
    else
      i = 0;
    ea = mem[ea] & amask;                       /* go indirect */
    fprintf(stderr," Indirect, new i=%d, new ea=%o\n", i!=0, ea);
  }
  if (x) {
    fprintf(stderr," Postindex, old ea=%o, X='%o/%d\n", ea, mem[X], *(short *)(mem+X));
    ea += mem[X];
    fprintf(stderr," Postindex, new ea=%o\n", ea);
  }
  return ea & amask;

special:
  class = inst & 3;                              /* class bits = 15 & 16 */
  *opcode = *opcode | ((inst >> 2) & 3);         /* opcode extension */
  fprintf(stderr," special, new opcode=%5#0o, class=%d\n", *opcode, class);

  if (class < 2) {                               /* class 0/1 */
    ea = mem[mem[P]++];                          /* get A from next word */
    fprintf(stderr," Class %d, new ea=%o\n", class, ea);
    if (class == 1)
      ea += mem[S];
    if (x) {
      fprintf(stderr," Preindex, ea=%o, X='%o/%d\n", ea, mem[X], *(short *)(mem+X));
      ea += mem[X];
      fprintf(stderr," Preindex, new ea=%o\n", ea);
    }
    while (i) {
      fprintf(stderr," Indirect, old ea=%o, [ea]=%o\n", ea, mem[ea]);
      if ((keys & 016000) == 06000)
	i = mem[ea] & 0100000;
      else
	i = 0;
      ea = mem[ea] & amask;
      fprintf(stderr," Indirect, new i=%d, new ea=%o\n", i!=0, ea);
    }

  } else if (i && x) {                           /* class 2/3, ix=11 */
    fprintf(stderr," class 2/3, ix=11\n");
    ea = mem[mem[P]++];                          /* get A from next word */
    fprintf(stderr," ea=%o\n", ea);
    if (class == 3)
      ea += (short) mem[S];
    while (i) {
      fprintf(stderr," Indirect, ea=%o, [ea]=%o\n", ea, mem[ea]);
      if ((keys & 016000) == 06000)
	i = mem[ea] & 0100000;
      else
	i = 0;
      ea = mem[ea] & amask;
      fprintf(stderr," Indirect, new i=%d, new ea=%o\n", i!=0, ea);
    }
    fprintf(stderr," Postindex, old ea=%o, X='%o/%d\n", ea, mem[X], *(short *)(mem+X));
    ea += (short) mem[X];
    fprintf(stderr," Postindex, new ea=%o\n", ea);

  } else {                                       /* class 2/3, ix != 11 */
    if (class == 2)
      ea = mem[S]++;
    else
      ea = --mem[S];
    fprintf(stderr," Class 2/3, new ea=%o, new S=%o\n", ea, mem[S]);
    if (x) {
      if ((keys & 016000) == 06000)
	i = mem[ea] & 0100000;
      ea = mem[ea] & amask;
    }
    while (i) {
      if ((keys & 016000) == 06000)
	i = mem[ea] & 0100000;
      else
	i = 0;
      ea = mem[ea] & amask;
    }
    if (x)
      ea += mem[X];
  }
  return ea & amask;
}

unsigned int ea64v (unsigned short inst, short i, short x, short *opcode) {

  unsigned short ea_s;                           /* eff address segno */
  unsigned short ea_r;                           /* eff address ring */
  unsigned short ea_w;                           /* eff address wordno */
  unsigned short r;                              /* true if same ring */
  unsigned int ea;                               /* full seg/word va */

  //  ea_s = reg[pb.h] & segmask;
  ea_w = mem[P];
  ea_r = 1;
  if (inst & 001000)                             /* sector bit 7 set? */
    if ((inst & 0740) != 0400) {                 /* PC relative? */
      ea_w = mem[P] + (((short) (inst << 7)) >> 7);   /* yes, sign extend D */
      fprintf(stderr," PC relative, P=%o, new ea_w=%o\n", mem[P], ea_w);
    }
    else 
      goto labB;                                 /* special cases */
  else if (i) {
    ea_w = (inst & 0777);                        /* sector 0 */
    fprintf(stderr," Sector 0, new ea_w=%o\n", ea_w);
    if (ea_w < 0100 && x) {                      /* preindex by X */
      fprintf(stderr," Preindex, ea_w=%o, X='%o/%d\n", ea_w, mem[X], *(short *)(mem+X));
      ea_w += mem[X];
      fprintf(stderr," Preindex, new ea_w=%o\n", ea_w);
      x = 0;
    }
  } else 
    goto labA;

  if (i) {
    fprintf(stderr," Indirect, old ea=%o, [ea]=%o\n", ea, mem[ea]);
    ea = mem[ea] & amask;                       /* go indirect */
    fprintf(stderr," Indirect, new i=%d, new ea=%o\n", i!=0, ea);
  }
  if (x) {
    fprintf(stderr," Postindex, old ea=%o, X='%o/%d\n", ea, mem[X], *(short *)(mem+X));
    ea += mem[X];
    fprintf(stderr," Postindex, new ea=%o\n", ea);
  }
  return ea;

labA:
labB:
  return ea;
}

unsigned int ea32i (unsigned short inst, short i, short x) {
  fprintf(stderr,"Mode 32I not implemented\n");
}


/* exception handler types:

  'i' = integer exception
  'd' = decimal exception
  'f' = floating point exception

  Depending on the keys settings, either set the C-bit or take the
  appropriate fault.

  Questions:

  1. Should the C-bit always get set, even if a fault is taken?
  2. Should the operation occur first, updating all registers, then
     the exception occurs?  (see PIMA)
*/

cpuexception(unsigned char extype)
{
  switch (extype) {
  case 'i':
    if (keys & 0400) {
      printf(" Integer exception fault\n");
      exit(1);
    } else {
      keys |= 0x8000;
    }
    break;
  case 'd':
    if (keys & 40) {
      printf(" Decimal exception fault\n");
      exit(1);
    } else {
      keys |= 0x8000;
    }
    break;
  case 'f':
    if (keys & 01000) {
      keys |= 0x8000;
    } else {
      printf(" FP exception fault\n");
      exit(1);
    }
    break;
  default:
    printf(" Unrecognized exception type '%c'\n", extype);
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


main (int argc, char **argv) {

  signed short tempa,tempa1,tempa2;
  unsigned short utempa;
  signed int templ,templ1,templ2;
  signed long long ll;
  unsigned int utempl;
  float tempf,tempf1,tempf2;
  double tempd,tempd1,tempd2;
  signed short tempda[4],tempda1[4];
  unsigned int ea32;                   /* full V/I mode eff address */
  unsigned short ea;                   /* effective address (word) */
  unsigned short opcode;
  short i,x;
  unsigned short class;
  unsigned short xx;
  int d;                               /* displacement */
  int nw;
  unsigned short rvec[9];    /* SA, EA, P, A, B, X, keys, dummy, dummy */
  unsigned short inst;
  int scount;                          /* shift count */
  int instcount=0;


  /* master clear:
     - clear all registers
     - set P to '1000
     - 16S mode, single precision
     - interrupts and machine checks inhibited
     - standard interrupt mode
  */

  verbose = 0;
  domemdump = 0;
  boot = 0;

  /* check args */

  for (i=1; i<argc; i++) {
    if (strcmp(argv[i],"--vv") == 0)
      verbose = 2;
    else if (strcmp(argv[i],"--v") == 0)
      verbose = 1;
    else if (strcmp(argv[i],"--memdump") == 0)
      domemdump = 1;
    else if (strcmp(argv[i],"--boot") == 0)
      boot = 1;
    else if (argv[i][0] == '-' && argv[i][1] == '-')
      fprintf(stderr,"Unrecognized argument: %s\n", argv[i]);
  }

  os_init();

  if (boot) {
    rvec[0] = 0760;
    rvec[1] = 0760+1040-1;
    rvec[2] = 01000;
    rvec[3] = rvec[4] = rvec[5] = 0;
    rvec[6] = 0;
  } else {

    /* read 9-word rvec header */

    for (i=0; i<9; i++)
      rvec[i] = readshort();
    fprintf(stderr,"SA=%o, EA=%o, P=%o, A=%o, B=%o, X=%o, K=%o\n\n", rvec[0], rvec[1],
	    rvec[2], rvec[3], rvec[4], rvec[5], rvec[6]);
    if (rvec[2] > rvec[1]) {
      fprintf(stderr,"Program start > EA: runfile is trashed\n");
      exit(1);
    }
  }

  /* read memory image from SA to EA inclusive */

  nw = rvec[1]-rvec[0]+1;
  if (fread(mem+rvec[0], sizeof(short), nw, stdin) != nw) {
    perror("Error reading memory image");
    exit(1);
  }

  /* setup execution (registers, keys, address mask, etc.) from rvec */

  modals = 0;
  mem[A] = rvec[3];
  mem[B] = rvec[4];
  mem[X] = rvec[5];
  newkeys(rvec[6]);
  mem[P] = rvec[2];

  if (mem[P] == 0161000)      /* hack for *DOS64; P is off by 3?? */
    mem[P] = 0161003;

  if (domemdump)
    memdump(rvec[0], rvec[1]);

  /* main instruction decode loop */

  while (1) {
    
    if (mem[P] > rvec[1]) {       /* hack for testing */
      fprintf(stderr,"\nOOPS! Program counter %o > EA %o\n", mem[P], rvec[1]);
      exit(1);
    }

    prevpc = mem[P];
    inst = mem[mem[P]];
    instcount++;
    fprintf(stderr,"\n%o: %o		A='%o/%:0d B='%o/%d X=%o/%d Y=%o/%d C=%d L=%d LT=%d EQ=%d	#%d\n", mem[P], inst, mem[A], *(short *)(mem+A), mem[B], *(short *)(mem+B), mem[X], *(short *)(mem+X), mem[Y], *(short *)(mem+Y), (keys&0100000) != 0, (keys&040000) != 0, (keys&0200) != 0, (keys&0100) != 0, instcount);
    mem[P]++;

    /* generic? */

    if ((inst & 036000) == 0) {
      fprintf(stderr," generic\n");
      class = inst>>14;
      if (class == 0) {
	fprintf(stderr," generic class 0\n");

	if (inst == 000005) {                 /* SGL */
	  fprintf(stderr," SGL\n");
	  newkeys(keys & ~040000);
	  continue;
	}

	if (inst == 000011) {                 /* E16S */
	  fprintf(stderr," E16S\n");
	  newkeys(keys & 0161777);
	  continue;
	}

	if (inst == 000013) {                 /* E32S */
	  fprintf(stderr," E32S\n");
	  newkeys((keys & 0161777) | 1<<10);
	  continue;
	}

	if (inst == 001013) {                 /* E32R */
	  fprintf(stderr," E32R\n");
	  newkeys((keys & 0161777) | 3<<10);
	  continue;
	}

	if (inst == 001011) {                 /* E64R */
	  fprintf(stderr," E64R\n");
	  newkeys((keys & 0161777) | 2<<10);
	  continue;
	}

	if (inst == 000010) {                 /* E64V */
	  fprintf(stderr," E64V\n");
	  newkeys((keys & 0161777) | 6<<10);
	  continue;
	}

	if (inst == 001010) {                 /* E32I */
	  fprintf(stderr," E32I\n");
	  newkeys((keys & 0161777) | 4<<10);
	  continue;
	}

	if (inst == 000505) {                 /* SVC */
	  fprintf(stderr," SVC\n");
	  svc();
	  continue;
	}

	if (inst == 000111) {                  /* CEA */
	  fprintf(stderr," CEA\n");
	  switch ((keys & 016000) >> 10) {
	  case 0:                       /* 16S */
	    ea = mem[A];
	    i = ea & 0100000;
	    x = ea & 040000;
	    ea &= 037777;
	    while (1) {
	      if (x)                           /* indexed */
		ea += mem[X];
	      if (!i)                          /* not indirect */
		break;
	      i = mem[ea] & 0100000;
	      x = mem[ea] & 040000;
	      ea = mem[ea] & 037777;           /* go indirect */
	    }
	    mem[A] = ea;
	    break;
	  case 1:                       /* 32S */
	  case 3:                       /* 32R */
	    while (mem[A] & 0100000) {
	      mem[A] = mem[mem[A] & 077777];
	    }
	  }
	  continue;
	}

	if (inst == 000000) {
	  fprintf(stderr," HLT\n");
	  fprintf(stderr,"\nProgram halt at %o\n", prevpc);
	  exit(1);
	}

	if (inst == 000201) {
	  fprintf(stderr," IAB\n");
	  tempa = mem[B];
	  mem[B] = mem[A];
	  mem[A] = tempa;
	  continue;
	}

	if (inst == 000205) {                /* PIM (R-mode) */
	  fprintf(stderr," PIM\n");
	  mem[A] = mem[B] | (mem[A] & 0x8000);
	  continue;
	}

	if (inst == 000211) {                /* PID (R-mode) */
	  fprintf(stderr," PID\n");
	  *(int *)(mem+A) = *(short *)(mem+A);
	  mem[B] &= 0x7fff;
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
	  fprintf(stderr," DBL\n");
	  newkeys(keys | 040000);
	  continue;
	}

	if (inst == 001314) {
	  fprintf(stderr," CGT\n");
	  tempa = mem[mem[P]];              /* get number of words */
	  if (1 <= mem[A] && mem[A] <= tempa)
	    mem[P] = mem[mem[P]+mem[A]];
	  else
	    mem[P] = mem[mem[P]+tempa+1];
	  continue;
	}

	if (inst == 000115) {
	  fprintf(stderr," PIDA\n");
	  *(int *)(mem+A) = *(short *)(mem+A);
	  continue;
	}

	/* XXX: how does PIMA affect registers when overflow occurs? */

	if (inst == 000015) {
	  fprintf(stderr," PIMA\n");
	  if (mem[A] == 0 | mem[A] == 0177777) {
	    keys &= 077777;
	    mem[A] = mem[B];
	  } else {
	    mem[A] = mem[B];
	    cpuexception('i');
	  }
	  continue;
	}

	if (inst == 000041) {
	  fprintf(stderr," SCA\n");
	  mem[A] = mem[VSC] & 0xFF;
	  continue;
	}

	if (inst == 000043) {
	  fprintf(stderr," INK\n");
	  mem[A] = (keys & 0xFF00) | (mem[VSC] & 0xFF);
	  continue;
	}

	if (inst == 001005) {
	  fprintf(stderr," TKA\n");
	  mem[A] = keys;
	  continue;
	}

	if (inst == 000405) {
	  fprintf(stderr," OTK\n");
	  newkeys(mem[A] & 0xFF00);
	  mem[VSC] = mem[A] & 0xFF;
	  continue;
	}

	if (inst == 001015) {
	  fprintf(stderr," TAK\n");
	  newkeys(mem[A]);
	  continue;
	}

	if (inst == 000001) {
	  fprintf(stderr," NOP\n");
	  continue;
	}

	if (inst == 000101) {
	  fprintf(stderr," NRM\n");
	  mem[VSC] = 0;
	  if (mem[A] == 0) {
	    if (mem[B] == 0)
	      continue;
	    mem[A] = mem[B];
	    mem[B] = 0;
	    mem[VSC] = 15;
	  } else if (mem[A] == 0xFFFF) {
	    mem[A] = mem[B] | 0x8000;
	    mem[B] = 0;
	    mem[VSC] = 15;
	  }
	  while (!((mem[A] ^ (mem[A] << 1)) & 0x8000)) {
	    fprintf(stderr, " step %d: mem[A]=%o, mem[B]=%o\n", mem[VSC], mem[A], mem[B]);
	    mem[B] = mem[B] << 1;
	    mem[A] = (mem[A] & 0x8000) | ((mem[A] << 1) & 0x7FFE) | (mem[B] >> 15);
	    mem[B] &= 0x7FFF;
	    mem[VSC]++;
	  }
	  fprintf(stderr, " finished with %d shifts: mem[A]=%o, mem[B]=%o\n", mem[VSC], mem[A], mem[B]);
	  continue;
	}

	if (inst == 000401) {
	  fprintf(stderr," ENB\n");
	  modals &= ~0100000;
	  continue;
	}

	if (inst == 001001) {
	  fprintf(stderr," INH\n");
	  modals |= 0100000;
	  continue;
	}

	if (inst == 000415) {
	  fprintf(stderr," ESIM\n");
	  modals &= ~040000;
	  continue;
	}

	if (inst == 000417) {
	  fprintf(stderr," EVIM\n");
	  modals |= 040000;
	  continue;
	}

	/* unusual restricted instructions */

	for (i=0; i<GEN0TABSIZE; i++) {
	  if (inst == gen0tab[i]) {
	    fprintf(stderr," %s\n", gen0nam[i]);
	    break;
	  }
	}
	if (i < GEN0TABSIZE)
	  continue;

	fprintf(stderr," unrecognized generic class 0 instruction!\n");
	if (mem[066] != 0) {
	  fprintf(stderr," JST* '66 [%o]\n", mem[066]);
	  mem[mem[066]] = mem[P];
	  mem[P] = mem[066]+1;
	  continue;
	}

	exit(1);
	
      }

      if (class == 3) {
	fprintf(stderr," generic class 3\n");

	if (inst == 0141604) {
	  fprintf(stderr," BCLT\n");
bclt:
	  if (keys & 0200)
	    mem[P] = mem[mem[P]];
	  else
	    mem[P]++;
	  continue;
	}

	if (inst == 0141600) {
	  fprintf(stderr," BCLE\n");
bcle:
	  if (keys & 0300)
	    mem[P] = mem[mem[P]];
	  else
	    mem[P]++;
	  continue;
	}

	if (inst == 0141602) {
	  fprintf(stderr," BCEQ\n");
bceq:
	  if (keys & 0100)
	    mem[P] = mem[mem[P]];
	  else
	    mem[P]++;
	  continue;
	}

	if (inst == 0141603) {
	  fprintf(stderr," BCNE\n");
bcne:
	  if (!(keys & 0100))
	    mem[P] = mem[mem[P]];
	  else
	    mem[P]++;
	  continue;
	}

	if (inst == 0141605) {
	  fprintf(stderr," BCGE\n");
bcge:
	  if (!(keys & 0200))
	    mem[P] = mem[mem[P]];
	  else
	    mem[P]++;
	  continue;
	}

	if (inst == 0141601) {
	  fprintf(stderr," BCGT\n");
bcgt:
	  if (!(keys & 0300))
	    mem[P] = mem[mem[P]];
	  else
	    mem[P]++;
	  continue;
	}

	if (inst == 0141705) {
	  fprintf(stderr," BCR\n");
	  if (!(keys & 0100000))
	    mem[P] = mem[mem[P]];
	  else
	    mem[P]++;
	  continue;
	}

	if (inst == 0141704) {
	  fprintf(stderr," BCS\n");
	  if (keys & 0100000)
	    mem[P] = mem[mem[P]];
	  else
	    mem[P]++;
	  continue;
	}

	if (inst == 0141707) {
	  fprintf(stderr," BLR\n");
	  exit(1);
	  if (!(keys & 020000))
	    mem[P] = mem[mem[P]];
	  else
	    mem[P]++;
	  continue;
	}

	if (inst == 0141706) {
	  fprintf(stderr," BLS\n");
	  exit(1);
	  if (keys & 020000)
	    mem[P] = mem[mem[P]];
	  else
	    mem[P]++;
	  continue;
	}

	if (inst == 0140614) {
	  fprintf(stderr," BLT\n");
	  SETCC_A;
	  goto bclt;
	}

	if (inst == 0140610) {
	  fprintf(stderr," BLE\n");
	  SETCC_A;
	  goto bcle;
	}

	if (inst == 0140612) {
	  fprintf(stderr," BEQ\n");
	  SETCC_A;
	  goto bceq;
	}

	if (inst == 0140613) {
	  fprintf(stderr," BNE\n");
	  SETCC_A;
	  goto bcne;
	}

	if (inst == 0140615) {
	  fprintf(stderr," BGE\n");
	  SETCC_A;
	  goto bcge;
	}

	if (inst == 0140611) {
	  fprintf(stderr," BGT\n");
	  SETCC_A;
	  goto bcgt;
	  continue;
	}

	if (inst == 0140700) {
	  fprintf(stderr," BLLE\n");
	  SETCC_L;
	  goto bcle;
	}

	if (inst == 0140702) {
	  fprintf(stderr," BLEQ\n");
	  SETCC_L;
	  goto bceq;
	}

	if (inst == 0140703) {
	  fprintf(stderr," BLNE\n");
	  SETCC_L;
	  goto bcne;
	}

	if (inst == 0140701) {
	  fprintf(stderr," BLGT\n");
	  SETCC_L;
	  goto bcgt;
	}

	if (inst == 0141614) {
	  fprintf(stderr," BFLT\n");
	  SETCC_F;
	  goto bclt;
	}

	if (inst == 0141610) {
	  fprintf(stderr," BFLE\n");
	  SETCC_F;
	  goto bcle;
	}

	if (inst == 0141612) {
	  fprintf(stderr," BFEQ\n");
	  SETCC_F;
	  goto bceq;
	}

	if (inst == 0141613) {
	  fprintf(stderr," BFNE\n");
	  SETCC_F;
	  goto bcne;
	}

	if (inst == 0141615) {
	  fprintf(stderr," BFGE\n");
	  SETCC_F;
	  goto bcge;
	}

	if (inst == 0141611) {
	  fprintf(stderr," BFGT\n");
	  SETCC_F;
	  goto bcgt;
	}

	if (inst == 0141334) {
	  fprintf(stderr," BIX\n");
	  mem[X]++;
bidx:
	  if (mem[X] == 0)
	    mem[P] = mem[mem[P]];
	  else
	    mem[P]++;
	  continue;
	}

	if (inst == 0141324) {
	  fprintf(stderr," BIY\n");
	  mem[Y]++;
bidy:
	  if (mem[Y] == 0)
	    mem[P] = mem[mem[P]];
	  else
	    mem[P]++;
	  continue;
	}

	if (inst == 0140724) {
	  fprintf(stderr," BDY\n");
	  mem[Y]--;
	  goto bidy;
	}

	if (inst == 0140734) {
	  fprintf(stderr," BDX\n");
	  mem[X]--;
	  goto bidx;
	}

	if (inst == 0141206) {                /* A1A */
	  fprintf(stderr," A1A\n");
	  mem[A]++;
	  SETC(mem[A] == 32768);
	  SETCC_A;
	  continue;
	}

	if (inst == 0140304) {                /* A2A */
	  fprintf(stderr," A2A\n");
	  mem[A] += 2;
	  SETC(*(short *)(mem+A) <= -32767);
	  SETCC_A;
	  continue;
	}

	if (inst == 0141216) {                /* ACA */
	  fprintf(stderr," ACA\n");
	  if (keys & 0100000) {
	    mem[A]++;
	    SETC(mem[A] == 32768);
	  } else 
	    SETC(0);
	  SETCC_A;
	  continue;
	}

	if (inst == 0140110) {                /* S1A */
	  fprintf(stderr," S1A\n");
	  mem[A]--;
	  SETC(mem[A] == 32767);
	  SETCC_A;
	  continue;
	}

	if (inst == 0140310) {                /* S2A */
	  fprintf(stderr," S2A\n");
	  mem[A] -= 2;
	  SETC(*(short *)(mem+A) >= -32766);
	  SETCC_A;
	  continue;
	}

	if (inst == 0141050) {                /* CAL */
	  fprintf(stderr," CAL\n");
	  mem[A] &= 0xFF;
	  continue;
	}

	if (inst == 0141044) {                /* CAR */
	  fprintf(stderr," CAR\n");
	  mem[A] &= 0xFF00;
	  continue;
	}

	if (inst == 0140040) {                /* CRA */
	  fprintf(stderr," CRA\n");
	  mem[A] = 0;
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

	if (inst == 0140014) {                /* P300 CRB */
	  fprintf(stderr," P300CRB\n");
	  mem[B] = 0;
	  continue;
	}

	if (inst == 0140015) {                /* CRB */
	  fprintf(stderr," CRB\n");
	  mem[B] = 0;
	  continue;
	}

	if (inst == 0140016) {                /* FDBL */
	  fprintf(stderr," FDBL\n");
	  continue;
	}

	if (inst == 0140010) {                /* CRL */
	  fprintf(stderr," CRL\n");
	  mem[A] = 0; mem[B] = 0;
	  continue;
	}

	/* this should set cc */

	if (inst == 0140214) {                /* CAZ */
	  fprintf(stderr," CAZ\n");
	  SETCC_A;
	  goto compskip;
	  continue;
	}

	/* NOTE: using "if mem[X]++ == 0" doesn't work because of unsigned
	   short type promotion! */

	if (inst == 0140114) {                /* IRX */
	  fprintf(stderr," IRX\n");
	  mem[X]++;
	  if (mem[X] == 0)
	    mem[P]++;
	  continue;
	}

	if (inst == 0140210) {                /* DRX */
	  fprintf(stderr," DRX\n");
	  mem[X]--;
	  if (mem[X] == 0)
	    mem[P]++;
	  continue;
	}

	if (inst == 0141240) {                /* ICR */
	  fprintf(stderr," ICR\n");
	  mem[A] = mem[A] << 8;
	  continue;
	}

	if (inst == 0141140) {                /* ICL */
	  fprintf(stderr," ICL\n");
	  mem[A] = mem[A] >> 8;
	  continue;
	}

	if (inst == 0141340) {                /* ICA */
	  fprintf(stderr," ICA\n");
	  mem[A] = (mem[A] >> 8) | (mem[A] << 8);
	  continue;
	}

	if (inst == 0140417) {                /* LT */
	  fprintf(stderr," LT\n");
	  mem[A] = 1;
	  keys = (keys & ~0300);
	  continue;
	}

	if (inst == 0140416) {                /* LF */
	  fprintf(stderr," LF\n");
	  mem[A] = 0;
	  keys = (keys & ~0300) | 0100;
	  continue;
	}

	if (inst == 0140314) {
	  fprintf(stderr," TAB\n");
	  mem[B] = mem[A];
	  continue;
	}

	if (inst == 0140504) {
	  fprintf(stderr," TAX\n");
	  mem[X] = mem[A];
	  continue;
	}

	if (inst == 0140505) {
	  fprintf(stderr," TAY\n");
	  mem[Y] = mem[A];
	  continue;
	}

	if (inst == 0140604) {
	  fprintf(stderr," TBA\n");
	  mem[A] = mem[B];
	  continue;
	}

	if (inst == 0141034) {
	  fprintf(stderr," TXA\n");
	  mem[A] = mem[X];
	  continue;
	}

	if (inst == 0141124) {
	  fprintf(stderr," TYA\n");
	  mem[A] = mem[Y];
	  continue;
	}

	if (inst == 0140104) {
	  fprintf(stderr," XCA\n");
	  mem[B] = mem[A];
	  mem[A] = 0;
	  continue;
	}

	if (inst == 0140204) {
	  fprintf(stderr," XCB\n");
	  mem[A] = mem[B];
	  mem[B] = 0;
	  continue;
	}

	if (inst == 0140407) {
	  fprintf(stderr," TCA\n");
	  *(short *)(mem+A) = - (*(short *)(mem+A));
	  SETCC_A;
	  if (*(unsigned short *)(mem+A) == 0x8000)
	    cpuexception('i');
	  continue;
	}

	if (inst == 0141210) {
	  fprintf(stderr," TCL\n");
	  *(int *)(mem+A) = - (*(int *)(mem+A));
	  SETCC_L;
	  if (*(unsigned int *)(mem+A) == 0x80000000)
	    cpuexception('i');
	  continue;
	}

	if (inst == 0140600) {
	  fprintf(stderr," SCB\n");
	  newkeys(keys | 0100000);
	  continue;
	}

	if (inst == 0140200) {
	  fprintf(stderr," RCB\n");
	  newkeys(keys & 077777);
	  continue;
	}

	if (inst == 0140024) {
	  fprintf(stderr," CHS\n");
	  mem[A] ^= 0x8000;
	  continue;
	}

	if (inst == 0140500) {
	  fprintf(stderr," SSM\n");
	  mem[A] |= 0100000;
	  continue;
	}

	if (inst == 0140100) {
	  fprintf(stderr," SSP\n");
	  mem[A] &= 077777;
	  continue;
	}

	if (inst == 0140401) {
	  fprintf(stderr," CMA\n");
	  mem[A] = ~mem[A];
	  continue;
	}

	if (inst == 0140320) {
	  fprintf(stderr," CSA\n");
	  newkeys((keys & 077777) | (mem[A] & 0x8000));
	  mem[A] = mem[A] & 077777;
	  continue;
	}

	if (inst == 0141500) {
	  fprintf(stderr," LCLT\n");
lclt:
	  mem[A] = ((keys & 0300) == 0200);
	  continue;
	}

	if (inst == 0141501) {
	  fprintf(stderr," LCLE\n");
lcle:
	  mem[A] = ((keys & 0300) != 0);
	  continue;
	}

	if (inst == 0141503) {
	  fprintf(stderr," LCEQ\n");
lceq:
	  mem[A] = ((keys & 0100) != 0);
	  continue;
	}

	if (inst == 0141502) {
	  fprintf(stderr," LCNE\n");
lcne:
	  mem[A] = ((keys & 0100) == 0);
	  continue;
	}

	if (inst == 0141504) {
	  fprintf(stderr," LCGE\n");
lcge:
	  mem[A] = !(keys & 0200) || (keys & 0100);
	  continue;
	}

	if (inst == 0141505) {
	  fprintf(stderr," LCGT\n");
lcgt:
	  mem[A] = ((keys & 0300) == 0);
	  continue;
	}

	if (inst == 0140410) {
	  fprintf(stderr," LLT\n");
	  SETCC_A;
	  goto lclt;
	}

	if (inst == 0140411) {
	  fprintf(stderr," LLE\n");
	  SETCC_A;
	  goto lcle;
	}

	if (inst == 0140412) {
	  fprintf(stderr," LNE\n");
	  SETCC_A;
	  goto lcne;
	}

	if (inst == 0140413) {
	  fprintf(stderr," LEQ\n");
	  SETCC_A;
	  goto lceq;
	}

	if (inst == 0140414) {
	  fprintf(stderr," LGE\n");
	  SETCC_A;
	  goto lcge;
	}

	if (inst == 0140415) {
	  fprintf(stderr," LGT\n");
	  SETCC_A;
	  goto lcgt;
	}

	if (inst == 0140511) {
	  fprintf(stderr," LLLE\n");
	  SETCC_L;
	  goto lcle;
	}

	if (inst == 0141513) {
	  fprintf(stderr," LLEQ\n");
	  SETCC_L;
	  goto lceq;
	}

	if (inst == 0141512) {
	  fprintf(stderr," LLNE\n");
	  SETCC_L;
	  goto lcne;
	  mem[A] = *(int *)(mem+A) != 0;
	  continue;
	}

	if (inst == 0141515) {
	  fprintf(stderr," LLGT\n");
	  SETCC_L;
	  goto lcgt;
	}

	if (inst == 0141110) {
	  fprintf(stderr," LFLT\n");
	  SETCC_F;
	  goto lclt;
	}

	if (inst == 0141111) {
	  fprintf(stderr," LFLE\n");
	  SETCC_F;
	  goto lcle;
	}

	if (inst == 0141113) {
	  fprintf(stderr," LFEQ\n");
	  SETCC_F;
	  goto lceq;
	}

	if (inst == 0141112) {
	  fprintf(stderr," LFNE\n");
	  SETCC_F;
	  goto lcne;
	}

	if (inst == 0141114) {
	  fprintf(stderr," LFGE\n");
	  SETCC_F;
	  goto lcge;
	}

	if (inst == 0141115) {
	  fprintf(stderr," LFGT\n");
	  SETCC_F;
	  goto lcgt;
	}

	if (inst == 0140550) {
	  fprintf(stderr," FLOT\n");
	  templ = mem[A];
	  templ = mem[B] | (templ<<15);
	  tempf = templ;
	  ieeepr4(&tempf);
	  mem[FLTH] = (*(unsigned int *)&tempf) >> 16;
	  mem[FLTL] = (*(unsigned int *)&tempf) & 0xFF00;
	  mem[FEXP] = (*(unsigned int *)&tempf) & 0xFF;
	  continue;
	}

	if (inst == 0140534) {
	  fprintf(stderr," FRN\n");
	  continue;
	}

	if (inst == 0140574) {
	  fprintf(stderr," DFCM\n");
	  fprintf(stderr," FEXP=%d (dec), FLTH='%o, FLTL='%o\n", mem[FEXP], mem[FLTH], mem[FLTL]);
	  tempda[0] = mem[FLTH];
	  tempda[1] = mem[FLTL];
	  tempda[2] = 0;
	  tempda[3] = mem[FEXP];
	  prieee8(tempda); 
	  *(double *)tempda = -(*(double *)tempda);
	  ieeepr8(tempda);
	  mem[FLTH] = tempda[0];
	  mem[FLTL] = tempda[1];
	  mem[FEXP] = tempda[3];
	  SETC(0);
	  continue;
	}

	if (inst == 0140000) {
	  fprintf(stderr," ADLL\n");
	  exit(1);
	  if (keys & 020000)
	    (*(int *)(mem+A))++;
	  continue;
	}

	if (inst == 0140530) {
	  fprintf(stderr," FCM\n");
	  fprintf(stderr," FEXP=%d (dec), FLTH='%o, FLTL='%o\n", mem[FEXP], mem[FLTH], mem[FLTL]);
	  *(int *)&tempf = (mem[FLTH]<<16) | (mem[FLTL] & 0xFF00) | (mem[FEXP] & 0xFF);
	  prieee4(&tempf);
	  tempf = -tempf;
	  ieeepr4(&tempf);
	  mem[FLTH] = (*(unsigned int *)&tempf) >> 16;
	  mem[FLTL] = (*(unsigned int *)&tempf) & 0xFF00;
	  mem[FEXP] = (*(unsigned int *)&tempf) & 0xFF;
	  SETC(0);
	  continue;
	}

	if (inst == 0140510) {
	  fprintf(stderr," FSZE\n");
	  if (*(int *)(mem+FLTH) == 0 && mem[FEXP] == 0)
	    mem[P]++;
	  continue;
	}

	if (inst == 0140511) {
	  fprintf(stderr," FSNZ\n");
	  if (*(int *)(mem+FLTH) != 0 || mem[FEXP] != 0)
	    mem[P]++;
	  continue;
	}

	if (inst == 0140512) {
	  fprintf(stderr," FSMI\n");
	  if (*(int *)(mem+FLTH) < 0)
	    mem[P]++;
	  continue;
	}

	if (inst == 0140513) {
	  fprintf(stderr," FSPL\n");
	  if (*(int *)(mem+FLTH) >= 0)
	    mem[P]++;
	  continue;
	}

	if (inst == 0140514) {
	  fprintf(stderr," FSLE\n");
	  if (*(int *)(mem+FLTH) < 0 || (*(int *)(mem+FLTH) == 0 && mem[FEXP] == 0))
	    mem[P]++;
	  continue;
	}

	if (inst == 0140515) {
	  fprintf(stderr," FSGT\n");
	  if (*(int *)(mem+FLTH) >= 0 && (*(int *)(mem+FLTH) != 0 || mem[FEXP] != 0))
	    mem[P]++;
	  continue;
	}

	if (inst == 0140554) {
	  fprintf(stderr," INT\n");
	  *(int *)&tempf = (mem[FLTH]<<16) | (mem[FLTL] & 0xFF00) | (mem[FEXP] & 0xFF);
	  prieee4(&tempf);
	  templ = tempf;
	  mem[B] = templ & 0x7FFF;
	  mem[A] = templ >> 15;
	  continue;
	}

	if (inst == 0141414) {
	  fprintf(stderr," ILE\n");
	  templ = *(int *)(mem+A);
	  *(int *)(mem+A) = *(int *)(mem+E);
	  *(int *)(mem+E) = templ;
	  continue;
	}

	if (inst == 0141707) {
	  fprintf(stderr," BMLT\n");
	  if (keys & 020000)
	    goto bclt;
	  mem[P]++;
	  continue;
	}

	if (inst == 0141711) {
	  fprintf(stderr," BMLE\n");
	  if (keys & 020000)
	    goto bcle;
	  mem[P]++;
	  continue;
	}

	if (inst == 0141602) {
	  fprintf(stderr," BMEQ\n");
	  if (keys & 020000)
	    goto bceq;
	  mem[P]++;
	  continue;
	}

	if (inst == 0141603) {
	  fprintf(stderr," BMNE\n");
	  if (keys & 020000)
	    goto bcne;
	  mem[P]++;
	  continue;
	}

	if (inst == 0141606) {
	  fprintf(stderr," BMGE\n");
	  if (keys & 020000)
	    goto bcge;
	  mem[P]++;
	  continue;
	}

	if (inst == 0141710) {
	  fprintf(stderr," BMGT\n");
	  if (keys & 020000)
	    goto bcgt;
	  mem[P]++;
	  continue;
	}

	fprintf(stderr," unrecognized generic class 3 instruction!\n");
	if (mem[066] != 0) {
	  fprintf(stderr," JST* '66 [%o]\n", mem[066]);
	  mem[mem[066]] = mem[P];
	  mem[P] = mem[066]+1;
	  continue;
	}

	exit(1);
	
      }


      if (class == 1) {
	fprintf(stderr," shift group\n");
	scount = -inst & 077;
	if (scount > 0)
	  switch (inst & 01700) {

	  case 00000: /* LRL */
	    fprintf(stderr," LRL %d\n", scount);
	    if (scount <= 32) {
	      utempl = *(unsigned int *)(mem+A);
	      SETC(utempl & bitmask32[33-scount]);
	      utempl = utempl >> scount;
	      *(unsigned int *)(mem+A) = utempl;
	    } else {
	      *(unsigned int *)(mem+A) = 0;
	      SETC(0);
	    }
	    break;

	  case 00100: /* LRS (different in R & V modes) */
	    fprintf(stderr," LRS %d\n", scount);
	    if (keys & 010000) {          /* V/I mode */
	      if (scount <= 32) {
		templ = *(int *)(mem+A);
		SETC(templ & bitmask32[33-scount]);
		templ = templ >> scount;
		*(int *)(mem+A) = templ;
	      } else if (mem[A] & 0x8000) {
		*(int *)(mem+A) = 0xFFFFFFFF;
		SETC(1);
	      } else {
		*(int *)(mem+A) = 0;
		SETC(0);
	      }
	    } else {
	      if (scount <= 31) {
		templ = mem[A];
		templ = (templ<<16) | (mem[B]<<1);
		SETC(templ & bitmask32[32-scount]);
		templ = templ >> (scount+1);
		mem[A] = templ >> 15;
		mem[B] = templ & 0x7FFF;
	      } else if (mem[A] & 0x8000) {
		*(int *)(mem+A) = 0xFFFF7FFF;
		SETC(1);
	      } else {
		*(int *)(mem+A) = 0;
		SETC(0);
	      }
	    }
	    break;

	  case 00200: /* LRR */
	    fprintf(stderr," LRR %d\n", scount);
	    if (scount > 32)
	      scount = scount - 32;
	    utempl = *(unsigned int *)(mem+A);
	    SETC(utempl & bitmask32[33-scount]);
	    utempl = (utempl >> scount) | (utempl << (32-scount));
	    *(unsigned int *)(mem+A) = utempl;
	    break;

	  case 00400: /* ARL */
	    fprintf(stderr," ARL %d\n", scount);
	    if (scount <= 16) {
	      utempa = mem[A];
	      SETC(utempa & bitmask16[17-scount]);
	      utempa = utempa >> scount;
	      mem[A] = utempa;
	    } else {
	      mem[A] = 0;
	      SETC(0);
	    }
	    break;

	  case 00500: /* ARS */
	    fprintf(stderr," ARS %d\n", scount);
	    if (scount <= 16) {
	      tempa = *(short *)(mem+A);
	      SETC(tempa & bitmask16[17-scount]);
	      tempa = tempa >> scount;
	      *(short *)(mem+A) = tempa;
	    } else if (mem[A] & 0x8000) {
	      *(short *)(mem+A) = 0xFFFF;
	      SETC(1);
	    } else {
	      *(short *)(mem+A) = 0;
	      SETC(0);
	    }
	    break;

	  case 00600: /* ARR */
	    fprintf(stderr," ARR %d\n", scount);
	    scount = ((scount-1)%16)+1;   /* make scount 1-16 */
	    SETC(mem[A] & bitmask16[17-scount]);
	    mem[A] = (mem[A] >> scount) | (mem[A] << (16-scount));
	    break;

	  case 01000: /* LLL */
	    fprintf(stderr," LLL %d\n", scount);
	    if (scount <= 32) {
	      utempl = *(unsigned int *)(mem+A);
	      SETC(utempl & bitmask32[scount]);
	      utempl = utempl << scount;
	      *(unsigned int *)(mem+A) = utempl;
	    } else {
	      *(unsigned int *)(mem+A) = 0;
	      SETC(0);
	    }
	    break;

	  case 01100: /* LLS (different in R/V modes) */
	    fprintf(stderr," LLS %d\n", scount);
	    if (keys & 010000) {          /* V/I mode */
	      if (scount < 32) {
		templ = 0x80000000;
		templ = templ >> scount;         /* create mask */
		templ = templ & *(int *)(mem+A); /* grab bits */
		templ = templ >> (31-scount);    /* extend them */
		SETC(!(templ == -1 || templ == 0));
		*(int *)(mem+A) = *(int *)(mem+A) << scount;
	      } else {
		*(int *)(mem+A) = 0;
		SETC(0);
	      }
	    } else {
              if (scount < 31) {
                templ = mem[A];
                templ = (templ<<16) | (mem[B]<<1);
		templ2 = 0x80000000;
		templ2 = templ2 >> scount;         /* create mask */
		templ2 = templ2 & templ;           /* grab bits */
		templ2 = templ2 >> (31-scount);    /* extend them */
		SETC(!(templ2 == -1 || templ2 == 0));
                templ = templ << scount;
                mem[A] = templ >> 16;
                mem[B] = (templ >> 1) & 0x7FFF;
              } else {
                *(int *)(mem+A) = 0;
                SETC(0);
              }
	    }
	    break;

	  case 01200: /* LLR */
	    fprintf(stderr," LLR %d\n", scount);
	    if (scount > 32)
	      scount = scount - 32;
	    utempl = *(unsigned int *)(mem+A);
	    SETC(utempl & bitmask32[scount]);
	    utempl = (utempl << scount) | (utempl >> (32-scount));
	    *(unsigned int *)(mem+A) = utempl;
	    break;

	  case 01400: /* ALL */
	    fprintf(stderr," ALL %d\n", scount);
	    if (scount <= 16) {
	      SETC(mem[A] & bitmask16[scount]);
	      mem[A] = mem[A] << scount;
	    } else {
	      mem[A] = 0;
	      SETC(0);
	    }
	    break;

	  case 01500: /* ALS */
	    fprintf(stderr," ALS %d\n", scount);
	    if (scount <= 15) {
	      tempa = 0100000;
	      tempa = tempa >> scount;         /* create mask */
	      tempa = tempa & mem[A];          /* grab bits */
	      tempa = tempa >> (15-scount);    /* extend them */
	      SETC(!(tempa == -1 || tempa == 0));
	      mem[A] = mem[A] << scount;
	    } else if (mem[A] == 0) {
	      SETC(0);
	    } else {
	      mem[A] = 0;
	      SETC(1);
	    }
	    break;

	  case 01600: /* ALR */
	    fprintf(stderr," ALR %d\n", scount);
	    scount = ((scount-1)%16)+1;   /* make scount 1-16 */
	    SETC(mem[A] & bitmask16[scount]);
	    mem[A] = (mem[A] << scount) | (mem[A] >> (16-scount));
	    break;

	  default:
	    fprintf(stderr," unrecognized shift instruction\n");
	    exit(1);
	  }
	continue;
      }

      if (class == 2) {
	fprintf(stderr," skip group\n");

	if (inst == 0101000) {
	  fprintf(stderr," NOP\n");
	  continue;
	}

	if (inst == 0100000) {
	  fprintf(stderr," SKP\n");
	  mem[P]++;
	  continue;
	}

	if (inst == 0101400) {
	  fprintf(stderr," SMI/SLT\n");
	  if (*(short *)(mem+A) < 0)
	    mem[P]++;
	  continue;
	}

	if (inst == 0100400) {
	  fprintf(stderr," SPL/SGE\n");
	  if (*(short *)(mem+A) >= 0)
	    mem[P]++;
	  continue;
	}

	if (inst == 0101100) {
	  fprintf(stderr," SLN\n");
	  if (mem[A] & 1)
	    mem[P]++;
	  continue;
	}

	if (inst == 0100100) {
	  fprintf(stderr," SLZ\n");
	  if (!(mem[A] & 1))
	    mem[P]++;
	  continue;
	}

	if (inst == 0101040) {
	  fprintf(stderr," SNZ/SNE\n");
	  if (mem[A] != 0)
	    mem[P]++;
	  continue;
	}

	if (inst == 0100040) {
	  fprintf(stderr," SZE/SEQ\n");
	  if (mem[A] == 0)
	    mem[P]++;
	  continue;
	}

	if (inst == 0101220) {
	  fprintf(stderr," SLE\n");
	  if (*(short *)(mem+A) <= 0)
	    mem[P]++;
	  continue;
	}

	if (inst == 0100220) {
	  fprintf(stderr," SGT\n");
	  if (*(short *)(mem+A) > 0)
	    mem[P]++;
	  continue;
	}

	if (inst == 0101001) {
	  fprintf(stderr," SSC\n");
	  if (keys & 0100000)
	    mem[P]++;
	  continue;
	}

	if (inst == 0100001) {
	  fprintf(stderr," SRC\n");
	  if (!(keys & 0100000))
	    mem[P]++;
	  continue;
	}

	if ((inst & 0177760) == 0100260) {
	  fprintf(stderr," SAR %d\n", (inst & 017)+1);
	  if (!(mem[A] & bitmask16[(inst & 017)+1]))
	    mem[P]++;
	  continue;
	}

	if ((inst & 0177760) == 0101260) {
	  fprintf(stderr," SAS %d\n", (inst & 017)+1);
	  if (mem[A] & bitmask16[(inst & 017)+1])
	    mem[P]++;
	  continue;
	}

	fprintf(stderr," unrecognized skip instruction\n");
	exit(1);

      }
    }

    /* here for non-generic instructions: memory references or pio */

    if (keys & 010000) {
      fprintf(stderr," VI-mode MR decode\n");
      exit(1);
    }

    /* pio? */

    if ((inst & 036000) == 030000) {
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
       by X, so if an instruction specifies indexing by X, it acts
       like an opcode extension.  Opcodes listed as '35 02 for example
       (sty in V-mode, jdx in R-mode) have X=1 with the 4 opcode bits
       1101 ('15)

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
    
    if (opcode == 01500) {          /* '15 = ldx/stx form, depending on X */
      opcode = opcode | ((inst & 040000)>>4);   /* if X set, expand opcode */
      x = 0;                        /* clear X bit (these can't be indexed) */
      fprintf(stderr," ldx/stx opcode adjusted\n");
    }

    fprintf(stderr," opcode=%5#0o, i=%o, x=%o\n", opcode, i, x);

    switch ((keys & 016000) >> 10) {
    case 0:  /* 16S */
      ea = ea16s(inst, i, x);
      break;
    case 1:  /* 32S */
      ea = ea32s(inst, i, x);
      break;
    case 2:  /* 64R */
    case 3:  /* 32R */
      ea = ea32r64r(inst, i, x, &opcode);
      break;
    case 4:  /* 32I */
      ea = ea32i(inst, i, x);
      exit(1);
      break;
    case 6:  /* 64V */
      ea = ea64v(inst, i, x, &opcode);
      exit(1);
      break;
    default:
      fprintf(stderr,"Bad CPU mode in EA calculation, keys = %o\n", keys);
      exit(1);
    }

    fprintf(stderr," ea=%o (%o/%o), [ea]='%o/%d\n", ea, ea>>16, ea & 0xFFFF, mem[ea], *(short *)(mem+ea));

    if (opcode == 00100) {
      fprintf(stderr," JMP\n");
      mem[P] = ea;
      continue;
    }

    if (opcode == 00200) {
      if ((keys & 050000) == 040000) {  /* R-mode and DP */
	fprintf(stderr," DLD\n");
	*(unsigned int *)(mem+A) = *(unsigned int *)(mem+ea);
      } else {
	fprintf(stderr," LDA\n");
	mem[A] = mem[ea];
      }
      continue;
    }

    if (opcode == 00400) {
      if ((keys & 050000) == 040000) {
	fprintf(stderr," DST\n");
	*(unsigned int *)(mem+ea) = *(unsigned int *)(mem+A);
      } else {
	fprintf(stderr," STA\n");
	mem[ea] = mem[A];
      }
      continue;
    }

    if (opcode == 00600) {
      utempa = mem[A];
      if ((keys & 050000) == 040000) {
	fprintf(stderr," DAD\n");
	mem[B] += mem[ea+1];
	if (mem[B] & 0x8000) {
	  mem[A]++;
	  mem[B] &= 0x7fff;
	}
	mem[A] += mem[ea];
	SETCC_L;
      } else {
	fprintf(stderr," ADD\n");
	*(short *)(mem+A) += *(short *)(mem+ea);
	SETCC_A;
      }
      SETC(((~utempa ^ mem[ea]) & (utempa ^ mem[A])) & 0x8000);
      continue;
    }

    if (opcode == 00700) {
      utempa = mem[A];
      if ((keys & 050000) == 040000) {
	fprintf(stderr," DSB\n");
	mem[B] -= mem[ea+1];
	if (mem[B] & 0x8000) {
	  mem[A]--;
	  mem[B] &= 0x7fff;
	}
	mem[A] -= mem[ea];
	SETCC_L;
      } else {
	fprintf(stderr," SUB\n");
	*(short *)(mem+A) -= *(short *)(mem+ea);
	SETCC_A;
      }
      SETC(((~utempa ^ mem[ea]) & (utempa ^ mem[A])) & 0x8000);
      continue;
    }

    if (opcode == 00300) {
      fprintf(stderr," ANA\n");
      mem[A] &= mem[ea];
      continue;
    }

    if (opcode == 00500) {
      fprintf(stderr," ERA\n");
      mem[A] ^= mem[ea];
      continue;
    }

    if (opcode == 00302) {
      fprintf(stderr," ORA\n");
      mem[A] |= mem[ea];
      continue;
    }

    if (opcode == 01000) {
      fprintf(stderr," JST\n");
      mem[ea] = mem[P];
      mem[P] = ea+1;
      continue;
    }

    if (opcode == 01100) {
      fprintf(stderr," CAS\n");
      utempa = mem[A];
      *(short *)(mem+A) -= *(short *)(mem+ea);
      SETCC_A;
      mem[A] = utempa;
compskip:
      if (keys & 0200)
	mem[P] += 2;
      else if (keys & 0100)
	mem[P]++;
      continue;
    }

    if (opcode == 01200) {
      fprintf(stderr," IRS\n");
      mem[ea]++;
      if (mem[ea] == 0)
	mem[P]++;
      continue;
    }

    if (opcode == 01300) {
      fprintf(stderr," IMA\n");
      tempa = mem[ea];
      mem[ea] = mem[A];
      mem[A] = tempa;
      continue;
    }

    if (opcode == 01400) {
      /* if V-mode, JSY, else (LDX in R-mode?) */
      fprintf(stderr," JSY/LDX\n");
      exit(1);
    }

    if (opcode == 01500) {
      fprintf(stderr," STX\n");
      mem[ea] = mem[X];
      continue;
    }

    if (opcode == 01600) {
      fprintf(stderr," MPY\n");
      templ = *(short *)(mem+A) * *(short *)(mem+ea);
      if (keys & 010000) {          /* V/I mode */
	*(int *)(mem+A) = templ;
      } else {                      /* R/S mode */
	mem[A] = (templ >> 15);
	mem[B] = templ & 077777;
      }
      continue;
    }

    if (opcode == 01700) {
      fprintf(stderr," DIV\n");
      if (keys & 010000) {          /* V/I mode */
	templ = *(int *)(mem+A);
      } else {                      /* R/S mode */
	templ = mem[A];           /* convert to 32-bit signed */
	templ = mem[B] | (templ<<15);
      }
      mem[A] = templ / *(short *)(mem+ea);
      mem[B] = templ % *(short *)(mem+ea);
      continue;
    }

    if (opcode == 03500) {
      fprintf(stderr," LDX\n");
      mem[X] = mem[ea];
      continue;
    }

    if (opcode == 00101) {
      if (keys & 010000) {          /* V/I mode */
	fprintf(stderr," EAL\n");
	exit(1);
	*(unsigned int *)(mem+A) = ea32;
      } else {
	fprintf(stderr," EAA\n");
	mem[A] = ea;
      }
      continue;
    }

    if (opcode == 00101) {
      fprintf(stderr," EAA\n");
      mem[A] = ea;
      continue;
    }

    /* NOTE: P300 u-code listings show CC set on Jxx instructions */

    if (opcode == 00203) {
      if (keys & 010000) {          /* V/I mode */
	fprintf(stderr," LDL\n");
	*(unsigned int *)(mem+A) = *(unsigned int *)(mem+ea);
      } else {
	fprintf(stderr," JEQ\n");
	if (*(short *)(mem+A) == 0)
	  mem[P] = ea;
      }
      continue;
    }

    if (opcode == 00703) {
      if (keys & 010000) {          /* V/I mode */
	fprintf(stderr," SBL\n");
	*(int *)(mem+A) -= *(int *)(mem+ea);
      } else {
	fprintf(stderr," JGE\n");
	if (*(short *)(mem+A) >= 0)
	  mem[P] = ea;
      }
      continue;
    }

    if (opcode == 01002) {
      if (keys & 010000) {          /* V/I mode */
	fprintf(stderr," PCL\n");
	exit(1);
      } else {
	fprintf(stderr," CREP\n");
	exit(1);
      }
      continue;
    }

    if (opcode == 00503) {
      if (keys & 010000) {          /* V/I mode */
	fprintf(stderr," ERL\n");
	*(int *)(mem+A) ^= *(int *)(mem+ea);
      } else {
	fprintf(stderr," JGT\n");
	if (*(short *)(mem+A) > 0)
	  mem[P] = ea;
      }
      continue;
    }

    if (opcode == 00403) {
      if (keys & 010000) {          /* V/I mode */
	fprintf(stderr," STL\n");
	*(int *)(mem+ea) = *(int *)(mem+A);
      } else {
	fprintf(stderr," JLE\n");
	if (*(short *)(mem+A) <= 0)
	  mem[P] = ea;
      }
      continue;
    }

    if (opcode == 00603) {
      if (keys & 010000) {          /* V/I mode */
	fprintf(stderr," ADL\n");
	*(int *)(mem+A) += *(int *)(mem+ea);
      } else {
	fprintf(stderr," JLT\n");
	if (*(short *)(mem+A) < 0)
	  mem[P] = ea;
      }
      continue;
    }

    if (opcode == 00303) {
      if (keys & 010000) {          /* V/I mode */
	fprintf(stderr," ANL\n");
	*(unsigned int *)(mem+A) &= *(unsigned int *)(mem+ea);
      } else {
	fprintf(stderr," JNE\n");
	if (*(short *)(mem+A) != 0)
	  mem[P] = ea;
      }
      continue;
    }

    /* NOTE: P300 u-code shows CC set for JIX/JDX */

    if (opcode == 01502) {
      if (keys & 010000) {          /* V/I mode */
	fprintf(stderr," DFLX\n");
	mem[X] = mem[ea] * 4;
      } else {
	fprintf(stderr," JDX\n");
	mem[X]--;
	if (mem[X] != 0)
	  mem[P] = ea;
      }
      continue;
    }

    if (opcode == 03502) {
      fprintf(stderr," STY\n");
      mem[ea] = mem[Y];
      continue;
    }

    if (opcode == 01503) {
      fprintf(stderr," JIX\n");
      mem[X]++;
      if (mem[X] != 0)
	mem[P] = ea;
      continue;
    }

    if (opcode == 01501) {
      fprintf(stderr," FLX\n");
      mem[X] = mem[ea] * 2;
      continue;
    }

    if (opcode == 03501) {
      fprintf(stderr," LDY\n");
      mem[Y] = mem[ea];
      continue;
    }

    if (opcode == 03503) {
      fprintf(stderr," JSX\n");
      mem[X] = mem[P];
      mem[P] = ea;
      continue;
    }

    if (opcode == 01103) {
      fprintf(stderr," CLS\n");
      utempl = *(unsigned int *)(mem+A);
      *(int *)(mem+A) -= *(int *)(mem+ea);
      SETCC_L;
      *(unsigned int *)(mem+A) = utempl;
      goto compskip;
    }

    if (opcode == 00601) {
      fprintf(stderr," FAD\n");
      fprintf(stderr," FEXP=%d (dec), FLTH='%o, FLTL='%o\n", mem[FEXP], mem[FLTH], mem[FLTL]);
      fprintf(stderr," ea EXP=%d (dec), ea H='%o, ea L='%o\n", (mem[ea+1] & 0xFF), mem[ea], (mem[ea+1] & 0xFF00));
      *(int *)&tempf = (mem[FLTH]<<16) | (mem[FLTL] & 0xFF00) | (mem[FEXP] & 0xFF);
      prieee4(&tempf);
      *(int *)&tempf1 = *(int *)(mem+ea);
      prieee4(&tempf1);
      tempf += tempf1;
      ieeepr4(&tempf);
      mem[FLTH] = (*(unsigned int *)&tempf) >> 16;
      mem[FLTL] = (*(unsigned int *)&tempf) & 0xFF00;
      mem[FEXP] = (*(unsigned int *)&tempf) & 0xFF;
      SETC(0);
      continue;
    }

    if (opcode == 01101) {
      fprintf(stderr," FCS\n");
      fprintf(stderr," FEXP=%d (dec), FLTH='%o, FLTL='%o\n", mem[FEXP], mem[FLTH], mem[FLTL]);
      fprintf(stderr," ea EXP=%d (dec), ea H='%o, ea L='%o\n", (mem[ea+1] & 0xFF), mem[ea], (mem[ea+1] & 0xFF00));
      *(int *)&tempf = (mem[FLTH]<<16) | (mem[FLTL] & 0xFF00) | (mem[FEXP] & 0xFF);
      prieee4(&tempf);
      *(int *)&tempf1 = *(int *)(mem+ea);
      prieee4(&tempf1);
      tempf -= tempf1;
      keys &= ~0300;
      if (tempf < 0.0)
	keys |= 0200;
      else if (tempf == 0.0)
	keys |= 0100;
      goto compskip;
    }

    if (opcode == 01701) {
      fprintf(stderr," FDV\n");
      fprintf(stderr," FEXP=%d (dec), FLTH='%o, FLTL='%o\n", mem[FEXP], mem[FLTH], mem[FLTL]);
      fprintf(stderr," ea EXP=%d (dec), ea H='%o, ea L='%o\n", (mem[ea+1] & 0xFF), mem[ea], (mem[ea+1] & 0xFF00));
      *(int *)&tempf = (mem[FLTH]<<16) | (mem[FLTL] & 0xFF00) | (mem[FEXP] & 0xFF);
      prieee4(&tempf);
      *(int *)&tempf1 = *(int *)(mem+ea);
      prieee4(&tempf1);
      tempf /= tempf1;
      ieeepr4(&tempf);
      mem[FLTH] = (*(unsigned int *)&tempf) >> 16;
      mem[FLTL] = (*(unsigned int *)&tempf) & 0xFF00;
      mem[FEXP] = (*(unsigned int *)&tempf) & 0xFF;
      SETC(0);
      continue;
    }

    if (opcode == 0201) {
      fprintf(stderr," FLD\n");
      mem[FLTH] = mem[ea];
      mem[FLTL] = mem[ea+1] & 0xFF00;
      mem[FEXP] = mem[ea+1] & 0xFF;
      continue;
    }

    if (opcode == 01601) {
      fprintf(stderr," FMP\n");
      fprintf(stderr," FEXP=%d (dec), FLTH='%o, FLTL='%o\n", mem[FEXP], mem[FLTH], mem[FLTL]);
      fprintf(stderr," ea EXP=%d (dec), ea H='%o, ea L='%o\n", (mem[ea+1] & 0xFF), mem[ea], (mem[ea+1] & 0xFF00));
      *(int *)&tempf = (mem[FLTH]<<16) | (mem[FLTL] & 0xFF00) | (mem[FEXP] & 0xFF);
      prieee4(&tempf);
      *(int *)&tempf1 = *(int *)(mem+ea);
      prieee4(&tempf1);
      tempf *= tempf1;
      ieeepr4(&tempf);
      mem[FLTH] = (*(unsigned int *)&tempf) >> 16;
      mem[FLTL] = (*(unsigned int *)&tempf) & 0xFF00;
      mem[FEXP] = (*(unsigned int *)&tempf) & 0xFF;
      SETC(0);
      continue;
    }

    if (opcode == 00701) {
      fprintf(stderr," FSB\n");
      fprintf(stderr," FEXP=%d (dec), FLTH='%o, FLTL='%o\n", mem[FEXP], mem[FLTH], mem[FLTL]);
      fprintf(stderr," ea EXP=%d (dec), ea H='%o, ea L='%o\n", (mem[ea+1] & 0xFF), mem[ea], (mem[ea+1] & 0xFF00));
      *(int *)&tempf = (mem[FLTH]<<16) | (mem[FLTL] & 0xFF00) | (mem[FEXP] & 0xFF);
      prieee4(&tempf);
      *(int *)&tempf1 = *(int *)(mem+ea);
      prieee4(&tempf1);
      tempf -= tempf1;
      ieeepr4(&tempf);
      mem[FLTH] = (*(unsigned int *)&tempf) >> 16;
      mem[FLTL] = (*(unsigned int *)&tempf) & 0xFF00;
      mem[FEXP] = (*(unsigned int *)&tempf) & 0xFF;
      SETC(0);
      continue;
    }

    if (opcode == 0401) {
      fprintf(stderr," FST\n");
      if (mem[FEXP] & 0xFF00)
	cpuexception('f');
      mem[ea] = mem[FLTH];
      mem[ea+1] = (mem[FLTL] & 0xFF00) | mem[FEXP];
      continue;
    }

    if (opcode == 0602) {
      fprintf(stderr," DFAD\n");
      fprintf(stderr," FEXP=%d (dec), FLTH='%o, FLTL='%o\n", mem[FEXP], mem[FLTH], mem[FLTL]);
      fprintf(stderr," ea EXP=%d (dec), ea H='%o, ea L='%o\n", (mem[ea+1] & 0xFF), mem[ea], (mem[ea+1] & 0xFF00));
      tempda[0] = mem[FLTH];
      tempda[1] = mem[FLTL];
      tempda[2] = mem[FLTD];
      tempda[3] = mem[FEXP];
      prieee8(tempda);
      tempd = *(double *)(mem+ea);
      prieee8(&tempd);
      *(double *)tempda += tempd;
      ieeepr8(tempda);
      mem[FLTH] = tempda[0];
      mem[FLTL] = tempda[1];
      mem[FLTD] = tempda[2];
      mem[FEXP] = tempda[3];
      SETC(0);
      continue;
    }

    if (opcode == 01102) {
      fprintf(stderr," DFCS\n");
      fprintf(stderr," FEXP=%d (dec), FLTH='%o, FLTL='%o\n", mem[FEXP], mem[FLTH], mem[FLTL]);
      fprintf(stderr," ea EXP=%d (dec), ea H='%o, ea L='%o\n", (mem[ea+1] & 0xFF), mem[ea], (mem[ea+1] & 0xFF00));
      tempda[0] = mem[FLTH];
      tempda[1] = mem[FLTL];
      tempda[2] = mem[FLTD];
      tempda[3] = mem[FEXP];
      prieee8(tempda);
      tempd = *(double *)(mem+ea);
      prieee8(&tempd);
      *(double *)tempda -= tempd;
      keys &= ~0300;
      if (*(double *)tempda < 0.0)
	keys |= 0200;
      else if (*(double *)tempda == 0.0)
	keys |= 0100;
      goto compskip;
    }

    if (opcode == 01702) {
      fprintf(stderr," DFDV\n");
      fprintf(stderr," FEXP=%d (dec), FLTH='%o, FLTL='%o\n", mem[FEXP], mem[FLTH], mem[FLTL]);
      fprintf(stderr," ea EXP=%d (dec), ea H='%o, ea L='%o\n", (mem[ea+1] & 0xFF), mem[ea], (mem[ea+1] & 0xFF00));
      tempda[0] = mem[FLTH];
      tempda[1] = mem[FLTL];
      tempda[2] = mem[FLTD];
      tempda[3] = mem[FEXP];
      prieee8(tempda);
      tempd = *(double *)(mem+ea);
      prieee8(&tempd);
      *(double *)tempda /= tempd;
      ieeepr8(tempda);
      mem[FLTH] = tempda[0];
      mem[FLTL] = tempda[1];
      mem[FLTD] = tempda[2];
      mem[FEXP] = tempda[3];
      SETC(0);
      continue;
    }

    if (opcode == 0202) {
      fprintf(stderr," DFLD\n");
      *(double *)tempda = *(double *)(mem+ea);
      mem[FLTH] = tempda[0];
      mem[FLTL] = tempda[1];
      mem[FLTD] = tempda[2];
      mem[FEXP] = tempda[3];
      continue;
    }

    if (opcode == 01602) {
      fprintf(stderr," DFMP\n");
      fprintf(stderr," FEXP=%d (dec), FLTH='%o, FLTL='%o\n", mem[FEXP], mem[FLTH], mem[FLTL]);
      fprintf(stderr," ea EXP=%d (dec), ea H='%o, ea L='%o\n", (mem[ea+1] & 0xFF), mem[ea], (mem[ea+1] & 0xFF00));
      tempda[0] = mem[FLTH];
      tempda[1] = mem[FLTL];
      tempda[2] = mem[FLTD];
      tempda[3] = mem[FEXP];
      prieee8(tempda);
      tempd = *(double *)(mem+ea);
      prieee8(&tempd);
      *(double *)tempda *= tempd;
      ieeepr8(tempda);
      mem[FLTH] = tempda[0];
      mem[FLTL] = tempda[1];
      mem[FLTD] = tempda[2];
      mem[FEXP] = tempda[3];
      SETC(0);
      continue;
    }

    if (opcode == 0702) {
      fprintf(stderr," DFSB\n");
      fprintf(stderr," FEXP=%d (dec), FLTH='%o, FLTL='%o\n", mem[FEXP], mem[FLTH], mem[FLTL]);
      fprintf(stderr," ea EXP=%d (dec), ea H='%o, ea L='%o\n", (mem[ea+1] & 0xFF), mem[ea], (mem[ea+1] & 0xFF00));
      tempda[0] = mem[FLTH];
      tempda[1] = mem[FLTL];
      tempda[2] = mem[FLTD];
      tempda[3] = mem[FEXP];
      prieee8(tempda);
      tempd = *(double *)(mem+ea);
      prieee8(&tempd);
      *(double *)tempda -= tempd;
      ieeepr8(tempda);
      mem[FLTH] = tempda[0];
      mem[FLTL] = tempda[1];
      mem[FLTD] = tempda[2];
      mem[FEXP] = tempda[3];
      SETC(0);
      continue;
    }

    if (opcode == 0402) {
      fprintf(stderr," DFST\n");
      mem[ea] = mem[FLTH];
      mem[ea+1] = mem[FLTL];
      mem[ea+2] = mem[FLTD];
      mem[ea+3] = mem[FEXP];
      continue;
    }

    if (mem[066] != 0) {
      fprintf(stderr," JST* '66 [%o]\n", mem[066]);
      mem[mem[066]] = mem[P];
      mem[P] = mem[066]+1;
      continue;
    }
    fprintf(stderr," UNKNOWN OPCODE: %o\n", opcode);
    exit(1);
  }
}
    
  
/* Handle SVC instruction.  For real hardware emulation on an R-mode
   such as the P300, SVC would interrupt (JST*) through location '75
   (in vectored mode) or would fault on the P400.  

   Since we're running programs without an OS underneath us, handle
   the SVC's here.

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

  /* get svc code word, break into class and function */

  code = mem[mem[P]];
  class = (code >> 6) & 077;
  if (class == 0)
    class = 1;
  func = code & 077;

  /* determine argument list location and create arg list vector */

  if (code & 0100000)
    argl = mem[mem[P]-2];
  else if (code & 040000)
    argl = mem[P]+2;
  else
    argl = mem[P]+1;

  fprintf(stderr," code=%o, class=%o, func=%o, argl=%o\n", code, class, func, argl);
  if (class <= MAXCLASS && func <= MAXFUNC)
    fprintf(stderr," name=%s, #args=%d, LOC args=%o\n", svcinfo[class][func].name, svcinfo[class][func].numargs, svcinfo[class][func].locargs);
  else
    goto badsvc;

  /* if location '65 is set, do indirect JST to handle svc */

  if (mem[065] != 0) {
    fprintf(stderr," JST* '65 [%o]\n", mem[065]);
    mem[mem[065]] = mem[P];
    mem[P] = mem[065]+1;
    return;
  }

  if (svcinfo[class][func].numargs == 99)
    goto badsvc;

  if (svcinfo[class][func].locargs != 0) {
    fprintf(stderr," Can't handle LOC() args\n");
    exit(1);
  }

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

  fprintf(stderr," return=%o, actargs=%d\n", argl, actargs);

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

  fprintf(stderr," returning from SVC to %o\n", argl);
  mem[P] = argl;
  return;


unimp:

  fprintf(stderr," svc not implemented\n", class, func);
  exit(1);

  /* here on a bad svc; if the bounce bit (bit 2) is set in the code word,
     jump to the location following the code word (which is typically a
     JMP instruction).  If the bounce bit isn't set, we have to halt */

badsvc:

  if (code & 040000) {
    mem[P]++;
    fprintf(stderr," bouncing svc error to address %o\n", mem[P]);
    return;
  }
  
  fprintf(stderr," halting on bad svc, class=%o, func=%o\n", class, func);
  exit(1);
}



/* here for PIO instructions: OCP, SKS, INA, OTA.  The instruction
   word is passed in as an argument to handle EIO (Execute I/O) in
   V-mode.
*/

pio(unsigned int inst) {
  short class;
  short func;
  short device;

  class = inst >> 14;
  func = (inst >> 6) & 017;
  device = inst & 077;
  fprintf(stderr," pio, class=%d, func='%o, device='%o\n", class, func, device);
  if (devmap[device])
    devmap[device](class, func, device);
  else {
    fprintf(stderr," no handler for device\n");
    exit(1);
  }
}
