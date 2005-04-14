/* Pr1me Computer emulator, Jim Wilcoxson (jim@meritnet.com), April 4, 2005
   Copyright (C) 2005, Jim Wilcoxson (jim@meritnet.com).  All Rights Reserved.

   Restores a Prime R-mode .save image from stdin to memory and
   emulates execution.  This version supports 16S, 32S, 32R, and 64R,
   though only 1 32R program has been tested.

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

   - only 1 32R program has been executed, and it may have issues;
   no other CPU modes have been tested at all

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
#define FLTH 4
#define FLTL 5
#define FEXP 6
#define P 7
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

unsigned short mem[64*1024];         /* 64K words of main memory */
unsigned short prevpc,keys;          /* program counter, prev pc, keys */
unsigned short amask;                /* address mask */
int verbose;
int memdump;                         /* -memdump arg */

/* I/O device map table, containing function pointers to handle device I/O */

#include "emdev.h"

void (*devmap[64])(unsigned short, unsigned short) = {
  0,0,0,0,devasr,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
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
    break;
  case 6:                     /* 64V */
    fprintf(stderr,"Entering 64V mode, keys=%o\n", keys);
    amask = 0177777;
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
    fprintf(stderr," Indirect, new i=%d, new ea=%o, [ea]=%o\n", i!=0, ea, mem[ea]);
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
      fprintf(stderr," Indirect, new i=%d, new ea=%o, [ea]=%o\n", i!=0, ea, mem[ea]);
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
      fprintf(stderr," Indirect, new i=%d, new ea=%o, [ea]=%o\n", i!=0, ea, mem[ea]);
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

unsigned short ea64v (unsigned short inst, short i, short x, short *opcode) {
  fprintf(stderr,"Mode 64V not implemented\n");
}

unsigned short ea32i (unsigned short inst, short i, short x) {
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
    if (keys & 40) {
      printf(" Decimal exception fault\n");
      exit(1);
    } else {
      keys |= 0x8000;
    }
    break;
  default:
    printf(" Unrecognized exception type '%c'\n", extype);
  }
}




main (int argc, char **argv) {

  signed short tempa;
  unsigned short utempa;
  signed int templ;
  unsigned int utempl;
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


  verbose = 0;
  memdump = 0;

  /* check args */

  for (i=1; i<argc; i++) {
    if (strcmp(argv[i],"-vv") == 0)
      verbose = 2;
    else if (strcmp(argv[i],"-v") == 0)
      verbose = 1;
    else if (strcmp(argv[i],"-memdump") == 0)
      memdump = 1;
    else if (argv[i][0] == '-')
      fprintf(stderr,"Unrecognized argument: %s\n", argv[i]);
  }

  /* read 9-word rvec header */

  for (i=0; i<9; i++)
    rvec[i] = readshort();
  fprintf(stderr,"SA=%o, EA=%o, P=%o, A=%o, B=%o, X=%o, K=%o\n\n", rvec[0], rvec[1],
         rvec[2], rvec[3], rvec[4], rvec[5], rvec[6]);
  if (rvec[2] > rvec[1]) {
    fprintf(stderr,"Program start > EA: runfile is trashed\n");
    exit(1);
  }

  /* read memory image from SA to EA inclusive */

  nw = rvec[1]-rvec[0]+1;
  if (fread(mem+rvec[0], sizeof(short), nw, stdin) != nw) {
    perror("Error reading memory image");
    exit(1);
  }

  /* setup execution (registers, keys, address mask, etc.) from rvec */

  mem[A] = rvec[3];
  mem[B] = rvec[4];
  mem[X] = rvec[5];
  newkeys(rvec[6]);
  mem[P] = rvec[2];

  if (memdump) {

    /* dump sector zero for debugging */

    fprintf(stderr,"\nSector 0:\n");
    for (i=0; i<01000; i=i+8)
      if (mem[i]|mem[i+1]|mem[i+2]|mem[i+3]|mem[i+4]|mem[i+5]|mem[i+6]|mem[i+7])
	fprintf(stderr,"%3o: %6o %6o %6o %6o %6o %6o %6o %6o\n", i, mem[i], mem[i+1], mem[i+2], mem[i+3], mem[i+4], mem[i+5], mem[i+6], mem[i+7]);
    
    /* dump main memory for debugging */

    fprintf(stderr,"\nMain memory:\n");
    for (i=rvec[2]; i<rvec[1]; i=i+8)
      if (mem[i]|mem[i+1]|mem[i+2]|mem[i+3]|mem[i+4]|mem[i+5]|mem[i+6]|mem[i+7])
	fprintf(stderr,"%o: %6o %6o %6o %6o %6o %6o %6o %6o\n", i, mem[i], mem[i+1], mem[i+2], mem[i+3], mem[i+4], mem[i+5], mem[i+6], mem[i+7]);
  }

  /* main instruction decode loop */

  while (1) {
    
    if (mem[P] > rvec[1]) {       /* hack for testing */
      fprintf(stderr,"\nOOPS! Program counter %o > EA %o\n", mem[P], rvec[1]);
      exit(1);
    }

    prevpc = mem[P];
    inst = mem[mem[P]];
    instcount++;
    fprintf(stderr,"\n%o: %o		A='%o/%:0d, B='%o/%d, X=%o/%d	instcount=%d\n", mem[P], inst, mem[A], *(short *)(mem+A), mem[B], *(short *)(mem+B), mem[X], *(short *)(mem+X), instcount);
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

	if (inst == 000000) {                 /* HLT :( */
	  fprintf(stderr," HLT\n");
	  fprintf(stderr,"\nProgram halt at %o\n", prevpc);
	  exit(1);
	}

	if (inst == 000201) {                /* IAB */
	  fprintf(stderr," IAB\n");
	  tempa = mem[B];
	  mem[B] = mem[A];
	  mem[A] = tempa;
	  continue;
	}

	if (inst == 000205) {                /* PIM (R-mode) */
	  fprintf(stderr," PIM\n");
	  mem[A] = mem[B] | (mem[A] << 15);
	  continue;
	}

	if (inst == 000211) {                /* PID (R-mode) */
	  fprintf(stderr," PID\n");
	  mem[B] = mem[A] & 0x7fff;
	  if (mem[A] & 0x8000)
	    mem[A] = -1;
	  else
	    mem[A] = 0;
	  continue;
	}

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
	  templ = *(short *)(mem+A);
	  *(int *)(mem+A) = templ;
	  continue;
	}

	if (inst == 000015) {
	  fprintf(stderr," PIMA\n");
	  if (mem[A] == 0 | mem[A] == 0177777) {
	    keys &= 077777;
	    mem[A] = mem[B];
	  } else {
	    cpuexception('i');
	  }
	  continue;
	}

	if (inst == 000041) {
	  fprintf(stderr," SCA\n");
	  mem[A] = keys & 0377;
	  continue;
	}

	if (inst == 000043 || inst == 001005) {
	  fprintf(stderr," INK/TKA\n");
	  mem[A] = keys;
	  continue;
	}

	if (inst == 000405 || inst == 001015) {
	  fprintf(stderr," OTK/TAK\n");
	  newkeys(mem[A]);
	  continue;
	}

	if (inst == 000001) {
	  fprintf(stderr," NOP\n");
	  continue;
	}

	fprintf(stderr," unrecognized generic class 0 instruction!\n");
	exit(1);
	
      }

      if (class == 3) {
	fprintf(stderr," generic class 3\n");

	if (inst == 0141604) {
	  fprintf(stderr," BCLT\n");
	  if ((keys & 0300) == 0200)
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0141600) {
	  fprintf(stderr," BCLE\n");
	  if (keys & 0300)
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0141602) {
	  fprintf(stderr," BCEQ\n");
	  if (keys & 0100)
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0141603) {
	  fprintf(stderr," BCNE\n");
	  if (!(keys & 0100))
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0141605) {
	  fprintf(stderr," BCGE\n");
	  if (!(keys & 0200) || (keys & 0100))
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0141601) {
	  fprintf(stderr," BCGT\n");
	  if (!(keys & 0300))
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0141705) {
	  fprintf(stderr," BCR\n");
	  if (!(keys & 0100000))
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0141704) {
	  fprintf(stderr," BCS\n");
	  if (keys & 0100000)
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0141707) {
	  fprintf(stderr," BLR\n");
	  if (!(keys & 020000))
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0141706) {
	  fprintf(stderr," BLS\n");
	  if (keys & 020000)
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0140614) {
	  fprintf(stderr," BLT\n");
	  if (*(short *)(mem+A) < 0)
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0140610) {
	  fprintf(stderr," BLE\n");
	  if (*(short *)(mem+A) <= 0)
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0140612) {
	  fprintf(stderr," BEQ\n");
	  if (*(short *)(mem+A) == 0)
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0140613) {
	  fprintf(stderr," BNE\n");
	  if (*(short *)(mem+A) != 0)
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0140615) {
	  fprintf(stderr," BGE\n");
	  if (*(short *)(mem+A) >= 0)
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0140611) {
	  fprintf(stderr," BGT\n");
	  if (*(short *)(mem+A) > 0)
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0140700) {
	  fprintf(stderr," BLLE\n");
	  if (*(int *)(mem+A) <= 0)
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0140702) {
	  fprintf(stderr," BLEQ\n");
	  if (*(int *)(mem+A) == 0)
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0140703) {
	  fprintf(stderr," BLNE\n");
	  if (*(int *)(mem+A) != 0)
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0140701) {
	  fprintf(stderr," BLGT\n");
	  if (*(int *)(mem+A) > 0)
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0141614) {
	  fprintf(stderr," BFLT\n");
	  if (*(short *)(mem+FLTH) < 0)
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0141610) {
	  fprintf(stderr," BFLE\n");
	  if (*(int *)(mem+FLTH) < 0 || (*(int *)(mem+FLTH) == 0 && *(short *)(mem+FEXP) == 0))
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0141612) {
	  fprintf(stderr," BFEQ\n");
	  if (*(int *)(mem+FLTH) == 0 && *(short *)(mem+FEXP) == 0)
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0141613) {
	  fprintf(stderr," BFNE\n");
	  if (*(int *)(mem+FLTH) != 0 || *(short *)(mem+FEXP) != 0)
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0141615) {
	  fprintf(stderr," BFGE\n");
	  if (*(int *)(mem+FLTH) >= 0)
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0141611) {
	  fprintf(stderr," BFGT\n");
	  if (*(int *)(mem+FLTH) >= 0 && (*(int *)(mem+FLTH) != 0 || *(short *)(mem+FEXP) != 0))
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0141334) {
	  fprintf(stderr," BIX\n");
	  mem[X]++;
	  if (mem[X] == 0)
	    mem[P] = mem[mem[P]];
	  continue;
	}

#if 0
	if (inst == 0141324) {
	  fprintf(stderr," BIY\n");
	  mem[Y]++;
	  if (mem[Y] == 0)
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0140724) {
	  fprintf(stderr," BDY\n");
	  mem[Y]--;
	  if (mem[Y] == 0)
	    mem[P] = mem[mem[P]];
	  continue;
	}
#endif

	if (inst == 0140734) {
	  fprintf(stderr," BDX\n");
	  mem[X]--;
	  if (mem[X] == 0)
	    mem[P] = mem[mem[P]];
	  continue;
	}

	if (inst == 0141206) {                /* A1A */
	  fprintf(stderr," A1A\n");
	  mem[A]++;
	  continue;
	}

	if (inst == 0140304) {                /* A2A */
	  fprintf(stderr," A2A\n");
	  mem[A] += 2;
	  continue;
	}

	if (inst == 0141216) {                /* ACA */
	  fprintf(stderr," ACA\n");
	  if (keys & 0100000)
	    mem[A]++;
	  continue;
	}

	if (inst == 0140110) {                /* S1A */
	  fprintf(stderr," S1A\n");
	  mem[A]--;
	  continue;
	}

	if (inst == 0140310) {                /* S2A */
	  fprintf(stderr," S2A\n");
	  mem[A] -= 2;
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

	if (inst == 0140015) {                /* CRB */
	  fprintf(stderr," CRB\n");
	  mem[B] = 0;
	  continue;
	}

	if (inst == 0140010) {                /* CRL */
	  fprintf(stderr," CRL\n");
	  mem[A] = 0; mem[B] = 0;
	  continue;
	}

	if (inst == 0140214) {                /* CAZ */
	  fprintf(stderr," CAZ\n");
	  if (mem[A] == 0)
	    mem[P]++;
	  else if (*(short *)(mem+A) < 0)
	    mem[P] += 2;
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
	  continue;
	}

	if (inst == 0140416) {                /* LF */
	  fprintf(stderr," LF\n");
	  mem[A] = 0;
	  continue;
	}

	if (inst == 0140410) {                /* LLT */
	  fprintf(stderr," LLT\n");
	  if (*(short *)(mem+A) < 0)
	    mem[A] = 1;
	  else
	    mem[A] = 0;
	  continue;
	}

	if (inst == 0140411) {                /* LLE */
	  fprintf(stderr," LLE\n");
	  if (*(short *)(mem+A) <= 0)
	    mem[A] = 1;
	  else
	    mem[A] = 0;
	  continue;
	}

	if (inst == 0140412) {                /* LNE */
	  fprintf(stderr," LNE\n");
	  if (*(short *)(mem+A) != 0)
	    mem[A] = 1;
	  else
	    mem[A] = 0;
	  continue;
	}

	if (inst == 0140413) {                /* LEQ */
	  fprintf(stderr," LEQ\n");
	  if (*(short *)(mem+A) == 0)
	    mem[A] = 1;
	  else
	    mem[A] = 0;
	  continue;
	}

	if (inst == 0140414) {                /* LGE */
	  fprintf(stderr," LGE\n");
	  if (*(short *)(mem+A) >= 0)
	    mem[A] = 1;
	  else
	    mem[A] = 0;
	  continue;
	}

	if (inst == 0140411) {                /* LGT */
	  fprintf(stderr," LGT\n");
	  if (*(short *)(mem+A) > 0)
	    mem[A] = 1;
	  else
	    mem[A] = 0;
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

	/* TAY */

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

#if 0
	if (inst == 0141124) {
	  fprintf(stderr," TYA\n");
	  mem[A] = mem[Y];
	  continue;
	}
#endif

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
	  continue;
	}

	if (inst == 0141210) {
	  fprintf(stderr," TCL\n");
	  *(int *)(mem+A) = - (*(int *)(mem+A));
	  if (*(unsigned int *)(mem+A) == 0x80000000)
	    cpuexception('i');
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
	  mem[A] = ((keys & 0300) == 0200);
	  continue;
	}

	if (inst == 0141501) {
	  fprintf(stderr," LCLE\n");
	  mem[A] = (keys & 0300);
	  continue;
	}

	if (inst == 0141503) {
	  fprintf(stderr," LCEQ\n");
	  mem[A] = (keys & 0100);
	  continue;
	}

	if (inst == 0141502) {
	  fprintf(stderr," LCNE\n");
	  mem[A] = !(keys & 0100);
	  continue;
	}

	if (inst == 0141504) {
	  fprintf(stderr," LCGE\n");
	  mem[A] = !(keys & 0200) || (keys & 0100);
	  continue;
	}

	if (inst == 0141505) {
	  fprintf(stderr," LCGT\n");
	  mem[A] =!(keys & 0300);
	  continue;
	}

	if (inst == 0140511) {
	  fprintf(stderr," LLLE\n");
	  mem[A] = *(int *)(mem+A) <= 0;
	  continue;
	}

	if (inst == 0141513) {
	  fprintf(stderr," LLEQ\n");
	  mem[A] = *(int *)(mem+A) == 0;
	  continue;
	}

	if (inst == 0141512) {
	  fprintf(stderr," LLNE\n");
	  mem[A] = *(int *)(mem+A) != 0;
	  continue;
	}

	if (inst == 0141515) {
	  fprintf(stderr," LLGT\n");
	  mem[A] = *(int *)(mem+A) > 0;
	  continue;
	}

	if (inst == 0141110) {
	  fprintf(stderr," LFLT\n");
	  mem[A] = *(short *)(mem+FLTH) < 0;
	  continue;
	}

	if (inst == 0141111) {
	  fprintf(stderr," LFLE\n");
	  mem[A] = *(int *)(mem+FLTH) < 0 || (*(int *)(mem+FLTH) == 0 && *(short *)(mem+FEXP) == 0);
	  continue;
	}

	if (inst == 0141113) {
	  fprintf(stderr," LFEQ\n");
	  mem[A] = *(int *)(mem+FLTH) == 0 && *(short *)(mem+FEXP) == 0;
	  continue;
	}

	if (inst == 0141112) {
	  fprintf(stderr," LFNE\n");
	  mem[A] = *(int *)(mem+FLTH) != 0 || *(short *)(mem+FEXP) != 0;
	  continue;
	}

	if (inst == 0141114) {
	  fprintf(stderr," LFGE\n");
	  mem[A] = *(int *)(mem+FLTH) >= 0;
	  continue;
	}

	if (inst == 0141115) {
	  fprintf(stderr," LFGT\n");
	  mem[A] = *(int *)(mem+FLTH) >= 0 && (*(int *)(mem+FLTH) != 0 || *(short *)(mem+FEXP) != 0);
	  continue;
	}

	if (inst == 0140000) {
	  fprintf(stderr," ADLL\n");
	  if (keys & 020000)
	    (*(int *)(mem+A))++;
	  continue;
	}

	fprintf(stderr," unrecognized generic class 3 instruction!\n");
	exit(1);
	
      }


      if (class == 1) {
	fprintf(stderr," shift group\n");
	scount = -inst & 077;
	switch (inst & 01700) {

	case 00000: /* LRL */
	  fprintf(stderr," LRL %d\n", scount);
	  utempl = *(unsigned int *)(mem+A);
	  utempl = utempl >> scount;
	  *(int *)(mem+A) = utempl;
	  break;

	case 00100: /* LRS (s.b. different in R/V modes) */
	  fprintf(stderr," LRS %d\n", scount);
	  templ = *(int *)(mem+A);
	  templ = templ >> scount;
	  *(int *)(mem+A) = templ;
	  break;

	case 00200: /* LRR */
	  fprintf(stderr," LRR %d\n", scount);
	  scount = scount & 037;
	  utempl = *(unsigned int *)(mem+A);
	  utempl = (utempl >> scount) | (utempl << (32-scount));
	  *(unsigned int *)(mem+A) = utempl;
	  break;

	case 00400: /* ARL */
	  fprintf(stderr," ARL %d\n", scount);
	  utempa = mem[A];
	  utempa = utempa >> scount;
	  mem[A] = utempa;
	  break;

	case 00500: /* ARS */
	  fprintf(stderr," ARS %d\n", scount);
	  tempa = *(short *)(mem+A);
	  tempa = tempa >> scount;
	  *(short *)(mem+A) = tempa;
	  break;

	case 00600: /* ARR */
	  fprintf(stderr," ARR %d\n", scount);
	  scount = scount & 017;
	  mem[A] = (mem[A] >> scount) | (mem[A] << (16-scount));
	  break;

	case 01000: /* LLL */
	  fprintf(stderr," LLL %d\n", scount);
	  utempl = *(unsigned int *)(mem+A);
	  utempl = utempl << scount;
	  *(unsigned int *)(mem+A) = utempl;
	  break;

	case 01100: /* LLS (s.b. different in R/V modes) */
	  fprintf(stderr," LLS %d\n", scount);
	  templ = *(int *)(mem+A);
	  templ = templ << scount;
	  *(int *)(mem+A) = templ;
	  break;

	case 01200: /* LLR */
	  fprintf(stderr," LLR %d\n", scount);
	  scount = scount & 037;
	  utempl = *(unsigned int *)(mem+A);
	  utempl = (utempl << scount) | (utempl >> (32-scount));
	  *(unsigned int *)(mem+A) = utempl;
	  break;

	case 01400: /* ALL */
	case 01500: /* ALS */
	  fprintf(stderr," ALL/ALS %d\n", scount);
	  utempa = *(short *)(mem+A);
	  utempa = utempa << scount;
	  *(short *)(mem+A) = utempa;
	  break;

	case 01600: /* ALR */
	  fprintf(stderr," ALR %d\n", scount);
	  scount = scount & 017;
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
	  fprintf(stderr," SMI\n");
	  if (*(short *)(mem+A) < 0)
	    mem[P]++;
	  continue;
	}

	if (inst == 0100400) {
	  fprintf(stderr," SPL\n");
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
	  fprintf(stderr," SNZ\n");
	  if (mem[A] != 0)
	    mem[P]++;
	  continue;
	}

	if (inst == 0100040) {
	  fprintf(stderr," SZE\n");
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

	if (inst == 0101270) {       /* weird combo skip for PSD */
	  fprintf(stderr," weird skip\n");
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

    fprintf(stderr," SR-mode MR decode\n");

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
         x=1, opcode='15 02 -> sty (V) (aka '35 02)
	 x=1, opcode='15 02 -> jdx (R) (aka '35 02)

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

    fprintf(stderr," ea=%o, [ea]='%o/%d\n", ea, mem[ea], *(short *)(mem+ea));

    if (opcode == 00100) {
      fprintf(stderr," JMP\n");
      mem[P] = ea;
      continue;
    }

    if (opcode == 00200) {
      if (keys & 040000) {
	fprintf(stderr," DLD\n");
	*(unsigned int *)(mem+A) = *(unsigned int *)(mem+ea);
      } else {
	fprintf(stderr," LDA\n");
	mem[A] = mem[ea];
      }
      continue;
    }

    if (opcode == 00203) {
      fprintf(stderr," LDL\n");
      *(unsigned int *)(mem+A) = *(unsigned int *)(mem+ea);
      continue;
    }

    if (opcode == 00300) {
      fprintf(stderr," ANA\n");
      mem[A] &= mem[ea];
      continue;
    }

    if (opcode == 00303) {
      fprintf(stderr," ANL\n");
      *(unsigned int *)(mem+A) &= *(unsigned int *)(mem+ea);
      continue;
    }

    if (opcode == 00400) {
      if (keys & 040000) {
	fprintf(stderr," DST\n");
	mem[ea] = mem[A];
	mem[ea+1] = mem[B];
      } else {
	fprintf(stderr," STA\n");
	mem[ea] = mem[A];
      }
      continue;
    }

    if (opcode == 00403) {
      fprintf(stderr," STL\n");
      *(int *)(mem+ea) = *(int *)(mem+A);
      continue;
    }

    if (opcode == 00500) {
      fprintf(stderr," ERA\n");
      mem[A] ^= mem[ea];
      continue;
    }

    if (opcode == 00503) {
      fprintf(stderr," ERL\n");
      *(int *)(mem+A) ^= *(int *)(mem+ea);
      continue;
    }

    if (opcode == 00302) {
      fprintf(stderr," ORA\n");
      *(short *)(mem+A) |= *(short *)(mem+ea);
      continue;
    }

    if (opcode == 00600) {
      fprintf(stderr," ADD\n");
      *(short *)(mem+A) += *(short *)(mem+ea);
      continue;
    }

    if (opcode == 00603) {
      fprintf(stderr," ADL\n");
      *(int *)(mem+A) += *(int *)(mem+ea);
      continue;
    }

    if (opcode == 00700) {
      fprintf(stderr," SUB\n");
      *(short *)(mem+A) -= *(short *)(mem+ea);
      continue;
    }

    if (opcode == 00703) {
      fprintf(stderr," SBL\n");
      *(int *)(mem+A) -= *(int *)(mem+ea);
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
      if (mem[A] == mem[ea])
	mem[P]++;
      else if (*(short*)(mem+A) < *(short *)(mem+ea))
	mem[P] += 2;
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
      /* if V-mode, JSY, else LDX in R-mode */
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
	if (mem[A] != 0 && mem[A] != 0xffff) {
	  fprintf(stderr," Register A should be 0 or -1 before R-mode DIV\n");
	  exit(1);
	}
	if (mem[A]) {               /* dividend was negative before PID */
	  tempa = mem[B] | 0x8000;  /* make 16-bit negative again */
	  templ = tempa;            /* extend to 32-bit negative */
	} else 
	  templ = *(short *)(mem+B);/* make 32-bit positive */
      }
      mem[A] = templ / *(short *)(mem+ea);
      mem[B] = templ % *(short *)(mem+ea);
      continue;
    }

    if (opcode == 03500) {
      fprintf(stderr," LDX instruction, new value=%o\n", mem[ea]);
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

    if (opcode == 00203) {
      fprintf(stderr," JEQ\n");
      if (*(short *)(mem+A) == 0)
	mem[P] = ea;
      continue;
    }

    if (opcode == 00703) {
      fprintf(stderr," JGE\n");
      if (*(short *)(mem+A) >= 0)
	mem[P] = ea;
      continue;
    }

    if (opcode == 00503) {
      fprintf(stderr," JGT\n");
      if (*(short *)(mem+A) > 0)
	mem[P] = ea;
      continue;
    }

    if (opcode == 00403) {
      fprintf(stderr," JLE\n");
      if (*(short *)(mem+A) <= 0)
	mem[P] = ea;
      continue;
    }

    if (opcode == 00603) {
      fprintf(stderr," JLT\n");
      if (*(short *)(mem+A) < 0)
	mem[P] = ea;
      continue;
    }

    if (opcode == 00303) {
      fprintf(stderr," JNE\n");
      if (*(short *)(mem+A) != 0)
	mem[P] = ea;
      continue;
    }

    if (opcode == 01502) {
      fprintf(stderr," JDX\n");
      mem[X]--;
      if (mem[X] == 0)
	mem[P] = ea;
      continue;
    }

    if (opcode == 01503) {
      fprintf(stderr," JIX\n");
      mem[X]++;
      if (mem[X] == 0)
	mem[P] = ea;
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
      if (*(int *)(mem+A) == *(int *)(mem+ea))
	mem[P]++;
      else if (*(int *)(mem+A) < *(int *)(mem+ea))
	mem[P] += 2;
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

  unsigned short code;             /* code word following SVC instruction */
  unsigned short argl;             /* address of argument list */
  unsigned short a1,a2,a3,a4,a5,a6; /* args */
  short i,tempa;
  char *bp;
  short class;                     /* SVC class, from bits 5-10 */
  short func;                      /* SVC func within class, bits 11-16 */
  
  code = mem[mem[P]];
  if (code & 0100000)
    argl = mem[mem[P]-2];
  else if (code & 040000)
    argl = mem[P]+2;
  else
    argl = mem[P]+1;
  class = (code >> 6) & 077;
  func = code & 077;
  fprintf(stderr," code=%o, class=%o, func=%o, arglist=%o\n", code, class, func, argl);

  switch (class) {
  case 0: /* same as class 1 */
  case 1: /* funcs 0-'15 */
    switch (func) {
    case 5: /* EXIT */
      fprintf(stderr,"\nProgram called EXIT\n");
      exit(1);
    default:
      goto unimp;
    }
    break;

  case 2: /* funcs 0-3 */
    goto unimp;
    break;

  case 3: /* func 0 = PRWFIL */
    goto unimp;
    break;

  case 4: /* func 0 = FAMSVC (obsolete) */
    goto unimp;
    break;

  case 5: /* funcs 0-'17 */
    goto unimp;
    break;

  case 6: /* funcs 0-5 */
    switch (func) {
    case 0:  /* comanl, 0 args */
      goto unimp;
    case 1:  /* c1in, 1 arg */
      fprintf(stderr," c1in - enter a character!\n");
      goto badsvc;
      a1 = mem[argl++];
      tempa=getchar() | 0x80;   /* WRONG - needs to read from /dev/tty */
      mem[a1] = tempa;
      break;
    default:
      goto unimp;
    }
    break;

  case 7:
    switch (func) {
    case 0:
      fprintf(stderr," t1in\n");
      exit(1);
      break;
    case 1:
      fprintf(stderr," t1ou\n");
      a1 = mem[argl++];          /* get address of arg1 */
      a1 = mem[a1];              /* get the character to print */
      putchar(a1 & 0x7f);        /* write the character w/o parity */
      break;
    case 2: /* TNOU */                       
    case 3: /* TNOUA */
      a1 = mem[argl++];
      a2 = mem[argl++];
      bp = (char *)(mem+a1);
      fprintf(stderr," tnou/a, arg1 @ %o, arg2 @ %o\n", a1, a2);
      for (i=0; i < mem[a2]; i++,bp++)
	putchar(*bp & 0x7f);
      if (func == 2)
	putchar('\n');
      fflush(stdout);
      break;
    case 4:
      fprintf(stderr," tooct\n");
      exit(1);
      break;
    case 5:
      fprintf(stderr," duplx$\n");
      exit(1);
      break;
    default:
      goto unimp;
    }
    break;

  case 010:
    goto unimp;
    break;

  case 011:
    goto unimp;
    break;

  case 012:
    goto unimp;
    break;

  case 013:
    goto unimp;
    break;

  case 014:
    goto unimp;
    break;

  case 015:
    switch (func) {
    case 024:                     /* ERKL$$(KEY,ERASECH,KILLCH,CODE) */
      fprintf(stderr," erlk$$\n");
      a1 = mem[argl++];
      a2 = mem[argl++];
      a3 = mem[argl++];
      a4 = mem[argl++];
      if (mem[a1] == k$read) {
	mem[a2] = 0210;
	mem[a3] = 0377;
      }
      mem[a4] = 0;
      break;
    default:
      goto unimp;
    }
    break;

  default:
    goto unimp;                   /* bad class */
  }

  /* after the SVC, argl is the return address; look for zero arg list
     terminator and ignore it) */

  if (mem[argl] == 0)
    argl++;
  fprintf(stderr," returning from SVC to %o\n", argl);
  mem[P] = argl;
  return;


unimp:

  fprintf(stderr," svc class '%o function '%o is not implemented\n", class, func);
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
  
  fprintf(stderr," halting on bad svc\n");
  exit(1);
}



/* here for PIO instructions: OCP, SKS, INA, OTA.  The instruction
   word is passed in as an argument to handle EIO (Execute I/O) in
   V-mode.
*/

pio(unsigned int inst) {
  unsigned short class;
  unsigned short func;
  unsigned short device;

  class = inst >> 14;
  func = (inst >> 6) & 017;
  device = inst & 077;
  fprintf(stderr," pio, class=%d, func=%o, device=%o\n", class, func, device);
  if (devmap[device])
    devmap[device](class, func);
  else
    fprintf(stderr," no handler for device; instruction ignored\n");
}
