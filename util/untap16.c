/* untap.c, J. Wilcoxson, March 19, 2005
   Reads a tap magtape file and removes the tap information.
   tap reference: http://simh.trailing-edge.com/docs/simh_magtape.pdf
   NOTE: for this modified version, a 16-bit Prime word length is used
   to delimit records.
*/

#include <stdlib.h>
#include <stdio.h>
#define BUFSIZE 16*1024

/* reads a 16-bit, big-endian record length, which is the number of 16-bit
   words in the tape record.  The upper 3 bits are apparently flag bits,
   and these are moved the the upper 3 bits of the 32-bit word */

int readint_be () {
  int n,ch;

  n = (getchar()<<8 | getchar());
  n = ((n & 0xE000) << 16) | ((n & 0x1FFF) * 2);
  if (feof(stdin)) {
    fprintf(stderr,"End of file reading record length, position = %d\n", ftell(stdin));
    exit(0);
  }
  return n;
}

main (int argc, char** argv) {
  int fp;                        /* input file position, zero based */
  int recno;                     /* record number, 1st record is 1 */
  int reclen;                    /* length of record (bytes) */
  int flag1,flag2;               /* leading & trailing flags */
  int i,ch;
  int ones;                      /* count of held-back "all ones" bytes */
  int allff;                     /* true if record was all 1 bits */
  int verbose;
  unsigned char buf[BUFSIZE];
  unsigned char bufhdr[5];       /* hack to display Prime 4-char tape hdr */

  /* init */

  recno = 0;
  ones = 0;
  fp = 0;
  verbose = 0;

  /* check args */

  for (i=1; i<argc; i++) {
    if (strcmp(argv[i],"-vv") == 0)
      verbose = 2;
    else if (strcmp(argv[i],"-v") == 0)
      verbose = 1;
  }

  while (1) {
    recno++;
    if (feof(stdin)) {
      if (verbose >= 1)
	fprintf(stderr,"End of file at record %d\n", recno);
      exit(0);
    }
    fp=ftell(stdin); 

    reclen = readint_be();
    if (reclen == 0xFFFFFFFF) {    /* end of medium */
      if (verbose >= 1)
	fprintf(stderr,"End of medium at record %d, position %d\n", recno, fp);
      exit(0);
    }

    if (reclen == 0) {    /* tape mark */
      if (verbose >= 1)
	fprintf(stderr,"Tape mark at record %d, position %d\n", recno, fp);
      continue;
    }
    flag1 = reclen & 0xF0000000;
    if (flag1 != 0) fprintf(stderr,"Leading flags are 0x%08x, position %d\n", flag1, fp);
    reclen = reclen & 0x0FFFFFFF;
    if (reclen > BUFSIZE) {
      fprintf(stderr,"Record too big at record %d, position %d: increase BUFSIZE to %d\n", recno, fp, reclen);
      exit(1);
    }

    allff = 1;
    for (i=0; i<reclen; i++) {
      buf[i] = getchar();
      if (buf[i] != 0xff)
	allff = 0;
    }

    i = readint_be();
    flag2 = i & 0xF0000000;
    if (flag2 != 0 && flag2 != 0x60000000) fprintf(stderr,"Trailing flags are 0x%08x, position %d\n", flag2, fp);
    i = i & 0x0FFFFFFF;
    if (i != reclen) {
      fprintf(stderr,"Record length mismatch at record %d, position %d: %d != %d\n", recno, fp, reclen, i);
      exit(1);
    }

    if (reclen&1) {
      fprintf(stderr,"WARNING: Record %d has odd length of %d, position %d; record discarded!\n", recno, reclen, fp);
      continue;
    }

    if (flag1 & 0x80000000) {    /* error in record */
      fprintf(stderr,"Record %d flagged as error, position %d; IGNORED!\n", recno, fp);
      continue;
    }

    /* hack to display Prime drb record headers */

    if (verbose >= 2) {
      for (i=0; i<4; i++)
	bufhdr[i] = buf[i] & 0x7f;
      bufhdr[4] = 0;
      fprintf(stderr,"Record %d, position %d, length %d, hdr %s\n", recno, fp, reclen, bufhdr);
    }

    /* sometimes tap files end with a stream of 0xff bytes when there
       is no more data on the tape.  To prevent writing this trailing
       stream of garbage, keep a count of records full of 0xff bytes
       and write them out when real data is seen again */

    if (allff) {
      if (verbose >= 2)
	fprintf(stderr,"Record %d is all ones, position %d\n", recno, fp);
      ones += reclen;
    } else {
      while (ones-- > 0)
	putchar(0xff);
      ones = 0;
      for (i=0; i<reclen; i++)
	putchar(buf[i]);
    }

    /* if flag bits are 0x60000000, then this is the last logical record in
       a block and there is a 16-bit thing following */

    if (flag2 == 0x60000000) {
      for (i=0; i<16; i++)
	buf[i] = getchar();
    }
  }
}
