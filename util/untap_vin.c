/* untap.c, J. Wilcoxson, March 19, 2005
   Reads a tap magtape file and removes the tap information.
   tap reference: http://simh.trailing-edge.com/docs/simh_magtape.pdf
*/

#include <stdlib.h>
#include <stdio.h>
#define BUFSIZE 128*1024

/* in the tap format, record lengths are always stored little-endian */

int readint_le () {
  int n,ch;

  n = getchar() | getchar()<<8;
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

    reclen = readint_le();
    if (reclen == 0) {    /* end of medium */
      if (verbose >= 1)
	fprintf(stderr,"Zero rec length at record %d, position %d\n", recno, fp);
      exit(0);
    }
    if (reclen == 0xFFFF) {    /* tape mark */
      if (verbose >= 1)
	fprintf(stderr,"Tape mark at record %d, position %d\n", recno, fp);
      continue;
    }
    if (reclen & 0x80000000) {    /* error in record */
      fprintf(stderr,"Record %d flagged as error, position %d\n", recno, fp);
#if 0
      if (recno > 1)
        exit(1);
#endif
      continue;
    }
    reclen = reclen & 0x00FFFFFF;
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

    i = readint_le();
    if (i != reclen) {
      fprintf(stderr,"Record length mismatch at record %d, position %d: %d != %d\n", recno, fp, reclen, i);
      exit(1);
    }

    if (reclen&1) {
      fprintf(stderr,"WARNING: Record %d has odd length of %d, position %d; record discarded!\n", recno, reclen, fp);
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
  }
}
