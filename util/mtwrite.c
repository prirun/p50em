/* mtwrite.c, Jim Wilcoxson, July 14, 2011
   Writes a .TAP disk file to a magtape
*/

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/mtio.h>

#define BUFSIZE 20000

/* in the tap format, record lengths are always stored little-endian */

int readint_le () {
  int n,ch;

  n = getchar() | getchar()<<8 | getchar()<<16 | getchar()<<24;
  if (feof(stdin)) {
    fprintf(stderr,"End of file reading record length, position = %d\n", ftell(stdin));
    exit(0);
  }
  return n;
}

main (int argc, char **argv) {

  char *infile;
  int tapefd, fdin;
  struct mtop mt_cmd;
  struct mtget mt_status;
  struct mtpos mt_pos;
  int filenr, relrecnr;
  int reclen, reclen2;
  int n,n2;
  int outpos;
  int mark;
  char buf[BUFSIZE];

  filenr = 1;
  relrecnr = 0;
  mark = 0;

  if ((tapefd=open("/dev/st0", O_RDWR)) == -1) {
    perror("Error opening tape drive");
    goto done;
  }

  /* set tape blocksize to 0 */

  mt_cmd.mt_op = MTSETDRVBUFFER;
  mt_cmd.mt_count = 0;
  if (ioctl(tapefd, MTIOCTOP, &mt_cmd) != 0) {
    perror("Error setting variable blocksize");
    goto done;
  }

  /* set buffering */

  mt_cmd.mt_op = MTSETDRVBUFFER;
  mt_cmd.mt_count = 1;
  if (ioctl(tapefd, MTIOCTOP, &mt_cmd) != 0) {
    perror("Error setting buffering params");
    goto done;
  }
  mt_cmd.mt_op = MTSETDRVBUFFER;
  mt_cmd.mt_count = 1;
  mt_cmd.mt_count |= MT_ST_BOOLEANS | MT_ST_BUFFER_WRITES | MT_ST_ASYNC_WRITES;
  if (ioctl(tapefd, MTIOCTOP, &mt_cmd) != 0) {
    perror("Error setting buffering params");
    goto done;
  }

#if 0
  /* set density: 1=800bpi; 2=1600bpi; 3=6250bpi */

  mt_cmd.mt_op = MTSETDENSITY;
  mt_cmd.mt_count = 2;
  if (ioctl(tapefd, MTIOCTOP, &mt_cmd) != 0) {
    perror("Error setting variable blocksize");
    goto done;
  }
#endif

  while(1) {
    relrecnr++;
    reclen = readint_le();
    if (reclen == 0xFFFFFFFF)     /* end of medium */
      goto done;
    else if (reclen & 0x80000000) {    /* error in record */
      printf("File %d, record %d contains error; writing zero record\n", filenr, relrecnr);
      *(int *)buf = 0;
      buf[0] = 4;
      *(int *)(buf+4) = 0;
      *(int *)(buf+8) = *(int *)buf;
      if (write(tapefd, buf, 12) != 12) {
	perror("Error writing error record");
	goto done;
      }
    } else if (reclen == 0) {
      printf("File mark %d\n", filenr);
      mt_cmd.mt_op = MTWEOF;
      mt_cmd.mt_count = 1;
      if (ioctl(tapefd, MTIOCTOP, &mt_cmd) != 0) {
	perror("Error writing tape mark");
	goto done;
      }
      filenr++;
      relrecnr = 0;
    } else {
      reclen = reclen & 0x00FFFFFF;
      if (reclen > BUFSIZE) {
	fprintf(stderr,"Record too big at record %d: increase BUFSIZE to %d\n", relrecnr, reclen);
	exit(1);
      }
      n=fread(buf, 1, reclen, stdin);
      if (n != reclen) {
	fprintf(stderr,"Read error at record %d, expected %d, got %d bytes\n", relrecnr, reclen, n);
	exit(1);
      }
      reclen2 = readint_le();
      if (reclen2 != reclen) {
	fprintf(stderr,"Read error at record %d, expected trailing reclen %d, got %d\n", relrecnr, reclen, reclen2);
	exit(1);
      }
      if ((n2=write(tapefd, buf, reclen)) != reclen) {
	if (n2 == -1) {
	  perror("Error writing tape device");
	  goto done;
	}
	printf("Error writing tape device, read %d, wrote %d\n", reclen, n2);
	goto done;
      }
    }
  }

done:
  sleep(2);
  mt_cmd.mt_op = MTREW;
  mt_cmd.mt_count = 0;
  if (ioctl(tapefd, MTIOCTOP, &mt_cmd) != 0) {
    perror("Error rewinding");
  }
  close(tapefd);
  exit(1);
}
