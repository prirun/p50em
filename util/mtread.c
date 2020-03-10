/* mtread.c, Jim Wilcoxson, February 4, 2007
   Reads a magtape connected to a Linux system and writes records in .TAP
   format.  The entire tape is read until:
   - a read error occurs
   - the EOT is encountered
   - two file marks are read (logical EOT)
*/

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/mtio.h>

main (int argc, char **argv) {

  char *outfile;
  int tapefd, fdout;
  struct mtop mt_cmd;
  struct mtget mt_status;
  struct mtpos mt_pos;
  int filenr, relrecnr;
  int n,n2;
  int outpos;
  int mark;
  char buf[20000];

  if (argc < 2) {
    printf("Usage: mtread <output file>\n");
    exit(1);
  }

  outpos = 0;
  filenr = 1;
  relrecnr = 0;
  mark = 0;

  outfile = argv[1];
  if ((fdout=open(outfile, O_WRONLY+O_CREAT+O_EXCL, 0770)) == -1) {
    perror("Error opening output file");
    exit(1);
  }

  if ((tapefd=open("/dev/st0", O_RDONLY)) == -1) {
    perror("Error opening tape drive");
    goto done;
  }

#if 0

  /* set tape to do fast EOM.  This can be useful if a tape is "loose"
     on the reel, to tighten up the tape */

  mt_cmd.mt_op = MTSETDRVBUFFER;
  mt_cmd.mt_count = MT_ST_SETBOOLEANS | MT_ST_FAST_MTEOM;
  if (ioctl(tapefd, MTIOCTOP, &mt_cmd) != 0) {
    perror("Error setting fast EOM");
    goto done;
  }

  /* skip to EOM */
  
  mt_cmd.mt_op = MTEOM;
  mt_cmd.mt_count = 0;
  if (ioctl(tapefd, MTIOCTOP, &mt_cmd) != 0) {
    perror("Error skipping to EOM");
    goto done;
  }
#endif

  /* rewind tape */

#if 1
  mt_cmd.mt_op = MTREW;
  mt_cmd.mt_count = 0;
  if (ioctl(tapefd, MTIOCTOP, &mt_cmd) != 0) {
    perror("Error rewinding");
    goto done;
  }
#endif

  /* set tape blocksize to 0 */

  mt_cmd.mt_op = MTSETDRVBUFFER;
  mt_cmd.mt_count = 0;
  if (ioctl(tapefd, MTIOCTOP, &mt_cmd) != 0) {
    perror("Error setting variable blocksize");
    goto done;
  }

  while(1) {
    n=read(tapefd, buf+4, sizeof(buf)-8);
    relrecnr++;
    if (n == -1) {
#if 1
      if (mark == 2) {
	printf("Read error after double tape mark, assuming tape EOF\n");
	goto done;
      }
#endif
      perror("Read error");
      printf("File %d, record %d: read error\n", filenr, relrecnr);
      *(int *)buf = 0;
      buf[3] = 0x80;
      if (write(fdout, buf, 4) != 4) {
	perror("Error writing error mark");
	goto done;
      }
#if 0
      goto done;
#else
      printf("Hit Enter to continue...");
      getchar();
#endif
    } else if (n == 0) {
      printf("File mark %d at outpos %d\n", filenr, outpos);
      *(int *)buf = 0;
      if (write(fdout, buf, 4) != 4) {
	perror("Error writing file mark");
	goto done;
      }
      mark += 1;
      filenr++;
      relrecnr = 0;
    } else {
      *(int *)buf = n;
      *(int *)(buf+n+4) = n;
      mark = 0;
#if 0
      printf("File %d record %d outpos %d size %d\n", filenr, relrecnr, outpos, n);
#endif
      if ((n2=write(fdout, buf, n+8)) != n+8) {
	if (n2 == -1) {
	  perror("Error writing output file");
	  goto done;
	}
	printf("Error writing output file, read %d, wrote %d\n", n, n2);
	goto done;
      }
      outpos += n+8;
    }
  }

done:
  if (outpos == 0)
    unlink(outfile);
  sleep(2);
  mt_cmd.mt_op = MTREW;
  mt_cmd.mt_count = 0;
  if (ioctl(tapefd, MTIOCTOP, &mt_cmd) != 0) {
    perror("Error rewinding");
  }
  close(tapefd);
  exit(1);

}
