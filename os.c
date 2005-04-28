/* os.c, Jim Wilcoxson, 4/15/2005
   Emulations of Primos Operating System routines for Unix.
*/

#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <time.h>
#include <sys/times.h>

#include "os.h"
#include "syscom/keys.ins.cc"
#include "syscom/errd.ins.cc"


os_init() {

  os.ttydev = open("/dev/tty", O_RDWR, 0);
  if (os.ttydev < 0) {
    perror(" error opening /dev/tty");
    exit(1);
  }
}


os_break$(short *onoff) {
}

os_c1in(short *charg) {
  unsigned char ch;
  int n;

  fprintf(stderr," c1in waiting for data\n");
  n = read(os.ttydev, &ch, 1);
  if (n < 0) {
    perror(" error reading from tty");
    exit(1);
  }
  if (n != 1) {
    fprintf(stderr," unexpected error reading from tty, n=%d", n);
    exit(1);
  }
  *charg = ch | 0x80;
}


os_comanl() {
}


os_cnin$(unsigned char *buf, short *maxchars, short *n) {
  short ch;

  *n = 0;
  while (*n < *maxchars) {
    os_c1in(&ch);
    buf[*n] = ch;
    (*n)++;
    if (ch == 0212) break;
  }
}


os_exit() {
  fprintf(stderr,"\nProgram called EXIT\n");
  exit(1);
}


os_erkl$$(short *key, short *erasech, short *killch, short *code) {
  if (*key == k$read) {
    *erasech = 0210;
    *killch = 0377;
    *code = 0;
  }
}


os_errpr$ (short *key, short *code, char *msg, short *msglen, char *prog, short *proglen) {
  if (*code == 0) return;
  fprintf (stderr, " key=%d, code=%d, msglen=%d, proglen=%d\n", *key, *code, *msglen, *proglen);
  exit(1);
}

os_ginfo (short *buf, short *bufsiz) {
  int i;

  fprintf(stderr," ginfo bufsiz=%d\n", *bufsiz);
  if (*bufsiz <= 0) return;
  buf[0] = 0;
  if (*bufsiz > 1) buf[1] = 0;
  if (*bufsiz > 2) buf[2] = 61;   /* max device # */
  for (i=0; i < *bufsiz-3; i++)
    buf[i+3] = 440;               /* disk record size */
}

/* RDTK$$

   key -  1=read UC, 2=read, 3=reset, 4=rest, 5=init
   info - output array
     [0] = type - 1=normal, 2=regular, 5=null, 6=eol
     info[1] = length
     info[2] = flags
       :100000=decimal, :40000=octal, :20000=dash, :10000=info[3] < 8
     info[3] = positional token's position 0-9 (1/44,  4/20, etc)
     info[4] = positional token's value
   buf - output token buffer
   buflen - size in words of buf (in)
   code - return code

*/

os_rdtk$$(short *key, short *info, char *buf, short *buflen, short *code) {
  fprintf(stderr," key=%d\n", *key);
  *code = 0;
  if (*key = 1) {
    info[0] = 6;
    info[1] = 0;
    info[2] = 0;
    info[3] = 0;
    info[4] = 0;
  } else {
    exit(1);
  }
}

os_t1ou(short *charg) {
  putchar(*charg & 0x7f);
  fflush(stdout);
}


os_timdat(short *userbuf, short *n) {
  clock_t tod;
  struct tm tms;
  struct {
    char mmddyy[6];
    short timemins;     /* minutes since midnight */
    short timesecs;     /* plus seconds (0-50) */
    short timeticks;    /* plus ticks (0-329 330ths of a second) */
    short cpusecs;
    short cputicks;
    short iosecs;
    short ioticks;
    short tickspersec;
    short userno;
    char  username[32];
  } timbuf;

  if (sizeof(timbuf) != sizeof(short)*28) {
    fprintf(stderr," sizeof(timebuf)=%d, size of 28-word array=%d\n", 
	    sizeof(timbuf), sizeof(short)*28);
    exit(1);
  }

  tod = time(NULL);
  localtime_r(&tod, &tms);
  strncpy(timbuf.mmddyy,"042105",6);
  timbuf.timemins = tms.tm_hour*60 + tms.tm_min;
  timbuf.timesecs = tms.tm_sec;
  timbuf.timeticks = 0;
  timbuf.cpusecs = timbuf.cputicks = timbuf.iosecs = timbuf.ioticks = 0;
  timbuf.tickspersec = 330;
  timbuf.userno = 1;
  strncpy(timbuf.username,"SYSTEM                          ",32);
  memcpy(userbuf, &timbuf, *n*2);
}


os_tnoua(unsigned char *s, short *len) {
  int i;
  fprintf(stderr," writing %d char string \"", *len);
  for (i=0; i < *len; i++)
    fputc(s[i] & 0x7f,stderr);
  fprintf(stderr,"\"\n");

  for (i=0; i < *len; i++)
    putchar(s[i] & 0x7f);
  fflush(stdout);
}
