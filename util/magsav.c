/* magsav.c, Jim Wilcoxson, February 23, 2007
   Unix version of magsav, to create Prime tape files from Unix file systems.
   This program creates old Magsav format tapes, because they're simpler.

   Structure of an old Magsav tape:

   1. Every record has a 3-word header
         word 1: logical record number in logical tape, starting with 1
	 word 2: record length, including the 3-word header
	 word 3: record id:
                   4 = start logical tape
                   2 = name record (block of ufd entries)
                   1 = data record
                   5 = end logical tape

   1. start logical tape record:
         word 4: tape format (see magrst; we use 0xC000: rev 19 w/o ACLS)
	 word 5-7:  MMDDYY date (text)
	 word 8: user version number (short int)
	 word 9: reel
	 word 10-12: tape name (text)

   2. tape file mark
 
   3. Name record, data record(s), Name record, data record(s), ...

   4. end logical tape record

   5. 2 tape file marks
*/

#define TAP_FILE_MARK 0
#define MS_DATA 1
#define MS_NAME 2
#define MS_START_LOG_TAPE 4
#define MS_END_LOG_TAPE 5

#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>      /* mkdir */
#include <sys/types.h>     /* mkdir */
#include <errno.h>
#include <time.h>          /* mktime */
#include <utime.h>         /* utimes */
#include <dirent.h>

#include "istext.h"

#define MAXDIRENTRIES 32

#define MAXEXTENSION 10

static struct {
  char ext[MAXEXTENSION];
  int  ftype;
} exttype[] = {
  {".como", 1},
  {".run", 1},
};
#define EXTENTRIES sizeof(exttype)/sizeof(exttype[0])

/* global vars */

FILE *outfile;
int verbose = 0;
unsigned short lrecl;
struct {
  unsigned short ibuf[24];
} ds[MAXDIRENTRIES];
int dl = -1;               /* # of entries in ds; -1=empty */

helpexit() {
  fprintf(stderr, "Usage: magsav [-v] [-vv] -f <output file> [file1] [file2] ...\n");
  fprintf(stderr, "Saves file1-filen in Prime MAGSAV format to output file.\n");
  exit(0);
}

/* this function takes a Unix timestamp and converts it to a Prime
   filesystem timestamp (a 32-bit integer).
   Format of a Prime FS timestamp is:

   left 16 bits: YYYYYYYMMMMDDDDD, year is mod 100
   right 16 bits: seconds since midnight divided by 4, ie, 0-21599
*/

unsigned int utimestampp(time_t unixtime) {
  int i;
  unsigned short pdate;
  unsigned short ptime;
  struct tm *tms;

  /* break down Unix timestamp */

  tms = localtime(&unixtime);
  i = tms->tm_year;
  if (i > 127)
    i = 127;            /* dont' let it overflow! */
  pdate = (tms->tm_year<<9) | ((tms->tm_mon+1)<<5) | tms->tm_mday;
  ptime = (tms->tm_hour*3600 + tms->tm_min*60 + tms->tm_sec)/4;
  return (pdate<<16) | ptime;
}


/* write a Magsav tape record in .TAP format:
   - 4 bytes for .TAP format record length (bytes following this)
   - 3 word (6 byte) Magsav tape record header
   - Magsav record
   - 4 bytes for .TAP format ending record length
*/

mtwrite (int recid, unsigned short *ibuf, int nw) {
  
  int tapbytes;
  unsigned char tapbuf[4];
  short mshdr[3];

  if (recid == TAP_FILE_MARK)
    tapbytes = 0;
  else
    tapbytes = (nw+3)*2;
  tapbuf[0] = tapbytes & 0xFF;
  tapbuf[1] = (tapbytes>>8) & 0xFF;
  tapbuf[2] = (tapbytes>>16) & 0xFF;
  tapbuf[3] = (tapbytes>>24) & 0xFF;
  if (fwrite(tapbuf, 1, 4, outfile) != 4) {
    perror("Writing .TAP leading record length");
    exit(1);
  }
  if (recid != TAP_FILE_MARK) {
    mshdr[0] = htons(lrecl);
    mshdr[1] = htons(nw+3);
    mshdr[2] = htons(recid);
    if (fwrite(mshdr, 2, 3, outfile) != 3) {
      perror("Writing MAGSAV record header");
      exit(1);
    }
    if (fwrite(ibuf, 2, nw, outfile) != nw) {
      perror("Writing MAGSAV record buffer");
      exit(1);
    }
    if (fwrite(tapbuf, 1, 4, outfile) != 4) {
      perror("Writing .TAP trailing record length");
      exit(1);
    }
  }
  lrecl++;
}


savename() {
  mtwrite(MS_NAME, (unsigned short *)&ds, (dl+1)*24);
}


pushname(char *name, struct stat sb) {
  int i,j;
  char *p, ch;
  int filetype;
  unsigned char extension[8];

  dl++;
  if (dl > MAXDIRENTRIES) {
    fprintf(stderr, "directory stack overflowflow!\n");
    exit(1);
  }
  if ((sb.st_mode & S_IFMT) == S_IFDIR)
    filetype = 5;     /* ACL directory type */
  else {

    /* scan backward to get file extension */

    filetype = 0;     /* assume SAM file type */
    extension[0] = 0;
    for (i=strlen(name)-1; i >= 0; i--)
      if (name[i] == '.') {
	strncpy(extension, name+i, sizeof(extension)-1);
	break;
      }
    if (extension[0] == '.') 
      for (i=0; i < EXTENTRIES; i++)
	if (strcasecmp(extension, exttype[i].ext) == 0)
	  filetype = exttype[i].ftype;
  }

  /* ECW: left byte=dir entry type, right byte = size in words, incl. ECW
     
     Directory entry types:
       0 = old directory header
       1 = directory header
       2 = vacant entry
       3 = file entry
       4 = access category
       5 = ACL
       6 = directory index block
  */

  ds[dl].ibuf[0] = htons((3<<8) | 24);
  p = (char *)(ds[dl].ibuf+1);      /* point to name part */
  j = 0;
  for (i=0; i<32; i++) {            /* copy name, upcase, fix parity */
    ch = name[j++];
    if (ch == 0)
      ch = ' ';
    else if (ch == 'S')
      ch = '/';
    else if ('a' <= ch && ch <= 'z')
      ch = ch - 'a' + 'A';
    *p++ = ch | 0x80;
  }
  ds[dl].ibuf[17] = htons(0xFF00);  /* owner/non-owner protection */
  ds[dl].ibuf[18] = 0;              /* ACL protection */
  ds[dl].ibuf[19] = htons(filetype);
  *(int *)(ds[dl].ibuf+20) = htonl(utimestampp(sb.st_mtime));
  ds[dl].ibuf[22] = 0;              /* reserved */
  ds[dl].ibuf[23] = 0;
}

popname() {
  if (dl >= 0)
    dl--;
  else {
    fprintf(stderr, "directory stack underflow!\n");
    exit(1);
  }
}
    
savefile(char *path, char *name, struct stat sb) {

  char buf[4090];
  int i, n, fd, nw;
  int first, text;
  utextp_t state;

  if ((fd=open(path, O_RDONLY)) == -1) {
    perror(NULL);
    return;
  }
  pushname(name, sb);
  savename();
  popname();

  state.oddbyte = 0;
  state.col = 0;
  state.spaces = 0;

  first = 1;
  while ((n=read(fd, buf, sizeof(buf))) > 0) {
    if (first)
      text = isutext(path, buf, n);
    if (n & 1) {
      fprintf(stderr,"Warning: odd-length file padded with newline or zero\n");
      if (text)
	buf[n++] = '\n';
      else
	buf[n++] = 0;
    }
    if (text) {
    }
    nw = (n+1)/2;
    mtwrite(MS_DATA, (unsigned short *)buf, nw);
  }
  close(fd);
}


savedir(char *path, char *name, struct stat sb) {

  unsigned short ibuf[9];
  DIR *dp;
  struct dirent *de;
  int i;

  if ((dp=opendir(path)) == NULL) {
    perror(NULL);
    return;
  }
  pushname(name, sb);
  savename();
  
  /* write MAGSAV directory data record */

  ibuf[0] = 8;
  for (i=1; i<3; i++)
    ibuf[i] = ((' '<<8) | ' ') | 0x8080;  /* owner pw */
  ibuf[4] = ibuf[5] = ibuf[6] = 0;        /* non-owner password */
  ibuf[7] = 0;
  ibuf[8] = 0;
  mtwrite(MS_DATA, ibuf, 9);

  /* write all directory entries */

  while ((de=readdir(dp)) != NULL) {
    if (strcmp(de->d_name,".") == 0 || strcmp(de->d_name,"..") == 0)
      continue;
    save(path, de->d_name);
  }

  /* all done */

  closedir(dp);
  popname();
}


save(char *parent, char *name) {

  char path[1024];
  struct stat sb;

  path[0] = 0;
  strcpy(path, parent);
  strcat(path, "/");
  strcat(path, name);
  printf("%s\n", path);
  if (stat(path, &sb) == -1) {
    perror("Stat error");
    return;
  }
  if ((sb.st_mode & S_IFMT) == S_IFDIR)
    savedir(path, name, sb);
  else if ((sb.st_mode & S_IFMT) == S_IFREG)
    savefile(path, name, sb);
  else 
    fprintf(stderr, "Ignored %s: can't save this file type\n", name);
}

main (int argc, char** argv) {

  int argx;
  union {
    unsigned short i[16];
    unsigned char c[32];
  } buf;
  int i;

  /* init */

  outfile = NULL;

  /* any args? */

  for (argx=1; argx<argc; argx++) {
    if (strcmp(argv[argx],"-vv") == 0)
      verbose = 2;
    else if (strcmp(argv[argx],"-v") == 0)
      verbose = 1;
    else if (strcmp(argv[argx],"-f") == 0) {
      argx++;
      if (argx < argc) {
	if ((outfile=fopen(argv[argx], "w")) == NULL) {
	  perror("Unable to open output file");
	  exit(1);
	}
      } else {
	fprintf(stderr, "No output file following -f\n");
	helpexit();
      }
    } else if (argv[argx][0] == '-')
      helpexit();
    else
      break;
  }

  /* if no output file specified, give help */

  if (!outfile)
    helpexit();

  /* write "start logical tape" record */

  lrecl = 1;
  buf.i[0] = htons(0xC000);
  strcpy(buf.c+2, "010199"); uasciip(buf.c+2, 6);
  buf.i[4] = 0;
  buf.i[5] = htons(1);
  strcpy(buf.c+12, "MAGSAV"); uasciip(buf.c+12, 6);
  mtwrite(MS_START_LOG_TAPE, buf.i, 9);
  mtwrite(TAP_FILE_MARK, 0, 0);

  /* write files */

  lrecl = 1;
  for (;argx < argc; argx++) {
    save(".", argv[argx]);
  }

  /* end logical tape */

  mtwrite(MS_END_LOG_TAPE, 0, 0);
  mtwrite(TAP_FILE_MARK, 0, 0);
  mtwrite(TAP_FILE_MARK, 0, 0);
  fclose(outfile);
}   
     
