/* magrst.c, Jim Wilcoxson, March 19, 2005
   Reads both old and new (drb) format Magsav tape files on Unix.

   Still to do:
   - nested segdirs haven't been tested
   - .TAP files aren't handled directly - need to untap first
   - file types are lost, so Unix Magsav has to guess the file type
   - filename translation (slashes, etc.) is not well thought out
   - make a hidden symlink for max entries in a segdir (emulation of segdir)
   - acls and category acls aren't handled

   - roam, rbf, and cam entries aren't handled (who cares!)
   - print a warning/error if reels are out of order
   - no partial restores
   - not tested on multi-reel backups
   - does a zero-length file have a data record?
   - needs error check for path and buf overflows
   - should have a force-overwrite option, and not overwrite otherwise
   - an option to save the boot program to disk?
   - an option to just display an index with gory details?
   - an option to not lowercase filenames?
   - ptimestampu should check to see if current timezone observes DST
   - isptext maybe should ensure :001 is at the beginning of the line
   - a single non-text character prevents text conversion; use a percentage?
   - when weirdness happens, set "skipping" more often instead of bombing
   - test that skipping=1 actually works
   - have a list of suffixes that are most likely text (.cpl, .list, etc)
     and a list that are most likely binary (.bin, .run, etc.).  If tasting
     the file shows a different type, prompt user (or write both types and
     let them choose later, in the emulator)
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>      /* mkdir */
#include <sys/types.h>     /* mkdir */
#include <errno.h>
#include <time.h>          /* mktime */
#include <utime.h>         /* utimes */

/* Magsav and "new" Magsav (aka drb) record ids.
   NOTE: magsav recid's are positive on the tape! */

#define MS_DATA -1
#define MS_NAME -2
#define MS_START_LOG_TAPE -4
#define MS_END_LOG_TAPE -5

#define DRB_START_LOG_TAPE 1
#define DRB_START_OBJ 2
#define DRB_ACL_DATA 3
#define DRB_ACAT_DATA 4
#define DRB_FILE_DATA 5
#define DRB_ROAM_DATA 6
#define DRB_END_OBJ 7
#define DRB_END_LOG_TAPE 8

#define DRB_DIR_OBJ 1
#define DRB_FILE_OBJ 2
#define DRB_SEGDIR_OBJ 3
#define DRB_ACAT_OBJ 4
#define DRB_ROAM_OBJ 5
#define DRB_ROAM_SEGDIR_OBJ 6
#define DRB_SEG_SUBFILE_OBJ 7
#define DRB_SEG_SUBDIR_OBJ 8


/* writes a buffer of Prime text, converting it to Unix text.
   The 2-character space compression sequences may cross tape
   buffers; "state" is used to track this:
   
   state=0 means no compression pending
   state=1 means the next buffer character is the compression count

   Before calling convtext for a new file, state must be initialized
   to zero in the caller, then left alone after that.
 */

int convtext(int fd, unsigned char *buf, int len, int *state) {
  int i, n;
  unsigned char ch;

  /* NOTE: one interation through the text conversion loop could add up
     to 255 spaces because of text compression, so some slop is added 
     to the size of the output buffer in the declaration */

#define OBUFMAX 4096
  unsigned char obuf[OBUFMAX+256];

  n = 0;                      /* next output buffer postion */
  for (i=0; i<len; i++) {
    ch = buf[i];
    if (*state == 1) {        /* expand spaces */
      while (ch--)
	obuf[n++] = ' ';
      *state = 0;
    } else if (ch == 0221)    /* start of compression sequence */
      *state = 1;
    else {
      obuf[n++] = (ch & 0x7f);
      if (ch == 0212 && (i&1) == 0)
	i++;
    }
    if (n >= OBUFMAX) {
      if (fd > 0 && write(fd, obuf, n) != n) {
	fprintf(stderr,"File write error text conversion, n=%d\n", n);
	exit(1);
      }
      n = 0;
    }
  }
  if (fd > 0 && n > 0 && write(fd, obuf, n) != n) {
    fprintf(stderr,"File write error text conversion, n=%d\n", n);
    exit(1);
  }
  return len;
}


/* this function takes a Prime filesystem timestamp (a 32-bit integer)
   and returns a Unix timestamp.  Since Primos doesn't store timezone
   information in the timestamp, the current timezone is used.
   Format of a Prime FS timestamp is:

   left 16 bits: YYYYYYYMMMMDDDDD, year is mod 100
   right 16 bits: seconds since midnight divided by 4, ie, 0-21599
*/

time_t ptimestampu(unsigned int ptime) {
  int i;
  time_t unixtime;
  struct tm tms;

  i = ptime >> 25;      /* year mod 100 */
  if (i < 75)           /* assume 2000 if year >= 75 */
    i += 100;
  tms.tm_year = i;      /* mktime wants years since 1900 */
  tms.tm_mon = (ptime >> 21) & 0xf;
  tms.tm_mday = (ptime >> 16) & 0x1f;

  /* convert secs since midnight/4 to hours, minutes, and seconds */

  i = (ptime & 0xffff) * 4;
  tms.tm_hour = i/3600;
  tms.tm_min = (i%3600)/60;
  tms.tm_sec = i%60;
  tms.tm_isdst = 1;       /* use current timezone's DST flag? */

  unixtime = mktime(&tms);
  if (unixtime == -1) {
    fprintf(stderr,"Unable to convert Prime timestamp:\n");
    fprintf(stderr,"  year=%d, mon=%d, day=%d, hour=%d, min=%d, sec=%d\n", tms.tm_year, tms.tm_mon, tms.tm_mday, tms.tm_hour, tms.tm_min, tms.tm_sec);
  }
  return unixtime;
}


/* read a short (16-bit) integer in big-endian format into native format */

unsigned short readshort () {

  return getchar()<<8 | getchar();
}

/* read a long (32-bit) integer in big-endian format into native format */

int readlong () {
  int n,ch;

  return getchar()<<24 | getchar()<<16 | getchar()<<8 | getchar();
}


main (int argc, char** argv) {
  int verbose;              /* verbose level */
  int nowrite;              /* true if indexing only */
  int binary;               /* true = don't translate text files */
  int text;                 /* true = only restore text files */
  int drb;                  /* true if this is a drb save */
  int firstrec;             /* true if first record */
  int fp;                   /* current record's file position */
  int logrecno;             /* Magsav logical record number */
  int explogrecno;          /* expected logical record number (Magsav) */
  int expblockno;           /* expected block number (drb) */
  int nwords;               
  int i,ch;
  int recid;                /* record id */
  int wordsleft;            /* number of words left in current record */
  int tapeformat;           /* pre-19, 19 w/o ACLS, 19 w/ACLS, etc. */
  int objtype;              /* drb object type */
  int lrecversion;          /* drb logical record version */
  int filetype = -1;        /* Primos file type being restored */
  unsigned int dtm,dta,dtc,lsrdtm; /* date modified, accessed, created, parent dtm */
  int fd;                   /* Unix fd for writing output files */
  int nwritten;
  int ecwskip;              /* words to skip at end of entry */
  int skipping;             /* true if skipping the current object */
  int textfile;             /* true if converting a text file */
  int textstate;            /* tracks text file compression state */
  int maxentries;           /* maximum entries in a segdir */
  int segentry;             /* this file's entry in a segdir */
  int insegdir;             /* true if inside a segdir */
  unsigned char path[4096]; /* object pathname */
  struct {                  /* directory stack info */
    char  path[4096];       /* pathname */ 
    unsigned int   dta;     /* access time */
    unsigned int   dtm;     /* modification time */
  } dirstack[64];
  int dirlevel;             /* last entry in dirstack, -1 if empty */
  
  unsigned char *p;
  unsigned char buf[16*2048]; /* tape buffer (limit of 16K words on Prime) */
  struct utimbuf ut;
  int reel;                 /* reel number (old magsav) */
  short bootskiprecno, bootskipreclen, bootskiprecid;
  drb = 0;                  /* assume it's an old magsav tape initially */
  skipping = 1;             /* might allow us to correctly start w/reel 2 */
  fd = -1;                  /* make sure it doesn't look like a file is open */
  wordsleft = 0;            /* words left in this logical record */

  verbose = 0;
  nowrite = 0;
  binary = 0;
  text = 0;

  /* any args? */

  for (i=1; i<argc; i++) {
    if (strcmp(argv[i],"-vv") == 0)
      verbose = 2;
    else if (strcmp(argv[i],"-v") == 0)
      verbose = 1;
    else if (strcmp(argv[i],"-nw") == 0)
      nowrite = 1;
    else if (strcmp(argv[i],"-binary") == 0)
      binary = 1;
    else if (strcmp(argv[i],"-text") == 0)
      text = 1;
    else if (strcmp(argv[i],"-h") == 0 || strcmp(argv[i],"-help") == 0) {
      fprintf(stdout, "Usage: magrst [-v] [-vv] [-nw] [-binary] [-text]\n");
      fprintf(stdout, "Reads old and new style magrst data from stdin.\n");
      exit(0);
    }
  }

  explogrecno = 1;
  expblockno = 0;
  while (1) {

    /* if too many words were consumed, it's an error; if not enough
       were consumed, skip them now.  This is at the top of the while
       loop so that continues can be used inside the loop to skip stuff. */

    if (wordsleft < 0) {
      fprintf(stderr, "Tape parse error at position %d, recid = %d, wordsleft=%d\n", ftell(stdin), recid, wordsleft);
      exit(1);
    } else if (wordsleft > 0) {
      if (verbose >= 2)
	fprintf(stderr, "WARNING: %d words left unread at position %d\n", wordsleft, ftell(stdin));
      if (fread(buf, wordsleft*2, 1, stdin) != 1) {
	fprintf(stderr, "fread read error at position %d\n", fp);
	exit(1);
      }
    }
    wordsleft = 0;

    /* now we're at some kind of record header, in theory */

    fp = ftell(stdin);       /* might give -1 on some OS's */

    /* read the tape header; for a newer-format (drb) tape, the first
       4 bytes are "LSR " */

    if (fread(buf, 4, 1, stdin) != 1)
      if (feof(stdin)) {
	if (verbose >= 1) fprintf(stderr,"End of file at position %d\n", fp);
	exit(0);
      } else {
	fprintf(stderr, "Error reading header at position %d\n", fp);
	exit(1);
      }
    buf[4]=0;

    /* some drb tapes have ANSI labels; skip them */

    if (strcmp(buf,"VOL1") == 0) {
      drb = 1;
      expblockno++;
      if (verbose >= 1)
	fprintf(stderr,"Skipping %s label record at position %d\n", buf, fp);
      fread(buf,76,1,stdin);

      /* try to find start of VOL2 record.  This is the way we bypass
         the boot program, which immediately follows VOL1 if a drb tape has
         a boot program.  Assumes the string VOL2 doesn't occur in the boot
	 program. */

      strcpy(buf,"VOL2");
      i=0;
      while(i < 4) {
	ch = getchar();
	if (ch == buf[i])
	  i++;
	else
	  i = 0;
      }
    }

    if (strncmp(buf,"VOL",3) == 0 || strncmp(buf,"UVL",3) == 0  || strncmp(buf,"HDR",3) == 0 || strncmp(buf,"UHL",3) == 0 || strncmp(buf,"EOF",3) == 0 || strncmp(buf,"EOV",3) == 0 || strncmp(buf,"UTL",3) == 0 ) {
      drb = 1;
      expblockno++;
      if (verbose >= 1)
	fprintf(stderr,"Skipping %s label record at position %d\n", buf, fp);
      fread(buf,76,1,stdin);
      continue;
    }

    /* "LSR " header might be in either Prime ASCII or standard ASCII */

    if (strcmp(buf,"\314\323\322\240") == 0 || strcmp(buf,"LSR ") == 0) {

      drb = 1;
      if (verbose >= 2)	fprintf(stderr, "drb: POS %d, ", fp);

      i = readshort();
      if (i != 1) {
	fprintf(stderr, "Logical tape version is %d, but expected 1\n", i);
	exit(1);
      }

      i = readshort();
      if (verbose >= 2) fprintf(stderr, "save %d, ", i);

      i = readlong();
      if (verbose >= 2) lsrdtm = ptimestampu(i);

      i = readlong();
      if (verbose >= 2) fprintf(stderr, "block #%d, ", i);
      if (i != expblockno)
	fprintf(stderr," ************ EXPECTED BLOCK %d, READ BLOCK %d\n", expblockno, i);
      expblockno = i+1;

      i = readshort();
      if (verbose >= 2) fprintf(stderr, "size %d, ", i);
      wordsleft = i-10;

      i = readshort();
      if (verbose >= 2) fprintf(stderr, "lrecs %d, ", i);

      if (verbose >= 2) fprintf(stderr, "%s", ctime((time_t *)&lsrdtm));

      /* read the logical record header following LSR, then fall through */

      if (fread(buf, 4, 1, stdin) != 1) {
	fprintf(stderr, "Error reading header following LSR at position %d\n", fp);
	exit(1);
      }
    }

    /* every drb logical record has a 4-byte header:
         byte 1: record id
         byte 2: version
         bytes 3-4: record length, including the 4-byte header
       old magsav logical records have a 3-word header:
         word 1: logical record number in logical tape, starting with 1
	 word 2: record length, including the 3-word header
	 word 3: record id
    */

    /* if this is an old magsav record, get the 3-word header.
       Otherwise it's a drb record with a 2-word header.  Either way, the
       record size is always in word 2 */

    nwords = buf[2]<<8 | buf[3];
    if (nwords > 16*1024) {
      fprintf(stderr, "Record size is %d words at position %d\n", nwords, fp);
      exit(1);
    }

    if (drb) {
      recid = buf[0];
      lrecversion = buf[1];
      if (lrecversion != 1 && lrecversion != 128) {
	fprintf(stderr, "Logical record version is %d, but expected 1 at position %d\n", lrecversion, fp);
	exit(1);
      }
      if (verbose >= 2)
	fprintf(stderr,"drb: recid=%d, lrecversion=%d, nwords=%d\n", recid, lrecversion, nwords);
      wordsleft = nwords-2;
    } else {
      logrecno = buf[0]<<8 | buf[1];
      recid = -readshort();    /* NOTE: make magsav recid's negative! */
bootskipdone:
      wordsleft = nwords-3;
      if (verbose >= 2)
	fprintf(stderr,"magrst: recno %d, %d words, recid %d\n", logrecno, nwords, recid);
      if (logrecno != explogrecno) {
	fprintf(stderr,"********** magrst: expected logrec %d, saw %d\n", explogrecno, logrecno);
	explogrecno = logrecno;
      }
      explogrecno++;
    }

    /* if there is a file open and this isn't a continuation of it, close
       the file now, set the file timestamp */

    if (fd >= 0 && recid != MS_DATA && recid != DRB_FILE_DATA) {
      if (fd > 0)
	close(fd);
      fd = -1;

      /* now go back up the directory stack and set dtm/dta in reverse
	 order */

      for (i = dirlevel; i>= 0; i--) {
	ut.actime = ptimestampu(dirstack[i].dta);
	ut.modtime = ptimestampu(dirstack[i].dtm);
	if (ut.modtime >= 0) {
	  if (verbose >= 2)
	    fprintf(stderr,"Setting timestamp on %s to %d\n", dirstack[i].path, dirstack[i].dtm);
	  if (ut.actime < 0)
	    ut.actime = ut.modtime;
	  if (utime(dirstack[i].path, &ut) == -1) {
	    fprintf(stderr,"Error setting timestamp for %s:", dirstack[i].path);
	    perror(NULL);
	  }
	}
      }
    }

    if (recid == MS_START_LOG_TAPE) {
      fprintf(stderr,"\nStart of logical tape at position %d\n", fp);
      
      /* word 4: tape format */

      tapeformat = readshort(); wordsleft--;
      if (tapeformat == 0x8000)
	fprintf(stderr,"Tape format: pre rev 19\n");
      else if (tapeformat == 0xC000)
	fprintf(stderr,"Tape format: rev 19 w/o ACLs\n");
      else if (tapeformat == 0xE000)
	fprintf(stderr,"Tape format: rev 19 with ACLs\n");
      else if (tapeformat == 0xD000)
	fprintf(stderr,"Tape format: rev 20 w/o ACLs\n");
      else if (tapeformat == 0xF000)
	fprintf(stderr,"Tape format: rev 20 with ACLs\n");
      else if (tapeformat == 0xA000)
	fprintf(stderr,"Tape format: rev 22 w/o ACLs\n");
      else if (tapeformat == 0xB000)
	fprintf(stderr,"Tape format: rev 22 with ACLs\n");
      else
	fprintf(stderr,"Tape format: 0x%x\n", tapeformat);

      /* words 5-7: date as MMDDYY */

      if (fread(buf, 6, 1, stdin) != 1) {
	fprintf(stderr, "error reading date at position %d\n", fp);
	exit(1);
      }
      buf[6] = 0;
      pasciiu(buf,6);
      wordsleft -= 3;
      fprintf(stderr, "Date: %6s\n", buf);

      /* word 8: user version */

      i = readshort(); wordsleft--;
      fprintf(stderr, "User version: %d\n", i);

      /* word 9: reel number */

      reel = readshort(); wordsleft--;       /* reel number */
      fprintf(stderr, "Reel: %d\n", reel);

      /* words 10-12: tape name */

      if (fread(buf, 6, 1, stdin) != 1) {
	fprintf(stderr, "error reading tape name at position %d\n", fp);
	exit(1);
      }
      buf[6] = 0;
      wordsleft -= 3;
      pasciiu(buf,6);
      fprintf(stderr, "Tape name: %6s\n", buf);

      /* the "start logical tape" record length doesn't include the size
	 of the boot program, if present.  Magsav adds the boot program
	 to the first record on reel 1 of a backup.  AFAIK, it doesn't
	 write the boot record on reel 2 of a continued backup.  At rev
	 19 and 20, it appears to add the boot record to every logical
	 tape header, even if it isn't the first one on the tape.

         If the boot program is present, the first record will be 1024
	 bytes for rev 19 and earlier tapes.  Later revs of magsav
	 store much longer boots - for example, rev 20.2 stores over
	 4K bytes in the first tape record.  Without record markers, it's
	 not possible to tell how long the physical tape record is, so
	 we need to add some code to heuristically determine the
	 length of the boot program by looking for logical record 1's
	 header in the data stream.
      */

      if (reel == 1) {
	if (verbose >= 1)
	  fprintf(stderr, "Skipping boot at BOT, nwords=%d...\n", nwords);
	
	/* the first record is at least 1024 bytes (512 words).  The
	   start logical tape header size (nwords) doesn't include the
	   boot, so skip 512-nwords first */

#if 0
	if (fread(buf, 512-nwords, 2, stdin) != 512-nwords) {
	  perror("Error skipping boot");
	  exit(1);
	}
#endif
	wordsleft = 4096;

	/* look for logical record 1, record length n (usually 27),
	   and record id MS_NAME (remember, we negate old magsav
	   recid's to distinguish them from drb */

	while (1) {
	  bootskiprecno = readshort(); wordsleft--;
	  if (bootskiprecno != 1)
	    continue;
skipcont:
	  bootskipreclen = readshort(); wordsleft--;
	  if (bootskipreclen < 1)      /* not part of header */
	    continue;

	  /* rec lengths must be 3 words or greater, but if it's
	     a 1, it might be the recno, so backup */

	  if (bootskipreclen == 1) {
	    bootskiprecno = bootskipreclen;
	    goto skipcont;
	  }
#if 1
	  /* might want to add this later as an additional check;
	     for example, a reclen of 4 would cause this loop to
	     exit, but it's probably not logical record 1 because
	     MS_NAME records have more data than this */

	  if (bootskipreclen < 27 || bootskipreclen > 100)
	    continue;
#endif
	  bootskiprecid = readshort(); wordsleft--;
	  if (-bootskiprecid == MS_NAME) {
	    explogrecno = 1;
	    logrecno = 1;
	    recid = MS_NAME;
	    nwords = bootskipreclen;
	    goto bootskipdone;
	  }

	  /* this word might be the start of logical record 1 */

	  if (bootskiprecid == 1) {
	    bootskiprecno = bootskiprecid;
	    goto skipcont;
	  }
	}
	fprintf(stderr,"Unable to find logical reccord 1\n");
	exit(1);
      }

    } else if (recid == MS_NAME || recid == DRB_START_OBJ) {

      skipping = 0;
      insegdir = 0;
      if (recid == DRB_START_OBJ) {

	/* drb stores all possible attributes in the same record layout,
	   even though most attributes only apply to certain object types */

	objtype = readshort();
	if (verbose >= 2)
	  fprintf(stderr,"Start object type %d at position %d\n", objtype, fp);
	i = readlong();     /* dtm */
	if (objtype == DRB_DIR_OBJ || objtype == DRB_FILE_OBJ || objtype == DRB_SEGDIR_OBJ) 
	  dtm = i;
	if (objtype == DRB_SEG_SUBFILE_OBJ || objtype == DRB_SEG_SUBDIR_OBJ)
	  insegdir = 1;
	i = readlong();     /* dtc */
	i = readlong();     /* dta */
	if (objtype == DRB_DIR_OBJ || objtype == DRB_FILE_OBJ || objtype == DRB_SEGDIR_OBJ) 
	  dta = i;
	i = readshort();    /* file protection */
	i = readlong();     /* segdir entry class */
	i = readshort();    /* file object file type: 1=SAM, 2=DAM, 3=CAM */
	filetype = 0;       /* assume it's a regular SAM file for now */
	i = readshort();    /* CAM extent info */
	i = readlong();     /* max quota (dir) */
	if (fread(buf, 12, 1, stdin) != 1) {   /* owner & non-owner pass */
	  fprintf(stderr, "error reading owner/non-owner at %d\n", fp);
	  exit(1);
	}
	i = readshort();    /* dir type, 1=PW, 2=ACL (dir) */
	if (objtype == DRB_DIR_OBJ)   /* use filesystem & old magsav types */
	  if (i == 1)
	    filetype = 4;
	  else if (i == 2)
	    filetype = 5;
	  else {
	    fprintf(stderr, "Skipping drb dir type %d at position %d\n", i, fp);
	    skipping = 1;
	  }
	i = readshort();    /* file bits (rwlock, etc.) */
	i = readshort();    /* roam segdir type (roam) */
	i = readshort();    /* regular segdir type, 1=SAM, 2=DAM (segdir) */
	if (objtype == DRB_SEGDIR_OBJ)
	  if (i == 1)
	    filetype = 2;
	  else if (i == 2)
	    filetype = 3;
	  else {
	    fprintf(stderr, "Skipping drb segdir type %d at position %d\n", i, fp);
	    skipping = 1;
	  }

	maxentries = readlong();     /* max entries (segdir) */
	i = readshort();    /* dummy */
	i = readshort();    /* object level */
	i = readshort();    /* 0=regular, 1=roam */
	i = readshort();    /* seg level: 1=subfile, 2=file under seg subdir */
	i = readshort();    /* object name length in bytes */
	if (i < 1 || i > 32) {
	  fprintf(stderr, "object length = %d at %d\n", i, fp);
	  exit(1);
	}
	if (fread(buf, 32, 1, stdin) != 1) {   /* object name */
	  fprintf(stderr, "error reading object name at %d\n", fp);
	  exit(1);
	}
	buf[i] = 0;
	i = readshort();     /* object pathname length (bytes) */
	wordsleft -= 48;
	if (fread(path, (i+1)/2*2, 1, stdin) != 1) {   /* object name */
	  fprintf(stderr, "error reading object name at %d\n", fp);
	  exit(1);
	}
	wordsleft -= (i+1)/2;
	path[i] = 0;

      } else if (recid == MS_NAME) {
	p = path;            /* pointer to accumulate pathname */
	while (wordsleft > 0) {

#if 0
	  if (wordsleft % 24 != 0) {
	    fprintf(stderr, "name parse error, position %d, wordsleft=%d\n", fp, wordsleft);
	    exit(1);
	  }
#endif

	  /* for a segdir, entries look like this:

	     word 1: type of subfile (parent or this file?)
	     word 2: zero
	     word 3: segdir subfile entry number
	     word 4: protection (?)
	     words 5-19: unused
	     word 20: subfile type (parent or this file?)
	     words 21-24: unused

	     NOTE: Segment directories can be nested.  Need a test tape
	     to implement the restore for these.

	     For non-segdirs, entries look like this:

	     word 1: entry control word (right byte = length in words)
	     words 2-17: filename
	     word 18: owner/non-owner protection bits
	     word 19: acl protection; bit 1 set if non-default ACL protection
	     word 20: file type:
	       left byte:
		 :100000 = special file (BOOT/DSKRAT)
		 :040000 = clear if file has been changed since last backup
		 :020000 = set if modified by Primos II (timestamp is inaccurate)
		 :010000 = set for special BOOT, MFD, BADSPT, and DSKRAT files
		 bits 5-6 are the file's read/write lock:
		 00 = use system rwlock (dflt)
		 01 = N readers or 1 writer (excl)
		 10 = N readers and 1 writer (updt)
		 11 = N readers and N writers (none)
		 NOTE: UFD's don't have rwlocks; segdirs do.
		 Segment subfiles have the same rwlock as the (top-level?) segdir
	       right byte:
		 0 = SAM (sequential) file
		 1 = DAM (direct access) file
		 2 = SEGSAM (sequential segment directory)
		 3 = SEGDAM (direct seqment directory)
		 4 = password directory
		 5 = ACL directory
		 6 = category ACL
		 7 = CAM (contiguous) file
	  */

	  /* ecw */

	  i = readshort(); wordsleft--;
	  if (i & 0xFF < 24) {
	    fprintf(stderr, "ecw = %d/%d (size != 24) at position %d\n", i>>8, i&0xff, fp);
	    exit(1);
	  }
	  ecwskip = (i & 0xFF) - 22;
	  ecwskip = 2;   /* only skip words 23 & 24? */

	  /* words 2-17: filename (regular entry) */

	  if (fread(p, 32, 1, stdin) != 1) {
	    fprintf(stderr, "error reading file name at position %d\n", fp);
	    exit(1);
	  }
	  wordsleft -= 16;

	  /* for segment subfiles (word 2 is zero), add the entry number */

	  if (*p == 0 && *(p+1) == 0) {
	    segentry = *(p+2)<<8 | *(p+3);
	    sprintf(p, "%d/", segentry);
	    p = path + strlen(path);
	    insegdir = 1;
	  } else {
	    for (i=0; i<32; i++) {          /* find the end of the filename */
	      if (*p == 0240) {             /* space w/parity */
		break;
	      }
	      p++;
	    }
	    *p++ = '>';
	    *p = 0;
	  }

	  /* word 18: owner/non-owner protection bits */

	  i = readshort(); wordsleft--;

	  /* word 19: acl protection; bit 1 set if non-default ACL */

	  i = readshort(); wordsleft--;

	  filetype = readshort(); wordsleft--;
	  filetype &= 0xff;

	  /* word 21-22: date/time modified */

	  dta = 0;
	  dtm = readlong(); wordsleft -= 2;

	  /* words 23 to (ecw length) are dummy */

	  while (ecwskip > 0) {
	    i = readshort(); 
	    wordsleft--;
	    ecwskip--;
	  }
	}
	*(--p) = 0;
      }

      /* pathname postprocessing: strip parity, change > to /, change / to S */

      for (p=path; *p; p++) {
	*p = *p & 0x7f;
	if ('A' <= *p && *p <= 'Z')   /* lowercase the name for Unix */
	  *p = *p+('a'-'A');
	if (*p == '/')
	  *p = 'S';
	if (*p == '>')
	  *p = '/';
      }

      /* if path starts with <DISKNAME>BLAH, strip the leading < */

      if (path[0] == '<')
	strcpy(path,path+1);

      fprintf(stderr,"%s\n", path);
      if (fd > 0) {
	fprintf(stderr,"fd should be zero or -1??\n");
	exit(1);
      }

	/* create the parent directories */

      if (nowrite == 0) {
	dirlevel = -1;
	for (p=path; *p != 0; p++)
	  if (*p == '/') {
	    *p = 0;
	    if (mkdir(path, 0755) == -1 && errno != EEXIST) {
	      fprintf(stderr,"Creating directory %s\n", path);
	      perror("  Error is");
	      exit(1);
	    }
	    dirlevel++;
	    strcpy(dirstack[dirlevel].path, path);
	    *p = '/';
	  }

	dirlevel++;
	strcpy(dirstack[dirlevel].path, path);
	dirstack[dirlevel].dtm = dtm;
	dirstack[dirlevel].dta = dta;
	//fprintf(stderr,"Saved dtm for %s as %d\n", dirstack[dirlevel].path, dirstack[dirlevel].dtm);
      }

    } else if (recid == MS_DATA && (filetype == 4 || filetype == 5)) {

      /* Magsav data record following a password or ACL directory:
         word 1: always 8
	 words 2-4: owner password
	 words 5-7: non-owner password
	 word 8: always zero
	 word 9: zero for pre-quota saves (and last word)
	 words 9-10: "QT$$" for quota saves
	 word 11: 0 if quota directory, 1 if non-quota
	 words 12-19: q$read return array; word 14 is max quota
      */

      i = readshort(); wordsleft--;
      if (i != 8) {
	fprintf(stderr,"Expected 8 but got %d for ufd at position %d\n", i, fp);
	exit(1);
      }

    } else if (recid == MS_DATA && (filetype == 2 || filetype == 3)) {

      /* Magsav data record following a segment directory:
	 word 1: maximum entries in the segdir */

      maxentries = readshort(); wordsleft--;
#if 0
      fprintf(stderr,"Segdir %s, maxentries=%d\n", path, maxentries);
#endif

      /* below occurs when reel 2 starts with file data and is restored alone */

    } else if ((recid == MS_DATA || recid == DRB_FILE_DATA) && filetype == -1) {
      fprintf(stderr,"Skipping file data at position %d\n", fp);

    } else if ((recid == MS_DATA || recid == DRB_FILE_DATA) && (filetype == 0 || filetype == 1)) {   /* SAM or DAM data */

      if (drb) {               /* read file data size in bytes */
	i = readlong(); wordsleft -= 2;
	if (verbose >= 2)
	  fprintf(stderr, "File data size = %d bytes\n", i);
      }

      if (fread(buf, wordsleft*2, 1, stdin) != 1) {
	fprintf(stderr, "error reading file data at position %d\n", fp);
	exit(1);
      }
      if (nowrite == 0) {
	if (fd < 0) {         /* first data record; open output file */
	  textstate = 0;
	  if (!binary && !insegdir && isptext(path,filetype,buf,wordsleft*2))
	    textfile = 1;
	  else
	    textfile = 0;

	  /* open the file for writing (should check for overwrite) */

	  fd = creat(path, 0644);
	  if (fd == -1) {
	    fprintf(stderr,"Opening file %s\n", path);
	    perror("  Error is");
	  }
	  if (!textfile && text) {  /* don't restore binary files */
	    close(fd);
	    fd = 0;
	  }
	}
	if (textfile)
	  nwritten = convtext(fd, buf, 2*wordsleft, &textstate);
	else if (fd > 0) {
	  nwritten = write(fd, buf, 2*wordsleft);
	  if (nwritten != 2*wordsleft) {
	    fprintf(stderr,"Writing file %s\n", path);
	    perror("  Error is");
	  }
	}
      }
      wordsleft = 0;

    } else if (recid == DRB_FILE_DATA) {
      i = readlong(); wordsleft -= 2;
      fprintf(stderr,"Unrecognized drb filetype = %d ignored\n", filetype);

    } else if (recid == DRB_END_OBJ) {
      i = readshort(); wordsleft--;
      if (i == 0x8000)
	fprintf(stderr, "WARNING: File may be incomplete (open?), status = 0x%04x\n", i);
      else if (i & 0xF800) {
	fprintf(stderr, "WARNING: File save status = 0x%04x\n", i);
      }

    } else if (recid == MS_END_LOG_TAPE || recid == DRB_END_LOG_TAPE) {
      fprintf(stderr,"End of logical tape at position %d\n", fp);
#if 0
      exit(0);
#endif
    }
  }
}
