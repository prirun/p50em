#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* this function decides if this is a Prime text file or binary file
   by checking the contents of the first tape buffer.  The conditions
   for a text file are:

   - it can't be inside a segdir (checked before calling here)
   - it has to be a SAM or DAM file - COMO files are DAM files :(
   - it has to have at least 1 newline in the first buffer, usually
     4090+ bytes; text files with an initial line longer than this have
     to be converted after the restore using ptextu
   - lines have to start on word boundaries, with zero padding after nl
     (NOTE: como files have space padding, so allow that too)
   - only text characters, nl, ff, :001 at the beginning of a line,
     :221 (compression) and :211 (tab) are allowed in text files
*/

/* max length of extension including . and ending null byte */

#define MAXEXTENSION 10

static struct {
  char ext[MAXEXTENSION];
  int  ftype;
} exttype[] = {
  {".basic", 1},
  {".cbl", 1},
  {".c", 1},
  {".cc", 1},
  {".ci", 1},
  {".cobol", 1},
  {".comi", 1},
  {".como", 1},
  {".cpl", 1},
  {".f77", 1},
  {".ftn", 1},
  {".ibas", 1},
  {".ins", 1},
  {".list", 1},
  {".map", 1},
  {".mod", 1},
  {".pascal", 1},
  {".plp", 1},
  {".pl1", 1},
  {".pl1g", 1},
  {".pma", 1},
  {".rpg", 1},
  {".runi", 1},
  {".runo", 1},
  {".spl", 1},
  {".sr", 1},
  {".vrpg", 1},

  {".bin", 0},
  {".dl", 0}, 
  {".save", 0}, 
};
#define EXTENTRIES sizeof(exttype)/sizeof(exttype[0])

int isptext(char *path, int filetype, unsigned char *buf, int len) {
  int i, hasnl, skipline;
  unsigned char ch;
  unsigned char extension[MAXEXTENSION];

  /* scan backward to get file extension */

  extension[0] = 0;
  for (i=strlen(path)-1; i >= 0; i--)
    if (path[i] == '.') {
      strncpy(extension, path+i, sizeof(extension)-1);
      break;
    }
  if (extension[0] == '.') 
    for (i=0; i < EXTENTRIES; i++)
      if (strcasecmp(extension, exttype[i].ext) == 0)
	return exttype[i].ftype;

  if (filetype == 0 || (filetype == 1 && strcasecmp(extension,".como") == 0))
    ;
  else {
#ifdef DEBUG
    fprintf(stderr, "Filetype %d can't be a text file\n", filetype);
#endif
    return 0;
  }
    
  hasnl = 0;
  skipline = 0;
  for (i=0; i<len; i++) {
    ch = buf[i];
    if (ch == 0212) {
      skipline = 0;
      hasnl = 1;
      if ((i&1) == 0)         /* nl is in the left byte */
	if (buf[++i] != 0 && buf[i] != 0240) 
	  return 0;           /* unusual padding = not a text file */
    } else if (skipline)      /* skipping this line? */
      continue;
    else if (ch == 0221)      /* space compression */
      i++;                    /* skip the compression count */
    else if (ch == 0001)      /* spooler pagination control lines; skip 'em */
      skipline = 1;
    else if ((ch & 0x7f) == 014 || ch == 0211 || (0240 <= ch /* && ch <= 0377 */))
      ;
    else {
#ifdef DEBUG
      fprintf(stderr,"Character %o at position %d kept this from being a text file.\n", ch, i);
#endif
      return 0;
    }
  }
  return hasnl;     /* buffer has to contain a newline to be a text file */
}


/* writes a buffer of Prime text, converting it to Unix text.  The
   2-character space compression sequences may cross buffers; "state"
   is used to track this:
   
   state=0 means no compression pending
   state=1 means the next buffer character is the compression count

   Before calling convtext for a new file, state must be initialized
   to zero in the caller, then left alone after that.
 */

int ptextu(int fd, unsigned char *buf, int len, int *state) {
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
      if (fd != -1)
	if (write(fd, obuf, n) != n) {
	  fprintf(stderr,"File write error text conversion, n=%d\n", n);
	  exit(1);
	}
      n = 0;
    }
  }
  if (n > 0 && fd != -1 && write(fd, obuf, n) != n) {
    fprintf(stderr,"File write error text conversion, n=%d\n", n);
    exit(1);
  }
  return len;
}

/* scans buffer contents to see if it qualifies as a Unix text file that
   should be converted to Prime text.  Text files can only have:
   - printable ASCII characters
   - tab
   - newline
   - carriage return
   - form feed
*/

int isutext(char *path, unsigned char *buf, int len) {
  int i, hasnl, skipline;
  unsigned char ch;
  unsigned char extension[MAXEXTENSION];

  /* scan backward to get file extension */

  extension[0] = 0;
  for (i=strlen(path)-1; i >= 0; i--)
    if (path[i] == '.') {
      strncpy(extension, path+i, sizeof(extension)-1);
      break;
    }
  if (extension[0] == '.') 
    for (i=0; i < EXTENTRIES; i++)
      if (strcasecmp(extension, exttype[i].ext) == 0)
	return exttype[i].ftype;

  /* extension didn't determine type; check file contents */

  hasnl = 0;
  for (i=0; i<len; i++) {
    ch = buf[i];
    if (ch == '\n') {
      skipline = 0;
      hasnl = 1;
    } else if (skipline)      /* skipping this line? */
      continue;
    else if (ch == '\f' || ch == '\t' || ch == '\r' || (040 <= ch && ch <= 0177 ))
      ;
    else {
#ifdef DEBUG
      fprintf(stderr,"Character %o at position %d kept this from being a text file.\n", ch, i);
#endif
      return 0;
    }
  }
  return hasnl;     /* buffer has to contain a newline to be a text file */
}


#if 0
/* writes a buffer of Unix or Windows text, converting it to Prime
   uncompressed text.  

   The state argument is a structure that needs to be initialized
   before each new file:

      state->oddbyte = 0;
      state->col = 0;
      state->spaces = 0;

   It's possible that a string of spaces could be lost at the end of
   the file, but this is very unlikely since text files are supposed
   to end with a newline.
 */

int utextp(unsigned char *buf, int len, utextp_t *state) {
  int i, n, nsp;
  unsigned char ch;

  n = 0;                       /* next output buffer position */
  for (i=0; i<len; i++) {
    ch = buf[i];
    if (ch == ' ')
      state->spaces++;
    else if (ch == '\t') {
      nsp = 8 - (state->col & 7);
      state->spaces += nsp;
      state->col += nsp;
    } else {
      while (state->spaces) {        /* dump held-up spaces for non-space */
	if (state->spaces < 3) {
	  state->obuf[n++] = 0240;
	  state->spaces--;
	  state->oddbyte = ~state->oddbyte;
	} else {
	  nsp = state->spaces;
	  if (nsp > 255)       /* can only handle 255 at once! */
	    nsp = 255;
	  state->obuf[n++] = 0221;
	  state->obuf[n++] = nsp;
	  state->spaces = state->spaces - nsp;
	}
      }
      if (ch == '\r')           /* ignore carriage returns (Windoze) */
	continue;
      state->obuf[n++] = ch | 0x80;
      if (ch == 0212 && !state->oddbyte)
	state->obuf[n++] = 0;         /* pad line to a word boundary */
      else
	state->oddbyte = ~state->oddbyte;
    }
    if (n >= OBUFMAX) {
      if (fd != -1)
	if (write(fd, state->obuf, n) != n) {
	  fprintf(stderr,"File write error text conversion, n=%d\n", n);
	  exit(1);
	}
      n = 0;
    }
  }
  if (n > 0 && fd != -1 && write(fd, state->obuf, n) != n) {
    fprintf(stderr,"File write error text conversion, n=%d\n", n);
    exit(1);
  }
  return len;
}
#endif

/* converts a fixed-length string in place from Prime to regular ascii */

void pasciiu(char *p, int len) {
  int i;

  for (i=0; i<len; i++)
    p[i] &= 0x7f;
}


/* converts a fixed-length string in place from regular to Prime ascii */

void uasciip(char *p, int len) {
  int i;

  for (i=0; i<len; i++)
    p[i] |= 0x80;
}
