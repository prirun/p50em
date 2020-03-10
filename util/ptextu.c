/* ptextu.c, J. Wilcoxson, March 16, 2005
   Reads a Prime text file from stdin and writes a Unix text file to stdout.

Prime text files have 3 unique features:

- the text characters all have the high bit set, including newline

- text files may be compressed or uncompressed - compressed is
typical.  Compressed files represent runs of 3-255 spaces as a 2-byte
sequence: 0221 (control-q w/parity) followed by the number of actual
spaces in the next byte.  Uncompressed files are typically used in
record-oriented programs, where individual records may need to be
randomly updated or positioned.  Compressed text files can't be
updated in general, other than to append or truncate, because the line
length may change if the number of spaces changes.  When the Prime
compressed file input routines are used, blanks are expanded.  Any
Prime program that handles compressed text files will also work with
uncompressed text files (just slower), but programs expecting
uncompressed files won't work with compressed files since blank
expansion doesn't happen.

- all text file lines must start on a 16-bit word boundary.  To
enforce this restriction, when a newline character occurs in the left
byte of a 16-bit word, ie, it's in an odd file position (with counting
starting at 1), it is followed by a dummy character, typically a zero,
that isn't part of the text file.  This null has to be skipped for
Unix since Unix treats every byte as part of the text file.

Notes:

- if the Prime text file ends with a compression character, that
character is not written to the output file.  This is an invalid Prime
text file.

Here is an octal dump of a Prime text file.  Lines 1, 2, and 4 needed
padding after the newline, but not line 3 or 5.  Line 6 begins with the
compression character sequence, representing 3 spaces in 2 characters,
and has the same sequence later in the line.

0000000  354 351 363 364 346 272 240 360 362 357 343 273 212  \0 212  \0
0000020  245 351 356 343 354 365 344 345 240 247 363 371 363 343 357 355
0000040  276 353 345 371 363 256 351 356 363 256 360 354 261 247 273 212
0000060  212  \0 344 343 354 212 221 003 345 362 362 360 362 244 221 003
0000100  345 356 364 362 371 240 250 342 351 356 254 240 342 351 356 254
0000120  240 343 350 341 362 250 252 251 254 240 342 351 356 254 240 343
0000140  350 341 362 250 252 251 254 240 342 351 356 251 254 212 221 003
0000160  354 351 363 364 346 364 221 003 345 356 364 362 371 240 357 360
0000200  364 351 357 356 363 240 250 366 341 362 351 341 342 354 345 251
0000220  254 212 221 003 364 356 357 365 341 221 004 345 356 364 362 371
0000240  240 250 343 350 341 362 250 252 251 254 240 342 351 356 251 273
0000260  212  \0 212  \0 344 343 354 212 221 003 261 240 342 365 346 254
0000300  212  \0 221 006 262 240 354 345 356 240 342 351 356 254 212  \0
0000320  221 006 262 240 363 364 362 240 343 350 341 362 250 265 260 260
0000340  260 251 254 212 221 003 360 357 363 221 003 342 351 356 250 263
0000360  261 251 254 212 221 003 343 357 344 345 240 240 342 351 356 273
0000400  212  \0 212  \0 221 003 360 357 363 240 275 240 260 273 212  \0
0000420  221 003 344 357 240 365 356 364 351 354 240 250 342 365 346 256
0000440  354 345 356 240 276 240 260 251 273 212 221 006 343 341 354 354
0000460  240 354 351 363 364 346 364 240 250 342 365 346 254 240 262 265
0000500  260 260 254 240 360 357 363 254 240 343 357 344 345 251 273 212
0000520  221 006 351 346 240 343 357 344 345 240 336 275 240 260 240 364
0000540  350 345 356 212 221  \t 343 341 354 354 240 345 362 362 360 362
0000560  244 240 250 353 244 356 362 364 356 254 240 343 357 344 345 254
0000600  240 247 247 254 240 260 254 240 247 314 311 323 324 306 247 254
0000620  240 265 251 273 212  \0 221 006 343 341 354 354 240 364 356 357
0000640  365 341 240 250 342 365 346 256 363 364 362 254 240 341 342 363
0000660  240 250 342 365 346 256 354 345 356 251 251 273 212  \0 221 006
0000700  345 356 344 273 212  \0 212  \0 345 356 344 273 212  \0        
0000716

---  Here is the text version: ---
listf: proc;

%include 'syscom>keys.ins.pl1';

dcl
   errpr$   entry (bin, bin, char(*), bin, char(*), bin),
   listft   entry options (variable),
   tnoua    entry (char(*), bin);

dcl
   1 buf,
      2 len bin,
      2 str char(5000),
   pos   bin(31),
   code  bin;

   pos = 0;
   do until (buf.len > 0);
      call listft (buf, 2500, pos, code);
      if code ^= 0 then
         call errpr$ (k$nrtn, code, '', 0, 'LISTF', 5);
      call tnoua (buf.str, abs (buf.len));
      end;

end;
--- End of Unix text file ---

*/

#include <stdio.h>

main () {
  int n,ch;

  n = 0;
  while ((ch=getchar()) != EOF) {
    n++;
    if (ch == 0221) {
      n++;
      if ((ch=getchar()) != EOF)  /* avoid adding tons of blanks here */
	while (ch--)
	  putchar(' ');
    } else {
      ch &= 0x7f;                 /* turn off high bit for Unix text */
      putchar(ch);
      if (ch == '\n' && n&1) {    /* skip pad byte following newline */
	getchar();
	n++;
      }
    }
  }
}
