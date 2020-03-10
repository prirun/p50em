/* utextp.c, Jim Wilcoxson, March 16, 2005
   Reads a Unix text file and converts it to a compressed Prime text file. 
   This means:
   - turn high bit on
   - expand Unix tabs to spaces
   - change multiple spaces to rcp (Prime text compression)
   - ignore carriage returns (Windoze)
   - ensure each lines begins on a 16-bit word boundary
*/

#include <stdio.h>

main () {
  int n,i,ch,col,space,nsp;

  n = 0;
  col = 0;
  space = 0;
  while ((ch=getchar()) != EOF) {
    if (ch == '\t') {         /* expand Unix tabs */
      nsp = 8 - (col & 7);
      space += nsp;
      col += nsp;
    } else if (ch == ' ') {
      space++;
      col++;
    } else if (ch == '\r')
      ;
    else {                    /* not a space character */
      while (space) {         /* dump held-back spaces first */
	if (space < 3) {      /* write regular spaces */
	  putchar(' ');
	  space--;
	  n++;
	} else {              /* write compressed spaces */
	  putchar(0221);
	  if (space > 255)    /* limited to 255 spaces per compression */
	    nsp = 255;
	  else
	    nsp = space;
	  putchar(nsp);
	  n = n+2;
	  space = space - nsp;
	}
      }
      putchar(ch | 0x80);     /* write the text char w/parity */
      col++;
      n++;
      if (ch == '\n') {
	col = 0;
	if (n&1) { /* Prime lines must start on a 16-bit word boundary */
	  putchar(0);
	  n++;
	}
      }
    }
  }
}
