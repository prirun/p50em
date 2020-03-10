/* strip8.c, J. Wilcoxson, March 16, 2005
   Reads a Prime data file and strips the high bit to make text readable.

   NOTE: this program is not useful for production data, because Prime
   text files have a specific format.  Use prtoux to convert Prime text
   files to Unix text files.
*/

#include <stdio.h>

main () {
  int ch;

  while ((ch = getchar()) != EOF)
    putchar(ch & 0x7f);
}
