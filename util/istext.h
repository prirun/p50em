/* istext.h, Jim Wilcoxson, April 5, 2007
   Text conversion routines for the Prime emulator.
*/

#define OBUFMAX 4096

typedef struct {
  int oddbyte;           /* true if next char written is in right byte */
  int col;               /* column # of next byte to be written, 0-based */
  int spaces;            /* # of held-back spaces */
  unsigned char obuf[OBUFMAX];
} utextp_t;
