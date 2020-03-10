/* show size of C integers */

#include <stdio.h>

main() {
  int i;
  short s;
  long l;
  long long ll;

  printf("size of int is %d\n", sizeof(i));
  printf("size of short is %d\n", sizeof(s));
  printf("size of long is %d\n", sizeof(l));
  printf("size of long long is %d\n", sizeof(ll));
}
