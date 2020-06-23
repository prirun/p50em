/* Show approximate minimum clock resolution
   Derived from the example in the linux man page for clock_gettime() */

#define _XOPEN_SOURCE 600
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>

static void displayClock(clockid_t clock, char *name)
{
   struct timespec ts;
   time_t oldsec;
   long oldnsec;

   clock_gettime(clock, &ts);
   oldsec = ts.tv_sec;
   oldnsec = ts.tv_nsec;
   while (ts.tv_sec == oldsec && ts.tv_nsec == oldnsec)
   {
      if (clock_gettime(clock, &ts) == -1) {
         perror("clock_gettime");
         exit(EXIT_FAILURE);
      }
   }
   printf("%-15s: %10ld sec %09ld nsec\n", name,
      (long) ts.tv_sec - oldsec, ts.tv_nsec - oldnsec);
}

int main(int argc, char *argv[])
{
   displayClock(CLOCK_MONOTONIC_RAW, "CLOCK_MONOTONIC_RAW");
   exit(0);
}
