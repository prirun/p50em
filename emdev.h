/* Device '4: system console

   OCP '0004 = initialize for input only, echoplex, 110 baud, 8-bit, no parity
   OCP '0104 = same, but for output only
   OCP '1004 = Full duplex; software must echo
   OCP '1204 = Prime normal, independent xmit and recv w/echoplex
   OCP '1304 = Self test mode (internally connects transmitter to receiver)
   OCP '1704 = same as above, but clears interrupt masks and dma/c enables


*/

#include <fcntl.h>

void devasr (unsigned short class, unsigned short func) {

  static int ttydev=-1;
  unsigned char ch;
  int n;

  if (ttydev < 0) {
    ttydev = open("/dev/tty", O_RDWR+O_NONBLOCK, 0);
    if (ttydev < 0) {
      perror(" error opening /dev/tty");
      exit(1);
    }
  }

  switch (class) {

  case 0:
    fprintf(stderr," OCP '%o04\n", func);
    break;

  case 1:
    fprintf(stderr," SKS\n");
    if (func <= 7)
      mem[P]++;                     /* assume it's always ready */
    else {
      fprintf(stderr," unimplemented SKS '04 function\n");
      exit(1);
    }
    break;

  case 2:
    fprintf(stderr," INA\n");
    if (func == 0 || func == 010) {
      usleep(100000);
      n = read(ttydev, &ch, 1);
      if (n < 0) {
	if (errno != EAGAIN) {
	  perror(" error reading from tty");
	  exit(1);
	}
      } else if (n == 1) {
	mem[A] = ch | 0x80;
	mem[P]++;
      } else  if (n != 0) {
	fprintf(stderr," unexpected error reading from tty, n=%d", n);
	exit(1);
      }
    } else if (func == 012) {    /* no idea what this does... */
      mem[P]++;
    } else {
      fprintf(stderr," unimplemented INA '04 function\n");
      exit(1);
    }
    break;

  case 3:
    fprintf(stderr," OTA\n");
    if (func == 0 | func == 1) {
      fprintf(stderr," char to write=%o\n", mem[A]);
      putchar(mem[A] & 0x7f);
      fflush(stdout);
      mem[P]++;                     /* OTA '0004 always works on Unix */
    } else {
      fprintf(stderr," unimplemented OTA '04 function\n");
      exit(1);
    }
    break;
  }
}

