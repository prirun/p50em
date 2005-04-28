/* emdev.h, Jim Wilcoxson, April 17, 2005
   Device handlers for pio instructions.  Use devnull as a template for
   new handlers.
   NOTES:

   OCP instructions never skip
   SKS instructions skip on specified conditions
   INA/OTA instructions skip if they succeed (data was read/written)
*/

#include <fcntl.h>

void devnull (short class, short func, short device) {

  switch (class) {

  case 0:
    fprintf(stderr," OCP '%2#0o%2#0o\n", func, device);
    if (func == 0) {
      ;
    } else {
      fprintf(stderr," unimplemented OCP device %2#0o function\n", device);
      exit(1);
    }
    break;

  case 1:
    fprintf(stderr," SKS '%2#0o%2#0o\n", func, device);
    if (func == 0)
      mem[P]++;                     /* assume it's always ready */
    else {
      fprintf(stderr," unimplemented SKS device %2#0o function\n", device);
      exit(1);
    }
    break;

  case 2:
    fprintf(stderr," INA '%2#0o%2#0o\n", func, device);
    if (func == 0) {
      ;
    } else {
      fprintf(stderr," unimplemented INA device %2#0o function\n", device);
      exit(1);
    }
    break;

  case 3:
    fprintf(stderr," OTA '%2#0o%2#0o\n", func, device);
    if (func == 0 | func == 1) {
      mem[P]++;                     /* OTA '0004 always works on Unix */
    } else {
      fprintf(stderr," unimplemented OTA device %2#0o function\n", device);
      exit(1);
    }
    break;
  }
}


/* Device '4: system console

   OCP '0004 = initialize for input only, echoplex, 110 baud, 8-bit, no parity
   OCP '0104 = same, but for output only
   OCP '1004 = Full duplex; software must echo
   OCP '1204 = Prime normal, independent xmit and recv w/echoplex
   OCP '1304 = Self test mode (internally connects transmitter to receiver)
   OCP '1704 = same as above, but clears interrupt masks and dma/c enables


*/

void devasr (short class, short func, short device) {

  static int ttydev=-1;
  unsigned char ch;
  int ttyflags, newflags;
  int n;

  if (ttydev < 0) {
    ttydev = open("/dev/tty", O_RDWR, 0);
    if (ttydev < 0) {
      perror(" error opening /dev/tty");
      exit(1);
    }
  }

  switch (class) {

  case 0:
    fprintf(stderr," OCP '%2#0o%2#0o\n", func, device);
    break;

  case 1:
    fprintf(stderr," SKS '%2#0o%2#0o\n", func, device);
    if (func <= 7)
      mem[P]++;                     /* assume it's always ready */
    else {
      fprintf(stderr," unimplemented SKS '04 function\n");
      exit(1);
    }
    break;

  case 2:
    fprintf(stderr," INA '%2#0o%2#0o\n", func, device);
    if (func == 0 || func == 010) {
      if (fcntl(ttydev, F_GETFL, ttyflags) == -1) {
	perror(" unable to get tty flags");
	exit(1);
      }
      if (mem[mem[P]] == 03776)       /* JMP *-1 -> blocking read */
	newflags = ttyflags & ~O_NONBLOCK;
      else
	newflags = ttyflags | O_NONBLOCK;
      if (fcntl(ttydev, F_SETFL, newflags) == -1) {
	perror(" unable to set tty flags");
	exit(1);
      }
      n = read(ttydev, &ch, 1);
      if (n < 0) {
	if (errno != EAGAIN) {
	  perror(" error reading from tty");
	  exit(1);
	}
      } else if (n == 1) {
	if (func >= 010)
	  mem[A] = 0;
	mem[A] = mem[A] | ch | 0x80;
	mem[P]++;
      } else  if (n != 0) {
	fprintf(stderr," unexpected error reading from tty, n=%d", n);
	exit(1);
      }
    } else if (func == 011) {    /* read device id? */
      mem[A] = 4;
      mem[P]++;
    } else if (func == 012) {    /* read control word */
      mem[A] = 04110;
      mem[P]++;
    } else {
      fprintf(stderr," unimplemented INA '04 function\n");
      exit(1);
    }
    break;

  case 3:
    fprintf(stderr," OTA '%2#0o%2#0o\n", func, device);
    if (func == 0) {
      fprintf(stderr," char to write=%o\n", mem[A]);
      putchar(mem[A] & 0x7f);
      fflush(stdout);
      mem[P]++;                     /* OTA '0004 always works on Unix */
    } else if (func == 1) {         /* write control word */
      mem[P]++;
    } else {
      fprintf(stderr," unimplemented OTA '04 function\n");
      exit(1);
    }
    break;
  }
}




/* Device '20: control panel switches and lights

   OTA '1720 = write to lights (sets CP fetch address)
   INA '1420 = read location from CP ROM (not implemented, used to boot)
   INA '1620 = read control panel switches
*/

void devcp (short class, short func, short device) {

  switch (class) {

  case 0:
    fprintf(stderr," OCP '%2#0o%2#0o\n", func, device);
    fprintf(stderr," unimplemented OCP device %2#0o function\n", device);
    exit(1);
    break;

  case 1:
    fprintf(stderr," SKS '%2#0o%2#0o\n", func, device);
    fprintf(stderr," unimplemented SKS device %2#0o function\n", device);
    exit(1);
    break;

  case 2:
    fprintf(stderr," INA '%2#0o%2#0o\n", func, device);
    if (func == 016) {
      mem[A] = 0;
    } else {
      fprintf(stderr," unimplemented INA device %2#0o function\n", device);
      exit(1);
    }
    break;

  case 3:
    fprintf(stderr," OTA '%2#0o%2#0o\n", func, device);
    if (func == 017) {           /* write lights */
      mem[P]++;
    } else {
      fprintf(stderr," unimplemented OTA device %2#0o function\n", device);
      exit(1);
    }
    break;
  }
}


