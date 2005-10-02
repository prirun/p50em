/* emdev.h, Jim Wilcoxson, April 17, 2005
   Device handlers for pio instructions.  Use devnull as a template for
   new handlers.
   NOTES:

   OCP instructions never skip
   SKS instructions skip on specified conditions
   INA/OTA instructions skip if they succeed (data was read/written)

   Device numbers:
   '00 = polling (?)
   '01 = paper tape reader
   '02 = paper tape punch
   '03 = #1 MPC/URC (line printer/card reader/card punch)
   '04 = SOC board (system console/user terminal)
   '05 = #2 MPC/URC (line printer/card reader/card punch)
   '06 = card punch? (RTOS User Guide, A-1) / IPC (Engr Handbook p.101)
   '06 = Interproc. Channel (IPC) (R20 Hacker's Guide)
   '07 = #1 PNC
   '10 = ICS2 #1 or ICS1
   '11 = ICS2 #2 or ICS1
   '12 = floppy disk/diskette
   '13 = #2 magtape controller
   '14 = #1 magtape controller
   '15 = #5 AMLC or ICS1
   '16 = #6 AMLC or ICS1
   '17 = #7 AMLC or ICS1
   '20 = control panel / real-time clock
   '21 = 1st 4002 (Option B') disk controller
   '22 = disk #3
   '23 = disk #4
   '24 = disk (was Writable Control Store)
   '25 = disk (was 4000 disk controller)
   '26 = 1st disk controller
   '27 = 2nd disk controller
   '30-32 = BPIOC #1-3 (RTOS User Guide, A-1)
   '32 = AMLC #8 or ICS1
   '33 = #1 Versatec (verdim)
   '34 = #2 Versatec
   '35 = #4 AMLC or ICS1
   '36-37 = ELFBUS #1 & 2 (ICS1 #1, ICS1 #2)
   '40 = A/D converter type 6000
   '41 = digital input type 6020
   '42 = digital input #2
   '43 = digital output type 6040
   '44 = digital output #2
   '45 = disk (was D/A converter type 6060 (analog output) - obsolete)
   '46 = disk
   '47 = #2 PNC
   '50 = #1 HSSMLC (cs/slcdim.pma)
   '51 = #2 HSSMLC
   '52 = #3 AMLC or ICS1
   '53 = #2 AMLC 
   '54 = #1 AMLC
   '55 = MACI autocall unit
   '56 = old SMLC (RTOS User Guide, A-1 & Hacker's Guide)
   '60-67 = reserved for user devices (GPIB)
   '70-'73 = Megatek graphics terminals

   Devices emulated by Primos in ks/ptrap.ftn for I/O instructions in Ring 3:
     '01 = paper tape reader
     '02 = paper tape punch
     '04 = console
     '20 = control panel lights & sense switches
*/

#include <errno.h>
#include <fcntl.h>
#include <sys/time.h>
#include <sys/select.h>
#include <unistd.h>
#include <termios.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <time.h>

/* this macro is used when I/O is successful.  In VI modes, it sets
   the EQ condition code bit.  In SR modes, it does a skip */

#define IOSKIP \
  if (crs[KEYS] & 010000) \
    crs[KEYS] |= 0100; \
  else \
    RPL++

/* macro to detect if an I/O instruction is followed by JMP *-1 (SR modes)
   or BCNE *-2 (VI modes).  If process exchange is enabled, I/O can never
   block. */

#if 1
#define BLOCKIO \
  (!(crs[MODALS] & 010) && (get16(RP) == 03776 || (get16(RP) == 0141603 && get16(RP+1) == RPL-2)))
#else
#define BLOCKIO 0
#endif



/* this is a template for new device handlers */

int devnew (short class, short func, short device) {

  switch (class) {

  case -1:
    return 0;

  case 0:
    if (T_INST) fprintf(stderr," OCP '%02o%02o\n", func, device);
    if (func == 99) {
      ;
    } else {
      printf("Unimplemented OCP device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 1:
    if (T_INST) fprintf(stderr," SKS '%02o%02o\n", func, device);
    if (func == 99)
      IOSKIP;                     /* assume it's always ready */
    else {
      printf("Unimplemented SKS device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 2:
    if (T_INST) fprintf(stderr," INA '%02o%02o\n", func, device);
    if (func == 99) {
      ;
    } else {
      printf("Unimplemented INA device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 3:
    if (T_INST) fprintf(stderr," OTA '%02o%02o\n", func, device);
    if (func == 99) {
      IOSKIP;
    } else {
      printf("Unimplemented OTA device '%02o function '%02o, A='%o\n", device, func, crs[A]);
      fatal(NULL);
    }
    break;
  }
}


/* this is a template for null (not present) devices */

int devnone (short class, short func, short device) {

  switch (class) {

  case -1:
    return 0;

  case 0:
    if (T_INST) fprintf(stderr," OCP '%02o%02o\n", func, device);
    fprintf(stderr, " unimplemented device '%o\n", device);
    break;

  case 1:
    if (T_INST) fprintf(stderr," SKS '%02o%02o\n", func, device);
    fprintf(stderr, " unimplemented device '%o\n", device);
    break;

  case 2:
    if (T_INST) fprintf(stderr," INA '%02o%02o\n", func, device);
    fprintf(stderr, " unimplemented device '%o\n", device);
    break;

  case 3:
    if (T_INST) fprintf(stderr," OTA '%02o%02o\n", func, device);
    fprintf(stderr, " unimplemented device '%o\n", device);
    break;
  }
}


/* Device '4: system console

   NOTES:
   - needs to reset tty attributes when emulator shuts down
   - Primos only handles ASRATE 110 (10cps), 1010 (30cps), 3410 (960 cps)
   - input ID is wrong, causes OCP '0477 on clock interrupt
   - instruction counters on output are bogus, need to intercept TNOUA instead

   OCP '0004 = initialize for input only, echoplex, 110 baud, 8-bit, no parity
   OCP '0104 = same, but for output only
   OCP '0204 = set receive interrupt mask
   OCP '0304 = enable receive DMA/C
   OCP '0404 = reset receive interrupt mask and DMA/C enable
   OCP '0504 = set transmit interrupt mask
   OCP '0604 = enable transmit DMA/C
   OCP '0704 = reset transmit interrupt mask and DMA/C enable
   OCP '1004 = Full duplex; software must echo
   OCP '1104 = output a sync pulse (diagnostics) 
   OCP '1204 = Prime normal, independent xmit and recv w/echoplex
   OCP '1304 = Self test mode (internally connects transmitter to receiver)
   OCP '1504 = Set both xmit & rcv interrupt masks
   OCP '1604 = Reset both xmit & rcv interrupt masks
   OCP '1704 = same as '0004, but clears interrupt masks and dma/c enables
               (this is the state after Master Clear)

   SKS '0004 = skip if either receive or xmit ready, whichever are enabled
   SKS '0104 = skip if not busy
   SKS '0204 = skip if receiver not interrupting
   SKS '0304 = skip if control registers valid
   SKS '0404 = skip if neither xmit nor recv are interrupting
   SKS '0504 = skip if xmit not interrupting
   SKS '0604 = skip is xmit ready (can accept a character)
   SKS '0704 = skip if recv ready (character present)
   SKS '1104-'1404 = skip if input bit 1/2/3/4 marking
   SKS '1504 = skip if parity error
   SKS '1604 = skip if character overrun
   SKS '1704 = skip if framing error

   INA '0004 = "or" character into right byte of A, not modifying left byte
   INA '0404 = input receive control register 1 (always works)
   INA '0504 = input receive control register 2 (always works)
   INA '0604 = input xmit control register 1 (always works)
   INA '0704 = input xmit control register 2 (always works)
   INA '1004 = like '0004, but clear A first
   INA '1104 = input device ID
   INA '1404 = input recv DMA/C channel address (these last 4 always work)
   INA '1504 = input xmit DMA/C channel address
   INA '1604 = input recv interrupt vector
   INA '1704 = input xmit interrupt vector

   OTA '0004 = xmit character from A; fails if inited for recv only
   OTA '0404 = output rcv CR 1
   OTA '0504 = output rcv CR 2
   OTA '0604 = output xmit CR 1
   OTA '0704 = output xmit CR 2
   OTA '1404 = output recv DMA/C channel address (these last 4 always work)
   OTA '1504 = output xmit DMA/C channel address
   OTA '1604 = output recv interrupt vector
   OTA '1704 = output xmit interrupt vector

*/


int devasr (short class, short func, short device) {

  static int ttydev;
  static int ttyflags;
  static int needflush;     /* true if data has been written but not flushed */
  static int atbol=1;       /* true if cursor is at bol */
  static int atnewl=1;      /* true if cursor is on a blank line */
  static struct termios terminfo;
  static fd_set fds;
  struct timeval timeout;
  unsigned char ch;
  int newflags;
  int n;


  switch (class) {

  case -1:
    setsid();
    ttydev = open("/dev/tty", O_RDWR, 0);
    if (ttydev < 0) {
      perror(" error opening /dev/tty");
      fatal(NULL);
    }
    if (fcntl(ttydev, F_GETFL, ttyflags) == -1) {
      perror(" unable to get tty flags");
      fatal(NULL);
    }
    FD_ZERO(&fds);
    if (tcgetattr(ttydev, &terminfo) == -1) {
      perror(" unable to get tty attributes");
      fatal(NULL);
    }
    terminfo.c_iflag &= ~(INLCR | ICRNL);
    terminfo.c_lflag &= ~(ECHOCTL | ICANON);
    terminfo.c_cc[VMIN] = 0;
    terminfo.c_cc[VTIME] = 0;
    if (tcsetattr(ttydev, TCSANOW, &terminfo) == -1) {
      perror(" unable to set tty attributes");
      fatal(NULL);
    }
    return 0;

  case 0:
    if (T_INST) fprintf(stderr," OCP '%02o%02o\n", func, device);
    break;

  case 1:
    if (T_INST) fprintf(stderr," SKS '%02o%02o\n", func, device);

    if (func == 6) {               /* skip if room for a character */
#if 0
      timeout.tv_sec = 0;
      timeout.tv_usec = 0;
      FD_SET(ttydev, &fds);
      n = select(ttydev+1, NULL, &fds, NULL, &timeout);
      if (n == -1) {
	perror(" unable to do write select on ttydev");
	fatal(NULL);
      }
      if (n) {
	IOSKIP;
      }
#else
      IOSKIP;                      /* assume there is room under Unix */
#endif

    } else if (func == 7) {        /* skip if received a char */
      if (crs[MODALS] & 010)       /* PX enabled? */
	timeout.tv_sec = 0;        /* yes, can't delay */
      else
	timeout.tv_sec = 1;
      timeout.tv_usec = 0;
      FD_SET(ttydev, &fds);
      n = select(ttydev+1, &fds, NULL, NULL, &timeout);
      if (n == -1) {
	perror(" unable to do select on tty");
	fatal(NULL);
      }
      if (n) {
	IOSKIP;
      }

    } else if (func <= 014)
      IOSKIP;                     /* assume it's always ready */
    break;

  case 2:
    if (T_INST) fprintf(stderr," INA '%02o%02o\n", func, device);
    if (func == 0 || func == 010) {
      if (BLOCKIO)
	newflags = ttyflags & ~O_NONBLOCK;
      else
	newflags = ttyflags | O_NONBLOCK;
      if (newflags != ttyflags && fcntl(ttydev, F_SETFL, newflags) == -1) {
	perror(" unable to set tty flags");
	fatal(NULL);
      }
      ttyflags = newflags;
      if (needflush && BLOCKIO) {
	fflush(stdout);
	needflush = 0;
	devpoll[device] = 0;
      }
readasr:
      n = read(ttydev, &ch, 1);
      if (n < 0) {
	if (errno != EAGAIN) {
	  perror(" error reading from tty");
	  fatal(NULL);
	}
      } else if (n == 1) {
	if (ch == '') {
	  traceflags = ~traceflags;
	  traceflags &= ~TB_MAP;
	  if (traceflags == 0)
	    fprintf(stderr,"\nTRACE DISABLED:\n\n");
	  else
	    fprintf(stderr,"\nTRACE ENABLED:\n\n");
	  //memdump(0, 0xFFFF);
	  goto readasr;
	}
	if (func >= 010)
	  crs[A] = 0;
	crs[A] = crs[A] | ch;
	if (T_INST) fprintf(stderr," character read=%o: %c\n", crs[A], crs[A] & 0x7f);
	IOSKIP;
      } else  if (n != 0) {
	printf("Unexpected error reading from tty, n=%d\n", n);
	fatal(NULL);
      }
    } else if (04 <= func && func <= 07) {  /* read control register 1/2 */
      crs[A] = 0;
      IOSKIP;
    } else if (func == 011) {    /* read device id? */
      crs[A] = 4;
      IOSKIP;
    } else if (func == 012) {    /* read control word */
      crs[A] = 04110;
      IOSKIP;
    } else if (func == 017) {    /* read xmit interrupt vector */
      crs[A] = 0;
      IOSKIP;
    } else {
      printf("Unimplemented INA '04 function '%02o\n", func);
      fatal(NULL);
    }
    break;

  case 3:
    if (T_INST) fprintf(stderr," OTA '%02o%02o\n", func, device);
    if (func == 0) {
      ch = crs[A] & 0x7f;
      if (T_INST) fprintf(stderr," char to write=%o: %c\n", crs[A], ch);
      if (ch == 0 || ch == 0x7f) {
	IOSKIP;
	return;
      }
#if 0
      timeout.tv_sec = 0;
      timeout.tv_usec = 0;
      FD_SET(2, &fds);
      n = select(2+1, NULL, &fds, NULL, &timeout);
      if (n == -1) {
	perror(" unable to do write select on stdout");
	fatal(NULL);
      }
      if (!n)
	return;
#endif
#if 0
      if (atbol && atnewl)
	printf("%10d| ", instcount);
#endif
      putchar(ch);
      if (ch == 015)
	atbol = 1;
      else if (ch == 012)
	atnewl = 1;
      else {
	atbol = 0;
	atnewl = 0;
      }
      needflush = 1;
      if (devpoll[device] == 0)
	devpoll[device] = instpermsec*100;
      IOSKIP;
    } else if (func == 1) {       /* write control word */
      IOSKIP;
    } else if (04 <= func && func <= 07) {  /* write control register 1/2 */
      IOSKIP;
    } else if (func == 013) {
      /* NOTE: does this in rev 20 on settime command (Option A maybe?) */
      IOSKIP;
    } else if (func == 017) {
      /* NOTE: 9950 does this in rev 20, others don't */
      IOSKIP;
    } else {
      printf("Unimplemented OTA device '%02o function '%02o, A='%o\n", device, func, crs[A]);
      fatal(NULL);
    }
    break;

  case 4:
    /* since the tty device is in non-blocking mode under Primos, keep
       flushing output every .1 seconds for smoothest and fastest
       output */
    if (needflush) {
      fflush(stdout);
      needflush = 1;
      devpoll[device] = instpermsec*100;
    }
  }
}




/* Device '14 - magtape controller #1
 */

int devmt (short class, short func, short device) {

  switch (class) {

  case -1:
    return 0;

  case 0:
    if (T_INST) fprintf(stderr," OCP '%02o%02o\n", func, device);
    break;

  case 1:
    if (T_INST) fprintf(stderr," SKS '%02o%02o\n", func, device);
    break;

  case 2:
    if (T_INST) fprintf(stderr," INA '%02o%02o\n", func, device);
    if (BLOCKIO) {
      printf("Device '%o not supported, so I/O hangs\n", device);
      fatal(NULL);
    }
    break;

  case 3:
    if (T_INST) fprintf(stderr," OTA '%02o%02o\n", func, device);
    if (BLOCKIO) {
      printf("Device '%o not supported, so I/O hangs\n", device);
      fatal(NULL);
    }
    break;
  }
}


/* Device '20: control panel switches and lights, and realtime clock

   OCP '0020 = start Line Frequency Clock, enable mem increment, ack previous overflow
   OCP '0120 = ack PIC interrupt
   OCP '0220 = stop LFC, disable mem increment, ack previous overflow
   OCP '0420 = select LFC for memory increment
   OCP '0520 = select external clock for memory increment
   OCP '0620 = starts a new 50-ms watchdog timeout interval
   OCP '0720 = stops the watchdog timer
   OCP '1520 = set interrupt mask
   OCP '1620 = reset interrupt mask
   OCP '1720 = initialize as in Master Clear

   SKS '0020 = skip if clock IS interrupting
   SKS '0220 = skip if clock IS NOT interrupting
   SKS '0520 = skip if WDT timed out
   SKS '0720 = skip if WDT caused external interrupt (loc '60)

   OTA '0220 = set PIC Interval register (negative, interrupts on incr to zero)
   OTA '0720 = set control register
   OTA '1220 = set Memory Increment Cell address
   OTA '1320 = set interrupt vector address
   OTA '1720 = write to lights (sets CP fetch address)

   INA '0220 = read PIC Interval register
   INA '1120 = read device ID, don't clear A first
   INA '1220 = read Memory Increment Cell address
   INA '1320 = read interrupt vector address
   INA '1420 = read location from CP ROM (not implemented, used to boot)
   INA '1620 = read control panel up switches
   INA '1720 = read control panel down switches

   IMPORTANT NOTE: this device ('20) never skips!
*/

int devcp (short class, short func, short device) {
  static short enabled = 0;
  static unsigned short clkvec;
  static short clkpic;

  int datnow, i;
  time_t unixtime;
  ea_t datnowea;
  struct tm *tms;

#define SETCLKPOLL devpoll[device] = instpermsec*(-clkpic*3.2)/1000;

  switch (class) {

  case -1:
    return 0;

  case 0:
    if (T_INST) fprintf(stderr," OCP '%02o%02o\n", func, device);

    if (func == 01) {           /* ack PIC interrupt */
      ;

    } else if (func == 015) {   /* set interrupt mask */
      fprintf(stderr,"Clock interrupt enabled!\n");
      /* this enables tracing when the clock process initializes */
      //traceflags = ~TB_MAP;
      enabled = 1;
#if 1
      SETCLKPOLL;
#else
      devpoll[device] = 600000;
#endif
      datnowea = 0;
      for (i=0; i<numsyms; i++) {
	if (strcmp(mapsym[i].symname, "DATNOW") == 0)
	  datnowea = mapsym[i].address;
      }
      if (datnowea != 0) {
	unixtime = time(NULL);
	tms = localtime(&unixtime);
	datnow = tms->tm_year<<25 | (tms->tm_mon+1)<<21 | tms->tm_mday<<16 | ((tms->tm_hour*3600 + tms->tm_min*60 + tms->tm_sec)/4);
	put32(datnow, datnowea);
      }

    } else if (func == 016 || func == 017) {
      enabled = 0;
      devpoll[device] = 0;

    } else {
      printf("Unimplemented OCP device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 1:
    if (T_INST) fprintf(stderr," SKS '%02o%02o\n", func, device);
    printf("Unimplemented SKS device '%02o function '%02o\n", device, func);
    fatal(NULL);
    break;

  case 2:
    if (T_INST) fprintf(stderr," INA '%02o%02o\n", func, device);
    if (func == 011) {             /* input ID */
      crs[A] = 0120;               /* this is the SOC board */
    } else if (func == 016) {
      crs[A] = sswitch;
    } else if (func == 017) {      /* read switches in momentary down position */
      crs[A] = 0;
    } else {
      printf("Unimplemented INA device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 3:
    if (T_INST) fprintf(stderr," OTA '%02o%02o\n", func, device);
    if (func == 02) {            /* set PIC interval */
      clkpic = *(short *)(crs+A);
      SETCLKPOLL;
      //printf("Clock PIC interval set to %d\n", clkpic);
    } else if (func == 07) {
      //printf("Clock control register set to '%o\n", crs[A]);
    } else if (func == 013) {
      clkvec = crs[A];
      //printf("Clock interrupt vector address = '%o\n", clkvec);
    } else if (func == 017) {           /* write lights */
    } else {
      printf("Unimplemented OTA device '%02o function '%02o, A='%o\n", device, func, crs[A]);
      fatal(NULL);
    }
    break;

  case 4:
#if 1
    if (enabled) {
      if (intvec == -1)
	intvec = clkvec;
      SETCLKPOLL;
    }
#endif
    break;
  }
}


/* disk controller at '26 and '27

  NOTES:
  - in the DSEL disk channel program command, unit number is a 4-bit field,
  with these binary values:
  0001 = unit 0 (pdev 460/461)
  0010 = unit 1 (pdev 462/463)
  0100 = unit 2 (pdev 464/465)
  1000 = unit 3 (pdev 466/467)

  OCP '1626 = reset interrupt
  OCP '1726 = reset controller

  INA '0626 = ??

  INA '1126 = input ID, don't clear A first, fails if no controller
  - bits 1,2,8-16 are significant, bits 8-9 are type, 10-16 are ID
  - 4004 controller responds '26 (type=0)
  - 4005 controller responds '126 (type=1)
  - 2382 controller responds '040100 (type=1)
  - LCDTC controller responds '226 (type=2)
  - 10019 controller responds '040100 (type=1)
  - 6590 controller responds '040100 (type=1)

  INA '1726 = read oar, fails if controller is not halted

  OTA '1726 = load OAR (Order Address Register), ie, run channel
  program, address is in A

 */

int devdisk (short class, short func, short device) {

#define S_HALT 0
#define S_RUN 1
#define S_INT 2

#define MAXDRIVES 8

#if 0
  #define CID4005 0100
#else
  #define CID4005 0
#endif


  static struct {
    unsigned short oar;
    unsigned short state;                  /* channel program state: S_XXXX */
    unsigned short status;                 /* controller status */
    short usel;                            /* unit selected (0-3, -1=none) */
    short dmachan;                         /* dma channel selected */
    short dmanch;                          /* number of dma channels-1 */
    struct {
      int rtfd;                            /* read trace file descriptor */
      unsigned short theads;               /* total heads (cfg file) */
      unsigned short spt;                  /* sectors per track */
      int devfd;                           /* Unix device file descriptor */
      int readnum;                         /* increments on each read */
    } unit[MAXDRIVES];
  } dc[64];

  short i,u;

  /* temps for running channel programs */

  unsigned short order;
  unsigned short m,m1,m2;
  short head, track, rec, recsize, nwords;
  unsigned short dmareg, dmaaddr;
  unsigned short iobuf[4096];                /* local I/O buf, before mapped I/O */
  unsigned short *iobufp;
  unsigned short access;
  short dmanw, dmanw1, dmanw2;
  unsigned int utempl;
  char ordertext[8];
  int theads, spt, phyra;
  int nb;                   /* number of bytes returned from read/write */
  char devfile[8];
  char rtfile[16];          /* read trace file name */
  int rtnw;                 /* total number of words read (all channels) */

  switch (class) {

  case -1:
    for (i=0; i<64; i++) {
      dc[i].state = S_HALT;
      dc[i].status = 0100000;
      dc[i].usel = -1;
      for (u=0; u<MAXDRIVES; u++) {
	dc[i].unit[u].rtfd = -1;
	dc[i].unit[u].theads = 40;
	dc[i].unit[u].spt = 9;
	dc[i].unit[u].devfd = -1;
	dc[i].unit[u].readnum = -1;
      }
    }
    return 0;
      
  case 0:
    if (T_INST || T_DIO) fprintf(stderr," OCP '%2o%2o\n", func, device);
    if (func == 016) {                /* reset interrupt */
      if (dc[device].state == S_INT) {
	dc[device].state = S_RUN;
	devpoll[device] = 1;
      }
    } else if (func == 017) {         /* reset controller */
      dc[i].state = S_HALT;
      dc[i].status = 0100000;
      dc[i].usel = -1;
    } else {
      fprintf(stderr," Unrecognized OCP '%2o%2o\n", func, device);
      fatal(NULL);
    }
    break;

  case 1:
    if (T_INST || T_DIO) fprintf(stderr," SKS '%2o%2o\n", func, device);
    if (func == 04) {                  /* skip if not interrupting */
      if (dc[device].state != S_INT)
	IOSKIP;
    } else {
      fprintf(stderr," Unrecognized SKS '%2o%2o\n", func, device);
      fatal(NULL);
    }
    break;

  case 2:
    if (T_INST || T_DIO) fprintf(stderr," INA '%2o%2o\n", func, device);

    /* this turns tracing on when the Primos disk processes initialize */
    //traceflags = ~TB_MAP;

    /* INA's are only accepted when the controller is not busy */

    if (dc[device].state != S_HALT)
      return;

    if (func == 01)          /* read device id, clear A first */
      crs[A] = CID4005 + device;
    else if (func == 011)    /* read device id, don't clear A */
      crs[A] |= (CID4005 + device);
    else if (func == 017) {  /* read OAR */
      crs[A] = dc[device].oar;
    } else {
      printf("Unimplemented INA device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    IOSKIP;
    break;

  case 3:
    if (T_INST || T_DIO) fprintf(stderr," OTA '%02o%02o\n", func, device);
    if (func == 017) {        /* set OAR (order address register) */
      dc[device].state = S_RUN;
      dc[device].oar = crs[A];
      devpoll[device] = 1;
    } else {
      printf("Unimplemented OTA device '%02o function '%02o, A='%o\n", device, func, crs[A]);
      fatal(NULL);
    }
    IOSKIP;
    break;

  case 4:   /* poll (run channel program) */

    while (dc[device].state == S_RUN) {
      m = get16r(dc[device].oar, 0);
      m1 = get16r(dc[device].oar+1, 0);
      if (T_INST || T_DIO) fprintf(stderr,"\nDIOC %o: %o %o %o\n", dc[device].oar, m, m1, get16r(dc[device].oar+2, 0));
      dc[device].oar += 2;
      order = m>>12;
      if (m & 04000) {   /* "execute if ..." */
	if (order == 2 || order == 5 || order == 6)
	  dc[device].oar++;
	continue;
      }

      switch (order) {

      case 0: /* DHLT = Halt */
	dc[device].state = S_HALT;
	devpoll[device] = 0;
	if (T_INST || T_DIO) fprintf(stderr," channel halted at '%o\n", dc[device].oar);
	break;

      case 2: /* SFORM = Format */
      case 5: /* SREAD = Read */
      case 6: /* SWRITE = Write */
	m2 = get16r(dc[device].oar++, 0);
	recsize = m & 017;
	track = m1 & 01777;
	rec = m2 >> 8;   /* # records for format, rec # for R/W */
	head = m2 & 077;
	u = dc[device].usel;
	if (order == 2)
	  strcpy(ordertext,"Format");
	else if (order == 5)
	  strcpy(ordertext,"Read");
	else if (order == 6)
	  strcpy(ordertext,"Write");
	if (T_INST || T_DIO) fprintf(stderr," %s, head=%d, track=%d, rec=%d, recsize=%d\n", ordertext, head, track, rec, recsize);
	if (dc[device].unit[u].devfd == -1) {
	  if (T_INST || T_DIO) fprintf(stderr," Unit not selected or not ready\n");
	  dc[device].status = 0100001;
	} else if (order == 2) {
	  if (T_INST || T_DIO) fprintf(stderr," Format order\n");
	  //fatal("DFORMAT channel order not implemented");
	} else {            /* order = 5 (read) or 6 (write) */

	  /* translate head/track/sector to drive record address */

	  phyra = (track*dc[device].unit[u].theads*dc[device].unit[u].spt) + head*9 + rec;
	  if (T_INST || T_DIO) fprintf(stderr, " Unix ra=%d, byte offset=%d\n", phyra, phyra*2080);
	  if (lseek(dc[device].unit[u].devfd, phyra*2080, SEEK_SET) == -1) {
	    perror("Unable to seek drive file");
	    fatal(NULL);
	  }
	  while (dc[device].dmanch >= 0) {
	    dmareg = ((dc[device].dmachan & 036) << 1) | (dc[device].dmachan & 1);
	    dmanw = regs.sym.regdmx[dmareg];
	    dmanw = -(dmanw>>4);
	    dmaaddr = regs.sym.regdmx[dmareg+1];
	    if (T_INST || T_DIO) fprintf(stderr, " DMA channels: nch-1=%d, ['%o]='%o, ['%o]='%o, nwords=%d\n", dc[device].dmanch, dc[device].dmachan, regs.sym.regdmx[dmareg], dc[device].dmachan+1, dmaaddr, dmanw);
	    
	    if (order == 5) {
	      if (crs[MODALS] & 020)
		if ((dmaaddr & 01777) || dmanw > 1024)
		  iobufp = iobuf;
		else
		  iobufp = mem+mapva(dmaaddr, WACC, &access, 0);
	      else
		iobufp = mem+dmaaddr;
	      if ((nb=read(dc[device].unit[u].devfd, (char *)iobufp, dmanw*2)) != dmanw*2) {
		fprintf(stderr, "Disk read error: device='%o, u=%d, fd=%d, nb=%d\n", device, u, dc[device].unit[u].devfd, nb);
		if (nb == -1) perror("Unable to read drive file");
		memset((char *)iobufp, 0, dmanw*2);
	      }
	      if (iobufp == iobuf)
		for (i=0; i<dmanw; i++)
		  put16r(iobuf[i], dmaaddr+i, 0);
	    } else {
	      if (crs[MODALS] & 020) {
		iobufp = iobuf;
		for (i=0; i<dmanw; i++)
		  iobuf[i] = get16r(dmaaddr+i, 0);
	      } else
		iobufp = mem+dmaaddr;
	      if (write(dc[device].unit[u].devfd, (char *)iobufp, dmanw*2) != dmanw*2) {
		perror("Unable to write drive file");
		fatal(NULL);
	      }
	    }
	    regs.sym.regdmx[dmareg] = 0;
	    regs.sym.regdmx[dmareg+1] += dmanw;
	    dc[device].dmachan += 2;
	    dc[device].dmanch--;
	  }
	}
	break;

	/* NOTE: for seek command, the track should probably be stored
	   in a state variable, then checked for equality on
	   read/write.  If not equal, a disk fault should occur rather
	   than reading/writing from the wrong cylinder. */

      case 3: /* SSEEK = Seek */
	track = m1 & 01777;
	if (T_INST || T_DIO) fprintf(stderr," seek track %d, restore=%d, clear=%d\n", track, (m1 & 0100000) != 0, (m1 & 040000) != 0);
	break;

      case 4: /* DSEL = Select unit */
	u = (m1 & 017) >> 1;       /* unit = 0/1/2/4 */
	if (u == 4) u = 3;         /* unit = 0/1/2/3 */
	dc[device].usel = u;
	if (dc[device].unit[u].devfd == -1) {
	  snprintf(devfile,sizeof(devfile),"dev%ou%d", device, u);
	  if (T_INST || T_DIO) fprintf(stderr," filename for unit %d is %s\n", u, devfile);
	  if ((dc[device].unit[u].devfd = open(devfile, O_RDWR, 0)) == -1)
	    dc[device].status = 0100001;    /* not ready */
	}
	if (T_INST || T_DIO) fprintf(stderr," select unit %d\n", u);
	break;

      case 7: /* DSTALL = Stall */
	if (T_INST || T_DIO) fprintf(stderr," stall\n");

	/* NOTE: technically, the stall command is supposed to wait
	   210 usecs, so that the disk controller doesn't hog the I/O
	   bus by looping in a channel program waiting for I/O to
	   complete.  With the emulator, this delay isn't necessary,
	   although it will cause DIAG tests to fail if the delay is
	   omitted.  Hence the PX test.  Ignoring stall gives a 25%
	   increase in I/O's per second on a 2GHz Mac (8MB emulator). */

	if (crs[MODALS] & 010)             /* PX enabled? */
	  break;                           /* yes, no stall */
	devpoll[device] = instpermsec/5;   /* 200 microseconds, sb 210 */
	return;

      case 9: /* DSTAT = Store status to memory */
	if (T_INST || T_DIO) fprintf(stderr, " store status='%o to '%o\n", dc[device].status, m1);
	put16r(dc[device].status,m1,0);
	break;

      case 11: /* DOAR = Store OAR to memory (2 words) */
	if (T_INST || T_DIO) fprintf(stderr, " store OAR='%o to '%o\n", dc[device].oar, m1);
	put16r(dc[device].oar,m1,0);
	break;

      case 13: /* SDMA = select DMA channel(s) to use */
	dc[device].dmanch = m & 017;
	dc[device].dmachan = m1;
	if (T_INST || T_DIO) fprintf(stderr, " set DMA channels, nch-1=%d, channel='%o\n", dc[device].dmanch, dc[device].dmachan);
	break;

      case 14: /* DINT = generate interrupt through vector address */
	if (T_INST || T_DIO) fprintf(stderr, " interrupt through '%o\n", m1);
	if (intvec >= 0 || !(crs[MODALS] & 0100000) || inhcount > 0)
	  dc[device].oar -= 2;     /* can't take interrupt right now */
	else {
	  intvec = m1;
	  dc[device].state = S_INT;
	}
	//traceflags = ~TB_MAP;
	devpoll[device] = 10;
	return;

      case 15: /* DTRAN = channel program jump */
	dc[device].oar = m1;
	if (T_INST || T_DIO) fprintf(stderr, " jump to '%o\n", m1);
	break;

      default:
	printf("Unrecognized channel order = %d\n", order);
	fatal(NULL);
      }
    }
  }
}

/* 
  AMLC I/O operations:

  OCP '0054 - stop clock
  OCP '0154 - single step clock

  OCP '1234 - set normal/DMT mode
  OCP '1354 - set diagnostic/DMQ mode
  OCP '1554 - enable interrupts
  OCP '1654 - disable interrupts
  OCP '1754 - initialize AMLC

  SKS '0454 - skip if NOT interrupting

  INA '0054 - input data set sense bit for all lines (read clear to send)
  INA '0754 - input AMLC status and clear (always skips)
  INA '1154 - input AMLC controller ID
  INA '1454 - input DMA/C channel number
  INA '1554 - input DMT/DMQ base address
  INA '1654 - input interrupt vector address

  OTA '0054 - output line number for INA '0054 (older models only)
  OTA '0154 - output line configuration for 1 line
  OTA '0254 - output line control for 1 line
  OTA '1454 - output DMA/C channel number
  OTA '1554 - output DMT/DMQ base address
  OTA '1654 - output interrupt vector address
  OTA '1754 - output programmable clock constant

  Primos AMLC usage:

  OCP '17xx
  - initialize controller
  - clear all registers and flip-flops
  - start line clock
  - clear all line control bits during the 1st line scan
  - responds not ready until 1 line scan is complete

  INA '11xx
  - read AMLC ID
  - emulator always returns '20054 (DMQ, 16 lines)

  OTA '17xx
  - set AMLC programmable clock constant
  - ignored by the emulator

  OTA '14xx
  - set DMC channel address for double input buffers
  - emulator stores in a structure

  OCP '13xx
  - set dmq mode (used to be "set diagnostic mode")
  - ignored by the emulator

  OTA '15xx
  - set DMT Base Address
  - also used for DMQ address?
  - emulator stores in a structure

  OTA '16xx
  - set interrupt vector address
  - emulator stores in a structure

  OTA '01xx
  - set line configuration
  - emulator ignores: all lines are 8-bit raw

  OTA '02xx
  - set line control
  - emulator ignores: all lines are enabled

  OCP '15xx/'16xx
  - enable/disable interrupts
  - emulator stores in a structure
*/

int devamlc (short class, short func, short device) {

#define MAXLINES 128
#define MAXFD MAXLINES+20
#define MAXBOARDS 8
  /* AMLC poll rate in milliseconds */
#define AMLCPOLL 50

#if 1
  #define QAMLC 020000   /* this is to enable QAMLC/DMQ functionality */
#else
  #define QAMLC 0
#endif

  static short inited = 0;
  static short devices[MAXBOARDS];         /* list of AMLC devices initialized */
  static int tsfd;                         /* socket fd for terminal server */
  static struct {                          /* maps socket fd to device & line */
    int device;
    int line;
  } socktoline[MAXLINES];
  static struct {
    char dmqmode;                          /* 0=DMT, 1=DMQ */
    char bufnum;                           /* 0=1st input buffer, 1=2nd */
    char eor;                              /* 1=End of Range on input */
    unsigned short dmcchan;                /* DMC channel (for input) */
    unsigned short baseaddr;               /* DMT/Q base address (for output) */
    unsigned short intvector;              /* interrupt vector */
    char intenable;                        /* interrupts enabled? */
    char interrupting;                     /* am I interrupting? */
    unsigned short xmitenabled;            /* 1 bit per line */
    unsigned short recvenabled;            /* 1 bit per line */
    unsigned short ctinterrupt;            /* 1 bit per line */
    unsigned short dss;                    /* 1 bit per line */
    unsigned short sockfd[16];             /* Unix socket fd, 1 per line */
  } dc[64];

  int lx;
  unsigned short utempa;
  unsigned int utempl;
  ea_t qcbea, dmcea, dmcbufbegea, dmcbufendea;
  unsigned short dmcnw;
  int dmcpair;
  int optval;
  int tsflags;
  struct sockaddr_in addr;
  int fd;
  unsigned int addrlen;
  unsigned char buf[1024];
  int i, n, nw, newdevice;

  switch (class) {

  case -1:

    /* this part of initialization only occurs once, no matter how many
       AMLC boards are configured */

    if (!inited) {
      for (i=0; i<MAXBOARDS; i++)
	devices[i] = 0;
      if (tport != 0) {
	tsfd = socket(AF_INET, SOCK_STREAM, 0);
	if (tsfd == -1) {
	  perror("socket failed for AMLC");
	  fatal(NULL);
	}
	optval = 1;
	if (setsockopt(tsfd, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof(optval))) {
	  perror("setsockopt failed for AMLC");
	  fatal(NULL);
	}
	addr.sin_family = AF_INET;
	addr.sin_port = htons(tport);
	addr.sin_addr.s_addr = INADDR_ANY;
	if(bind(tsfd, (struct sockaddr *)&addr, sizeof(addr))) {
	  perror("bind: unable to bind for AMLC");
	  fatal(NULL);
	}
	if(listen(tsfd, 10)) {
	  perror("listen failed for AMLC");
	  fatal(NULL);
	}
	if (fcntl(tsfd, F_GETFL, tsflags) == -1) {
	  perror("unable to get ts flags for AMLC");
	  fatal(NULL);
	}
	tsflags |= O_NONBLOCK;
	if (fcntl(tsfd, F_SETFL, tsflags) == -1) {
	  perror("unable to set ts flags for AMLC");
	  fatal(NULL);
	}
      } else
	printf("--tport is zero, can't start AMLC devices");
      inited = 1;
    }

    /* this part of initialization occurs for every AMLC board */

    if (!inited)
      return -1;

    /* add this device to the devices array, in the proper slot
       so we can tell what order the boards should be in */

    switch (device) {
    case 054: devices[0] = device; break;
    case 053: devices[1] = device; break;
    case 052: devices[2] = device; break;
    case 035: devices[3] = device; break;
    case 015: devices[4] = device; break;
    case 016: devices[5] = device; break;
    case 017: devices[6] = device; break;
    case 032: devices[7] = device; break;
    }
    return 0;

  case 0:
    if (T_INST) fprintf(stderr," OCP '%02o%02o\n", func, device);
    //printf(" OCP '%02o%02o\n", func, device);

    if (func == 012) {            /* set normal (DMT) mode */
      dc[device].dmqmode = 0;

    } else if (func == 013 && QAMLC) {     /* set diagnostic (DMQ) mode */
      dc[device].dmqmode = 1;

    } else if (func == 015) {     /* enable interrupts */
      dc[device].intenable = 1;

    } else if (func == 016) {     /* disable interrupts */
      dc[device].intenable = 0;

    } else if (func == 017) {     /* initialize AMLC */
      dc[device].dmqmode = 0;
      dc[device].bufnum = 0;
      dc[device].dmcchan = 0;
      dc[device].baseaddr = 0;
      dc[device].intvector = 0;
      dc[device].intenable = 0;
      dc[device].interrupting = 0;
      dc[device].xmitenabled = 0;
      dc[device].recvenabled = 0;
      dc[device].ctinterrupt = 0;
      dc[device].dss = 0;         /* NOTE: this is stored inverted: 1=connected */
      dc[device].eor = 0;

    } else {
      printf("Unimplemented OCP device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 1:
    if (T_INST) fprintf(stderr," SKS '%02o%02o\n", func, device);

    if (func == 04) {             /* skip if not interrupting */
      if (!dc[device].interrupting)
	IOSKIP;

    } else {
      printf("Unimplemented SKS device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 2:
    if (T_INST) fprintf(stderr," INA '%02o%02o\n", func, device);

    if (func == 00) {              /* input Data Set Sense (carrier) */
      crs[A] = ~dc[device].dss;    /* to the outside world, 1 = no carrier*/
      IOSKIP;

    } else if (func == 07) {       /* input AMLC status */
      crs[A] = 040000 | (dc[device].bufnum<<8) | (dc[device].intenable<<5) || (dc[device].dmqmode<<4);
      if (dc[device].eor) {
	crs[A] |= 0100000;
	dc[device].eor = 0;
      }
      if (dc[device].ctinterrupt)
	if (dc[device].ctinterrupt & 0xfffe)
	  crs[A] |= 0xcf;          /* multiple char time interrupt */
	else
	  crs[A] |= 0x8f;          /* last line cti */
      dc[device].interrupting = 0;
      //printf("INA '07%02o returns 0x%x\n", device, crs[A]);
      IOSKIP;

    } else if (func == 011) {      /* input ID */
      crs[A] = QAMLC | 054;
      IOSKIP;

    } else {
      printf("Unimplemented INA device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 3:
    if (T_INST) fprintf(stderr," OTA '%02o%02o\n", func, device);

    if (func == 01) {              /* set line configuration */
      lx = crs[A] >> 12;
      //printf("OTA '01%02o: AMLC line %d config = %x\n", device, lx, crs[A]);
      /* if DTR drops on a connected line, disconnect */
      if (!(crs[A] & 0x400) && (dc[device].dss & bitmask16[lx+1])) {
	/* see similar code below */
	write(dc[device].sockfd[lx], "\r\nDisconnecting on logout\r\n", 27);
	close(dc[device].sockfd[lx]);
	dc[device].dss &= ~bitmask16[lx+1];
	printf("Closing AMLC line %d on device '%o\n", lx, device);
      }
      IOSKIP;

    } else if (func == 02) {      /* set line control */
      //printf("OTA '02%02o: AMLC line control = %x\n", device, crs[A]);
      lx = (crs[A]>>12);
      if (crs[A] & 040)           /* character time interrupt enable/disable */
	dc[device].ctinterrupt |= bitmask16[lx+1];
      else
	dc[device].ctinterrupt &= ~bitmask16[lx+1];
      if (crs[A] & 010)           /* transmit enable/disable */
	dc[device].xmitenabled |= bitmask16[lx+1];
      else
	dc[device].xmitenabled &= ~bitmask16[lx+1];
      if (crs[A] & 01)            /* receive enable/disable */
	dc[device].recvenabled |= bitmask16[lx+1];
      else
	dc[device].recvenabled &= ~bitmask16[lx+1];
      if ((dc[device].ctinterrupt || dc[device].xmitenabled || dc[device].recvenabled) && devpoll[device] == 0)
	devpoll[device] = AMLCPOLL*instpermsec;  /* setup another poll */
      IOSKIP;

    } else if (func == 014) {      /* set DMA/C channel (for input) */
      dc[device].dmcchan = crs[A] & 0x7ff;
      //printf("OTA '14%02o: AMLC chan = %o\n", device, dc[device].dmcchan);
      if (!(crs[A] & 0x800))
	fatal("Can't run AMLC in DMA mode!");
      IOSKIP;

    } else if (func == 015) {      /* set DMT/DMQ base address (for output) */
      dc[device].baseaddr = crs[A];
      IOSKIP;

    } else if (func == 016) {      /* set interrupt vector */
      dc[device].intvector = crs[A];
      IOSKIP;

    } else if (func == 017) {      /* set programmable clock constant */
      IOSKIP;

    } else {
      printf("Unimplemented OTA device '%02o function '%02o, A='%o\n", device, func, crs[A]);
      fatal(NULL);
    }
    break;

  case 4:
    //printf("poll device '%o, cti=%x, xmit=%x, recv=%x, dss=%x\n", device, dc[device].ctinterrupt, dc[device].xmitenabled, dc[device].recvenabled, dc[device].dss);
    
    /* check for new connections */

conloop:

    while ((fd = accept(tsfd, (struct sockaddr *)&addr, &addrlen)) == -1 && errno == EINTR)
      ;
    if (fd == -1) {
      if (errno != EWOULDBLOCK) {
	perror("accept error for AMLC");
	fatal("accept error for AMLC");
      }
    } else {
      if (fd >= MAXFD)
	fatal("New connection fd is too big");
      newdevice = 0;
      for (i=0; !newdevice && i<MAXBOARDS; i++)
	if (devices[i])
	  for (lx=0; lx<16; lx++)
	    if (!(dc[devices[i]].dss & bitmask16[lx+1])) {
	      newdevice = devices[i];
	      socktoline[fd].device = newdevice;
	      socktoline[fd].line = lx;
	      dc[newdevice].dss |= bitmask16[lx+1];
	      dc[newdevice].sockfd[lx] = fd;
	      printf("AMLC connection, fd=%d, device='%o, line=%d\n", fd, newdevice, lx);
	      break;
	    }
      if (!newdevice) {
	warn("No free AMLC connection");
	write(fd, "\rAll AMLC lines are in use!\r\n", 29);
	close(fd);
      }
      goto conloop;
    }

    /* do a receive/transmit scan loop for every line
       NOTE: should probably do a read & write select to find lines to process */

    for (lx = 0; lx < 16; lx++) {
      if (dc[device].xmitenabled & bitmask16[lx+1]) {
	n = 0;
	if (dc[device].dmqmode) {
	  qcbea = dc[device].baseaddr + lx*4;
	  if (dc[device].dss & bitmask16[lx+1]) {
	    while (n < sizeof(buf) && rtq(qcbea, &utempa, 0)) {
	      if (utempa & 0x8000) {           /* valid character */
		//printf("Device %o, line %d, entry=%o (%c)\n", device, lx, utempa, utempa & 0x7f);
		buf[n++] = utempa ^ 0x80;
	      }
	    }
	  } else {         /* no line connected, just drain queue */
	    //printf("Draining output queue on line %d\n", lx);
	    put16r(get16r(qcbea, 0), qcbea+1, 0);
	  }
	} else {  /* DMT */
	  utempa = get16r(dc[device].baseaddr + lx, 0);
	  if (utempa != 0) {
	    if ((utempa & 0x8000) && (dc[device].dss & bitmask16[lx+1])) {
	      //printf("Device %o, line %d, entry=%o (%c)\n", device, lx, utempa, utempa & 0x7f);
	      buf[n++] = utempa ^ 0x80;
	    }
	    put16r(0, dc[device].baseaddr + lx, 0);
	  }
	  /* need to setup DMT xmit poll here, and/or look for char time interrupt */
	}
	
	/* NOTE: probably need to check for partial writes here, or put the
	   socket in blocking mode.  Would be best to know how many characters
	   to remove from the queue in the loop above. */

	if (n > 0)
	  if ((nw = write(dc[device].sockfd[lx], buf, n)) != n) {
	    perror("Writing to AMLC");
	    fatal("Writing to AMLC");
	  }
      }

      /* process input, but only as much as will fit into the DMC buffer */

      if ((dc[device].dss & dc[device].recvenabled & bitmask16[lx+1]) && !dc[device].eor) {
	if (dc[device].bufnum)
	  dmcea = dc[device].dmcchan ^ 2;
	else
	  dmcea = dc[device].dmcchan;
	dmcpair = get32r(dmcea, 0);
	dmcbufbegea = dmcpair>>16;
	dmcbufendea = dmcpair & 0xffff;
	dmcnw = dmcbufendea - dmcbufbegea + 1;
	if (dmcnw <= 0)
	  continue;
	if (dmcnw > sizeof(buf))
	  dmcnw = sizeof(buf);
	while ((n = read(dc[device].sockfd[lx], buf, dmcnw)) == -1 && errno == EINTR)
	  ;
	//printf("processing recv on device %o, line %d, b#=%d, dmcnw=%d, n=%d\n", device, lx, dc[device].bufnum, dmcnw, n);
	if (n == 0) {
	  n = -1;
	  errno = EPIPE;
	}
	if (n == -1) {
	  n = 0;
	  if (errno == EAGAIN || errno == EWOULDBLOCK)
	    ;
	  else if (errno == EPIPE) {
	    /* see similar code above */
	    close(dc[device].sockfd[lx]);
	    dc[device].dss &= ~bitmask16[lx+1];
	    printf("Closing AMLC line %d on device '%o\n", lx, device);
	  } else {
	    perror("Reading AMLC");
	    fatal("Reading AMLC");
	  }
	}

	if (n > 0) {
	  for (i=0; i<n; i++) {
	    utempa = lx<<12 | 0x0200 | buf[i];
	    put16r(utempa, dmcbufbegea, 0);
	    //printf("******* stored character %o (%c) at %o\n", utempa, utempa&0x7f, dmcbufbegea);
	    dmcbufbegea = INCVA(dmcbufbegea, 1);
	  }
	  put16r(dmcbufbegea, dmcea, 0);
	  if (dmcbufbegea > dmcbufendea) {          /* end of range has occurred */
	    dc[device].bufnum = 1-dc[device].bufnum;
	    dc[device].eor = 1;
	  }
	}
      }
    }
    if (intvec == -1 && dc[device].intenable && (dc[device].ctinterrupt || dc[device].eor)) {
      intvec = dc[device].intvector;
      dc[device].interrupting = 1;
    }

    if ((dc[device].ctinterrupt || dc[device].xmitenabled || dc[device].recvenabled) && devpoll[device] == 0)
      devpoll[device] = AMLCPOLL*instpermsec;  /* setup another poll */
    break;
  }
}
