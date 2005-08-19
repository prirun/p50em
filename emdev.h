/* emdev.h, Jim Wilcoxson, April 17, 2005
   Device handlers for pio instructions.  Use devnull as a template for
   new handlers.
   NOTES:

   OCP instructions never skip
   SKS instructions skip on specified conditions
   INA/OTA instructions skip if they succeed (data was read/written)

   Device numbers:
   '01 = paper tape reader
   '02 = paper tape punch
   '03 = 1st MPC (line printer/card reader/card punch)
   '04 = SOC board (system console/user terminal)
   '05 = 2nd MPC (line printer/card reader/card punch)
   '06 = card punch? (RTOS User Guide, A-1) / IPC (Engr Handbook p.101)
   '07 = PNC
   '12 = diskette
   '13 = 2nd magtape controller
   '14 = 1st magtape controller
   '20 = control panel / real-time clock
   '21 = 1st 4002 disk controller
   '22 = disk
   '23 = disk
   '24 = disk
   '25 = disk
   '26 = 1st disk controller
   '27 = 2nd disk controller
   '30-32 = BPIOC #1-3 (RTOS User Guide, A-1)
   '33 = 1st Versatec (verdim)
   '34 = 2nd Versatec
   '35 = 4th AMLC
   '36-37 = ELFBUS #1 & 2
   '40 = A/D converter type 6000
   '41 = digital input type 6020
   '42 = digital input #2
   '43 = digital output type 6040
   '44 = digital output #2
   '45 = disk (was D/A converter type 6060 (analog output) - obsolete)
   '46 = disk
   '50 = 1st HSSMLC (cs/slcdim.pma)
   '51 = 2nd HSSMLC
   '52 = 3rd AMLC
   '53 = 2nd AMLC 
   '54 = 1st AMLC
   '55 = MACI autocall unit
   '56 = old SMLC (RTOS User Guide, A-1)
   '60-67 = reserved for user devices
   '70-'73 = Megatek graphics terminals

   Devices emulated by Primos in ks/ptrap.ftn:
     '04 = console, '01 = paper tape reader, '02 = paper tape punch, 
     '20 = control panel
*/

#include <fcntl.h>
#include <sys/time.h>
#include <sys/select.h>
#include <unistd.h>

/* this macro is used when I/O is successful.  In VI modes, it sets
   the EQ condition code bit.  In SR modes, it does a skip */

#define IOSKIP \
  if (crs[KEYS] & 010000) \
    crs[KEYS] |= 0100; \
  else \
    RPL++

/* macro to detect if an I/O instruction is followed by JMP *-1 (SR modes)
   or BCNE *-2 (VI modes) */

#if 1
#define BLOCKIO \
  (get16(RP) == 03776 || (get16(RP) == 0141603 && get16(RP+1) == RPL-2))
#else
#define BLOCKIO 0
#endif


void devnull (short class, short func, short device) {

  switch (class) {

  case 0:
    if (T_INST) fprintf(stderr," OCP '%02o%02o\n", func, device);
    if (func == 0) {
      ;
    } else {
      printf("Unimplemented OCP device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 1:
    if (T_INST) fprintf(stderr," SKS '%02o%02o\n", func, device);
    if (func == 0)
      IOSKIP;                     /* assume it's always ready */
    else {
      printf("Unimplemented SKS device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 2:
    if (T_INST) fprintf(stderr," INA '%02o%02o\n", func, device);
    if (func == 0) {
      ;
    } else {
      printf("Unimplemented INA device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 3:
    if (T_INST) fprintf(stderr," OTA '%02o%02o\n", func, device);
    if (func == 0 | func == 1) {
      IOSKIP;                     /* OTA '0004 always works on Unix */
    } else {
      printf("Unimplemented OTA device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;
  }
}


/* Device '4: system console

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

void devasr (short class, short func, short device) {

  static int ttydev;
  static int ttyflags;
  static int needflush;     /* true if data has been written but not flushed */
  fd_set readfds;
  struct timeval timeout;
  unsigned char ch;
  int newflags;
  int n;


  switch (class) {

  case -1:
    ttydev = open("/dev/tty", O_RDWR, 0);
    if (ttydev < 0) {
      perror(" error opening /dev/tty");
      fatal(NULL);
    }
    if (fcntl(ttydev, F_GETFL, ttyflags) == -1) {
      perror(" unable to get tty flags");
      fatal(NULL);
    }
    FD_ZERO(&readfds);
    FD_SET(ttydev, &readfds);
    break;

  case 0:
    if (T_INST) fprintf(stderr," OCP '%02o%02o\n", func, device);
    break;

  case 1:
    if (T_INST) fprintf(stderr," SKS '%02o%02o\n", func, device);
    if (needflush) {
      fflush(stdout);
      needflush = 0;
      devpoll[device] = 0;
    }
    if (func == 7) {         /* skip if received a char */
      timeout.tv_sec = 0;
#if 0
      timeout.tv_usec = 100000;
#else
      timeout.tv_usec = 1;
#endif
      if (select(1, &readfds, NULL, NULL, &timeout) == 1)
	IOSKIP;
    } else if (func <= 014)
      IOSKIP;                     /* assume it's always ready */
    break;

  case 2:
    if (T_INST) fprintf(stderr," INA '%02o%02o\n", func, device);
    if (needflush) {
      fflush(stdout);
      needflush = 0;
      devpoll[device] = 0;
    }
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
readasr:
      n = read(ttydev, &ch, 1);
      if (n < 0) {
	if (errno != EAGAIN) {
	  perror(" error reading from tty");
	  fatal(NULL);
	}
      } else if (n == 1) {
	if (ch == '') {
	  traceflags = -1;
	  traceflags = ~TB_MAP;
	  fprintf(stderr,"\nTRACE ENABLED:\n\n");
	  memdump(0, 0xFFFF);
	  goto readasr;
	}
	if (func >= 010)
	  crs[A] = 0;
	/* NOTE: probably don't need 0x80 here... */
	crs[A] = crs[A] | ch | 0x80;
	if (T_INST) fprintf(stderr," character read=%o: %c\n", crs[A], crs[A]);
	IOSKIP;
      } else  if (n != 0) {
	printf("Unexpected error reading from tty, n=%d", n);
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
      if (ch > 0) {
	putchar(crs[A] & 0x7f);
	needflush = 1;
	devpoll[device] = instpermsec*100;
      }
      IOSKIP;                     /* OTA '0004 always works on Unix */
    } else if (func == 1) {       /* write control word */
      IOSKIP;
    } else if (04 <= func && func <= 07) {  /* write control register 1/2 */
      IOSKIP;
    } else {
      printf("Unimplemented OTA '04 function '%02o\n", func);
      fatal(NULL);
    }
    break;

  case 4:
    if (needflush) {
      fflush(stdout);
      needflush = 0;
      devpoll[device] = 0;
    }
  }
}




/* Device '14 - magtape controller #1
 */

void devmt (short class, short func, short device) {

  switch (class) {

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

void devcp (short class, short func, short device) {
  static short enabled = 0;
  static unsigned short clkvec;
  static short clkpic;

#if 0
  /* NOTE: setting this to 64,000 causes pointer faults when doing console
     I/O during coldstart:
OCP '1520
C Pointer fault, ea_s=6, ea_w=42266, [ea]=100015
 Pointer fault, ea_s=6, ea_w=42266, [ea]=100015

     Setting it to 300,000 causes:
CUnimplemented INA device '26 function '06
Fatal error: instruction #75661720 at 6/54305: 71404 101100
keys = 14000, modals=137

     Setting it to 600,000 works, but console output is very slow
     Setting it to 200,000 causes undefined INA '0626
     Setting it to 100,000 causes a hang after "Coldstarting PRIMOS..."
 */
  #define SETCLKPOLL devpoll[device] = 600000;
#else
  #define SETCLKPOLL devpoll[device] = instpermsec*(-clkpic*3.2)/1000;
#endif

  switch (class) {

  case 0:
    if (T_INST) fprintf(stderr," OCP '%02o%02o\n", func, device);
    printf("OCP '%02o%02o\n", func, device);

    if (func == 0 || func == 015) {
      fprintf(stderr,"Clock process initialized!\n");
      /* this enables tracing when the clock process initializes */
      //traceflags = ~TB_MAP;
      enabled = 1;
#if 0
      SETCLKPOLL;
#else
      devpoll[device] = 600000;
#endif
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
      crs[A] != 0120;               /* this is the SOC board */
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
      printf("Clock PIC interval set to %d\n", clkpic);
    } else if (func == 07) {
      printf("Clock control register set to '%o\n", crs[A]);
    } else if (func == 013) {
      clkvec = crs[A];
      printf("Clock interrupt vector address = '%o\n", clkvec);
    } else if (func == 017) {           /* write lights */
    } else {
      printf("Unimplemented OTA device '%02o function '%02o\n", device, func);
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

void devdisk (short class, short func, short device) {

#define S_HALT 0
#define S_RUN 1
#define S_INT 2

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
    } unit[4];
  } dc[64];

  short i,u;

  /* temps for running channel programs */

  unsigned short order;
  unsigned short m,m1,m2;
  short head, track, rec, recsize, nwords;
  unsigned short dmareg, dmaaddr;
  short iobuf[4096];                       /* local I/O buf, before mapped I/O */
  short *iobufp;
  short dmanw, dmanw1, dmanw2;
  unsigned int utempl;
  char ordertext[8];
  int theads, spt, phyra;
  char devfile[8];
  char rtfile[16];          /* read trace file name */
  int rtnw;                 /* total number of words read (all channels) */

  switch (class) {

  case -1:
    for (i=0; i<64; i++) {
      dc[i].state = S_HALT;
      dc[i].status = 0100000;
      dc[i].usel = -1;
      for (u=0; u<4; u++) {
	dc[i].unit[u].rtfd = -1;
	dc[i].unit[u].theads = 40;
	dc[i].unit[u].spt = 9;
	dc[i].unit[u].devfd = -1;
	dc[i].unit[u].readnum = -1;
      }
    }
    break;
      
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
#if 0
    /* this happens when the clock poll rate is set to 100000 (?) */
    else if (func == 06)     /* don't know what this is... */
      return;
#endif
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
      printf("Unimplemented OTA device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    IOSKIP;
    break;

  case 4:   /* poll (run channel program) */

    while (dc[device].state == S_RUN) {
      m = get16(dc[device].oar);
      m1 = get16(dc[device].oar+1);
      if (T_INST || T_DIO) fprintf(stderr,"\nDIOC %o: %o %o %o\n", dc[device].oar, m, m1, get16(dc[device].oar+2));
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
	m2 = get16(dc[device].oar++);
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
	  fatal("DFORMAT channel order not implemented");
	} else if (order == 5) {

	  /* translate head/track/sector to drive record address */

	  phyra = (track*dc[device].unit[u].theads*dc[device].unit[u].spt) + head*9 + rec;
	  if (T_INST || T_DIO) fprintf(stderr, " Unix ra=%d, byte offset=%d\n", phyra, phyra*2080);
	  if (lseek(dc[device].unit[u].devfd, phyra*2080, SEEK_SET) == -1) {
	    perror("Unable to seek drive file");
	    fatal(NULL);
	  }
	  dc[device].unit[u].readnum++;
	  rtnw = 0;
	  while (dc[device].dmanch >= 0) {
	    dmareg = ((dc[device].dmachan & 036) << 1) | (dc[device].dmachan & 1);
	    dmanw = regs.sym.regdmx[dmareg];
	    dmanw = -(dmanw>>4);
	    dmaaddr = regs.sym.regdmx[dmareg+1];
	    if (T_INST || T_DIO) fprintf(stderr, " DMA channels: nch-1=%d, ['%o]='%o, ['%o]='%o, nwords=%d\n", dc[device].dmanch, dc[device].dmachan, regs.sym.regdmx[dmareg], dc[device].dmachan+1, dmaaddr, dmanw);
	    
	    /* later, use mapva to do page-based mapped I/O in pieces */

	    if (crs[MODALS] & 020)
	      iobufp = iobuf;
	    else
	      iobufp = mem+dmaaddr;
	    if (read(dc[device].unit[u].devfd, (char *)iobufp, dmanw*2) != dmanw*2) {
	      perror("Unable to read drive file");
	      fatal(NULL);
	      }
	    if (crs[MODALS] & 020)
	      for (i=0; i<dmanw; i++)
		put16(iobuf[i], dmaaddr+i);

#if 0
	    if (T_DIO) {
	      fprintf(stderr, "\nRT: Read #%d.%d, RA=%d, numwords=%d, memaddr=%o, Unix pos=%d\n", readnum, dmanch, phyra, dmanw, dmaaddr, phyra*2080+rtnw*2);
	      snprintf(rtfile,sizeof(rtfile),"rt/rt%d.%d", readnum, dmanch);
	      if ((rtfd = open(rtfile, O_WRONLY+O_CREAT, 0777)) == -1) {
		printf("Read trace filename: %s\n", rtfile);
		perror("Read trace file open");
		fatal(NULL);
	      }
/* NEEDS HELP! */
	      if (write(rtfd, mem+dmaaddr, dmanw*2) != dmanw*2) {
		perror("Unable to write read trace file");
		fatal(NULL);
	      }
	      close(rtfd);
	      rtnw += dmanw;
	      if (dmanw == 16)
		fprintf(stderr, "RT: cra=%d, bra=%d, cnt=%d, type=%d, next=%d, prev=%d\n", *(int *)(mem+dmaaddr), *(int *)(mem+dmaaddr+2), mem[dmaaddr+4], mem[dmaaddr+5], *(int *)(mem+dmaaddr+6), *(int *)(mem+dmaaddr+8));
	    }
#endif
	    regs.sym.regdmx[dmareg] = 0;
	    regs.sym.regdmx[dmareg+1] += dmanw;
	    dc[device].dmachan += 2;
	    dc[device].dmanch--;
	  }

	} else if (order == 6) {
	  fatal("DWRITE channel order not implemented");
	}
	break;

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
	  if ((dc[device].unit[u].devfd = open(devfile, O_RDONLY, 0)) == -1)
	    dc[device].status = 0100001;    /* not ready */
	}
	if (T_INST || T_DIO) fprintf(stderr," select unit %d\n", u);
	break;

      case 7: /* DSTALL = Stall */
	if (T_INST || T_DIO) fprintf(stderr," stall\n");
	devpoll[device] = instpermsec/5;
	return;

      case 9: /* DSTAT = Store status to memory */
	if (T_INST || T_DIO) fprintf(stderr, " store status='%o to '%o\n", dc[device].status, m1);
	put16(dc[device].status,m1);
	break;

      case 11: /* DOAR = Store OAR to memory (2 words) */
	if (T_INST || T_DIO) fprintf(stderr, " store OAR='%o to '%o\n", dc[device].oar, m1);
	put16(dc[device].oar,m1);
	break;

      case 13: /* SDMA = select DMA channel(s) to use */
	dc[device].dmanch = m & 017;
	dc[device].dmachan = m1;
	if (T_INST || T_DIO) fprintf(stderr, " set DMA channels, nch-1=%d, channel='%o\n", dc[device].dmanch, dc[device].dmachan);
#if 0
	dmareg = ((m1 & 036) << 1) | (m1 & 1);
	//for (i=0; i<4; i++)
	//fprintf(stderr, " dmareg+%d = %o  ", i, regs.sym.regdmx[dmareg+i]);
	//fprintf(stderr, "\n");

	dmanw1 = regs.sym.regdmx[dmareg];
	dmanw1 = -(dmanw1>>4);
	dmanw2 = regs.sym.regdmx[dmareg+4];
	dmanw2 = -(dmanw2>>4);
	if (T_INST || T_DIO) fprintf(stderr, " dmareg=%d, nch=%d, dmanw1=%d, dmanw2=%d\n", dmareg, dc[device].dmanch, dmanw1, dmanw2);
	if (dc[device].dmanch == 1 && dmanw1 == 1024 && dmanw2 == 16) {
	  if (T_INST || T_DIO) fprintf(stderr, " swapped DMA channels!\n");
	  utempl = *(int *)(regs.sym.regdmx+dmareg);
	  *(int *)(regs.sym.regdmx+dmareg) = *(int *)(regs.sym.regdmx+dmareg+4);
	  *(int *)(regs.sym.regdmx+dmareg+4) = utempl;
	}
#endif
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
