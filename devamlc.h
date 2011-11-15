/* 
  Implements the AMLC subsystem for Primos.  In earlier versions
  (r186), a more hardware-centric implementation was used that closely
  followed the design of Prime's real AMLC board.  For example, every
  board generated its own interrupts.  This version uses knowledge of
  how the Primos AMLDIM software operates for a more efficient
  implementation.  There are several key points:

  - up to 8 AMLC boards are supported, with 16 lines on each

  - an internal terminal server allocates lines as telnet connections
    are received.  Connections from specific IP addresses can be
    mapped to specific AMLC lines.

  - hardware serial ports are supported via USB serial devices and
    an amlc.cfg config file that ties specific AMLC lines to Unix
    devices.  Unix flow control can be enabled.

  - only QAMLC is implemented, not the older DMT-based boards

  - the last line of the last board is the only line with per-character
    interrupts enabled; this is the "clock line", and the baud rate
    set on this line determined the general polling frequency for
    AMLDIM.  This is simulated here with AMLCPOLL, and set to 10 polls
    per second - typical for a real Prime.

  - AMLDIM processes *every* AMLC board's input whenever *any* board
    interrupts with an end-of-range (EOR).  So to optimize, only the
    clock-line board generates EOR interrupts in this implementation.
    AMLDIM does not fill output queues on just an EOR interrupt.

  - AMLDIM fills *every* AMLC board's output queue only when the
    clock line board interrupts with the "character time interrupt"
    (CTI) bit set; it also reads *every* AMLC board's input.  This
    is why normal interactive typing works: no EOR occurs for single
    characters; they are read periodically when CTI occurs

  - the AMLDIM process needs the periodic clock line CTI interrupts
    to cause it to run periodically, but does not rely on the frequency
    of the interrupts for timing; it uses the Primos clock process
    timers for real-time activities.  This allows the emulator the
    flexibility of increasing the poll rate when there is demand.

  NOTE: varying the AMLC poll rates dynamically is the default, but
        this can lead to unpredictable performance.  If consistent,
	predictable performance is more important than absolute
	performance, MAXAMLCSPEEDUP can be set to 1.  Then this 
	implementation will function more or less like a real Prime.
    

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

AMLC status word (from AMLCT5):

     BITS                 DESCRIPTION
  
      1                   END OF RANGE INTERRUPT
      2                   CLOCK RUNNING
      3-6                 LINE # OF LINE WHOSE DATA SET STATUS HAS CHANGED
                          (VALID ONLY WHEN BIT 7 IS SET)
      7                   AMLC INTERRUPTING BECAUSE DATA SET STATUS HAS CHANGED
      8                   0 = AMLC RECEIVING INTO FIRST BUFFER
                          1 = AMLC RECEIVING INTO SECOND BUFFER
      9                   CHARACTER TIME INTRP INDICATION #1
      10                  MULTIPLE LINES HAD CHAR TIME INTERRUPT
      11                  INTERRUPTS ENABLED
      12                  0 = CONTROLLER IN DMT MODE
                          1 = CONTROLLER IN DMQ MODE
      13-16               LINE # OF LINE CAUSING CHARACTER TIME INTRP.

*/

/* this macro closes an AMLC connection - used in several places */

#define AMLC_CLOSE_LINE \
  /* printf("em: closing AMLC line %d on device '%o\n", lx, device); */ \
  write(dc[dx].fd[lx], "\r\nPrime session disconnected\r\n", 30); \
  close(dc[dx].fd[lx]); \
  dc[dx].fd[lx] = -1; \
  dc[dx].dss &= ~BITMASK16(lx+1); \
  dc[dx].connected &= ~BITMASK16(lx+1);

/* macro to setup the next AMLC poll */

#define AMLC_SET_POLL \
  if (devpoll[device] == 0 || devpoll[device] > AMLCPOLL*gvp->instpermsec/pollspeedup) \
    devpoll[device] = AMLCPOLL*gvp->instpermsec/pollspeedup;  /* setup another poll */


int devamlc (int class, int func, int device) {

#ifdef DEMO
  #define AMLCLINESPERBOARD 1
  #define MAXLINES 1
  #define MAXBOARDS 1
#else
  #define AMLCLINESPERBOARD 16
  #define MAXLINES 128
  #define MAXBOARDS 8
#endif

  /* check for 1 new connection every .1 seconds */

#define AMLCACCEPTSECS .10

  /* seconds to make outbound connections for dedicated (assigned) lines */

#define AMLCCONNECT 15

  /* AMLC poll rate (ms).  Max data rate = queue size*1000/AMLCPOLL
     The max AMLC output queue size is 1023 (octal 2000), so a poll
     rate of 100 (10 times per second) will generate about 10230 chars
     per second.  This rate may be further boosted by a factor of
     MAXAMLCSPEEDUP if we send out a full DMQ buffer of 1023
     characters. */

#define AMLCPOLL 100
#define MAXAMLCSPEEDUP 16

  /* DSSCOUNTDOWN is the number of carrier status requests that should
     occur before polling real serial devices.  Primos does a carrier
     check 5x per second.  All this is really good for is disconnecting
     logged-out terminals, so we poll the real status every 5 seconds. */

#define DSSCOUNTDOWN 25

  /* connection types for each line.  This _doesn't_ imply the line is
     actually connected, ie, an AMLC line may be tied to a specific 
     serial device (like a USB->serial gizmo), but the USB device may
     not be plugged in.  The connection type would be CT_SERIAL but
     the line's fd would be -1 and the "connected" bit would be 0 */
     
#define CT_SOCKET 1
#define CT_SERIAL 2
#define CT_DEDIP 3

  /* terminal states needed to process telnet connections.
     Ref: http://support.microsoft.com/kb/231866
     Ref: http://www.iana.org/assignments/telnet-options */

#define TS_DATA 0      /* data state, looking for IAC */
#define TS_IAC 1       /* have seen initial IAC */
#define TS_SUBOPT 2    /* inside a suboption */
#define TS_OPTION 3    /* inside an option */

  /* telnet protocol special characters */

#define TN_IAC 255
#define TN_WILL 251
#define TN_WONT 252
#define TN_DO 253
#define TN_DONT 254

  /* telnet options */

#define TN_BINARY 0   /* put telnet client in binary mode */
#define TN_ECHO 1     /* we echo, not telnet client */
#define TN_SGA 3      /* means this is a full-duplex connection */
#define TN_SUBOPT 250

  static short inited = 0;
  static int wascti = 0;
  static int anyeor = 0;
  static float pollspeedup = 1;
  //  static int baudtable[16] = {1200, 2400, 4800, 9600, 19200, 38400, 57600, 115200};
  static int baudtable[16] = {300, 1200, 9600, 19200, 9600, 38400, 57600, 115200};
  static int tsfd;                      /* socket fd for terminal server */
  static int haveob = 0;                /* true if there are outbound socket lines */
  static struct timeval timeout = {0, 0};
  static double acceptts = 0;           /* timestamp for next accept */
  static struct {
    unsigned short deviceid;            /* this board's device ID */
    unsigned short dmcchan;             /* DMC channel (for input) */
    unsigned short baseaddr;            /* DMT/Q base address (for output) */
    unsigned short intvector;           /* interrupt vector */
    unsigned short intenable;           /* interrupts enabled? */
    unsigned short interrupting;        /* am I interrupting? */
    unsigned short xmitenabled;         /* 1 bit per line */
    unsigned short recvenabled;         /* 1 bit per line */
    unsigned short ctinterrupt;         /* 1 bit per line */
    unsigned short dss;                 /* 1 bit per line */
    unsigned short connected;           /* 1 bit per line */
    unsigned short serial;              /* true if any CT_SERIAL lines */
    unsigned short dsstime;             /* countdown to dss poll */
             short fd[16];              /* Unix fd, 1 per line */
    unsigned short tstate[16];          /* telnet state */
    unsigned short lconf[16];           /* line configuration word */
    unsigned short ctype[16];           /* connection type for each line */
    unsigned int   obhost[16];          /* outbound telnet host */
    unsigned short obport[16];          /* outbound telnet port */
    unsigned int   obtimer[16];         /* outbound connect timer */
    unsigned short modemstate[16];      /* Unix modem state bits (serial) */
    unsigned short recvlx;              /* next line to check for recv data */
    char bufnum;                        /* 0=1st input buffer, 1=2nd */
    char eor;                           /* 1=End of Range on input */
  } dc[MAXBOARDS];

  int dx, dxsave, lx;
  char buf[1024];      /* max size of DMQ buffer */
  int i, j;
  int maxxmit;

  /* save this board's device id in the dc[] array so we can tell
     what order the boards should be in.  This is necessary to find
     the clock line: the line that controls the AMLC poll rate.  The
     last line on the last defined board is the clock line.  The
     emulator also assumes that there are no gaps in the AMLC board
     configuration, which I think is also a Primos requirement, ie,
     you can't have board '54 and '52, with '53 missing. */

  switch (device) {
  case 054: dx = 0; break;
#ifndef DEMO
  case 053: dx = 1; break;
  case 052: dx = 2; break;
  case 035: dx = 3; break;
  case 015: dx = 4; break;
  case 016: dx = 5; break;
  case 017: dx = 6; break;
  case 032: dx = 7; break;
#endif
  default:
    fprintf(stderr, "devamlc: non-AMLC device id '%o ignored\n", device);
    return -1;
  }
  dxsave = dx;

  switch (class) {

  case -1:

    /* this part of initialization only occurs once, no matter how
       many AMLC boards are configured.  Parts of the amlc device
       context that are emulator-specfic need to be initialized here,
       only once, because Primos may issue the OCP to initialize an
       AMLC board more than once.  This would interfere with the
       dedicated serial line setup. */

    if (!inited) {
      FILE *cfgfile;
      char devname[32];
      int lc;
      int tempport;
      struct hostent* host;
      char *p;
  
      /* initially, we don't know about any AMLC boards */

      for (dx=0; dx<MAXBOARDS; dx++) {
	dc[dx].deviceid = 0;
	dc[dx].connected = 0;
	dc[dx].serial = 0;
	for (lx = 0; lx < 16; lx++) {
	  dc[dx].fd[lx] = -1;
	  dc[dx].tstate[lx] = TS_DATA;
	  dc[dx].lconf[lx] = 0;
	  dc[dx].ctype[lx] = CT_SOCKET;
	  dc[dx].modemstate[lx] = 0;
	  dc[dx].obtimer[lx] = 0;
	}
	dc[dx].recvlx = 0;
      }

      /* read the amlc.cfg file.  This file has 3 uses:

         1. maps Prime async lines to real serial devices, like host
            serial ports or USB serial boxes.  

            Format: <line #> /dev/<Unix usb device name>

	 2. maps incoming telnet connections from a specific IP
            address to a specific Prime async line.  This is used for
            serial device servers with a serial port + serial device
            on one side and TCP/IP on the other.  When the serial
            device is turned on, the SDS initiates a telnet connection
            to the Prime, then serial data flows back and forth.

	    Format: <line #> tcpaddr         NOTE: no port number!

         3. specify Prime async lines that should be connected at all
            times to a specific IP address:port.  This is used for
            network printers for example.  If the connection dies, the
            emulator will try to reconnect periodically.

	    Format: <line #> tcpaddr:port    NOTE: has port number!

         Entries can be in any order, comment lines begin with # or
	 semi-colon.  If the line number begins with a zero, it is
	 assumed to be octal, otherwise decimal.  Lines in amlc.cfg
	 will not be used by the emulator's built-in terminal server.
      */

      if ((cfgfile = fopen("amlc.cfg", "r")) == NULL) {
	if (errno != ENOENT)
	  printf("em: error opening amlc config file: %s", strerror(errno));
      } else {
	lc = 0;
	while (fgets(buf, sizeof(buf), cfgfile) != NULL) {
	  int n;
	  lc++;
	  buf[sizeof(devname)] = 0;   /* don't let sscanf overwrite anything */
	  buf[strlen(buf)-1] = 0;    /* remove trailing nl */
	  if (strcmp(buf,"") == 0 || buf[0] == ';' || buf[0] == '#')
	    continue;
	  if (buf[0] == '0')
	    n = sscanf(buf, "%o %s", &i, devname);
	  else
	    n = sscanf(buf, "%d %s", &i, devname);
	  if (n != 2) {
	    printf("em: can't parse amlc config file line #%d: %s\n", lc, buf);
	    continue;
	  }
	  if (i < 0 || i >= MAXLINES) {
	    printf("em: amlc line # '%o (%d) out of range in amlc config file at line #%d: %s\n", i, i, lc, buf);
	    continue;
	  }
	  //printf("devamlc: lc=%d, line '%o (%d) set to device %s\n", lc, i, i, devname);
	  dx = i/16;
	  lx = i & 0xF;
	  if (devname[0] == '/') {      /* USB serial port */
	    int fd;
	    if ((fd = open(devname, O_RDWR | O_NONBLOCK | O_EXLOCK)) == -1) {
	      printf("em: error connecting AMLC line '%o (%d) to device %s: %s\n", i, i, devname, strerror(errno));
	      continue;
	    }
	    printf("em: connected AMLC line '%o (%d) to device %s\n", i, i, devname);
	    dc[dx].fd[lx] = fd;
	    dc[dx].connected |= BITMASK16(lx+1);
	    dc[dx].serial = 1;
	    dc[dx].ctype[lx] = CT_SERIAL;
	  } else {

	    /* might be IP address:port for outbound telnet (printers) */

	    if (strlen(devname) > MAXHOSTLEN) {
	      fprintf(stderr,"Line %d of amlc.cfg ignored: IP address too long\n", lc);
	      continue;
	    }
	    
	    /* break out host and port number; no port means this is
	       an incoming dedicated line: no connects out.  With a
	       port means we need to connect to it. */

	    if ((p=strtok(devname, PDELIM)) != NULL) {
	      host = gethostbyname(p);
	      if (host == NULL) {
		fprintf(stderr,"Line %d of amlc.cfg ignored: can't resolve IP address %s\n", lc, p);
		continue;
	      }
	      if ((p=strtok(NULL, DELIM)) != NULL) {
		tempport = atoi(p);
		if (tempport < 1 || tempport > 65000) {
		  fprintf(stderr,"Line %d of amlc.cfg ignored: port number %d out of range 1-65000\n", tempport, lc);
		  continue;
		}
	      } else
		tempport = 0;
	      dc[dx].obhost[lx] = *(unsigned int *)host->h_addr;
	      dc[dx].obport[lx] = tempport;
	      dc[dx].ctype[lx] = CT_DEDIP;
	      haveob = 1;
	      //printf("Dedicated socket, host=%x, port=%d, cont=%d, line=%d\n", dc[dx].obhost[lx], tempport, dx, lx);
	    }
	  }
	}
	fclose(cfgfile);
      }

      /* start listening for incoming telnet connections */

      if (tport != 0) {
	int optval, tsflags;
	struct sockaddr_in addr;
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
	if ((tsflags = fcntl(tsfd, F_GETFL)) == -1) {
	  perror("unable to get ts flags for AMLC");
	  fatal(NULL);
	}
	tsflags |= O_NONBLOCK;
	if (fcntl(tsfd, F_SETFL, tsflags) == -1) {
	  perror("unable to set ts flags for AMLC");
	  fatal(NULL);
	}
      } else
	fprintf(stderr, "-tport is zero, AMLC devices not started\n");
      inited = 1;
    }

    /* this part of initialization occurs for every AMLC board */

    dx = dxsave;
    if (!inited || tport == 0)
      return -1;

    dc[dx].deviceid = device;
    return 0;

  case 0:
    TRACE(T_INST, " OCP '%02o%02o\n", func, device);
    //printf(" OCP '%02o%02o\n", func, device);

    if (func == 012) {            /* set normal (DMT) mode */
      fatal("AMLC DMT mode not supported");

    } else if (func == 013) {     /* set diagnostic (DMQ) mode */
      ;

    } else if (func == 015) {     /* enable interrupts */
      dc[dx].intenable = 1;

    } else if (func == 016) {     /* disable interrupts */
      dc[dx].intenable = 0;

    } else if (func == 017) {     /* initialize AMLC */
      //printf("devamlc: Initializing controller '%d, dx=%d\n", device, dx);
      dc[dx].dmcchan = 0;
      dc[dx].baseaddr = 0;
      dc[dx].intvector = 0;
      dc[dx].intenable = 0;
      dc[dx].interrupting = 0;
      dc[dx].xmitenabled = 0;
      dc[dx].recvenabled = 0;
      dc[dx].ctinterrupt = 0;
      dc[dx].dss = 0;      /* NOTE: 1=asserted in emulator, 0=asserted on Prime */
      dc[dx].dsstime = DSSCOUNTDOWN;
      dc[dx].bufnum = 0;
      dc[dx].eor = 0;

    } else {
      printf("Unimplemented OCP device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 1:
    TRACE(T_INST, " SKS '%02o%02o\n", func, device);

    if (func == 04) {             /* skip if not interrupting */
      if (!dc[dx].interrupting)
	IOSKIP;

    } else {
      printf("Unimplemented SKS device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 2:
    TRACE(T_INST, " INA '%02o%02o\n", func, device);

    /* XXX: this constant is redefined because of a bug in the
       OSX Prolific USB serial driver at version 1.2.1r2.  They should be
       turning on bit 0100, but are turning on 0x0100. */
#define TIOCM_CD 0x0100    

    if (func == 00) {             /* input Data Set Sense (carrier) */
      if (dc[dx].serial) {        /* any serial connections? */
	if (--dc[dx].dsstime == 0) {
	  dc[dx].dsstime = DSSCOUNTDOWN;
	  for (lx = 0; lx < 16; lx++) {  /* yes, poll them */
	    if (dc[dx].ctype[lx] == CT_SERIAL) {
	      int modemstate;
	      if (ioctl(dc[dx].fd[lx], TIOCMGET, &modemstate))
		perror("devamlc: unable to get modem state");
	      else if (modemstate & TIOCM_CD)
		dc[dx].dss |= BITMASK16(lx+1);
	      else
		dc[dx].dss &= ~BITMASK16(lx+1);
	      if (modemstate != dc[dx].modemstate[lx]) {
		//printf("devamlc: line %d modemstate was '%o now '%o\n", lx, dc[dx].modemstate[lx], modemstate);
		dc[dx].modemstate[lx] = modemstate;
	      }
	    }
	  }
	}
      }
      //printf("devamlc: dss for device '%o = 0x%x\n", device, dc[dx].dss);
      putcrs16(A, ~dc[dx].dss);    /* to the outside world, 1 = no carrier */
      IOSKIP;

    } else if (func == 07) {       /* input AMLC status */
      dc[dx].interrupting = 0;
      putcrs16(A, 040000 | (dc[dx].bufnum<<8) | (dc[dx].intenable<<5) | (1<<4));
      if (anyeor) {
	putcrs16(A, getcrs16(A) | 0100000);
	for (dx=0; dx<MAXBOARDS; dx++)
	  dc[dx].eor = 0;
	anyeor = 0;
      }
      if (wascti) {
	putcrs16(A, getcrs16(A) | 0x8f);    /* last line cti */
	wascti = 0;
      }
      //printf("INA '07%02o returns 0x%x\n", device, getcrs16(A));
      IOSKIP;
      if (getcrs16(A) & 0100000) {
	maxxmit = 0;               /* ugh! sloppy... */
	goto dorecv;
      }

    } else if (func == 011) {      /* input ID */
      putcrs16(A, 020054);         /* 020000 = QAMLC */
      IOSKIP;

    } else {
      printf("Unimplemented INA device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 3:
    TRACE(T_INST, " OTA '%02o%02o\n", func, device);

    /* func 00 = Output Line # to read Data Set Status, only implemented
       on 2-board AMLC sets with full data set control (uncommon) */

    /* func 01 = Set Line Configuration.  Primos issues this on
       logged-out lines to drop DTR so it can test carrier.  It also
       drops DTR (configurable) after logout to physically disconnect
       the line, either telnet or over a modem */

    if (func == 01) {              /* set line configuration */
      lx = getcrs16(A) >> 12;
      //printf("OTA '01%02o: AMLC line %d new config = '%o, old config = '%o\n", device, lx, getcrs16(A) & 0xFFF, dc[dx].lconf[lx] & 0xFFF);

      switch (dc[dx].ctype[lx]) {

      case CT_SOCKET:
      case CT_DEDIP:
	if (!(getcrs16(A) & 0x400) && dc[dx].fd[lx] >= 0) {  /* if DTR drops, disconnect */
	  AMLC_CLOSE_LINE;
	}
	break;

	
      case CT_SERIAL: {
	int fd;
	    
	/* setup line characteristics if they have changed (check for
	   something other than DTR changing) */

	fd = dc[dx].fd[lx];
	if ((getcrs16(A) ^ dc[dx].lconf[lx]) & ~02000) {
	  int baud;
	  struct termios terminfo;

	  //printf("devamlc: serial config changed!\n");
	  if (tcgetattr(fd, &terminfo) == -1) {
	    fprintf(stderr, "devamlc: unable to get terminfo for device '%o line %d", device, lx);
	    break;
	  }
	  memset(&terminfo, 0, sizeof(terminfo));
	  cfmakeraw(&terminfo);
	  baud = baudtable[(getcrs16(A) >> 6) & 7];
	  if (cfsetspeed(&terminfo, baud) == -1)
	    perror("em: unable to set AMLC line speed");
	  else
	    printf("em: AMLC line %d set to %d bps\n", dx*16 + lx, baud);
	  terminfo.c_cflag = CREAD | CLOCAL;		// turn on READ and ignore modem control lines
	  switch (getcrs16(A) & 3) {              /* data bits */
	  case 0:
	    terminfo.c_cflag |= CS5;
	    break;
	  case 1:
	    terminfo.c_cflag |= CS7;    /* this is not a typo! */
	    break;
	  case 2:
	    terminfo.c_cflag |= CS6;    /* this is not a typo! */
	    break;
	  case 3:
	    terminfo.c_cflag |= CS8;
	    break;
	  }
	  if (getcrs16(A) & 020)
	    terminfo.c_cflag |= CSTOPB;
	  if (!(getcrs16(A) & 010)) {
	    terminfo.c_cflag |= PARENB;
	    if (!(getcrs16(A) & 4))
	      terminfo.c_cflag |= PARODD;
	  }

#if 0
	  /* on the Prime AMLC, flow control is done in software and is
	     specified with the AMLC "lword" - an AMLDIM (software)
	     variable.  To enable Unix xon/xoff, RTS/CTS, and DTR flow
	     control, bits 5 & 7 of the lconf word have been taken over by
	     the emulator.  Bit 6 is the state of DTR.  The values for
	     bits 5-7 are:

	     0_0 - no low-level flow control, like the Prime
	     0_1 - xon/xoff flow control (2413 becomes 3413)
	     1_0 - cts/rts flow control (2413 becomes 6413)
	     1_1 - dsr/dtr flow control (2413 becomes 7413)

	     NOTE: bit 11 also appears to be free, but Primos doesn't 
	     let it flow through to the AMLC controller. :(
	  */

	  switch ((getcrs16(A) >> 9) & 7) {
	  case 1: case 3:
	    terminfo.c_iflag |= IXON | IXOFF;
	    terminfo.c_cc[VSTART] = 0x11;
	    terminfo.c_cc[VSTOP] = 0x13;
	    break;
	  case 4: case 6:
	    terminfo.c_cflag |= CCTS_OFLOW | CRTS_IFLOW;
	    break;
	  case 5: case 7:
	    terminfo.c_cflag |= CDSR_OFLOW | CDTR_IFLOW;
	    break;
	  }
#else
	  /* for now, use bit 7: 2413 -> 3413 to enable Unix
	     (hardware) xon/xoff.  This is much more responsive than
	     Primos xon/xoff, but can't be used for user terminals
	     because software (Emacs) may want to disable xon/xoff,
	     and since it is really an lword (software) control set
	     with DUPLX$, the hardware control word we're using won't
	     get changed.  And xon/xoff will still be enabled.  This
	     Unix xon/xoff feature is good for serial I/O devices,
	     where it stays enabled. */

	  if (getcrs16(A) & 01000) {
	    terminfo.c_iflag |= IXON | IXOFF;
	    terminfo.c_cc[VSTART] = 0x11;
	    terminfo.c_cc[VSTOP] = 0x13;
	  }
#endif

#if 0
	  printf("em: set terminfo: iFlag %x  oFlag %x  cFlag %x  lFlag %x  speed %d\n",
		 terminfo.c_iflag,
		 terminfo.c_oflag,
		 terminfo.c_cflag,
		 terminfo.c_lflag,
		 terminfo.c_ispeed);
#endif
	  if (tcsetattr(fd, TCSANOW, &terminfo) == -1) {
	    fprintf(stderr, "devamlc: unable to set terminal attributes");
	    perror("devamlc error");
	  }
	}

	/* set DTR high (02000) or low if it has changed */

	if ((getcrs16(A) ^ dc[dx].lconf[lx]) & 02000) {
	  int modemstate;
	  //printf("devamlc: DTR state changed\n");
	  ioctl(fd, TIOCMGET, &modemstate);
	  if (getcrs16(A) & 02000)
	    modemstate |= TIOCM_DTR;
	  else {
	    modemstate &= ~TIOCM_DTR;
	    dc[dx].dsstime = 1;
	  }
	  ioctl(fd, TIOCMSET, &modemstate);
	}
	break;
      }

      default:
	fatal("devamlc: unrecognized connection type");
      }

      /* finally, update line config */

      dc[dx].lconf[lx] = getcrs16(A);
      IOSKIP;

    } else if (func == 02) {      /* set line control */
      lx = (getcrs16(A)>>12);
      //printf("OTA '02%02o: AMLC line %d control = %x\n", device, lx, getcrs16(A));
      if (getcrs16(A) & 040)           /* character time interrupt enable/disable */
	dc[dx].ctinterrupt |= BITMASK16(lx+1);
      else
	dc[dx].ctinterrupt &= ~BITMASK16(lx+1);
      if (getcrs16(A) & 010)           /* transmit enable/disable */
	dc[dx].xmitenabled |= BITMASK16(lx+1);
      else
	dc[dx].xmitenabled &= ~BITMASK16(lx+1);
      if (getcrs16(A) & 01)            /* receive enable/disable */
	dc[dx].recvenabled |= BITMASK16(lx+1);
      else
	dc[dx].recvenabled &= ~BITMASK16(lx+1);
      if (dc[dx].ctinterrupt) {
	AMLC_SET_POLL;
      }
      IOSKIP;

      /* this is a new "experimental" OTA */
    } else if (func == 03) {      /* set room in input buffer */
      IOSKIP;

    } else if (func == 014) {      /* set DMA/C channel (for input) */
      dc[dx].dmcchan = getcrs16(A) & 0x7ff;
      if (!(getcrs16(A) & 0x800))
	fatal("Can't run AMLC in DMA mode!");
      IOSKIP;

    } else if (func == 015) {      /* set DMT/DMQ base address (for output) */
      dc[dx].baseaddr = getcrs16(A);
      IOSKIP;

    } else if (func == 016) {      /* set interrupt vector */
      dc[dx].intvector = getcrs16(A);
      IOSKIP;

    } else if (func == 017) {      /* set programmable clock constant */
      baudtable[4] = 115200/(getcrs16(A)+1);
      printf("em: AMLC baud rates are");
      for (i=0; i<8; i++)
	printf(" %d", baudtable[i]);
      printf(" bps\n");
      IOSKIP;

    } else {
      printf("Unimplemented OTA device '%02o function '%02o, A='%o\n", device, func, getcrs16(A));
      fatal(NULL);
    }
    break;

  case 4: {
    struct timeval tv;
    double ts;
    int neweor, activelines;
    //printf("poll device '%o, speedup=%5.2f, cti=%x, xmit=%x, recv=%x, dss=%x\n", device, pollspeedup, dc[dx].ctinterrupt, dc[dx].xmitenabled, dc[dx].recvenabled, dc[dx].dss);
    
    /* check for 1 new telnet connection every AMLCACCEPTSECS seconds
       (10 times per second) */

    if (gettimeofday(&tv, NULL) != 0)
      fatal("amlc gettimeofday failed");
    ts = tv.tv_sec + tv.tv_usec/1000000.0;
    if (ts > acceptts) {
      struct sockaddr_in addr;
      unsigned int addrlen;
      int fd;
      acceptts += AMLCACCEPTSECS;
      addrlen = sizeof(addr);
      fd = accept(tsfd, (struct sockaddr *)&addr, &addrlen);
      if (fd == -1) {
	if (errno != EWOULDBLOCK && errno != EINTR) {
	  perror("accept error for AMLC");
	}
      } else {
	int allbusy;
	unsigned int ipaddr;
	char ipstring[16];

	ipaddr = ntohl(addr.sin_addr.s_addr);
	//snprintf(ipstring, sizeof(ipstring), "%d.%d.%d.%d", (ipaddr&0xFF000000)>>24, (ipaddr&0x00FF0000)>>16, (ipaddr&0x0000FF00)>>8, (ipaddr&0x000000FF)); printf("Connect from IP %s\n", ipstring);

	/* if there are dedicated AMLC lines (specific IP address), we
	   have to make 2 passes: the first pass checks for IP address
	   matches; if none match, the second pass looks for a free
	   line.  If there are no dedicated AMLC lines (haveob is 0),
	   don't do this pass (j starts at 1) */

	allbusy = 1;
	for (j=!haveob; j<2; j++)
	  for (i=0; dc[i].deviceid && i<MAXBOARDS; i++)
	    for (lx=0; lx<AMLCLINESPERBOARD; lx++) {
	      /* NOTE: don't allow connections on clock line */
	      if (lx == 15 && (i+1 == MAXBOARDS || !dc[i+1].deviceid))
		  break;
	      if ((j == 0 && dc[i].ctype[lx] == CT_DEDIP && dc[i].obhost[lx] == ipaddr) ||
		  (j == 1 && dc[i].ctype[lx] == CT_SOCKET && dc[i].fd[lx] < 0)) {
		allbusy = 0;
		if (dc[i].fd[lx] >= 0)
		  close(dc[i].fd[lx]);
		dc[i].dss |= BITMASK16(lx+1);
		dc[i].connected |= BITMASK16(lx+1);
		dc[i].fd[lx] = fd;
		dc[i].tstate[lx] = TS_DATA;
		//printf("em: AMLC connection, fd=%d, device='%o, line=%d\n", fd, dc[i].deviceid, lx);
		goto endconnect;
	      }
	    }
  endconnect:
	if (allbusy) {
	  //warn("No free AMLC connection");
	  strcpy(buf,"\r\nAll available connections are in use.\r\n\n");
	  write(fd, buf, strlen(buf));
	  close(fd);
	} else {
	  int optval, tsflags;
	  int ttyfd;

	  if ((tsflags = fcntl(fd, F_GETFL)) == -1) {
	    perror("unable to get ts flags for AMLC line");
	  }
	  tsflags |= O_NONBLOCK;
	  if (fcntl(fd, F_SETFL, tsflags) == -1) {
	    perror("unable to set ts flags for AMLC line");
	  }
	  optval = 1;
	  if (setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, &optval, sizeof(optval)) == -1)
	    perror("unable to set TCP_NODELAY");

	  /* these Telnet commands put the connecting telnet client
	     into character-at-a-time mode and binary mode.  Since
	     these probably display garbage for other connection
	     methods, this stuff might be better off in a very thin
	     connection server */

	  buf[0] = TN_IAC;
	  buf[1] =   TN_WILL;
	  buf[2] =   TN_ECHO;
	  buf[3] = TN_IAC;
	  buf[4] =   TN_WILL;
	  buf[5] =   TN_SGA;
	  buf[6] = TN_IAC;
	  buf[7] =   TN_DO;
	  buf[8] =   TN_BINARY;
	  write(fd, buf, 9);

	  /* send out the ttymsg greeting */

	  if ((ttyfd = open("ttymsg", O_RDONLY, 0)) >= 0) {
	    int n;
	    n = read(ttyfd, buf, sizeof(buf));
	    if (n > 0)
	      write(fd, buf, n);
	    close(ttyfd);
	  } else if (errno != ENOENT) {
	    perror("Unable to open ttymsg file");
	  }
	}
      }
    }

    /* do a transmit scan loop for every line.  This loop is fairly
       efficient because Primos turns off xmitenable on DMQ lines
       after a short time (5 seconds) w/o any output.

       NOTE: it's important to do xmit processing even if a line is
       not currently connected, to drain the tty output buffer.
       Otherwise, the DMQ buffer fills, this stalls the tty output
       buffer, and when the next connection occurs on this line, the
       output buffer from the previous terminal session will be
       displayed to the new user. */

    maxxmit = 0;
    for (dx=0; dx<MAXBOARDS; dx++) {
      if (!dc[dx].deviceid || !dc[dx].xmitenabled)
	continue;
      for (lx = 0; lx < 16; lx++) {
	if (dc[dx].xmitenabled & BITMASK16(lx+1)) {
	  int n, maxn;
	  unsigned short qtop, qbot, qtemp,qseg, qmask, qents;
	  ea_t qentea, qcbea;
	  n = 0;
	  qcbea = dc[dx].baseaddr + lx*4;
	  if (dc[dx].connected & BITMASK16(lx+1)) {

	    /* this line is connected, determine max chars to write
	       XXX: maxn should scale, depending on the actual line
	       throughput and AMLC poll rate */

	    qtop = get16io(qcbea);
	    qbot = get16io(qcbea+1);
	    if (qtop == qbot)
	      continue;        /* queue is empty, try next line */
	    qseg = get16io(qcbea+2);
	    qmask = get16io(qcbea+3);
	    qents = (qbot-qtop) & qmask;
	    maxn = sizeof(buf);
	    /* XXX: for FTDI USB->serial chip, optimal request size
	       is a multiple of 62 bytes, ie, 992 bytes, not 1K */
	    if (qents < maxn)
	      maxn = qents;
	    qentea = MAKEVA(qseg & 0xfff, qtop);

	    /* pack DMQ characters into a buffer & fix parity
	       XXX: turning off the high bit at this low level
	       precludes the use of TTY8BIT mode... */

	    n = 0;
	    for (i=0; i < maxn; i++) {
	      unsigned short utemp;
	      utemp = get16mem(qentea);
	      qentea = (qentea & ~qmask) | ((qentea+1) & qmask);
	      //printf("Device %o, line %d, entry=%o (%c)\n", device, lx, utemp, utemp & 0x7f);
	      buf[n++] = utemp & 0x7F;
	    }
	  } else {

	    /* no line is connected; if this is a dedicated outbound
	       line, try to re-connect; otherwise, just drain queue */

	    if (dc[dx].ctype[lx] == CT_DEDIP) {
	      if (dc[dx].obtimer[lx] < tv.tv_sec) {

		int fd, sockflags;
		struct sockaddr_in raddr;
  
		dc[dx].obtimer[lx] = tv.tv_sec + AMLCCONNECT;
		printf("em: trying to connect to 0x%08x:%d\n", dc[dx].obhost[lx], dc[dx].obport[lx]);
		fd = socket(AF_INET, SOCK_STREAM, 0);
		if (fd < 0) {
		  perror("em: unable to create socket for outbound AMLC connection");
		  continue;
		}
		if ((sockflags = fcntl(fd, F_GETFL)) == -1) {
		  perror("em: unable to get flags for outbound AMLC connection");
		  continue;
		}
		sockflags |= O_NONBLOCK;
		if (fcntl(fd, F_SETFL, sockflags) == -1) {
		  perror("em: unable to set flags for outbound AMLC connection");
		  continue;
		}
		bzero((char *) &raddr, sizeof(raddr));
		raddr.sin_family = AF_INET;
                raddr.sin_addr.s_addr = dc[dx].obhost[lx];
		raddr.sin_port = dc[dx].obport[lx];
		if (connect(fd, (struct sockaddr *)&raddr, sizeof(raddr)) < 0 && errno != EINPROGRESS) {
		  perror("em: outbound AMLC connection failed");
		  continue;
		}
		dc[dx].fd[lx] = fd;
		dc[dx].connected |= BITMASK16(lx+1);
		printf("em: connected to 0x%08x:%d, fd=%d\n", dc[dx].obhost[lx], dc[dx].obport[lx], fd);
	      }
	    } else {
	      //printf("Draining output queue on line %d\n", lx);
	      put16io(get16io(qcbea), qcbea+1);
	    }
	  }

	  /* n chars have been packed into buf; see how many we can send */

	  if (n > 0) {
	    int nw;
	    nw = write(dc[dx].fd[lx], buf, n);
	    //if (nw != n) printf("devamlc: XMIT tried %d, wrote %d\n", n, nw);
	    if (nw > 0) {

	      /* nw chars were sent; for DMQ, update the queue head
		 top to reflect nw dequeued entries.

		 XXX: Might be good to keep write stats here, to
		 decide how many chars to dequeue above and/or how
		 often to do writes, or do a write select on each line
		 first.  There's overhead if large DMQ buffers are
		 used and Unix buffers get full so writes can't
		 complete */

	      qtop = (qtop & ~qmask) | ((qtop+nw) & qmask);
	      put16io(qtop, qcbea);
	      if (nw > maxxmit)
		maxxmit = nw;
	    } else if (nw == -1)
	      if (errno == EAGAIN || errno == EWOULDBLOCK)
		;

	      /* on Mac OSX, USB serial ports (Prolific chip) return
	         ENXIO - 'Device not configured" when USB is
	         unplugged.  If it is plugged back in, new device names
	         are created, so there's not much we can do except close
	         the fd. :( */

	      else if (errno == ENXIO && dc[dx].ctype[lx] == CT_SERIAL) {
		warn("USB serial device unplugged!  Reboot host.");
		AMLC_CLOSE_LINE;

	      } else if (errno == EPIPE || errno == ECONNRESET) {
		AMLC_CLOSE_LINE;

	      } else {
		perror("Writing to AMLC");
		fprintf(stderr," %d bytes written, but %d bytes sent\n", n, nw);
	      }
	  }
	}
      }
    }

    /* process input, but only as much as will fit into the DMC
       buffer.  

       Because the size of the AMLC tumble tables is limited, this
       could pose a denial of service issue.  Input is processed in a
       round to be more fair with this limited resource.

       Update 9/16/2011: the tumble table space is apportioned for
       each line with data waiting to be read.

       The AMLC tumble tables should never overflow, because we only
       read as many characters from the socket buffers as will fit in
       the tumble tables. However, the user input ring buffer may
       overflow, causing data from the terminal to be dropped. */

dorecv:
    neweor = 0;
    for (dx=0; dx<MAXBOARDS; dx++) {
      fd_set readfds;
      int nfds;
      if (dc[dx].deviceid == 0 || dc[dx].connected == 0 || dc[dx].eor)
	continue;

      /* select to see which lines have data to be read */

      FD_ZERO(&readfds);
      nfds = -1;
      for (lx = 0; lx < 16; lx++) {
	int fd;
	if ((dc[dx].connected & dc[dx].recvenabled & BITMASK16(lx+1))) {
	  fd = dc[dx].fd[lx];
	  FD_SET(fd, &readfds);
	  if (fd > nfds)
	    nfds = fd;
	}
      }
      activelines = select(nfds+1, &readfds, NULL, NULL, &timeout);
      if (activelines == -1) {
	if (errno == EINTR || errno == EAGAIN)
	  activelines = 0;
	else {
	  perror("devamlc: unable to do read select");
	  fatal(NULL);
	}
      }
      if (activelines) {
	int dmcpair, lcount;
	ea_t dmcea, dmcbufbegea, dmcbufendea;
	unsigned short dmcnw;
	if (dc[dx].bufnum)
	  dmcea = dc[dx].dmcchan + 2;
	else
	  dmcea = dc[dx].dmcchan;
	dmcpair = get32io(dmcea);
	dmcbufbegea = dmcpair>>16;
	dmcbufendea = dmcpair & 0xffff;
	dmcnw = dmcbufendea - dmcbufbegea + 1;
	//printf("AMLC: dmcnw=%d for %o, activelines=%d\n", dmcnw, dc[dx].deviceid, activelines);
	lx = dc[dx].recvlx;
	for (lcount = 0; lcount < 16 && dmcnw > 0; lcount++) {
	  int fd;
	  fd = dc[dx].fd[lx];
	  if (fd >= 0 && FD_ISSET(fd, &readfds)) {
	    int n, n2;

	    /* dmcnw is the # of characters left in the dmc buffer (each
	       character occupies 2 bytes or 1 16-bit word) */

	    n2 = dmcnw / activelines;
	    if (n2 > sizeof(buf))
	      n2 = sizeof(buf);
	    activelines -= 1;
	    n = read(fd, buf, n2);

	    //printf("processing recv on device %o, line %d, b#=%d, n2=%d, n=%d\n", device, lx, dc[dx].bufnum, n2, n);

	    /* zero length read means the fd has been closed */

	    if (n == 0) {
	      n = -1;
	      errno = EPIPE;
	    }
	    if (n == -1) {
	      n = 0;
	      if (errno == EAGAIN || errno == EWOULDBLOCK || errno == EINTR)
		;
	      else if (errno == EPIPE || errno == ECONNRESET || errno == ENXIO) {
		AMLC_CLOSE_LINE;
	      } else {
		perror("Reading AMLC");
	      }
	    }

	    /* very primitive support here for telnet - only enough to
	       ignore commands sent by the telnet client.  Telnet
	       commands could be split across reads and AMLC interrupts,
	       so a small state machine is used for each line.

	       For direct serial connections, the line stays in TS_DATA
	       state so no telnet processing occurs. */

	    if (n > 0) {
	      int tstate, toper;
	      //printf("devamlc: RECV dx=%d, lx=%d, b=%d, tried=%d, read=%d\n", dx, lx, dc[dx].bufnum, n2, n);
	      tstate = dc[dx].tstate[lx];
	      for (i=0; i<n; i++) {
		unsigned char ch;
		ch = buf[i];
		switch (tstate) {
		case TS_DATA:
		  if (ch == TN_IAC && (dc[dx].ctype[lx] == CT_SOCKET || dc[dx].ctype[lx] == CT_DEDIP))
		    tstate = TS_IAC;
		  else {
		    unsigned short utemp;
      storech:
		    utemp = lx<<12 | 0x0200 | ch;
		    put16io(utemp, dmcbufbegea);
		    //printf("******* stored character %o (%c) at %o\n", utemp, utemp & 0x7f, dmcbufbegea);
		    dmcbufbegea = INCVA(dmcbufbegea, 1);
		    dmcnw--;
		  }
		  break;
		case TS_IAC:
		  switch (ch) {
		  case TN_IAC:
		    tstate = TS_DATA;
		    goto storech;
		  case TN_WILL:
		  case TN_WONT:
		  case TN_DO:
		  case TN_DONT:
		    //printf("\nReceived command %d\n", ch);
		    tstate = TS_OPTION;
		    toper = ch;
		    break;
		  case TN_SUBOPT:
		    //printf("\nReceived SUBOPT command\n");
		    tstate = TS_SUBOPT;
		    break;
		  default:    /* ignore other chars after IAC */
		    //printf("\nReceived unknown IAC command %d\n", ch);
		    tstate = TS_DATA;
		  }
		  break;
		case TS_SUBOPT:
		  //printf("\nsubopt received %d\n", ch);
		  if (ch == TN_IAC)
		    tstate = TS_IAC;
		  break;
		case TS_OPTION:
		  //printf("\nReceived option %d\n", ch);
		  if (toper == TN_WILL) {
		    if (ch == TN_BINARY)
		      ;
		    else {
		      buf[0] = TN_IAC;
		      buf[1] =   TN_DONT;
		      buf[2] =   ch;
		      //printf("Sending DONT %d\n", ch);
		      write(fd, buf, 3);
		    }
		  } else if (toper == TN_DO) {
		    if (ch == TN_ECHO || ch == TN_SGA)
		      ;
		    else {
		      buf[0] = TN_IAC;
		      buf[1] =   TN_WONT;
		      buf[2] =   ch;
		      //printf("Sending WONT %d\n", ch);
		      write(fd, buf, 3);
		    }
		  }
		  tstate = TS_DATA;
		  break;
		default:
		  tstate = TS_DATA;
		}
	      }
	      dc[dx].tstate[lx] = tstate;
	    }
	  }
	  lx = (lx+1) & 0xF;
	}
	dc[dx].recvlx = lx;
	if (dmcbufbegea-1 > dmcbufendea)
	  fatal("AMLC tumble table overflowed?");
	put16io(dmcbufbegea, dmcea);
	if (dmcbufbegea > dmcbufendea) {          /* end of range has occurred */
	  dc[dx].bufnum = 1-dc[dx].bufnum;
	  dc[dx].eor = 1;
	  neweor = 1;
	  anyeor = 1;
	}
      }
    }

    /* time to interrupt? */

    dx = dxsave;
    if (class == 4)                    /* this was a poll */
      wascti = 1;
    if (dc[dx].intenable && (wascti || neweor)) {
      if (gvp->intvec == -1) {
	gvp->intvec = dc[dx].intvector;
	dc[dx].interrupting = 1;
      } else
	devpoll[device] = 100;         /* come back soon! */
    }

    /* the largest DMQ buffer size is 1023 chars.  If any line's DMQ
       buffer is getting filled completely, then we need to poll
       faster to increase throughput.  Otherwise, poll slower to
       decrease the interrupt rate.  We test against 1023 instead of
       the actual queue size, because if they want high performance,
       using a smaller queue size is not optimal and they probably
       don't care about performance.

       If any line had an eor this time, poll faster next time.
    */

    if (maxxmit >= 1023 || neweor) {
      if (pollspeedup < MAXAMLCSPEEDUP) {
	pollspeedup += .5;
	//printf("%d ", pollspeedup); fflush(stdout);
      }
    } else if (pollspeedup > 1) {
      pollspeedup -= .25;
      //printf("%d ", pollspeedup); fflush(stdout);
    }

    /* setup another poll */

    AMLC_SET_POLL;
    break;
  }
  }
}
