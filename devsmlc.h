#define DEBUG 0
/* 
  Implements the SMLC subsystem for Primos.

  - up to 2 SMLC boards are supported, with 4 lines on each

  - an smlc.cfg config file ties specific SMLC lines to TCP
    addresses of other emulators supporting HASP. 

  Primos SMLC usage:

  SKS '04xx
  - interrupt test

  OCP '00xx
  - enable high-speed SMLC clock

  OCP '13xx
  - acknowledge and clear interrupt

  OCP '15xx
  - enable interrupts

  OCP '16xx
  - disable interrupts

  OCP '17xx
  - initialize controller

  INA '00xx
  - read data set status

  INA '11xx
  - read device ID

  OTA '00xx
  - set function code
    '00nn set modem controls
    '10nn set special character
    '12nn set configuration word
    '14nn set primary I/O channel
    '15nn enable
    '16nn set backup I/O channel
    '17nn set status channel

  OTA '01xx
  - set function data

*/

#include <stdbool.h>
#include <time.h>

#define SMLC_DEVICEID      0050  /* HSSMLC/MDLC is '50, SMLC is '56 */

  /* SMLC poll rate (ms). */

#define SMLC_POLL          100  /* default polling rate in msec */

#define SMLC_BUFSIZE       2048
#define SMLC_LINESPERBOARD 4
#define SMLC_MAXBOARDS     2
#define SMLC_MAXLINES      8
#define SMLC_MAXSPCHARS    4
#define SMLC_RECVIX        0
#define SMLC_XMITIX        1

  /* Special EBCDIC characters for BSC protocol */

#define SMLC_SOH           0x01
#define SMLC_STX           0x02
#define SMLC_ETX           0x03
#define SMLC_DLE           0x10
#define SMLC_ITB           0x1f
#define SMLC_ETB           0x26
#define SMLC_ENQ           0x2d
#define SMLC_SYN           0x32
#define SMLC_EOT           0x37
#define SMLC_NAK           0x3d
#define SMLC_DLE           0x10
#define SMLC_ACK0          0x70

  /* MDLC status bits */

#define SMLC_XMIT          000001  /* transmit status */
#define SMLC_DSS_BIT       000010  /* data set status */
#define SMLC_ENCODE_BIT    000400  /* encoded status indicator */
#define SMLC_FORMAT_ERR    001000  /* format error status */
#define SMLC_LCT           001000  /* last character transmitted status */
#define SMLC_OVERUN_ERR    002000  /* overrun error status */
#define SMLC_TIMEFILL      003000  /* time fill status */
#define SMLC_BCC_GOOD      004000  /* good BCC status */
#define SMLC_NO_DATA       004000  /* data not available status */
#define SMLC_BCC_BAD       010000  /* bad BCC status */
#define SMLC_PARITY_ERR    014000  /* parity error status */
#define SMLC_EOC           020000  /* EOC status */
#define SMLC_EOR           040000  /* EOR status */

#define SMLC_DSR_BIT       000020  /* data set ready */
#define SMLC_CTS_BIT       000040  /* clear to send */
#define SMLC_DCD_BIT       000100  /* data carrier detect */
#define SMLC_SQ_BIT        000200  /* signal quality */

  /* Encoded status values */

#define SMLC_SOH_STATUS   (001<<4) /* encoded SOH-enq status */
#define SMLC_STX_STATUS   (003<<4) /* encoded DLE-STX status */
#define SMLC_ITB_STATUS   (004<<4) /* encoded DLE-ITB status */
#define SMLC_ETB_STATUS   (005<<4) /* encoded DLE-ETB status */
#define SMLC_ETX_STATUS   (006<<4) /* encoded ETX status */
#define SMLC_ACK0_STATUS  (007<<4) /* encoded ACK0 status */
#define SMLC_NAK_STATUS   (013<<4) /* encoded NAK status */
#define SMLC_ENQ_STATUS   (014<<4) /* encoded soh-ENQ status */

  /* seconds between attempts to make TCP connections */

#define SMLC_CONNECTINTERVAL 15

#if DEBUG
#define HexColumn(x) (3 * (x) + 4)
#define AsciiColumn(x) (HexColumn(16) + 2 + (x))
#define LogLineLength (AsciiColumn(16))

static void smlclogbytes(unsigned char *bytes, int len);
static void smlclogflush(void);

static FILE *smlclog = NULL;
static char smlclogbuf[LogLineLength + 1];
static int  smlclogbytescol = 0;
static char smlctimestamp[20];
#endif

/* connection states */

typedef enum {
  SMLC_STATEDISCONNECTED = 0,
  SMLC_STATECONNECTING,
  SMLC_STATECONNECTED,
  SMLC_STATEDISCONNECTING
} SmlcConnectionState;

/* HASP receive states */

typedef enum {
  SMLC_STATERCVSYN = 0,
  SMLC_STATERCVDLE,
  SMLC_STATERCVCHAR
} SmlcReceiveState;

/* controller interrupt states */

typedef enum {
  SMLC_NOTINTERRUPTING = 0,
  SMLC_INTERRUPTING,
  SMLC_INTERRUPTPENDING
} SmlcInterruptState;

#if DEBUG
static char *intstates[] = {
  "not interrupting",
  "interrupting",
  "interrupt pending"
};
#endif

/* send/receive buffer */

typedef struct smlcbuffer {
  unsigned short in;
  unsigned short out;
  char data[SMLC_BUFSIZE];
} SmlcBuffer;

/* Simplex Line Control Block. */

typedef struct slcb {                     /* simplex line control block */
           bool  enabled;                 /* TRUE if line enabled  */
           bool  dtr;                     /* TRUE if DTR enabled */
  unsigned short dmcprimary;              /* DMC primary channel */
  unsigned short dmcprimarycount;         /* DMC primary channel receive buffer count */
  unsigned short dmcprimaryidx;           /* DMC primary channel receive buffer index */
  unsigned short dmcbackup;               /* DMC backup channel */
  unsigned short dmcbackupcount;          /* DMC backup channel receive buffer count */
  unsigned short dmcbackupidx;            /* DMC backup channel receive buffer index */
  unsigned short configword;              /* control word */
           char  spchars[SMLC_MAXSPCHARS]; /* special characters */
           SmlcBuffer buf;                /* network I/O buffer */
} SLCB;

/* Physical Line Control Block. Each physical line comprises of two simples lines, a receive
   line, and a transit line */

typedef struct plcb {                     /* physical line control block */
           SmlcConnectionState connstate; /* connection state */
           SmlcReceiveState recvstate;    /* receive state */
           bool     starting;             /* TRUE if waiting for response to SOH-ENQ */
           bool     naksent;              /* TRUE if last frame sent was a NAK */
           int      fd;                   /* Unix fd */
           char     *remoteID;            /* remote TCP endpoint identifier */
           uint32_t host;                 /* TCP host */
  unsigned short    port;                 /* TCP port */
           time_t   nextconntime;         /* time of next connection attempt */
           SLCB     slines[2];            /* simplex lines, 0: receive, 1: transmit */
} PLCB;

/* Device control block. Each HSSMLC/MDLC device has four physical lines */

typedef struct smlcdcb{
  unsigned short deviceid;                /* this board's device ID */
  unsigned short fncode;                  /* OTA function code */
  unsigned short intvector;               /* interrupt vector */
           bool  intenabled;              /* interrupts enabled? */
           SmlcInterruptState intstate;   /* interrupt state */
  unsigned short lineno;                  /* OTA line number */
  unsigned short dmcstatus;               /* DMC status channel */
  PLCB plines[SMLC_LINESPERBOARD];        /* physical line control blocks */
} SmlcDCB;

static void smlcaddstatus(SmlcDCB *dcbp, int line, unsigned short status) {
  ea_t dmcbufbegea;
  ea_t dmcbufendea;
  unsigned int dmcpair;

  dmcpair = get32io(dcbp->dmcstatus);
  dmcbufbegea = dmcpair >> 16;
  dmcbufendea = dmcpair & 0xffff;
  if (dmcbufendea >= dmcbufbegea) {
#if DEBUG
    fprintf(smlclog, "%s Interrupt status '%06o added for line %d on device '%02o, next '%06o, last '%06o\n",
      smlctimestamp, status, line, dcbp->deviceid, dmcbufbegea, dmcbufendea);
#endif
    put16io(status + (line * 2), dmcbufbegea);
    dmcbufbegea = INCVA(dmcbufbegea, 1);
    put16io(dmcbufbegea, dcbp->dmcstatus);
    dcbp->intstate = SMLC_INTERRUPTPENDING;
#if DEBUG
  } else {
    fprintf(smlclog, "%s Interrupt status '%06o -NOT- added for line %d on device '%02o, next '%06o, last '%06o\n",
      smlctimestamp, status, line, dcbp->deviceid, dmcbufbegea, dmcbufendea);
#endif
  }
}

int devsmlc (int class, int func, int device) {

  static SmlcDCB dc[SMLC_MAXBOARDS];
  static short inited = 0;
  static int optenable = 1;
  static struct timeval timeout = {0, 0};

  char *bp;
  char buf[SMLC_BUFSIZE];
  int bufidx;
  unsigned char ch;
  time_t currenttime;
  uint16_t data;
  ea_t dmcbufbegea;
  ea_t dmcbufendea;
  short dmcnw;
  unsigned int dmcpair;
  int dx;
  char *ep;
  int fd;
  uint32_t hostaddr;
  int i;
  char *ip;
  PLCB *lp;
  bool isetb;
  int lx;
  int maxbytes;
  int maxfd;
  int n;
  int nbytes;
  char *np;
  char *op;
  socklen_t optlen;
  int optval;
  struct sockaddr_in raddr;
  fd_set readfds;
  int readycount;
  int rc;
  SLCB *sp;
  unsigned short status;
  int sx;
  unsigned short word;
  fd_set writefds;
#if DEBUG
  struct tm *tp;
  struct timespec sts;
#endif

  currenttime = time(0);
#if DEBUG
  tp = localtime(&currenttime);
  clock_gettime(CLOCK_REALTIME, &sts);
  sprintf(smlctimestamp, "%02d:%02d:%02d.%09d", tp->tm_hour, tp->tm_min, tp->tm_sec, sts.tv_nsec);
#endif

  switch (device) {
  case 050: dx = 0; break;
  case 051: dx = 1; break;
  default:
    fprintf(stderr, "Non-SMLC device id '%o ignored\n", device);
    return -1;
  }

  switch (class) {

  /* --------------------------------------------------------------------------- */
  /*   Initialize                                                                */
  /* --------------------------------------------------------------------------- */

  case -1:

    /* this part of initialization only occurs once, no matter how
       many SMLC boards are configured.  Parts of the SMLC device
       context that are emulator-specfic need to be initialized here,
       only once, because Primos may issue the OCP to initialize an
       SMLC board more than once. */

    if (!inited) {
      FILE *cfgfile;
      char devname[32];
      int lc;
      int tempport;
      struct hostent* host;
      char *p;

#if DEBUG
      smlclog = fopen("smlc.log", "wt");
      if (smlclog == NULL) {
        fprintf(stderr, "Failed to create smlclog.txt - aborting\n");
        fatal(NULL);
      }
      smlclogflush();    // initialize log buffer
#endif
  
      /* initially, we don't know about any SMLC boards */

      for (dx = 0; dx < SMLC_MAXBOARDS; dx++) {
        dc[dx].deviceid = 0;
        for (lx = 0; lx < SMLC_LINESPERBOARD; lx++) {
          dc[dx].plines[lx].connstate = SMLC_STATEDISCONNECTED;
          dc[dx].plines[lx].nextconntime = 0;
          dc[dx].plines[lx].fd = -1;
          dc[dx].plines[lx].remoteID = NULL;
          dc[dx].plines[lx].host = 0;
          dc[dx].plines[lx].port = 0;
        }
      }

      /* read the smlc.cfg file.  This file specifies sync lines that
         should be connected to TCP ports on other emulators supporting
         the host side of HASP RJE interfaces. The emulator establishes
         and maintains connections to the specified TCP ports. The
         format of each line in this file is:

            <line #> tcpaddr:port

         Entries can be in any order, comment lines begin with # or
         semi-colon. Line numbers range from 0 to 7. Line numbers 0 - 3
         will be associated with SMLC board 0 (device '50), and line
         numbers 4 - 7 will associated with SMLC board 1 (device '51).
      */

      if ((cfgfile = fopen("smlc.cfg", "r")) == NULL) {
        if (errno != ENOENT)
          fprintf(stderr, "Failed to open smlc.cfg: %s", strerror(errno));
      } else {
        lc = 0;
        while (fgets(buf, sizeof(buf), cfgfile) != NULL) {
          int n;
          lc++;
          buf[sizeof(devname)] = 0;  /* don't let sscanf overwrite anything */
          buf[strlen(buf) - 1] = 0;  /* remove trailing newline */
          if (buf[0] == '\0' || buf[0] == ';' || buf[0] == '#')
            continue;
          n = sscanf(buf, "%d %s", &i, devname);
          if (n != 2) {
            fprintf(stderr, "Can't parse smlc.cfg line #%d: %s\n", lc, buf);
            continue;
          }
          if (i < 0 || i >= SMLC_MAXLINES) {
            fprintf(stderr, "Line # %d out of range in smlc.cfg at line #%d: %s\n", i, lc, buf);
            continue;
          }
#if DEBUG
          fprintf(smlclog, "%s Line %d, SMLC line %d set to device %s\n", smlctimestamp, lc, i, devname);
#endif
          dx = i / SMLC_LINESPERBOARD;
          lx = i % SMLC_LINESPERBOARD;
          if (strlen(devname) > MAXHOSTLEN) {
            fprintf(stderr, "Line %d of smlc.cfg ignored: IP address too long\n", lc);
            continue;
          }
          tempport = 0;
          if ((p=strtok(devname, PDELIM)) != NULL) {
            host = gethostbyname(p);
            if (host == NULL) {
              fprintf(stderr, "Line %d of smlc.cfg ignored: can't resolve IP address %s\n", lc, p);
              continue;
            }
            if ((p=strtok(NULL, DELIM)) != NULL) {
              tempport = atoi(p);
              if (tempport < 1 || tempport > 65000) {
                fprintf(stderr, "Line %d of smlc.cfg ignored: port number %d out of range 1-65000\n", tempport, lc);
                continue;
              }
            }
          }
          if (tempport == 0) {
            fprintf(stderr, "Line %d of smlc.cfg ignored: no IP address or port number specified\n", lc);
            continue;
          }
          dc[dx].plines[lx].remoteID = (char *)malloc(32);
          hostaddr = ntohl(*(unsigned int *)host->h_addr);
          sprintf(dc[dx].plines[lx].remoteID, "%d.%d.%d.%d:%d", (hostaddr >> 24) & 0xff, (hostaddr >> 16) & 0xff,
            (hostaddr >> 8) & 0xff, hostaddr & 0xff, tempport);
          dc[dx].plines[lx].host = hostaddr;
          dc[dx].plines[lx].port = tempport;
#if DEBUG
          fprintf(smlclog, "%s TCP address, host=%x, port=%d, controller=%d, line=%d\n", smlctimestamp, dc[dx].plines[lx].host,
            tempport, dx, lx);
#endif
        }
        fclose(cfgfile);
      }
      inited = 1;
    }

    /* this part of initialization occurs for every SMLC board */

    if (!inited) return -1;

    dc[device - 050].deviceid = device;
    return 0;

  /* --------------------------------------------------------------------------- */
  /*   OCP                                                                       */
  /* --------------------------------------------------------------------------- */

  case 0:
    TRACE(T_INST, " OCP '%02o%02o\n", func, device);

    switch (func) {
 
   case 000: // enable high-speed SMLC clock
#if DEBUG
      fprintf(smlclog, "%s OCP '%02o%02o enable high-speed clock\n", smlctimestamp, func, device);
#endif
      IOSKIP;
      break;

    case 013: // acknowledge and clear interrupt
#if DEBUG
      fprintf(smlclog, "%s OCP '%02o%02o acknowledge and clear interrupt\n", smlctimestamp, func, device);
#endif
      dc[dx].intstate = SMLC_NOTINTERRUPTING;
      IOSKIP;
      break;

    case 015: // enable interrupts
#if DEBUG
      fprintf(smlclog, "%s OCP '%02o%02o enable interrupts\n", smlctimestamp, func, device);
#endif
      dc[dx].intenabled = 1;
      IOSKIP;
      break;

    case 016: // disable interrupts
#if DEBUG
      fprintf(smlclog, "%s OCP '%02o%02o disable interrupts\n", smlctimestamp, func, device);
#endif
      dc[dx].intenabled = 0;
      IOSKIP;
      break;

    case 017: // initialize controller
#if DEBUG
      fprintf(smlclog, "%s OCP '%02o%02o initialize controller\n", smlctimestamp, func, device);
#endif
      dc[dx].intvector  = 0;
      dc[dx].intenabled = 0;
      dc[dx].intstate   = SMLC_NOTINTERRUPTING;
      dc[dx].dmcstatus  = 0;
      for (lx = 0; lx < SMLC_LINESPERBOARD; lx++) {
        if (dc[dx].plines[lx].fd != -1) {
          close(dc[dx].plines[lx].fd);
          dc[dx].plines[lx].fd = -1;
          dc[dx].plines[lx].connstate = SMLC_STATEDISCONNECTED;
          dc[dx].plines[lx].nextconntime = 0;
        }
        sp = &dc[dx].plines[lx].slines[SMLC_RECVIX];
        sp->enabled = 0;
        sp->dtr = 0;
        sp->dmcprimary = 0;
        sp->dmcprimarycount = 0;
        sp->dmcprimaryidx = 0;
        sp->dmcbackup = 0;
        sp->dmcbackupcount = 0;
        sp->dmcbackupidx = 0;
        sp->configword = 0;
        sp->buf.in = 0;
        sp->buf.out = 0;

        sp = &dc[dx].plines[lx].slines[SMLC_XMITIX];
        sp->enabled = 0;
        sp->dtr = 0;
        sp->dmcprimary = 0;
        sp->dmcprimarycount = 0;
        sp->dmcprimaryidx = 0;
        sp->dmcbackup = 0;
        sp->dmcbackupcount = 0;
        sp->dmcbackupidx = 0;
        sp->configword = 0;
        sp->buf.in = 0;
        sp->buf.out = 0;
      }
      IOSKIP;
      break;

    default:
      fprintf(stderr, "Unimplemented OCP device '%02o function '%02o\n", device, func);
      fatal(NULL);
      break;
    }
    break;

  /* --------------------------------------------------------------------------- */
  /*   SKS                                                                       */
  /* --------------------------------------------------------------------------- */

  case 1:
    TRACE(T_INST, " SKS '%02o%02o\n", func, device);

    if (func == 004) { /* skip if not interrupting */
#if DEBUG
      fprintf(smlclog, "%s SKS '02%02o skip if not interrupting, state is %s\n", smlctimestamp, device, intstates[dc[dx].intstate]);
#endif
      if (dc[dx].intstate == SMLC_INTERRUPTING) {
        IOSKIP;
      }

    } else {
      fprintf(stderr, "Unimplemented SKS device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  /* --------------------------------------------------------------------------- */
  /*   INA                                                                       */
  /* --------------------------------------------------------------------------- */

  case 2:
    TRACE(T_INST, " INA '%02o%02o\n", func, device);

    if (func == 000) {       /* input SMLC status */
      dc[dx].intstate = SMLC_NOTINTERRUPTING;
      data = 0;
      for (lx = 3; lx >= 0; lx--) {
        data <<= 4;
        if (dc[dx].plines[lx].connstate == SMLC_STATECONNECTED) {
          data |= 017;
        }
      }
      putcrs16(A, data);
#if DEBUG
      fprintf(smlclog, "%s INA '00%02o input status returns 0x%04x\n", smlctimestamp, device, data);
#endif
      IOSKIP;

    } else if (func == 011) { /* report device ID */
      putcrs16(A, SMLC_DEVICEID);
#if DEBUG
      fprintf(smlclog, "%s INA '11%02o report device ID returns 0x%04x\n", smlctimestamp, device, getcrs16(A));
#endif
      IOSKIP;

    } else {
      fprintf(stderr, "Unimplemented INA device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  /* --------------------------------------------------------------------------- */
  /*   OTA                                                                       */
  /* --------------------------------------------------------------------------- */

  case 3:
    TRACE(T_INST, " OTA '%02o%02o\n", func, device);

    switch (func) {

    case 000: // set function code
      data = getcrs16(A);
      dc[dx].fncode = data >> 8;
      dc[dx].lineno = data &  0377;
#if DEBUG
      fprintf(smlclog, "%s OTA '00%02o set function/line '%03o/'%03o\n", smlctimestamp, device, dc[dx].fncode, dc[dx].lineno);
#endif
      switch (dc[dx].fncode) {
 
      case 000: // set modem controls
      case 010: // set special character
      case 012: // set configuration word
      case 014: // set primary I/O channel
      case 015: // enable
      case 016: // set backup I/O channel
      case 017: // set status channel
        IOSKIP;
        break;

      default:
        fprintf(stderr, "Unimplemented OTA device '%02o function code '%02o, line %d\n", device,
          dc[dx].fncode, dc[dx].lineno);
        fatal(NULL);
        break;
      }
      break;

    case 001: // set function data
      data = getcrs16(A);
      sx = dc[dx].lineno &  1;
      lx = dc[dx].lineno >> 1;
      lp = &dc[dx].plines[lx];
      sp = &lp->slines[sx];
      switch (dc[dx].fncode) {

      case 000: // set modem controls
#if DEBUG
        fprintf(smlclog, "%s OTA '01%02o line '%02o set modem controls '%06o\n", smlctimestamp, device, dc[dx].lineno, data);
        fprintf(smlclog, "%s     DTR: %d, RTS: %d\n", smlctimestamp, data & 1, (data & 2) == 1);
#endif
        sp->dtr = data & 1;
        if ((data & 1) == 0 && lp->fd != -1) {
#if DEBUG
          fprintf(smlclog, "%s     close connection to %s\n", smlctimestamp, lp->remoteID);
#endif
          close(lp->fd);
          lp->fd = -1;
          lp->connstate = SMLC_STATEDISCONNECTED;
          lp->nextconntime = 0;
        }
        IOSKIP;
        break;

      case 010: // set special character
        i = (data >> 8) & 0xff;
        ch = data & 0xff;
#if DEBUG
        fprintf(smlclog, "%s OTA '01%02o line '%02o set special character %d <%02x>\n", smlctimestamp, device, dc[dx].lineno, i, ch);
#endif
        if (i < SMLC_MAXSPCHARS) {
          sp->spchars[i] = ch;
        } else {
          fprintf(stderr, "Too many special characters specified for line '%02o of device '%02o\n", dc[dx].lineno, device);
          fatal(NULL);
        }
        IOSKIP;
        break;

      case 012: // set configuration word
#if DEBUG
        fprintf(smlclog, "%s OTA '01%02o line '%02o set configuration word 0x%04x\n", smlctimestamp, device, dc[dx].lineno, data);
#endif
        sp->configword = data;
        IOSKIP;
        break;

      case 014: // set primary I/O channel
#if DEBUG
        fprintf(smlclog, "%s OTA '01%02o line '%02o set primary I/O channel '%06o\n", smlctimestamp, device, dc[dx].lineno, data);
#endif
        sp->dmcprimarycount = (data >> 12) + 1;
        sp->dmcprimaryidx = 0;
        sp->dmcprimary = data & 0x7fe;
        if (data != 0) {
          if ((data & 04000) == 0) fatal("Can't run SMLC in DMA mode!");
#if DEBUG
          fprintf(smlclog, "%s   %d buffers\n", smlctimestamp, sp->dmcprimarycount);
          for (i = 0; i < sp->dmcprimarycount; i++) {
            dmcpair = get32io ((data & 0x7fe) + (i * 2));
            dmcbufbegea = dmcpair >> 16;
            dmcbufendea = dmcpair & 0xffff;
            dmcnw = (dmcbufendea - dmcbufbegea) + 1;
            fprintf(smlclog, "%s     %d: next '%06o, last '%06o, words %d\n", smlctimestamp, i, dmcbufbegea, dmcbufendea, dmcnw);
          }
#endif
        }
        IOSKIP;
        break;

      case 015: // enable
#if DEBUG
        fprintf(smlclog, "%s OTA '01%02o line '%02o enable '%06o (%s %s)\n", smlctimestamp, device, dc[dx].lineno, data,
          sx ? "xmit" : "recv", (data & 1) ? "on" : "off");
#endif
        sp->enabled = data & 1;
        if (sp->enabled) devpoll[device] = 1;
        IOSKIP;
        break;

      case 016: // set backup I/O channel
        /*
         *  NOTE: PRIMOS does not appear to use the backup i/O channel. In particular, PRIMOS rev 19 never issues
         *  this OTA function. Consequently, this emulation module does not currently implement any logic to alternate
         *  storing recceived data between the primary and backup channels.
         */
#if DEBUG
        fprintf(smlclog, "%s OTA '01%02o line '%02o set backup I/O channel '%06o\n", smlctimestamp, device, dc[dx].lineno, data);
#endif
        sp->dmcbackupcount = (data >> 12) + 1;
        sp->dmcbackupidx = 0;
        sp->dmcbackup = data & 0x7fe;
        if (data != 0) {
          if ((data & 04000) == 0) fatal("Can't run SMLC in DMA mode!");
#if DEBUG
          fprintf(smlclog, "%s   %d buffers\n", smlctimestamp, sp->dmcbackupcount);
          for (i = 0; i < sp->dmcbackupcount; i++) {
            dmcpair = get32io ((data & 0x7fe) + (i * 2));
            dmcbufbegea = dmcpair >> 16;
            dmcbufendea = dmcpair & 0xffff;
            dmcnw = (dmcbufendea - dmcbufbegea) + 1;
            fprintf(smlclog, "%s     %d: next '%06o, last '%06o, words %d\n", smlctimestamp, i, dmcbufbegea, dmcbufendea, dmcnw);
          }
#endif
        }
        IOSKIP;
        break;

      case 017: // set status channel
#if DEBUG
        fprintf(smlclog, "%s OTA '01%02o line '%02o set status channel '%06o\n", smlctimestamp, device, dc[dx].lineno, data);
#endif
        if (!(data & 04000) && data != 0)
          fatal("Can't run SMLC in DMA mode!");
        if (lx != 0 || sx != 0) {
          fprintf(stderr, "DMC status channel of SMLC device '%02o specified on line '%02o instead of '00!\n",
            device, dc[dx].lineno);
          fatal(NULL);
        }
        dc[dx].dmcstatus = data & 0x7fe;
#if DEBUG
        fprintf(smlclog, "%s   next %06o, last %06o\n", smlctimestamp, get32io(dc[dx].dmcstatus) >> 16, get32io(dc[dx].dmcstatus) & 0xffff);
#endif
        IOSKIP;
        break;

      default:
        fprintf(stderr, "Unimplemented OTA device '%02o SMLC function code '%02o\n", device, dc[dx].fncode);
        fatal(NULL);
        break;
      }
      break;

    case 016: // set interrupt vector
#if DEBUG
      fprintf(smlclog, "%s OTA '16%02o set interrupt vector '%06o\n", smlctimestamp, device, getcrs16(A));
#endif
      dc[dx].intvector = getcrs16(A);
      IOSKIP;
      break;

    default:
      fprintf(stderr, "Unimplemented OTA device '%02o function '%02o\n", device, func);
      fatal(NULL);
      break;
    }

  /* --------------------------------------------------------------------------- */
  /*   Poll                                                                      */
  /* --------------------------------------------------------------------------- */

  case 4:
    if (dc[dx].deviceid == 0) return 0;

    devpoll[device] = SMLC_POLL*gv.instpermsec;

    if (dc[dx].intenabled == 0 || dc[dx].intstate == SMLC_INTERRUPTING) return 0;

    if (dc[dx].intstate == SMLC_INTERRUPTPENDING) {
      devpoll[device] = 1;
      if (gv.intvec != -1) {
        devpoll[device] = 1; /* can't interrupt yet, try again ASAP */
      } else {
        gv.intvec = dc[dx].intvector;
        dc[dx].intstate = SMLC_INTERRUPTING;
#if DEBUG
        fprintf(smlclog, "%s Raise interrupt on device '%02o\n", smlctimestamp, device);
#endif
      }
      return 0;
    }

    maxfd = -1;
    FD_ZERO(&readfds);
    FD_ZERO(&writefds);

    for (lx = 0; lx < SMLC_LINESPERBOARD; lx++) {
      lp = &dc[dx].plines[lx];
      if (lp->host == 0) continue;
      switch (lp->connstate) {

      case SMLC_STATEDISCONNECTED:
        if (lp->nextconntime <= currenttime && lp->slines[SMLC_RECVIX].dtr != 0) {
          fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
          if (fd < 0) {
            fprintf(stderr, "Failed to create socket for SMLC line %d\n", (dx * SMLC_LINESPERBOARD) + lx);
            fatal(NULL);
          }
          setsockopt(fd, SOL_SOCKET, SO_KEEPALIVE, (void *)&optenable, sizeof(optenable));
          fcntl(fd, F_SETFL, O_NONBLOCK);
          bzero((char *) &raddr, sizeof(raddr));
          raddr.sin_family = AF_INET;
          raddr.sin_addr.s_addr = htonl(dc[dx].plines[lx].host);
          raddr.sin_port = htons(dc[dx].plines[lx].port);
          rc = connect(fd, (struct sockaddr *)&raddr, sizeof(raddr));
          lp->connstate = SMLC_STATECONNECTING;
          if (rc < 0 && errno != EINPROGRESS) {
            fprintf(stderr, "Failed to create connection to %s for SMLC line %d\n", lp->remoteID,
              (dx * SMLC_LINESPERBOARD) + lx);
            close(fd);
            lp->nextconntime = currenttime + SMLC_CONNECTINTERVAL;
            lp->connstate = SMLC_STATEDISCONNECTED;
            break;
          } else { // connection in progress
#if DEBUG
            fprintf(smlclog, "%s Connection initiated to %s for SMLC line %d\n", smlctimestamp, lp->remoteID,
              (dx * SMLC_LINESPERBOARD) + lx);
#endif
            lp->fd = fd;
          }
        }
        /* fall through when no error has occurred */

      case SMLC_STATECONNECTING:
        FD_SET(lp->fd, &writefds);
        if (lp->fd > maxfd) maxfd = lp->fd;
        break;

      case SMLC_STATECONNECTED:
        sp = &lp->slines[SMLC_RECVIX];
        if (sp->buf.out >= sp->buf.in) {
          sp->buf.in = sp->buf.out = 0;
        }
        if (sp->buf.in < sizeof(sp->buf.data)) {
          FD_SET(lp->fd, &readfds);
          if (lp->fd > maxfd) maxfd = lp->fd;
        }
        sp = &lp->slines[SMLC_XMITIX];
        if (sp->buf.out >= sp->buf.in) {
          sp->buf.in = sp->buf.out = 0;
        }
        if (sp->enabled && sp->dmcprimary != 0 && sp->dmcprimaryidx < sp->dmcprimarycount) {
          ip = &sp->buf.data[sp->buf.in];
          ep = &sp->buf.data[sizeof(sp->buf.data)];
          np = ip;
          while (np + 1 < ep && sp->dmcprimaryidx < sp->dmcprimarycount) {
            dmcpair = get32io(sp->dmcprimary + (sp->dmcprimaryidx * 2));
            dmcbufbegea = dmcpair >> 16;
            dmcbufendea = dmcpair & 0xffff;
            dmcnw = (dmcbufendea - dmcbufbegea) + 1;
            if (dmcnw <= 0) {
              sp->dmcprimaryidx += 1;
            } else {
#if DEBUG
              fprintf(smlclog, "%s Line %d on device '%02o send to %s from channel '%06o\n", smlctimestamp, lx, device, lp->remoteID,
                sp->dmcprimary);
              fprintf(smlclog, "%s   next '%06o, last '%06o, words %d\n", smlctimestamp, dmcbufbegea, dmcbufendea, dmcnw);
#endif
              n = 0;
              while (n < dmcnw && np + 1 < ep) {
                word = get16io(dmcbufbegea);
                dmcbufbegea = INCVA(dmcbufbegea, 1);
                *np++ = word >> 8;
                *np++ = word & 0377;
                n += 1;
              }
#if DEBUG
              fprintf(smlclog, "%s   %d words transferred to output buffer\n", smlctimestamp, n);
              fprintf(smlclog, "%s   next '%06o, last '%06o\n", smlctimestamp, dmcbufbegea, dmcbufendea);
#endif
              put16io(dmcbufbegea, sp->dmcprimary + (sp->dmcprimaryidx * 2));
              if (n == dmcnw) {
                status = SMLC_EOR | SMLC_XMIT;
                sp->dmcprimaryidx += 1;
                if (sp->dmcprimaryidx >= sp->dmcprimarycount) status |= SMLC_EOC;
                smlcaddstatus(&dc[dx], lx, status);
              } else if (n > dmcnw) {
                fatal("DMC channel overrun!");
              }
            }
          }
          if (np - op > 2  // trim possible trailing byte
              && (   (*(np - 2) == SMLC_ETB && *(np - 3) == SMLC_DLE)
                  || (*(np - 2) == SMLC_NAK && *(np - 3) == SMLC_SYN))) {
            np -= 1;
          }
          sp->buf.in += np - ip;
        }
        if (sp->buf.out < sp->buf.in) {
          FD_SET(lp->fd, &writefds);
          if (lp->fd > maxfd) maxfd = lp->fd;
        }
        break;

      case SMLC_STATEDISCONNECTING:
        close(lp->fd);
        lp->fd = -1;
        lp->nextconntime = currenttime + SMLC_CONNECTINTERVAL;
        lp->connstate = SMLC_STATEDISCONNECTED;
        return 0;

      default:
        fprintf(stderr, "Invalid state %d of line %d on device '%02o\n", lp->connstate, lx, device);
        fatal(NULL);
        break;
      }
    }

    if (maxfd < 0) return 0;

    readycount = select(maxfd + 1, &readfds, &writefds, NULL, &timeout);
    if (readycount < 1) return 0;

    for (lx = 0; lx < SMLC_LINESPERBOARD; lx++) {
      lp = &dc[dx].plines[lx];
      if (lp->host == 0) continue;
      switch (lp->connstate) {

      case SMLC_STATECONNECTING:
        if (FD_ISSET(lp->fd, &writefds)) {
          optlen = (socklen_t)sizeof(optval);
          rc = getsockopt(lp->fd, SOL_SOCKET, SO_ERROR, &optval, &optlen);
          if (rc < 0) {
            fprintf(stderr, "Failed to get socket status for SMLC line %d\n", (dx * SMLC_LINESPERBOARD) + lx);
            close(lp->fd);
            lp->fd = -1;
            lp->nextconntime = currenttime + SMLC_CONNECTINTERVAL;
            lp->connstate = SMLC_STATEDISCONNECTED;
          } else if (optval != 0) { // connection failed
            fprintf(stderr, "Failed to create connection to %s for SMLC line %d\n", lp->remoteID, (dx * SMLC_LINESPERBOARD) + lx);
            close(lp->fd);
            lp->fd = -1;
            lp->nextconntime = currenttime + SMLC_CONNECTINTERVAL;
            lp->connstate = SMLC_STATEDISCONNECTED;
          } else {
#if DEBUG
            fprintf(smlclog, "%s Connection created to %s for SMLC line %d\n", smlctimestamp, lp->remoteID, (dx * SMLC_LINESPERBOARD) + lx);
#endif
            lp->connstate = SMLC_STATECONNECTED;
            lp->recvstate = SMLC_STATERCVSYN;
            lp->starting = 0;
            lp->naksent = 0;
            lp->slines[SMLC_RECVIX].buf.in = 0;
            lp->slines[SMLC_RECVIX].buf.out = 0;
            lp->slines[SMLC_XMITIX].buf.in = 0;
            lp->slines[SMLC_XMITIX].buf.out = 0;
            smlcaddstatus(&dc[dx], lx, SMLC_DSS_BIT | SMLC_DSR_BIT | SMLC_CTS_BIT | SMLC_DCD_BIT | SMLC_SQ_BIT);
          }
        }
        if (lp->connstate != SMLC_STATECONNECTED)
          break;

      case SMLC_STATECONNECTED:
        sp = &lp->slines[SMLC_RECVIX];
        if (FD_ISSET(lp->fd, &readfds)) {
          n = read(lp->fd, &sp->buf.data[sp->buf.in], sizeof(sp->buf.data) - sp->buf.in);
          if (n > 0) {
#if DEBUG
            fprintf(smlclog, "%s Line %d on device '%02o received %d bytes from %s:\n", smlctimestamp, lx, device, n, lp->remoteID);
            smlclogbytes(&sp->buf.data[sp->buf.in], n);
            smlclogflush();
#endif
            sp->buf.in += n;
          } else if (n < 0 && (errno == EWOULDBLOCK || errno == EAGAIN || errno == EINTR)) {
            n = 0;
#if DEBUG
            fprintf(smlclog, "%s recv from %s for line %d returned errno %d (ignored)\n",
              smlctimestamp, lp->remoteID, (dx * SMLC_LINESPERBOARD) + lx, errno);
#endif
          } else {
#if DEBUG
            if (n < 0) {
              fprintf(smlclog, "%s recv from %s for line %d failed with errno %d\n",
                smlctimestamp, lp->remoteID, (dx * SMLC_LINESPERBOARD) + lx, errno);
            } else {
              fprintf(smlclog, "%s recv from %s for line %d returned EOF\n", smlctimestamp, lp->remoteID, (dx * SMLC_LINESPERBOARD) + lx);
            }
#endif
            lp->connstate = SMLC_STATEDISCONNECTING;
            smlcaddstatus(&dc[dx], lx, SMLC_DSS_BIT);
            devpoll[device] = 1; /* return ASAP */
            return 0;
          }
        }
        if (sp->enabled && sp->dmcprimary != 0 && sp->dmcprimaryidx < sp->dmcprimarycount) {
          ip = &sp->buf.data[sp->buf.in];
          op = &sp->buf.data[sp->buf.out];
          np = op;
          while (np < ip && sp->dmcprimaryidx < sp->dmcprimarycount) {
            dmcpair = get32io(sp->dmcprimary + (sp->dmcprimaryidx * 2));
            dmcbufbegea = dmcpair >> 16;
            dmcbufendea = dmcpair & 0xffff;
            dmcnw = (dmcbufendea - dmcbufbegea) + 1;
            if (dmcnw <= 0) {
              sp->dmcprimaryidx += 1;
              continue;
            }
#if DEBUG
            fprintf(smlclog, "%s Line %d on device '%02o receive from %s to channel '%06o\n", smlctimestamp, lx, device, lp->remoteID,
              sp->dmcprimary);
            fprintf(smlclog, "%s   next '%06o, last '%06o, words %d\n", smlctimestamp, dmcbufbegea, dmcbufendea, dmcnw);
#endif
            maxbytes = dmcnw * 2;
            n = 0;
            dmcnw = 0;
            status = 0;
            isetb = 0;
            while (n < maxbytes && np < ip) {
              ch = *np++;
              if (lp->recvstate == SMLC_STATERCVSYN) {
                if (ch == SMLC_SYN) {
                  continue;
                } else if (ch == SMLC_NAK) {
                  if (lp->naksent == 0) {
                    word = ch << 8;
                    n += 1;
                    status |= SMLC_ENCODE_BIT | SMLC_NAK_STATUS;
                    lp->recvstate = SMLC_STATERCVSYN;
                    break;
                  } else {
#if DEBUG
                    fprintf(smlclog, "%s NAK received after sending NAK to %s for line %d\n", smlctimestamp, lp->remoteID,
                      (dx * SMLC_LINESPERBOARD) + lx);
#endif
                    lp->connstate = SMLC_STATEDISCONNECTING;
                    smlcaddstatus(&dc[dx], lx, SMLC_DSS_BIT);
                    devpoll[device] = 1; /* return ASAP */
                    return 0;
                  }
                } else {
                  lp->recvstate = SMLC_STATERCVCHAR;
                }
              }
              if ((n & 1) != 0) {
                word |= ch;
                put16io(word, dmcbufbegea);
                dmcbufbegea = INCVA(dmcbufbegea, 1);
                dmcnw += 1;
              } else {
                word = ch << 8;
              }
              n += 1;
              if (lp->recvstate == SMLC_STATERCVDLE) {
                lp->recvstate = SMLC_STATERCVCHAR;
                if (ch == SMLC_ACK0) {
                  status |= SMLC_ENCODE_BIT | SMLC_ACK0_STATUS;
                  lp->recvstate = SMLC_STATERCVSYN;
                  break;
                } else if (ch == SMLC_STX) {
                  status |= SMLC_ENCODE_BIT | SMLC_STX_STATUS;
                  break;
                } else if (ch == SMLC_ETB) {
                  status |= SMLC_ENCODE_BIT | SMLC_ETB_STATUS;
                  lp->recvstate = SMLC_STATERCVSYN;
                  isetb = 1;
                  break;
                }
              } else if (ch == SMLC_DLE) {
                lp->recvstate = SMLC_STATERCVDLE;
              }
            }
            if ((n & 1) != 0) { // odd number of bytes processed, so store "incomplete" word, or back up one byte
              if (n < maxbytes
                  && ((n > 1 && *(np - 1) == SMLC_ETB && *(np - 2) == SMLC_DLE)
                      || (n == 1 && *(np - 1) == SMLC_NAK))) {
                put16io(word, dmcbufbegea);
                dmcbufbegea = INCVA(dmcbufbegea, 1);
                dmcnw += 1;
                n += 1;
              } else {
                np -= 1;
              }
            }
            put16io(dmcbufbegea, sp->dmcprimary + (sp->dmcprimaryidx * 2));
            sp->dmcprimaryidx += 1;
#if DEBUG
            fprintf(smlclog, "%s   %d words transferred from input buffer\n", smlctimestamp, dmcnw);
            fprintf(smlclog, "%s   next '%06o, last '%06o\n", smlctimestamp, dmcbufbegea, dmcbufendea);
#endif
            if (n == maxbytes) {
              status |= SMLC_EOR;
            } else if (n > maxbytes) {
              fatal("SMLC buffer overrun!");
            }
            if (np == ip || sp->dmcprimaryidx == sp->dmcprimarycount) {
              status |= SMLC_EOC | SMLC_EOR;
            } else if (np > ip || sp->dmcprimaryidx > sp->dmcprimarycount) {
              fatal("SMLC pointer or index off by 1!");
            }
            smlcaddstatus(&dc[dx], lx, status);
            if (isetb) {
              smlcaddstatus(&dc[dx], lx, SMLC_BCC_GOOD);
            }
            sp->buf.out += np - op;
          }
        }

        if (FD_ISSET(lp->fd, &writefds)) {
          sp = &lp->slines[SMLC_XMITIX];
          nbytes = sp->buf.in - sp->buf.out;
          if (nbytes > 0) {
            lp->starting = nbytes >= 6
              && sp->buf.data[sp->buf.out + 4] == SMLC_SOH
              && sp->buf.data[sp->buf.out + 5] == SMLC_ENQ;
            lp->naksent = nbytes >= 5 && sp->buf.data[sp->buf.out + 4] == SMLC_NAK;
            n = write(lp->fd, &sp->buf.data[sp->buf.out], nbytes);
            if (n >= 0) {
#if DEBUG
              fprintf(smlclog, "%s Line %d on device '%02o sent %d bytes to %s:\n", smlctimestamp, lx, device, n, lp->remoteID);
              smlclogbytes(&sp->buf.data[sp->buf.out], n);
              smlclogflush();
#endif
              sp->buf.out += n;
              devpoll[device] = 1;
              if (n >= nbytes && sp->dmcprimaryidx >= sp->dmcprimarycount) {
                smlcaddstatus(&dc[dx], lx, SMLC_LCT | SMLC_XMIT);
              }
            } else if (errno != EWOULDBLOCK && errno != EAGAIN && errno != EINTR) {
#if DEBUG
              fprintf(smlclog, "%s Connection failed to %s for line %d with errno %d\n", smlctimestamp, lp->remoteID,
                (dx * SMLC_LINESPERBOARD) + lx, errno);
#endif
              lp->connstate = SMLC_STATEDISCONNECTING;
              smlcaddstatus(&dc[dx], lx, SMLC_DSS_BIT);
              devpoll[device] = 1; /* return ASAP */
              return 0;
            }
          }
        }
        break;

      default:
        // nothing to do for other states
        break;
      }
    }

    if (dc[dx].intstate == SMLC_INTERRUPTPENDING) {
      devpoll[device] = 1; /* return ASAP */
    }
    break;
  }
  return 0;
}

#if DEBUG
static unsigned char ebcdicToAscii[256] = {
  /* 00-07 */  0x00,    0x01,    0x02,    0x03,    0x1a,    0x09,    0x1a,    0x7f,
  /* 08-0F */  0x1a,    0x1a,    0x1a,    0x0b,    0x0c,    0x0d,    0x0e,    0x0f,
  /* 10-17 */  0x10,    0x11,    0x12,    0x13,    0x1a,    0x1a,    0x08,    0x1a,
  /* 18-1F */  0x18,    0x19,    0x1a,    0x1a,    0x1c,    0x1d,    0x1e,    0x1f,
  /* 20-27 */  0x1a,    0x1a,    0x1a,    0x1a,    0x1a,    0x0a,    0x17,    0x1b,
  /* 28-2F */  0x1a,    0x1a,    0x1a,    0x1a,    0x1a,    0x05,    0x06,    0x07,
  /* 30-37 */  0x1a,    0x1a,    0x16,    0x1a,    0x1a,    0x1a,    0x1a,    0x04,
  /* 38-3F */  0x1a,    0x1a,    0x1a,    0x1a,    0x14,    0x15,    0x1a,    0x1a,
  /* 40-47 */  0x20,    0x1a,    0x1a,    0x1a,    0x1a,    0x1a,    0x1a,    0x1a,
  /* 48-4F */  0x1a,    0x1a,    0x5b,    0x2e,    0x3c,    0x28,    0x2b,    0x5d,
  /* 50-57 */  0x26,    0x1a,    0x1a,    0x1a,    0x1a,    0x1a,    0x1a,    0x1a,
  /* 58-5F */  0x1a,    0x1a,    0x21,    0x24,    0x2a,    0x29,    0x3b,    0x5e,
  /* 60-67 */  0x2d,    0x2f,    0x1a,    0x1a,    0x1a,    0x1a,    0x1a,    0x1a,
  /* 68-6F */  0x1a,    0x1a,    0x7c,    0x2c,    0x25,    0x5f,    0x3e,    0x3f,
  /* 70-77 */  0x1a,    0x1a,    0x1a,    0x1a,    0x1a,    0x1a,    0x1a,    0x1a,
  /* 78-7F */  0x1a,    0x60,    0x3a,    0x23,    0x40,    0x27,    0x3d,    0x22,

  /* 80-87 */  0x1a,    0x61,    0x62,    0x63,    0x64,    0x65,    0x66,    0x67,
  /* 88-8F */  0x68,    0x69,    0x1a,    0x1a,    0x1a,    0x1a,    0x1a,    0x1a,
  /* 90-97 */  0x1a,    0x6a,    0x6b,    0x6c,    0x6d,    0x6e,    0x6f,    0x70,
  /* 98-9F */  0x71,    0x72,    0x1a,    0x1a,    0x1a,    0x1a,    0x1a,    0x1a,
  /* a0-a7 */  0x1a,    0x7e,    0x73,    0x74,    0x75,    0x76,    0x77,    0x78,
  /* a8-aF */  0x79,    0x7a,    0x1a,    0x1a,    0x1a,    0x5b,    0x1a,    0x1a,
  /* b0-b7 */  0x1a,    0x1a,    0x1a,    0x1a,    0x1a,    0x1a,    0x1a,    0x1a,
  /* b8-bF */  0x1a,    0x1a,    0x1a,    0x1a,    0x1a,    0x5d,    0x1a,    0x1a,
  /* c0-c7 */  0x7b,    0x41,    0x42,    0x43,    0x44,    0x45,    0x46,    0x47,
  /* c8-cF */  0x48,    0x49,    0x1a,    0x1a,    0x1a,    0x1a,    0x1a,    0x1a,
  /* d0-d7 */  0x7d,    0x4a,    0x4b,    0x4c,    0x4d,    0x4e,    0x4f,    0x50,
  /* d8-dF */  0x51,    0x52,    0x1a,    0x1a,    0x1a,    0x1a,    0x1a,    0x1a,
  /* e0-e7 */  0x5c,    0x1a,    0x53,    0x54,    0x55,    0x56,    0x57,    0x58,
  /* e8-eF */  0x59,    0x5a,    0x1a,    0x1a,    0x1a,    0x1a,    0x1a,    0x1a,
  /* f0-f7 */  0x30,    0x31,    0x32,    0x33,    0x34,    0x35,    0x36,    0x37,
  /* f8-fF */  0x38,    0x39,    0x1a,    0x1a,    0x1a,    0x1a,    0x1a,    0x1a
};

static void smlclogflush(void) {
  if (smlclogbytescol > 0) {
    fputs(smlclogbuf, smlclog);
    fputc('\n', smlclog);
    fflush(smlclog);
  }
  smlclogbytescol = 0;
  memset(smlclogbuf, ' ', LogLineLength);
  smlclogbuf[LogLineLength] = '\0';
}

static void smlclogbytes(unsigned char *bytes, int len) {
  unsigned char ac;
  int ascCol;
  unsigned char b;
  char hex[3];
  int hexCol;
  int i;

  ascCol = AsciiColumn(smlclogbytescol);
  hexCol = HexColumn(smlclogbytescol);

  for (i = 0; i < len; i++) {
    b = bytes[i];
    ac = ebcdicToAscii[b];
    if (ac < 0x20 || ac >= 0x7f) {
      ac = '.';
    }
    sprintf(hex, "%02x", b);
    memcpy(smlclogbuf + hexCol, hex, 2);
    hexCol += 3;
    smlclogbuf[ascCol++] = ac;
    if (++smlclogbytescol >= 16) {
      smlclogflush();
      ascCol = AsciiColumn(smlclogbytescol);
      hexCol = HexColumn(smlclogbytescol);
    }
  }
}
#endif
