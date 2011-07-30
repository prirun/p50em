/* PNC (ring net) device handler

  On a real Prime ring network, each node has a unique node id from 1
  to 254.  The node id is configured with software and stored into the
  PNC during network initialization.  The node id of a master-cleared
  PNC is zero, and Primos may tell a PNC to connect to the ring with
  node id 0 so that the PNC will regenerate packets ("transceive") as
  they pass by.  The reason is that when a PNC disconnects from the
  ring, the segment between its two neighbors becomes longer, and
  there is a limit to the distance between active nodes.  Since no
  node ever has a real node id of zero, this PNC will not ack or
  receive any packets.  The broadcast node id is 255 (all PNCs accept
  packets with this "to" id).  In practice, CONFIG_NET limits the node
  id to 247.  When a node starts, it sends a message to itself.  If
  any system acks this packet, it means the node id is already in use
  and the new node will disconnect from the ring.  Since all systems
  in a ring have to be physically cabled together, there was usually a
  common network administration to ensure that no two nodes had the
  same node id.  Prime rings were often logically segmented: nodes A,
  B, C, D in a physical ring might be configured as two logical
  networks: AB and CD.  But all 4 systems must still have unique node
  ids since they are in the same physical ring.

  The emulation of the PNC controller uses TCP/IP and sockets to
  communicate with other emulators.  The main differences are unique
  point-to-point connections between individual systems, and a
  byte-stream interface vs the message-oriented interface of the real
  PNC.

  Because the emulated PNC has unique connections to each node, the
  unique node id concept isn't required and doesn't make a lot of
  sense: if 2 guys running the emulator have a 2-node Ringnet with
  their nodes numbered 1 and 2 (network A), and 2 other guys have a
  2-node network with their nodes numbered 1 and 2 (network B), then
  it wouldn't be possible for a node in network A to add a node in
  network B without one/both of them changing their node ids to ensure
  uniqueness.  To get around this, the emulator has a config file,
  ring.cfg, that lets you say "Here are the nodes in *my* ring (with
  all unique node ids), here is each node's IP address and port (to
  connect), and a 16-byte unique id used as a password and unique node
  identifier.  This allows the Prime node id to be a local number that
  doesn't need to be coordinated with remote emulators, and allows one
  emulator to be in multiple rings simultaneously.  The only items two
  people need to share are their IP/port addresses, the 16-byte unique
  id, and the node-to-node password in the Primenet config.  Actually,
  the Primenet config node-to-node password becomes redundant and
  should probably be left blank for ease in configuration. Cool, eh?

  Prior to rev 19.3, each node sends periodic "timer" messages to all
  nodes it knows about.  If the message returns ACK'd, the remote node
  is up.  Otherwise, the remote node must be down.  This is not very
  efficient, so beginning with 19.3, a broadcast timer message is sent
  every 10 seconds to let all nodes know that a machine is up.  On a
  100-node network, instead of sending 100*99=9900 timer messages,
  only 100 are required.  Unfortunately, the emulated PNC does not
  have the efficient broadcast mechanism of a physical token ring, so
  broadcasts must be simulated by sending packets to each node.  It
  would be possible to store the first broadcast timer message
  received from each node, and simply loop them all back within each
  emulator every 10 seconds as long as the TCP/IP connection is up.
  Not sure it's worth the trouble though...

  In early version of Primos, PNC ring buffers (physical packets) are
  256 words, with later versions of Primos also supporting 512, and
  1024 word packets.  "nbkini" (rev 18) allocates 12 ring buffers + 1
  for every 2 nodes in the ring.  Both the 1-to-2 queue for received
  packets, and 2-to-1 queue for xmit packets have 63 entries.  PNC
  ring buffers are wired and never cross page boundaries.

  The PNC controller data buffer has a 2-word header, followed by the
  data.  I believe the PNC hardware only looks at the first word.

       0: To (left) and From (right) bytes containing node-ids
       1: "Type" word. 
          Bit 1 set = odd number of bytes (if set, last word is only 1 byte)
          Bit 7 set = normal data messages (otherwise, a timer message)
	  Bit 16 set = broadcast timer message

  NOTE: at the hardware level, there are many other fields associated
  with a ring packet on the wire, for example, a CRC, ack byte, etc.
  These are generated and used only by the PNC hardware; the Prime
  software, even PNCDIM, never sees these fields.

  The PNC emulation expands the packet format during transmission by
  inserting a 16-bit byte count before the first word.  This is
  necessary since we're emulating a message-based protocol over a
  stream-based interface.  As a precaution, the emulator also adds the
  16-bit byte count at the end of the packet.  If these two counters
  don't match, there has been some kind of mishap in the PNC
  emulation.  These fields are similar to the CRC and ack byte of a
  real PNC: the Prime I/O bus never sees them.

  Because of TCP/IP streaming, it is important that a ring packet is
  sent in its entirety or not at all.  If there is a situation where a
  packet has been partially sent or received and can't be finished,
  the socket connection must be terminated and restarted.  An example
  of this is sending to a node with a suspended emulator.  The sending
  emulator's socket buffer is filled, a packet may be partially
  transmitted (to the socket buffer), then Primos eventually decides
  the transmit is taking too long and cancels the transmit.  There is
  no way to cancel the partial packet in the socket buffer.  The
  emulator uses SO_SNDLOWAT with 2052 to reserve 2052 bytes in the
  transmit socket buffer.  This should prevent partial packet
  transmissions.

  At a higher level, each ring buffer has an associated "block
  header", stored in a different location in system memory, that is 8
  words.  The block header is used by Prime software, not by the PNC
  hardware.  The BH fields are (16-bit words):
       0: type field (1)
       1: free pool id (3 for ring buffers)
       2-3: pointer to data block

       4-7 are available for use by drivers.  For the PNC:
       4: number of words received (calculated by pncdim based on DMA regs)
       5: receive status word
       6: data 1
       7: data 2

  Primos PNC usage:

  OCP '0007
  - disconnect from ring

  OCP '0207
  - inject a token into the ring

  OCP '0507
  - set PNC into "delay" mode

  OCP '1007
  - stop any xmit in progress

  INA '1707
  - read network status word
  - does this in a loop until "connected" bit is clear after disconnect above

  INA '1207
  - read receive status word

  INA '1307
  - read xmit status word

  OCP '1707
  - initialize

  OTA '1707
  - set my node ID (in A register)
  - if this fails, no PNC present

  OTA '1607
  - set interrupt vector (in A reg)

  OTA '1407
  - initiate receive, dma channel in A

  OTA '1507
  - initiate xmit, dma channel in A

  OCP '0107
  - connect to the ring
  - (Primos follows this with INA '1707 loop until "connected" bit is set)

  OCP '1407
  - ack receive (clears recv interrupt request)

  OCP '0407
  - ack xmit (clears xmit interrupt request)

  OCP '1507
  - set interrupt mask (enable interrupts)

  OCP '1107
  - stop receive in progress

*/

#ifndef HOBBY

/* PNC poll rate is 1/10th of a second */

#define PNCPOLL 100

/* PNC network status bits
     ACK = the node the packet was addressed to saw the packet correctly
     WACK = same as ACK, but the node couldn't accept the packet (no recv active)
     NAK = the packet was corrupt when some (any) node saw it

   NOTE: many fields are commented out since they can't occur in the
   emulator implementation; they're specified as documentation */

#define PNCNSRCVINT    0x8000  /* bit 1 rcv interrupt (rcv complete) */
#define PNCNSXMITINT   0x4000  /* bit 2 xmit interrupt (xmit complete) */
  //#define PNCNSPNC2      0x2000  /* bit 3 PNC II = 1 (aka pncbooster) */
  //#define PNCNSNEWMODE   0x1000  /* bit 4 PNC in "new"/II mode */
  //#define PNCNSERROR     0x0800  /* bit 5 u-verify failure or bad command (II) */
#define PNCNSCONNECTED 0x0400  /* bit 6 connected to ring */
  //#define PNCNSMULTOKEN  0x0200  /* bit 7 multiple tokens (only after xmit EOR) */
#define PNCNSTOKEN     0x0100  /* bit 8, token (only after xmit EOR) */
#define PNCNSNODEID    0x00FF  /* bits 9-16 node-id mask */

/* receive status word: first 8 bits are the "ACK byte" */

#define PNCRSACK       0x8000  /* ACK'd */
  //#define PNCRSMACK      0x4000  /* multiple ACK */
#define PNCRSWACK      0x2000  /* WACK'd */
  //#define PNCRSNAK       0x1000  /* NAK'd */
  //#define PNCRSABPE      0x0200  /* ack byte parity error */
  //#define PNCRSABCE      0x0100  /* ack byte check error */
  //#define PNCRSRBPE      0x0080  /* receive buffer parity error */
#define PNCRSBUSY      0x0040  /* receive busy */
#define PNCRSEOR       0x0020  /* EOR before end of message */

/* xmit status word: first 8 bits are the "ACK byte" */

#define PNCXSACK       0x8000  /* ACK'd */
  //#define PNCXSMACK      0x4000  /* multiple ACK */
#define PNCXSWACK      0x2000  /* WACK'd */
  //#define PNCXSNAK       0x1000  /* NAK'd */
  //#define PNCXSABPE      0x0200  /* ack byte parity error */
  //#define PNCXSABCE      0x0100  /* ack byte check error */
  //#define PNCXSXBPE      0x0080  /* transmit buffer parity error */
#define PNCXSBUSY      0x0040  /* xmit busy */
  //#define PNCXSNORET     0x0020  /* packet didn't return */
  //#define PNCXSRETBAD    0x0010  /* returned w/Ack byte or CRC error */
  //#define PNCXSRETRYFAIL 0x0008  /* retry not successful */
  //#define PNCXSRETRIES   0x0007  /* retry count */

#define MINACCEPTTIME 1      /* wait 1 second before accepting new connections */
#define MAXACCEPTTIME 5      /* only wait 5 seconds to receive uid */
#define MINCONNTIME 30       /* wait 30 seconds between connect attempts */
#define MAXPNCBYTES 2048     /* max of 2048 byte packets */
#define MAXPKTBYTES 2052     /* adds 16-bit length word to each end */

#define MAXHOSTLEN 64        /* max length of remote host name */
#define MAXUIDLEN 16         /* max length of unique id/password */

static short configured = 0;      /* true if PNC configured */
static unsigned short pncstat;    /* controller status word */
static unsigned short rcvstat;    /* receive status word */
static unsigned short xmitstat;   /* xmit status word */
static unsigned short pncvec;     /* PNC interrupt vector */
static unsigned short myid;       /* my PNC node id */
static unsigned short enabled;    /* interrupts enabled flag */
static int pncfd;                 /* socket fd for all PNC network connections */

/* the ni structure contains the important information for each node
   in the network and is indexed by the node id */

static struct {             /* node info for each node in my network */
  short cstate;             /* connection state (see below) */
  short fd;                 /* socket fd for this node, -1 if unconnected */
  char  host[MAXHOSTLEN+1]; /* host name/address of the remote node */
  short port;               /* emulator network port on the remote node */
  char  uid[MAXUIDLEN+1];   /* unique ID/password for this node (16 + null) */
  char  rcvpkt[MAXPKTBYTES];/* receive packet w/leading + trailing length */
  short rcvoffset;          /* next byte offset for receive */
  time_t conntime;          /* time of last connect attempt */
} ni[255];                  /* ring id 0-254 (255 is broadcast) */

/* PNC connection states.  This is a bit complex because the socket
   operations are all non-blocking and the unique ID has to be sent
   after connecting */

#define PNCCSNONE 0           /* not configured */
#define PNCCSDISC 1           /* not connected */
#define PNCCSCONN 2           /* connected */
#define PNCCSAUTH 3           /* unique ID / password sent */

/* array to map socket fd's to node id's for accessing the ni
   structure.  Not great programming, because the host OS could
   choose to use large fd's, which will cause a runtime error */

#define MAXFD 1023
#define FDMAPSIZE MAXFD+1

static short fdnimap[FDMAPSIZE];

/* xmit/recv buffer states and buffers.  The recv buffer must be
   static since data may be received over multiple devpnc calls.  The
   xmit buffer is on the stack: we copy whole packets into the socket
   buffer in 1 devpnc call */

#define PNCBSIDLE 0          /* initial state: no xmit or recv */
#define PNCBSRDY  1          /* ready to recv or xmit */
#define PNCBSXFER 2          /* transferring data over socket */

typedef struct {
  short toid, fromid;
  short state;
  short pktsize;           /* size of packet in bytes */
  short offset;
  unsigned short dmachan, dmareg, dmaaddr;
  short dmanw, dmabytesleft;
  unsigned short *memp;      /* ptr to Prime memory */
  unsigned char iobuf[MAXPKTBYTES];
} t_dma;
static t_dma rcv;
t_dma xmit;

/* return pointer to a hex-formatted uid */

char * pnchexuid(char * uid) {
  static char hexuid[MAXUIDLEN*2];
  int i;
  char ch;

  for (i=0; i<MAXUIDLEN; i++) {
    ch = uid[i] >> 4;
    if (0 <= ch  && ch <= 9)
      hexuid[i*2] = ch + '0';
    else
      hexuid[i*2] = ch - 10 + 'a';
    ch = uid[i] & 0xF;
    if (0 <= ch && ch <= 9)
      hexuid[i*2+1] = ch + '0';
    else
      hexuid[i*2+1] = ch - 10 + 'a';
  }
  return hexuid;
}

/* disconnect from a node */

unsigned short pncdisc(nodeid) {
  if (ni[nodeid].cstate > PNCCSDISC) {
    TRACE(T_RIO, " pncdisc: disconnect from node %d\n", nodeid);
    close(ni[nodeid].fd);
    fdnimap[ni[nodeid].fd] = -1;
    ni[nodeid].cstate = PNCCSDISC;
    ni[nodeid].fd = -1;
  }
}

/* initialize a socket fd for PNC emulation */

pncinitfd(int fd) {
  int optval, fdflags;

  if ((fdflags = fcntl(fd, F_GETFL, 0)) == -1) {
    perror("unable to get ts flags for PNC");
    fatal(NULL);
  }
  fdflags |= O_NONBLOCK;
  if (fcntl(fd, F_SETFL, fdflags) == -1) {
    perror("unable to set fdflags for PNC");
    fatal(NULL);
  }
  optval = MAXPKTBYTES;
  if (setsockopt(fd, SOL_SOCKET, SO_SNDLOWAT, &optval, sizeof(optval))) {
    perror("setsockopt failed for PNC");
    fatal(NULL);
  }
}

/* process a new incoming connection */

pncaccept(time_t timenow) {
  static time_t accepttime = 0;
  static int fd = -1;
  static struct sockaddr_in addr;
  static unsigned int addrlen;
  char uid[MAXUIDLEN+1];
  int i,n;

  if (!(pncstat & PNCNSCONNECTED))
    return;
  if (fd == -1) {
    if (timenow-accepttime < MINACCEPTTIME)
      return;
    accepttime = timenow;
    fd = accept(pncfd, (struct sockaddr *)&addr, &addrlen);
    if (fd == -1) {
      if (errno != EWOULDBLOCK && errno != EINTR && errno != EAGAIN)
	perror("accept error for PNC");
      return;
    }
    if (fd >= MAXFD)
      fatal("New connection fd is too big");
    TRACE(T_RIO, " new PNC connection, fd %d\n", fd);
  }

  /* PNC connect request seen:
     - read unique id / password
     - scan host table to find node id
     - if no matching unique id / password, display warning and disconnect
     - if already connected, display warning error and disconnect
  */

  if (timenow-accepttime > MAXACCEPTTIME) {
    fprintf(stderr, "devpnc: too long to receive uid\n");
    goto disc;
  }
  n = read(fd, uid, MAXUIDLEN);
  if (n == -1) {
    if (errno == EWOULDBLOCK || errno == EINTR || errno == EAGAIN)
      return;
    perror("error reading PNC uid");
    goto disc;
  }
  if (n != MAXUIDLEN) {
    fprintf(stderr, "devpnc: error reading uid, expected %d bytes, got %d\n", MAXUIDLEN, n);
    goto disc;
  }
  TRACE(T_RIO, " uid is %s for new PNC fd %d\n", pnchexuid(uid), fd);

  /* look up the uid in our ring.cfg array to determine the node id
     we'll use for this remote emulator */

  for (i=1; i<255; i++)
    if (memcmp(uid, ni[i].uid, MAXUIDLEN) == 0)
      break;
  if (i == 255) {
    fprintf(stderr, "devpnc: couldn't find uid %s\n", uid);
    goto disc;
  }
  if (ni[i].cstate > PNCCSDISC) {
    TRACE(T_RIO, " devpnc: already connected to node %d, uid %s\n", i, uid);
    if (memcmp(uid, ni[myid].uid, MAXUIDLEN) < 0)
      goto disc;
    else
      pncdisc(i);
  }
  ni[i].cstate = PNCCSAUTH;
  ni[i].fd = fd;
  fdnimap[fd] = i;
  fd = -1;
  return;

disc:
  close(fd);
  fd = -1;
}

/* connect to a node, without blocking */

unsigned short pncconn1(nodeid, timenow) {
  struct hostent* server;
  struct sockaddr_in addr;
  unsigned int addrlen;
  int fd;

  TRACE(T_RIO, "try connect to node %d\n", nodeid);
  ni[nodeid].conntime = timenow;
  if ((fd = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
    perror ("Unable to create socket");
    exit(1);
  }
  pncinitfd(fd);
  server = gethostbyname(ni[nodeid].host);
  if (server == NULL) {
    fprintf(stderr,"devpnc: cannot resolve %s\n", ni[nodeid].host);
    close(fd);
    return;
  }
  bzero((char *) &addr, sizeof(addr));
  addr.sin_family = AF_INET;
  bcopy((char *)server->h_addr, (char *)&addr.sin_addr.s_addr, server->h_length);
  addr.sin_port = htons(ni[nodeid].port);
  if (connect(fd, (void *) &addr,(socklen_t) sizeof(addr)) < 0 && errno != EINPROGRESS) {
    perror("devpnc: error connecting to node\n");
    close(fd);
    return;
  }
  ni[nodeid].fd = fd;
  ni[nodeid].cstate = PNCCSCONN;
}

/* send authorization uid / password after a connect */

pncauth1(int nodeid, time_t timenow) {
  int n;

  n = write(ni[nodeid].fd, ni[nodeid].uid, MAXUIDLEN);
  TRACE(T_RIO, "sent uid %s, hex %s, for node %d, fd %d, %d bytes\n", ni[nodeid].uid, pnchexuid(ni[nodeid].uid), nodeid, ni[nodeid].fd, n);
  if (n == MAXUIDLEN) {
    ni[nodeid].cstate = PNCCSAUTH;
    return;
  }
  if (n == -1) {
    if (errno == EWOULDBLOCK || errno == EINTR || errno == EAGAIN || errno == EINPROGRESS)
      return;
    if (errno != EPIPE)
      perror("error sending PNC uid");
    goto disc;
  }
  fprintf(stderr, "devpnc: expected %d bytes, only wrote %d bytes for uid\n", MAXUIDLEN, n);
  
disc:
  close(ni[nodeid].fd);
  ni[nodeid].fd = -1;
  ni[nodeid].cstate = PNCCSDISC;
}

/* connect to the next unconnected node.  We only try to connect to a
   remote system once: if they aren't up, we assume they will try to
   connect to us when they come up */

pncconnect(time_t timenow) {
  static int prevnode=0;
  int i, nodeid;

  if (myid == 0)
    return;
  i = prevnode;
  while (1) {
    i = (i % 254) + 1;
    if (i == prevnode)      /* went round once? */
      break;
    if (i == myid)          /* don't connect to myself */
      continue;
    if (ni[i].cstate == PNCCSCONN) {
      pncauth1(i, timenow);
      break;
    }
    //TRACE(T_RIO, "pncconnect: node %d, state %d, timenow %d, conntime %d\n", i, ni[i].cstate, timenow, ni[i].conntime);
    if (ni[i].cstate == PNCCSDISC && timenow - ni[i].conntime > MINCONNTIME) {
      pncconn1(i, timenow);
      break;
    }
  }
  prevnode = i;
}

/* initialize a dma transfer */

pncinitdma(t_dma *iob, char *iotype) {
  (*iob).dmachan = crs[A];
  (*iob).dmareg = (*iob).dmachan << 1;
  (*iob).dmanw = regs.sym.regdmx[(*iob).dmareg];
  if ((*iob).dmanw <= 0)
    (*iob).dmanw = -((*iob).dmanw>>4);
  else
    (*iob).dmanw = -(((*iob).dmanw>>4) ^ 0xF000);
  (*iob).dmaaddr = ((regs.sym.regdmx[(*iob).dmareg] & 3)<<16) | regs.sym.regdmx[(*iob).dmareg+1];
  (*iob).memp = MEM + mapio((*iob).dmaaddr);
  (*iob).state = PNCBSRDY;
  (*iob).offset = 0;
  TRACE(T_RIO, " pncinitdma: %s dmachan=%o, dmareg=%o, dmaaddr=%o, dmanw=%d\n", iotype, (*iob).dmachan, (*iob).dmareg, (*iob).dmaaddr, (*iob).dmanw);
}


unsigned short pncxmit1(short nodeid, t_dma *iob) {
  int nwritten, ntowrite;

  if (ni[nodeid].fd == -1) {        /* not connected yet */
    if (ni[nodeid].cstate != PNCCSNONE)
      TRACE(T_RIO, " can't transmit: not connected to node %d\n", nodeid);
    return 0;
  }
  if (ni[nodeid].cstate != PNCCSAUTH) { /* not authorized yet */
    TRACE(T_RIO, " can't transmit: not authorized to node %d\n", nodeid);
    return 0;
  }

  /* NOTE: setsockopt SO_SNDLOWAT ensures that a partial packet write
     doesn't occur here */

  ntowrite = (*iob).dmanw*2 + 4;
  if ((nwritten=write(ni[nodeid].fd, (*iob).iobuf, ntowrite)) < 0) {
    if (errno == EAGAIN || errno == EWOULDBLOCK) {
      TRACE(T_RIO, " wack packet to node %d\n", nodeid);
      return PNCXSWACK;
    } else {
      TRACE(T_RIO, " error writing packet to node %d: %s\n", nodeid, strerror(errno));
      pncdisc(nodeid);
      perror("devpnc: write error");
      return 0;
    }
  } else if (nwritten != ntowrite) {
    fprintf(stderr, "devpnc: pncxmit1 tried to write %d, wrote %d\n", ntowrite, nwritten);
    pncdisc(nodeid);
    return 0;
  }
  return PNCXSACK;
}

/* transmit a packet to its destination, either a single nodeid or
   255, the broadcast nodeid */

unsigned short pncxmit(t_dma *iob) {
  short  i;

  if ((*iob).toid == 255) {
    for (i=1; i<255; i++)
      xmitstat |= pncxmit1(i, iob);
  } else {
    xmitstat |= pncxmit1((*iob).toid, iob);
  }
  return xmitstat;
}

/* process receives on all connections */

pncrecv(time_t timenow) {
}

int devpnc (int class, int func, int device) {

  short i;
  short len;
  unsigned short dmaword;
  time_t timenow;
  struct sockaddr_in addr;
  unsigned int addrlen;
  int fd, optval, fdflags;

#define DELIM " \t\n"
#define PDELIM ":"

  FILE *ringfile;
  char *tok, buf[128], *p;
  int n, linenum;
  int tempid, tempport, cfgerrs;
  char temphost[MAXHOSTLEN+1];

  //gvp->traceflags = ~T_MAP;

  if (class >= 0 && !configured) {
    TRACE(T_INST|T_RIO, "PIO to PNC ignored, class=%d, func='%02o, device=%02o\n", class, func, device);
    return -1;
  }

  switch (class) {

  case -1:
    if (nport <= 0) {
      fprintf(stderr, "-nport is zero, PNC not started\n");
      return -1;
    }

    pncstat = 0;
    rcvstat = 0;
    xmitstat = 0;
    pncvec = 0;
    myid = 0;                 /* set initial node id */
    enabled = 0;
    pncfd = -1;
    for (i=0; i<FDMAPSIZE; i++)
      fdnimap[i] = -1;
    for (i=0; i<256; i++) {
      ni[i].cstate = PNCCSNONE;
      ni[i].fd = -1;
      ni[i].host[0] = 0;
      ni[i].uid[0] = 0;
      ni[i].port = 0;
      ni[i].conntime = 0;
    }
    rcv.state = PNCBSIDLE;

    /* read the ring.cfg config file.  Each line contains:
          nodeid  host:port  uid/password    comment
       where:
          nodeid = node's id (1-247) on my ring
          host = the remote emulator's TCP/IP address or name
	  port = the remote emulator's TCP/IP PNC port
	  uid = up to 16 byte password, no spaces
	  NOTE: use od -h /dev/urandom|head to create a uid
    */

    linenum = 0;
    if ((ringfile=fopen("ring.cfg", "r")) != NULL) {
      while (fgets(buf, sizeof(buf), ringfile) != NULL) {
	len = strlen(buf);
	linenum++;
	if (buf[len-1] != '\n') {
	  fprintf(stderr,"Line %d of ring.cfg: line too long, can't parse file\n", linenum);
	  return -1;
	}
	buf[len-1] = 0;
	if (strcmp(buf,"") == 0 || buf[0] == ';')
	  continue;

	if ((p=strtok(buf, DELIM)) == NULL) {
	  fprintf(stderr,"Line %d of ring.cfg: node id missing\n", linenum);
	  continue;
	}
	tempid = atoi(p);
	if (tempid < 1 || tempid > 247) {
	  fprintf(stderr,"Line %d of ring.cfg: node id is out of range 1-247\n", linenum);
	  continue;
	}
	if (ni[tempid].cstate != PNCCSNONE) {
	  fprintf(stderr,"Line %d of ring.cfg: node id occurs more than once\n", linenum);
	  continue;
	}

	if ((p=strtok(NULL, DELIM)) == NULL) {
	  fprintf(stderr,"Line %d of ring.cfg: host address missing\n", linenum);
	  continue;
	}
	if (strlen(p) > MAXHOSTLEN) {
	  fprintf(stderr,"Line %d of ring.cfg: IP address too long\n", linenum);
	  continue;
	}
	strcpy(temphost, p);

	if ((p=strtok(NULL, DELIM)) == NULL) {
	  fprintf(stderr,"Line %d of ring.cfg: unique id/password missing\n", linenum);
	  continue;
	}
	if (strlen(p) > MAXUIDLEN) {
	  fprintf(stderr,"Line %d of ring.cfg: unique id/password too long\n", linenum);
	  continue;
	}
	bzero(ni[tempid].uid, sizeof(ni[tempid].uid));
	strcpy(ni[tempid].uid, p);

	/* parse the port number from the IP address */

	tempport = -1;
	if ((p=strtok(temphost, PDELIM)) != NULL) {
	  strcpy(ni[tempid].host, p);
	  if ((p=strtok(NULL, PDELIM)) != NULL) {
	    tempport = atoi(p);
	    if (tempport < 1 || tempport > 65000)
	      fprintf(stderr,"Line %d of ring.cfg: port number out of range 1-65000\n", linenum);
	  }
	}
	if (tempport <= 0) {
	  fprintf(stderr, "Line %d of ring.cfg: can't parse port number from %s\n", linenum, temphost);
	  continue;
	}
	ni[tempid].cstate = PNCCSDISC;
	ni[tempid].port = tempport;
	TRACE(T_RIO, "Line %d: id=%d, host=\"%s\", port=%d, uid=%s, hex=%s\n", linenum, tempid, temphost, tempport, ni[tempid].uid, pnchexuid(ni[tempid].uid));
	configured = 1;
      }
      if (!feof(ringfile)) {
	perror(" error reading ring.cfg");
	fatal(NULL);
      }
      fclose(ringfile);
    } else
      perror("error opening ring.cfg");
    if (!configured) {
      fprintf(stderr, "PNC not configured.\n");
      return -1;
    }

    /* start listening on the network port */

    pncfd = socket(AF_INET, SOCK_STREAM, 0);
    if (pncfd == -1) {
      perror("socket failed for PNC");
      fatal(NULL);
    }
    pncinitfd(pncfd);
    optval = 1;
    if (setsockopt(pncfd, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof(optval))) {
      perror("setsockopt failed for PNC listen");
      fatal(NULL);
    }
    addr.sin_family = AF_INET;
    addr.sin_port = htons(nport);
    addr.sin_addr.s_addr = INADDR_ANY;
    if(bind(pncfd, (struct sockaddr *)&addr, sizeof(addr))) {
      perror("bind: unable to bind for PNC");
      fatal(NULL);
    }
    if(listen(pncfd, 10)) {
      perror("listen failed for PNC");
      fatal(NULL);
    }
    TRACE(T_RIO, "PNC configured\n");
    return 0;

  case 0:

    if (func == 00) {    /* OCP '0700 - disconnect */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - disconnect\n", func, device);
      for (i=0; i<256; i++) {
	fd = ni[i].fd;
	if (fd >= 0) {
	  fdnimap[fd] = -1;
	  close(fd);
	  ni[i].fd = -1;
	}
      }
      rcv.state = PNCBSIDLE;
      rcvstat = 0;
      xmitstat = 0;
      pncstat &= PNCNSNODEID;

    } else if (func == 01) {    /* OCP '0701 connect to the ring */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - connect\n", func, device);
      pncstat |= PNCNSCONNECTED;

    } else if (func == 02) {    /* OCP '0702 inject a token */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - inject token\n", func, device);

    } else if (func == 04) {    /* OCP '0704 ack xmit (clear xmit int) */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - ack xmit int\n", func, device);
      pncstat &= ~PNCNSXMITINT;   /* clear "xmit interrupting" */
      pncstat &= ~PNCNSTOKEN; /* clear "token detected" */
      xmitstat = 0;
      xmit.state = PNCBSIDLE;

    } else if (func == 05) {    /* OCP '0705 set PNC into "delay" mode */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - set delay mode\n", func, device);

    } else if (func == 010) {   /* OCP '0710 stop xmit in progress */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - stop xmit\n", func, device);
      if (xmit.offset == 0) {
	xmitstat = 0;
	xmit.state = PNCBSIDLE;
      }

    } else if (func == 011) {   /* OCP '0711 stop recv in progress */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - stop recv\n", func, device);

    } else if (func == 012) {   /* OCP '0712 set normal mode */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - set normal mode\n", func, device);

    } else if (func == 013) {   /* OCP '0713 set diagnostic mode */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - set diag mode\n", func, device);

    } else if (func == 014) {   /* OCP '0714 ack receive (clear rcv int) */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - ack recv int\n", func, device);
      rcvstat = 0;
      pncstat &= ~PNCNSRCVINT;

    } else if (func == 015) {   /* OCP '0715 set interrupt mask (enable int) */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - enable int\n", func, device);
      enabled = 1;

    } else if (func == 016) {   /* OCP '0716 clear interrupt mask (disable int) */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - disable int\n", func, device);
      enabled = 0;

    } else if (func == 017) {   /* OCP '0717 initialize */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - initialize\n", func, device);
      devpnc(0, 0, 7);   /* disconnect */
      pncvec = 0;
      enabled = 0;

    } else {
      printf("Unimplemented OCP device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 1:
    TRACE(T_INST|T_RIO, " SKS '%02o%02o\n", func, device);
    if (func == 99)
      IOSKIP;                     /* assume it's always ready */
    else {
      printf("Unimplemented SKS device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 2:
    if (func == 011) {          /* input ID */
      TRACE(T_INST|T_RIO, " INA '%02o%02o - input ID\n", func, device);
      crs[A] = 07;
      IOSKIP;

    } else if (func == 012) {   /* read receive status word */   
      TRACE(T_INST|T_RIO, " INA '%02o%02o - get recv status '%o\n", func, device, rcvstat);
      crs[A] = rcvstat;
      IOSKIP;

    } else if (func == 013) {   /* DIAG - read static register; not impl. */
      crs[A] = 0;
      IOSKIP;

    } else if (func == 014) {   /* read xmit status word */   
      TRACE(T_INST|T_RIO, " INA '%02o%02o - get xmit status '%o\n", func, device, xmitstat);
      crs[A] = xmitstat;
      IOSKIP;

    } else if (func == 017) {   /* read network status word */   
      TRACE(T_INST|T_RIO, " INA '%02o%02o - get ctrl status '%o\n", func, device, pncstat);
      crs[A] = pncstat;
      IOSKIP;

    } else {
      printf("Unimplemented INA device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 3:
    TRACE(T_INST|T_RIO, " OTA '%02o%02o\n", func, device);
    if (func == 011) {          /* DIAG - single step; not impl.*/
      IOSKIP;

    } else if (func == 014) {   /* initiate recv, dma chan in A */
      if (rcvstat & PNCRSBUSY) {  /* already busy? */
	warn("pnc: recv when already busy!");
	return;                 /* yes, return and don't skip */
      }
      IOSKIP;
      rcvstat = PNCRSBUSY;        /* set receive busy */
      pncinitdma(&rcv, "rcv");
      devpoll[device] = 10;

    } else if (func == 015) {   /* initiate xmit, dma chan in A */
      if (xmitstat & PNCXSBUSY) {  /* already busy? */
	warn("pnc: xmit when already busy!");
	return;                 /* yes, return and don't skip */
      }
      IOSKIP;
      xmitstat = PNCXSBUSY;

      /* read the first word, the to and from node id's */

      pncinitdma(&xmit, "xmit");
      dmaword = *xmit.memp;
      xmit.toid = dmaword >> 8;
      xmit.fromid = dmaword & 0xFF;
      TRACE(T_INST|T_RIO, " xmit: toid=%d, fromid=%d\n", xmit.toid, xmit.fromid);

      /* check for unreasonable situations */

      if (xmit.fromid != myid) {
	printf("PNC: xmit fromid=0x%02x != myid=0x%02x\n", xmit.fromid, myid);
	fatal(NULL);
      }

      /* bump the DMA registers to show the transfer */

      regs.sym.regdmx[xmit.dmareg] += xmit.dmanw;   /* bump xmit count */
      regs.sym.regdmx[xmit.dmareg+1] += xmit.dmanw; /* and address */

      /* the Primenet startup code sends a single ring packet from me
	 to me, to verify that my node id is unique.  This packet must
	 be received with the ACK bit clear, meaning no (other) node
	 ack'd the packet.  All other cases of loopback are handled at
	 higher levels within Primos.

	 So, if this xmit is to me and there is a receive pending and
	 room in the receive buffer, put the packet directly in my
	 receive buffer.  If we can't receive it now, set WACK xmit
	 status. */

      if (xmit.toid == myid) {
	if (rcv.state == PNCBSRDY && rcv.dmanw >= xmit.dmanw) {
	  memcpy(rcv.memp, xmit.memp, xmit.dmanw*2);
	  regs.sym.regdmx[rcv.dmareg] += xmit.dmanw;     /* bump recv count */
	  regs.sym.regdmx[rcv.dmareg+1] += xmit.dmanw;   /* and address */
	  pncstat |= PNCNSRCVINT;             /* set recv interrupt bit */
	  rcvstat = 0;                        /* set receive status (no ack!) */
	  xmitstat = 0;                       /* same for transmit status */
	  rcv.state = PNCBSIDLE;              /* no longer ready to recv */
	} else {
	  xmitstat = PNCXSWACK;               /* no receive, wack it */
	}
      } else {                                /* regular transmit */

	/* add leading and trailing byte counts to the packet */

	len = xmit.dmanw*2 + 4;
	*(short *)(xmit.iobuf+0) = len;
	memcpy(xmit.iobuf+2, xmit.memp, xmit.dmanw*2);
	*(short *)(xmit.iobuf+2+xmit.dmanw*2) = len;

	/* send packet, set transmit interrupt, return */

	xmitstat = pncxmit(&xmit);
      }
      pncstat |= PNCNSXMITINT;                      /* set xmit int */
      pncstat |= PNCNSTOKEN;                        /* and token seen */
      goto intrexit;

    } else if (func == 016) {   /* set interrupt vector */
      pncvec = crs[A];
      TRACE(T_INST|T_RIO, " interrupt vector = '%o\n", pncvec);
      IOSKIP;

    } else if (func == 017) {   /* set my node ID */
      myid = crs[A] & 0xFF;
      pncstat = (pncstat & 0xFF00) | myid;
      TRACE(T_INST|T_RIO, " my node id is %d\n", myid);
      IOSKIP;

    } else {
      printf("Unimplemented OTA device '%02o function '%02o, A='%o\n", device, func, crs[A]);
      fatal(NULL);
    }
    break;

  case 4:
    TRACE(T_RIO, " POLL '%02o%02o\n", func, device);

    time(&timenow);
    pncaccept(timenow);   /* accept 1 new connection each poll */
    pncconnect(timenow);  /* try to connect to a disconnected node */

    /* set default repoll and take any pending interrupt */

    devpoll[device] = PNCPOLL*gvp->instpermsec;

intrexit:
    if (enabled && (pncstat & 0xC000)) {
      if (gvp->intvec == -1)
	gvp->intvec = pncvec;
      else
	devpoll[device] = 100;
    }
    break;

  default:
    fatal("Bad func in devpcn");
  }

#else
int devpnc (int class, int func, int device) {
  return -1;
#endif
