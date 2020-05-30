/* PNC (ring net) device handler

  On a real Prime ring network, each node has a unique node id from 0
  to 254.  The node id is configured with software and stored into the
  PNC during network initialization.  The node id of a master-cleared
  PNC is 0, and Primos may tell a PNC to connect to the ring with node
  id 0 so that the PNC will regenerate packets ("transceive") as they
  pass by.  The reason is that when a PNC disconnects from the ring,
  the segment between its two neighbors becomes longer, and there is a
  limit to the distance between active nodes.  Since in practice, no
  node ever has a real node id of zero, this PNC will not ack or
  receive any packets.  Node id 0 is also used in the T&M test prmnt1.
  The broadcast node id is 255 (all PNCs accept packets with this "to"
  id).  In practice, CONFIG_NET limits the node id to 247.  When a
  node starts, it sends a message to itself.  If any system acks this
  packet, it means the node id is already in use and the new node will
  disconnect from the ring.  Since all systems in a ring have to be
  physically cabled together, there was usually a common network
  administration to ensure that no two nodes had the same node id.
  Prime rings were often logically segmented: nodes A, B, C, D in a
  physical ring might be configured as two logical networks: AB and
  CD.  But all 4 systems must still have unique node ids since they
  are in the same physical ring.

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

  Prior to rev 19.3, each Primenet node sends periodic "timer"
  messages to all nodes it knows about.  If the message returns ACK'd,
  the remote node is up.  Otherwise, the remote node must be down.
  This is not very efficient, so beginning with 19.3, a broadcast
  timer message is sent every 10 seconds to let all nodes know that a
  machine is up.  On a 100-node network, instead of sending
  100*99=9900 timer messages, only 100 are required.  Unfortunately,
  the emulated PNC does not have the efficient broadcast mechanism of
  a physical token ring, so broadcasts must be simulated by sending
  packets to each node, getting us back to the inefficient method.  It
  would be possible to store the first broadcast timer message
  received from each node, and simply loop them all back within each
  emulator every 10 seconds as long as the TCP/IP connection is up.
  Not sure it's worth the trouble though...  At later revs, the timer
  message has important version information.

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
  don't match, there has been some kind of mishap in the PNC emulation
  and the connection is dropped.  These fields are similar to the CRC
  and ack byte of a real PNC: the Prime I/O bus never sees them.

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

  PNC diagnostic register:
        NORXTX   EQU   '100000         DISABLE TX AND RX
        TXDATER  EQU   '040000         FORCE RX CRC ERROR
        TXACKER  EQU   '020000         FORCE TX ACK ERROR
        RXACKER  EQU   '010000         FORCE RX ACK BYTE ERROR
        CABLE    EQU   '000001         LOOPBACK OVER CABLE
        ANALOG   EQU   '000002         LOOPBACK THRU ANALOG DEVICES
        SINGSTEP EQU   '000004         LOOPBACK THRU SHIFT REG
        RETRY    EQU   '000000         ALLOW HARDWR RETRY
        NORETRY  EQU   '000010         NO HARDWR RETRY

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

/* PNC poll rate in ms, mostly for connecting to new nodes */

#define PNCPOLL 1000

/* PNC network status bits

   NOTE: many fields are commented out since they can't occur in the
   emulator implementation; they're specified as documentation */

#define PNCNSRCVINT    0x8000  /* bit 1 rcv interrupt (rcv complete) */
#define PNCNSXMITINT   0x4000  /* bit 2 xmit interrupt (xmit complete) */
#define PNCNSPNC2      0x2000  /* bit 3 PNC II = 1 (aka pncbooster) */
  //#define PNCNSNEWMODE   0x1000  /* bit 4 PNC in "new"/II mode */
  //#define PNCNSERROR     0x0800  /* bit 5 u-verify failure or bad command (II) */
#define PNCNSCONNECTED 0x0400  /* bit 6 connected to ring */
  //#define PNCNSMULTOKEN  0x0200  /* bit 7 multiple tokens (only after xmit EOR) */
#define PNCNSTOKEN         0x0100  /* bit 8, token (only after xmit EOR) */
#define PNCNSNODEIDMASK    0x00FF  /* bits 9-16 node-id mask */

/* receive status word: first 8 bits are the "ACK byte"; PNCDIM
   expects all bits except busy to be zero for a good receive */

  //#define PNCRSACK       0x8000  /* ACK'd */
  //#define PNCRSMACK      0x4000  /* multiple ACK */
  //#define PNCRSWACK      0x2000  /* WACK'd */
  //#define PNCRSNAK       0x1000  /* NAK'd */
  //#define PNCRSABPE      0x0200  /* ack byte parity error */
  //#define PNCRSABCE      0x0100  /* ack byte check error */
  //#define PNCRSRBPE      0x0080  /* receive buffer parity error */
#define PNCRSBUSY      0x0040  /* receive busy */
  //#define PNCRSEOR       0x0020  /* EOR before end of message */

/* xmit status word: first 8 bits are the "ACK byte"

     ACK = the node the packet was addressed to saw the packet correctly
     WACK = same as ACK, but the node couldn't accept the packet (no recv active)
     NAK = the packet was corrupt when some (any) node saw it

   I initially thought a packet addressed to an inactive node would be
   returned with no bits set in the ACK byte, but T&M prmnt1 indicates
   that the NAK bit is set.  When I tried that, it caused Primenet to
   keep retrying the packet.  So I changed it back to no NAK.
*/

#define PNCXSACK       0x8000  /* ACK'd */
  //#define PNCXSMACK      0x4000  /* multiple ACK */
#define PNCXSWACK      0x2000  /* WACK'd */
  //#define PNCXSNAK       0x1000  /* NAK'd (no node ack'd packet) */
  //#define PNCXSABPE      0x0200  /* ack byte parity error */
  //#define PNCXSABCE      0x0100  /* ack byte check error */
  //#define PNCXSXBPE      0x0080  /* transmit buffer parity error */
#define PNCXSBUSY      0x0040  /* xmit busy */
  //#define PNCXSNORET     0x0020  /* packet didn't return */
  //#define PNCXSRETBAD    0x0010  /* returned w/Ack byte or CRC error */
  //#define PNCXSRETRYFAIL 0x0008  /* retry not successful */
  //#define PNCXSRETRIES   0x0007  /* retry count (3 bits) */

#define MINACCEPTTIME 0      /* wait N seconds before accepting new connections */
#define MAXACCEPTTIME 15     /* max wait 15 seconds to receive uid */
#define MAXPNCWORDS 1024     /* max packet size in 16-bit words */
#define MAXPNCBYTES MAXPNCWORDS*2     /* same in bytes */
#define MAXPKTBYTES (MAXPNCBYTES+4)   /* adds 16-bit length word to each end */

#define MAXNODEID 254      /* 0 is a dummy, 255 is broadcast */

#define MAXHOSTLEN 64        /* max length of remote host name */
#define MAXUIDLEN 16         /* max length of unique id/password */

static short configured = 0;      /* true if PNC configured */
static unsigned short pncdiag=0;  /* controller diagnostic register */
static unsigned short intstat;    /* interrupt status word */
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
  short fd;                 /* socket fd, -1 if unconnected */
  char  host[MAXHOSTLEN+1]; /* TCP/IP host name/address */
  short port;               /* emulator network port */
  char  uid[MAXUIDLEN+1];   /* unique ID/password (+ null byte) */
  unsigned char rcvpkt[MAXPKTBYTES];/* receive packet w/leading + trailing length */
  short rcvlen;             /* length received so far */
  time_t conntime;          /* time of last connect */
} ni[MAXNODEID+1];          /* ring id 0-254 (255 is broadcast) */

/* PNC connection states.  This is a bit complex because the socket
   operations are all non-blocking and the unique ID has to be sent
   after connecting */

#define PNCCSNONE 0           /* not configured in ring.cfg */
#define PNCCSDISC 1           /* not connected */
#define PNCCSCONN 2           /* connecting */
#define PNCCSAUTH 3           /* unique ID / password sent */

/* xmit/recv buffer states and buffers.  These must be static because
   they also contain the dma register settings set with an OTA, and
   can be read later with an INA.  The state field is really only
   important for the recv buffer, to indicate there is a receive
   pending on the Prime.  For transmits, the packet is copied into the
   socket buffer in 1 devpnc call, so there are no transmit states. */

#define PNCBSIDLE 0          /* initial state: no recv pending */
#define PNCBSRDY  1          /* recv is pending */

typedef struct {
  short toid, fromid;
  short state;
  short pktsize;           /* size of packet in bytes */
  unsigned short dmachan, dmareg;
  unsigned int dmaaddr;
  short dmanw;
  unsigned short *memp;      /* ptr to Prime memory */
  unsigned char iobuf[MAXPKTBYTES];
} t_dma;
static t_dma rcv, xmit;

static int sawsigio=0, olddevpoll;

static double tv0ts;

void pnchavedata(int s) {
  if (sawsigio) return;
  olddevpoll = devpoll[7];
  devpoll[7] = 1;
  sawsigio = 1;
}

#define HEXNIBBLE(ch) (0 <= (ch) && (ch) <= 9)? (ch) + '0': (ch) - 10 + 'a'

char * pncdumppkt(unsigned char * pkt, int len) {
  static unsigned char hexpkt[MAXPKTBYTES*3 + 1];
  unsigned int i, ch;
  double ts;
  struct timeval tv1;

#ifndef NOTRACE
  if (!gv.traceflags & T_RIO) return;

  if (gettimeofday(&tv1, NULL) != 0)
    fatal("pnc gettimeofday 3 failed");
  ts = (tv1.tv_sec + tv1.tv_usec/1000000.0) - tv0ts;
  TRACE(T_RIO, " @ %.2f: len=%d\n", ts, len);

  if (len > MAXPKTBYTES)
    len = MAXPKTBYTES;
  for (i=0; i<len; i++) {
    ch = pkt[i] >> 4;
    hexpkt[i*3] = HEXNIBBLE(ch);
    ch = pkt[i] & 0xF;
    hexpkt[i*3+1] = HEXNIBBLE(ch);
    hexpkt[i*3+2] = ' ';
  }
  hexpkt[len*3] = 0;
  TRACE(T_RIO, "%s\n", hexpkt);
  for (i=0; i<len; i++) {
    ch = pkt[i] & 0x7f;
    if (' ' <= ch && ch <= '~')
      hexpkt[i] = ch;
    else
      hexpkt[i] = '_';
  }
  hexpkt[len] = 0;
  TRACE(T_RIO, "%s\n", hexpkt);
#endif
}

/* disconnect from a node */

unsigned short pncdisc(int nodeid, char *why) {
  if (nodeid != myid && ni[nodeid].cstate > PNCCSDISC) {
    if (ni[nodeid].cstate > PNCCSCONN)
      fprintf(stderr, "devpnc: disconnect from authenticated node %d: %s\n", nodeid, why);
    TRACE(T_RIO, " pncdisc: disconnect from node %d\n", nodeid);
    close(ni[nodeid].fd);
    ni[nodeid].cstate = PNCCSDISC;
    ni[nodeid].rcvlen = 0;
    ni[nodeid].fd = -1;
  }
}

/* initialize a socket fd for PNC emulation */

void pncinitfd(int fd) {
  int optval, fdflags;
  
  optval = 1;
  if (setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, &optval, sizeof(optval))) {
    perror("setsockopt TCP_NODELAY failed for PNC");
    fatal(NULL);
  }
#ifdef __APPLE__
  optval = MAXPKTBYTES;
  if (setsockopt(fd, SOL_SOCKET, SO_SNDLOWAT, &optval, sizeof(optval))) {
    perror("setsockopt SO_SNDLOWAT failed for PNC");
    fatal(NULL);
  }
  if (setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &optval, sizeof(optval))) {
    perror("setsockopt SO_NOSIGPIPE failed for PNC");
    fatal(NULL);
  }
#endif
  if (fcntl(fd, F_SETOWN, getpid()) == -1) {
    perror("unable to SETOWN for PNC");
    fatal(NULL);
  }
  if ((fdflags = fcntl(fd, F_GETFL, 0)) == -1) {
    perror("unable to get ts flags for PNC");
    fatal(NULL);
  }
#ifdef __sun__
  fdflags |= O_NONBLOCK;
#else
  fdflags |= O_NONBLOCK+O_ASYNC;
#endif
  if (fcntl(fd, F_SETFL, fdflags) == -1) {
    perror("unable to set fdflags for PNC");
    fatal(NULL);
  }
}

/* process a new incoming connection.  This may require multiple calls
   to pncaccept, hence the static variables for the accept state. */

void pncaccept(time_t timenow) {
  static time_t accepttime = 0;
  static int fd = -1;
  static struct sockaddr_in addr;
  static unsigned int addrlen;
  char uid[MAXUIDLEN+1];
  int i,n;

#if 0
  if (!(pncstat & PNCNSCONNECTED))
    return;
#endif
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
    if (!(pncstat & PNCNSCONNECTED)) {
      fprintf(stderr, " not connected to ring, closed new PNC connection, fd %d\n", fd);
      goto disc;
    }
    TRACE(T_RIO, " new PNC connection, fd %d\n", fd);
    pncinitfd(fd);
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
  if (n == 0) {
    TRACE(T_RIO, " read eof on new PNC fd %d\n", fd);
    goto disc;
  }
  if (n == -1) {
    if (errno == EWOULDBLOCK || errno == EINTR || errno == EAGAIN)
      return;
    fprintf(stderr, "error reading PNC uid from fd %d: %s\n", fd, strerror(errno));
    goto disc;
  }
  if (n != MAXUIDLEN) {
    fprintf(stderr, "devpnc: error reading uid from fd %d, expected %d bytes, got %d\n", fd, MAXUIDLEN, n);
    goto disc;
  }
  TRACE(T_RIO, " uid is %s for new PNC fd %d\n", uid, fd);

  /* look up the uid in our ring.cfg array to determine the node id
     we'll use for this remote emulator */

  for (i=1; i<=MAXNODEID; i++)
    if (memcmp(uid, ni[i].uid, MAXUIDLEN) == 0)
      break;
  if (i > MAXNODEID) {
    fprintf(stderr, "devpnc: uid \"%s\" not in ring.cfg\n", uid);
    goto disc;
  }
  if (ni[i].cstate == PNCCSNONE) {
    TRACE(T_RIO, " devpnc: node %d is disabled\n", i);
    goto disc;
  }

  /* you're now authenticated.  If I'm connecting to you, drop my
     connection.  If I'm authenticated to you (crossed connections),
     use memcmp to keep one connection and drop the other.

     NOTE: memcmp on uid is used instead of comparing nodeids because
     a system's node id can be different in two different rings.  My
     nodeid might be < yours in my ring and yours < mine in your ring,
     so we'd both disconnect.  uid is always unique.
 */

  if (ni[i].cstate > PNCCSDISC) {
    TRACE(T_RIO, " devpnc: already connected to node %d%s\n", i);
    if (ni[i].cstate == PNCCSCONN)
      pncdisc(i, "was connecting");  /* and fall through to use new connection */
    else if (memcmp(uid, ni[myid].uid, MAXUIDLEN) > 0)
      pncdisc(i, "cross connect");   /* and fall through to use new connection */
    else
      fprintf(stderr, " devpnc: already connected to node %d, closing new connection\n", i);
      goto disc;
  }
  TRACE(T_RIO, " devpnc: node %d authorized, fd %d\n", i, fd);
  ni[i].cstate = PNCCSAUTH;
  ni[i].rcvlen = 0;
  ni[i].fd = fd;
  fd = -1;
  return;

disc:
  TRACE(T_RIO, " devpnc: close new connection from node %d fd %d\n", i, fd);
  close(fd);
  fd = -1;
}

/* connect to a node, without blocking
   State on entry must be PNCCSDISC
   State on exit will be PNCCSCONN */

void pncconn1(nodeid, timenow) {
  struct hostent* server;
  struct sockaddr_in addr;
  int fd;

  TRACE(T_RIO, "try connect to node %d\n", nodeid);
  if (ni[nodeid].cstate != PNCCSDISC) {
    printf("devpnc: pncconn1 entry state for node %d is %d\n", nodeid, ni[nodeid].cstate);
    fatal(NULL);
  }
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

void pncauth1(int nodeid, time_t timenow) {
  int n;

  n = write(ni[nodeid].fd, ni[myid].uid, MAXUIDLEN);
  TRACE(T_RIO, "sent my uid %s, to node %d, fd %d, %d bytes\n", ni[myid].uid, nodeid, ni[nodeid].fd, n);
  if (n == MAXUIDLEN) {
    ni[nodeid].cstate = PNCCSAUTH;
    return;
  }
  if (n == -1) {
    if (errno == EWOULDBLOCK || errno == EINTR || errno == EAGAIN || errno == EINPROGRESS)
      return;
    if (errno != EPIPE && errno != ENOTCONN)
      fprintf(stderr, "devpnc: error sending PNC uid to node %d: %s\n", nodeid, strerror(errno));
  } else
    fprintf(stderr, "devpnc: wrote %d of %d uid bytes to node %d\n", n, MAXUIDLEN, nodeid);
  pncdisc(nodeid, "uid write error");
}

/* finish one connection in progress */

void pncconnect(time_t timenow) {
  static int prevnode=MAXNODEID;
  int nodeid;

  if (myid == 0)
    return;
  nodeid = prevnode;
  do {
    nodeid = (nodeid % MAXNODEID) + 1;
    if (nodeid == myid)          /* don't connect to myself */
      continue;
    if (ni[nodeid].port == 0)    /* incoming-only connection */
      continue;               
    if (ni[nodeid].cstate == PNCCSCONN) {
      pncauth1(nodeid, timenow);
      break;
    }
  } while (nodeid != prevnode);      /* went round once? */
  prevnode = nodeid;
}

/* initialize a dma transfer */

void pncinitdma(t_dma *iob, char *iotype) {
  if (getcrs16(A) > 037)
    fatal("PNC doesn't support chaining, DMC, or DMA reg > 037");
  (*iob).dmachan = getcrs16(A);
  (*iob).dmareg = (*iob).dmachan << 1;
  (*iob).dmanw = -((getar16(REGDMX16 + ((*iob).dmareg))>>4) | 0xF000);
  if ((*iob).dmanw > MAXPNCWORDS)
    (*iob).dmanw = MAXPNCWORDS;      /* clamp it */
  (*iob).dmaaddr = ((getar16(REGDMX16 + ((*iob).dmareg)) & 3)<<16) | getar16(REGDMX16 + ((*iob).dmareg+1));
  TRACE(T_RIO, " pncinitdma: %s dmachan=%o, dmareg=%o, [dmaregs]=%o|%o, dmaaddr=%o/%o, dmanw=%d\n", iotype, (*iob).dmachan, (*iob).dmareg, swap16(regs.sym.regdmx[(*iob).dmareg]), swap16(regs.sym.regdmx[(*iob).dmareg+1]), (*iob).dmaaddr>>16, (*iob).dmaaddr&0xFFFF, (*iob).dmanw);
  (*iob).memp = MEM + mapio((*iob).dmaaddr);
  (*iob).state = PNCBSRDY;
}

/* transmit to a node.  If the node is disabled, fail.  If not yet
   connected, initiate a connect.  If connecting, try to authenticate.
   If authenticated, transmit */

unsigned short pncxmit1(short nodeid) {
  int nwritten, ntowrite;
  time_t timenow;

  TRACE(T_RIO, " xmit packet to node %d\n", nodeid);
  if (ni[nodeid].cstate == PNCCSNONE) {
    fprintf(stderr, " can't transmit: node %d not configured in ring.cfg\n", nodeid);
    return 0;
  }

  /* the Primenet startup code sends a single ring packet from me
     to me, to verify that my node id is unique.  This packet must
     be received with the ACK bit clear, meaning no (other) node
     ack'd the packet.  All other cases of loopback are handled at
     higher levels within Primos.

     So, if this xmit is to me and there is a receive pending and
     room in the receive buffer, put the packet directly in my
     receive buffer.  If we can't receive it now, set WACK xmit
     status. */

  if (nodeid == myid) {
    if (rcv.state == PNCBSRDY && rcv.dmanw >= xmit.dmanw) {
      TRACE(T_INST|T_RIO, " xmit: loopback, rcv.memp=%o/%o\n", ((int)(rcv.memp))>>16, ((int)(rcv.memp))&0xFFFF);
      memcpy(rcv.memp, xmit.memp, xmit.dmanw*2);
      putar16(REGDMX16 + rcv.dmareg, getar16(REGDMX16 + rcv.dmareg) + (xmit.dmanw<<4));  /* bump recv count */
      putar16(REGDMX16 + rcv.dmareg+1, getar16(REGDMX16 + rcv.dmareg+1) + xmit.dmanw); /* and address */
      pncstat |= PNCNSRCVINT;             /* set recv interrupt */
      rcv.state = PNCBSIDLE;              /* no longer ready to recv */
      return PNCXSACK;
    }
    return PNCXSWACK;              /* no receive, wack it */
  }

  /* not transmitting to myself, make sure connection is ready */

  if (ni[nodeid].cstate == PNCCSDISC) {
    time(&timenow);
    pncconn1(nodeid, timenow);
    return 0;
  }
  if (ni[nodeid].cstate == PNCCSCONN) {
    time(&timenow);
    pncauth1(nodeid, timenow);
    if (ni[nodeid].cstate != PNCCSAUTH)
      return 0;
  }

  ntowrite = xmit.dmanw*2 + 4;
  pncdumppkt(xmit.iobuf, ntowrite);
  if ((nwritten=write(ni[nodeid].fd, xmit.iobuf, ntowrite)) < 0) {
    if (errno == EAGAIN || errno == EWOULDBLOCK) {
      TRACE(T_RIO, " wack packet to node %d\n", nodeid);
      return PNCXSWACK;
    }
    fprintf(stderr, " error writing packet to node %d: %s\n", nodeid, strerror(errno));
    pncdisc(nodeid, "packet write error");
    return 0;
  }
  if (nwritten != ntowrite) {
    fprintf(stderr, "devpnc: pncxmit1 wrote %d of %d bytes to node %d; disconnecting\n", nwritten, ntowrite, nodeid);
    pncdisc(nodeid, "partial packet written");
    return 0;
  }
  return PNCXSACK;
}

/* transmit a packet to its destination, either a single nodeid or
   255, the broadcast nodeid */

unsigned short pncxmit() {
  short  nodeid;
  short len;
  unsigned short dmaword;

  xmitstat = PNCXSBUSY;

  /* read the first word, the to and from node id's */

  pncinitdma(&xmit, "xmit");
  dmaword = swap16(*xmit.memp);
  xmit.toid = dmaword >> 8;
  xmit.fromid = dmaword & 0xFF;
  TRACE(T_INST|T_RIO, " xmit: toid=%d, fromid=%d, myid=%d\n", xmit.toid, xmit.fromid, myid);

  /* bump the DMA registers to show the transfer */

  putar16(REGDMX16+xmit.dmareg, getar16(REGDMX16+xmit.dmareg) + (xmit.dmanw<<4));   /* bump xmit count */
  putar16(REGDMX16+xmit.dmareg+1, getar16(REGDMX16+xmit.dmareg+1) + xmit.dmanw);  /* and address */

  /* add leading and trailing byte counts to the packet */

  len = xmit.dmanw*2 + 4;
  *(short *)(xmit.iobuf+0) = swap16(len);
  memcpy(xmit.iobuf+2, xmit.memp, xmit.dmanw*2);
  *(short *)(xmit.iobuf+2+xmit.dmanw*2) = swap16(len);

  /* send packet, set transmit interrupt, return */

  if (xmit.toid == 255) {
    for (nodeid=1; nodeid<=MAXNODEID; nodeid++)
      if (ni[nodeid].cstate != PNCCSNONE)
        xmitstat |= pncxmit1(nodeid);
  } else {
    xmitstat |= pncxmit1(xmit.toid);
  }
  pncstat |= PNCNSXMITINT;       /* set xmit interrupt */
  pncstat |= PNCNSTOKEN;         /* and token seen */
}

/* do socket reads on all active connections with data available until
   1 connection has a complete packet, then return that packet to the
   Prime */

void pncrecv() {
  static int prevnode=MAXNODEID;
  static struct timeval timeout = {0, 0};
  int nodeid, n, fd;
  fd_set fds;

  /* if connected with id 0, don't do reads */

  if (myid == 0)
    return;

  /* do a select on all active nodes to see if there's any data available */

  n = -1;
  FD_ZERO(&fds);
  for (nodeid=0; nodeid<=MAXNODEID; nodeid++)
    if (ni[nodeid].cstate == PNCCSAUTH) {
      fd = ni[nodeid].fd;
      if (fd != -1)
        FD_SET(fd, &fds);
      if (fd > n)
        n = fd;
    }
  n = select(n+1, &fds, NULL, NULL, &timeout);
  if (n == -1) {
    if (errno == EINTR || errno == EAGAIN)
      return;
    perror("unable to do read select on PNC fds");
    fatal(NULL);
  }
  if (n == 0) {
    TRACE(T_RIO, " pncrecv: no data\n");
    return;
  }

  nodeid = prevnode;
  do {
    nodeid = (nodeid % MAXNODEID) + 1;
    fd = ni[nodeid].fd;
    if (fd >= 0 && FD_ISSET(fd, &fds))
      if (pncrecv1(nodeid))
        break;
  } while (nodeid != prevnode);      /* went round once? */
  prevnode = nodeid;
}

/* try to read a complete packet from a node.  On entry, the receive
   can be in two states:

   1. haven't read the entire 2-byte packet length yet
   2. got the length, but have more to read
*/

int pncrecv1(int nodeid) {
  int pktlen, nw;

  TRACE(T_RIO, " pncrecv1: read node %d\n", nodeid);
  if (ni[nodeid].rcvlen < 2) {
    if (!pncread(nodeid, 2 - ni[nodeid].rcvlen))
      return 0;
  }
  pktlen = swap16(*(short *)(ni[nodeid].rcvpkt));
  nw = (pktlen-4)/2;
  TRACE(T_RIO, " pncrecv1: have %d of %d bytes\n", ni[nodeid].rcvlen, pktlen);
  if (nw > rcv.dmanw) {
    fprintf(stderr, "devpnc: packet size %d > max %d words from node %d\n", nw, rcv.dmanw, nodeid);
    fprintf(stderr, "devpnc: disabling node %d until reboot\n", nodeid);
    pncdisc(nodeid, "received packet too big");
    ni[nodeid].cstate = PNCCSNONE;
    return 0;
  }
  if (!pncread(nodeid, pktlen - ni[nodeid].rcvlen))
    return 0;
  if (pktlen != swap16(*(short *)(ni[nodeid].rcvpkt+pktlen-2))) {
    fprintf(stderr, "devpnc: node %d, pktlen mismatch: %d != %d\n", nodeid, pktlen, swap16(*(short *)(ni[nodeid].rcvpkt+pktlen-2)));
    pncdisc(nodeid, "packet length mismatch");
    return 0;
  }

  /* send completed packet to the Prime */

  TRACE(T_RIO, " pncrecv1: store pkt\n");
  pncdumppkt(ni[nodeid].rcvpkt, ni[nodeid].rcvlen);

  /* modify to/from word to allow me to be in multiple rings */
  
  ni[nodeid].rcvpkt[2] = myid;
  ni[nodeid].rcvpkt[3] = nodeid;    
  memcpy(rcv.memp, ni[nodeid].rcvpkt+2, nw*2);
  putar16(REGDMX16 + rcv.dmareg, getar16(REGDMX16 + rcv.dmareg) + (nw<<4));  /* bump recv count */
  putar16(REGDMX16 + rcv.dmareg+1, getar16(REGDMX16 + rcv.dmareg+1) + nw); /* and address */
  pncstat |= PNCNSRCVINT;                /* set recv interrupt bit */
  rcv.state = PNCBSIDLE;                 /* no longer ready to recv */
  ni[nodeid].rcvlen = 0;                 /* reset for next read */
  return 1;
}

/* read nbytes from a connection, return true if that many were read */

int pncread (int nodeid, int nbytes) {
  int n;

  n = read(ni[nodeid].fd, ni[nodeid].rcvpkt+ni[nodeid].rcvlen, nbytes);
  if (n == 0) {
    TRACE(T_RIO, " read eof on node %d, disconnecting\n", nodeid);
    pncdisc(nodeid, "eof reading packet");
  }
  if (n == -1) {
    if (errno != EWOULDBLOCK && errno != EINTR && errno != EAGAIN) {
      if (errno != EPIPE)
        fprintf(stderr, "error reading %d bytes from node %d: %s\n", nbytes, nodeid, strerror(errno));
      pncdisc(nodeid, "error reading packet");
    }
    n = 0;
  }
  ni[nodeid].rcvlen += n;
  return (n == nbytes);
}
  
int devpnc (int class, int func, int device) {

  short i;
  short len;
  time_t timenow;
  struct sockaddr_in addr;
  int optval;

  struct timeval tv0,tv1;
  double pollts;

  //gv.traceflags = ~T_MAP;

  if (class >= 0 && !configured) {
    TRACE(T_INST|T_RIO, "PIO to PNC ignored, class=%d, func='%02o, device=%02o\n", class, func, device);
    return -1;
  }

  switch (class) {

  case -2:
    break;

  case -1: {
    FILE *ringfile;
    char buf[128], *p;
    int linenum;
    int tempid, tempport;
    char temphost[MAXHOSTLEN+1];

#define DELIM " \t\n"
#define PDELIM ":"

    if (nport <= 0) {
      fprintf(stderr, "-nport is zero, PNC not started\n");
      return -1;
    }

    pncstat = 0;
    pncstat = PNCNSPNC2;      /* this enables PNC II */
    intstat = 0;
    rcvstat = 0;
    xmitstat = 0;
    pncvec = 0;
    myid = 0;                 /* set initial node id */
    enabled = 0;
    pncfd = -1;
    for (i=0; i<=MAXNODEID; i++) {
      ni[i].cstate = PNCCSNONE;
      ni[i].fd = -1;
      ni[i].rcvlen = 0;
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
          NOTE: host:port may be - for incoming-only nodes
          uid = 16 byte password, no spaces
          NOTE: use od -h /dev/urandom|head to create a uid
    */

    linenum = 0;
    if ((ringfile=fopen("ring.cfg", "r")) != NULL) {
      while (fgets(buf, sizeof(buf), ringfile) != NULL) {
        len = strlen(buf);
        linenum++;
        if (buf[len-1] != '\n') {
          fprintf(stderr,"Line %d of ring.cfg ignored: line too long, can't parse file\n", linenum);
          return -1;
        }
        buf[len-1] = 0;
        if (strcmp(buf,"") == 0 || buf[0] == ';' || buf[0] == '#')
          continue;

        if ((p=strtok(buf, DELIM)) == NULL) {
          fprintf(stderr,"Line %d of ring.cfg ignored: node id missing\n", linenum);
          continue;
        }
        tempid = atoi(p);
        if (tempid < 1 || tempid > MAXNODEID) {
          fprintf(stderr,"Line %d of ring.cfg ignored: node id is out of range 1-%d\n", linenum, MAXNODEID);
          continue;
        }
        if (ni[tempid].cstate != PNCCSNONE) {
          fprintf(stderr,"Line %d of ring.cfg ignored: node id occurs more than once\n", linenum);
          continue;
        }

        if ((p=strtok(NULL, DELIM)) == NULL) {
          fprintf(stderr,"Line %d of ring.cfg ignored: host address missing\n", linenum);
          continue;
        }
        if (strlen(p) > MAXHOSTLEN) {
          fprintf(stderr,"Line %d of ring.cfg ignored: IP address too long\n", linenum);
          continue;
        }
        strncpy(temphost, p, MAXHOSTLEN);

        if ((p=strtok(NULL, DELIM)) == NULL) {
          fprintf(stderr,"Line %d of ring.cfg ignored: unique id/password missing\n", linenum);
          continue;
        }
        if (strlen(p) > MAXUIDLEN) {
          fprintf(stderr,"Line %d of ring.cfg ignored: unique id/password too long\n", linenum);
          continue;
        }
        bzero(ni[tempid].uid, sizeof(ni[tempid].uid));
        for (i=0; i<=MAXNODEID; i++)
          if (strcmp(p, ni[i].uid) == 0) {
            fprintf(stderr,"Line %d of ring.cfg ignored: unique id/password is not unique\n", linenum);
            break;
          }
        if (i <= MAXNODEID)
          continue;
        strncpy(ni[tempid].uid, p, MAXUIDLEN);

        /* parse the port number from the IP address */

        tempport = 0;
        if (strcmp(temphost, "-") != 0) {
          if ((p=strtok(temphost, PDELIM)) != NULL) {
            strncpy(ni[tempid].host, p, MAXHOSTLEN);
            if ((p=strtok(NULL, PDELIM)) != NULL) {
              tempport = atoi(p);
              if (tempport < 1 || tempport > 65000)
                fprintf(stderr,"Line %d of ring.cfg ignored: port number out of range 1-65000\n", linenum);
            }
          }
          if (tempport <= 0) {
            fprintf(stderr, "Line %d of ring.cfg ignored: can't parse port number from %s\n", linenum, temphost);
            continue;
          }
        }
        ni[tempid].cstate = PNCCSDISC;
        ni[tempid].port = tempport;
        TRACE(T_RIO, "Line %d: id=%d, host=\"%s\", port=%d, uid=%s\n", linenum, tempid, temphost, tempport, ni[tempid].uid);
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

    /* set async I/O signal handler */

    if (signal(SIGIO, pnchavedata) == SIG_ERR) {
      perror("installing SIGIO handler");
      fatal(NULL);
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
      perror("setsockopt SO_REUSEADDR failed for PNC listen");
      fatal(NULL);
    }
#ifdef __APPLE__
    if (setsockopt(pncfd, SOL_SOCKET, SO_NOSIGPIPE, &optval, sizeof(optval))) {
      perror("setsockopt SO_NOSIGPIPE failed for PNC");
      fatal(NULL);
    }
#endif
    addr.sin_family = AF_INET;
    addr.sin_port = htons(nport);
    addr.sin_addr.s_addr = bindaddr;
    if (bind(pncfd, (struct sockaddr *)&addr, sizeof(addr))) {
      perror("bind: unable to bind for PNC");
      fatal(NULL);
    }
    if (listen(pncfd, MAXNODEID)) {
      perror("listen failed for PNC");
      fatal(NULL);
    }

    /* a uid is always sent after a connect, so don't bother with the
       accept until the uid is received.  This is Linux-specific and
       can be disabled, though that may cause problems for rings where
       the max node id > MAXACCEPTTIME because of connect delays */

#ifdef __linux__
    optval = MAXNODEID;
    if (setsockopt(pncfd, IPPROTO_TCP, TCP_DEFER_ACCEPT, &optval, sizeof(optval))) {
      perror("setsockopt TCP_DEFER_ACCEPT failed for PNC");
      fatal(NULL);
    }
#endif

    TRACE(T_RIO, "PNC configured\n");
    devpoll[device] = PNCPOLL*gv.instpermsec;

    if (gettimeofday(&tv0, NULL) != 0)
      fatal("pnc gettimeofday 1 failed");
    tv0ts = tv0.tv_sec + tv0.tv_usec/1000000.0;
    
    return 0;
  }

  case 0:

    if (func == 00) {    /* OCP '0007 - disconnect */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - disconnect\n", func, device);
      for (i=0; i<=MAXNODEID; i++)
        pncdisc(i, "ring disconnect");
      rcv.state = PNCBSIDLE;
      rcvstat = PNCRSBUSY;
      xmitstat = PNCXSBUSY;
      pncstat &= (PNCNSPNC2 | PNCNSNODEIDMASK);

    } else if (func == 01) {    /* OCP '0107 connect to the ring */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - connect\n", func, device);
      pncstat |= PNCNSCONNECTED;

    } else if (func == 02) {    /* OCP '0207 inject a token */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - inject token\n", func, device);

    } else if (func == 03) {    /* OCP '0307 simulate a token */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - simulate token\n", func, device);

    } else if (func == 04) {    /* OCP '0407 ack xmit (clear xmit int) */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - ack xmit int\n", func, device);
      pncstat &= ~PNCNSXMITINT;   /* clear "xmit interrupt" */
      intstat &= ~PNCNSXMITINT;   /* clear "xmit interrupting" */
      pncstat &= ~PNCNSTOKEN;     /* clear "token detected" */
      xmitstat = 0;               /* clear xmit busy */

    } else if (func == 05) {    /* OCP '0507 set PNC into "delay" mode */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - set delay mode\n", func, device);

    } else if (func == 010) {   /* OCP '1007 stop xmit in progress */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - stop xmit\n", func, device);
      xmitstat = 0;               /* clear xmit busy */
      pncstat &= ~PNCNSTOKEN;     /* clear "token detected" */

    } else if (func == 011) {   /* OCP '1107 stop recv in progress */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - stop recv\n", func, device);
      rcvstat = 0;                /* clear recv busy */
      rcv.state = PNCBSIDLE;      /* need a new DMA setup */

    } else if (func == 012) {   /* OCP '1207 set normal mode */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - set normal mode\n", func, device);

    } else if (func == 013) {   /* OCP '1307 set diagnostic mode */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - set diag mode\n", func, device);

    } else if (func == 014) {   /* OCP '1407 ack receive (clear rcv int) */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - ack recv int\n", func, device);
      rcvstat = 0;                /* clear recv busy */
      pncstat &= ~PNCNSRCVINT;    /* clear recv interrupt */
      intstat &= ~PNCNSRCVINT;    /* clear recv interrupting */

    } else if (func == 015) {   /* OCP '1507 set interrupt mask (enable int) */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - enable int\n", func, device);
      enabled = 1;
      goto intrexit;

    } else if (func == 016) {   /* OCP '1607 clear interrupt mask (disable int) */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - disable int\n", func, device);
      enabled = 0;

    } else if (func == 017) {   /* OCP '1707 initialize */
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
      putcrs16(A, 07);
      IOSKIP;

    } else if (func == 012) {   /* read receive status word */   
      TRACE(T_INST|T_RIO, " INA '%02o%02o - get recv status '%o 0x%04x\n", func, device, rcvstat, rcvstat);
      putcrs16(A, rcvstat);
      IOSKIP;

    } else if (func == 013) {   /* read xmit status word */
      TRACE(T_INST|T_RIO, " INA '%02o%02o - get xmit status '%o 0x%04x\n", func, device, xmitstat, xmitstat);
      putcrs16(A, xmitstat);
      IOSKIP;

      /* these next two are mostly bogus: prmnt1 T&M wants them this
         way because it puts the PNC in diagnostic mode, disables
         transmit and receive, then does transmit and receive
         operations to see how the DMA registers are affected.  I
         couldn't get very far with this T&M (couldn't complete test
         1) because it is so specific to the exact states the PNC
         hardware moves through. */

    } else if (func == 014) {   /* read recv DMX channel */
      TRACE(T_INST|T_RIO, " INA '%02o%02o - get recv DMX chan '%o 0x%04x\n", func, device, rcv.dmachan, rcv.dmachan);
      putcrs16(A, (~rcv.dmachan & 0xF000) | rcv.dmachan & 0x0FFE);
      IOSKIP;

    } else if (func == 015) {   /* read xmit DMX channel */
      TRACE(T_INST|T_RIO, " INA '%02o%02o - get xmit DMX chan '%o 0x%04x\n", func, device, xmit.dmachan, xmit.dmachan);
      putcrs16(A, (~xmit.dmachan & 0xF000) | xmit.dmachan & 0x0FFE);
      TRACE(T_INST|T_RIO, " returning '%o 0x%04x\n", getcrs16(A), getcrs16(A));
      IOSKIP;

    } else if (func == 016) {   /* read diagnostic register */
      TRACE(T_INST|T_RIO, " INA '%02o%02o - read diag reg '%o 0x%04x\n", func, device, pncdiag, pncdiag);
      putcrs16(A, pncdiag);
      IOSKIP;

    } else if (func == 017) {   /* read network status word */   
      TRACE(T_INST|T_RIO, " INA '%02o%02o - get ctrl status '%o 0x%04x\n", func, device, pncstat, pncstat);
      putcrs16(A, pncstat);
      IOSKIP;

    } else {
      printf("Unimplemented INA device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 3:
    TRACE(T_INST|T_RIO, " OTA '%02o%02o\n", func, device);
    if (func == 011) {          /* output diagnostic register */
      pncdiag = getcrs16(A);
      IOSKIP;

    } else if (func == 0) {     /* PNC2: set statistics update frequency */
      IOSKIP;

    } else if (func == 014) {   /* initiate recv, dma chan in A */
      if (!(pncstat & PNCNSCONNECTED)) {
        break;                  /* yes, return and don't skip */
      }
      if (rcvstat & PNCRSBUSY) {  /* already busy? */
        warn("pnc: recv when already busy ignored");
        break;                  /* yes, return and don't skip */
      }
      IOSKIP;
      rcvstat = PNCRSBUSY;        /* set receive busy */
      pncinitdma(&rcv, "rcv");
      goto rcvexit;               /* try read & maybe interrupt */

    } else if (func == 015) {   /* initiate xmit, dma chan in A */
      if (!(pncstat & PNCNSCONNECTED)) {
        break;                  /* yes, return and don't skip */
      }
      if (xmitstat & PNCXSBUSY) {  /* already busy? */
        warn("pnc: xmit when already busy ignored");
        break;                  /* yes, return and don't skip */
      }
      IOSKIP;
      pncxmit();
      goto rcvexit;                  /* rcv & interrupt following xmit */

    } else if (func == 016) {   /* set interrupt vector */
      pncvec = getcrs16(A);
      TRACE(T_INST|T_RIO, " interrupt vector = '%o\n", pncvec);
      IOSKIP;

    } else if (func == 017) {   /* set my node ID */
      myid = getcrs16(A) & 0xFF;
      if (myid > MAXNODEID) {
        printf("em: my nodeid %d > max nodeid %d; check Primenet config\n", myid, MAXNODEID);
        myid = 0;
      }
      if (myid != 0 && ni[myid].cstate == PNCCSNONE) {
        printf("em: my nodeid %d not in ring.cfg; PNC disabled\n", myid);
        myid = 0;
      }
      pncstat = (pncstat & 0xFF00) | myid;
      ni[myid].cstate = PNCCSAUTH;
      TRACE(T_INST|T_RIO, " my node id is %d\n", myid);
      IOSKIP;

    } else {
      printf("Unimplemented OTA device '%02o function '%02o, A='%o\n", device, func, getcrs16(A));
      fatal(NULL);
    }
    break;

  case 4:
    if (gettimeofday(&tv1, NULL) != 0)
      fatal("pnc gettimeofday 2 failed");
    pollts = (tv1.tv_sec + tv1.tv_usec/1000000.0) - tv0ts;
    TRACE(T_RIO, " POLL '%02o%02o @ %10.2f\n", func, device, pollts);

    /* on SIGIO, reset to the previous poll time and just do reads;
       otherwise, a regularly scheduled poll */

    if (sawsigio) {
      devpoll[device] = olddevpoll;
      sawsigio = 0;
    } else {
      devpoll[device] = PNCPOLL*gv.instpermsec;
      time(&timenow);
      pncaccept(timenow);   /* accept 1 new connection each poll */
      pncconnect(timenow);  /* finish a pending connection */
    }

rcvexit:
    if (rcv.state == PNCBSRDY)
      pncrecv();            /* try to read from any node */

intrexit:
    if (enabled && ((pncstat & 0xC000) | intstat) != intstat) {
      if (gv.intvec == -1) {
        gv.intvec = pncvec;
        intstat |= (pncstat & 0xC000);
      } else
        devpoll[device] = 100;
    }
    break;

  default:
    fatal("Bad func in devpcn");
  }
}
