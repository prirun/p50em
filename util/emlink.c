/* emlink.c, Jim Wilcoxson, February 1, 2006
   Simple telnet-like client to connect to a system & port in raw mode.
   This is designed to connect to the Prime emulator as a full-duplex
   terminal, without having to mess with a lot of telnet configuration
   and/or security issues.

   Usage:  emlink <system name/address> <port>
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <termios.h>

#define ESCAPE 035

main (int argc, char **argv) {

  int port;
  int sockfd;

  int i;
  struct hostent *server;
  struct sockaddr_in addr;
  unsigned int addrlen;
  static struct termios terminfo,resetinfo;
  static fd_set fdread,fdexcp;
  struct timeval timeout;
  unsigned char ch;
  int ttydev;
  int ttyflags, newflags;
  int n,n2;
#define BUFCHARS 16*1024
  unsigned char buf[BUFCHARS];

  sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd == -1) {
    perror ("Unable to create socket");
    exit(1);
  }
  sscanf(argv[2],"%d", &port);
  printf ("Connecting to %s port %d...\n", argv[1], port);

  server = gethostbyname(argv[1]);
  if (server == NULL) {
    fprintf(stderr,"ERROR, no such host\n");
    exit(1);
  }
  bzero((char *) &addr, sizeof(addr));
  addr.sin_family = AF_INET;
  bcopy((char *)server->h_addr, (char *)&addr.sin_addr.s_addr, server->h_length);
  addr.sin_port = htons(port);
  if (connect(sockfd, (void *) &addr,(socklen_t) sizeof(addr)) < 0) {
    fprintf(stderr,"Error connecting to server\n");
    exit(1);
  }

  /* setup terminal in raw mode */

  ttydev = 0;
  if (fcntl(ttydev, F_GETFL, ttyflags) == -1) {
    perror(" unable to get tty flags");
    exit(1);
  }
  if (tcgetattr(ttydev, &terminfo) == -1) {
    perror(" unable to get tty attributes");
    exit(1);
  }
  resetinfo = terminfo;

  terminfo.c_iflag = 0;
  terminfo.c_lflag = 0;
  terminfo.c_cflag = 0;
  terminfo.c_oflag = 0;
#if 0
  terminfo.c_cc[VMIN] = 0;
  terminfo.c_cc[VTIME] = 0;
#endif
  if (tcsetattr(ttydev, TCSANOW, &terminfo) == -1) {
    perror(" unable to set tty attributes");
    exit(1);
  }

  printf("Connected!  Use ^] to disconnect.\r\n");

  /* read/write loop, with tty in raw mode:
     read from stdin, write to socket
     read from socket, write to stdout
  */

  FD_ZERO(&fdread);
  FD_ZERO(&fdexcp);
  while (1) {

    /* wait until socket or stdin have data */

    FD_SET(0, &fdread);          /* stdin */
    FD_SET(0, &fdexcp);
    FD_SET(sockfd, &fdread);     /* socket */
    FD_SET(sockfd, &fdexcp);
#if 0
    timeout.tv_sec = 100;
    timeout.tv_usec = 0;
#endif
    n = select(sockfd+1, &fdread, NULL, &fdexcp, NULL);
    if (n == -1) {
      if (errno == EINTR)
	continue;
      tcsetattr(ttydev, TCSANOW, &resetinfo);
      perror("Unable to do read select");
      exit(1);
    }
    if (n == 0)
      continue;

    if (FD_ISSET(0, &fdexcp)) {
      tcsetattr(ttydev, TCSANOW, &resetinfo);
      fprintf(stderr,"Exception on tty\n");
      exit(1);
    }
    if (FD_ISSET(0, &fdread)) {
      n = read(0, buf, sizeof(buf));
      if (n == -1) {
	tcsetattr(ttydev, TCSANOW, &resetinfo);
	fprintf(stderr,"Error reading from stdin\n");
	exit(1);
      }
      if (n == 0) {
	tcsetattr(ttydev, TCSANOW, &resetinfo);
	fprintf(stderr,"TTY disconnected\n");
	exit(1);
      }
      if (n > 0) {
	for (i=0; i<n; i++)
	  if (buf[i] == ESCAPE) {
	    tcsetattr(ttydev, TCSANOW, &resetinfo);
	    fprintf(stderr,"\r\nUser disconnect\n");
	    exit(1);
	  }
	n2 = write(sockfd, buf, n);   /* send stdin data to socket */
	if (n2 != n) {
	  tcsetattr(ttydev, TCSANOW, &resetinfo);
	  fprintf(stderr,"Only wrote %d of %d bytes to socket\n", n2, n);
	  exit(1);
	}
      }
    }

    if (FD_ISSET(sockfd, &fdexcp)) {
      tcsetattr(ttydev, TCSANOW, &resetinfo);
      fprintf(stderr,"Exception on socket\n");
      exit(1);
    }
    if (FD_ISSET(sockfd, &fdread)) {
      n = read(sockfd, buf, sizeof(buf));
      if (n == -1) {
	tcsetattr(ttydev, TCSANOW, &resetinfo);
	fprintf(stderr,"Error reading from socket\n");
	exit(1);
      }
      if (n == 0) {
	tcsetattr(ttydev, TCSANOW, &resetinfo);
	fprintf(stderr,"Remote disconnect\n");
	exit(1);
      }
      if (n > 0) {
	n2 = write(1, buf, n);     /* send socket data to stdout */
	if (n2 != n) {
	  tcsetattr(ttydev, TCSANOW, &resetinfo);
	  fprintf(stderr,"Only wrote %d of %d bytes to stdout\n", n2, n);
	  exit(1);
	}
      }
    }
  }
  if (tcsetattr(ttydev, TCSANOW, &resetinfo) == -1)
    fprintf(stderr,"Unable to reset terminal\n");
}
