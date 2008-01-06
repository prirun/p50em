/* emdev.h, Jim Wilcoxson (prirun@gmail.com), April 17, 2005
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
   '04 = SOC/Option A/VCP board (system console/user terminal)
   '05 = #2 MPC/URC (line printer/card reader/card punch)
   '06 = card punch? (RTOS User Guide, A-1) / IPC (Engr Handbook p.101)
   '06 = Interproc. Channel (IPC) (R20 Hacker's Guide)
   '07 = #1 PNC
   '10 = ICS2 #1 or ICS1
   '11 = ICS2 #2 or ICS1
   '12 = floppy disk/diskette (magtape controller #3 at rev 22.1?)
   '13 = #2 magtape controller
   '14 = #1 magtape controller
   '15 = #5 AMLC or ICS1
   '16 = #6 AMLC or ICS1
   '17 = #7 AMLC or ICS1
   '20 = control panel / real-time clock
   '21 = 1st 4002 (Option B') disk controller
   '22 = disk #3 (0-7)
   '23 = disk #4
   '24 = disk #0 (0-7; was Writable Control Store)
   '25 = disk #2 (0-7; was 4000 disk controller)
   '26 = disk #1 (0-7)
   '27 = #7 disk (0-7)
   '30-32 = BPIOC #1-3 (RTOS User Guide, A-1)
   '32 = AMLC #8 or ICS1
   '33 = #1 Versatec
   '34 = #2 Versatec
   '35 = #4 AMLC or ICS1
   '36-37 = ELFBUS #1 & 2 (ICS1 #1, ICS1 #2)
   '40 = A/D converter type 6000
   '41 = digital input type 6020
   '42 = digital input #2
   '43 = digital output type 6040
   '44 = digital output #2
   '45 = disk #4 (0-7; was D/A converter type 6060 (analog output) - obsolete)
   '46 = disk #6 (0-7)
   '47 = #2 PNC
   '50 = #1 HSSMLC/MDLC (synchronous comm)
   '51 = #2 HSSMLC/MDLC
   '52 = #3 AMLC or ICS1
   '53 = #2 AMLC 
   '54 = #1 AMLC
   '55 = MACI autocall unit
   '56 = old SMLC (RTOS User Guide, A-1 & Hacker's Guide)
   '60-67 = reserved for user devices (GPIB)
   '70-'73 = Megatek graphics terminals
   '75-'76 = T$GPPI

   Devices emulated by Primos in ks/ptrap.ftn for I/O instructions in Ring 3:
     '01 = paper tape reader
     '02 = paper tape punch
     '04 = console
     '20 = control panel lights & sense switches

   Disk controller addresses are indirectly determined from an index
   0-n embedded in the physical device number (pdev), which has
   undergone 2 expansions.  Originally, there was only one bit used
   for the controller index - bit 9.  So the top 2-head partition on
   controller 0, unit 0 was pdev 460, and for the 2nd controller, it
   was pdev 660.  Controller 0 was conventionally at address '26, and
   controller 1 was at address '27.

   To support 4 disk controllers, Prime expanded the controller index
   to bits 9-10 of pdev, as follows:

           00 = '26     01 = '22     10 = '27     11 = '23

   This assignment meant that existing customers didn't have to change
   their disk numbering scheme.

   To support 8 disk controllers, Prime expanded the controller index
   to bits 9-11 of pdev, as follows:

   000='24  001='26  010='25  011='22  100='45  101='27  110='46  111='23

   Again, because of the assignments chosen, customer didn't have to
   change their numbering schemes.  But it's confusing nonetheless,
   because a site with one disk controller typically had it at address
   '26.  The disk controller *installation* convention was:

     '26 = 1st   '27 = 2nd   '22 = 3rd   '23 = 4th

   I don't know if there was a convention after the 4th controller, but
   I'm guessing there wasn't, because most Prime sites didn't have more
   than 4 disk controllers, especially considering that each controller
   would support 4-8 physical drives, depending on the controller type.
*/

#include "secure.h"

/* this macro is used when I/O is successful.  In VI modes, it sets
   the EQ condition code bit.  In SR modes, it does a skip */

#define IOSKIP \
  if (crs[KEYS] & 010000) \
    crs[KEYS] |= 0100; \
  else \
    RPL++

/* this macro is used to decide whether blocking should be enabled for
   an I/O operation.  The rules are:
   - if process exchange is disabled then it's safe to block
   - if the instruction (assumed to be I/O) is followed by JMP *-1 (SR
     modes) or BCNE *-2 (VI modes), then it's okay to block
*/

#if 1
#define BLOCKIO \
  (!(crs[MODALS] & 010) && (iget16(RP) == 03776 || (iget16(RP) == 0141603 && iget16(RP+1) == RPL-2)))
#else
#define BLOCKIO 0
#endif



/* this is a template for new device handlers */

int devnew (int class, int func, int device) {

  switch (class) {

  case -1:
    return 0;

  case 0:
    TRACE(T_INST, " OCP '%02o%02o\n", func, device);
    if (func == 99) {
      ;
    } else {
      printf("Unimplemented OCP device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 1:
    TRACE(T_INST, " SKS '%02o%02o\n", func, device);
    if (func == 99)
      IOSKIP;                     /* assume it's always ready */
    else {
      printf("Unimplemented SKS device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 2:
    TRACE(T_INST, " INA '%02o%02o\n", func, device);
    if (func == 99) {
      ;
    } else {
      printf("Unimplemented INA device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 3:
    TRACE(T_INST, " OTA '%02o%02o\n", func, device);
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

int devnone (int class, int func, int device) {

  static int seen[64] = {64*0};

  switch (class) {

  case -1:   /* emulator initialization */
  case -2:   /* emulator termination  */
    return 0;

  case 0:
    TRACE(T_INST, " OCP '%02o%02o\n", func, device);
    break;

  case 1:
    TRACE(T_INST, " SKS '%02o%02o\n", func, device);
    break;

  case 2:
    TRACE(T_INST, " INA '%02o%02o\n", func, device);
    break;

  case 3:
    TRACE(T_INST, " OTA '%02o%02o\n", func, device);
    break;
  }
  if (!seen[device])
    TRACEA("pio to unimplemented device '%o, class '%o, func '%o\n", device, class, func);
  seen[device] = 1;
}


/* Device '4: system console

   NOTES:
   - this driver only implements the basic needs of the system console
   - needs to reset tty attributes when emulator shuts down
   - Primos only handles ASRATE 110 (10cps), 1010 (30cps), 3410 (960 cps)
   - input ID is wrong, causes OCP '0477 on clock interrupt
   - issues with setting blocking flags & terminal attributes: doesn't block

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


int devasr (int class, int func, int device) {

/* on OSX, doing select() to see if it's okay to write means that there is
   room for at least 1024 characters.  On other platforms, this constant
   may need to be lowered.  It can be lowered to 1. */

#define MAXASRBUF 1024

  static initialized = 0;
  static FILE *conslog;
  static int ttydev;
  static int ttyflags;
  static int needflush;     /* true if data has been written but not flushed */
  static struct termios origterminfo, terminfo;
  static fd_set fds;
  static short vcptime[8] = {7*0, 1};
  static short vcptimeix;
  static short roomleft = MAXASRBUF;
  static short xoff = 0;    /* true if currently xoffed */

  struct timeval timeout;
  unsigned char ch;
  int newflags;
  int n;
  int doblock;
  time_t unixtime;
  struct tm *tms;
  
  doblock = BLOCKIO;

  switch (class) {

  case -2:    /* cleanup */
    if (!initialized) return;
    if (tcsetattr(ttydev, TCSANOW, &origterminfo) == -1)
      perror(" unable to reset tty attributes");
    fclose(conslog);
    break;

  case -1:    /* initialize */
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
    
    /* save initial terminal setup to restore when exiting */

    origterminfo = terminfo;

    /* NOTE: some of these are not restored by the host OS after the
       emulator is suspended (VSUSP) then restarted, eg, the VSUSP and
       VINTR characters */

    terminfo.c_iflag &= ~(INLCR | ICRNL | IXOFF | IXON);
    terminfo.c_lflag &= ~(ECHOCTL | ICANON);
    terminfo.c_oflag &= ~(TOSTOP);
    terminfo.c_cc[VINTR] = _POSIX_VDISABLE;  /* use ^\ instead */
    terminfo.c_cc[VSUSP] = '';
    terminfo.c_cc[VMIN] = 0;
    terminfo.c_cc[VTIME] = 0;
    if (tcsetattr(ttydev, TCSANOW, &terminfo) == -1) {
      perror(" unable to set tty attributes");
      fatal(NULL);
    }

    /* ignore SIGTTIN in case the emulator is put in the background */

#ifdef OSX
    signal(SIGTTIN, SIG_IGN);
#endif

    /* open console log file */

    if ((conslog = fopen("console.log", "w")) == NULL) {
      perror(" unable to open console log file");
      fatal(NULL);
    }
    //setvbuf(conslog, NULL, _IOLBF, 0);  /* XXX set to line buffering */
    initialized = 1;
    return 0;

  case 0:
    TRACE(T_INST, " OCP '%02o%02o\n", func, device);
    if (func == 010 || func == 012) {
      if (func == 010)       /* enable full duplex */
	terminfo.c_lflag &= ~ECHO;
      else                   /* enable "echoplex" */
	terminfo.c_lflag |= ECHO;
      if (tcsetattr(ttydev, TCSANOW, &terminfo) == -1) {
	perror(" unable to set tty attributes");
	fatal(NULL);
      }
    } else
      TRACEA("devasr: unrecognized OCP '%02o%02o\n", func, device);
    break;

  case 1:
    TRACE(T_INST, " SKS '%02o%02o\n", func, device);
    if (func == 6) {               /* skip if room for a character */
      if (xoff)                    /* no output if xoff */
	return;
      if (roomleft <= 0) {         /* shouldn't be negative, but safer */
	if (crs[MODALS] & 010)     /* PX enabled? */
	  timeout.tv_sec = 0;      /* yes, can't delay */
	else
	  timeout.tv_sec = 1;      /* single user: okay to delay */
	timeout.tv_usec = 0;
	FD_SET(ttydev, &fds);
	n = select(ttydev+1, NULL, &fds, NULL, &timeout);
	if (n == -1) {
	  perror(" unable to do write select on tty");
	  fatal(NULL);
	} else if (n > 0)
	  roomleft = MAXASRBUF;
      }
      if (roomleft > 0) {
	IOSKIP;
      }

    } else if (func == 7) {        /* skip if received a char */
      if (crs[MODALS] & 010)       /* PX enabled? */
	timeout.tv_sec = 0;        /* yes, can't delay */
      else {
	timeout.tv_sec = 1;        /* single user: okay to delay */
	fflush(gvp->tracefile);    /* flush for DIAG testing */
      }
      timeout.tv_usec = 0;
      FD_SET(ttydev, &fds);
      n = select(ttydev+1, &fds, NULL, NULL, &timeout);
      if (n == -1) {
	perror(" unable to do read select on tty");
	fatal(NULL);
      }
      if (n) {
	IOSKIP;
      }

    } else if (func <= 014)
      IOSKIP;                     /* assume it's always ready */
    break;

  /* signal SIGTTIN is ignore during initialization, so if the
     emulator is put in the background, read() will return EIO on
     OSX */

  case 2:
    TRACE(T_INST, " INA '%02o%02o\n", func, device);
    //TRACE(T_INST, "INA, RPH=%o, RPL=%o, [RP]=%o, [RP+1]=%o, BLOCKIO=%d\n", RPH, RPL, iget16(RP), iget16(RP+1),BLOCKIO);
    if (func == 0 || func == 010) {         /* read a character */
      if (doblock)
	newflags = ttyflags & ~O_NONBLOCK;
      else
	newflags = ttyflags | O_NONBLOCK;
      if (newflags != ttyflags && fcntl(ttydev, F_SETFL, newflags) == -1) {
	perror(" unable to set tty flags");
	fatal(NULL);
      }
      ttyflags = newflags;
      if (doblock) {                    /* doblock = no PX = running diags */
	fflush(gvp->tracefile);         /* flush trace buffer when testing */
	if (needflush) {
	  if (fflush(stdout) == 0) {
	    needflush = 0;
	    devpoll[device] = 0;
	  }
	  fflush(conslog);
	}
      }
readasr:
      n = read(ttydev, &ch, 1);
      if (n == 0) {
	if (doblock) {
	  usleep(500000);
	  goto readasr;
	}
      } else if (n < 0) {
	if (errno != EAGAIN && errno != EIO) {
	  perror(" error reading from tty");
	  fatal(NULL);
	}
      } else if (n == 1) {
#ifndef NOTRACE
	if (ch == '') {
	  printf("Trace owner = %o/%o\n", crs[OWNER], crs[OWNERL]);
	  if (gvp->savetraceflags == 0) {
	    TRACEA("\nTRACE ENABLED:\n\n");
	    gvp->savetraceflags = ~T_MAP;
	    gvp->savetraceflags = T_FLOW|T_FAULT;
	    gvp->savetraceflags = T_GET;
	    gvp->savetraceflags = ~0;
	  } else {
	    TRACEA("\nTRACE DISABLED:\n\n");
	    gvp->savetraceflags = 0;
	  }
	  fflush(gvp->tracefile);
	  goto readasr;
	}
#endif
	
	/* do xon/xoff processing here, because if Unix does it, the
	   emulator might block.  It would be nice to pass these onto
	   Primos so that Emacs could use them, but at older revs, the
	   system console didn't support xon/xoff.  If they are passed
	   on, then the next command usually fails because the flow
	   control characters are stored in the command line. */

	if (ch == '') {
	  xoff = 1;
	  goto readasr;
	} else if (ch == '') {
	  xoff = 0;
	  goto readasr;
	}
	xoff = 0;                /* enable output if any characters typed */
	if (func >= 010)
	  crs[A] = 0;
	crs[A] = crs[A] | ch;
	TRACE(T_INST, " character read=%o: %c\n", crs[A], crs[A] & 0x7f);
	if (!(terminfo.c_lflag & ECHO) && ch != 015) /* log all except CR */
	  fputc(ch, conslog);
	fflush(conslog);         /* immediately flush when typing */
	fflush(gvp->tracefile);
	IOSKIP;
      } else {
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
    } else if (func == 017) {    /* read xmit interrupt vector -OR- clock */
      crs[A] = vcptime[vcptimeix++];
      if (vcptimeix > 7)
	vcptimeix = 0;
      IOSKIP;
    } else {
      printf("Unimplemented INA '04 function '%02o\n", func);
      fatal(NULL);
    }
    break;

  case 3:
    TRACE(T_INST, " OTA '%02o%02o\n", func, device);
    if (func == 0) {
      ch = crs[A] & 0x7f;
      TRACE(T_INST, " char to write=%o: %c\n", crs[A], ch);
      if (ch == 0 || ch == 0x7f) {  /* ignore null and del (rubout) */
	IOSKIP;
	return;
      }
#if 0
      /* could do this here too, but Primos does it with SKS before
	 doing the OTA for the console, so it isn't really necessary.
	 Also, if done here, it might confuse a program if SKS said
	 the character could be output, then OTA said it couldn't.
	 The program might stay in a tight OTA loop and hang the
	 machine until sys console output clears */

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
      putchar(ch);
      roomleft--;
      if (ch != 015)
	putc(ch, conslog);
      needflush = 1;
      if (devpoll[device] == 0)
	devpoll[device] = gvp->instpermsec*100;
      IOSKIP;
    } else if (func == 1) {       /* write control word */
      TRACEA("OTA 4, func %d, A='%o/%d\n", func, crs[A], *(short *)(crs+A));
      IOSKIP;
    } else if (04 <= func && func <= 07) {  /* write control register 1/2 */
      TRACEA("OTA 4, func %d, A='%o/%d\n", func, crs[A], *(short *)(crs+A));
      IOSKIP;
    } else if (func == 012) {
      TRACEA("OTA 4, func %d, A='%o/%d\n", func, crs[A], *(short *)(crs+A));
      /* NOTE: does this in rev 23 when system is shutdown with '4110 in A */
      IOSKIP;

    } else if (func == 013) {
      TRACEA("OTA 4, func %d, A='%o/%d\n", func, crs[A], *(short *)(crs+A));
      /* NOTE: does this in rev 20 on settime command (set clock on VCP?) */
      IOSKIP;

    } else if (func == 017) {
      if (crs[A] == 0) {

	/* setup to read VCP battery backup clock, only available on
	   Prime models with cpuid >= 5.  All words are 2 BCD digits */

#define BCD2(i) ((((i)/10)<<4) | ((i)%10))

	unixtime = time(NULL);
	tms = localtime(&unixtime);
	vcptime[0] = BCD2(tms->tm_year);
	vcptime[1] = BCD2(tms->tm_mon+1);
	vcptime[2] = BCD2(tms->tm_mday);
	vcptime[3] = BCD2(tms->tm_wday);
	vcptime[4] = BCD2(tms->tm_hour);
	vcptime[5] = BCD2(tms->tm_min);
	vcptime[6] = BCD2(tms->tm_sec);
	vcptime[7] = 0;
	vcptimeix = 0;
      } else {
	TRACEA("OTA 4, func '%o, A='%o/%d\n", func, crs[A], *(short *)(crs+A));
      }
      IOSKIP;
    } else {
      printf("Unimplemented OTA device '%02o function '%02o, A='%o\n", device, func, crs[A]);
      fatal(NULL);
    }
    break;

  case 4:

    /* tty output is blocking, even under Primos, which means that
       writes and fflush can hang the entire system.  Console output
       should be changed to non-blocking one of these days... */

    if (needflush) {
      if (fflush(stdout) == 0)
	needflush = 0;
      else
	devpoll[device] = gvp->instpermsec*100;
      fflush(conslog);
    }
  }
}




/* Device '14 - magtape controller #1

   NOTES: 

   This code only supports 1 tape controller (4 units), but could be
   extended to support 2 controllers (eg, see disk controller code)

   All tape files are assumed to be in SimH .TAP format:
   - 4-bytes (little endian integer) indicating data record length (in bytes)
   - n bytes of data
   - repeat of the 4-byte data record length
   - odd length records have a pad byte inserted
   - if MSB of record length is set, there is an error in the record
   - record length of zero indicates a file mark (doesn't repeat)
   - record length of all 1's indicates end-of-tape mark (doesn't repeat)

   Tape files are named according to the device and unit, like disk drives:
   - dev14u0, dev14u1, dev14u2, dev14u3
   These names are usually just links to the actual tape file.

   Tape status word bits (MSB to LSB):
    1 = Vertical Parity Error
    2 = Runaway
    3 = CRC Error
    4 = LRCC Error
    5 = False Gap or Insufficient DMX Range
    6 = Uncorrectable Read Error
    7 = Raw Erorr
    8 = Illegal Command
    9 = Selected Tape Ready (online and not rewinding)
   10 = Selected Tape Online
   11 = Selected Tape is at End Of Tape (EOT)
   12 = Selected Tape is Rewinding
   13 = Selected Tape is at Beginning Of Tape (BOT)
   14 = Selected Tape is Write Protected
   15 = DMX Overrun
   16 = Rewind Interrupt

   OTA '01 Motion control A-register bits:
    1 = Select Tape Only
    2 = 0 for File operation, 1 for Record operation
    3 = 1 for Space operation
    4 = 1 for Read & Correct
    5 = 1 for 7-track
    6 = 1 for 9-track
    7 = unused
    8 = 1 for 2 chars/word, 0 for 1 char/word (not supported here)
    9-11 = motion:  100 = Forward,  010 = Reverse,  001 = Rewind
    12 = 1 for Write
    13 = 1 for Unit 0
    14 = 1 for Unit 1
    15 = 1 for Unit 2
    16 = 1 for Unit 3

    If the tape device file is empty, then BOT and EOT occur together.
    But, this doesn't happen with real magtapes, and some Prime software
    doesn't like it (Rev 23 Magsav+).  So instead of returning EOT when
    positioned to BOT, we return a read error, like a real tape would.

*/

int mtread (int fd, unsigned short *iobuf, int nw, int cmd, int *mtstat) {
  unsigned char buf[4];
  int n,reclen,reclen2,bytestoread;

  TRACE(T_TIO, " mtread, nw=%d, initial tape status is 0x%04x\n", nw, *mtstat);
  if (cmd & 0x80) {                 /* forward motion */
    if (*mtstat & 0x20)             /* already at EOT, can't read */
      return 0;
retry:
    n = read(fd, buf, 4);
    TRACE(T_TIO, " mtread read foward, %d bytes for reclen\n", n);
    if (n == 0) {                   /* now we're at EOT */
      if (*mtstat & 0x8)            /* were we at BOT? */
	*mtstat |= 0x200;           /* yes, return error instead of EOT */
      else
	*mtstat |= 0x20;            /* no, EOT is okay now */
      return 0;
    }
    *mtstat &= ~8;                   /* not at BOT now */
readerr:
    if (n == -1) {
      perror("Error reading from tape file");
      *mtstat |= 0x200;              /* raw error */
      return 0;
    }
    if (n < 4) {
      fprintf(stderr," only read %d bytes for reclen\n", n);
fmterr:
      fprintf(stderr," TAP format error at position %d\n", lseek(fd, 0, SEEK_CUR));
      *mtstat |= 0x200;              /* raw error */
      return 0;
    }
    reclen = buf[0] | (buf[1]<<8) | (buf[2]<<16) | (buf[3]<<24);
    TRACE(T_TIO,  " mtread reclen = %d bytes\n", reclen);
    if (reclen == 0) {             /* hit a file mark */
      *mtstat |= 0x100;
      return 0;
    }
    if (reclen == 0xFFFFFFFF) {    /* hit EOT mark */

      /* NOTE: simh .tap doc says to backup here, probably to wipe out
       the EOT if more data is written.  IMO, EOT should never be
       written to simulated tape files - it only causes problems.

       The Prime emulator ignores .tap EOT marks, since that makes it
       possible to concatenate .tap files */

      goto retry;
    }
    if (reclen & 0x80000000) {     /* record marked in error */
      /* XXX: can .tap have non-zero record length here? */
      fprintf(stderr,"tape read error at position %lld\n", lseek(fd, 0, SEEK_CUR));
      *mtstat |= 0xB600;           /* set all error bits */;
      return 0;
    }
    if (reclen & 1)
      warn("odd-length record in tape file!");
    if (reclen < 0) {
      fprintf(stderr," negative record length %d\n", reclen);
      goto fmterr;
    }

    /* now either position or read forward */

    if (cmd & 0x2000) {            /* spacing only */
      if ((n=lseek(fd, reclen, SEEK_CUR)) == -1) {
	perror("em: unable to forward space record");
	goto fmterr;
      } else {
	TRACE(T_TIO, " spaced forward %d bytes to position %d\n", reclen, n);
      }
    } else {
      if ((reclen+1)/2 > nw) {
	fprintf(stderr,"em: reclen = %d bytes in tape file - too big!\n", reclen);
	*mtstat |= 2;              /* set DMX overrun status */
	bytestoread = nw*2;
      } else {
	bytestoread = reclen;
      }
      n = read(fd, iobuf, bytestoread);
      TRACE(T_TIO, " mtread read %d/%d bytes of data \n", n, reclen);
      if (n == -1) goto readerr;
      if (n != bytestoread) goto fmterr;
      if (bytestoread != reclen) {     /* skip the rest of the record */
	if ((n=lseek(fd, reclen-bytestoread, SEEK_CUR)) == -1) {
	  fprintf(stderr,"em: unable to handle large record\n");
	  goto readerr;
	}
      }
    }

    /* now get the trailing record length */

    n = read(fd, buf, 4);
    TRACE(T_TIO, " mtread read %d bytes for trailer reclen\n", n);
    if (n == -1) goto readerr;
    if (n != 4) goto fmterr;
    reclen2 = buf[0] | (buf[1]<<8) | (buf[2]<<16) | (buf[3]<<24);
    if (reclen2 != reclen) {
      fprintf(stderr," record length mismatch; leading %d, trailing %d\n", reclen, reclen2);
      goto fmterr;
    }
    /* XXX: maybe should pad odd-length record with a zero... */
    return (bytestoread+1)/2;

  } else {

    /* spacing backward, see if we're at BOT */

    if (lseek(fd, 0, SEEK_CUR) == 0) {
      *mtstat = (*mtstat | 8) & ~0x20;   /* at BOT, clear EOT */
      return 0;
    }

    /* backup 4 bytes, read reclen */

    if (lseek(fd, -4, SEEK_CUR) == -1) {
      perror("backspacing trailing reclen");
      goto fmterr;
    }
    n = read(fd, buf, 4);
    if (n == -1) goto readerr;
    if (n != 4) {
      fprintf(stderr,"only read %d bytes for trailing reclen backspacing\n", n);
      goto fmterr;
    }
    reclen = buf[0] | (buf[1]<<8) | (buf[2]<<16) | (buf[3]<<24);

    /* backup reclen+8 bytes unless this is a file mark, error,
       or EOT */

    if (reclen == 0) {
      *mtstat |= 0x100;        /* set filemark status */
      goto repo;
    }
    
    /* ignore attempts to backspace over error records.  This will
       cause Magsav to read the next record instead, and it may
       recover.  Re-reading the error record 10 times won't help! */

    if (reclen & 0x80000000)   /* error record (don't report) */
      return 0;
    if (reclen == 0xFFFFFFFF) {
      warn("em: devmt: read EOT backwards??");
      reclen = 0;
      goto repo;
    }
    if (lseek(fd, -(reclen+8), SEEK_CUR) == -1) {
      perror("lseek failed backspacing");
      goto fmterr;
    }

    /* read leading reclen again to make sure we're positioned correctly */
      
    n = read(fd, buf, 4);
    if (n == -1) goto readerr;
    if (n != 4) {
      fprintf(stderr, "only read %d bytes for leading reclen backspacing\n", n);
      goto fmterr;
    }
    reclen2 = buf[0] | (buf[1]<<8) | (buf[2]<<16) | (buf[3]<<24);
    if (reclen2 != reclen) {
      fprintf(stderr," record length mismatch backspacing; leading %d, trailing %d\n", reclen, reclen2);
      goto fmterr;
    }

    /* finally, backup over the reclen to be positioned for read */

repo:

    if ((n = lseek(fd, -4, SEEK_CUR)) == -1)
      goto readerr;
    if (n == 0)
      *mtstat = (*mtstat | 8) & ~0x20;   /* at BOT, clear EOT */
    return 0;
  }
}

int mtwrite (int fd, unsigned short *iobuf, int nw, int *mtstat) {
  int n;

  n = write(fd, iobuf, nw*2);
  if (nw*2 != n) {
    perror("Error writing to tape file");
    fatal(NULL);
  }
  *mtstat &= ~8;                     /* not at BOT now */
}

int devmt (int class, int func, int device) {

  static unsigned short mtvec = 0114;          /* interrupt vector */
  static unsigned short dmxchan = 0;           /* dmx channel number */
  static unsigned short datareg = 0;           /* INA 00 data register */
  static unsigned short ready = 0;             /* true if INA 00 ready */
  static unsigned short busy = 0;              /* true if MPC busy */
  static unsigned short enabled = 0;           /* interrupts enabled */
  static unsigned short interrupting = 0;      /* 1 if pending, 2 if active */
  static unsigned short usel = 0;              /* last unit selected */
  static struct {
    int fd;                           /* tape file descriptor */
    int mtstat;                       /* last tape status */
    int firstwrite;                   /* true if next write is the first */
  } unit[4];

  int u;
  char devfile[8];

  /* the largest rec size Primos ever supported is 8K halfwords, plus
   4 words for the 4-byte .TAP format record length at the beginning &
   end of each record */

#define MAXTAPEWORDS 8*1024

  unsigned short iobuf[MAXTAPEWORDS+4]; /* 16-bit WORDS! */
  unsigned short *iobufp;
  unsigned short dmxreg;                /* DMA/C register address */
  short dmxnch;                         /* number of DMX channels - 1 */
  unsigned int dmxaddr;
  unsigned long dmcpair;
  short dmxnw, dmxtotnw;
  int i,n;
  char reclen[4];
  unsigned short ioword;

  switch (class) {

  case -1:                    /* initialize emulator device */
    for (u=0; u<4; u++) {
      unit[u].fd = -1;
      unit[u].mtstat = 0;
      unit[u].firstwrite = 1;
    }
    return 0;

  case 0:
    TRACE(T_INST|T_TIO, " OCP '%02o%02o\n", func, device);

    if (func == 012 || func == 013) {     /* set normal/diag mode - ignored */
      ;

    } else if (func == 014) {             /* ack interrupt */
      interrupting = 0;

    } else if (func == 015) {             /* set interrupt mask */
      enabled = 1;
      if (interrupting == 1)              /* if interrupt is pending */
	devpoll[device] = 10;             /* try to interrupt soon */
      
    } else if (func == 016) {             /* reset interrupt mask */
      enabled = 0;

    } else if (func == 017) {             /* initialize */
      mtvec = 0114;
      dmxchan = 0;
      datareg = 0;
      interrupting = 0;
      enabled = 0;
      ready = 0;
      busy = 1;
      usel = 0;

    } else {
      printf("Unimplemented OCP device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 1:
    TRACE(T_INST|T_TIO, " SKS '%02o%02o\n", func, device);
    if (func == 00) {                      /* skip if ready */
      if (ready)
	IOSKIP;
    } else if (func == 01) {               /* skip if not busy */
      if (busy)                            /* return busy once after init */
	busy = 0;
      else
	IOSKIP;
    } else if (func == 04) {               /* skip if not interrupting */
      if (interrupting != 2)
	IOSKIP;
    } else {
      printf("Unimplemented SKS device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 2:

    /* according to version 0 controller docs, INA '014 should only
       respond ready one time after an OTA '0214, and should respond
       not ready otherwise (ie, the INA doesn't skip).  But, this
       causes Primos to lock up, spinning in an INA '014 loop, so the
       emulator returns 0 on subsequent INA's */

    TRACE(T_INST|T_TIO, " INA '%02o%02o\n", func, device);
    if (func == 0) {
      if (!ready) {
	TRACE(T_INST|T_TIO, "INA 0 on tape device w/o matching OTA!\n");
	crs[A] = 0;
      } else {
	TRACE(T_INST|T_TIO, "  INA 0 returns '%06o 0x%04x\n", datareg, datareg);
	crs[A] = datareg;
      }
      ready = 0;
      IOSKIP;

    } else if (func == 011) {
      crs[A] = (1 << 8) | 0214;   /* backplane slot + device ID */
      IOSKIP;

    } else {
      printf("Unimplemented INA device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 3:
    TRACE(T_INST|T_TIO, " OTA '%02o%02o, A='%06o %04x\n", func, device, crs[A], crs[A]);

    if (func != 0)
      busy = 0;

    if (func == 00) {
      datareg = crs[A];
      IOSKIP;

    } else if (func == 01) {

      /* here's the hard part where everything happens... decode unit first */

      u = crs[A] & 0xF;
      if (u == 1) u = 3;
      else if (u == 2) u = 2;
      else if (u == 4) u = 1;
      else if (u == 8) u = 0;
      else
	fatal("em: no unit selected on tape OTA '01");
      usel = u;

      /* if the tape is at BOT, close and re-open it on every operation
	 so that if the tape device file is changed, we'll see the new
	 file.  Hacky, but it works. :) */

      if (unit[u].fd >= 0 && (unit[u].mtstat & 8)) {
	close(unit[u].fd);
	unit[u].fd = -1;
      }

      /* if the tape file has never been opened, do it now. */

      if (unit[u].fd == -1) {
	unit[u].mtstat = 0;
	unit[u].firstwrite = 1;
	snprintf(devfile,sizeof(devfile),"mt%d", u);
	TRACE(T_TIO, " filename for tape dev '%o unit %d is %s\n", device, u, devfile);
	if ((unit[u].fd = open(devfile, O_RDWR+O_CREAT, 0660)) == -1) {
	  if ((unit[u].fd = open(devfile, O_RDONLY)) == -1) {
	    fprintf(stderr,"em: unable to open tape device file %s for device '%o unit %d\n", devfile, device, u);
	    IOSKIP;
	    break;
	  } else
	    unit[u].mtstat = 0x00CC;   /* Ready, Online, BOT, WP */
	} else
	  unit[u].mtstat = 0x00C8;   /* Ready, Online, BOT */
      }
      
      /* "select only" is ignored.  On a real tape controller, this
       blocks (I think) if the previous tape operation is in progress */

      if ((crs[A] & 0xFFF00) == 0x8000) {
	TRACE(T_TIO, " select only\n");
	IOSKIP;
	break;
      }

      /* clear "last operation" error bits in mtstat, but preserve status
	 things like "at BOT", "at EOT", "write protected" */

      unit[u].mtstat &= 0x00EC;

      /* for rewind, read, write, & space, setup a completion interrupt */

      interrupting = 1;
      devpoll[device] = 10;

      if ((crs[A] & 0x00E0) == 0x0020) {       /* rewind */
	//gvp->traceflags = ~T_MAP;
	TRACE(T_TIO, " rewind\n");
	if (lseek(unit[u].fd, 0, SEEK_SET) == -1) {
	  perror("Unable to rewind tape drive file");
	  fatal(NULL);
	}
	unit[u].mtstat = 0x00D0;    /* Ready, Online, Rewinding */
	IOSKIP;
	break;
      }

      /* Now we're reading, writing, or spacing either a record or file:
	 - space file/record, forward or backward = okay
	 - read record = okay, read file doesn't make sense
	 - write record = okay, write file = write file mark */


      /* write file mark
	 NOTE: the tape file should probably be truncated on the first
	 write, not on each file mark, although this does let us
	 create garbage tape images for testing by doing a Magsav,
	 rewind, starting another Magsav, and aborting it. */

      if ((crs[A] & 0x4010) == 0x0010) {
	TRACE(T_TIO, " write file mark\n");
	*(int *)iobuf = 0;
	mtwrite(unit[u].fd, iobuf, 2, &unit[u].mtstat);
	ftruncate(unit[u].fd, lseek(unit[u].fd, 0, SEEK_CUR));
	IOSKIP;
	break;
      }

      /* space forward or backward a record or file at a time */

      if (crs[A] & 0x2000) {           /* space operation */
	if ((crs[A] & 0xC0) == 0)
	  warn("Motion = 0 for tape spacing operation");
	else if (crs[A] & 0x4000) {    /* record operation */
	  TRACE(T_TIO, " space record, dir=%x\n", crs[A] & 0x80);
	  mtread(unit[u].fd, iobuf, 0, crs[A], &unit[u].mtstat);
	} else {                       /* file spacing operation */
	  TRACE(T_TIO, " space file, dir=%x\n", crs[A] & 0x80);
	  do {
	    mtread(unit[u].fd, iobuf, 0, crs[A], &unit[u].mtstat);
	  } while (!(unit[u].mtstat & 0x128));  /* FM, EOT, BOT */
	}
	IOSKIP;
	break;
      }

      /* read/write backward aren't supported */

      if (((crs[A] & 0x00E0) == 0x0040) && ((crs[A] & 0x2000) == 0)) {
	warn("devtape: read/write reverse not supported");
	unit[u].mtstat = 0;
	IOSKIP;
	break;
      }

      /* for read command, fill the io buffer first.  Note that mtread
	 doesn't store record lengths in iobuf and the return value is
	 the actual data record length in words.  It also ensures that
	 the return value is never > the # of words requested (MAXTAPEWORDS)

         for write commands, the 4-byte .TAP record length IS stored in
	 iobuf and the length returned by mtwrite reflects that */

      if (crs[A] & 0x10) {         /* write record */
	TRACE(T_TIO, " write record\n");
	dmxtotnw = 0;
	iobufp = iobuf+2;
      } else {
	TRACE(T_TIO, " read record\n");
	dmxtotnw = mtread(unit[u].fd, iobuf, MAXTAPEWORDS, crs[A], &unit[u].mtstat);
	iobufp = iobuf;
      }

      /* data transfer from iobuf (read) or to iobuf (write) */

      while (1) {
	dmxreg = dmxchan & 0x7FF;
	if (dmxchan & 0x0800) {         /* DMC */
	  dmcpair = get32io(dmxreg);    /* fetch begin/end pair */
	  dmxaddr = dmcpair>>16;
	  dmxnw = (dmcpair & 0xffff) - dmxaddr + 1;
	  TRACE(T_INST|T_TIO,  " DMC channels: ['%o]='%o, ['%o]='%o, nwords=%d", dmxreg, dmxaddr, dmxreg+1, (dmcpair & 0xffff), dmxnw);
	} else {                        /* DMA */
	  dmxreg = dmxreg << 1;
	  dmxnw = regs.sym.regdmx[dmxreg];
	  dmxnw = -((dmxnw>>4) | 0xF000);
	  dmxaddr = ((regs.sym.regdmx[dmxreg] & 3)<<16) | regs.sym.regdmx[dmxreg+1];
	  TRACE(T_INST|T_TIO, " DMA channels: ['%o]='%o, ['%o]='%o/%o, nwords=%d", dmxreg, regs.sym.regdmx[dmxreg], dmxreg+1, dmxaddr>>16, dmxaddr&0xffff, dmxnw);
	}
	if (dmxnw < 0) {            /* but is legal for >32K DMC transfer... */
	  printf("devmt: requested negative DMX of size %d\n", dmxnw);
	  fatal(NULL);
	}
	if (crs[A] & 0x10) {            /* write record */
	  if (dmxtotnw+dmxnw > MAXTAPEWORDS)
	    fatal("Tape write is too big");
	  for (i=0; i < dmxnw; i++) {
	    ioword = get16io(dmxaddr+i);
#if 0
	    if (i%10 == 0)
	      TRACE(T_TIO, "\n %04d: ", i);
	    TRACE(T_TIO, " %03o %03o", (unsigned)ioword>>8, ioword&0xff);
#endif
	    *iobufp++ = ioword;
	  }
	  TRACE(T_TIO, "\n");
	  dmxtotnw = dmxtotnw + dmxnw;
	} else {
	  if (dmxnw > dmxtotnw)
	    dmxnw = dmxtotnw;
	  for (i=0; i < dmxnw; i++) {
	    ioword = *iobufp++;
#if 0
	    if (i%10 == 0)
	      TRACE(T_TIO, "\n %04d: ", i);
	    TRACE(T_TIO, " %03o %03o", (unsigned)ioword>>8, ioword&0xff);
#endif
	    put16io(ioword, dmxaddr+i);
	  }
	  TRACE(T_TIO, "\n");
	  dmxtotnw = dmxtotnw - dmxnw;
	}
	TRACE(T_TIO, " transferred %d words\n", dmxnw);
	if (dmxchan & 0x0800) {                    /* if DMC... */
	  put16io(dmxaddr+dmxnw, dmxreg);          /* update starting address */
	} else {                                   /* if DMA...
	  regs.sym.regdmx[dmxreg] += (dmxnw<<4);   /* increment # words */
	  regs.sym.regdmx[dmxreg+1] += dmxnw;      /* increment address */
	}

	/* if chaining, bump channel number and decrement # channels */

	if (dmxchan & 0xF000)
	  dmxchan = dmxchan + 2 - (1<<12);
	else
	  break;
      }

      /* for write record, do the write */

      if (crs[A] & 0x10) {             /* write record */
	n = dmxtotnw*2;
	reclen[0] = n & 0xFF;
	reclen[1] = n>>8 & 0xFF;
	reclen[2] = n>>16 & 0xFF;
	reclen[3] = n>>24 & 0xFF;
	*(int *)iobuf = *(int *)reclen;
	*(int *)(iobuf+2+dmxtotnw) = *(int *)reclen;
	mtwrite(unit[u].fd, iobuf, dmxtotnw+4, &unit[u].mtstat);
      } else {                         /* read record */
	if (dmxtotnw > 0) {
	  TRACE(T_TIO,  " DMA Overrun, lost %d words\n", dmxtotnw);
	  unit[u].mtstat |= 0x800;
	}
      }
      IOSKIP;
      break;
	
    } else if (func == 02) {
      ready = 1;
      if (crs[A] & 0x8000) {      /* status word 1 */
	datareg = unit[usel].mtstat;
	
	/* if the tape was rewinding, return rewinding status once, then
	   change it to BOT */

	if (datareg & 0x10)
	  unit[usel].mtstat = unit[usel].mtstat & ~0x10 | 0x8;
      } else if (crs[A] & 0x4000)
	datareg = 0214;           /* device ID */
      else if (crs[A] & 0x2000)
	datareg = dmxchan;
      else if (crs[A] & 0x1000)
	datareg = mtvec;
      else if (crs[A] & 0x800)
	datareg = 0;              /* status word 2 */
      else {
	TRACE(T_TIO, "  Bad OTA '02 to tape drive, A='%06o, 0x$04x\n", crs[A], crs[A]);
	datareg = 0;
	interrupting = 1;
	devpoll[device] = 10;
      }
      TRACE(T_TIO,  "  setup INA 0, datareg='%06o, 0x%04x\n", datareg, datareg);
      IOSKIP;

    } else if (func == 03) {                /* power on */
      TRACE(T_TIO,  " power on\n");
      IOSKIP;

    } else if (func == 05) {                /* illegal - DIAG */
      TRACE(T_TIO,  " illegal DIAG OTA '05\n");
      interrupting = 1;
      devpoll[device] = 10;
      IOSKIP;

    } else if (func == 014) {               /* set DMX channel */
      dmxchan = crs[A];
      TRACE(T_TIO,  " dmx channel '%o, 0x%04x\n", dmxchan, dmxchan);
      IOSKIP;

    } else if (func == 015) {               /* start u-code test */
      TRACE(T_TIO,  " u-code test\n");
      IOSKIP;

    } else if (func == 016) {               /* set interrupt vector */
      mtvec = crs[A];
      TRACE(T_TIO,  " set int vec '%o\n", mtvec);
      IOSKIP;

    } else {
      printf("Unimplemented OTA device '%02o function '%02o, A='%o\n", device, func, crs[A]);
    }
    break;

  case 4:
    TRACE(T_TIO,  " POLL device '%02o, enabled=%d, interrupting=%d\n", device, enabled, interrupting);
    if (enabled && (interrupting == 1)) {
      devpoll[device] = 100;         /* assume interrupt will be deferred */
      if (gvp->intvec == -1 && (crs[MODALS] & 0100000)) {
	TRACE(T_TIO,  " CPU interrupt to vector '%o\n", mtvec);
	gvp->intvec = mtvec;
	devpoll[device] = 0;
	interrupting = 2;
      }
    }
  }
}

/* Device '20: control panel switches and lights, and realtime clock

   OCP '0020 = start Line Frequency Clock, enable mem increment, ack previous overflow (something else on VCP?)
   OCP '0120 = ack PIC interrupt
   OCP '0220 = stop LFC, disable mem increment, ack previous overflow
   OCP '0420 = select LFC for memory increment (something else on VCP?)
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

   IMPORTANT NOTE 1: this device ('20) never skips!

   IMPORTANT NOTE 2: when the emulator wakes up after the host system
   suspends, devcp will either reset the clock (if it knows where
   Primos' DATNOW variable is), or will make the clock tick faster
   until it catches up to where it should be.  Fake clock interrupts
   are injected every 100 Prime instructions to catch up.  These fast
   interrupts can't occur too quickly (like every 10 Prime
   instructions), because CLKDIM can't execute completely in 10
   instructions and the repeated interrupts will eventually cause the
   clock semaphore to overflow.
*/

/* initclock sets Primos' real-time clock variable */

initclock(ea_t datnowea) {
  int datnow, i;
  time_t unixtime;
  struct tm *tms;

  unixtime = time(NULL);
  tms = localtime(&unixtime);
  datnow = tms->tm_year<<25 | (tms->tm_mon+1)<<21 | tms->tm_mday<<16 | ((tms->tm_hour*3600 + tms->tm_min*60 + tms->tm_sec)/4);
  put32r0(datnow, datnowea);
}


int devcp (int class, int func, int device) {
  static short enabled = 0;
  static unsigned short clkvec = 0;
  static short clkpic = -947;
  static float clkrate = 3.2;
  static unsigned long ticks = -1;
  static unsigned long absticks = -1;
  static struct timeval start_tv;
  static ea_t datnowea = 0;
  static struct timeval prev_tv;
  static unsigned long previnstcount=0; /* value of instcount corresponding to above */

  struct timeval tv;
  unsigned long elapsedms,targetticks;
  int i;

#define SETCLKPOLL devpoll[device] = gvp->instpermsec*(-clkpic*clkrate)/1000;

  switch (class) {

  case -1:

    /* if -map is used, lookup DATNOW symbol and set the 32-bit 
       Primos time value (DATNOW+TIMNOW) */

    datnowea = 0;
    for (i=0; i<numsyms; i++) {
      if (strcmp(mapsym[i].symname, "DATNOW") == 0)
	datnowea = mapsym[i].address;
    }
    return 0;

  case 0:
    TRACE(T_INST, " OCP '%02o%02o\n", func, device);

    if (func == 00) {           /* does something on VCP after OCP '0420 */
      ;

    } else if (func == 01) {    /* ack PIC interrupt */
      ; 

    } else if (func == 02) {    /* stop LFC (IO.PNC DIAG) */
      ; 

    } else if (func == 04) {    /* does something on VCP during boot */
      ;

    } else if (func == 015) {   /* set interrupt mask */
      /* this enables tracing when the clock process initializes */
      //gvp->traceflags = ~T_MAP;
      //TRACEA("Clock interrupt enabled!\n");
      enabled = 1;
      SETCLKPOLL;
      ticks = -1;

    } else if (func == 016 || func == 017) {
      enabled = 0;
      devpoll[device] = 0;

    } else {
      printf("Unimplemented OCP device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 1:
    TRACE(T_INST, " SKS '%02o%02o\n", func, device);
    printf("Unimplemented SKS device '%02o function '%02o\n", device, func);
    fatal(NULL);
    break;

  case 2:
    TRACE(T_INST, " INA '%02o%02o\n", func, device);
    if (func == 011) {             /* input ID */
      crs[A] = 020;                /* this is the Option-A board */
      crs[A] = 0120;               /* this is the SOC board */
      //gvp->traceflags = ~T_MAP;
    } else if (func == 016) {      /* read switches that are up */
      crs[A] = sswitch;
    } else if (func == 017) {      /* read switches pushed down */
      crs[A] = dswitch;
    } else {
      printf("Unimplemented INA device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 3:
    TRACE(T_INST, " OTA '%02o%02o\n", func, device);

    /* standard Primos always sets a PIC interval of 947 and uses the
       hardware clock rate of 3.2 usec.  This causes a software interrupt
       every 947*3.2 usec, or 3030.4 milliseconds, and this works out
       to 330 software interrupts per second.  BUT newer machines tick
       at whatever rate they want and ignore the PIC interval set here.

       To keep emulator overhead low, some versions of Primos have been
       modified to set the PIC interval to 15625 and send 20 ticks per
       second to Primos.

       To handle both cases, this code looks at the cpuid to determine
       the actual # of ticks per second expected by Primos whenever
       the PIC interval is set to 947.  If PIC is set to something
       other than 947, that value is used.
    */

    if (func == 02) {            /* set PIC interval */
      clkpic = *(short *)(crs+A);
      if (clkpic == -947)
	if (cpuid == 11 || cpuid == 20)
	  clkpic = -625;         /* P2250: 500 ticks/second */
	else if (cpuid >= 15)    /* newer machines: 250 ticks/second */
	  clkpic = -1250;
      TRACE(T_INST, "Clock PIC %d requested, set to %d\n", *(short *)(crs+A), clkpic);
      SETCLKPOLL;

    } else if (func == 07) {
      TRACE(T_INST, "Clock control register set to '%o\n", crs[A]);
      if (crs[A] & 020)
	clkrate = 102.4;
      else
	clkrate = 3.2;
      SETCLKPOLL;

    } else if (func == 013) {     /* set interrupt vector */
      clkvec = crs[A];
      TRACE(T_INST, "Clock interrupt vector address = '%o\n", clkvec);

    } else if (func == 017) {     /* write lights */

    } else {
      printf("Unimplemented OTA device '%02o function '%02o, A='%o\n", device, func, crs[A]);
      fatal(NULL);
    }
    break;

    /* Clock poll important considerations are:
       1. ticks = -1 initially; this triggers initialization here
       2. start_tv corresponds to the time where ticks = 0
       3. if the clock gets out of sync, it ticks faster or slower until
          it is right
       4. if the clock is WAY out of sync, try to jump to the correct time
       5. once the clock is in sync, reset start_tv if ticks gets too big
          (uh, after 5 months!) to prevent overflows in time calculations
    */

  case 4:
    /* interrupt if enabled and no interrupt active */

    if (enabled) {
      if (gvp->intvec == -1) {
	gvp->intvec = clkvec;
	SETCLKPOLL;
	ticks++;
	if (gettimeofday(&tv, NULL) != 0)
	  fatal("em: gettimeofday 3 failed");
	if (ticks == 0) {
	  start_tv = tv;
	  prev_tv = tv;
	  previnstcount = gvp->instcount;
	  if (datnowea != 0)
	    initclock(datnowea);
	} 
	elapsedms = (tv.tv_sec-start_tv.tv_sec-1)*1000 + (tv.tv_usec+1000000-start_tv.tv_usec)/1000;
	targetticks = elapsedms/(-clkpic*clkrate/1000);
#if 0
	absticks++;
	if (absticks%1000 == 0 || abs(ticks-targetticks) > 5)
	  printf("\nClock: target=%d, ticks=%d, offset=%d\n", targetticks, ticks, ticks-targetticks);
#endif

	/* if the clock gets way out of whack (eg, because of a host
	   suspend), reset it IFF datnowea is set.  This causes an
	   immediate jump to the correct time.  If datnowea is not
	   available (no Primos maps), then we have to tick our way to
	   the correct time.  In addition to lowering overhead, slower
	   clock tick rates make catching up much faster.
	   
	   When ticking faster, we need to be careful not to tick too
	   fast, because that will cause timed events like disk I/O to
	   timeout prematurely.  With rev 20 Primos, setting the
	   faster poll time to 500 instructions will cause a failure
	   if the disk is active, 750 instructions works, so I set it
	   to a minimum of 1000 instructions for a safety margin. */

#ifndef FIXEDCLOCK
	if (abs(ticks-targetticks) > 5000 && datnowea != 0)
	  ticks = -1;
	else if (ticks < targetticks)           /* behind, so catch up */
	  if (targetticks-ticks < 100)
	    devpoll[device] = devpoll[device]/2;  /* slow catch up */
	  else
	    devpoll[device] = 1000;               /* fast catch up */
	else if (ticks > targetticks)
	  devpoll[device] = devpoll[device]*2;  /* ahead, so slow down */
	else {                                  /* just right! */
	  if (ticks > 1000000000) {             /* after a long time, */
	    start_tv = tv;                      /* reset tick vars */
	    ticks = 0;
	  }
	}
#endif

	/* update instpermsec every 5 seconds.  Check for instcount
	   overflow and reset when it occurs.

	   XXX: this code should probably be done whether or not the
	   clock is running */

	if ((gvp->instcount < previnstcount) || (gvp->instcount-previnstcount > gvp->instpermsec*1000*5)) {
	  if (gvp->instcount-previnstcount > gvp->instpermsec*1000*5) {
	    gvp->instpermsec = (gvp->instcount-previnstcount) /
	      ((tv.tv_sec-prev_tv.tv_sec-1)*1000 + (tv.tv_usec+1000000-prev_tv.tv_usec)/1000);
	    //printf("gvp->instcount = %u, previnstcount = %u, diff=%u, instpermsec=%d\n", gvp->instcount, previnstcount, gvp->instcount-previnstcount, gvp->instpermsec);
#ifdef NOIDLE
	    //printf("\ninstpermsec=%d\n", gvp->instpermsec);
#endif

	    /* call the security check code every 5 seconds */

	    secure(tv);
	  }
	  previnstcount = gvp->instcount;
	  prev_tv = tv;
	}
      } else {
	devpoll[device] = 100;         /* couldn't interrupt, try again soon */
      }
    }
    break;
  }
}


/* helper function to return a disk's file name given a controller and unit */

int globdisk (char *devfile, int size, int device, int unit) {
  glob_t g;
  int globerr;

  snprintf(devfile, size, "disk%ou%d.*", device, unit);
  if ((globerr=glob(devfile, GLOB_ERR|GLOB_NOSORT, NULL, &g)) != 0) {
    fprintf(stderr,"globdisk: glob returned %d opening %s\n", globerr, devfile);
    return -1;
  }
  if (g.gl_pathc != 1) {
    fprintf(stderr,"globdisk: %d matches for %s\n", g.gl_pathc, devfile);
    return -1;
  }
  strncpy(devfile, g.gl_pathv[0], size);
  devfile[size-1] = 0;
  TRACE(T_INST|T_DIO, " filename for dev '%o unit %d is %s\n", device, unit, devfile);
  return 0;
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

#include "geomhash.h"

int devdisk (int class, int func, int device) {

#include "geom.h"

#define S_HALT 0
#define S_RUN 1
#define S_INT 2

#define HASHMAX 4451

#if 1
  #define CID4005 0100
#else
  #define CID4005 0
#endif


  static struct {
    unsigned short device;                 /* controller device address */
    unsigned short oar;
    unsigned short state;                  /* channel program state: S_XXXX */
    unsigned short status;                 /* controller status */
    short usel;                            /* unit selected (0-3, -1=none) */
    short dmachan;                         /* dma channel selected */
    short dmanch;                          /* number of dma channels-1 */
    struct {
      int rtfd;                            /* read trace file descriptor */
      short heads;                         /* total heads */
      short spt;                           /* sectors per track */
      short maxtrack;                      /* cylinder limit */
      short curtrack;                      /* current head position */
      short wp;                            /* true if write protected */
      int devfd;                           /* Unix device file descriptor */
      int readnum;                         /* increments on each read */
      unsigned char** modrecs;             /* hash table of modified records */
    } unit[MAXDRIVES];
  } dc[MAXCTRL];

  short i,u;
  int dx;

  /* temps for running channel programs */

  unsigned short order;
  unsigned short m,m1,m2;
  short head, track, rec, recsize, nwords;
  unsigned short dmareg;
  unsigned int dmaaddr;
  unsigned char *hashp;
  int lockkey;

  unsigned short iobuf[1040];             /* local I/O buf (for mapped I/O) */
  unsigned short *iobufp;
  unsigned short access;
  short dmanw, dmanw1, dmanw2;
  unsigned int utempl;
  char ordertext[8];
  int phyra;
  int nb;                   /* number of bytes returned from read/write */
  char devfile[16];
  char rtfile[16];          /* read trace file name */
  int rtnw;                 /* total number of words read (all channels) */

  /* map device id to device context index

     NOTE: the dc table is indexed the way a typical Prime installation
     was setup, and is not related to the pdev bits!  See notes above. */


  switch (device) {
  case 026: dx = 0; break;
#ifndef HOBBY
  case 027: dx = 1; break;
  case 022: dx = 2; break;
  case 023: dx = 3; break;
  case 024: dx = 4; break;
  case 025: dx = 5; break;
  case 045: dx = 6; break;
  case 046: dx = 7; break;
#endif
  default:
    fprintf(stderr, "devdisk: non-disk device id '%o ignored\n", device);
    return -1;
  }


  //gvp->traceflags |= T_DIO;

  switch (class) {

  case -1:
#ifdef DISKSAFE
    printf("em: Running in DISKSAFE mode; no changes will be permanent\n");
#endif
    dc[dx].device = device;
    dc[dx].state = S_HALT;
    dc[dx].status = 0100000;
    dc[dx].usel = -1;
    if (geomcksum != geomhash((char *)geom, sizeof(geom)))
      RP=MAKEVA(01000,0);
    for (u=0; u<MAXDRIVES; u++) {
      dc[dx].unit[u].rtfd = -1;
      dc[dx].unit[u].heads = -1;
      dc[dx].unit[u].spt = -1;
      dc[dx].unit[u].maxtrack = -1;
      dc[dx].unit[u].curtrack = -1;
      dc[dx].unit[u].wp = -1;
      dc[dx].unit[u].devfd = -1;
      dc[dx].unit[u].readnum = -1;
      dc[dx].unit[u].modrecs = NULL;
    }
    return 0;
      
  case 0:
    TRACE(T_INST|T_DIO, " OCP '%2o%2o\n", func, device);
    if (func == 016) {                /* reset interrupt */
      if (dc[dx].state == S_INT) {
	dc[dx].state = S_RUN;
	devpoll[device] = 1;
      }
    } else if (func == 017) {         /* reset controller */
      dc[dx].state = S_HALT;
      dc[dx].status = 0100000;
      dc[dx].usel = -1;
    } else {
      printf(" Unrecognized OCP '%2o%2o\n", func, device);
      fatal(NULL);
    }
    break;

  case 1:
    TRACE(T_INST|T_DIO, " SKS '%2o%2o\n", func, device);
    if (func == 04) {                  /* skip if not interrupting */
      if (dc[dx].state != S_INT)
	IOSKIP;
    } else {
      printf(" Unrecognized SKS '%2o%2o\n", func, device);
      fatal(NULL);
    }
    break;

  case 2:
    TRACE(T_INST|T_DIO, " INA '%2o%2o\n", func, device);

    /* this turns tracing on when the Primos disk processes initialize */
    //gvp->traceflags = ~T_MAP;

    /* INA's are only accepted when the controller is not busy */

    if (dc[dx].state != S_HALT)
      return;

    if (func == 01)          /* read device id, clear A first */
      crs[A] = CID4005 + device;
    else if (func == 011)    /* read device id, don't clear A */
      crs[A] |= (CID4005 + device);
    else if (func == 017) {  /* read OAR */
      crs[A] = dc[dx].oar;
    } else {
      printf("Unimplemented INA device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    IOSKIP;
    break;

  case 3:
    TRACE(T_INST|T_DIO, " OTA '%02o%02o\n", func, device);
    if (func == 017) {        /* set OAR (order address register) */
      dc[dx].state = S_RUN;
      dc[dx].oar = crs[A];
      devpoll[device] = 1;
    } else {
      printf("Unimplemented OTA device '%02o function '%02o, A='%o\n", device, func, crs[A]);
      fatal(NULL);
    }
    IOSKIP;
    break;

  case 4:   /* poll (run channel program) */

    while (dc[dx].state == S_RUN) {
      m = get16io(dc[dx].oar);
      m1 = get16io(dc[dx].oar+1);
      TRACE(T_INST|T_DIO, "\nDIOC %o: %o %o %o\n", dc[dx].oar, m, m1, get16io(dc[dx].oar+2));
      dc[dx].oar += 2;
      order = m>>12;

      /* this is for conditional execution, and needs some work... */

      if (m & 04000) {   /* "execute if ..." */
	if (order == 2 || order == 5 || order == 6)
	  dc[dx].oar++;
	continue;
      }

      switch (order) {

      case 0: /* DHLT = Halt */
	dc[dx].state = S_HALT;
	devpoll[device] = 0;
	TRACE(T_INST|T_DIO, " channel halted at '%o\n", dc[dx].oar);
	break;

      case 2: /* SFORM = Format */
      case 5: /* SREAD = Read */
      case 6: /* SWRITE = Write */
	dc[dx].status &= ~076000;             /* clear bits 2-6 */
	m2 = get16io(dc[dx].oar++);
	recsize = m & 017;
	track = m1 & 03777;
	rec = m2 >> 8;   /* # records for format, rec # for R/W */
	head = m2 & 077;
	u = dc[dx].usel;
	if (order == 2)
	  strcpy(ordertext,"Format");
	else if (order == 5)
	  strcpy(ordertext,"Read");
	else if (order == 6)
	  strcpy(ordertext,"Write");
	TRACE(T_INST|T_DIO, "%s, head=%d, track=%d, rec=%d, recsize=%d\n", ordertext, head, track, rec, recsize);
	if (u == -1) {
	  fprintf(stderr," Device '%o, order %d with no unit selected\n", device, order);
	  dc[dx].status |= 2;      /* select error (right?)... */
	  break;
	}
	if (recsize != 0) {
	  fprintf(stderr," Device '%o, order %d, recsize=%d\n", device, order, recsize);
	  dc[dx].status |= 02000;  /* header check (right error?) */
	  break;
	}
#if 0
	/* this has been disabled because preseeks in the disk driver sometimes
	   goof up, depending on timings in the emulator */

	if (track != dc[dx].unit[u].curtrack) {
	  fprintf(stderr," Device '%o, order %d at track %d, but positioned to track %d\n", device, order, track, dc[dx].unit[u].curtrack);
	  dc[dx].status |= 4;      /* illegal seek */
	  break;
	}
#endif
	if (track > dc[dx].unit[u].maxtrack) {
	  fprintf(stderr," Device '%o, unit %d, seek to track %d > cylinder limit of %d\n", device, u, track, dc[dx].unit[u].maxtrack);
	  dc[dx].status |= 4;      /* illegal seek */
	  break;
	}

	if (head > dc[dx].unit[u].heads) {
	  fprintf(stderr," Device '%o, unit %d, head %d > head limit of %d\n", device, u, head, dc[dx].unit[u].heads);
	  dc[dx].status |= 2;      /* select error (right?)... */
	  break;
	}

	if (dc[dx].unit[u].devfd < 0) {
	  TRACE(T_INST|T_DIO, " Device '%o unit %d not ready\n", device, u);
	  dc[dx].status = 0100001;
	} else if (order == 2) {
	  TRACE(T_INST|T_DIO, " Format order\n");
	  //fatal("DFORMAT channel order not implemented");
	} else {            /* order = 5 (read) or 6 (write) */

	  /* translate head/track/sector to drive record address */

	  phyra = (track*dc[dx].unit[u].heads*dc[dx].unit[u].spt) + head*dc[dx].unit[u].spt + rec;
	  TRACE(T_INST|T_DIO,  " Unix ra=%d, byte offset=%d\n", phyra, phyra*2080);
	  /* XXX: check for phyra > 1032444, which is > 2GB max file size */

	  /* does this record exist in the disk unit hash table?  If it does,
	     we'll do I/O to the hash entry.  If it doesn't, then for read,
	     go to the disk file; for write, make a new hash entry */

	  hashp = NULL;

#ifdef DISKSAFE
	  //fprintf(stderr," R/W, modrecs=%p\n", dc[dx].unit[u].modrecs);
	  for (hashp = dc[dx].unit[u].modrecs[phyra%HASHMAX]; hashp != NULL; hashp = *((unsigned char **)hashp)) {
	    //fprintf(stderr," lookup, hashp=%p\n", hashp);
	    if (phyra == *((int *)(hashp+sizeof(void *))))
	      break;
	  }
	  //fprintf(stderr,"After search, hashp=%p\n", hashp);

	  if (hashp == NULL)
	    if (order == 5) {        /* read */
#endif
	      if (lseek(dc[dx].unit[u].devfd, phyra*2080, SEEK_SET) == -1) {
		fprintf(stderr,"devdisk: seek error, phyra=%d\n", phyra);
		perror("Unable to seek drive file");
		fatal(NULL);
	      }
#ifdef DISKSAFE
	    } else {                 /* write */
	      hashp = malloc(1040*2 + sizeof(void*) + sizeof(int));
	      *(unsigned char **)hashp = dc[dx].unit[u].modrecs[phyra%HASHMAX];
	      *((int *)(hashp+sizeof(void *))) = phyra;
	      //fprintf(stderr," Write, new hashp = %p, old bucket head = %p\n", hashp, *(unsigned char **)hashp);
	      dc[dx].unit[u].modrecs[phyra%HASHMAX] = hashp;
	      hashp = hashp + sizeof(void*) + sizeof(int);
	    }
	  else
	    hashp = hashp + sizeof(void*) + sizeof(int);
	  //fprintf(stderr," Before disk op %d, hashp=%p\n", order, hashp);
#endif

	  while (dc[dx].dmanch >= 0) {
	    dmareg = dc[dx].dmachan << 1;
	    dmanw = regs.sym.regdmx[dmareg];
	    dmanw = -(dmanw>>4);
	    if (dmanw > 1040) {
	      warn("disk I/O limited to 1040 words");
	      dmanw = 1040;
	    }
	    if (dmanw < 0) {
	      warn("disk I/O size < 0; set to 0");
	      dmanw = 0;
	    }
	    dmaaddr = ((regs.sym.regdmx[dmareg] & 3)<<16) | regs.sym.regdmx[dmareg+1];
	    TRACE(T_INST|T_DIO,  " DMA channels: nch-1=%d, ['%o]='%o, ['%o]='%o, nwords=%d\n", dc[dx].dmanch, dc[dx].dmachan, regs.sym.regdmx[dmareg], dc[dx].dmachan+1, dmaaddr, dmanw);
	    
	    if (order == 5) {
	      if (crs[MODALS] & 020)
		if ((dmaaddr & 01777) || dmanw > 1024)
		  iobufp = iobuf;
		else
		  iobufp = MEM+mapio(dmaaddr);
	      else
		iobufp = MEM+dmaaddr;
	      if (hashp != NULL) {
		memcpy((char *)iobufp, hashp, dmanw*2);
		hashp += dmanw*2;
	      } else if ((nb=read(dc[dx].unit[u].devfd, (char *)iobufp, dmanw*2)) != dmanw*2) {
		if (nb != 0) fprintf(stderr, "Disk read error: device='%o, u=%d, fd=%d, nb=%d\n", device, u, dc[dx].unit[u].devfd, nb);
		if (nb == -1) {
		  perror("Unable to read drive file");
		  dc[dx].status |= 010000;  /* read check */
		}
		memset((char *)iobufp, 0, dmanw*2);
	      }
	      if (iobufp == iobuf)
		for (i=0; i<dmanw; i++)
		  put16io(iobuf[i], dmaaddr+i);
	    } else {
	      if (crs[MODALS] & 020) {
		iobufp = iobuf;
		for (i=0; i<dmanw; i++)
		  iobuf[i] = get16io(dmaaddr+i);
	      } else
		iobufp = MEM+dmaaddr;
	      if (hashp != NULL) {
		memcpy(hashp, (char *)iobufp, dmanw*2);
		hashp += dmanw*2;
	      } else if (write(dc[dx].unit[u].devfd, (char *)iobufp, dmanw*2) != dmanw*2) {
		perror("Unable to write drive file");
		fatal(NULL);
	      }
	    }
	    regs.sym.regdmx[dmareg] = 0;
	    regs.sym.regdmx[dmareg+1] += dmanw;
	    dc[dx].dmachan += 2;
	    dc[dx].dmanch--;
	  }
	}
	break;

      case 3: /* SSEEK = Seek */
	u = dc[dx].usel;
	if (u == -1) {
	  fprintf(stderr," Device '%o, order %d with no unit selected\n", device, order);
	  dc[dx].status |= 2;      /* select error (right?)... */
	  break;
	}
	dc[dx].status &= ~4;   /* clear bit 14: seek error */
	if (m1 & 0100000) {
	  track = 0;
	} else {
	  track = m1 & 03777;
	}
	TRACE(T_INST|T_DIO, " seek track %d, restore=%d, clear=%d\n", track, (m1 & 0100000) != 0, (m1 & 040000) != 0);
#if 0
	/* this has been disabled because SCSI drives sometimes seek to 
	   track 512 (special meaning in controller?) */

	if (track > dc[dx].unit[u].maxtrack) {
	  fprintf(stderr," Device '%o, unit %d, seek to track %d > cylinder limit of %d\n", device, u, track, dc[dx].unit[u].maxtrack);
	  dc[dx].status |= 4;    /* set bit 14: seek error */
	  track = -1;
	}
#endif
	dc[dx].unit[u].curtrack = track;
	break;

      case 4: /* DSEL = Select unit */
	u = (m1 & 0377);            /* get unit bits */
	dc[dx].status &= ~3;  /* clear 15-16: select err + unavailable */
	dc[dx].usel = -1;      /* de-select */
	if (u == 0) {
	  TRACE(T_INST|T_DIO, " de-select\n");
	  break;
	}
	if (u == 1) u = 0;
#ifndef HOBBY
	else if (u == 2) u = 1;
	else if (u == 4) u = 2;
	else if (u == 8) u = 3;
	else if (u == 16) u = 4;
	else if (u == 32) u = 5;
	else if (u == 64) u = 6;
	else if (u == 128) u = 7;
#endif
	else u = 99;
	if (u >= MAXDRIVES) {
	  fprintf(stderr," Device '%o, bad select '%o\n", device, u);
	  dc[dx].status |= 0100002;  /* set bit 15: select error */
	  break;
	}
	TRACE(T_INST|T_DIO, " select unit %d\n", u);
	if (dc[dx].unit[u].devfd < 0) {
	  if (dc[dx].unit[u].devfd == -2 || globdisk(devfile, sizeof(devfile), device, u) != 0) {
	    dc[dx].unit[u].devfd = -2;
	    dc[dx].status |= 0100001;  /* set bit 16: not ready */
	    break;
	  }
	  if ((dc[dx].unit[u].devfd = open(devfile, O_RDWR)) == -1) {
	    if ((dc[dx].unit[u].devfd = open(devfile, O_RDONLY)) == -1) {
	      fprintf(stderr, "em: unable to open disk device file %s for device '%o unit %d\n", devfile, device, u);
	      dc[dx].unit[u].devfd = -2;
	      dc[dx].status = 0100001;    /* not ready */
	      break;
	    } else {
	      lockkey = LOCK_SH;
	      dc[dx].unit[u].wp = 1;
	    }
	  } else {
	    lockkey = LOCK_EX;
	    dc[dx].unit[u].wp = 0;
	  }
	  /* determine geometry from disk file suffix */
	  for (i=0; i < NUMGEOM; i++)
	    if (strcasestr(devfile, geom[i].suffix)) {
	      dc[dx].unit[u].heads = geom[i].heads;
	      dc[dx].unit[u].spt = geom[i].spt;
	      dc[dx].unit[u].maxtrack = geom[i].maxtrack;
	      break;
	    }
	  if (i == NUMGEOM) {
	    fprintf(stderr, "em: unknown geometry for %s\n", devfile);
	    close(dc[dx].unit[u].devfd);
	    dc[dx].unit[u].devfd = -2;
	    dc[dx].status = 0100001;    /* not ready */
	    break;
	  }
#ifdef DISKSAFE
	  if (flock(dc[dx].unit[u].devfd, LOCK_SH+LOCK_NB) == -1)
	    fatal("Disk drive file is in use");
	  dc[dx].unit[u].modrecs = calloc(HASHMAX, sizeof(void *));
	  //fprintf(stderr," Device '%o, unit %d, modrecs=%p\n", device, u, dc[dx].unit[u].modrecs);
#else
	  if (flock(dc[dx].unit[u].devfd, lockkey+LOCK_NB) == -1)
	    fatal("Disk drive file is in use");
#endif
	}
	dc[dx].usel = u;
	break;

      case 7: /* DSTALL = Stall */
	TRACE(T_INST|T_DIO, " stall\n");

	/* NOTE: technically, the stall command is supposed to wait
	   210 usecs, so that the disk controller doesn't hog the I/O
	   bus by looping in a channel program waiting for I/O to
	   complete.  With the emulator, this delay isn't necessary,
	   although it will cause DIAG tests to fail if the delay is
	   omitted.  Hence the PX test.  Ignoring stall gives a 25%
	   increase in I/O's per second on a 2GHz Mac (8MB emulator). */

	if (crs[MODALS] & 010)             /* PX enabled? */
	  break;                           /* yes, no stall */
	devpoll[device] = gvp->instpermsec/5;   /* 200 microseconds, sb 210 */
	return;

      case 9: /* DSTAT = Store status to memory */
	TRACE(T_INST|T_DIO,  " store status='%o to '%o\n", dc[dx].status, m1);
	put16io(dc[dx].status,m1);
	break;

      case 11: /* DOAR = Store OAR to memory (2 words) */
	TRACE(T_INST|T_DIO,  " store OAR='%o to '%o\n", dc[dx].oar, m1);
	put16io(dc[dx].oar,m1);
	break;

      case 13: /* SDMA = select DMA channel(s) to use */
	dc[dx].dmanch = m & 017;
	dc[dx].dmachan = m1;
	TRACE(T_INST|T_DIO,  " set DMA channels, nch-1=%d, channel='%o\n", dc[dx].dmanch, dc[dx].dmachan);
	break;

      case 14: /* DINT = generate interrupt through vector address */
	TRACE(T_INST|T_DIO,  " interrupt through '%o\n", m1);
	if (gvp->intvec >= 0 || !(crs[MODALS] & 0100000))
	  dc[dx].oar -= 2;     /* can't take interrupt right now */
	else {
	  gvp->intvec = m1;
	  dc[dx].state = S_INT;
	}
	//gvp->traceflags = ~T_MAP;
	devpoll[device] = 10;
	return;

      case 15: /* DTRAN = channel program jump */
	dc[dx].oar = m1;
	TRACE(T_INST|T_DIO,  " jump to '%o\n", m1);
	break;

      default:
	/* NOTE: orders 1 & 8 are supposed to halt the channel program
	   but leave the controller busy (model 4004).
           Orders 10 & 12 (Store/Load) are not implemented */
	printf("Unrecognized channel order = %d\n", order);
	fatal(NULL);
      }
    }
  }
}

#ifndef HOBBY

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
  if ((dc[dx].ctinterrupt || dc[dx].xmitenabled || (dc[dx].recvenabled & dc[dx].connected))) \
    if (devpoll[device] == 0 || devpoll[device] > AMLCPOLL*gvp->instpermsec/pollspeedup) \
      devpoll[device] = AMLCPOLL*gvp->instpermsec/pollspeedup;  /* setup another poll */

int devamlc (int class, int func, int device) {

#define MAXLINES 128
#define MAXBOARDS 8
#define MAXROOM 1024

  /* AMLC poll rate (ms).  Max data rate = queue size*1000/AMLCPOLL
     The max AMLC output queue size is 1023 (octal 2000), so a poll
     rate of 33 (1000/33 = 30 times per second) will generate about
     31,000 chars per second.  This rate may be further boosted if
     there are lines DMQ buffers with 255 or more characters. */

#define AMLCPOLL 50

  /* DSSCOUNTDOWN is the number of carrier status requests that should
     occur before polling real serial devices.  Primos does a carrier
     check 5x per second.  All this is really good for is disconnecting
     logged-out terminals, so we poll the real status every 5 seconds. */

#define DSSCOUNTDOWN 25

#if 1
  #define QAMLC 020000   /* this is to enable QAMLC/DMQ functionality */
#else
  #define QAMLC 0
#endif

  /* connection types for each line.  This _doesn't_ imply the line is
     actually connected, ie, an AMLC line may be tied to a specific 
     serial device (like a USB->serial gizmo), but the USB device may
     not be plugged in.  The connection type would be CT_SERIAL but
     the line's fd would be zero and the "connected" bit would be 0 */
     
#define CT_SOCKET 1
#define CT_SERIAL 2

  /* terminal states needed to process telnet connections */

#define TS_DATA 0      /* data state, looking for IAC */
#define TS_IAC 1       /* have seen initial IAC */
#define TS_SUBOPT 2    /* inside a suboption */
#define TS_OPTION 3    /* inside an option */

  static short inited = 0;
  static int pollspeedup = 1;
  static int baudtable[16] = {1200, 2400, 4800, 9600, 19200, 38400, 57600, 115200};
  static char ttymsg[1024];
  static int ttymsglen = 0;
  static int tsfd;                      /* socket fd for terminal server */

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
    unsigned short dedicated;           /* 1 bit per line */
    unsigned short dsstime;             /* countdown to dss poll */
             short fd[16];              /* Unix fd, 1 per line */
    unsigned short tstate[16];          /* telnet state */
    unsigned short room[16];            /* room (chars) left in input buffer */
    unsigned short lconf[16];           /* line configuration word */
    unsigned short ctype[16];           /* connection type for each line */
    unsigned short modemstate[16];      /* Unix modem state bits (serial) */
    unsigned short recvlx;              /* next line to check for recv data */
    unsigned short pclock;              /* programmable clock */
    char dmqmode;                       /* 0=DMT, 1=DMQ */
    char bufnum;                        /* 0=1st input buffer, 1=2nd */
    char eor;                           /* 1=End of Range on input */
  } dc[MAXBOARDS];

  int dx, dx2, lx, lcount;
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
  unsigned char buf[1024];      /* max size of DMQ buffer */
  int i, n, maxn, n2, nw;
  fd_set fds;
  struct timeval timeout;
  unsigned char ch;
  int state;
  int msgfd;
  int allbusy;
  unsigned short qtop, qbot, qtemp;
  unsigned short qseg, qmask, qents;
  ea_t qentea;
  char line[100];
  int lc;
  FILE *cfgfile;
  char devname[32];
  int baud;
  struct termios terminfo;
  int modemstate;
  int maxxmit;
  int tcpoptval;

  /* save this board's device id in the dc[] array so we can tell
     what order the boards should be in.  This is necessary to find
     the clock line: the line that controls the AMLC poll rate.  The
     last line on the last defined board is the clock line.  The
     emulator also assumes that there are no gaps in the AMLC board
     configuration, which I think is also a Primos requirement, ie,
     you can't have board '54 and '52, with '53 missing. */

  switch (device) {
  case 054: dx = 0; break;
  case 053: dx = 1; break;
  case 052: dx = 2; break;
  case 035: dx = 3; break;
  case 015: dx = 4; break;
  case 016: dx = 5; break;
  case 017: dx = 6; break;
  case 032: dx = 7; break;
  default:
    fprintf(stderr, "devamlc: non-AMLC device id '%o ignored\n", device);
    return -1;
  }

  switch (class) {

  case -1:

    /* this part of initialization only occurs once, no matter how
       many AMLC boards are configured.  Parts of the amlc device
       context that are emulator-specfic need to be initialized here,
       only once, because Primos may issue the OCP to initialize an
       AMLC board more than once.  This would interfere with the
       dedicated serial line setup. */

    if (!inited) {

      /* initially, we don't know about any AMLC boards */

      for (dx2=0; dx2<MAXBOARDS; dx2++) {
	dc[dx2].deviceid = 0;
	for (lx = 0; lx < 16; lx++) {
	  dc[dx2].connected = 0;
	  dc[dx2].dedicated = 0;
	  for (lx = 0; lx < 16; lx++) {
	    dc[dx2].fd[lx] = -1;
	    dc[dx2].tstate[lx] = TS_DATA;
	    dc[dx2].room[lx] = 64;
	    dc[dx2].lconf[lx] = 0;
	    dc[dx2].ctype[lx] = CT_SOCKET;
	    dc[dx2].modemstate[lx] = 0;
	  }
	  dc[dx2].recvlx = 0;
	}
      }

      /* read the AMLC file, to see if any lines should be connected to
	 real serial devices.  This file has the format:
	   <line #> <Unix device name>
         The entries can be in any order.  If the line number begins with
	 a zero, it is assumed to be octal.  If no zero, then decimal.
      */

      if ((cfgfile = fopen("amlc", "r")) == NULL) {
	if (errno != ENOENT)
	  printf("em: error opening amlc config file: %s", strerror(errno));
      } else {
	lc = 0;
	while (fgets(line, sizeof(line), cfgfile) != NULL) {
	  lc++;
	  line[sizeof(devname)] = 0;   /* don't let sscanf overwrite anything */
	  if (line[0] == '0')
	    n = sscanf(line, "%o %s", &i, devname);
	  else
	    n = sscanf(line, "%d %s", &i, devname);
	  if (n != 2) {
	    printf("em: Can't parse amlc config file line #%d: %s\n", lc, line);
	    continue;
	  }
	  if (i < 0 || i >= MAXLINES) {
	    printf("em: amlc line # '%o (%d) out of range in amlc config file at line #%d: %s\n", i, i, lc, line);
	    continue;
	  }
	  //printf("devamlc: lc=%d, line '%o (%d) set to device %s\n", lc, i, i, devname);
	  dx2 = i/16;
	  lx = i & 0xF;
#if 1
	  if ((fd = open(devname, O_RDWR | O_NONBLOCK | O_EXLOCK)) == -1) {
#else
	  if ((fd = open(devname, O_RDWR | O_NONBLOCK)) == -1) {
#endif
	    printf("em: error connecting AMLC line '%o (%d) to device %s: %s\n", i, i, devname, strerror(errno));
	    continue;
	  }
	  printf("em: connected AMLC line '%o (%d) to device %s\n", i, i, devname);
	  dc[dx2].fd[lx] = fd;
	  dc[dx2].connected |= BITMASK16(lx+1);
	  dc[dx2].dedicated |= BITMASK16(lx+1);
	  dc[dx2].ctype[lx] = CT_SERIAL;
	}
	fclose(cfgfile);
      }

      /* start listening for incoming telnet connections */

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
	fprintf(stderr, "-tport is zero, can't start AMLC devices\n");
      inited = 1;
    }

    /* this part of initialization occurs for every AMLC board */

    if (!inited || tport == 0)
      return -1;

    dc[dx].deviceid = device;
    return 0;

  case 0:
    TRACE(T_INST, " OCP '%02o%02o\n", func, device);
    //printf(" OCP '%02o%02o\n", func, device);

    if (func == 012) {            /* set normal (DMT) mode */
      dc[dx].dmqmode = 0;

    } else if (func == 013 && QAMLC) {     /* set diagnostic (DMQ) mode */
      dc[dx].dmqmode = 1;

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
      dc[dx].pclock = 0;
      dc[dx].dmqmode = 0;
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
       Prolific USB serial driver at version 1.2.1r2.  They should be
       turning on bit 0100, but are turning on 0x0100. */
#define TIOCM_CD 0x0100    

    if (func == 00) {              /* input Data Set Sense (carrier) */
      if (dc[dx].dedicated) {      /* any serial connections? */
	if (--dc[dx].dsstime == 0) {
	  dc[dx].dsstime = DSSCOUNTDOWN;
	  for (lx = 0; lx < 16; lx++) {  /* yes, poll them */
	    if (dc[dx].ctype[lx] == CT_SERIAL) {
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
      crs[A] = ~dc[dx].dss;      /* to the outside world, 1 = no carrier */
      IOSKIP;

    } else if (func == 07) {       /* input AMLC status */
      crs[A] = 040000 | (dc[dx].bufnum<<8) | (dc[dx].intenable<<5) | (dc[dx].dmqmode<<4);
      if (dc[dx].eor) {
	crs[A] |= 0100000;
	dc[dx].eor = 0;
      }
      if (dc[dx].ctinterrupt)
	if (dc[dx].ctinterrupt & 0xfffe)
	  crs[A] |= 0xcf;          /* multiple char time interrupt */
	else
	  crs[A] |= 0x8f;          /* last line cti */
      dc[dx].interrupting = 0;
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
    TRACE(T_INST, " OTA '%02o%02o\n", func, device);

    /* func 00 = Output Line # to read Data Set Status, only implemented
       on 2-board AMLC sets with full data set control (uncommon) */

    /* func 01 = Set Line Configuration.  Primos issues this on
       logged-out lines to drop DTR so it can test carrier.  It also
       drops DTR (configurable) after logout to physically disconnect
       the line, either telnet or over a modem */

    if (func == 01) {              /* set line configuration */
      lx = crs[A] >> 12;
      //printf("OTA '01%02o: AMLC line %d new config = '%o, old config = '%o\n", device, lx, crs[A] & 0xFFF, dc[dx].lconf[lx] & 0xFFF);

      switch (dc[dx].ctype[lx]) {

      case CT_SOCKET:
	if (!(crs[A] & 0x400) && dc[dx].fd[lx] >= 0) {  /* if DTR drops, disconnect */
	  AMLC_CLOSE_LINE;
	}
	break;

	
      case CT_SERIAL:
	    
	/* setup line characteristics if they have changed (check for
	   something other than DTR changing) */

	fd = dc[dx].fd[lx];
	if ((crs[A] ^ dc[dx].lconf[lx]) & ~02000) {
	  //printf("devamlc: serial config changed!\n");
	  if (tcgetattr(fd, &terminfo) == -1) {
	    fprintf(stderr, "devamlc: unable to get terminfo for device '%o line %d", device, lx);
	    break;
	  }
	  memset(&terminfo, 0, sizeof(terminfo));
	  cfmakeraw(&terminfo);
	  baud = baudtable[(crs[A] >> 6) & 7];
	  cfsetspeed(&terminfo, baud);
	  terminfo.c_cflag = CREAD | CLOCAL;		// turn on READ and ignore modem control lines
	  switch (crs[A] & 3) {              /* data bits */
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
	  if (crs[A] & 020)
	    terminfo.c_cflag |= CSTOPB;
	  if (!(crs[A] & 010)) {
	    terminfo.c_cflag |= PARENB;
	    if (!(crs[A] & 4))
	      terminfo.c_cflag |= PARODD;
	  }

#if 0
	  /* on the Prime AMLC, flow control is done in software and is
	     specified with the AMLC "lword" - an AMLDIM (software)
	     variable.  To enable Unix xon/xoff, RTS/CTS, and DTR flow
	     control, bits 5 & 7 of the lconf word have been taken over by
	     the emulator.  Bit 6 is the state of DTR.  The values for
	     bits 5-7 are:

	     0x0 - no low-level flow control, like the Prime
	     0x1 - xon/xoff flow control (2413 becomes 3413)
	     1x0 - cts/rts flow control (2413 becomes 6413)
	     1x1 - dsr/dtr flow control (2413 becomes 7413)

	     NOTE: bit 11 also appears to be free, but Primos doesn't 
	     let it flow through to the AMLC controller. :(
	  */

	  switch ((crs[A] >> 9) & 7) {
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

	  if (crs[A] & 01000) {
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
	    fprintf(stderr, "devamlc: unable to set terminal attributes for device %s\n", devname);
	    perror("devamlc error");
	  }
	}

	/* set DTR high (02000) or low if it has changed */

	if ((crs[A] ^ dc[dx].lconf[lx]) & 02000) {
	  //printf("devamlc: DTR state changed\n");
	  ioctl(fd, TIOCMGET, &modemstate);
	  if (crs[A] & 02000)
	    modemstate |= TIOCM_DTR;
	  else {
	    modemstate &= ~TIOCM_DTR;
	    dc[dx].dsstime = 1;
	  }
	  ioctl(fd, TIOCMSET, &modemstate);
	}
	break;

      default:
	fatal("devamlc: unrecognized connection type");
      }

      /* finally, update line config */

      dc[dx].lconf[lx] = crs[A];
      IOSKIP;

    } else if (func == 02) {      /* set line control */
      lx = (crs[A]>>12);
      //printf("OTA '02%02o: AMLC line %d control = %x\n", device, lx, crs[A]);
      if (crs[A] & 040)           /* character time interrupt enable/disable */
	dc[dx].ctinterrupt |= BITMASK16(lx+1);
      else
	dc[dx].ctinterrupt &= ~BITMASK16(lx+1);
      if (crs[A] & 010)           /* transmit enable/disable */
	dc[dx].xmitenabled |= BITMASK16(lx+1);
      else
	dc[dx].xmitenabled &= ~BITMASK16(lx+1);
      if (crs[A] & 01)            /* receive enable/disable */
	dc[dx].recvenabled |= BITMASK16(lx+1);
      else
	dc[dx].recvenabled &= ~BITMASK16(lx+1);
      AMLC_SET_POLL;
      IOSKIP;

    } else if (func == 03) {      /* set room in input buffer */
      lx = (crs[A]>>12);
      dc[dx].room[lx] = crs[A] & 0xFFF;
      //printf("OTA '03%02o: AMLC line %d, room=%d, A=0x%04x\n", device, lx, dc[dx].room[lx], crs[A]);
      IOSKIP;

    } else if (func == 014) {      /* set DMA/C channel (for input) */
      dc[dx].dmcchan = crs[A] & 0x7ff;
      //printf("OTA '14%02o: AMLC chan = %o\n", device, dc[dx].dmcchan);
      if (!(crs[A] & 0x800))
	fatal("Can't run AMLC in DMA mode!");
#if 0
	    dmcea = dc[dx].dmcchan;
	  dmcpair = get32io(dmcea);
	  dmcbufbegea = dmcpair>>16;
	  dmcbufendea = dmcpair & 0xffff;
	  dmcnw = dmcbufendea - dmcbufbegea + 1;
	  printf("AMLC: dmcnw=%d\n", dmcnw);
#endif
      IOSKIP;

    } else if (func == 015) {      /* set DMT/DMQ base address (for output) */
      dc[dx].baseaddr = crs[A];
      IOSKIP;

    } else if (func == 016) {      /* set interrupt vector */
      dc[dx].intvector = crs[A];
      IOSKIP;

    } else if (func == 017) {      /* set programmable clock constant */
      dc[dx].pclock = crs[A];
      /* XXX: reprogram baud rate for lines that use the programmable clock */
      IOSKIP;

    } else {
      printf("Unimplemented OTA device '%02o function '%02o, A='%o\n", device, func, crs[A]);
      fatal(NULL);
    }
    break;

  case 4:

    maxxmit = 0;

    //printf("poll device '%o, cti=%x, xmit=%x, recv=%x, dss=%x\n", device, dc[dx].ctinterrupt, dc[dx].xmitenabled, dc[dx].recvenabled, dc[dx].dss);
    
    /* check for 1 new telnet connection on each AMLC poll */

    addrlen = sizeof(addr);
    while ((fd = accept(tsfd, (struct sockaddr *)&addr, &addrlen)) == -1 && errno == EINTR)
      ;
    if (fd == -1) {
      if (errno != EWOULDBLOCK) {
	perror("accept error for AMLC");
      }
    } else {
      allbusy = 1;
      for (i=0; dc[i].deviceid && i<MAXBOARDS; i++)
	for (lx=0; lx<16; lx++) {
	  /* NOTE: don't allow connections on clock line */
	  if (lx == 15 && (i+1 == MAXBOARDS || !dc[i+1].deviceid))
	      break;
	  if (dc[i].fd[lx] < 0 && dc[i].ctype[lx] == CT_SOCKET) {
	    allbusy = 0;
	    dc[i].dss |= BITMASK16(lx+1);
	    dc[i].connected |= BITMASK16(lx+1);
	    dc[i].fd[lx] = fd;
	    dc[i].tstate[lx] = TS_DATA;
	    dc[i].room[lx] = MAXROOM;
	    dc[i].ctype[lx] = CT_SOCKET;
	    //printf("em: AMLC connection, fd=%d, device='%o, line=%d\n", fd, dc[i].deviceid, lx);
	    goto endconnect;
	  }
	}
endconnect:
      if (allbusy) {
	warn("No free AMLC connection");
	write(fd, "\rAll AMLC lines are in use!\r\n", 29);
	close(fd);
      } else {

	if ((tsflags = fcntl(fd, F_GETFL)) == -1) {
	  perror("unable to get ts flags for AMLC line");
	}
	tsflags |= O_NONBLOCK;
	if (fcntl(fd, F_SETFL, tsflags) == -1) {
	  perror("unable to set ts flags for AMLC line");
	}
	tcpoptval = 1;
	if (setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, &tcpoptval, sizeof(tcpoptval)) == -1)
	  perror("unable to set TCP_NODELAY");

	/* these Telnet commands put the connecting telnet client
	   into character-at-a-time mode and binary mode.  Since
	   these probably display garbage for other connection
	   methods, this stuff might be better off in a very thin
	   connection server */
	
	buf[0] = 255;   /* IAC */
	buf[1] = 251;   /* will */
	buf[2] = 1;     /* echo */
	buf[3] = 255;   /* IAC */
	buf[4] = 251;   /* will */
	buf[5] = 3;     /* supress go ahead */
	buf[6] = 255;   /* IAC */
	buf[7] = 253;   /* do */
	buf[8] = 0;     /* binary mode */
	write(fd, buf, 9);

	/* send out the ttymsg greeting */

	if ((msgfd = open("ttymsg", O_RDONLY, 0)) >= 0) {
	  ttymsglen = read(msgfd, ttymsg, sizeof(ttymsg)-1);
	  if (ttymsglen >= 0) {
	    ttymsg[ttymsglen] = 0;
	    write(fd, ttymsg, ttymsglen);
	  }
	  close(msgfd);
	} else if (errno != ENOENT) {
	  perror("Unable to open ttymsg file");
	}
      }
    }

    /* do a transmit scan loop for every line.  This loop is fairly
       efficient because Primos turns off xmitenable on DMQ lines
       after a short time w/o any output.

       NOTE: it's important to do xmit processing even if a line is
       not currently connected, to drain the tty output buffer.
       Otherwise, the DMQ buffer fills, this stalls the tty output
       buffer, and when the next connection occurs on this line, the
       output buffer from the previous terminal session will be
       displayed to the new user. */

    if (dc[dx].connected || dc[dx].xmitenabled) {
      for (lx = 0; lx < 16; lx++) {

	if (dc[dx].xmitenabled & BITMASK16(lx+1)) {
	  n = 0;
	  if (dc[dx].dmqmode) {
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
		utempa = MEM[qentea];
		qentea = (qentea & ~qmask) | ((qentea+1) & qmask);
		//printf("Device %o, line %d, entry=%o (%c)\n", device, lx, utempa, utempa & 0x7f);
		buf[n++] = utempa & 0x7F;
	      }
	    } else {         /* no line connected, just drain queue */
	      //printf("Draining output queue on line %d\n", lx);
	      put16io(get16io(qcbea), qcbea+1);
	    }
	  } else {  /* DMT */
	    utempa = get16io(dc[dx].baseaddr + lx);
	    if (utempa != 0) {
	      if ((utempa & 0x8000) && (dc[dx].connected & BITMASK16(lx+1))) {
		//printf("Device %o, line %d, entry=%o (%c)\n", device, lx, utempa, utempa & 0x7f);
		buf[n++] = utempa & 0x7F;
	      }
	    }

	    /* would need to setup DMT xmit poll here, and/or look for
	       char time interrupt.  In practice, DMT isn't used when
	       the AMLC device is configured as a QAMLC */
	  }

	  /* n chars have been packed into buf; see how many we can send */

	  if (n > 0) {
	    nw = write(dc[dx].fd[lx], buf, n);
	    if (nw != n) printf("devamlc: tried %d, wrote %d on line %d\n", n, nw, lx);
	    //printf("devamlc: XMIT tried %d, wrote %d\n", n, nw);
	    if (nw > 0) {

	      /* nw chars were sent; for DMQ, update the queue head
		 top to reflect nw dequeued entries.  For DMT, clear
		 the dedicated cell (only writes 1 char at a time).

		 XXX: Might be good to keep write stats here, to
		 decide how many chars to dequeue above and/or how
		 often to do writes.  There's overhead if large DMQ
		 buffers are used and Unix buffers get full so writes
		 can't complete */

	      if (dc[dx].dmqmode) {
		qtop = (qtop & ~qmask) | ((qtop+nw) & qmask);
		put16io(qtop, qcbea);
	      } else
		put16io(0, dc[dx].baseaddr + lx);
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
       round to be more fair with this limited resource.  Better yet,
       the tumble table space could be apportioned for each line with
       data waiting to be read.

       The AMLC tumble tables should never overflow, because we only
       read as many characters from the socket buffers as will fit in
       the tumble tables. However, the tty line buffers may overflow,
       causing data from the terminal to be dropped. To help avoid
       this, a new OTA "set room left" has been implemented.  If there
       is no room left in the tty input buffer, don't read any more
       characters from the socket for that line.  In case Primos has
       not been modified to use the room left feature, it is
       initialized to MAXROOM for each line, so that at most MAXROOM
       characters are read from a line during a poll.  If MAXROOM is
       small, like 64, this also makes overflow less likely if the
       line input buffer is set to '200 or so with AMLBUF.  To
       optimize transfers to the Prime over AMLC lines, MAXROOM is set
       much higher, to 1024 */

    if (!dc[dx].eor) {
      if (dc[dx].bufnum)
	dmcea = dc[dx].dmcchan ^ 2;
      else
	dmcea = dc[dx].dmcchan;
      dmcpair = get32io(dmcea);
      dmcbufbegea = dmcpair>>16;
      dmcbufendea = dmcpair & 0xffff;
      dmcnw = dmcbufendea - dmcbufbegea + 1;
      lx = dc[dx].recvlx;
      for (lcount = 0; lcount < 16 && dmcnw > 0; lcount++) {
	if ((dc[dx].connected & dc[dx].recvenabled & BITMASK16(lx+1))
	    && dc[dx].room[lx] >= 8) {

	  /* dmcnw is the # of characters left in the dmc buffer, but
	     there may be further size/space restrictions for this line */

	  n2 = dmcnw;
	  if (n2 > sizeof(buf))
	    n2 = sizeof(buf);
	  if (n2 > dc[dx].room[lx])
	    n2 = dc[dx].room[lx];
          if (n2 > MAXROOM)        /* don't let 1 line hog the resource */
	    n2 = MAXROOM;

	  while ((n = read(dc[dx].fd[lx], buf, n2)) == -1 && errno == EINTR)
	    ;
	  //printf("processing recv on device %o, line %d, b#=%d, n2=%d, n=%d\n", device, lx, dc[dx].bufnum, n2, n);

	  /* zero length read means the fd has been closed */

	  if (n == 0) {
	    n = -1;
	    errno = EPIPE;
	  }
	  if (n == -1) {
	    n = 0;
	    if (errno == EAGAIN || errno == EWOULDBLOCK)
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

	     XXX: need to respond to remote telnet server commands...

	     For direct serial connections, the line stays in TS_DATA
	     state so no telnet processing occurs. */

	  if (n > 0) {
	    //printf("devamlc: RECV dx=%d, lx=%d, b=%d, tried=%d, read=%d\n", dx, lx, dc[dx].bufnum, n2, n);
	    state = dc[dx].tstate[lx];
	    for (i=0; i<n; i++) {
	      ch = buf[i];
	      switch (state) {
	      case TS_DATA:
		if (ch == 255 && dc[dx].ctype[lx] == CT_SOCKET)
		  state = TS_IAC;
		else {
    storech:
		  utempa = lx<<12 | 0x0200 | ch;
		  put16io(utempa, dmcbufbegea);
		  //printf("******* stored character %o (%c) at %o\n", utempa, utempa&0x7f, dmcbufbegea);
		  dmcbufbegea = INCVA(dmcbufbegea, 1);
		  dmcnw--;
		}
		break;
	      case TS_IAC:
		switch (ch) {
		case 255:
		  state = TS_DATA;
		  goto storech;
		case 251:   /* will */
		case 252:   /* won't */
		case 253:   /* do */
		case 254:   /* don't */
		  state = TS_OPTION;
		  break;
		case 250:   /* begin suboption */
		  state = TS_SUBOPT;
		  break;
		default:    /* ignore other chars after IAC */
		  state = TS_DATA;
		}
		break;
	      case TS_SUBOPT:
		if (ch == 255)
		  state = TS_IAC;
		break;
	      case TS_OPTION:
	      default:
		state = TS_DATA;
	      }
	    }
	    dc[dx].tstate[lx] = state;
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
      }
    }

    /* time to interrupt?

       XXX: might be a bug here: with multiple controllers, maybe only
       the last controller (with ctinterrupt set) will get high
       performance (higher poll rates) while others will get the
       standard poll rate.  If a non-clock-line controller wants
       faster polling, we probably need to generate an interrupt on
       the clock line controller to cause AMLDIM to refill the
       buffers, or force the ctinterrupt status bit to be returned
       on the next status request.
    */

    if (dc[dx].intenable && (dc[dx].ctinterrupt || dc[dx].eor)) {
      if (gvp->intvec == -1) {
	gvp->intvec = dc[dx].intvector;
	dc[dx].interrupting = 1;
      } else
	devpoll[device] = 100;         /* come back soon! */
    }

    /* conditions to setup another poll:
       - any board with ctinterrupt set on any line is polled
       - any board with xmitenabled on any line is polled (to drain output)
       - any board with recvenabled on a connected line is polled
       - the device must not already be set for a poll

       NOTE: there is always at least one board with ctinterrupt set
       (the last board), so it will always be polling and checking
       for new incoming connections */


    /* the largest DMQ buffer size is 1023 chars.  If any line's DMQ
       buffer is getting filled completely, then we need to poll
       faster to increase throughput.  If the max queue size falls
       below 256, then decrease the interrupt rate.  Anywhere between
       256-1022, leave the poll rate alone.

       XXX NOTE: polling faster only causes AMLDIM to fill buffers
       faster for the last AMLC board (with ctinterrupt set).
    */

#if 1
    if (maxxmit >= 1023) {
      if (pollspeedup < 8) {
	pollspeedup++;
	//printf("%d ", pollspeedup);
	//fflush(stdout);
      }
    } else if (pollspeedup > 1 && maxxmit < 256) {
      pollspeedup--;
      //printf("%d ", pollspeedup);
      //fflush(stdout);
    }

#endif
    AMLC_SET_POLL;
    break;
  }
}
#endif


/* PNC (ring net) device handler

  On a real Prime ring network, each node has a unique node id from 1
  to 254.  The node id is configured with software and stored into the
  PNC during network initialization.  In practice, CONFIG_NET limits
  the node id to 247.  When a node starts, it sends a message to
  itself.  If any system acknowledges this packet, it means the node
  id is already in use.  If this occcurs, the new node will disconnect
  from the ring.  Since all systems in a ring have to be physically
  cabled together, there was usually a common network administration
  to ensure that no two nodes had the same node id.

  Node id 255 is the broadcast id - all nodes receive these messages.
  Beginning with 19.3, An "I'm alive" broadcast message is sent every
  10 seconds to let all nodes know that a machine is still up.

  For use with the emulator, the unique node id concept doesn't make a
  lot of sense.  If a couple of guys running the emulator wanted to
  have a 2-node network with nodes 1 and 2 (network A), and another
  couple of guys had a 2-node network with nodes 1 and 2 (network B),
  then it wouldn't be possible for a node in one network to add a node
  in the other network without other people redoing their node ids to
  ensure uniqueness.  To get around this, the emulator has a config
  file RING.CFG that lets you say "Here are the guys in *my* ring
  (with all unique node ids), here is each one's IP address and port,
  my node id on their ring is *blah*, their node id on their ring is
  *blah2*"".  This allows the node id to be a per-host number that
  only needs to be coordinated with two people that want to talk to
  each other, and effectively allows one emulator to be in multiple
  rings simulataneously.  Cool, eh?

  PNC ring buffers are 256 words, with later versions of Primos also
  supporting 512, and 1024 word packets.  "nbkini" (rev 18) allocates
  12 ring buffers + 1 for every 2 nodes in the ring.  Both the 1-to-2
  queue for received packets, and 2-to-1 queue for xmit packets have
  63 entries.  PNC ring buffers are wired and never cross page
  boundaries.

  The actual xmit/recv buffers for PNC are 256-1024 words, and each
  buffer has an associated "block header", stored in a different
  location in system memory, that is 8 words.

  The BH fields are (16-bit words):
       0: type field (1)
       1: free pool id (3 for ring buffers)
       2-3: pointer to data block

       4-7 are available for use by the drivers.  For PNC:
       4: number of words received (calculated by pncdim based on DMA regs)
       5: receive status word
       6: data 1
       7: data 2

  The PNC data buffer has a 2-word header, followed by the data:
       0: To (left) and From (right) bytes containing node-ids
       1: "Type" word. 
          Bit 1 set = odd number of bytes (if set, last word is only 1 byte)
          Bit 7 set = normal data messages (otherwise, a timer message)
	  Bit 16 set = broadcast timer message
  PNC data buffers never cross page boundaries.

  Primos PNC usage:

  OCP '0007
  - disconnect from ring

  OCP '0207
  - inject a token into the ring

  OCP '0507
  - set PNC into "delay" mode (token recovery BS)

  OCP '1007
  - stop any xmit in progress


  INA '1707
  - read PNC status word
  - does this in a loop until "connected" bit is clear after disconnect above
  - PNC status word:
      bit 1 set if receive interrupt (rcv complete)
      bit 2 set if xmit interrupt (xmit complete)
      bit 3 set if "PNC booster" (repeater?)
      bit 4-5 not used
      bit 6 set if connected to ring
      bit 7 set if multiple tokens detected (only after xmit EOR)
      bit 8 set if token detected (only after xmit EOR)
      bits 9-16 controller node ID

  INA '1207
  - read receive status word (byte?)
      bit 1 set for previous ACK
      bit 2 set for multiple previous ACK
      bit 3 set for previous WACK
      bit 4 set for previous NACK
      bits 5-6 unused
      bit 7 ACK byte parity error
      bit 8 ACK byte check error (parity on bits 1-6)
      bit 9 recv buffer parity error
      bit 10 recv busy
      bit 11 end of range before end of message (received msg was too big
             for the receive buffer)
      bits 12-16 unused

  INA '1307
  - read xmit status word
      bit 1 set for ACK
      bit 2 set for multiple ACK (more than 1 node accepted packet)
      bit 3 set for WACK
      bit 4 set for NACK (bad CRC)
      bit 5 unused
      bit 6 parity bit of ACK (PNC only)
      bit 7 ACK byte parity error
      bit 8 ACK byte check error (parity on bits 1-6)
      bit 9 xmit buffer parity error
      bit 10 xmit busy
      bit 11 packet did not return
      bit 12 packet returned with bits 6-8 nonzero
      bits 13-16 retry count (booster only, zero on PNC)

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
  - (Primos follows this with INA '1707 until "connected" bit is set)

  OCP '1407
  - ack receive (clears recv interrupt request)

  OCP '0407
  - ack xmit (clears xmit interrupt request)

  OCP '1507
  - set interrupt mask (enable interrupts)

  OCP '1107
  - rev 20 does this, not sure what it does

*/

int devpnc (int class, int func, int device) {

#if 0

#define PNCPOLL 100

  /* PNC controller status bits */
#define PNCRCVINT  0x8000    /* bit 1 rcv interrupt (rcv complete) */
#define PNCXMITINT 0x4000    /* bit 2 xmit interrupt (xmit complete) */
#define PNCBOOSTER 0x2000    /* bit 3 PNC booster = 1 */

#define PNCCONNECTED 0x400   /* bit 6 */
#define PNCTWOTOKENS 0x200   /* bit 7, only set after xmit EOR */
#define PNCTOKDETECT 0x100   /* bit 8, only set after xmit EOR */

  /* xmit/recv states */

#define XR_IDLE   0          /* initial state: no xmit or recv */
#define XR_READY  1          /* ready to recv or xmit */
#define XR_XFER   3          /* transferring data over socket */

#define MINCONNTIME 30       /* wait 30 seconds between connect attempts */
#define MAXPKTBYTES 2048     /* max of 2048 byte packets */

  static short configured = 0; /* true if PNC configured */
  static unsigned short pncstat;    /* controller status word */
  static unsigned short recvstat;   /* receive status word */
  static unsigned short xmitstat;   /* xmit status word */
  static unsigned short pncvec;     /* PNC interrupt vector */
  static unsigned short myid;       /* my PNC node id */
  static unsigned short enabled;    /* interrupts enabled flag */
  static int pncfd;        /* socket fd for all PNC network connections */

  /* the ni structure contains the important information for each node
     in the network and is indexed by the local node id */

  static struct {          /* node info for each node in my network */
    short cfg;             /* true if seen in ring.cfg */
    short fd;              /* socket fd for this node, -1 if unconnected */
    char  ip[16];          /* IP address of the remote node */
    short port;            /* emulator network port on the remote node */
    short myremid;         /* my node ID on the remote node's ring network */
    short yourremid;       /* remote node's id on its own network */
    time_t conntime;       /* time of last connect request */
  } ni[256];

  /* array to map socket fd's to local node id's for accessing the ni
     structure.  Not great programming, because the host OS could
     choose to use large fd's, which will cause a runtime error */

#define FDMAPSIZE 1024
  static short fdnimap[FDMAPSIZE];

  typedef struct {
    short state, offset;
    unsigned short dmachan, dmareg, dmaaddr;
    short dmanw, dmabytesleft, toid, fromid, remtoid, remfromid;
    unsigned char iobuf[MAXPKTBYTES+2];
  } t_dma;
  static t_dma recv, xmit;

  short i;
  unsigned short access, dmaword;
  unsigned short *iobufp;
  time_t timenow;
  struct hostent* server;

  struct sockaddr_in addr;
  int fd, optval, fdflags;
  unsigned int addrlen;

  FILE *ringfile;
  char *tok, buf[128], *p;
  int n, linenum;
  int tempid, tempmyremid, tempyourremid, tempport, cfgerrs;
  char tempip[22];       /* xxx.xxx.xxx.xxx:ppppp */
#define DELIM " \t\n"
#define PDELIM ":"

  //gvp->traceflags = ~T_MAP;

  if (nport <= 0) {
    if (class == -1)
      fprintf(stderr, "-nport is zero, PNC not started\n");
    else
      TRACE(T_INST|T_RIO, "nport <= 0, PIO to PNC ignored, class=%d, func='%02o, device=%02o\n", class, func, device);
    return -1;
  }

  switch (class) {

  case -1:
    for (i=0; i<FDMAPSIZE; i++)
      fdnimap[i] = -1;
    for (i=0; i<256; i++) {
      ni[i].cfg = 0;
      ni[i].fd = -1;
      strcpy(ni[i].ip, "               ");
      ni[i].port = 0;
      ni[i].myremid = 0;
      ni[i].yourremid = 0;
    }
    xmit.state = XR_IDLE;
    recv.state = XR_IDLE;
    myid = 0;                 /* set an invalid node id */

    /* read the ring.cfg config file.  Each line contains:
          localid  ip:port  [myremoteid yourremoteid]
       where:
          localid = the remote node's id (1-247) on my ring
          ip = the remote emulator's TCP/IP address (or later, name)
	  port = the remote emulator's TCP/IP PNC port
          myremoteid = my node's id (1-247) on the remote ring
	  yourremoteid = the remote node's id on its own ring

       The two remote id fields are optional, and allow the emulator
       to be in multiple rings with different administration.  If
       these are not specified, myremoteid = my local id, and
       yourremoteid = localid (the remote host's local id).

       There cannot be duplicates among the localid field, but there
       can be duplicates in the remoteid fields */

    linenum = 0;
    if ((ringfile=fopen("ring.cfg", "r")) != NULL) {
      while (fgets(buf, sizeof(buf), ringfile) != NULL) {
	linenum++;
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
	if (ni[tempid].cfg) {
	  fprintf(stderr,"Line %d of ring.cfg: node id occurs twice\n", linenum);
	  continue;
	}
	if ((p=strtok(NULL, DELIM)) == NULL) {
	  fprintf(stderr,"Line %d of ring.cfg: IP address missing\n", linenum);
	  continue;
	}
	if (strlen(p) > 21) {
	  fprintf(stderr,"Line %d of ring.cfg: IP address is too long\n", linenum);
	  continue;
	}
	strcpy(tempip, p);
	if ((p=strtok(NULL, DELIM)) != NULL) {
	  tempmyremid = atoi(p);
	  if (tempmyremid < 1 || tempmyremid > 247) {
	    fprintf(stderr,"Line %d of ring.cfg: my remote node id out of range 1-247\n", linenum);
	    continue;
	  }
	} else 
	  tempmyremid = -1;
	if ((p=strtok(NULL, DELIM)) != NULL) {
	  tempyourremid = atoi(p);
	  if (tempyourremid < 1 || tempyourremid > 247) {
	    fprintf(stderr,"Line %d of ring.cfg: your remote node id out of range 1-247\n", linenum);
	    continue;
	  }
	} else 
	  tempyourremid = tempid;
	if (tempyourremid == tempmyremid) {
	    fprintf(stderr,"Line %d of ring.cfg: my remote node id and your remote node id can't be equal\n", linenum);
	    continue;
	  }
	/* parse the port number from the IP address */
	tempport = -1;
	if ((p=strtok(tempip, PDELIM)) != NULL) {
	  strcpy(ni[tempid].ip, p);
	  if ((p=strtok(NULL, PDELIM)) != NULL) {
	    tempport = atoi(p);
	    if (tempport < 1 || tempport > 32000)
	      fprintf(stderr,"Line %d of ring.cfg: port number is out of range 1-32000\n", linenum);
	  }
	}
	if (tempport == -1) {
	  fprintf(stderr, "Line %d of ring.cfg: IP should be xxx.xxx.xxx.xxx:pppp\n", linenum);
	  continue;
	}
	ni[tempid].cfg = 1;
	ni[tempid].myremid = tempmyremid;
	ni[tempid].yourremid = tempyourremid;
	ni[tempid].port = tempport;
	TRACE(T_RIO, "Line %d: id=%d, ip=\"%s\", port=%d, myremid=%d, yourremid=%d\n", linenum, tempid, tempip, tempport, tempmyremid, tempyourremid);
	configured = 1;
      }
      if (!feof(ringfile)) {
	perror(" error reading ring.cfg");
	fatal(NULL);
      }
      fclose(ringfile);
    }
    if (!configured) {
      fprintf(stderr, "PNC not configured because ring.cfg missing or errors occurred.\n");
      return -1;
    }
    return 0;

  case 0:

    /* OCP '0700 - disconnect */

    if (func == 00) {
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - disconnect\n", func, device);
      if (pncfd >= 0) {
	close(pncfd);
	pncfd = -1;
      }
      for (i=0; i<256; i++) {
	if (ni[i].fd >= 0) {
	  fdnimap[ni[i].fd] = -1;
	  close(ni[i].fd);
	  ni[i].fd = -1;
	}
      }
      xmit.state = XR_IDLE;
      recv.state = XR_IDLE;
      pncstat &= ~PNCCONNECTED;

    /* OCP '0701 - connect 
       If any errors occur while trying to setup the listening socket,
       it seems reasonable to leave the PNC unconnected and continue,
       but this will cause Primos (rev 19) to hang in a spin loop. So
       for now, bomb.  Later, we could put the PNC in a disabled state,
       where INA/OTA don't skip, since Primos handles this better.
    */

    } else if (func == 01) {    /* connect to the ring */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - connect\n", func, device);

      /* start listening on the network port */

      pncfd = socket(AF_INET, SOCK_STREAM, 0);
      if (pncfd == -1) {
	perror("socket failed for PNC");
	fatal(NULL);
      }
      if (fcntl(pncfd, F_GETFL, fdflags) == -1) {
	perror("unable to get ts flags for PNC");
	fatal(NULL);
      }
      fdflags |= O_NONBLOCK;
      if (fcntl(pncfd, F_SETFL, fdflags) == -1) {
	perror("unable to set ts flags for PNC");
	fatal(NULL);
      }
      optval = 1;
      if (setsockopt(pncfd, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof(optval))) {
	perror("setsockopt failed for PNC");
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
      pncstat |= PNCCONNECTED;

    } else if (func == 02) {    /* inject a token */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - inject token\n", func, device);

    } else if (func == 04) {    /* ack xmit (clear xmit int) */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - ack xmit int\n", func, device);
      pncstat &= ~PNCXMITINT;   /* clear "xmit interrupting" */
      pncstat &= ~PNCTOKDETECT; /* clear "token detected" */
      xmitstat = 0;
      xmit.state = XR_IDLE;

    } else if (func == 05) {    /* set PNC into "delay" mode */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - set delay mode\n", func, device);

    } else if (func == 010) {   /* stop xmit in progress */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - stop xmit\n", func, device);
      xmitstat = 0;
      xmit.state = XR_IDLE;

    } else if (func == 011) {   /* dunno what this is - rev 20 startup */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - unknown\n", func, device);
      ;

    } else if (func == 012) {   /* set normal mode */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - set normal mode\n", func, device);
      ;

    } else if (func == 013) {   /* set diagnostic mode */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - set diag mode\n", func, device);
      ;

    } else if (func == 014) {   /* ack receive (clear rcv int) */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - ack recv int\n", func, device);
      pncstat &= ~PNCRCVINT;
      recvstat = 0;
      recv.active = 0;

    } else if (func == 015) {   /* set interrupt mask (enable int) */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - enable interrupts\n", func, device);
      enabled = 1;

    } else if (func == 016) {   /* clear interrupt mask (disable int) */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - disable interrupts\n", func, device);
      enabled = 0;

    } else if (func == 017) {   /* initialize */
      TRACE(T_INST|T_RIO, " OCP '%02o%02o - initialize\n", func, device);
      pncstat = 0;
      recvstat = 0;
      xmitstat = 0;
      pncvec = 0;
      enabled = 0;
      xmit.state = XR_IDLE;
      recv.state = XR_IDLE;

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
      TRACE(T_INST|T_RIO, " INA '%02o%02o - get recv status '%o\n", func, device, recvstat);
      crs[A] = recvstat;
      IOSKIP;

    } else if (func == 013) {   /* DIAG - read static register; not impl. */
      crs[A] = 0;
      IOSKIP;

    } else if (func == 014) {   /* read xmit status word */   
      TRACE(T_INST|T_RIO, " INA '%02o%02o - get xmit status '%o\n", func, device, xmitstat);
      crs[A] = xmitstat;
      IOSKIP;

    } else if (func == 017) {   /* read controller status word */   
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
      recvstat = 0x0040;        /* set receive busy */
      recv.dmachan = crs[A];
      recv.dmareg = recv.dmachan << 1;
      recv.dmanw = regs.sym.regdmx[recv.dmareg];
      if (recv.dmanw <= 0)
	recv.dmanw = -(recv.dmanw>>4);
      else
	recv.dmanw = -((recv.dmanw>>4) ^ 0xF000);
      recv.dmaaddr = regs.sym.regdmx[recv.dmareg+1];
      recv.iobufp = mem + mapio(recv.dmaaddr);
      recv.state = XR_READY;
      recv.offset = -1;         /* initialize for new packet */
      TRACE(T_INST|T_RIO, " recv: dmachan=%o, dmareg=%o, dmaaddr=%o, dmanw=%d\n", recv.dmachan, recv.dmareg, recv.dmaaddr, recv.dmanw);
      devpoll[device] = 10;
      IOSKIP;

    } else if (func == 015) {   /* initiate xmit, dma chan in A */
      if (xmitstat & 0x0040) {  /* already busy? */
	warn("pnc: xmit when already busy!");
	return;                 /* yes, return and don't skip */
      }
      xmitstat = 0x0040;        /* set xmit busy */
      xmit.dmachan = crs[A];
      xmit.dmareg = xmit.dmachan<<1;
      xmit.dmanw = regs.sym.regdmx[xmit.dmareg];
      if (xmit.dmanw <= 0)
	xmit.dmanw = -(xmit.dmanw>>4);
      else
	xmit.dmanw = -((xmit.dmanw>>4) ^ 0xF000);
      xmit.dmaaddr = regs.sym.regdmx[xmit.dmareg+1];
      TRACE(T_INST|T_RIO, " xmit: dmachan=%o, dmareg=%o, dmaaddr=%o, dmanw=%d\n", xmit.dmachan, xmit.dmareg, xmit.dmaaddr, xmit.dmanw);

      /* read the first word, the to and from node id's, and map them
	 to the remote hosts' expected to and from node id's */

      xmit.iobufp = mem + mapva(xmit.dmaaddr, 0, RACC, &access);
      dmaword = *xmit.iobufp++;
      xmit.toid = dmaword >> 8;
      xmit.fromid = dmaword & 0xFF;
      TRACE(T_INST|T_RIO, " xmit: toid=%d, fromid=%d\n", xmit.toid, xmit.fromid);

      /* broadcast packets are "I am up" msgs and are simply discarded
	 here, with a succesful transmit status.  Node up/down is
	 handled in the devpnc poll code.  

	XXX: should check that this really is the "I am up" msg */
      
      if (xmit.toid == 255) {
	goto xmitdone1;
      }

      /* if this xmit is to me and there is a new receive pending and
	 there is room left in the receive buffer, put the packet
	 directly in my receive buffer.  If we can't receive it now,
	 set NACK xmit and receive status.  If the packet is not to
	 me, copy it to the xmit.iobuf and add the header */

      if (xmit.toid == myid) {
	if (recv.state == XR_READY && recv.dmanw >= xmit.dmanw) {
	  memcpy(recv.iobufp, xmit.iobufp, xmit.dmanw*2);
	  regs.sym.regdmx[recv.dmareg] += xmit.dmanw;     /* bump recv count */
	  regs.sym.regdmx[recv.dmareg+1] += xmit.dmanw;   /* and address */
	  pncstat |= 0x8000;                /* set recv interrupt too */
	  recv.state = XR_IDLE;             /* no longer ready to recv */

xmitdone1:
	  regs.sym.regdmx[xmit.dmareg] += xmit.dmanw;     /* and xmit count */
	  regs.sym.regdmx[xmit.dmareg+1] += xmit.dmanw;   /* and address */
	  pncstat |= 0x4100;                  /* set xmit interrupt + token */
	  xmitstat |= 0x8000;                 /* set ACK xmit status */
	  goto xmitdone;

	} else {
	  xmitstat |= 0x1000;             /* set xmit NACK status */
	  recvstat |= 0x20;               /* set recv premature EOR */
	}
      }

      /* check for unreasonable situations */

      if (xmit.toid == 0)
	fatal("PNC: xmit.toid is zero");
      if (xmit.fromid == 0)
	fatal("PNC: xmit.fromid is zero");
      if (xmit.fromid != myid) {
	printf("PNC: xmit fromid=0x%02x != myid=0x%02x\n", xmit.fromid, myid);
	fatal(NULL);
      }

      /* map local to and from node id to remote to and from node id */

      if (ni[xmit.fromid].myremid > 0)
	xmit.remfromid = ni[xmit.fromid].myremid;
      else
	xmit.remfromid = myid;
      xmit.remtoid = ni[xmit.toid].yourremid;
      xmit.state = XR_READY;      /* xmit pending */
      xmit.offset = -1;           /* initialize for new xmit */
      devpoll[device] = 10;

xmitdone:
      if (enabled && (pncstat & 0xC000))
	if (gvp->intvec == -1)
	  gvp->intvec = pncvec;
	else
	  devpoll[device] = 100;
      IOSKIP;

    } else if (func == 016) {   /* set interrupt vector */
      pncvec = crs[A];
      TRACE(T_INST|T_RIO, " interrupt vector = '%o\n", pncvec);
      IOSKIP;

    } else if (func == 017) {   /* set my node ID */
      myid = crs[A] & 0xFF;
      pncstat = (pncstat & 0xFF00) | myid;
      TRACE(T_INST|T_RIO, " my node id is %d\n", myid);
      if (ni[myid].cfg)
	fprintf(stderr, "Warning: my node id of %d is in ring.cfg\n", myid);
      strcpy(ni[myid].ip, "127.0.0.1");
      ni[myid].port = nport;
      ni[myid].myremid = myid;
      ni[myid].yourremid = myid;
      IOSKIP;

    } else {
      printf("Unimplemented OTA device '%02o function '%02o, A='%o\n", device, func, crs[A]);
      fatal(NULL);
    }
    break;

  case 4:
    TRACE(T_INST|T_RIO, " POLL '%02o%02o\n", func, device);

    /* if a transmit is pending, start/continue it until completes */

    if (xmit.state > XR_IDLE) {
      if (ni[xmit.toid].fd == -1) {        /* not connected yet */
	if (time(&timenow) - ni[xmit.toid].conntime < MINCONNTIME) {
	  printf("em: waiting for connection timeout to node %d\n", xmit.toid);
	  goto xmiterr;
	}
	if ((ni[xmit.toid].fd = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
	  perror ("Unable to create socket");
	  exit(1);
	}
	server = gethostbyname(ni[xmit.toid].ip);
	if (server == NULL) {
	  fprintf(stderr,"pnc: cannot resolve %s\n", ni[xmit.toid].ip);
	  close(ni[xmit.toid].fd);
	  ni[xmit.toid].fd = -1;
	  goto xmiterr;
	}
	ni[xmit.toid].conntime = timenow;
	bzero((char *) &addr, sizeof(addr));
	addr.sin_family = AF_INET;
	bcopy((char *)server->h_addr, (char *)&addr.sin_addr.s_addr, server->h_length);
	addr.sin_port = htons(ni[xmit.toid].port);
	if (connect(ni[xmit.toid].fd, (void *) &addr,(socklen_t) sizeof(addr)) < 0) {
	  perror("pnc: error connecting to server\n");
	  close(ni[xmit.toid].fd);
	  ni[xmit.toid].fd = -1;
	  goto xmiterr;
	}
	xmit.state = XR_XFER;
      }

      /* start/continue the transfer */

      if (xmit.state == XR_XFER) {
	if ((n=write(ni[xmit.toid].fd, xmit.iobuf[offset], xmit.dmabytes-xmit.offset)) < 0) {
	  perror("pnc: write error");
	} else {
	  xmit.offset += n;
	  xmit.dmabytesleft -= n;
	  if (xmit.dmabytesleft == 0)
	    xmit.state = XR_IDLE;
	}
      }
	
#if 0
    while ((fd = accept(pncfd, (struct sockaddr *)&addr, &addrlen)) == -1 && errno == EINTR)
      ;
    if (fd == -1) {
      if (errno != EWOULDBLOCK) {
	perror("accept error for PNC");
      }
    } else {
      if (fd >= MAXFD)
	fatal("New connection fd is too big");
      printf("New PNC connection:\n");
      /*
- new connect request came in
- scan host table to find matching IP
- how to figure out when one IP has multiple emulators?  port?
- if already connected, display warning error and ignore

      newdevice = 0;
      for (i=0; devices[i] && !newdevice && i<MAXBOARDS; i++)
	for (lx=0; lx<16; lx++)
	  if (!(dc[devices[i]].dss & BITMASK16(lx+1))) {
	    newdevice = devices[i];
	    dc[newdevice].dss |= BITMASK16(lx+1);
	    dc[newdevice].sockfd[lx] = fd;
	    //printf("em: AMLC connection, fd=%d, device='%o, line=%d\n", fd, newdevice, lx);
	    break;
	  }
      if (!newdevice) {
	warn("No free AMLC connection");
	write(fd, "\rAll AMLC lines are in use!\r\n", 29);
	close(fd);
      }

xmitdone:
if (xmitstat == 0x0040) {             /* complete w/o errors? */
	pncstat |= 0x4100;                  /* set xmit interrupt + token */
	xmitstat |= 0x8000;                 /* yes, set ACK xmit status */
    }
#endif
    devpoll[device] = PNCPOLL*gvp->instpermsec;
    break;

  default:
    fatal("Bad func in devpcn");
#else
    return -1;
#endif
}
