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
*  '04 = SOC/Option A/VCP board (system console/user terminal)
   '05 = #2 MPC/URC (line printer/card reader/card punch)
   '06 = card punch? (RTOS User Guide, A-1) / IPC (Engr Handbook p.101)
   '06 = Interproc. Channel (IPC) (R20 Hacker's Guide)
*  '07 = #1 PNC
   '10 = ICS2 #1 or ICS1
   '11 = ICS2 #2 or ICS1
   '12 = floppy disk/diskette (magtape controller #3 at rev 22)
   '13 = #2 magtape controller
*  '14 = #1 magtape controller (emulator only supports this one)
*  '15 = #5 AMLC or ICS1
*  '16 = #6 AMLC or ICS1
*  '17 = #7 AMLC or ICS1
*  '20 = control panel / real-time clock
   '21 = 1st 4002 (Option B') disk controller
*  '22 = disk #3 (0-7)
*  '23 = disk #4
*  '24 = disk #0 (0-7; was Writable Control Store)
*  '25 = disk #2 (0-7; was 4000 disk controller)
*  '26 = disk #1 (0-7)
*  '27 = #7 disk (0-7)
   '30-32 = BPIOC #1-3 (RTOS User Guide, A-1)
*  '32 = AMLC #8 or ICS1
   '33 = #1 Versatec
   '34 = #2 Versatec
*  '35 = #4 AMLC or ICS1
   '36-37 = ELFBUS #1 & 2 (ICS1 #1, ICS1 #2)
   '40 = A/D converter type 6000
   '41 = digital input type 6020
   '42 = digital input #2
   '43 = digital output type 6040
   '44 = digital output #2
*  '45 = disk #4 (0-7; was D/A converter type 6060 (analog output) - obsolete)
*  '46 = disk #6 (0-7)
   '47 = #2 PNC
   '50 = #1 HSSMLC/MDLC (synchronous comm)
   '51 = #2 HSSMLC/MDLC
*  '52 = #3 AMLC or ICS1
*  '53 = #2 AMLC 
*  '54 = #1 AMLC
   '55 = MACI autocall unit
   '56 = old SMLC (RTOS User Guide, A-1 & Hacker's Guide)
   '60-67 = reserved for user devices (GPIB)
   '70-'73 = Megatek graphics terminals
   '75-'76 = MPC4 / T$GPPI programmable controller

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


/* this macro is used when I/O is successful.  In VI modes, it sets
   the EQ condition code bit.  In SR modes, it does a skip */

#define IOSKIP \
  if (getcrs16(KEYS) & 010000)			\
    putcrs16(KEYS, getcrs16(KEYS) | 0100);	\
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
  (!(getcrs16(MODALS) & 010) && (iget16(RP) == 03776 || (iget16(RP) == 0141603 && iget16(RP+1) == RPL-2)))
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
      printf("Unimplemented OTA device '%02o function '%02o, A='%o\n", device, func, getcrs16(A));
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

  static int initialized = 0;
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

    /* NOTE: on OSX 10.4, these are not restored by the fg command
       after the emulator is suspended (VSUSP) then restarted; so
       suspend has been disabled altogether */

    terminfo.c_iflag &= ~(INLCR | ICRNL | IXOFF | IXON);
    terminfo.c_lflag &= ~(ECHOCTL | ICANON);
    terminfo.c_oflag &= ~(TOSTOP);
    terminfo.c_cc[VINTR] = _POSIX_VDISABLE;  /* disable ^C interrupt; use ^\ */
#ifdef __APPLE__
    terminfo.c_cc[VDSUSP] = _POSIX_VDISABLE; /* disable ^Y dsuspend */
#endif
    terminfo.c_cc[VLNEXT] = _POSIX_VDISABLE; /* disable ^V lnext */
    terminfo.c_cc[VDISCARD] = _POSIX_VDISABLE; /* disable ^O discard */
#if 0
    terminfo.c_cc[VSUSP] = '';  /* change ^Z to ^] */
#else
  terminfo.c_cc[VSUSP] = _POSIX_VDISABLE;  /* disable suspend */
#endif
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
	if (getcrs16(MODALS) & 010)/* PX enabled? */
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
      if (getcrs16(MODALS) & 010)  /* PX enabled? */
	timeout.tv_sec = 0;        /* yes, can't delay */
      else {
	timeout.tv_sec = 1;        /* single user: okay to delay */
#ifndef NOTRACE
	fflush(gv.tracefile);    /* flush for DIAG testing */
#endif
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
#ifndef NOTRACE
	fflush(gv.tracefile);         /* flush trace buffer when testing */
#endif
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
	if (!(getcrs16(MODALS) & 010) && (ch == '')) {
	  printf("\nRebooting at instruction #%u\n", gv.instcount);
	  // gv.savetraceflags = ~T_MAP;  /****/
	  longjmp(bootjmp, 1);
	}
#ifndef NOTRACE
	if (ch == '') {
	  gv.tracetriggered = !gv.tracetriggered;
	  if (gv.tracetriggered) {
	    TRACEA("\nTRACE ENABLED:\n\n");
	  } else {
	    TRACEA("\nTRACE DISABLED:\n\n");
	  }
	  fflush(gv.tracefile);
	  goto readasr;
	}
#endif
	
	/* if doing local echo, do xon/xoff processing here.  If Unix
	   does it, the emulator might block on a console write.  If
	   the console is running full-duplex (no local echo), we can
	   pass xon & xoff onto Primos */

	if (terminfo.c_lflag & ECHO) {
	  if (ch == '') {
	    xoff = 1;
	    goto readasr;
	  } else if (ch == '') {
	    xoff = 0;
	    goto readasr;
	  }
	  xoff = 0;                /* enable output if any characters typed */
	  if (ch != 015)           /* log all except CR */
	    fputc(ch, conslog);
	}
	if (func >= 010)
	  putcrs16(A, 0);
	putcrs16(A, getcrs16(A) | ch);
	TRACE(T_INST, " character read=%o: %c\n", getcrs16(A), getcrs16(A) & 0x7f);
	fflush(conslog);         /* immediately flush when typing */
#ifndef NOTRACE
	fflush(gv.tracefile);
#endif
	IOSKIP;
      } else {
	printf("Unexpected error reading from tty, n=%d\n", n);
	fatal(NULL);
      }
    } else if (04 <= func && func <= 07) {  /* read control register 1/2 */
      putcrs16(A, 0);
      IOSKIP;
    } else if (func == 011) {    /* read device id? */
      putcrs16(A, 4);
      IOSKIP;
    } else if (func == 012) {    /* read control word */
      putcrs16(A, 04110);
      IOSKIP;
    } else if (func == 017) {    /* read xmit interrupt vector -OR- clock */
      putcrs16(A, vcptime[vcptimeix++]);
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
      ch = getcrs16(A) & 0x7f;
      TRACE(T_INST, " char to write=%o: %c\n", getcrs16(A), ch);
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
	devpoll[device] = gv.instpermsec*100;
      IOSKIP;
    } else if (func == 1) {       /* write control word */
      TRACEA("OTA 4, func %d, A='%o/%d\n", func, getcrs16(A), getcrs16s(A));
      IOSKIP;
    } else if (04 <= func && func <= 07) {  /* write control register 1/2 */
      TRACEA("OTA 4, func %d, A='%o/%d\n", func, getcrs16(A), getcrs16s(A));
      IOSKIP;
    } else if (func == 012) {
      TRACEA("OTA 4, func %d, A='%o/%d\n", func, getcrs16(A), getcrs16s(A));
      /* NOTE: does this in rev 23 when system is shutdown with '4110 in A */
      IOSKIP;

    } else if (func == 013) {
      TRACEA("OTA 4, func %d, A='%o/%d\n", func, getcrs16(A), getcrs16s(A));
      /* NOTE: does this in rev 20 on settime command (set clock on VCP?) */
      IOSKIP;

    } else if (func == 017) {
      if (getcrs16(A) == 0) {

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
	TRACEA("OTA 4, func '%o, A='%o/%d\n", func, getcrs16(A), getcrs16s(A));
      }
      IOSKIP;
    } else {
      printf("Unimplemented OTA device '%02o function '%02o, A='%o\n", device, func, getcrs16(A));
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
	devpoll[device] = gv.instpermsec*100;
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
    1 0x8000 = Vertical Parity Error
    2 0x4000 = Runaway
    3 0x2000 = CRC Error
    4 0x1000 = LRCC Error
    5 0x0800 = False Gap or Insufficient DMX Range
    6 0x0400 = Uncorrectable Read Error
    7 0x0200 = Raw Error
    8 0x0100 = Illegal Command (XXX: File Mark too?)
    9 0x0080 = Selected Tape Ready (online and not rewinding)
   10 0x0040 = Selected Tape Online
   11 0x0020 = Selected Tape is at End Of Tape (EOT)
   12 0x0010 = Selected Tape is Rewinding
   13 0x0008 = Selected Tape is at Beginning Of Tape (BOT)
   14 0x0004 = Selected Tape is Write Protected
   15 0x0002 = DMX Overrun
   16 0x0001 = Rewind Interrupt

   OTA '01 Motion control A-register bits:
    1 0x8000 = Select Tape Only
    2 0x4000 = 0 for File operation, 1 for Record operation
    3 0x2000 = 1 for Space operation
    4 0x1000 = 1 for Read & Correct
    5  0x800 = 1 for 7-track
    6  0x400 = 1 for 9-track
    7  0x200 = unused
    8  0x100 = 1 for 2 chars/word, 0 for 1 char/word (not supported here)
    9-11 = motion:
        0x80 = Forward
        0x40 = Reverse
        0x20 = Rewind
    12  0x10 = 1 for Write
    13   0x8 = Unit 0
    14   0x4 = Unit 1
    15   0x2 = Unit 2
    16   0x1 = Unit 3

    If the tape device file is empty, then BOT and EOT occur together.
    But, this doesn't happen with real magtapes, and some Prime software
    doesn't like it (Rev 23 Magsav+).  So instead of returning EOT when
    positioned to BOT, we return a read error, like a real tape would.

*/

int mtread (int fd, unsigned short *iobuf, int nw, int cmd, int *mtstat) {
  unsigned char buf[4];
  int n,reclen,reclen2,bytestoread;

  TRACE(T_TIO, " mtread, fpos=%u, nw=%d, cmd=0x%04x, status is 0x%04x\n", (unsigned int) lseek(fd, 0, SEEK_CUR), nw, cmd, *mtstat);
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
      fprintf(stderr," TAP format error at position %u\n", (unsigned int) lseek(fd, 0, SEEK_CUR));
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
#if 1
      *mtstat |= 0xB600;           /* set all error bits */;
      return 0;
#else
      *(int *)iobuf = 0;           /* return a 2-word zero record on errors */
      return 2;
#endif
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

    /* if we were at EOT, clear EOT; next read will get EOT again */

    if (*mtstat & 0x20) {
      *mtstat &= ~0x20;
      return 0;
    }

    /* now we're not at EOT */

    *mtstat &= ~0x20;

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
  unsigned int dmxaddr;
  unsigned int dmcpair;
  short dmxnw, dmxtotnw;
  int i,n;
  char reclen[4];

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
	putcrs16(A, 0);
      } else {
	TRACE(T_INST|T_TIO, "  INA 0 returns '%06o 0x%04x\n", datareg, datareg);
	putcrs16(A, datareg);
      }
      ready = 0;
      IOSKIP;

    } else if (func == 011) {
      putcrs16(A, (1 << 8) | 0214);   /* backplane slot + device ID */
      IOSKIP;

    } else {
      printf("Unimplemented INA device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 3:
    TRACE(T_INST|T_TIO, " OTA '%02o%02o, A='%06o %04x\n", func, device, getcrs16(A), getcrs16(A));

    if (func != 0)
      busy = 0;

    if (func == 00) {
      datareg = getcrs16(A);
      IOSKIP;

    } else if (func == 01) {

      /* here's the hard part where everything happens... decode unit first */

      u = getcrs16(A) & 0xF;
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

      if ((getcrs16(A) & 0xFFF00) == 0x8000) {
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

      if ((getcrs16(A) & 0x00E0) == 0x0020) {       /* rewind */
	//gv.traceflags = ~T_MAP;
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

      if ((getcrs16(A) & 0x4010) == 0x0010) {
	TRACE(T_TIO, " write file mark\n");
	*(int *)iobuf = 0;
	mtwrite(unit[u].fd, iobuf, 2, &unit[u].mtstat);
	ftruncate(unit[u].fd, lseek(unit[u].fd, 0, SEEK_CUR));
	IOSKIP;
	break;
      }

      /* space forward or backward a record or file at a time */

      if (getcrs16(A) & 0x2000) {           /* space operation */
	if ((getcrs16(A) & 0xC0) == 0)
	  warn("Motion = 0 for tape spacing operation");
	else if (getcrs16(A) & 0x4000) {    /* record operation */
	  TRACE(T_TIO, " space record, dir=%x\n", getcrs16(A) & 0x80);
	  mtread(unit[u].fd, iobuf, 0, getcrs16(A), &unit[u].mtstat);
	} else {                       /* file spacing operation */
	  TRACE(T_TIO, " space file, dir=%x\n", getcrs16(A) & 0x80);
	  do {
	    mtread(unit[u].fd, iobuf, 0, getcrs16(A), &unit[u].mtstat);
	  } while (!(unit[u].mtstat & 0x128));  /* FM, EOT, BOT */
	}
	IOSKIP;
	break;
      }

      /* read/write backward aren't supported */

      if (((getcrs16(A) & 0x00E0) == 0x0040) && ((getcrs16(A) & 0x2000) == 0)) {
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

      if (getcrs16(A) & 0x10) {         /* write record */
	TRACE(T_TIO, " write record\n");
	dmxtotnw = 0;
	iobufp = iobuf+2;
      } else {
	TRACE(T_TIO, " read record\n");
	dmxtotnw = mtread(unit[u].fd, iobuf, MAXTAPEWORDS, getcrs16(A), &unit[u].mtstat);
	iobufp = iobuf;
      }

      /* data transfer from iobuf (read) or to iobuf (write) */

      while (1) {
	dmxreg = dmxchan & 0x7FF;
	if (dmxchan & 0x0800) {         /* DMC */
	  dmcpair = get32io(dmxreg);    /* fetch begin/end pair */
	  dmxaddr = dmcpair>>16;
	  dmxnw = (dmcpair & 0xffff) - dmxaddr + 1;
	  TRACE(T_INST|T_TIO,  " DMC channels: ['%o]='%o, ['%o]='%o, nwords=%d", dmxreg, dmxaddr, dmxreg+1, (unsigned int) (dmcpair & 0xffff), dmxnw);
	} else {                        /* DMA */
	  dmxreg = dmxreg << 1;
	  dmxnw = getar16(REGDMX16+dmxreg);
	  dmxnw = -((dmxnw>>4) | 0xF000);
	  dmxaddr = ((getar16(REGDMX16+dmxreg) & 3)<<16) | getar16(REGDMX16+dmxreg+1);
	  TRACE(T_INST|T_TIO, " DMA channels: ['%o]='%o, ['%o]='%o/%o, nwords=%d", dmxreg, swap16(regs.sym.regdmx[dmxreg]), dmxreg+1, dmxaddr>>16, dmxaddr&0xffff, dmxnw);
	}
	if (dmxnw < 0) {            /* but is legal for >32K DMC transfer... */
	  printf("devmt: requested negative DMX of size %d\n", dmxnw);
	  fatal(NULL);
	}
	if (getcrs16(A) & 0x10) {            /* write record */
	  if (dmxtotnw+dmxnw > MAXTAPEWORDS)
	    fatal("Tape write is too big");
	  for (i=0; i < dmxnw; i++) {
#if 0
	    unsigned short ioword;
	    ioword = get16io(dmxaddr+i);
	    if (i%10 == 0)
	      TRACE(T_TIO, "\n %04d: ", i);
	    TRACE(T_TIO, " %03o %03o", (unsigned)ioword>>8, ioword&0xff);
#endif
	    *iobufp++ = MEM[mapio(dmxaddr+i)];  /* Prime->Prime: no swap */
	  }
	  TRACE(T_TIO, "\n");
	  dmxtotnw = dmxtotnw + dmxnw;
	} else {
	  if (dmxnw > dmxtotnw)
	    dmxnw = dmxtotnw;
	  for (i=0; i < dmxnw; i++) {
#if 0
	    unsigned short ioword;
	    ioword = *iobufp++;
	    if (i%10 == 0)
	      TRACE(T_TIO, "\n %04d: ", i);
	    TRACE(T_TIO, " %03o %03o", (unsigned)ioword>>8, ioword&0xff);
#endif
	    MEM[mapio(dmxaddr+i)] = *iobufp++;  /* Prime->Prime: no swap */
	  }
	  TRACE(T_TIO, "\n");
	  dmxtotnw = dmxtotnw - dmxnw;
	}
	TRACE(T_TIO, " transferred %d words\n", dmxnw);
	if (dmxchan & 0x0800) {                    /* if DMC... */
	  put16io(dmxaddr+dmxnw, dmxreg);          /* update starting address */
	} else {                                   /* if DMA... */
	  putar16(REGDMX16+dmxreg, getar16(REGDMX16+dmxreg) + (dmxnw<<4));   /* increment # words */
	  putar16(REGDMX16+dmxreg+1, getar16(REGDMX16+dmxreg+1) + dmxnw);    /* increment address */
	}

	/* if chaining, bump channel number and decrement # channels */

	if (dmxchan & 0xF000)
	  dmxchan = dmxchan + 2 - (1<<12);
	else
	  break;
      }

      /* for write record, do the write */

      if (getcrs16(A) & 0x10) {             /* write record */
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
      if (getcrs16(A) & 0x8000) {      /* status word 1 */
	datareg = unit[usel].mtstat;
	
	/* if the tape was rewinding, return rewinding status once, then
	   change it to BOT */

	if (datareg & 0x10)
	  unit[usel].mtstat = unit[usel].mtstat & ~0x10 | 0x8;
      } else if (getcrs16(A) & 0x4000)
	datareg = 0214;           /* device ID */
      else if (getcrs16(A) & 0x2000)
	datareg = dmxchan;
      else if (getcrs16(A) & 0x1000)
	datareg = mtvec;
      else if (getcrs16(A) & 0x800)
	datareg = 0;              /* status word 2 */
      else {
	TRACE(T_TIO, "  Bad OTA '02 to tape drive, A='%06o, 0x$04x\n", getcrs16(A), getcrs16(A));
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
      dmxchan = getcrs16(A);
      TRACE(T_TIO,  " dmx channel '%o, 0x%04x\n", dmxchan, dmxchan);
      IOSKIP;

    } else if (func == 015) {               /* start u-code test */
      TRACE(T_TIO,  " u-code test\n");
      IOSKIP;

    } else if (func == 016) {               /* set interrupt vector */
      mtvec = getcrs16(A);
      TRACE(T_TIO,  " set int vec '%o\n", mtvec);
      IOSKIP;

    } else {
      printf("Unimplemented OTA device '%02o function '%02o, A='%o\n", device, func, getcrs16(A));
    }
    break;

  case 4:
    TRACE(T_TIO,  " POLL device '%02o, enabled=%d, interrupting=%d\n", device, enabled, interrupting);
    if (enabled && (interrupting == 1)) {
      devpoll[device] = 100;         /* assume interrupt will be deferred */
      if (gv.intvec == -1 && (getcrs16(MODALS) & 0100000)) {
	TRACE(T_TIO,  " CPU interrupt to vector '%o\n", mtvec);
	gv.intvec = mtvec;
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

void initclock(ea_t datnowea) {
  short olddatnow;
  int datnow;
  time_t unixtime;
  struct tm *tms;

  /* for older processors (before 9950), don't set DATNOW initially.
     This has to be done with the SE(TTIME) command on the console
     after a coldstart to release users waiting on CLDSEM in older
     versions of Primos */

  if (cpuid < 15) {
    olddatnow = get32r0(datnowea);
    if (olddatnow == 0)
      return;
  }
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
  static unsigned int ticks = -1;
  static struct timeval start_tv;
  static ea_t datnowea = 0;
  static struct timeval prev_tv;
  static unsigned int previnstcount=0; /* value of instcount corresponding to above */

  struct timeval tv;
  unsigned int elapsedms,targetticks;
  int i;

#define SETCLKPOLL devpoll[device] = gv.instpermsec*(-clkpic*clkrate)/1000;

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
      //gv.traceflags = ~T_MAP;
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
      putcrs16(A, 020);            /* this is the Option-A board */
      putcrs16(A, 0120);           /* this is the SOC board */
      //gv.traceflags = ~T_MAP;
    } else if (func == 016) {      /* read switches that are up */
      putcrs16(A, sswitch);
    } else if (func == 017) {      /* read switches pushed down */
      putcrs16(A, dswitch);
    } else {
      printf("Unimplemented INA device '%02o function '%02o\n", device, func);
      fatal(NULL);
    }
    break;

  case 3:
    TRACE(T_INST, " OTA '%02o%02o\n", func, device);

    /* standard Primos always sets a PIC interval of 947 and uses the
       hardware clock rate of 3.2 usec.  This causes a software interrupt
       every 947*3.2 usec, or 3.0304 milliseconds, and this works out
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
      clkpic = getcrs16s(A);
      if (clkpic == -947)
	if (cpuid == 11 || cpuid == 20)
	  clkpic = -625;         /* P2250: 500 ticks/second */
	else if (cpuid >= 15)    /* newer machines: 250 ticks/second */
	  clkpic = -1250;
      TRACE(T_INST, "Clock PIC %d requested, set to %d\n", getcrs16s(A), clkpic);
      ticks = -1;
      SETCLKPOLL;

    } else if (func == 07) {
      if (getcrs16(A) & 020)
	clkrate = 102.4;
      else
	clkrate = 3.2;
      TRACE(T_INST, "Clock control register set to '%o\n", getcrs16(A));
      ticks = -1;
      SETCLKPOLL;

    } else if (func == 013) {     /* set interrupt vector */
      clkvec = getcrs16(A);
      TRACE(T_INST, "Clock interrupt vector address = '%o\n", clkvec);

    } else if (func == 017) {     /* write lights */
      lights = getcrs16(A);

    } else {
      printf("Unimplemented OTA device '%02o function '%02o, A='%o\n", device, func, getcrs16(A));
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
      if (gv.intvec == -1) {
	gv.intvec = clkvec;
	SETCLKPOLL;
	ticks++;
	if (gettimeofday(&tv, NULL) != 0)
	  fatal("em: gettimeofday 3 failed");
	if (ticks == 0) {
	  start_tv = tv;
	  prev_tv = tv;
	  previnstcount = gv.instcount;
	  if (datnowea != 0)
	    initclock(datnowea);
	} 
	elapsedms = (tv.tv_sec-start_tv.tv_sec)*1000.0 + (tv.tv_usec-start_tv.tv_usec)/1000.0;
	targetticks = elapsedms/(-clkpic*clkrate/1000);
#if 0
	if (abs(ticks-targetticks) > 5)
	  printf("\nClock: target=%d, ticks=%d, offset=%d, ipms=%d, poll=%d\n", targetticks, ticks, ticks-targetticks, gv.instpermsec, devpoll[device]);
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

	/* update instpermsec every IPMTIME milliseconds.  Check for
	   instcount overflow and reset when it occurs.  Also, suspend
	   can cause instpermsec to be zero.  That causes hangs in the
	   BDX idle code, so don't update instpermsec in that case.

	   XXX: this code should probably be done whether or not the
	   clock is running */

#define IPMTIME 1000

	if ((gv.instcount < previnstcount) || (gv.instcount-previnstcount > gv.instpermsec*IPMTIME)) {
	  if (gv.instcount-previnstcount > gv.instpermsec*IPMTIME) {
	    i = (gv.instcount-previnstcount) /
	      ((tv.tv_sec-prev_tv.tv_sec)*1000.0 + (tv.tv_usec-prev_tv.tv_usec)/1000.0);
	    if (i > 0) {
	      //printf("ic= %u, prev= %u, diff= %u, ipmsec= %d, prev= %d, diff= %d\n", gv.instcount, previnstcount, gv.instcount-previnstcount, i, gv.instpermsec, i-gv.instpermsec);
	      gv.instpermsec = i;
	    }
#ifdef NOIDLE
	    //printf("\ninstpermsec=%d\n", gv.instpermsec);
#endif
	  }
	  previnstcount = gv.instcount;
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
    fprintf(stderr,"globdisk: %d matches for %s\n", (int) g.gl_pathc, devfile);
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
  short head, track, rec, recsize;
  unsigned short dmareg;
  unsigned int dmaaddr;
  unsigned char *hashp;
  int lockkey;

  unsigned short iobuf[1040];             /* local I/O buf (for mapped I/O) */
  unsigned short *iobufp;
  short dmanw;
  char ordertext[8];
  int phyra;
  int nb;                   /* number of bytes returned from read/write */
  char devfile[16];

  /* map device id to device context index

     NOTE: the dc table is indexed the way a typical Prime installation
     was setup, and is not related to the pdev bits!  See notes above. */


  switch (device) {
  case 026: dx = 0; break;
  case 027: dx = 1; break;
  case 022: dx = 2; break;
  case 023: dx = 3; break;
  case 024: dx = 4; break;
  case 025: dx = 5; break;
  case 045: dx = 6; break;
  case 046: dx = 7; break;
  default:
    fprintf(stderr, "devdisk: non-disk device id '%o ignored\n", device);
    return -1;
  }


  //gv.traceflags |= T_DIO;

  switch (class) {

  case -1:
#ifdef DISKSAFE
    printf("em: Running in DISKSAFE mode; no changes will be permanent\n");
#endif
    dc[dx].device = device;
    dc[dx].state = S_HALT;
    dc[dx].status = 0100000;
    dc[dx].usel = -1;
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
    //gv.traceflags = ~T_MAP;

    /* INA's are only accepted when the controller is not busy */

    if (dc[dx].state != S_HALT)
      return;

    if (func == 01)          /* read device id, clear A first */
      putcrs16(A, CID4005 + device);
    else if (func == 011)    /* read device id, don't clear A */
      putcrs16(A, getcrs16(A) | (CID4005 + device));
    else if (func == 017) {  /* read OAR */
      putcrs16(A, dc[dx].oar);
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
      dc[dx].oar = getcrs16(A);
      devpoll[device] = 1;
    } else {
      printf("Unimplemented OTA device '%02o function '%02o, A='%o\n", device, func, getcrs16(A));
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
	dc[dx].status &= ~076007;             /* clear bits 2-6, 14-16 */
	m2 = get16io(dc[dx].oar++);
	recsize = m & 017;
	track = m1 & 03777;
	rec = m2 >> 8;   /* # records for format, rec # for R/W */
	head = m2 & 077;
	u = dc[dx].usel;
	if (order == 2)
	  strncpy(ordertext,"Format", 7);
	else if (order == 5)
	  strncpy(ordertext,"Read", 7);
	else if (order == 6)
	  strncpy(ordertext,"Write", 7);
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
	    dmanw = getar16(REGDMX16+dmareg);
	    dmanw = -(dmanw>>4);
	    if (dmanw > 1040) {
	      warn("disk I/O limited to 1040 words");
	      dmanw = 1040;
	    }
	    if (dmanw < 0) {
	      warn("disk I/O size < 0; set to 0");
	      dmanw = 0;
	    }
	    dmaaddr = ((getar16(REGDMX16+dmareg) & 3)<<16) | getar16(REGDMX16+dmareg+1);
	    TRACE(T_INST|T_DIO,  " DMA channels: nch-1=%d, ['%o]='%o, ['%o]='%o, nwords=%d\n", dc[dx].dmanch, dc[dx].dmachan, swap16(regs.sym.regdmx[dmareg]), dc[dx].dmachan+1, dmaaddr, dmanw);
	    
	    if (order == 5) {    /* read */
	      if (getcrs16(MODALS) & 020)  /* mapped read */
		if ((dmaaddr & 01777) || dmanw > 1024)
		  iobufp = iobuf;
		else
		  iobufp = MEM+mapio(dmaaddr);
	      else                         /* physical read */
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
		  MEM[mapio(dmaaddr+i)] = iobuf[i];  /* Prime->Prime: no swap */
	    } else {         /* disk write */
	      if (getcrs16(MODALS) & 020) {  /* mapped write */
		iobufp = iobuf;
		for (i=0; i<dmanw; i++)
		  iobuf[i] = MEM[mapio(dmaaddr+i)];  /* Prime->Prime: no swap */
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
	    putar16(REGDMX16+dmareg+1, getar16(REGDMX16+dmareg+1) + dmanw);
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
	else if (u == 2) u = 1;
	else if (u == 4) u = 2;
	else if (u == 8) u = 3;
	else if (u == 16) u = 4;
	else if (u == 32) u = 5;
	else if (u == 64) u = 6;
	else if (u == 128) u = 7;
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

	if (getcrs16(MODALS) & 010)             /* PX enabled? */
	  break;                           /* yes, no stall */
	devpoll[device] = gv.instpermsec/5;   /* 200 microseconds, sb 210 */
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
	if (gv.intvec >= 0 || !(getcrs16(MODALS) & 0100000))
	  dc[dx].oar -= 2;     /* can't take interrupt right now */
	else {
	  gv.intvec = m1;
	  dc[dx].state = S_INT;
	}
	//gv.traceflags = ~T_MAP;
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

#include "devpnc.h"

#include "devamlc.h"
