# makefile to create various emulator builds
# Note: -arch ppc requires /Developer/SDKs/MacOSX10.4u.sdk

REV=${shell hg id -n}

# for Intel, registers can't be used
# for PPC, registers are faster but some features are disabled:
# - PNC can't use signals to interrupt, so it's much slower
# - SIGTERM and SIGQUIT can't be trapped and handled gracefully
# for PPC debug, don't use registers

REGS=-DNOREGS
REGS=

.PHONY:	broken brokenp em emp debug debugp trace tracep vfy vfyp fixed fixedp demo demop demol dongleprog lmserver lmserverp magrst magsav parts smad smag mtread mtwrite

em:     # production (Intel)

	rm -rf em.o
	cc -arch i686 -DKEYID=${KEYID} -DREV=\"${REV}\" -DNOREGS -DNOTRACE -DFAST -DNOMEM -O -c em.c -fobey-inline -mdynamic-no-pic -Idongle/mx/Universal/API;g++ -arch i686 -o em em.o dongle/mx/Universal/API/10_6/libmxmac260.a -framework IOKit -framework CoreFoundation
	strip em
	rm em.o


emp:    # production (PowerPC)

	rm -rf em.o
	cc -arch ppc -DKEYID=${KEYID} ${REGS} -DREV=\"${REV}\" -DNOTRACE -DFAST -DNOMEM -O -c em.c -fobey-inline -mdynamic-no-pic -Idongle/mx/Universal/API;g++ -arch ppc -o em em.o dongle/mx/Universal/API/10_6/libmxmac260.a -framework IOKit -framework CoreFoundation
	strip em
	rm em.o

debug:   # gdb (Intel)

	rm -rf em.o
	cc -arch i686 -DKEYID=${KEYID} -DREV=\"${REV}\" -DNOREGS -g -O0 -DNOTRACE -DFAST -c em.c -fobey-inline -mdynamic-no-pic -Idongle/mx/Universal/API;g++ -arch i686 -o em em.o dongle/mx/Universal/API/10_6/libmxmac260.a -framework IOKit -framework CoreFoundation
	rm em.o


debugp:  # gdb (PowerPC)

	rm -rf em.o
	cc -arch ppc -DKEYID=${KEYID} -mmacosx-version-min=10.4 -DREV=\"${REV}\" -DNOREGS -g -O0 -DNOTRACE -DFAST -c em.c -fobey-inline -mdynamic-no-pic -Idongle/mx/Universal/API/10_6;g++ -arch ppc -o em em.o dongle/mx/Universal/API/10_6/libmxmac260.a -framework IOKit -framework CoreFoundation
	rm em.o


trace:   # tracing + gdb (Intel)

	rm -rf em.o
	cc -arch i686 -DKEYID=${KEYID} -DREV=\"${REV}\" -DNOREGS -g -O0 -DFAST -c em.c -fobey-inline -mdynamic-no-pic -Idongle/mx/Universal/API;g++ -arch i686 -o em em.o dongle/mx/Universal/API/10_6/libmxmac260.a -framework IOKit -framework CoreFoundation
	rm -rf em.o


tracep: # tracing + gdb (PowerPC)

	rm -rf em.o
	cc -arch ppc -DKEYID=${KEYID} ${REGS} -DREV=\"${REV}\" -DFAST -O -c em.c -fobey-inline -mdynamic-no-pic -Idongle/mx/Universal/API;g++ -arch ppc -o em em.o dongle/mx/Universal/API/10_6/libmxmac260.a -framework IOKit -framework CoreFoundation
	rm -rf em.o


vfy:   # prod + tracing to verify em changes (Intel)

	rm -rf em.o
	cc -arch i686 -DREV=\"\" -O -DNOREGS -DFAST -c em.c -fobey-inline -mdynamic-no-pic -Idongle/mx/Universal/API;g++ -arch i686 -o em em.o dongle/mx/Universal/API/10_6/libmxmac260.a -framework IOKit -framework CoreFoundation
	rm -rf em.o


vfyp: # prod + tracing to verify em changes (PowerPC)

	rm -rf em.o
	cc -arch ppc -mmacosx-version-min=10.4 -DREV=\"\" -O -DFAST -c em.c -fobey-inline -mdynamic-no-pic -Idongle/mx/Universal/API/10_6;g++ -arch ppc -o em em.o dongle/mx/Universal/API/10_6/libmxmac260.a -framework IOKit -framework CoreFoundation
	rm -rf em.o


fixed:  # fixed clock rate, gdb (Intel)

	rm -rf em.o
	cc -arch i686 -DREV=\"${REV}\" -DFIXEDCLOCK -DNOIDLE -DNOREGS -g -O0 -DFAST -c em.c -fobey-inline -mdynamic-no-pic -Idongle/mx/Universal/API;g++ -arch i686 -o em em.o dongle/mx/Universal/API/10_6/libmxmac260.a -framework IOKit -framework CoreFoundation
	rm em.o


fixedp: # fixed clock rate, gdb (PowerPC)

	rm -rf em.o
	cc -arch ppc -mmacosx-version-min=10.4 -DREV=\"${REV}\" -DFIXEDCLOCK -DNOIDLE ${REGS} -g -O0 -DFAST -c em.c -fobey-inline -mdynamic-no-pic -Idongle/mx/Universal/API/10_6;g++ -arch ppc -o em em.o dongle/mx/Universal/API/10_6/libmxmac260.a -framework IOKit -framework CoreFoundation
	rm em.o


demo:  # demo (limited: 1-2 amlc, 2 disk drive up to 160MB) (Intel)

	rm -rf em.o
	cc -DREV=\"${REV}\" -DNOREGS -DDEMO -DNOTRACE -DFAST -D__LITTLE_ENDIAN__ -O em.c -o em
	strip em
	rm -rf em.o


demop: # demo (PowerPC)

	rm -rf em.o
	cc ${REGS} -mmacosx-version-min=10.4 -fno-stack-protector -arch ppc -DREV=\"${REV}\" -DDEMO -DNOTRACE -DFAST -O em.c -fobey-inline -mdynamic-no-pic -o em
	strip em
	rm -rf em.o


demol:  # demo (limited: 1-2 amlc, 1 disk drive up to 160MB, one PNC node) (Intel, LLVM)

	rm -rf em.o
	llvm-gcc -DREV=\"${REV}\" -DNOREGS -DDEMO -DNOTRACE -DFAST -O em.c -fobey-inline -mdynamic-no-pic -o em
	strip em
	rm -rf em.o


dongleprog: # utility to program a dongle

	cc -arch ppc -DREV=\"${REV}\" -c dongleprog.c -Idongle/mx/Universal/API/10_6;g++ -arch ppc -o dongleprog dongleprog.o dongle/mx/Universal/API/10_6/libmxmac260.a -framework IOKit -framework CoreFoundation
	rm dongleprog.o


lmserver: # license server

	rm -rf lmserver.o
	cc -arch i686 -DREV=\"${REV}\" lmserver.c -o lmserver
	rm -rf lmserver.o

lmserverp: # license server

	rm -rf lmserver.o
	cc -arch ppc -DREV=\"${REV}\" -c lmserver.c -Idongle/mx/Universal/API/10_6;g++ -arch ppc lmserver.o -o lmserver dongle/mx/Universal/API/10_6/libmxmac260.a -framework IOKit -framework CoreFoundation
	rm lmserver.o

mtread: # Dump a tape to a .tap disk file (Linux only)

	rm -rf mtread.o
	cc -o mtread mtread.c


mtwrite: # write a physical tape from a .tap file (Linux only)

	rm -rf mtwrite.o
	cc -o mtwrite mtwrite.c


magrst: # Unix version of Prime's magrst

	rm -rf magrst.o
	cc -arch ppc -DREV=\"${REV}\" -o magrst magrst.c istext.c


magsav: # Unix version of Prime's magsav

	rm -rf magsav.o
	cc -arch ppc -DREV=\"${REV}\" -o magsav magsav.c istext.c


parts: # Unix utility to determine parttions in a drive file

	rm -rf parts.o
	cc -arch ppc -DREV=\"${REV}\" -o parts parts.c


smad: # Unix utility to decode Prime pdev

	rm -rf smad.o
	cc -arch ppc -DREV=\"${REV}\" -o smad smad.c

smag: # Unix create Prime pdev

	rm -rf smag.o
	cc -arch ppc -DREV=\"${REV}\" -o smag smag.c

broken: # production (Intel)

	rm -rf broken.o
	cc -arch i686 -O -c broken.c -fobey-inline -mdynamic-no-pic -Idongle/mx/Universal/API;g++ -arch i686 -o broken broken.o dongle/mx/Universal/API/10_6/libmxmac260.a -framework IOKit -framework CoreFoundation

brokenp: # production (PPC)

	rm -rf broken.o
	cc -arch ppc -O -c broken.c -fobey-inline -mdynamic-no-pic -Idongle/mx/Universal/API;g++ -arch ppc -o brokenp broken.o dongle/mx/Universal/API/10_6/libmxmac260.a -framework IOKit -framework CoreFoundation
