# makefile to create various emulator builds
# Note: -arch ppc requires /Developer/SDKs/MacOSX10.4u.sdk

REV=${shell hg id -n}

.PHONY:	em emp debug debugp trace tracep vfy vfyp fixed fixedp hobby hobbyp dongle lmserver magrst parts smad smag mtread mtwrite

em:     # production (Intel)

	cc -arch i686 -DREV=\"${REV}\" -DNOREGS -DNOTRACE -DFAST -DNOMEM -O -c em.c -fobey-inline -mdynamic-no-pic -I../dongle/mx/Universal/api;g++ -arch i686 -o em em.o ../dongle/mx/Universal/api/libmxmac260.a -framework IOKit -framework CoreFoundation
	strip em
	rm em.o


emp:    # production (PowerPC)

	cc -arch ppc -DREV=\"${REV}\" -DNOTRACE -DFAST -DNOMEM -O -c em.c -fobey-inline -mdynamic-no-pic -I../dongle/mx/ppc/api;g++ -arch ppc -o em em.o ../dongle/mx/ppc/api/libmxmac260.a -framework IOKit -framework CoreFoundation
	strip em
	rm em.o


debug:   # gdb (Intel)

	cc -arch i686 -DREV=\"${REV}\" -DNOREGS -g -O0 -DNOTRACE -DFAST -c em.c -fobey-inline -mdynamic-no-pic -I../dongle/mx/Universal/api;g++ -arch i686 -o em em.o ../dongle/mx/Universal/api/libmxmac260.a -framework IOKit -framework CoreFoundation
	rm em.o


debugp:  # gdb (PowerPC)

	cc -arch ppc -DREV=\"${REV}\" -DNOREGS -g -O0 -DNOTRACE -DFAST -c em.c -fobey-inline -mdynamic-no-pic -I../dongle/mx/ppc/api;g++ -arch ppc -o em em.o ../dongle/mx/ppc/api/libmxmac260.a -framework IOKit -framework CoreFoundation
	rm em.o


trace:   # tracing + gdb (Intel)

	cc -arch i686 -DREV=\"${REV}\" -DNOREGS -g -O0 -DFAST -c em.c -fobey-inline -mdynamic-no-pic -I../dongle/mx/Universal/api;g++ -arch i686 -o em em.o ../dongle/mx/Universal/api/libmxmac260.a -framework IOKit -framework CoreFoundation


tracep: # tracing + gdb (PowerPC)

	cc -arch ppc -DREV=\"${REV}\" -DNOREGS -g -O0 -DNOFAST -c em.c -fobey-inline -mdynamic-no-pic -I../dongle/mx/ppc/api;g++ -arch ppc -o em em.o ../dongle/mx/ppc/api/libmxmac260.a -framework IOKit -framework CoreFoundation


vfy:   # prod + tracing to verify em changes (Intel)

	cc -arch i686 -DREV=\"\" -O -DNOREGS -DFAST -c em.c -fobey-inline -mdynamic-no-pic -I../dongle/mx/Universal/api;g++ -arch i686 -o em em.o ../dongle/mx/Universal/api/libmxmac260.a -framework IOKit -framework CoreFoundation


vfyp: # prod + tracing to verify em changes (PowerPC)

	cc -arch ppc -DREV=\"\" -O -DFAST -c em.c -fobey-inline -mdynamic-no-pic -I../dongle/mx/ppc/api;g++ -arch ppc -o em em.o ../dongle/mx/ppc/api/libmxmac260.a -framework IOKit -framework CoreFoundation


fixed:  # fixed clock rate, gdb (Intel)

	cc -arch i686 -DREV=\"${REV}\" -DFIXEDCLOCK -DNOIDLE -DNOREGS -g -O0 -DFAST -c em.c -fobey-inline -mdynamic-no-pic -I../dongle/mx/Universal/api;g++ -arch i686 -o em em.o ../dongle/mx/Universal/api/libmxmac260.a -framework IOKit -framework CoreFoundation
	rm em.o


fixedp: # fixed clock rate, gdb (PowerPC)

	cc -arch ppc -DREV=\"${REV}\" -DFIXEDCLOCK -DNOIDLE -DNOREGS -g -O0 -DFAST -c em.c -fobey-inline -mdynamic-no-pic -I../dongle/mx/ppc/api;g++ -arch ppc -o em em.o ../dongle/mx/ppc/api/libmxmac260.a -framework IOKit -framework CoreFoundation
	rm em.o


hobby:  # hobby (limited: no amlc, 1 disk drive up to 160MB, no PNC) (Intel)

	cc -arch i686 -DREV=\"${REV}\" -DNOREGS -DHOBBY -DNOTRACE -DFAST -O em.c -fobey-inline -mdynamic-no-pic -o em
	strip em


hobbyp: # hobby (limited: no amlc, 1 disk drive up to 160MB, no PNC) (PowerPC)

	cc -arch ppc -DREV=\"${REV}\" -DHOBBY -DNOTRACE -DFAST -O em.c -fobey-inline -mdynamic-no-pic -o em
	strip em


dongle: # utility to program a dongle

	cc -arch ppc -DREV=\"${REV}\" -c dongle.c -I../dongle/mx/ppc/api;g++ -arch ppc -o dongle dongle.o ../dongle/mx/ppc/api/libmxmac260.a -framework IOKit -framework CoreFoundation
	rm dongle.o


lmserver: # license server

	cc -arch ppc -DREV=\"${REV}\" -c lmserver.c -I../dongle/mx/ppc/api;g++ -arch ppc lmserver.o -o lmserver ../dongle/mx/ppc/api/libmxmac260.a -framework IOKit -framework CoreFoundation
	rm lmserver.o

mtread: # Dump a tape to a .tap disk file (Linux only)

	cc -o mtread mtread.c


mtwrite: # write a physical tape from a .tap file (Linux only)

	cc -o mtwrite mtwrite.c


magrst: # Unix version of Prime's magrst

	cc -arch ppc -DREV=\"${REV}\" -o magrst magrst.c istext.c


parts: # Unix utility to determine parttions in a drive file

	cc -arch ppc -DREV=\"${REV}\" -o parts parts.c


smad: # Unix utility to decode Prime pdev

	cc -arch ppc -DREV=\"${REV}\" -o smad smad.c

smag: # Unix create Prime pdev

	cc -arch ppc -DREV=\"${REV}\" -o smag smag.c

