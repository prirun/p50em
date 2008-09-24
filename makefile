# makefile to create various emulator builds
# Targets:  em  debug  hobby  dongle  lmserver

em:
	cc -DNOTRACE -DFAST -DNOMEM -O -c em.c -fobey-inline -mdynamic-no-pic -I../dongle/mx/ppc/api;g++ -o em em.o ../dongle/mx/ppc/api/libmxmac260.a -framework IOKit -framework CoreFoundation
	strip em
	rm em.o


debug:
	cc -DNOREGS -g -O0 -DNOTRACE -DFAST -DNOMEM -c em.c -fobey-inline -mdynamic-no-pic -I../dongle/mx/ppc/api;g++ -o em em.o ../dongle/mx/ppc/api/libmxmac260.a -framework IOKit -framework CoreFoundation
	rm em.o


hobby:
	cc -DHOBBY -DNOTRACE -DFAST -O em.c -fobey-inline -mdynamic-no-pic -o em
	strip em


dongle:
	cc -c dongle.c -I../dongle/mx/ppc/api;g++ -o dongle dongle.o ../dongle/mx/ppc/api/libmxmac260.a -framework IOKit -framework CoreFoundation
	rm dongle.o


lmserver:
	cc -c lmserver.c -I../dongle/mx/ppc/api;g++ lmserver.o -o lmserver ../dongle/mx/ppc/api/libmxmac260.a -framework IOKit -framework CoreFoundation
	rm lmserver.o
