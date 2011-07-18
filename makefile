# makefile to create various emulator builds
# Targets:  em  debug  trace  hobby  dongle  lmserver  magrst
# Note: -arch ppc requires /Developer/SDKs/MacOSX10.4u.sdk

em:
	cc -arch ppc -DNOTRACE -DFAST -DNOMEM -O -c em.c -fobey-inline -mdynamic-no-pic -I../dongle/mx/ppc/api;g++ -arch ppc -o em em.o ../dongle/mx/ppc/api/libmxmac260.a -framework IOKit -framework CoreFoundation
	strip em
	rm em.o


debug:
	cc -arch ppc -DNOREGS -g -O0 -DNOTRACE -DFAST -DNOMEM -c em.c -fobey-inline -mdynamic-no-pic -I../dongle/mx/ppc/api;g++ -arch ppc -o em em.o ../dongle/mx/ppc/api/libmxmac260.a -framework IOKit -framework CoreFoundation
	rm em.o


trace:
	cc -arch ppc -DNOREGS -g -O0 -DFAST -DNOMEM -c em.c -fobey-inline -mdynamic-no-pic -I../dongle/mx/ppc/api;g++ -arch ppc -o em em.o ../dongle/mx/ppc/api/libmxmac260.a -framework IOKit -framework CoreFoundation
	rm em.o


fixed:
	cc -arch ppc -DFIXEDCLOCK -DNOIDLE -DNOREGS -g -O0 -DFAST -DNOMEM -c em.c -fobey-inline -mdynamic-no-pic -I../dongle/mx/ppc/api;g++ -arch ppc -o em em.o ../dongle/mx/ppc/api/libmxmac260.a -framework IOKit -framework CoreFoundation
	rm em.o


hobby:
	cc -arch ppc -DHOBBY -DNOTRACE -DFAST -O em.c -fobey-inline -mdynamic-no-pic -o em
	strip em


dongle:
	cc -arch ppc -c dongle.c -I../dongle/mx/ppc/api;g++ -arch ppc -o dongle dongle.o ../dongle/mx/ppc/api/libmxmac260.a -framework IOKit -framework CoreFoundation
	rm dongle.o


lmserver:
	cc -arch ppc -c lmserver.c -I../dongle/mx/ppc/api;g++ -arch ppc lmserver.o -o lmserver ../dongle/mx/ppc/api/libmxmac260.a -framework IOKit -framework CoreFoundation
	rm lmserver.o

magrst:
	cc -arch ppc -o magrst magrst.c istext.c
