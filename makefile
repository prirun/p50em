# makefile to create various emulator builds

REV=${shell hg id -n}

.PHONY:	broken brokenp em emp debug debugp trace tracep vfy vfyp fixed fixedp demo demop demol dongleprog lmserver lmserverp magrst magsav parts smad smag mtread mtwrite

em:     # Intel

	rm -rf em.o
	cc -DREV=\"${REV}\" -DNOTRACE -DFAST -DNOMEM -O em.c -o em


debug:   # gdb (Intel)

	rm -rf em.o
	cc -DREV=\"${REV}\" -DNOTRACE -DFAST -DNOMEM -g -O0 em.c -o em


trace:   # tracing + gdb (Intel)

	rm -rf em.o
	cc -DREV=\"${REV}\" -DFAST -DNOMEM -O em.c -o em


fixed:  # fixed clock rate, gdb (Intel)

	rm -rf em.o
	cc -DREV=\"${REV}\" -DFIXEDCLOCK -DNOIDLE -DNOTRACE -DFAST -DNOMEM -g -O0 em.c -o em


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
