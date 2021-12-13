# makefile to create various emulator builds

REV=${shell [ -d .hg ] && hg id -n || git rev-parse --short HEAD}

all_deps = makefile

em_objs = em.o em
em_deps = \
  em.c regs.h emdev.h ea64v.h ea32i.h fp.h dispatch.h geom.h \
  devpnc.h devamlc.h devsmlc.h swap.h

CFLAGS =
# Uncomment for building on SmartOS/Solaris:
# CFLAGS += -lsocket -lnsl

.PHONY:	emwarn debug trace fixed

# normal
em: $(em_deps) $(all_deps)
	$(CC) -DREV=\"${REV}\" ${CFLAGS} -DNOTRACE -DFAST -O -Winline -Wno-return-type em.c -o em

# lots of compiler warnings
emwarn: $(em_deps) $(all_deps)
	$(CC) -DREV=\"${REV}\" ${CFLAGS} -DNOTRACE -DFAST -O -Wall -Wextra -pedantic -Wconversion em.c -o em

# gdb
debug: $(em_deps) $(all_deps)
	$(CC) -DREV=\"${REV}\" ${CFLAGS} -DNOTRACE -DFAST -g -O0 em.c -o em

# tracing
trace: $(em_deps) $(all_deps)
	$(CC) -DREV=\"${REV}\" ${CFLAGS} -DFAST -O em.c -o em

# the fixed clock rate build is useful for making problems reproduceable.
#
# If the emulator crashes on a specific program, run it at the end of
# PRIMOS.COMI to get a more consistent instruction count for the
# failure, then enable tracing a little before that with -trace <IC - 100>

# fixed clock rate
fixed: $(em_deps) $(all_deps)
	$(CC) -DREV=\"${REV}\" ${CFLAGS} -DFIXEDCLOCK -DNOIDLE -DFAST -O em.c -o em

# Experimental "don't mask off the high bit on amlc lines" build
#
# Let's see if this is breaking e.g. ymodem file transfers.

nomaskamlc: $(em_deps) $(all_deps)
	$(CC) -DREV=\"${REV}\" -DNOMASKAMLC ${CFLAGS} -DNOTRACE -DFAST -O -Winline -Wno-return-type em.c -o em.nomaskamlc

clean:
	rm -f $(em_objs)
