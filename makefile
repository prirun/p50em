# makefile to create various emulator builds

REV=${shell [ -d .hg ] && hg id -n || git rev-parse --short HEAD}

em_objs = em.o em
em_deps = \
  em.c regs.h emdev.h ea64v.h ea32i.h fp.h dispatch.h geom.h \
  devpnc.h devamlc.h swap.h

.PHONY:	emwarn debug trace fixed

# normal
em: $(em_deps)
	$(CC) -DREV=\"${REV}\" -DNOTRACE -DFAST -O -Winline em.c -o em

# lots of compiler warnings
emwarn: $(em_deps)
	$(CC) -DREV=\"${REV}\" -DNOTRACE -DFAST -O -Wall -Wextra -pedantic -Wconversion em.c -o em

# gdb
debug: $(em_deps)
	$(CC) -DREV=\"${REV}\" -DNOTRACE -DFAST -g -O0 em.c -o em

# tracing
trace: $(em_deps)
	$(CC) -DREV=\"${REV}\" -DFAST -O em.c -o em

# the fixed clock rate build is useful for making problems reproduceable.
#
# If the emulator crashes on a specific program, run it at the end of 
# PRIMOS.COMI to get a more consistent instruction count for the
# failure, then enable tracing a little before that with -trace <IC - 100>

# fixed clock rate
fixed: $(em_deps)
	$(CC) -DREV=\"${REV}\" -DFIXEDCLOCK -DNOIDLE -DFAST -O em.c -o em

clean:
	rm -f $(em_objs)
