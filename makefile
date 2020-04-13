# makefile to create various emulator builds

REV=${shell [ -d .hg ] && hg id -n || git rev-parse --short HEAD}

.PHONY:	emwarn debug trace fixed

em:	# normal
	$(CC) -DREV=\"${REV}\" -DNOTRACE -DFAST -O -Winline em.c -o em

emwarn: # lots of compiler warnings
	$(CC) -DREV=\"${REV}\" -DNOTRACE -DFAST -O -Wall -Wextra -pedantic -Wconversion em.c -o em

debug:   # gdb
	$(CC) -DREV=\"${REV}\" -DNOTRACE -DFAST -g -O0 em.c -o em

trace:   # tracing
	$(CC) -DREV=\"${REV}\" -DFAST -O em.c -o em

# the fixed clock rate build is useful for making problems reproduceable.
#
# If the emulator crashes on a specific program, run it at the end of 
# PRIMOS.COMI to get a more consistent instruction count for the
# failure, then enable tracing a little before that with -trace <IC - 100>

fixed:  # fixed clock rate
	$(CC) -DREV=\"${REV}\" -DFIXEDCLOCK -DNOIDLE -DFAST -O em.c -o em

clean:
	rm -f em.o em
