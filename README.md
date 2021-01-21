# Prime 50-Series Emulator

## What Is This?

This is a software emulator for a minicomputer architecture sold by
Prime Computer from the early 70s through about 1993.  Prime's initial
business plan was to make systems compatible with the Honeywell x16
family, which had then-recently been discontinued.  Prime extended
the architecture heavily.

## Emulator History

Beginning in 2005, Jim Wilcoxson developed an emulator for Prime
Computer's 50-Series architecture.  The emulator originally ran on
the PowerPC architecture.  In late 2011, Jim ported it so it would
run on x86.  This entailed solving endianness issues (The 50-Series is
big-endian), as well as re-optimizing the code for performance without
the host processor having a large set of general-purpose registers.

## Emulator Documentation

Coming soon, we swear!  There is a unix man page included in this
repository.

Github discussions are now enabled for this repository, and can be
used to ask for help.

## Public Systems

There are a set of emulators available for public use.  These may be
accessed via `telnet` to the appropriate port on `em.prirun.com`.

| PRIMOS Revision | Port |
|-----------------|------|
| 18.3.1          | 8007 |
| 19.2            | 8001 |
| 20.2.8          | 8002 |
| 21.0.6          | 8003 |
| 22.1.4          | 8004 |
| 23.4.Y2K.R1     | 8005 |
| 24.0.0.r15      | 8006 |

For example, `telnet em.prirun.com 8001`.

## Prime History

Some information about the Prime company is available in the FAQ
which was assembled by denizens of the comp.sys.prime usenet group
and Info-Prime mailing list around the time Prime's 50-Series
business unit ceased to exist.  A reformatted copy is available
[here](https://sysovl.info/reference_prime_faq.html).

## Prime Documentation

A growing collection of Prime and related documentation is available
at [sysovl.info](https://sysovl.info/reference_prime.html).
A howto on installing PRIMOS in the emulator is
[here](https://sysovl.info/reference_prime_drb_installing_primos.html).
Discussion of adapting these instructions to
22.1.4 has been occurring on the [cctalk mailing
list](http://classiccmp.org/pipermail/cctalk/2020-March/052126.html).

## Getting PRIMOS

Distribution tape sets for four versions of PRIMOS,
with small sets of layered products, are available from
[sysovl.info](https://sysovl.info/downloads_prime_primedist.html):

* Rev 21.0.6, including BASIC, FTN, BRMS, INFORMATION
* Rev 23.2.0, including BASIC, FTN, MIDASPLUS, DRB, DTB, FS_RECOVER, PL1_LIBRARY
* Rev 23.4.Y2K,R1, including BASIC, FTN, MIDASPLUS, DRB, DTB, FS_RECOVER, PL1_LIBRARY
* Rev 24.0.0.r15, including BASIC, FTN, MIDASPLUS, DRB, DTB, FS_RECOVER, PL1_LIBRARY

Tapes for two versions of PRIMOS (these are not clean distribution sets)
are available from Bitsavers:

* [Rev 22.1.4](http://bitsavers.org/bits/Prime/primos_22.1.4.zip) [has issues, written with newer magsav, can't restore itself]
* [Rev 22.1.4 repacked](https://yagi.h-net.org/m2214repack.tar.gz) [use this instead, resaved with Rev. 22 magsav]
* [Rev 19.?](http://bitsavers.org/bits/Prime/pps/03_log.tape_I=boot_II=iptpal.tap.gz) [a backup from an installed system]

## Pre-compiled binaries

We'd prefer that you build your own from the source tree, but if that's
not possible, a set of pre-compiled binaries is available.  Included are:

* Linux i386
* Linux amd64
* Linux armhf (RasPi / BeagleBone)
* FreeBSD amd64
* Solaris amd64

[download](https://sysovl.info/pages/blobs/emulator/embinaries.20200504.tar.gz)

## Sample System Images

A set of sample system images derived from the public emulators can
be downloaded to get you started.  These tarballs preserve sparse
allocation ("holes"), so that uninitialized space in the disk images
does not occupy actual space.  You may need to tell `tar` to preserve
this sparse allocation when you extract, e.g. with the `-S` option.

The current
[tarball](https://sysovl.info/pages/blobs/emulator/p50em_samplemachines_v5.tar)
is 150,029,183 bytes, and its sha256sum is
f5b8008d7c53171f50ad95dd5cc537ba48ca419049518f8ea28deee009c6541a.
(gzip compression would only reduce this by ~400 kilobytes.)

V5 adds the source code for PRIMOS 19.2, and the diagnostics programs,
to that image.

V4 corrected ACL problems in the Rev19 and Rev24 images.

V3 removed additional junk, and rebuilt the disk images as 600
MB drives, split 30/10 heads filesystem/paging.  It also included
enhancements to the wrapper scripts: directory independence and the
ability to run the `runem` script from a terminal.
