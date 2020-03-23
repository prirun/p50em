# Prime 50-Series Emulator

## What Is This?

This is a software emulator for a minicomputer architecture sold
by Prime Computer from the early 70s through about 1993.  Prime's
initial business plan was to make systems compatible with the
Honeywell x16 family, which had then-recently been discontinued.
Prime extended the architecture heavily.

## Emulator History

Beginning in 2005, Jim Wilcoxson developed an emulator for Prime
Computer's 50-Series architecture.  The emulator originally ran on
the PowerPC architecture.  In late 2011, Jim ported it so it would
run on x86.  This entailed solving endianness issues (The 50-Series
is big-endian), as well as re-optimizing the code for performance
without the host processor having a large set of general-purpose
registers.

## Emulator Documentation

Coming soon, we swear!

## Public Systems

There are a set of emulators available for public use.  These may
be accessed via `telnet` to the appropriate port on `em.prirun.com`.

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
at [sysovl.info](https://sysovl.info/reference_prime.html).  A howto
on installing PRIMOS in the emulator is [here](https://sysovl.info/reference_prime_drb_installing_primos.html).
Discussion of adapting these instructions to 22.1.4 has been occurring on the [cctalk mailing list](http://classiccmp.org/pipermail/cctalk/2020-March/052126.html).

## Getting PRIMOS

Two versions of PRIMOS are available from Bitsavers:

* [Rev 22.1.4](http://bitsavers.org/bits/Prime/primos_22.1.4.zip) [has issues]
* [Rev 22.1.4 repacked](https://yagi.h-net.org/m2214repack.tar.gz) [use this]
* [Rev 19.?](http://bitsavers.org/bits/Prime/pps/03_log.tape_I=boot_II=iptpal.tap.gz)

The Rev. 19 tape is a save from an installed system.

## Sample System Images

A set of sample system images derived from the public emulators can
be downloaded to get you started.  These tarballs preserve 
sparse allocation ("holes"), so that uninitialized space in the disk 
images does not occupy actual space.  You may need to tell `tar` to 
preserve this sparse allocation when you extract, e.g. with the `-S` 
option.

The current
[tarball](https://sysovl.info/pages/blobs/emulator/p50em_samplemachines_v4.tar)
is 143510751 bytes, and its sha256sum is 
f7bf880cb1c7f32903ad85c4f52386cf2179067b8bfcf2c10b51c2e4dcf074dd.
(gzip compression would only reduce this by ~400 kilobytes.)

V4 corrects ACL problems in the Rev19 and Rev24 images.

V3 removed additional junk, and rebuildt the disk images as 600 MB 
drives, split 30/10 heads filesystem/paging.  It also included enhancements 
to the wrapper scripts: directory independence and the ability to run the 
`runem` script from a terminal.
