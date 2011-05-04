#include <stdint.h>

#if !defined(__BIG_ENDIAN__) && !defined(__LITTLE_ENDIAN__)
#  error Either __BIG_ENDIAN__ or __LITTLE_ENDIAN__ must be defined.
#endif

#if defined(__LITTLE_ENDIAN__)

  #define __const_swab16(x) ((uint16_t)(			      \
    (((uint16_t)(x) & (uint16_t)0xFFU) << 8) |                  \
    (((uint16_t)(x) & (uint16_t)0xFF00U) >> 8)))

  /* These are the byte swap implementations used by the Linux kernel.  */

  #define __const_swab32(x) ((uint32_t)(                        \
    (((uint32_t)(x) & (uint32_t)0x000000ffUL) << 24) |          \
    (((uint32_t)(x) & (uint32_t)0x0000ff00UL) <<  8) |          \
    (((uint32_t)(x) & (uint32_t)0x00ff0000UL) >>  8) |          \
    (((uint32_t)(x) & (uint32_t)0xff000000UL) >> 24)))

  #define __const_swab64(x) ((uint64_t)(                        \
   (((uint64_t)(x) & (uint64_t)0x00000000000000ffULL) << 56) |  \
   (((uint64_t)(x) & (uint64_t)0x000000000000ff00ULL) << 40) |  \
   (((uint64_t)(x) & (uint64_t)0x0000000000ff0000ULL) << 24) |  \
   (((uint64_t)(x) & (uint64_t)0x00000000ff000000ULL) <<  8) |  \
   (((uint64_t)(x) & (uint64_t)0x000000ff00000000ULL) >>  8) |  \
   (((uint64_t)(x) & (uint64_t)0x0000ff0000000000ULL) >> 24) |  \
   (((uint64_t)(x) & (uint64_t)0x00ff000000000000ULL) >> 40) |  \
   (((uint64_t)(x) & (uint64_t)0xff00000000000000ULL) >> 56)))

  inline uint16_t swap16 (uint16_t in) {
    return __const_swab16 (in);
  }

  inline uint32_t swap32 (uint32_t in) {
    return __const_swab32 (in);
  }

  inline uint64_t swap64 (uint64_t in) {
    return __const_swab64 (in);
  }
#else
  #define swap16(x) (uint16_t) (x)
  #define swap32(x) (uint32_t) (x)
  #define swap64(x) (uint64_t) (x)
#endif
