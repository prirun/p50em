#include <stdint.h>

#define __LITTLE_ENDIAN__ 1

#if !defined(__BIG_ENDIAN__) && !defined(__LITTLE_ENDIAN__)
#  error Either __BIG_ENDIAN__ or __LITTLE_ENDIAN__ must be defined.
#endif

#if defined(__BIG_ENDIAN__) && defined(__LITTLE_ENDIAN__)
#  error __BIG_ENDIAN__ and __LITTLE_ENDIAN__ are both defined.
#endif

#if defined(__LITTLE_ENDIAN__)
  inline uint16_t swap16 (uint16_t in) {
    return (in<<8) | (in>>8);
  }
  #define swap32(x) (uint32_t) (__builtin_bswap32(x))
  #define swap64(x) (uint64_t) (__builtin_bswap64(x))
#else
  #define swap16(x) (uint16_t) (x)
  #define swap32(x) (uint32_t) (x)
  #define swap64(x) (uint64_t) (x)
#endif
