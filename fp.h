/* fp.h, Jim Wilcoxson, June 2007
   Floating point conversion and helper routines for the Prime emulator.

   Prime DPFP format:
   - 48 mantissa bits stored in 2's complement format
   - 16 exponent bits stored in 2's complement with a bias of 128
   - exponent follows the mantissa in both memory and registers
   - some early Primes store the exponent in the 3rd halfword
   - the leading "1" bit in the mantissa is only suppressed for negative
     powers of 2
   - Prime treats zero mantissa as 0.0, even if exponent is non-zero
   - all FP operations are carried out in double precision

   IEEE DPFP format:
   - 1 sign bit
   - 11 exponent bits with a bias of 1023
   - 52 mantissa bits (sign-magnitude format)
   - leading bit of mantissa has a suppressed "1" (except subnormal)
   - if exponent is zero:
      - and frac = 0 => positive or negative zero, depending on sign
      - and frac non-zero => subnormal (aka denormal, unnormalized)
      - subnormals have an implied leading "0" bit (XXX: true??)
*/


/* getdp unpacks a Prime DPFP into 48-bit sign + mantissa (left
   justified in 64 bits) and a 32-bit signed exponent */

inline getdp (void *p, long long *frac64, int *exp32) {

  *frac64 = *(long long *)p & 0xFFFFFFFFFFFF0000LL;  /* unpack fraction */
  *exp32 = *((short *)p+3);                          /* unpack SIGNED exponent */
}

inline putdp (void *p, long long frac64, int exp32) {

  *(long long *)p = (frac64 & 0xFFFFFFFFFFFF0000LL) | (exp32 & 0xFFFF);
}


/* Conversion from Prime DPFP to IEEE DPFP
   Returns true if conversion succeeded, false if it failed.
   Conversions may fail because Prime exponents are 16 bits whereas
   IEEE DPFP exponents are only 11 bits.
*/

int prieee8(void *dp, double *d) {
  long long frac64, sign;
  int exp32;

  /* unpack Prime DPFP */

  getdp (dp, &frac64, &exp32);

  /* if negative, change to sign-magnitude */

  sign = 0;
  if (frac64 < 0) {

    /* special case: negative power of 2 */

    if (frac64 == 0x8000000000000000LL) {
      exp32 += (1023-128);
      if (exp32 < 0 || exp32 > 0x3ff)
	return 0;
      frac64 |= ((long long)exp32 << 52);
      *d = *(double *)&frac64;
      return 1;
    } else {
      sign = 0x8000000000000000LL;
      frac64 = -frac64;
    }

  /* special case: zero */

  } else if (frac64 == 0) {
    *d = 0.0;
    return 1;
  } 

  /* normalize positive fraction until bit 2 is 1 */

  while ((*(int *)&frac64 & 0x40000000) == 0) {
    frac64 = frac64 << 1;
    exp32--;
  }

  /* adjust exponent bias and check range */

  exp32 += (1023-128) - 1;
  if (exp32 < 0 || exp32 > 0x7ff)
    return 0;
  
  /* pack into an IEEE DPFP, losing the leading 1 bit in the process */

  frac64 = sign | ((long long)exp32 << 52) | ((frac64 >> 10) & 0xfffffffffffffLL);
  *d = *(double *)&frac64;
  return 1;
}

/* Conversion from Prime SPFP to IEEE DPFP */

double prieee4(unsigned int sp) {
  int frac32, sign;
  long long frac64;
  int exp32;

  /* unpack Prime SPFP */

  frac32 = sp & 0xFFFFFF00;
  exp32 = sp & 0xFF;

  /* if negative, change to sign-magnitude */

  sign = 0;
  if (frac32 < 0) {

    /* special case: negative power of 2 */

    if (frac32 == 0x80000000) {
      exp32--;
      frac64 = 0x8000000000000000LL | ((long long)exp32 << 52);
      return *(double *)&frac64;
    } else {
      sign = 0x80000000;
      frac32 = -frac32;
    }

  /* special case: zero */

  } else if (frac32 == 0)
    return 0.0;

  /* normalize positive fraction until bit 2 is 1 */

  while ((frac32 & 0x40000000) == 0) {
    frac32 = frac32 << 1;
    exp32--;
  }

  /* adjust exponent bias and check range */

  exp32 += (1023-128) - 1;
  
  /* pack into an IEEE DPFP, losing the leading 1 bit in the process */

  frac64 = (long long)sign | ((long long)exp32 << 52) | (((long long)frac32 << 22) & 0xfffffffffffffLL);
  return *(double *)&frac64;
}


/* conversion from IEEE back to Prime.  Prime exponents are larger, so
   this conversion cannot overflow/underflow, but precision is lost */

double ieeepr8(double d) {
  long long frac64;
  int exp32, neg;

  /* unpack IEEE DPFP */

  *(double *)&frac64 = d;
  neg = frac64 < 0;
  exp32 = (frac64 >> 52) & 0x7ff;
  frac64 &= 0xfffffffffffffLL;
  //printf("dp=%llx, neg=%d, frac64=%llx, exp32=%d, \n", *(long long *)dp, neg, frac64, exp32);

  /* special case: NaN & +-infinity (these shouldn't happen!) */

  if (exp32 == 0x7ff) {
    if (frac64 == 0)
      if (neg)
	printf("em: +infinity in ieeepr8\n");
      else
	printf("em: -infinity in ieeepr8\n");
    else
      printf("em: NaN in ieeepr8\n");
    return 0.0;
  }

  /* add back the hidden "1" bit except for the special cases +-0.0
     and subnormal */

  if (exp32 != 0)           /* typical IEEE normalized */
    frac64 |= 0x10000000000000LL;
  else if (frac64 == 0)     /* IEEE +-0.0 (zero exp+frac) */
    return 0.0;             /* IEEE and Prime zero are the same */
  else
      ;                     /* subnormal: no hidden 1 bit */

  /* adjust exponent, change sign-magnitude to 2's complement,
     and shift fraction into Prime placement (high 48 bits) */

  exp32 -= (1023-128) - 1;
  if (neg)
    frac64 = -frac64;
  frac64 <<= 10;

  /* normalize Prime DPFP */

  while ((frac64 ^ (frac64 << 1)) >= 0) {
    frac64 = frac64 << 1;
    exp32--;
  }

#if 0
  if (exp32 > 32767 || exp32 < -32768) {
    printf("em: exponent = %d in ieeepr8\n", exp32);
    return 0.0;
  }
#endif

  /* round the fraction to 48 bits, ensuring no overflow */

  if ((frac64 & 0x8000) && ((frac64 & 0x7fffffffffff0000LL) !=  0x7fffffffffff0000LL))
    /* XXX: should this be a subtract for negative numbers? */
    frac64 += 0x10000;

  frac64 = (frac64 & 0xffffffffffff0000LL) | (exp32 & 0xffff);
  return *(double *)&frac64;
}


/* 32-bit signed integer to Prime DPFP conversion */

double fltl (int int32) {
  long long frac64;
  int exp32, sign32;

  /* have to special case zero, or we end up with
     a "dirty zero" (zero fraction, exponent of 128) */

  if (int32 == 0)
    return 0.0;

  exp32 = 128+31;
  sign32 = int32 & 0x80000000;
  if ((int32 & 0xFFFF8000) == sign32>>16) {
    int32 = int32<<16;
    exp32 -= 16;
  }
  if ((int32 & 0xFF800000) == sign32>>8) {
    int32 = int32<<8;
    exp32 -= 8;
  }
  if ((int32 & 0xF8000000) == sign32>>4) {
    int32 = int32<<4;
    exp32 -= 4;
  }
  if ((int32 & 0xE0000000) == sign32>>2) {
    int32 = int32<<2;
    exp32 -= 2;
  }
  if ((int32 & 0xC0000000) == sign32>>1) {
    int32 = int32<<1;
    exp32 -= 1;
  }
  frac64 = ((long long)int32 << 32) | exp32;
  return *(double *)&frac64;
}


/* Prime DPFP complement */

dfcm (void *dp) {
  long long frac64;
  int exp32;

  CLEARC;
  getdp(dp, &frac64, &exp32);
  if (frac64 != 0) {                          /* can't normalize zero */
    if (frac64 == 0x8000000000000000LL) {     /* overflow case? */
      frac64 = 0x4000000000000000LL;          /* complement power of 2 */
      exp32 += 1;
    } else {
      frac64 = -frac64;                       /* complement fraction */
      while ((frac64 ^ (frac64 << 1)) >= 0) {
	frac64 = frac64 << 1;                 /* normalize */
	exp32--;
      }
    }
    if (exp32 > 32767 || exp32 < -32768)
      mathexception('f', FC_DFP_OFLOW, 0);
    else
      putdp(dp, frac64, exp32);
  }
}


/* double precision floating point normalize

   Passed a pointer to a Prime double precision variable and updates
   it in place.  May set the C-bit or cause a floating point
   exception. */

void norm(void *dp) {
  long long frac64;
  int exp32;

  getdp(dp, &frac64, &exp32);
  while ((frac64 ^ (frac64 << 1)) >= 0) {
    frac64 = frac64 << 1;
    exp32--;
  }

  putdp(dp, frac64, exp32);
  if (exp32 > 32767 || exp32 < -32768)
    mathexception('f', FC_DFP_OFLOW, 0);
}


/* double->single floating point round (FRN) instruction.

   Passed a pointer to a Prime double precision variable, one of the
   FACC's, and updates it in place.  May also take an arithmetic fault
   if overflow occurs.  For faults, the ea is zero because FACC's
   don't have an effective address. */

void frn(void *dp) {
  long long frac64;
  int exp32;
  int origsign, newsign;

  CLEARC;
  getdp(dp, &frac64, &exp32);
  if (frac64 == 0)
    *(long long *)dp = 0;
  else {
    origsign = (frac64 < 0);
    if ((frac64 & 0x18000000000LL)
	| ((frac64 & 0x8000000000LL) && (frac64 & 0x7FFFFF0000LL))) {
      frac64 +=      0x10000000000LL;
      frac64 &= 0xFFFFFF0000000000LL;
      norm(&frac64);
      *(long long *)dp = frac64;
      newsign = (frac64 < 0);
      if (newsign != origsign)
	/* XXX: is this fault code right? */
	mathexception('f', FC_DFP_OFLOW, 0);
    }
  }
}

#if 0

/* Prime DPFP multiply */

dfmp(void *dp1, void *dp2, ea_t ea) {
  long long frac64, frac641, frac642;
  int exp32, exp321, exp322;
  short fcode;

  fcode = 0;
  CLEARC;
  getdp(dp1, &frac641, &exp321);
  getdp(dp2, &frac642, &exp322);
  exp32 = exp321 + exp322;
  /* XXX: need to get 128-bit result to test for overflow? */
  frac64 = frac641 * frac642;
  if (exp32 > 32767 || exp32 < -32768)
    fcode = FC_DFP_OFLOW;
  /* insert (optional) rounding code here */
  if (fcode == 0)
    putdp(dp1, frac64, exp32);
  else
    mathexception('f', fcode, ea);
}
#endif
