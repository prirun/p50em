/* ** general conversion doc here, or (referenced) above. ***************** */
/*    doc exponent details here: (-1 & bin pt. placement by prieee) vs.
      vs. (-2 by ieeepr()): note that
      IEEE denormalized bin pxxxxxxx prime (relative to the 23 bits);
      in ieeepr the trick is to treat normalized #'s as the special case.   */


void prieee4(xlongp)     /* ****** convert from Prime to IEEE. ****** */
   long  *xlongp;
{
   long  xlong;
   long  sign;     /* sign */
   long  mant;     /* mantissa: note this is signed: mant = -mant below */
   long  exp;      /* exponent */

/* note: when converting form Prime to IEEE, we need to check for exponent
         underflow, but not for overflow. */

   xlong = *xlongp;
   mant = xlong & 0xffffff00;   /* 24 bits: includes sign bit */
   sign = mant & 0x80000000;
   mant >>= 8;  /* ok to trash upper byte, which will get masked out. */
   if (sign)
      mant = - mant;
   /* now have 24 bit # w/ leading 0. */

   exp = (xlong & 0x000000ff) - 1;  /* exp now in excess 127 (IEEE) form. */

   if (exp < 0)  {
      mant >>= 1;  /* tiny tiny #; will get shifted once more below. */
      ++exp;       /* Bring exp back up to 0. */
   }
   else  /* *** normalize the mantissa. If input Prime mantissa was already
                noramalized, this loop will still execute once, unless input #
                was -(2**n) in which case it will execute 0 times.  This is
                because Prime mantissas are 2's compliment, and powers of 2
                normalize differently when negated.  Note no loop for 0 exp. */
      while (exp  &&  !(mant & 0x00800000))  {
         mant <<= 1;
         --exp;
      }

   /* we now have a 24 bit unsigned # w/ a leading 1.  If the exponent is > 0,
      we are all set: the resulting IEEE number will be normalized.  This
      leading (hidden) IEEE 1 will be masked out below. */
   if ( ! exp)       /* result must be denormalized: shift 1 further to */
      mant >>= 1;    /* include IEEE leading bit, which may be 0 or 1.  */

   mant &= 0x007fffff;
   exp <<= 23;
   *xlongp = sign | exp | mant;
}


void ieeepr4(xlongp)     /* ****** convert from IEEE to Prime. ****** */
   long  *xlongp;
{
   long xlong;
   long  sign;     /* sign */
   long  mant;     /* mantissa: note this is signed: mant = -mant below */
   long  exp;      /* exponent */
   long  templ;

/* note: when converting form Prime to IEEE, we need to check for exponent
         overflow, but not for underflow. */
   xlong = *xlongp;

   if ( ! (xlong & 0x7fffffff))
      *xlongp = 0;                        /* +0 or -0 */
   else  {
      sign = xlong & 0x80000000;
      mant = (xlong & 0x007fffff) << 8; /* assume denormalized, adjust below */
      exp = (xlong & 0x7f800000) >> 23; /* still in excess 127 (IEEE) form. */
      if (exp == 0xff)  {      /* NaN (not a number) or +/- infinity? */
         if (mant == 0)  {
            /* +/- infinity. */
            if (sign == 0)
               *xlongp = 0x7fffffff;    /* largest Prime # in for +infinity. */
            else                        /* note: 0x800000ff cant be negated; */
               *xlongp = 0x800001ff;    /* use - 0x7fffffff for -infinity.   */
         }
         else  {
            /* NaN (not a number) */
            if (sign == 0)
               *xlongp = 0x539733ff;    /* use 1.11111e+38 for NaN, since */
            else                        /*      it stands out if printed. */
               *xlongp = 0xac68cdff;    /* use -1.11111e+38 for -NaN.     */
         }
      }
      else  {   /* actual number */
         if (exp != 0)  {
            if(mant != 0x7fffff00)/* Special case of mantissa value of all 1s*/
               mant = (mant >> 1) + 0x40000080; /* ieee normalized number:   */
                                                /* shift to make room for    */
                                                /* hidden leading 1 bit, 'or'*/
                                                /* it in and round truncated */
                                                /* LSBit up.                 */
            mant &= 0xffffff00;
         }
         exp += 2;      /* exp now in excess 128 (Prime) form. */

         if (sign != 0)  /* sign bit of mant will be 0 at this point. */
            mant = - mant;
         /* mant is now a 24 bit signed mantissa (shifted to Prime position) */

         /* *** normalize the number.  In most cases, the number will already
            be normalized.  Negative powers of 2 will be shifted once, since
            they normalize differently.  IEEE denormalized numbers can be
            adjusted at most 2 bits, due to the excess 127 vs. 128 exponent and
            binary point position differences (cant negate exponent). *** */
         while (exp  &&  ((~(mant^(mant<<1))) & 0x80000000))  {
            mant <<= 1;    /* sign and next most significant */
            --exp;         /* bit are equal: normalize.      */
         }
         if (exp > 0xff)  {
            if ( ! sign)
               *xlongp = 0x7fffffff;    /* largest Prime # in for +infinity. */
            else                        /* note: 0x800000ff cant be negated; */
               *xlongp = 0x800001ff;    /* use - 0x7fffffff for -infinity.   */
         }
         else
            *xlongp = mant | exp;   /* mant is signed; */
      }
   }
}


#define l1 0
#define l0 1

/* ** general conversion doc here, or (referenced) above. ***************** */
/*    doc exponent details here: (-1 & bin pt. placement by prieee) vs.
      vs. (-2 by ieeepr()): note that
      IEEE denormalized bin pxxxxxxx prime (relative to the 23 bits);
      in ieeepr the trick is to treat normalized #'s as the special case.   */


void prieee8(xlongp)     /* ****** convert from Prime to IEEE. ****** */
   long  *xlongp;
{
   long  xlong1,xlong0;
   long  mant1,mant0;   /* mantissa: these are signed: mant = -mant below */
   long  sign;          /* sign */
   long  exp;           /* exponent */

/* note: when converting REAL*8 form Prime to IEEE, we need to check for both
         exponent underflow, and overflow. */

   xlong1 = xlongp[l1];  /* high 4 bytes */
   xlong0 = xlongp[l0];  /* low  4 bytes */
   sign = xlong1 & 0x80000000;
   mant1 = xlong1 >> 11;                                 /* 48 bits, includes */
   mant0 = (xlong1 << 21) | ((xlong0 >> 11) & 0xfffe0);  /* includes sign bit */

   if ( ! mant0  &&  ! mant1)  {  /* zero (dirty or otherwise)? */
      xlongp[l1] = xlongp[l0] = 0;
      return;                                              /* return ****** */
   }
   if (sign)  {  /* 2's comp mant1/mant0 pair */
      mant0 = - mant0;
      mant1 = ~ mant1;
      if ( ! mant0)
         ++ mant1;  /* mant0==0: have a carry fr. mant0 to mant1. */
   }

   /* now have 48 bit # w/ leading 0. */

   exp = (xlong0 & 0xffff) - 0x80;   /* convert 16 bit    */
   if (exp & 0x8000)                 /* Prime exponent    */
      exp |= 0xffff0000;             /* from "excess 128" */
   else                              /* form to 2's       */
      exp &= 0x0000ffff;             /* complement form.  */
   exp += 0x3ff;   /* exp now in excess 1023 (IEEE) form. */

   if (exp < -50)  {               /* Prime exp too small? */
      xlongp[l1] = xlongp[l0] = 0;   /* closest IEEE is 0.   */
      return;                                              /* return ****** */
   }

   if (exp < 0)  {
      /* note: can still get 0, iff have leading (non sign bit) 0's */
      mant0 = (mant1 << 32+exp) | (mant0 >> -exp);    /* mant >>= -exp; */
      mant1 >>= -exp; /* tiny tiny #; will get shifted once more below. */
      ++mant0;  /* lsb will get lost below, since # is denormalized; round */
      if ( ! mant0)
         ++mant1;
      exp = 0;    /* exp += -exp. */
   }
   else  /* *** normalize the mantissa. */
      while (exp  &&  !(mant1 & 0x00100000))  {
         mant1 <<= 1;
         if (mant0 & 0x80000000)
            mant1 |= 1;
         mant0 <<= 1;
         --exp;
      }

   if (exp > 0x7fe)  {
      xlongp[l1] = sign | 0x7fefffff;  /* Prime exp too big; closest */
      xlongp[l0] =        0xffffffff;  /* closest IEEE is (+/-) max. */
   }
   else  {
      /* we now have a 48 bit unsigned # w/ a leading 1.  If the exponent is
         > 0, we are all set: the resulting IEEE number will be normalized.
         This leading (hidden) IEEE 1 will be masked out below. */
      if ( ! exp)  {   /* result must be denormalized: shift 1 further to */
         mant0 >>= 1;  /* include IEEE leading bit, which may be 0 or 1.  */
         if (mant1 & 1)
            mant0 |= 0x80000000;
         mant1 >>= 1;
      }
      mant1 &= 0x000fffff;
      exp <<= 20;
      xlongp[l1] = sign | exp | mant1;
      xlongp[l0] = mant0;
   }
}


void ieeepr8(xlongp)     /* ****** convert from IEEE to Prime. ****** */
   long  *xlongp;
{
   long xlong1,xlong0;
   long  mant1,mant0;   /* mantissa: these are signed: mant = -mant below */
   long  sign;     /* sign */
   long  exp;      /* exponent */

/* note: when converting REAL*8 form Prime to IEEE, we dont need to check for
         exponent overflow, or underflow. */
   xlong1 = xlongp[l1];  /* high 4 bytes */
   xlong0 = xlongp[l0];  /* low  4 bytes */

   if ((xlong1 & 0x7fffffff) == 0  &&  xlong0 == 0)  {
      xlongp[l1] = xlongp[l0] = 0;          /* +0 or -0 */
      return;                                              /* return ****** */
   }
   sign = xlong1 & 0x80000000;
   exp = (xlong1 & 0x7ff00000) >> 20; /* still in excess 1023 (IEEE) form. */
   mant1 = ((xlong1 & 0xfffff) << 11) | ((xlong0 >> 21) & 0x7ff);
   mant0 =  xlong0 << 11;         /* assume denormalized, adjust below */
   if (exp == 0x7ff)  {      /* NaN (not a number) or +/- infinity? */
      if (mant1 == 0  &&  mant0 == 0)  {
         /* +/- infinity. */
         if (sign == 0)  {
            xlongp[l1] = 0x7fffffff;    /* largest Prime # in for +infinity. */
            xlongp[l0] = 0xffffffff;
         }
         else  {                        /* note: 0x80000000 0000ffff cant be */
            xlongp[l1] = 0x80000000;    /* negated, use -0x7fffffff ffffffff */ 
            xlongp[l0] = 0x0001ffff;    /* -infinity.                        */
         }
      }
      else  {
         /* NaN (not a number) */
         if (sign == 0)  {
            xlongp[l1] = 0x7fffffff;    /* For now, use +/- infinity values */
            xlongp[l0] = 0xffffffff;    /* for +/- NaN.                     */
         }
         else  {
            xlongp[l1] = 0x80000000;
            xlongp[l0] = 0x0001ffff;
         }
      }
   }
   else  {   /* actual number */
      if (exp != 0)  {
         /* ieee normalized number: shift mantissa to make room for hidden */
         mant0 >>= 1;                     /* leading 1 bit and 'or' it in. */
         if (mant1 & 1)
            mant0 |= 0x80000000;
         else
            mant0 &= 0x7fffffff;
         mant1 >>= 1;
         mant1 |= 0x40000000;
      }
      exp -= 894;      /* exp now in 'excess 128' (Prime) form. */

      mant0 &= 0xffff0000;
      if (sign)  {      /* sign bit of mant will be 0 at this point. */
         mant0 |= 0xffff;  /* 2's comp mant1/mant0 pair */
         mant0 = - mant0;
         mant1 = ~ mant1;
         if ( ! mant0)
            ++ mant1;    /* mant0==0: have a carry fr. mant0 to mant1. */
      }
      /* mant is now a 48 bit signed mantissa (shifted to Prime position) */

      /* *** normalize the number.  In most cases, the number will already
         be normalized.  Negative powers of 2 will be shifted once, since
         they normalize differently.  IEEE denormalized numbers can be
         adjusted at most 2 bits, due to the excess 127 vs. 128 exponent and
         binary point position differences (cant negate exponent). *** */
      while (exp  &&  ((~(mant1^(mant1<<1))) & 0x80000000))  {
         mant1 <<= 1;               /* sign and next most significant */
         if (mant0 & 0x80000000)    /* bit are equal: normalize.      */
            mant1 |= 1;
         mant0 <<= 1;
         --exp;
      }
      mant0 &= 0xffff0000;
      xlongp[l1] = mant1;         /* mant is signed; no sign to 'or' in. */
      xlongp[l0] = mant0 | exp;
   }
}
