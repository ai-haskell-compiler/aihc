#include <stdio.h>
#include <HsFFI.h>

union stg_ieee754_dbl
{
  double d;
  struct {

#if WORDS_BIGENDIAN
    unsigned int negative:1;
    unsigned int exponent:11;
    unsigned int mantissa0:20;
    unsigned int mantissa1:32;
#else
#if FLOAT_WORDS_BIGENDIAN
    unsigned int mantissa0:20;
    unsigned int exponent:11;
    unsigned int negative:1;
    unsigned int mantissa1:32;
#else
    unsigned int mantissa1:32;
    unsigned int mantissa0:20;
    unsigned int exponent:11;
    unsigned int negative:1;
#endif
#endif
  } ieee;
  /* This format makes it easier to see if a NaN is a signalling NaN.  */
  struct {

#if WORDS_BIGENDIAN
    unsigned int negative:1;
    unsigned int exponent:11;
    unsigned int quiet_nan:1;
    unsigned int mantissa0:19;
    unsigned int mantissa1:32;
#else
#if FLOAT_WORDS_BIGENDIAN
    unsigned int mantissa0:19;
    unsigned int quiet_nan:1;
    unsigned int exponent:11;
    unsigned int negative:1;
    unsigned int mantissa1:32;
#else
    unsigned int mantissa1:32;
    unsigned int mantissa0:19;
    unsigned int quiet_nan:1;
    unsigned int exponent:11;
    unsigned int negative:1;
#endif
#endif
  } ieee_nan;
};



double c_succ(double y)
{
  union stg_ieee754_dbl su;
 
  su.d=y;
  if (su.ieee.negative==0) {   /*  y >= 0 */
    if (su.ieee.exponent!=2047 || su.ieee.mantissa0!=0 || su.ieee.mantissa1!=0)
      if (su.ieee.mantissa1==0xffffffff) { 
        su.ieee.mantissa1=0; 
        if (su.ieee.mantissa0==1048575) { 
          su.ieee.mantissa0=0; 
	  su.ieee.exponent++;
        } else { 
          su.ieee.mantissa0++;
        }
      } else { 
        su.ieee.mantissa1++;
      }
  } 
  else {                  /* y < 0 */
    if (su.ieee.exponent!=2047 || su.ieee.mantissa0!=0 || su.ieee.mantissa1==0){
      if (su.ieee.negative==1 && su.ieee.exponent==0 && su.ieee.mantissa0==0 && su.ieee.mantissa1==0) {
        su.ieee.negative=0;
        su.ieee.mantissa1=1;
      } else {
        if (su.ieee.mantissa1==0) { 
          su.ieee.mantissa1=0xffffffff; 
          if (su.ieee.mantissa0==0) { 
            su.ieee.mantissa0=1048575; 
	    su.ieee.exponent--;
          } else { 
            su.ieee.mantissa0--;
          }
        } else { 
          su.ieee.mantissa1--;
        }
      }
    }
  }
  return su.d;
}         /* end function q_succ */






double c_pred(double y)
{
  union stg_ieee754_dbl su;

  su.d=y;
  if (su.ieee.negative==1) {   /*  y < 0 */
    if (su.ieee.exponent!=2047 ||  su.ieee.mantissa0!=0 || su.ieee.mantissa1!=0 ) 
      if (su.ieee.mantissa1==0xffffffff) { 
        su.ieee.mantissa1=0; 
        if (su.ieee.mantissa0==1048575) { 
          su.ieee.mantissa0=0; 
          su.ieee.exponent++;
        } else { 
          su.ieee.mantissa0++;
        }
      } else
        su.ieee.mantissa1++;
  } else {                 /* y >= 0 */
    if (su.ieee.exponent!=2047 || su.ieee.mantissa0!=0 || su.ieee.mantissa1!=0) 
      if (su.ieee.exponent==0 && su.ieee.mantissa0==0 && su.ieee.mantissa1==0) {
        su.ieee.negative=1;
        su.ieee.mantissa1=1;
      } else {
        if (su.ieee.mantissa1==0) {
          su.ieee.mantissa1=0xffffffff; 
          if (su.ieee.mantissa0==0) { 
            su.ieee.mantissa0=1048575; 
            su.ieee.exponent--;
          } else { 
            su.ieee.mantissa0--;
          }
        } else { 
          su.ieee.mantissa1--;
        }
      }
  }
  
  return su.d;
}              /* end function q_pred */

