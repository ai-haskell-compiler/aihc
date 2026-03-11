#include "hsiconv.h"

/* On some platforms (notably darwin) the iconv functions are defined as
 * a macro rather than a real C function. Doh! That means we need these
 * wrappers to get a real C functions we can import via the Haskell FFI.
 */

iconv_t hs_wrap_iconv_open(const char *tocode, const char *fromcode) {
  return iconv_open(tocode, fromcode);
}

size_t hs_wrap_iconv(iconv_t cd,
                     char **inbuf, size_t *inbytesleft,
                     char **outbuf, size_t *outbytesleft) {
  return iconv(cd, inbuf, inbytesleft, outbuf, outbytesleft);
}

int hs_wrap_iconv_close(iconv_t cd) {
  return iconv_close(cd);
}
