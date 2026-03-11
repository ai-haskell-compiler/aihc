#include <iconv.h>

iconv_t hs_wrap_iconv_open(const char *tocode, const char *fromcode);

size_t hs_wrap_iconv(iconv_t cd,
                     char **inbuf, size_t *inbytesleft,
                     char **outbuf, size_t *outbytesleft);

int hs_wrap_iconv_close(iconv_t cd);
