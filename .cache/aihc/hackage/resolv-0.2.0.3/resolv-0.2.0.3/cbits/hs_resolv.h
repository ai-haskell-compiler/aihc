#if !defined(HS_RESOLV_H)
#define HS_RESOLV_H

#include "hs_resolv_config.h"

#include <sys/types.h>

#if defined(HAVE_NETINET_IN_H)
# include <netinet/in.h>
#endif

#if defined(HAVE_DECL_H_ERRNO)
# include <netdb.h>
#endif

#if defined(HAVE_ARPA_NAMESER_H)
# include <arpa/nameser.h>
#endif

#include <resolv.h>

#include <assert.h>

/* This is usually provided via <arpa/nameser_compat.h> */
#if !defined(QUERY)
# define QUERY ns_o_query
#endif

#if !defined(USE_RES_NQUERY)
# error USE_RES_NQUERY not defined
#endif

#if USE_RES_NQUERY && (SIZEOF_STRUCT___RES_STATE <= 0)
# error broken invariant
#endif

/* Macro to calculate error code returned by hs_get_h_errno */
#define __HS_GET_H_ERRNO(h_errno)    \
  switch(h_errno)                    \
  {                                  \
    case HOST_NOT_FOUND: return 1;   \
    case NO_DATA:        return 2;   \
    case NO_RECOVERY:    return 3;   \
    case TRY_AGAIN:      return 4;   \
    case 0:              return 0;   \
    default:             return -1;  \
  }

#if USE_RES_NQUERY

inline static int
res_opt_set_use_dnssec(struct __res_state *s)
{
  assert(s);

  if (!(s->options & RES_INIT)) {
    int rc = res_ninit(s);
    if (rc) return rc;
  }

  s->options |= RES_USE_DNSSEC | RES_USE_EDNS0;

  return 0;
}

inline static int
hs_res_mkquery(struct __res_state *s, const char *dname, int class, int type, unsigned char *req, int reqlen0)
{
  assert(s);

  int reqlen = res_nmkquery(s, QUERY, dname, class, type, NULL, 0, NULL, req, reqlen0);

  assert(reqlen <= reqlen0);

  return reqlen;
}

inline static int
hs_res_send(struct __res_state *s, const unsigned char *msg, int msglen, unsigned char *answer, int anslen)
{
  assert(s);

  return res_nsend(s, msg, msglen, answer, anslen);
}

inline static int
hs_res_query(struct __res_state *s, const char *dname, int class, int type, unsigned char *answer, int anslen)
{
  assert(s);

  return res_nquery(s, dname, class, type, answer, anslen);
}

/* res_nclose() finalizes resources allocated by res_ninit() and subsequent
 * calls to res_nquery() */

inline static void
hs_res_close(struct __res_state *s)
{
  assert(s);

  res_nclose(s);
}

inline static int
hs_get_h_errno(struct __res_state *s)
{
#if defined(HAVE_DECL_H_ERRNO)
#if defined(HAVE_STRUCT___RES_STATE_RES_H_ERRNO)
  assert(s);
  __HS_GET_H_ERRNO(s->res_h_errno)
#else
  __HS_GET_H_ERRNO(h_errno)
#endif
#else
  return -1;
#endif
}

#else  /* USE_RES_NQUERY */

/* use non-reentrant API */

inline static int
res_opt_set_use_dnssec(void *s)
{
  assert(!s);

  if (!(_res.options & RES_INIT)) {
    int rc = res_init();
    if (rc) return rc;
  }

  _res.options |= RES_USE_DNSSEC | RES_USE_EDNS0;

  return 0;
}


inline static int
hs_res_mkquery(void *s, const char *dname, int class, int type, unsigned char *req, int reqlen0)
{
  assert(!s);

  int reqlen = res_mkquery(QUERY, dname, class, type, NULL, 0, NULL, req, reqlen0);

  assert(reqlen <= reqlen0);

  return reqlen;
}

inline static int
hs_res_send(void *s, const unsigned char *msg, int msglen, unsigned char *answer, int anslen)
{
  assert(!s);

  return res_send(msg, msglen, answer, anslen);
}

inline static int
hs_res_query(void *s, const char *dname, int class, int type, unsigned char *answer, int anslen)
{
  assert(!s);

  return res_query(dname, class, type, answer, anslen);
}

inline static void
hs_res_close(void *s)
{
}

inline static int
hs_get_h_errno(void *s)
{
#if defined(HAVE_DECL_H_ERRNO)
  __HS_GET_H_ERRNO(h_errno)
#else
  return -1;
#endif
}

#endif /* USE_RES_NQUERY */

#endif /* HS_RESOLV_H */
