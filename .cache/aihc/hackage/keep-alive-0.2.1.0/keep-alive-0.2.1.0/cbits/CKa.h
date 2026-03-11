#ifndef CKA_H
#define CKA_H

#ifdef _WIN32
# include <winsock2.h>
# include <ws2tcpip.h>
# include <mswsock.h>
# include <Mstcpip.h>

#ifndef INLINE
# if defined(_MSC_VER)
#  define INLINE extern __inline
# elif defined(__GNUC_GNU_INLINE__)
#  define INLINE extern inline
# else
#  define INLINE inline
# endif
#endif

INLINE int
winSetKeepAlive(int s, ULONG onoff, ULONG time, ULONG intvl) {

    struct tcp_keepalive ka;

    DWORD size;

    ka.onoff = onoff;
    ka.keepalivetime = time;
    ka.keepaliveinterval = intvl;
    int sizeka = sizeof(ka);

    int rtn = WSAIoctl(s, SIO_KEEPALIVE_VALS, &ka, sizeka, NULL, 0, &size, NULL, NULL);

    return rtn;
};
#endif /* _WIN32 */

#endif /* CKA_H */

