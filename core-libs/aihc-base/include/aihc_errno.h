/* The errno constants that Foreign.C.Error exports.
 *
 * Every name here is a macro on every target, so the capi import of a
 * constant can name it without asking whether the C library of the target
 * has that error. A name the C library does not define is -1, which is what
 * GHC's own Foreign.C.Error gives such a constant and what isValidErrno
 * rejects.
 */

#ifndef AIHC_ERRNO_H
#define AIHC_ERRNO_H

#include <errno.h>

#ifdef E2BIG
#define AIHC_ERRNO_E2BIG E2BIG
#else
#define AIHC_ERRNO_E2BIG (-1)
#endif

#ifdef EACCES
#define AIHC_ERRNO_EACCES EACCES
#else
#define AIHC_ERRNO_EACCES (-1)
#endif

#ifdef EADDRINUSE
#define AIHC_ERRNO_EADDRINUSE EADDRINUSE
#else
#define AIHC_ERRNO_EADDRINUSE (-1)
#endif

#ifdef EADDRNOTAVAIL
#define AIHC_ERRNO_EADDRNOTAVAIL EADDRNOTAVAIL
#else
#define AIHC_ERRNO_EADDRNOTAVAIL (-1)
#endif

#ifdef EADV
#define AIHC_ERRNO_EADV EADV
#else
#define AIHC_ERRNO_EADV (-1)
#endif

#ifdef EAFNOSUPPORT
#define AIHC_ERRNO_EAFNOSUPPORT EAFNOSUPPORT
#else
#define AIHC_ERRNO_EAFNOSUPPORT (-1)
#endif

#ifdef EAGAIN
#define AIHC_ERRNO_EAGAIN EAGAIN
#else
#define AIHC_ERRNO_EAGAIN (-1)
#endif

#ifdef EALREADY
#define AIHC_ERRNO_EALREADY EALREADY
#else
#define AIHC_ERRNO_EALREADY (-1)
#endif

#ifdef EBADF
#define AIHC_ERRNO_EBADF EBADF
#else
#define AIHC_ERRNO_EBADF (-1)
#endif

#ifdef EBADMSG
#define AIHC_ERRNO_EBADMSG EBADMSG
#else
#define AIHC_ERRNO_EBADMSG (-1)
#endif

#ifdef EBADRPC
#define AIHC_ERRNO_EBADRPC EBADRPC
#else
#define AIHC_ERRNO_EBADRPC (-1)
#endif

#ifdef EBUSY
#define AIHC_ERRNO_EBUSY EBUSY
#else
#define AIHC_ERRNO_EBUSY (-1)
#endif

#ifdef ECHILD
#define AIHC_ERRNO_ECHILD ECHILD
#else
#define AIHC_ERRNO_ECHILD (-1)
#endif

#ifdef ECOMM
#define AIHC_ERRNO_ECOMM ECOMM
#else
#define AIHC_ERRNO_ECOMM (-1)
#endif

#ifdef ECONNABORTED
#define AIHC_ERRNO_ECONNABORTED ECONNABORTED
#else
#define AIHC_ERRNO_ECONNABORTED (-1)
#endif

#ifdef ECONNREFUSED
#define AIHC_ERRNO_ECONNREFUSED ECONNREFUSED
#else
#define AIHC_ERRNO_ECONNREFUSED (-1)
#endif

#ifdef ECONNRESET
#define AIHC_ERRNO_ECONNRESET ECONNRESET
#else
#define AIHC_ERRNO_ECONNRESET (-1)
#endif

#ifdef EDEADLK
#define AIHC_ERRNO_EDEADLK EDEADLK
#else
#define AIHC_ERRNO_EDEADLK (-1)
#endif

#ifdef EDESTADDRREQ
#define AIHC_ERRNO_EDESTADDRREQ EDESTADDRREQ
#else
#define AIHC_ERRNO_EDESTADDRREQ (-1)
#endif

#ifdef EDIRTY
#define AIHC_ERRNO_EDIRTY EDIRTY
#else
#define AIHC_ERRNO_EDIRTY (-1)
#endif

#ifdef EDOM
#define AIHC_ERRNO_EDOM EDOM
#else
#define AIHC_ERRNO_EDOM (-1)
#endif

#ifdef EDQUOT
#define AIHC_ERRNO_EDQUOT EDQUOT
#else
#define AIHC_ERRNO_EDQUOT (-1)
#endif

#ifdef EEXIST
#define AIHC_ERRNO_EEXIST EEXIST
#else
#define AIHC_ERRNO_EEXIST (-1)
#endif

#ifdef EFAULT
#define AIHC_ERRNO_EFAULT EFAULT
#else
#define AIHC_ERRNO_EFAULT (-1)
#endif

#ifdef EFBIG
#define AIHC_ERRNO_EFBIG EFBIG
#else
#define AIHC_ERRNO_EFBIG (-1)
#endif

#ifdef EFTYPE
#define AIHC_ERRNO_EFTYPE EFTYPE
#else
#define AIHC_ERRNO_EFTYPE (-1)
#endif

#ifdef EHOSTDOWN
#define AIHC_ERRNO_EHOSTDOWN EHOSTDOWN
#else
#define AIHC_ERRNO_EHOSTDOWN (-1)
#endif

#ifdef EHOSTUNREACH
#define AIHC_ERRNO_EHOSTUNREACH EHOSTUNREACH
#else
#define AIHC_ERRNO_EHOSTUNREACH (-1)
#endif

#ifdef EIDRM
#define AIHC_ERRNO_EIDRM EIDRM
#else
#define AIHC_ERRNO_EIDRM (-1)
#endif

#ifdef EILSEQ
#define AIHC_ERRNO_EILSEQ EILSEQ
#else
#define AIHC_ERRNO_EILSEQ (-1)
#endif

#ifdef EINPROGRESS
#define AIHC_ERRNO_EINPROGRESS EINPROGRESS
#else
#define AIHC_ERRNO_EINPROGRESS (-1)
#endif

#ifdef EINTR
#define AIHC_ERRNO_EINTR EINTR
#else
#define AIHC_ERRNO_EINTR (-1)
#endif

#ifdef EINVAL
#define AIHC_ERRNO_EINVAL EINVAL
#else
#define AIHC_ERRNO_EINVAL (-1)
#endif

#ifdef EIO
#define AIHC_ERRNO_EIO EIO
#else
#define AIHC_ERRNO_EIO (-1)
#endif

#ifdef EISCONN
#define AIHC_ERRNO_EISCONN EISCONN
#else
#define AIHC_ERRNO_EISCONN (-1)
#endif

#ifdef EISDIR
#define AIHC_ERRNO_EISDIR EISDIR
#else
#define AIHC_ERRNO_EISDIR (-1)
#endif

#ifdef ELOOP
#define AIHC_ERRNO_ELOOP ELOOP
#else
#define AIHC_ERRNO_ELOOP (-1)
#endif

#ifdef EMFILE
#define AIHC_ERRNO_EMFILE EMFILE
#else
#define AIHC_ERRNO_EMFILE (-1)
#endif

#ifdef EMLINK
#define AIHC_ERRNO_EMLINK EMLINK
#else
#define AIHC_ERRNO_EMLINK (-1)
#endif

#ifdef EMSGSIZE
#define AIHC_ERRNO_EMSGSIZE EMSGSIZE
#else
#define AIHC_ERRNO_EMSGSIZE (-1)
#endif

#ifdef EMULTIHOP
#define AIHC_ERRNO_EMULTIHOP EMULTIHOP
#else
#define AIHC_ERRNO_EMULTIHOP (-1)
#endif

#ifdef ENAMETOOLONG
#define AIHC_ERRNO_ENAMETOOLONG ENAMETOOLONG
#else
#define AIHC_ERRNO_ENAMETOOLONG (-1)
#endif

#ifdef ENETDOWN
#define AIHC_ERRNO_ENETDOWN ENETDOWN
#else
#define AIHC_ERRNO_ENETDOWN (-1)
#endif

#ifdef ENETRESET
#define AIHC_ERRNO_ENETRESET ENETRESET
#else
#define AIHC_ERRNO_ENETRESET (-1)
#endif

#ifdef ENETUNREACH
#define AIHC_ERRNO_ENETUNREACH ENETUNREACH
#else
#define AIHC_ERRNO_ENETUNREACH (-1)
#endif

#ifdef ENFILE
#define AIHC_ERRNO_ENFILE ENFILE
#else
#define AIHC_ERRNO_ENFILE (-1)
#endif

#ifdef ENOBUFS
#define AIHC_ERRNO_ENOBUFS ENOBUFS
#else
#define AIHC_ERRNO_ENOBUFS (-1)
#endif

#ifdef ENODATA
#define AIHC_ERRNO_ENODATA ENODATA
#else
#define AIHC_ERRNO_ENODATA (-1)
#endif

#ifdef ENODEV
#define AIHC_ERRNO_ENODEV ENODEV
#else
#define AIHC_ERRNO_ENODEV (-1)
#endif

#ifdef ENOENT
#define AIHC_ERRNO_ENOENT ENOENT
#else
#define AIHC_ERRNO_ENOENT (-1)
#endif

#ifdef ENOEXEC
#define AIHC_ERRNO_ENOEXEC ENOEXEC
#else
#define AIHC_ERRNO_ENOEXEC (-1)
#endif

#ifdef ENOLCK
#define AIHC_ERRNO_ENOLCK ENOLCK
#else
#define AIHC_ERRNO_ENOLCK (-1)
#endif

#ifdef ENOLINK
#define AIHC_ERRNO_ENOLINK ENOLINK
#else
#define AIHC_ERRNO_ENOLINK (-1)
#endif

#ifdef ENOMEM
#define AIHC_ERRNO_ENOMEM ENOMEM
#else
#define AIHC_ERRNO_ENOMEM (-1)
#endif

#ifdef ENOMSG
#define AIHC_ERRNO_ENOMSG ENOMSG
#else
#define AIHC_ERRNO_ENOMSG (-1)
#endif

#ifdef ENONET
#define AIHC_ERRNO_ENONET ENONET
#else
#define AIHC_ERRNO_ENONET (-1)
#endif

#ifdef ENOPROTOOPT
#define AIHC_ERRNO_ENOPROTOOPT ENOPROTOOPT
#else
#define AIHC_ERRNO_ENOPROTOOPT (-1)
#endif

#ifdef ENOSPC
#define AIHC_ERRNO_ENOSPC ENOSPC
#else
#define AIHC_ERRNO_ENOSPC (-1)
#endif

#ifdef ENOSR
#define AIHC_ERRNO_ENOSR ENOSR
#else
#define AIHC_ERRNO_ENOSR (-1)
#endif

#ifdef ENOSTR
#define AIHC_ERRNO_ENOSTR ENOSTR
#else
#define AIHC_ERRNO_ENOSTR (-1)
#endif

#ifdef ENOSYS
#define AIHC_ERRNO_ENOSYS ENOSYS
#else
#define AIHC_ERRNO_ENOSYS (-1)
#endif

#ifdef ENOTBLK
#define AIHC_ERRNO_ENOTBLK ENOTBLK
#else
#define AIHC_ERRNO_ENOTBLK (-1)
#endif

#ifdef ENOTCONN
#define AIHC_ERRNO_ENOTCONN ENOTCONN
#else
#define AIHC_ERRNO_ENOTCONN (-1)
#endif

#ifdef ENOTDIR
#define AIHC_ERRNO_ENOTDIR ENOTDIR
#else
#define AIHC_ERRNO_ENOTDIR (-1)
#endif

#ifdef ENOTEMPTY
#define AIHC_ERRNO_ENOTEMPTY ENOTEMPTY
#else
#define AIHC_ERRNO_ENOTEMPTY (-1)
#endif

#ifdef ENOTSOCK
#define AIHC_ERRNO_ENOTSOCK ENOTSOCK
#else
#define AIHC_ERRNO_ENOTSOCK (-1)
#endif

#ifdef ENOTSUP
#define AIHC_ERRNO_ENOTSUP ENOTSUP
#else
#define AIHC_ERRNO_ENOTSUP (-1)
#endif

#ifdef ENOTTY
#define AIHC_ERRNO_ENOTTY ENOTTY
#else
#define AIHC_ERRNO_ENOTTY (-1)
#endif

#ifdef ENXIO
#define AIHC_ERRNO_ENXIO ENXIO
#else
#define AIHC_ERRNO_ENXIO (-1)
#endif

#ifdef EOPNOTSUPP
#define AIHC_ERRNO_EOPNOTSUPP EOPNOTSUPP
#else
#define AIHC_ERRNO_EOPNOTSUPP (-1)
#endif

#ifdef EPERM
#define AIHC_ERRNO_EPERM EPERM
#else
#define AIHC_ERRNO_EPERM (-1)
#endif

#ifdef EPFNOSUPPORT
#define AIHC_ERRNO_EPFNOSUPPORT EPFNOSUPPORT
#else
#define AIHC_ERRNO_EPFNOSUPPORT (-1)
#endif

#ifdef EPIPE
#define AIHC_ERRNO_EPIPE EPIPE
#else
#define AIHC_ERRNO_EPIPE (-1)
#endif

#ifdef EPROCLIM
#define AIHC_ERRNO_EPROCLIM EPROCLIM
#else
#define AIHC_ERRNO_EPROCLIM (-1)
#endif

#ifdef EPROCUNAVAIL
#define AIHC_ERRNO_EPROCUNAVAIL EPROCUNAVAIL
#else
#define AIHC_ERRNO_EPROCUNAVAIL (-1)
#endif

#ifdef EPROGMISMATCH
#define AIHC_ERRNO_EPROGMISMATCH EPROGMISMATCH
#else
#define AIHC_ERRNO_EPROGMISMATCH (-1)
#endif

#ifdef EPROGUNAVAIL
#define AIHC_ERRNO_EPROGUNAVAIL EPROGUNAVAIL
#else
#define AIHC_ERRNO_EPROGUNAVAIL (-1)
#endif

#ifdef EPROTO
#define AIHC_ERRNO_EPROTO EPROTO
#else
#define AIHC_ERRNO_EPROTO (-1)
#endif

#ifdef EPROTONOSUPPORT
#define AIHC_ERRNO_EPROTONOSUPPORT EPROTONOSUPPORT
#else
#define AIHC_ERRNO_EPROTONOSUPPORT (-1)
#endif

#ifdef EPROTOTYPE
#define AIHC_ERRNO_EPROTOTYPE EPROTOTYPE
#else
#define AIHC_ERRNO_EPROTOTYPE (-1)
#endif

#ifdef ERANGE
#define AIHC_ERRNO_ERANGE ERANGE
#else
#define AIHC_ERRNO_ERANGE (-1)
#endif

#ifdef EREMCHG
#define AIHC_ERRNO_EREMCHG EREMCHG
#else
#define AIHC_ERRNO_EREMCHG (-1)
#endif

#ifdef EREMOTE
#define AIHC_ERRNO_EREMOTE EREMOTE
#else
#define AIHC_ERRNO_EREMOTE (-1)
#endif

#ifdef EROFS
#define AIHC_ERRNO_EROFS EROFS
#else
#define AIHC_ERRNO_EROFS (-1)
#endif

#ifdef ERPCMISMATCH
#define AIHC_ERRNO_ERPCMISMATCH ERPCMISMATCH
#else
#define AIHC_ERRNO_ERPCMISMATCH (-1)
#endif

#ifdef ERREMOTE
#define AIHC_ERRNO_ERREMOTE ERREMOTE
#else
#define AIHC_ERRNO_ERREMOTE (-1)
#endif

#ifdef ESHUTDOWN
#define AIHC_ERRNO_ESHUTDOWN ESHUTDOWN
#else
#define AIHC_ERRNO_ESHUTDOWN (-1)
#endif

#ifdef ESOCKTNOSUPPORT
#define AIHC_ERRNO_ESOCKTNOSUPPORT ESOCKTNOSUPPORT
#else
#define AIHC_ERRNO_ESOCKTNOSUPPORT (-1)
#endif

#ifdef ESPIPE
#define AIHC_ERRNO_ESPIPE ESPIPE
#else
#define AIHC_ERRNO_ESPIPE (-1)
#endif

#ifdef ESRCH
#define AIHC_ERRNO_ESRCH ESRCH
#else
#define AIHC_ERRNO_ESRCH (-1)
#endif

#ifdef ESRMNT
#define AIHC_ERRNO_ESRMNT ESRMNT
#else
#define AIHC_ERRNO_ESRMNT (-1)
#endif

#ifdef ESTALE
#define AIHC_ERRNO_ESTALE ESTALE
#else
#define AIHC_ERRNO_ESTALE (-1)
#endif

#ifdef ETIME
#define AIHC_ERRNO_ETIME ETIME
#else
#define AIHC_ERRNO_ETIME (-1)
#endif

#ifdef ETIMEDOUT
#define AIHC_ERRNO_ETIMEDOUT ETIMEDOUT
#else
#define AIHC_ERRNO_ETIMEDOUT (-1)
#endif

#ifdef ETOOMANYREFS
#define AIHC_ERRNO_ETOOMANYREFS ETOOMANYREFS
#else
#define AIHC_ERRNO_ETOOMANYREFS (-1)
#endif

#ifdef ETXTBSY
#define AIHC_ERRNO_ETXTBSY ETXTBSY
#else
#define AIHC_ERRNO_ETXTBSY (-1)
#endif

#ifdef EUSERS
#define AIHC_ERRNO_EUSERS EUSERS
#else
#define AIHC_ERRNO_EUSERS (-1)
#endif

#ifdef EWOULDBLOCK
#define AIHC_ERRNO_EWOULDBLOCK EWOULDBLOCK
#else
#define AIHC_ERRNO_EWOULDBLOCK (-1)
#endif

#ifdef EXDEV
#define AIHC_ERRNO_EXDEV EXDEV
#else
#define AIHC_ERRNO_EXDEV (-1)
#endif

#endif /* AIHC_ERRNO_H */
