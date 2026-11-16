-- | Declarations that the WASI libc leaves out, because WASI has no
-- UNIX-domain sockets, no terminals, and no threads that can stop. Packages
-- such as @network@, @terminal-size@, and @crypton@ read them at compile
-- time.
module WasiHeaders (wasiDeclarations) where

-- The compiler of the WASI target does not define @__unix__@. A package
-- such as @basement@ identifies a POSIX system by @_POSIX_VERSION@ before
-- it includes a libc header.
#if !defined(_POSIX_VERSION)
#error "_POSIX_VERSION is not defined before the first header"
#endif

#include <pthread.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/un.h>

-- | The size of @sun_path@, the size of @struct winsize@, the value of
-- @SCM_RIGHTS@, and the size of a pointer to @pthread_exit@.
wasiDeclarations :: [Int]
wasiDeclarations =
  [ #{const sizeof(((struct sockaddr_un *)0)->sun_path)},
    #{size struct winsize},
    #{const CMSG_LEN(0) > 0 ? SCM_RIGHTS : 0},
    #{const sizeof(&pthread_exit)}
  ]
