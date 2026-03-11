
#ifndef _HS_NETTLE_HASH_H
#define _HS_NETTLE_HASH_H _HS_NETTLE_HASH_H

#include <nettle/version.h>

#if (NETTLE_VERSION_MAJOR != 3)
#error unsupported nettle version
#endif
#if (NETTLE_VERSION_MAJOR == 3 && NETTLE_VERSION_MINOR < 2)
#error SHA-3 interface broke compat in 3.2; earlier versions not supported
#endif

#include <sys/types.h>
#include <nettle/cbc.h>
#include <nettle/gcm.h>
#include <nettle/memxor.h>

/* hash algorithms */
#include <nettle/gosthash94.h>
#include <nettle/md2.h>
#include <nettle/md4.h>
#include <nettle/md5.h>
#include <nettle/ripemd160.h>
#include <nettle/sha1.h>
#include <nettle/sha2.h>
#include <nettle/sha3.h>

#include <nettle/umac.h>

#endif
