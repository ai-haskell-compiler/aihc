{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
-- |
-- Module      : Crypto.Hash.CryptoAPI
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Cryptohash API exported through crypto-api.
--
-- Note: Current version (0.10) of crypto-api suffers a small performance problem.
-- see <http://tab.snarc.org/others/benchmark-cryptohash-0.8.html>.
-- Hopefully, future versions will fix this.
--
module Crypto.Hash.CryptoAPI
    ( MD2
    , MD4
    , MD5
    , SHA1
    , SHA224
    , SHA256
    , SHA384
    , SHA512
    , Skein256_256
    , Skein512_512
    , RIPEMD160
    , Tiger
    , Whirlpool
    , Hash(..)
    -- * Contexts
    , CTXMD2, CTXMD4, CTXMD5, CTXRIPEMD160, CTXSHA1, CTXSHA224
    , CTXSHA256, CTXSHA384, CTXSHA512, CTXSkein256_256, CTXSkein512_512
    , CTXTiger, CTXWhirlpool
    ) where

import qualified "cryptonite" Crypto.Hash as H
import qualified Data.ByteString.Lazy as L

import Control.Monad (liftM)
import Data.ByteString (ByteString)
import Data.Serialize (Serialize(..))
import Data.Serialize.Get (getByteString)
import Data.Serialize.Put (putByteString)
import Data.Tagged (Tagged(..))
import Crypto.Classes (Hash(..), hash, hash')
import qualified Data.ByteArray as B (convert)

-- 
-- need to redefine a context wrapper to not clash with the already existing
-- and avoid the "function dependencies conflict between instance declaration" error.
--
-- unfortunately haskell uses cpp in traditional mode to avoid problem, but traditional mode
-- doesn't do proper token concatenation, so need to define the ctxname in the macro
--

#define DEFINE_TYPE_AND_INSTANCES_SIMPLE(CTXNAME, MODULENAME, OUTPUTLEN, BLOCKLEN)    \
    DEFINE_TYPE_AND_INSTANCES(CTXNAME, MODULENAME, MODULENAME, OUTPUTLEN, BLOCKLEN)

#define DEFINE_TYPE_AND_INSTANCES(CTXNAME, NAME, MODULENAME, OUTPUTLEN, BLOCKLEN)    \
\
data NAME = NAME !ByteString deriving (Eq,Ord,Show); \
\
instance Serialize NAME where \
   { get          = liftM NAME (getByteString OUTPUTLEN) \
   ; put (NAME d) = putByteString d \
   }; \
\
instance Hash CTXNAME NAME where \
   { outputLength    = Tagged (OUTPUTLEN * 8) \
   ; blockLength     = Tagged (BLOCKLEN * 8)  \
   ; initialCtx      = CTXNAME H.hashInit       \
   ; updateCtx (CTXNAME ctx) = CTXNAME . H.hashUpdate ctx \
   ; finalize (CTXNAME ctx) bs = NAME $ B.convert $ H.hashFinalize (H.hashUpdate ctx bs) \

#define DEFINE_TYPE_AND_INSTANCES_WITHLEN(CTXNAME, NAME, ILEN, MODULENAME, OUTPUTLEN, BLOCKLEN)    \
\
data NAME = NAME !ByteString deriving (Eq,Ord,Show); \
\
instance Serialize NAME where \
   { get          = liftM NAME (getByteString OUTPUTLEN) \
   ; put (NAME d) = putByteString d \
   }; \
\
instance Hash CTXNAME NAME where \
   { outputLength    = Tagged (OUTPUTLEN * 8) \
   ; blockLength     = Tagged (BLOCKLEN * 8)  \
   ; initialCtx      = CTXNAME (H.hashInit) \
   ; updateCtx (CTXNAME ctx) = CTXNAME . H.hashUpdate ctx      \
   ; finalize (CTXNAME ctx) bs = NAME $ B.convert $ H.hashFinalize (H.hashUpdate ctx bs) \



newtype CTXMD2 = CTXMD2 (H.Context H.MD2)
newtype CTXMD4 = CTXMD4 (H.Context H.MD4)
newtype CTXMD5 = CTXMD5 (H.Context H.MD5)
newtype CTXSHA1 = CTXSHA1 (H.Context H.SHA1)
newtype CTXSHA224 = CTXSHA224 (H.Context H.SHA224)
newtype CTXSHA256 = CTXSHA256 (H.Context H.SHA256)
newtype CTXSHA384 = CTXSHA384 (H.Context H.SHA384)
newtype CTXSHA512 = CTXSHA512 (H.Context H.SHA512)
newtype CTXRIPEMD160 = CTXRIPEMD160 (H.Context H.RIPEMD160)
newtype CTXTiger = CTXTiger (H.Context H.Tiger)
newtype CTXWhirlpool = CTXWhirlpool (H.Context H.Whirlpool)
newtype CTXSkein256_256 = CTXSkein256_256 (H.Context H.Skein256_256)
newtype CTXSkein512_512 = CTXSkein512_512 (H.Context H.Skein512_512)

DEFINE_TYPE_AND_INSTANCES_SIMPLE(CTXMD2, MD2, 16, 16)
   ; hash  = MD2 . B.convert . (H.hashlazy :: L.ByteString -> H.Digest H.MD2)
   ; hash' = MD2 . B.convert . (H.hashWith H.MD2)
   };

DEFINE_TYPE_AND_INSTANCES_SIMPLE(CTXMD4, MD4, 16, 64)
   ; hash = MD4 . B.convert . (H.hashlazy :: L.ByteString -> H.Digest H.MD4)
   ; hash' = MD4 . B.convert . (H.hashWith H.MD4)
   };

DEFINE_TYPE_AND_INSTANCES_SIMPLE(CTXMD5, MD5, 16, 64)
   ; hash = MD5 . B.convert . (H.hashlazy :: L.ByteString -> H.Digest H.MD5)
   ; hash' = MD5 . B.convert . (H.hashWith H.MD5)
   };

DEFINE_TYPE_AND_INSTANCES_SIMPLE(CTXSHA1, SHA1, 20, 64)
   ; hash = SHA1 . B.convert . (H.hashlazy :: L.ByteString -> H.Digest H.SHA1)
   ; hash' = SHA1 . B.convert . (H.hashWith H.SHA1)
   };

DEFINE_TYPE_AND_INSTANCES_SIMPLE(CTXSHA224, SHA224, 28, 64)
   ; hash = SHA224 . B.convert . (H.hashlazy :: L.ByteString -> H.Digest H.SHA224)
   ; hash' = SHA224 . B.convert . (H.hashWith H.SHA224)
   };

DEFINE_TYPE_AND_INSTANCES_SIMPLE(CTXSHA256, SHA256, 32, 64)
   ; hash = SHA256 . B.convert . (H.hashlazy :: L.ByteString -> H.Digest H.SHA256)
   ; hash' = SHA256 . B.convert . (H.hashWith H.SHA256)
   };

DEFINE_TYPE_AND_INSTANCES_SIMPLE(CTXSHA384, SHA384, 48, 128)
   ; hash = SHA384 . B.convert . (H.hashlazy :: L.ByteString -> H.Digest H.SHA384)
   ; hash' = SHA384 . B.convert . (H.hashWith H.SHA384)
   };

DEFINE_TYPE_AND_INSTANCES_SIMPLE(CTXSHA512, SHA512, 64, 128)
   ; hash = SHA512 . B.convert . (H.hashlazy :: L.ByteString -> H.Digest H.SHA512)
   ; hash' = SHA512 . B.convert . (H.hashWith H.SHA512)
   };

DEFINE_TYPE_AND_INSTANCES_SIMPLE(CTXRIPEMD160, RIPEMD160, 20, 64)
   ; hash = RIPEMD160 . B.convert . (H.hashlazy :: L.ByteString -> H.Digest H.RIPEMD160)
   ; hash' = RIPEMD160 . B.convert . (H.hashWith H.RIPEMD160)
   };

DEFINE_TYPE_AND_INSTANCES_SIMPLE(CTXTiger, Tiger, 24, 64)
   ; hash = Tiger . B.convert . (H.hashlazy :: L.ByteString -> H.Digest H.Tiger)
   ; hash' = Tiger . B.convert . (H.hashWith H.Tiger)
   };

DEFINE_TYPE_AND_INSTANCES_SIMPLE(CTXWhirlpool, Whirlpool, 64, 64)
   ; hash = Whirlpool . B.convert . (H.hashlazy :: L.ByteString -> H.Digest H.Whirlpool)
   ; hash' = Whirlpool . B.convert . (H.hashWith H.Whirlpool)
   };

DEFINE_TYPE_AND_INSTANCES_WITHLEN(CTXSkein256_256, Skein256_256, 256, Skein256, 32, 32)
   ; hash = Skein256_256 . B.convert . (H.hashlazy :: L.ByteString -> H.Digest H.Skein256_256)
   ; hash' = Skein256_256 . B.convert . (H.hashWith H.Skein256_256)
   };

DEFINE_TYPE_AND_INSTANCES_WITHLEN(CTXSkein512_512, Skein512_512, 512, Skein512, 64, 64)
   ; hash = Skein512_512 . B.convert . (H.hashlazy :: L.ByteString -> H.Digest H.Skein512_512)
   ; hash' = Skein512_512 . B.convert . (H.hashWith H.Skein512_512)
   };

