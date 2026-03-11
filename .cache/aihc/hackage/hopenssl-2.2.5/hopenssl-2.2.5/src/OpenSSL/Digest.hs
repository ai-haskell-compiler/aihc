{-# LANGUAGE FlexibleInstances #-}

{- |
   Maintainer:  simons@cryp.to
   Stability:   provisional
   Portability: portable

   This module provides a generic high-level API to the message digest
   algorithms found in OpenSSL's @crypto@ library. There are two functions of
   particular interest: 'digestByName' and 'digest'. The former can be used to
   retrieve an 'Algorithm', i.e. an OpenSSL object that implements a particular
   algorithm. That type can then be used to compute actual message digests with
   the latter function:

   >>> import Data.ByteString.Char8 ( pack )
   >>> digest (digestByName "md5") (pack "Hello, world.")
   "\b\n\239\131\155\149\250\207s\236Y\147u\233-G"

   Neat pretty-printing can be achieved with 'toHex', which converts the binary
   representation of a message digest into the common hexadecimal one:

   >>> toHex $ digest (digestByName "md5") (pack "Hello, world.")
   "080aef839b95facf73ec599375e92d47"
   >>> toHex $ digest (digestByName "sha1") (pack "Hello, world.")
   "2ae01472317d1935a84797ec1983ae243fc6aa28"

   The precise set of available digest algorithms provided by OpenSSL depends
   on the version of the library installed into the system, obviously, but it's
   reasonable to expect the following algorithms to be present: MD5, RIPEMD160,
   SHA1, SHA224, SHA256, SHA384, and SHA512. If an algorithm is not available,
   'digestByName' will throw an 'DigestAlgorithmNotAvailableInOpenSSL'
   exception. If you don't like exceptions, use the tamer 'digestByName''
   variant:

   >>> digestByName' "i bet this algorithm won't exist"
   Nothing

   'Algorithm' is an instance of 'IsString', so with the proper GHC extensions
   enabled it's possible to simplify the call to 'digest' even further:

   >>> :set -XOverloadedStrings
   >>> toHex $ digest "sha256" (pack "The 'Through the Universe' podcast rules.")
   "73624694a9435095c8fdaad711273a23c02226196c452f817cfd86f965895614"

   Last but not least, 'digest' is actually a class method of 'Digestable',
   which collects things we can compute digests of. The defaults are
   conservative, i.e. we support all things that correspond roughly to C's
   construct of "void pointer plus a length". @digest@ can use with any of the
   following signatures:

   >>> let shape1 = digest :: Algorithm -> (Ptr (),    CSize) -> MessageDigest
   >>> let shape2 = digest :: Algorithm -> (Ptr Word8, CSize) -> MessageDigest
   >>> let shape3 = digest :: Algorithm -> (Ptr Word8, CUInt) -> MessageDigest
   >>> let shape4 = digest :: Algorithm -> (Ptr (),    Int)   -> MessageDigest
   >>> let shape5 = digest :: Algorithm -> StrictByteString   -> MessageDigest
   >>> let shape6 = digest :: Algorithm -> LazyByteString     -> MessageDigest

   'StrictByteString' and 'LazyByteString' are also instances of 'IsString' and
   therefore subject to implicit construction from string literals:

   >>> shape5 "sha256" "hello" == shape6 "sha256" "hello"
   True

   Note that this code offers no overloaded 'digest' version for 'String',
   because that function would produce non-deterministic results for Unicode
   characters. There is an instance for @[Word8]@, though, so strings can be
   hashed after a proper encoding has been applied. For those who don't care
   about determinism, there is the following specialized function:

   >>> toHex $ digestString "md5" "no Digestable instance for this sucker"
   "a74827f849005794565f83fbd68ad189"

   If you don't mind orphaned instances, however, feel free to shoot yourself
   in the foot:

   >>> :set -XFlexibleInstances
   >>> instance Digestable String where updateChunk ctx str = withCStringLen str (updateChunk ctx)
   >>> toHex $ digest "sha256" ("now we can hash strings" :: String)
   "7f2989f173125810aa917c4ffe0e26ae1b5f7fb852274829c210297a43dfc7f9"
-}

module OpenSSL.Digest
  ( -- * Generic digest API
    MessageDigest, digest, Digestable(..), digestByName, digestByName', Algorithm
  , -- * Special instances
    digestString
  , -- * Helper Types and Functions
    toHex, StrictByteString, LazyByteString
  )
  where

import OpenSSL.EVP.Digest
import qualified OpenSSL.Util as Util

import Control.Exception
import qualified Data.ByteString as Strict ( ByteString, packCStringLen, concatMap )
import Data.ByteString.Char8 as Strict8 ( pack )
import qualified Data.ByteString.Lazy as Lazy ( ByteString, toChunks )
import Data.ByteString.Unsafe ( unsafeUseAsCStringLen )
import Foreign
import Foreign.C
import System.IO.Unsafe as IO

-- Generic Class API ----------------------------------------------------------

-- | A message digest is essentially an array of 'Word8' octets.

type MessageDigest = StrictByteString

-- | Compute the given message digest of any 'Digestable' thing, i.e. any type
-- that can be converted /efficiently/ and /unambiguously/ into a continuous
-- memory buffer or a sequence of continuous memory buffers. Note that 'String'
-- does /not/ have that property, because the binary representation chosen for
-- Unicode characters during the marshaling process is determined by the
-- system's locale and is therefore non-deterministic.

digest :: Digestable a => Algorithm -> a -> MessageDigest
digest algo input =
  IO.unsafePerformIO $
    bracket newContext freeContext $ \ctx -> do
      initDigest algo ctx
      updateChunk ctx input
      let mdSize = fromIntegral (digestSize algo)
      allocaArray mdSize $ \md -> do
        finalizeDigest ctx md
        Strict.packCStringLen (castPtr md, mdSize)

-- | A class of things that can be part of a digest computations. By default,
-- we define instances only for various representations of plain memory
-- buffers, but in theory that class can be extended to contain all kinds of
-- complex data types.

class Digestable a where
  updateChunk :: Context -> a -> IO ()

instance Digestable (Ptr a, CSize) where
  {-# INLINE updateChunk #-}
  updateChunk ctx = uncurry (updateDigest ctx)

instance Digestable (Ptr a, CUInt) where
  {-# INLINE updateChunk #-}
  updateChunk ctx = updateChunk ctx . fmap (fromIntegral :: CUInt -> CSize)

instance Digestable (Ptr a, CInt) where
  {-# INLINE updateChunk #-}
  updateChunk ctx = updateChunk ctx . fmap (fromIntegral :: CInt -> CSize)

instance Digestable (Ptr a, Int) where
  {-# INLINE updateChunk #-}
  updateChunk ctx = updateChunk ctx . fmap (fromIntegral :: Int -> CSize)

instance Digestable [Word8] where
  {-# INLINE updateChunk #-}
  updateChunk ctx buf = withArrayLen buf $ \len ptr -> updateChunk ctx (ptr,len)

instance Digestable StrictByteString where
  {-# INLINE updateChunk #-}
  updateChunk ctx str = unsafeUseAsCStringLen str (updateChunk ctx)

instance Digestable LazyByteString where
  {-# INLINE updateChunk #-}
  updateChunk ctx = mapM_ (updateChunk ctx) . Lazy.toChunks

-- |We do /not/ define a 'Digestable' instance for 'String', because there is
-- no one obviously correct way to encode Unicode characters for purposes of
-- calculating a digest. We have, however, this specialized function which
-- computes a digest over a @String@ by means of 'withCStrinLen'. This means
-- that the representation of Unicode characters depends on the process locale
-- a.k.a. it's non-deterministc!
--
-- >>> toHex $ digestString (digestByName "sha1") "Hello, world."
-- "2ae01472317d1935a84797ec1983ae243fc6aa28"

digestString :: Algorithm -> String -> MessageDigest
digestString algo str = IO.unsafePerformIO $
  withCStringLen str (return . digest algo)

-- Helper functions -----------------------------------------------------------

-- | Synonym for the strict 'Strict.ByteString' variant to improve readability.

type StrictByteString = Strict.ByteString

-- | Synonym for the lazy 'Lazy.ByteString' variant to improve readability.

type LazyByteString = Lazy.ByteString

-- | Pretty-print a given message digest from binary into hexadecimal
-- representation.
--
-- >>> toHex (Data.ByteString.pack [0..15])
-- "000102030405060708090a0b0c0d0e0f"

toHex :: MessageDigest -> StrictByteString
toHex = Strict.concatMap (pack . Util.toHex)
