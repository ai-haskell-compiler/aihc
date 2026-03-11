-----------------------------------------------------------------------------
-- |
-- Module      :  Crypto.Skein
-- Copyright   :  (c) 2011 Felipe A. Lessa
-- License     :  BSD3 (see LICENSE)
--
-- Maintainer  :  felipe.lessa@gmail.com
-- Stability   :  provisional
-- Portability :  portable (needs FFI)
--
-- High-level interface for the Skein family of hash functions.
--
-----------------------------------------------------------------------------

module Crypto.Skein
    ( -- * Using this module
      -- $usingMod

      -- ** Skein as a cryptographic hash function
      -- $skeinAsHash

      -- ** Skein as a message authentication code (MAC)
      -- $skeinAsMAC

      -- * About Skein
      -- $about

      -- * Main cryptographic hash functions
      -- $mainHash

      -- ** Skein-512-512
      Skein_512_512
    , Skein_512_512_Ctx
      -- ** Skein-1024-1024
    , Skein_1024_1024
    , Skein_1024_1024_Ctx
      -- ** Skein-256-256
    , Skein_256_256
    , Skein_256_256_Ctx

      -- * Skein-MAC
      -- $skeinmac
    , Key
    , skeinMAC
    , skeinMAC'
    , SkeinMAC (skeinMACCtx)

      -- * Other variants of cryptographic hash functions
      -- $variants

      -- ** Skein-256-128
    , Skein_256_128
    , Skein_256_128_Ctx
      -- ** Skein-256-160
    , Skein_256_160
    , Skein_256_160_Ctx
      -- ** Skein-256-224
    , Skein_256_224
    , Skein_256_224_Ctx

      -- * Skein-512

      -- ** Skein-512-128
    , Skein_512_128
    , Skein_512_128_Ctx
      -- ** Skein-512-160
    , Skein_512_160
    , Skein_512_160_Ctx
      -- ** Skein-512-224
    , Skein_512_224
    , Skein_512_224_Ctx
      -- ** Skein-512-256
    , Skein_512_256
    , Skein_512_256_Ctx
      -- ** Skein-512-384
    , Skein_512_384
    , Skein_512_384_Ctx

      -- * Skein-1024

      -- ** Skein-1024-384
    , Skein_1024_384
    , Skein_1024_384_Ctx
      -- ** Skein-1024-512
    , Skein_1024_512
    , Skein_1024_512_Ctx
    ) where

-- from base
import Control.Monad (unless)
import Foreign
import Foreign.C
import qualified System.IO.Unsafe as U

-- from bytestring
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU

-- from cereal
import Data.Serialize

-- from tagged
import Data.Tagged (Tagged(..))

-- from crypto-api
import Crypto.Classes

-- from this package
import Crypto.Skein.Internal

----------------------------------------------------------------------

-- $usingMod
--
-- Currently this module provides both Skein as a cryptographic
-- hash function and Skein as a MAC.  This module currently does
-- not provide some Skein functions, such as tree hashing,
-- pseudo-random number generation, stream ciphing or key
-- derivation.
--
-- Terminology note: we say \"message\" for your variable-sized
-- data that is given to Skein.

-- $skeinAsHash
--
-- There are many variants of Skein as a cryptographic hash
-- function.  They are called @Skein_X_Y@, where @X@ is internal
-- state size in bits and @Y@ is the output size in bits.  The
-- main ones are 'Skein_512_512', 'Skein_1024_1024' and
-- 'Skein_256_256'.  If you are unsure, then use 'Skein_512_512'.
--
-- To use these data types, you have to use 'Hash' and
-- 'Serialize'.  Suppose you wanted to hash a lazy 'L.ByteString'
-- @bs@.  Then you could use
--
-- @
--   digest :: 'S.ByteString'
--   digest = let h = 'hash' bs :: 'Skein_512_512'
--            in 'encode' h
-- @
--
-- where 'hash' is from "Crypto.Classes" (@crypto-api@ package),
-- 'encode' is from 'Serialize' (@cereal@ package) and @digest@
-- is a strict 'B.ByteString' with the hash.  Given that we've
-- used 'Skein_512_512' which has an output size of 512 bits,
-- @digest@ will have @512 / 8 = 64@ bytes of length.

-- $skeinAsMAC
--
-- If you need a message authentication code (MAC), you may use
-- HMAC with Skein (e.g. HMAC-Skein-512-512).  Using HMAC with
-- Skein is supported and secure.  However, Skein also supports
-- another secure method, called Skein-MAC.
--
-- Skein-MAC is as secure as HMAC with Skein, however with a much
-- lower overhead (especially for short messages).  HMAC requires
-- two hash invocations.  Skein-MAC, on the other hand, requires
-- just one hash invocation after the setup.
--
-- To use Skein-MAC, you need 'skeinMAC' (or 'skeinMAC'') and
-- 'Serialize'.  You also need a 'Key' with at least as many bits
-- as the state size you choose (@256@, @512@ or @1024@ bits).
--
-- Suppose you want to use Skein-MAC with 'Skein_512_512'.  To
-- setup the MAC function with a 'Key' @key@ (that has at least
-- @512 / 8 = 64@ bytes), use
--
-- @
--   let calcMAC :: 'L.ByteString' -> 'Skein_512_512'
--       calcMAC = 'skeinMAC' key
-- @
--
-- It is recommended to partially apply 'skeinMAC' (or
-- 'skeinMAC'') to avoid recomputing the key setup.  We give an
-- explicit type to 'calcMAC' because we have to choose which
-- Skein we want.
--
-- Now, if you want to calculate the Skein-MAC of a lazy
-- 'L.ByteString' message @msg@, use
--
-- @
--   let msgMAC = 'encode' (calcMAC msg)
-- @
--
-- where 'encode' is from 'Serialize' (@cereal@ package) and
-- @msgMAC@ is a strict 'B.ByteString' with the MAC.
--
-- Now, suppose you are given a @msg'@ with MAC @msgMAC'@ and
-- want to check the integrity and authenticity of @msg'@.  You
-- may do so by calculating the MAC of @msg'@ and checking
-- against @msgMAC'@:
--
-- @
--   let msgMAC'' = 'encode' (calcMAC msg')
--   in if msgMAC' == msgMAC''
--      then Right \"Message is okay\"
--      else Left \"Message has been corrupted or tampered with\"
-- @

-- $about
--
-- From <http://www.skein-hash.info/about> at 09/2011:
--
-- Skein is a new family of cryptographic hash functions. Its
-- design combines speed, security, simplicity, and a great deal
-- of flexibility in a modular package that is easy to analyze.
--
-- Skein is fast. Skein-512 -- our primary proposal -- hashes data at
-- 6.1 clock cycles per byte on a 64-bit CPU. This means that on
-- a 3.1 GHz x64 Core 2 Duo CPU, Skein hashes data at 500
-- MBytes/second per core -- almost twice as fast as SHA-512 and
-- three times faster than SHA-256. An optional hash-tree mode
-- speeds up parallelizable implementations even more. Skein is
-- fast for short messages, too; Skein-512 hashes short messages
-- in about 1000 clock cycles.
--
-- Skein is secure. Our current best attack on Skein-512 is on 25
-- of 72 rounds, for a safety factor of 2.9. For comparison, at a
-- similar stage in the standardization process, the AES
-- encryption algorithm had an attack on 6 of 10 rounds, for a
-- safety factor of only 1.7. Additionally, Skein has a number of
-- provably secure properties, increasing confidence in the
-- algorithm.
--
-- Skein is simple. Using only three primitive operations, the
-- Skein compression function can be easily understood and
-- remembered. The rest of the algorithm is a straightforward
-- iteration of this function.
--
-- Skein is flexible. Skein is defined for three different
-- internal state sizes -- 256 bits, 512 bits, and 1024 bits --
-- and any output size. This allows Skein to be a drop-in
-- replacement for the entire SHA family of hash functions. A
-- completely optional and extendable argument system makes Skein
-- an efficient tool to use for a very large number of functions:
-- a PRNG, a stream cipher, a key derivation function,
-- authentication without the overhead of HMAC, and a
-- personalization capability. All these features can be
-- implemented with very low overhead. Together with the
-- Threefish large-block cipher at Skein core, this design
-- provides a full set of symmetric cryptographic primitives
-- suitable for most modern applications.
--
-- Skein is efficient on a variety of platforms, both hardware
-- and software. Skein-512 can be implemented in about 200 bytes
-- of state. Small devices, such as 8-bit smart cards, can
-- implement Skein-256 using about 100 bytes of memory. Larger
-- devices can implement the larger versions of Skein to achieve
-- faster speeds.
--
-- Skein was designed by a team of highly experienced
-- cryptographic experts from academia and industry, with
-- expertise in cryptography, security analysis, software, chip
-- design, and implementation of real-world cryptographic
-- systems. This breadth of knowledge allowed them to create a
-- balanced design that works well in all environments.

-- $mainHash
--
-- These are the main Skein hash functions.  Unless you have any
-- special reasons, you should use one of these.
--
--   ['Skein_512_512'] is the primary cryptographic hash function
--   of this package.  It can safely be used for all current
--   hashing applications, and should remain secure for the
--   foreseeable future.
--
--   ['Skein_1024_1024'] is the ultra-conservative variant.  Even
--   if some future attack managed to break 'Skein_512_512', it
--   is quite likely that 'Skein_1024_1024' would remain secure.
--
--   ['Skein_256_256'] is the low-memory variant.  It can be
--   implemented using 100 bytes of RAM, but this is not the case
--   with this implementation.  It is faster than 'Skein_512_512'
--   only for small message lengths, so it's preferable to use
--   'Skein_512_512'.  If you can't afford 512 bits of output,
--   you may get the speed advantage of 'Skein_512_512' by using
--   'Skein_512_256'.

-- $variants
--
-- These hash functions produce less output bits than their state
-- size.  For example, 'Skein_512_160' produces 160 output bits
-- while having 512 bits of state size.  Their main use is to be
-- a drop-in replacement to legacy hash functions.  If you don't
-- have any special reason for using them, use one of the main
-- hash functions above (e.g. 'Skein_512_512').
--
-- You may replace:
--
--   [MD5] with 'Skein_256_128' or 'Skein_512_128'.
--
--   [SHA-1] with 'Skein_256_160' or 'Skein_512_160'.
--
--   [SHA-224] with 'Skein_256_224' or 'Skein_512_224'.
--
--   [SHA-256] with 'Skein_256_256' or 'Skein_512_256'.
--
--   [SHA-384] with 'Skein_512_384' or 'Skein_1024_384'.
--
--   [SHA-512] with 'Skein_512_512' or 'Skein_1024_512'.

----------------------------------------------------------------------

-- $skeinmac
--
-- The standard way to use a hash function for authentication is
-- to use the HMAC construction.  While you may securely use
-- Skein with HMAC (e.g. HMAC-Skein-512-512), Skein also supports
-- another mode for MAC.  Skein-MAC is as secure as
-- HMAC-Skein, however faster.  Skein-MAC is as fast as
-- Skein as a hash function, with zero overhead.

-- | Secret key used to calculate the Skein-MAC.
--
-- The 'Key' may have any length.  However, it's recommended to
-- have at least the same number of bits of the state size.  For
-- example, when using 'skeinMAC' with 'Skein_512_256', it is
-- recommended to have a key with at least 64 bytes (512 bits),
-- which is the state size of 'Skein_512_256' (the first of the
-- two numbers).
type Key = B.ByteString

-- | Class of Skein contexts that may be used for Skein-MAC (all
-- of them).  Included here mostly for documentation purposes,
-- since adding new instances is not safe (functions using
-- 'SkeinMAC' unsurprisingly assume that they are using Skein).
class SkeinMAC skeinCtx where
    -- | Construct a context @skeinCtx@ given a 'Key'.  This
    -- context may be used with the usual 'Hash' interface to
    -- obtain a message authentication code (MAC).
    --
    -- For a simpler interface, see 'skeinMAC' and 'skeinMAC''.
    skeinMACCtx :: Key -> skeinCtx

-- | Calculate the Skein-MAC of a lazy 'L.ByteString' given a
-- 'Key'.  You probably also want to apply 'encode' to get a
-- 'B.ByteString' out of the @digest@.
--
-- This function may be partially applied for increased
-- performance.  Using a partially applied @skeinMAC@ is as fast
-- as using Skein as a cryptographic hash function.  So, instead
-- of
--
-- @
-- let mac1 = skeinMAC key message1
--     mac2 = skeinMAC key message2
--     mac3 = skeinMAC key message3
--     ...
-- @
--
-- write the following code:
--
-- @
-- let calcMAC = skeinMAC key
--     mac1 = calcMAC message1
--     mac2 = calcMAC message2
--     mac3 = calcMAC message3
--     ...
-- @
--
-- This way the key will be processed only once (with
-- 'skeinMACCtx').
skeinMAC :: (SkeinMAC skeinCtx, Hash skeinCtx digest) =>
            Key -> L.ByteString -> digest
skeinMAC k = go
    where
      ctx = skeinMACCtx k
      go  = go' ctx . L.toChunks
      go' ctx' []     = finalize ctx' B.empty
      go' ctx' [x]    = finalize ctx' x
      go' ctx' (x:xs) = go' (updateCtx ctx' x) xs
      -- See the comment below on skeinMAC'.

-- | Same as 'skeinMAC', however using a strict 'B.ByteString'.
-- Should be faster for small 'B.ByteString'@s@.
skeinMAC' :: (SkeinMAC skeinCtx, Hash skeinCtx digest) =>
             Key -> B.ByteString -> digest
skeinMAC' k = go
    where
      ctx = skeinMACCtx k
      go  = finalize ctx
      -- We can just call 'finalize' because of the way our
      -- implementation works.  Basically, we accept ByteString
      -- of any length on both 'updateCtx' and 'finalize'.
      -- Calling just 'finalize' is more efficient.


----------------------------------------------------------------------

-- | Helper function to create 'initialCtx'.
initialCtxSkein :: Storable internalCtx =>
                   CSize
                -> (Ptr internalCtx -> CSize -> IO CInt)
                -> (internalCtx -> externalCtx)
                -> externalCtx
initialCtxSkein bits init_ mkCtx =
    U.unsafePerformIO $
    alloca $ \ctx_ptr -> do
      check $ init_ ctx_ptr bits
      fmap mkCtx $ peek ctx_ptr

-- | Helper function to create 'updateCtxSkein'.
updateCtxSkein :: Storable internalCtx =>
                  (Ptr internalCtx -> Ptr Word8 -> CSize -> IO CInt)
               -> (externalCtx -> internalCtx)
               -> (internalCtx -> externalCtx)
               -> (externalCtx -> B.ByteString -> externalCtx)
updateCtxSkein update unCtx mkCtx = \ctx bs ->
    U.unsafePerformIO $
    BU.unsafeUseAsCStringLen bs $ \(bs_ptr, bs_len) ->
    with (unCtx ctx) $ \ctx_ptr -> do
      check $ update ctx_ptr (castPtr bs_ptr) (fromIntegral bs_len)
      fmap mkCtx $ peek ctx_ptr

-- | Helper function to create 'finalize'.
finalizeSkein :: Storable internalCtx =>
                 Int
              -> (Ptr internalCtx -> Ptr Word8 -> CSize -> IO CInt)
              -> (Ptr internalCtx -> Ptr Word8 -> IO CInt)
              -> (externalCtx -> internalCtx)
              -> (B.ByteString -> hash)
              -> (externalCtx -> B.ByteString -> hash)
finalizeSkein hashLenBytes update final unCtx mkHash = \ctx bs ->
    U.unsafePerformIO $
    with (unCtx ctx) $ \ctx_ptr -> do
      unless (B.null bs) $
        BU.unsafeUseAsCStringLen bs $ \(bs_ptr, bs_len) ->
          check $ update ctx_ptr (castPtr bs_ptr) (fromIntegral bs_len)
      fmap mkHash $ BI.create hashLenBytes $ check . final ctx_ptr . castPtr

-- | Helper function to create 'skeinMACCtx'.
skeinMACCtxSkein :: Storable internalCtx =>
                    CSize
                 -> (Ptr internalCtx -> CSize -> Word64 -> Ptr Word8 -> CSize -> IO CInt)
                 -> (internalCtx -> externalCtx)
                 -> (Key -> externalCtx)
skeinMACCtxSkein bits initExt mkCtx = \key ->
    U.unsafePerformIO $
    BU.unsafeUseAsCStringLen key $ \(key_ptr, key_len) ->
    alloca $ \ctx_ptr -> do
      check $ initExt ctx_ptr bits sKEIN_SEQUENTIAL (castPtr key_ptr) (fromIntegral key_len)
      fmap mkCtx $ peek ctx_ptr



----------------------------------------------------------------------
-- Skein-256
----------------------------------------------------------------------

-- | Context of the Skein-256-128 hash function.
newtype Skein_256_128_Ctx = S_256_128_Ctx {unS_256_128_Ctx :: Skein256Ctx}

-- | Skein-256-128 hash.  You probably want to use 'encode' to
-- obtain a 128-bit (16-byte) 'B.ByteString'.  May be used as a
-- drop-in replacement for MD5.
newtype Skein_256_128 = S_256_128 B.ByteString deriving (Eq, Ord)

instance Serialize Skein_256_128 where
    put (S_256_128 bs) = putByteString bs
    get = fmap S_256_128 $ getByteString 16

instance Hash Skein_256_128_Ctx Skein_256_128 where
    outputLength = Tagged 128
    blockLength  = Tagged 256
    initialCtx   = initialCtxSkein 128 skein256Init S_256_128_Ctx
    updateCtx    = updateCtxSkein skein256Update unS_256_128_Ctx S_256_128_Ctx
    finalize     = finalizeSkein 16 skein256Update skein256Final unS_256_128_Ctx S_256_128

instance SkeinMAC Skein_256_128_Ctx where
    skeinMACCtx = skeinMACCtxSkein 128 skein256InitExt S_256_128_Ctx


-- | Context of the Skein-256-160 hash function.
newtype Skein_256_160_Ctx = S_256_160_Ctx {unS_256_160_Ctx :: Skein256Ctx}

-- | Skein-256-160 hash.  You probably want to use 'encode' to
-- obtain a 160-bit (20-byte) 'B.ByteString'.  May be used as a
-- drop-in replacement for SHA-1.
newtype Skein_256_160 = S_256_160 B.ByteString deriving (Eq, Ord)

instance Serialize Skein_256_160 where
    put (S_256_160 bs) = putByteString bs
    get = fmap S_256_160 $ getByteString 20

instance Hash Skein_256_160_Ctx Skein_256_160 where
    outputLength = Tagged 160
    blockLength  = Tagged 256
    initialCtx   = initialCtxSkein 160 skein256Init S_256_160_Ctx
    updateCtx    = updateCtxSkein skein256Update unS_256_160_Ctx S_256_160_Ctx
    finalize     = finalizeSkein 20 skein256Update skein256Final unS_256_160_Ctx S_256_160

instance SkeinMAC Skein_256_160_Ctx where
    skeinMACCtx = skeinMACCtxSkein 160 skein256InitExt S_256_160_Ctx


-- | Context of the Skein-256-224 hash function.
newtype Skein_256_224_Ctx = S_256_224_Ctx {unS_256_224_Ctx :: Skein256Ctx}

-- | Skein-256-224 hash.  You probably want to use 'encode' to
-- obtain a 224-bit (28-byte) 'B.ByteString'.  May be used as a
-- drop-in replacement for SHA-224.
newtype Skein_256_224 = S_256_224 B.ByteString deriving (Eq, Ord)

instance Serialize Skein_256_224 where
    put (S_256_224 bs) = putByteString bs
    get = fmap S_256_224 $ getByteString 28

instance Hash Skein_256_224_Ctx Skein_256_224 where
    outputLength = Tagged 224
    blockLength  = Tagged 256
    initialCtx   = initialCtxSkein 224 skein256Init S_256_224_Ctx
    updateCtx    = updateCtxSkein skein256Update unS_256_224_Ctx S_256_224_Ctx
    finalize     = finalizeSkein 28 skein256Update skein256Final unS_256_224_Ctx S_256_224

instance SkeinMAC Skein_256_224_Ctx where
    skeinMACCtx = skeinMACCtxSkein 224 skein256InitExt S_256_224_Ctx


-- | Context of the Skein-256-256 hash function.
newtype Skein_256_256_Ctx = S_256_256_Ctx {unS_256_256_Ctx :: Skein256Ctx}

-- | Skein-256-256 hash.  You probably want to use 'encode' to
-- obtain a 256-bit (32-byte) 'B.ByteString'.  Usually it's better
-- to use 'Skein_512_256' (256 bits of output) or 'Skein_512_512'
-- (512 bits of output).
newtype Skein_256_256 = S_256_256 B.ByteString deriving (Eq, Ord)

instance Serialize Skein_256_256 where
    put (S_256_256 bs) = putByteString bs
    get = fmap S_256_256 $ getByteString 32

instance Hash Skein_256_256_Ctx Skein_256_256 where
    outputLength = Tagged 256
    blockLength  = Tagged 256
    initialCtx   = initialCtxSkein 256 skein256Init S_256_256_Ctx
    updateCtx    = updateCtxSkein skein256Update unS_256_256_Ctx S_256_256_Ctx
    finalize     = finalizeSkein 32 skein256Update skein256Final unS_256_256_Ctx S_256_256

instance SkeinMAC Skein_256_256_Ctx where
    skeinMACCtx = skeinMACCtxSkein 256 skein256InitExt S_256_256_Ctx




----------------------------------------------------------------------
-- Skein-512
----------------------------------------------------------------------

-- | Context of the Skein-512-128 hash function.
newtype Skein_512_128_Ctx = S_512_128_Ctx {unS_512_128_Ctx :: Skein512Ctx}

-- | Skein-512-128 hash.  You probably want to use 'encode' to
-- obtain a 128-bit (16-byte) 'B.ByteString'.  May be used as a
-- drop-in replacement for MD5.
newtype Skein_512_128 = S_512_128 B.ByteString deriving (Eq, Ord)

instance Serialize Skein_512_128 where
    put (S_512_128 bs) = putByteString bs
    get = fmap S_512_128 $ getByteString 16

instance Hash Skein_512_128_Ctx Skein_512_128 where
    outputLength = Tagged 128
    blockLength  = Tagged 512
    initialCtx   = initialCtxSkein 128 skein512Init S_512_128_Ctx
    updateCtx    = updateCtxSkein skein512Update unS_512_128_Ctx S_512_128_Ctx
    finalize     = finalizeSkein 16 skein512Update skein512Final unS_512_128_Ctx S_512_128

instance SkeinMAC Skein_512_128_Ctx where
    skeinMACCtx = skeinMACCtxSkein 128 skein512InitExt S_512_128_Ctx

-- | Context of the Skein-512-160 hash function.
newtype Skein_512_160_Ctx = S_512_160_Ctx {unS_512_160_Ctx :: Skein512Ctx}

-- | Skein-512-160 hash.  You probably want to use 'encode' to
-- obtain a 160-bit (20-byte) 'B.ByteString'.  May be used as a
-- drop-in replacement for SHA-1.
newtype Skein_512_160 = S_512_160 B.ByteString deriving (Eq, Ord)

instance Serialize Skein_512_160 where
    put (S_512_160 bs) = putByteString bs
    get = fmap S_512_160 $ getByteString 20

instance Hash Skein_512_160_Ctx Skein_512_160 where
    outputLength = Tagged 160
    blockLength  = Tagged 512
    initialCtx   = initialCtxSkein 160 skein512Init S_512_160_Ctx
    updateCtx    = updateCtxSkein skein512Update unS_512_160_Ctx S_512_160_Ctx
    finalize     = finalizeSkein 20 skein512Update skein512Final unS_512_160_Ctx S_512_160

instance SkeinMAC Skein_512_160_Ctx where
    skeinMACCtx = skeinMACCtxSkein 160 skein512InitExt S_512_160_Ctx


-- | Context of the Skein-512-224 hash function.
newtype Skein_512_224_Ctx = S_512_224_Ctx {unS_512_224_Ctx :: Skein512Ctx}

-- | Skein-512-224 hash.  You probably want to use 'encode' to
-- obtain a 224-bit (28-byte) 'B.ByteString'.  May be used as a drop-in replacement for SHA-224.
newtype Skein_512_224 = S_512_224 B.ByteString deriving (Eq, Ord)

instance Serialize Skein_512_224 where
    put (S_512_224 bs) = putByteString bs
    get = fmap S_512_224 $ getByteString 28

instance Hash Skein_512_224_Ctx Skein_512_224 where
    outputLength = Tagged 224
    blockLength  = Tagged 512
    initialCtx   = initialCtxSkein 224 skein512Init S_512_224_Ctx
    updateCtx    = updateCtxSkein skein512Update unS_512_224_Ctx S_512_224_Ctx
    finalize     = finalizeSkein 28 skein512Update skein512Final unS_512_224_Ctx S_512_224

instance SkeinMAC Skein_512_224_Ctx where
    skeinMACCtx = skeinMACCtxSkein 224 skein512InitExt S_512_224_Ctx


-- | Context of the Skein-512-256 hash function.
newtype Skein_512_256_Ctx = S_512_256_Ctx {unS_512_256_Ctx :: Skein512Ctx}

-- | Skein-512-256 hash.  You probably want to use 'encode' to
-- obtain a 256-bit (32-byte) 'B.ByteString'.
newtype Skein_512_256 = S_512_256 B.ByteString deriving (Eq, Ord)

instance Serialize Skein_512_256 where
    put (S_512_256 bs) = putByteString bs
    get = fmap S_512_256 $ getByteString 32

instance Hash Skein_512_256_Ctx Skein_512_256 where
    outputLength = Tagged 256
    blockLength  = Tagged 512
    initialCtx   = initialCtxSkein 256 skein512Init S_512_256_Ctx
    updateCtx    = updateCtxSkein skein512Update unS_512_256_Ctx S_512_256_Ctx
    finalize     = finalizeSkein 32 skein512Update skein512Final unS_512_256_Ctx S_512_256

instance SkeinMAC Skein_512_256_Ctx where
    skeinMACCtx = skeinMACCtxSkein 256 skein512InitExt S_512_256_Ctx


-- | Context of the Skein-512-384 hash function.
newtype Skein_512_384_Ctx = S_512_384_Ctx {unS_512_384_Ctx :: Skein512Ctx}

-- | Skein-512-384 hash.  You probably want to use 'encode' to
-- obtain a 384-bit (48-byte) 'B.ByteString'.  May be used as a
-- drop-in replacement for SHA-384.
newtype Skein_512_384 = S_512_384 B.ByteString deriving (Eq, Ord)

instance Serialize Skein_512_384 where
    put (S_512_384 bs) = putByteString bs
    get = fmap S_512_384 $ getByteString 48

instance Hash Skein_512_384_Ctx Skein_512_384 where
    outputLength = Tagged 384
    blockLength  = Tagged 512
    initialCtx   = initialCtxSkein 384 skein512Init S_512_384_Ctx
    updateCtx    = updateCtxSkein skein512Update unS_512_384_Ctx S_512_384_Ctx
    finalize     = finalizeSkein 48 skein512Update skein512Final unS_512_384_Ctx S_512_384

instance SkeinMAC Skein_512_384_Ctx where
    skeinMACCtx = skeinMACCtxSkein 384 skein512InitExt S_512_384_Ctx


-- | Context of the Skein-512-512 hash function.
newtype Skein_512_512_Ctx = S_512_512_Ctx {unS_512_512_Ctx :: Skein512Ctx}

-- | Skein-512-512 hash.  You probably want to use 'encode' to
-- obtain a 512-bit (64-byte) 'B.ByteString'.  It's the main Skein
-- hash function.  May be used as a drop-in replacement for
-- SHA-512 as well.
newtype Skein_512_512 = S_512_512 B.ByteString deriving (Eq, Ord)

instance Serialize Skein_512_512 where
    put (S_512_512 bs) = putByteString bs
    get = fmap S_512_512 $ getByteString 64

instance Hash Skein_512_512_Ctx Skein_512_512 where
    outputLength = Tagged 512
    blockLength  = Tagged 512
    initialCtx   = initialCtxSkein 512 skein512Init S_512_512_Ctx
    updateCtx    = updateCtxSkein skein512Update unS_512_512_Ctx S_512_512_Ctx
    finalize     = finalizeSkein 64 skein512Update skein512Final unS_512_512_Ctx S_512_512

instance SkeinMAC Skein_512_512_Ctx where
    skeinMACCtx = skeinMACCtxSkein 512 skein512InitExt S_512_512_Ctx



----------------------------------------------------------------------
-- Skein-1024
----------------------------------------------------------------------

-- | Context of the Skein-1024-384 hash function.
newtype Skein_1024_384_Ctx = S_1024_384_Ctx {unS_1024_384_Ctx :: Skein1024Ctx}

-- | Skein-1024-384 hash.  You probably want to use 'encode' to
-- obtain a 384-bit (48-byte) 'B.ByteString'.  May be used as a
-- drop-in replacement for SHA-384.
newtype Skein_1024_384 = S_1024_384 B.ByteString deriving (Eq, Ord)

instance Serialize Skein_1024_384 where
    put (S_1024_384 bs) = putByteString bs
    get = fmap S_1024_384 $ getByteString 48

instance Hash Skein_1024_384_Ctx Skein_1024_384 where
    outputLength = Tagged 384
    blockLength  = Tagged 1024
    initialCtx   = initialCtxSkein 384 skein1024Init S_1024_384_Ctx
    updateCtx    = updateCtxSkein skein1024Update unS_1024_384_Ctx S_1024_384_Ctx
    finalize     = finalizeSkein 48 skein1024Update skein1024Final unS_1024_384_Ctx S_1024_384

instance SkeinMAC Skein_1024_384_Ctx where
    skeinMACCtx = skeinMACCtxSkein 384 skein1024InitExt S_1024_384_Ctx


-- | Context of the Skein-1024-512 hash function.
newtype Skein_1024_512_Ctx = S_1024_512_Ctx {unS_1024_512_Ctx :: Skein1024Ctx}

-- | Skein-1024-512 hash.  You probably want to use 'encode' to
-- obtain a 512-bit (64-byte) 'B.ByteString'.  May be used as a
-- drop-in replacement for SHA-512.
newtype Skein_1024_512 = S_1024_512 B.ByteString deriving (Eq, Ord)

instance Serialize Skein_1024_512 where
    put (S_1024_512 bs) = putByteString bs
    get = fmap S_1024_512 $ getByteString 64

instance Hash Skein_1024_512_Ctx Skein_1024_512 where
    outputLength = Tagged 512
    blockLength  = Tagged 1024
    initialCtx   = initialCtxSkein 512 skein1024Init S_1024_512_Ctx
    updateCtx    = updateCtxSkein skein1024Update unS_1024_512_Ctx S_1024_512_Ctx
    finalize     = finalizeSkein 64 skein1024Update skein1024Final unS_1024_512_Ctx S_1024_512

instance SkeinMAC Skein_1024_512_Ctx where
    skeinMACCtx = skeinMACCtxSkein 512 skein1024InitExt S_1024_512_Ctx


-- | Context of the Skein-1024-1024 hash function.
newtype Skein_1024_1024_Ctx = S_1024_1024_Ctx {unS_1024_1024_Ctx :: Skein1024Ctx}

-- | Skein-1024-1024 hash.  You probably want to use 'encode' to
-- obtain a 1024-bit (128-byte) 'B.ByteString'.  This is the
-- ultra-conservative variant.  Even if some future attack
-- managed to break Skein-512, it's quite likely that Skein-1024
-- would remain secure.
newtype Skein_1024_1024 = S_1024_1024 B.ByteString deriving (Eq, Ord)

instance Serialize Skein_1024_1024 where
    put (S_1024_1024 bs) = putByteString bs
    get = fmap S_1024_1024 $ getByteString 128

instance Hash Skein_1024_1024_Ctx Skein_1024_1024 where
    outputLength = Tagged 1024
    blockLength  = Tagged 1024
    initialCtx   = initialCtxSkein 1024 skein1024Init S_1024_1024_Ctx
    updateCtx    = updateCtxSkein skein1024Update unS_1024_1024_Ctx S_1024_1024_Ctx
    finalize     = finalizeSkein 128 skein1024Update skein1024Final unS_1024_1024_Ctx S_1024_1024

instance SkeinMAC Skein_1024_1024_Ctx where
    skeinMACCtx = skeinMACCtxSkein 1024 skein1024InitExt S_1024_1024_Ctx
