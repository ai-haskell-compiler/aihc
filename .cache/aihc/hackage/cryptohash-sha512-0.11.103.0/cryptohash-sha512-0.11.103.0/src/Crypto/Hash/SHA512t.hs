{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Crypto.Hash.SHA512t
-- License     : BSD-style
-- Maintainer  : Herbert Valerio Riedel <hvr@gnu.org>
-- Stability   : stable
-- Portability : unknown
--
-- A module containing <https://en.wikipedia.org/wiki/SHA-2 SHA-512/t> bindings
--
-- @since 0.11.102.0
module Crypto.Hash.SHA512t
    (

    -- * Incremental API
    --
    -- | This API is based on 4 different functions, similar to the
    -- lowlevel operations of a typical hash:
    --
    --  - 'init': create a new hash context
    --  - 'update': update non-destructively a new hash context with a strict bytestring
    --  - 'updates': same as update, except that it takes a list of strict bytestrings
    --  - 'finalize': finalize the context and returns a digest bytestring.
    --
    -- all those operations are completely pure, and instead of
    -- changing the context as usual in others language, it
    -- re-allocates a new context each time.
    --
    -- Example:
    --
    -- > import qualified Data.ByteString
    -- > import qualified Crypto.Hash.SHA512t as SHA512t
    -- >
    -- > main = print digest
    -- >   where
    -- >     digest = SHA512t.finalize ctx
    -- >     ctx    = foldl SHA512t.update ctx0 (map Data.ByteString.pack [ [1,2,3], [4,5,6] ])
    -- >     ctx0   = SHA512t.init 224

      Ctx(..)
    , init     -- :: Int -> Ctx
    , update   -- :: Ctx -> ByteString -> Ctx
    , updates  -- :: Ctx -> [ByteString] -> Ctx
    , finalize -- :: Ctx -> ByteString
    , finalizeAndLength -- :: Ctx -> (ByteString,Word64)
    , start    -- :: Int -> ByteString -> Ctx
    , startlazy -- :: Int -> L.ByteString -> Ctx


    -- * Single Pass API
    --
    -- | This API use the incremental API under the hood to provide
    -- the common all-in-one operations to create digests out of a
    -- 'ByteString' and lazy 'L.ByteString'.
    --
    --  - 'hash': create a digest ('init' + 'update' + 'finalize') from a strict 'ByteString'
    --  - 'hashlazy': create a digest ('init' + 'update' + 'finalize') from a lazy 'L.ByteString'
    --
    -- Example:
    --
    -- > import qualified Data.ByteString
    -- > import qualified Crypto.Hash.SHA512t as SHA512t
    -- >
    -- > main = print $ SHA512t.hash 224 (Data.ByteString.pack [0..255])
    --
    -- __NOTE__: The returned digest is a binary 'ByteString'. For
    -- converting to a base16/hex encoded digest the
    -- <https://hackage.haskell.org/package/base16-bytestring base16-bytestring>
    -- package is recommended.

    , hash     -- :: Int -> ByteString -> ByteString
    , hashlazy -- :: Int -> L.ByteString -> ByteString
    , hashlazyAndLength -- :: Int -> L.ByteString -> (ByteString,Word64)

    -- ** HMAC-SHA-512/t
    --
    -- | <https://tools.ietf.org/html/rfc2104 RFC2104>-compatible
    -- <https://en.wikipedia.org/wiki/HMAC HMAC>-SHA-512/t digests

    , hmac     -- :: Int -> ByteString -> ByteString -> ByteString
    , hmaclazy -- :: Int -> ByteString -> L.ByteString -> ByteString
    , hmaclazyAndLength -- :: Int -> ByteString -> L.ByteString -> (ByteString,Word64)
    ) where

import Prelude hiding (init)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.ByteString.Internal (create, toForeignPtr, memcpy, mallocByteString)
import Data.Bits (xor)
import Data.Word
import System.IO.Unsafe (unsafeDupablePerformIO)

import Compat (constructBS)
import Crypto.Hash.SHA512.FFI hiding (Ctx(..))
import qualified Crypto.Hash.SHA512.FFI as FFI (Ctx(..))

-- | SHA-512/t Context
--
-- This extends the non-truncated SHA-512 Context (see 'FFI.Ctx')
-- with the value of the /t/ parameter which must be within the
-- range @[1..511]@ excluding the value @384@ as per FIPS-180-4
-- section 5.3.6.
data Ctx = Ctx !Int !FFI.Ctx
  deriving (Eq)

-- | perform IO for hashes that do allocation and ffi.
-- unsafeDupablePerformIO is used when possible as the
-- computation is pure and the output is directly linked
-- to the input. we also do not modify anything after it has
-- been returned to the user.
unsafeDoIO :: IO a -> a
unsafeDoIO = unsafeDupablePerformIO

{-# INLINE digestSize #-}
digestSize :: Int -> Int
digestSize t = (t+7) `div` 8

{-# INLINE sizeCtx #-}
sizeCtx :: Int
sizeCtx = 208

{-# INLINE withByteStringPtr #-}
withByteStringPtr :: ByteString -> (Ptr Word8 -> IO a) -> IO a
withByteStringPtr b f =
    withForeignPtr fptr $ \ptr -> f (ptr `plusPtr` off)
    where (fptr, off, _) = toForeignPtr b

{-# INLINE create' #-}
-- | Variant of 'create' which allows to return an argument
create' :: Int -> (Ptr Word8 -> IO a) -> IO (ByteString,a)
create' l f = do
    fp <- mallocByteString l
    x <- withForeignPtr fp $ \p -> f p
    let bs = constructBS fp l
    return $! x `seq` bs `seq` (bs,x)

copyCtx :: Ptr FFI.Ctx -> Ptr FFI.Ctx -> IO ()
copyCtx dst src = memcpy (castPtr dst) (castPtr src) (fromIntegral sizeCtx)

withCtxCopy :: Ctx -> (Ptr FFI.Ctx -> IO ()) -> IO Ctx
withCtxCopy (Ctx tbits (FFI.Ctx ctxB)) f = (Ctx tbits . FFI.Ctx) `fmap` createCtx
  where
    createCtx = create sizeCtx $ \dstPtr ->
                withByteStringPtr ctxB $ \srcPtr -> do
                    copyCtx (castPtr dstPtr) (castPtr srcPtr)
                    f (castPtr dstPtr)

withCtxThrow :: Ctx -> (Ptr FFI.Ctx -> IO a) -> IO a
withCtxThrow (Ctx _ (FFI.Ctx ctxB)) f =
    allocaBytes sizeCtx $ \dstPtr ->
    withByteStringPtr ctxB $ \srcPtr -> do
        copyCtx (castPtr dstPtr) (castPtr srcPtr)
        f (castPtr dstPtr)

withCtxNew :: Int -> (Ptr FFI.Ctx -> IO ()) -> IO Ctx
withCtxNew t f = (Ctx t . FFI.Ctx) `fmap` create sizeCtx (f . castPtr)

withCtxNewThrow :: (Ptr FFI.Ctx -> IO a) -> IO a
withCtxNewThrow f = allocaBytes sizeCtx (f . castPtr)

-- 'safe' call overhead neglible for 4KiB and more
c_sha512_update :: Ptr FFI.Ctx -> Ptr Word8 -> CSize -> IO ()
c_sha512_update pctx pbuf sz
  | sz < 4096 = c_sha512_update_unsafe pctx pbuf sz
  | otherwise = c_sha512_update_safe   pctx pbuf sz

-- 'safe' call overhead neglible for 4KiB and more
c_sha512t_hash :: Word16 -> Ptr Word8 -> CSize -> Ptr Word8 -> IO ()
c_sha512t_hash t pbuf sz pout
  | sz < 4096 = c_sha512t_hash_unsafe pbuf sz pout t
  | otherwise = c_sha512t_hash_safe   pbuf sz pout t

updateInternalIO :: Ptr FFI.Ctx -> ByteString -> IO ()
updateInternalIO ptr d =
    unsafeUseAsCStringLen d (\(cs, len) -> c_sha512_update ptr (castPtr cs) (fromIntegral len))

finalizeInternalIO :: Int -> Ptr FFI.Ctx -> IO ByteString
finalizeInternalIO t ptr = create (digestSize t) (c_sha512t_finalize ptr (fromIntegral t))

finalizeInternalIO' :: Int -> Ptr FFI.Ctx -> IO (ByteString,Word64)
finalizeInternalIO' t ptr = create' (digestSize t) (c_sha512t_finalize_len ptr (fromIntegral t))

{-# NOINLINE init #-}
-- | create a new hash context
init :: Int -> Ctx
init 224 = unsafeDoIO $ withCtxNew 224 $ flip c_sha512t_init 224
init 256 = unsafeDoIO $ withCtxNew 256 $ flip c_sha512t_init 256
init t   = unsafeDoIO $ withCtxNew t $ flip c_sha512t_init t'
  where
    !t' = tFromInt t -- will 'error' for invalid values

tFromInt :: Int -> Word16
tFromInt t
  | isValidT t = fromIntegral t
  | otherwise  = error ("invalid SHA512/t (with t=" ++ show t ++ ") requested")

-- see FIPS 180-4 section 5.3.6.
isValidT :: Int -> Bool
isValidT t = t > 0 && t < 512 && t /= 384

validCtx :: Ctx -> Bool
validCtx (Ctx t (FFI.Ctx b)) = isValidT t && B.length b == sizeCtx

{-# NOINLINE update #-}
-- | update a context with a bytestring
update :: Ctx -> ByteString -> Ctx
update ctx d
  | validCtx ctx = unsafeDoIO $ withCtxCopy ctx $ \ptr -> updateInternalIO ptr d
  | otherwise    = error "SHA512t.update: invalid Ctx"

{-# NOINLINE updates #-}
-- | updates a context with multiple bytestrings
updates :: Ctx -> [ByteString] -> Ctx
updates ctx d
  | validCtx ctx = unsafeDoIO $ withCtxCopy ctx $ \ptr -> mapM_ (updateInternalIO ptr) d
  | otherwise    = error "SHA512t.updates: invalid Ctx"

{-# NOINLINE finalize #-}
-- | finalize the context into a digest bytestring (/t/ bits)
finalize :: Ctx -> ByteString
finalize ctx@(Ctx t _)
  | validCtx ctx = unsafeDoIO $ withCtxThrow ctx (finalizeInternalIO t)
  | otherwise    = error "SHA512t.finalize: invalid Ctx"

{-# NOINLINE finalizeAndLength #-}
-- | Variant of 'finalize' also returning length of hashed content
finalizeAndLength :: Ctx -> (ByteString,Word64)
finalizeAndLength ctx@(Ctx t _)
  | validCtx ctx = unsafeDoIO $ withCtxThrow ctx (finalizeInternalIO' t)
  | otherwise    = error "SHA512t.finalize: invalid Ctx"

{-# NOINLINE hash #-}
-- | hash a strict bytestring into a digest bytestring (/t/ bits)
hash :: Int -> ByteString -> ByteString
hash t d = unsafeDoIO $ unsafeUseAsCStringLen d $ \(cs, len) ->
    create (digestSize t) (c_sha512t_hash t' (castPtr cs) (fromIntegral len))
  where
    !t' = tFromInt t

{-# NOINLINE start #-}
-- | hash a strict bytestring into a Ctx
start :: Int -> ByteString -> Ctx
start t d = unsafeDoIO $ withCtxNew t $ \ptr -> do
    c_sha512t_init ptr t' >> updateInternalIO ptr d
  where
    !t' = tFromInt t

{-# NOINLINE hashlazy #-}
-- | hash a lazy bytestring into a digest bytestring (/t/ bits)
hashlazy :: Int -> L.ByteString -> ByteString
hashlazy t l = unsafeDoIO $ withCtxNewThrow $ \ptr -> do
    c_sha512t_init ptr t' >> mapM_ (updateInternalIO ptr) (L.toChunks l) >> finalizeInternalIO t ptr
  where
    !t' = tFromInt t

{-# NOINLINE hashlazyAndLength #-}
-- | Variant of 'hashlazy' which simultaneously computes the hash and length of a lazy bytestring.
hashlazyAndLength :: Int -> L.ByteString -> (ByteString,Word64)
hashlazyAndLength t l = unsafeDoIO $ withCtxNewThrow $ \ptr ->
    c_sha512t_init ptr t' >> mapM_ (updateInternalIO ptr) (L.toChunks l) >> finalizeInternalIO' t ptr
  where
    !t' = tFromInt t

{-# NOINLINE startlazy #-}
-- | hash a lazy bytestring into a Ctx
startlazy :: Int -> L.ByteString -> Ctx
startlazy t l = unsafeDoIO $ withCtxNew t $ \ptr -> do
    c_sha512t_init ptr t' >> mapM_ (updateInternalIO ptr) (L.toChunks l)
  where
    t' = tFromInt t

{-# NOINLINE hmac #-}
-- | Compute /t/-bit <https://tools.ietf.org/html/rfc2104 RFC2104>-compatible
-- HMAC-SHA-512/t digest for a strict bytestring message
hmac :: Int        -- ^ digest length /t/ in bits
     -> ByteString -- ^ secret
     -> ByteString -- ^ message
     -> ByteString
hmac t secret msg = hash t $ B.append opad (hash t $ B.append ipad msg)
  where
    opad = B.map (xor 0x5c) k'
    ipad = B.map (xor 0x36) k'

    k'  = B.append kt pad
    kt  = if B.length secret > 128 then hash t secret else secret
    pad = B.replicate (128 - B.length kt) 0


{-# NOINLINE hmaclazy #-}
-- | Compute4 /t/-bit <https://tools.ietf.org/html/rfc2104 RFC2104>-compatible
-- HMAC-SHA-512/t digest for a lazy bytestring message
hmaclazy :: Int          -- ^ digest length /t/ in bits
         -> ByteString   -- ^ secret
         -> L.ByteString -- ^ message
         -> ByteString
hmaclazy t secret msg = hash t $ B.append opad (hashlazy t $ L.append ipad msg)
  where
    opad = B.map (xor 0x5c) k'
    ipad = L.fromChunks [B.map (xor 0x36) k']

    k'  = B.append kt pad
    kt  = if B.length secret > 128 then hash t secret else secret
    pad = B.replicate (128 - B.length kt) 0

-- | Variant of 'hmaclazy' which also returns length of message
hmaclazyAndLength :: Int          -- ^ digest length /t/ in bits
                  -> ByteString   -- ^ secret
                  -> L.ByteString -- ^ message
                  -> (ByteString,Word64) -- ^ digest (/t/ bits) and length of message
hmaclazyAndLength t secret msg =
    (hash t (B.append opad htmp), sz' - fromIntegral ipadLen)
  where
    (htmp, sz') = hashlazyAndLength t (L.append ipad msg)

    opad = B.map (xor 0x5c) k'
    ipad = L.fromChunks [B.map (xor 0x36) k']
    ipadLen = B.length k'

    k'  = B.append kt pad
    kt  = if B.length secret > 128 then hash t secret else secret
    pad = B.replicate (128 - B.length kt) 0
