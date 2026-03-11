{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.ByteString.Base16.Internal.Utils
( aix
, reChunk
, runShortST
, runDecodeST
, runDecodeST'
) where


import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Short.Internal
import Data.Primitive.ByteArray
import Data.Text (Text)

import GHC.Exts
import GHC.Word
import GHC.ST (ST(..))


-- | Read 'Word8' index off alphabet addr
--
aix :: Word8 -> Addr# -> Word8
aix w alpha = W8# (indexWord8OffAddr# alpha i)
  where
    !(I# i) = fromIntegral w
{-# INLINE aix #-}

-- | Form a list of chunks, and rechunk the list of bytestrings
-- into length multiples of 2
--
reChunk :: [ByteString] -> [ByteString]
reChunk [] = []
reChunk (c:cs) = case B.length c `divMod` 2 of
    (_, 0) -> c : reChunk cs
    (n, _) -> case B.splitAt (n * 2) c of
      ~(m, q) -> m : cont_ q cs
  where
    cont_ q [] = [q]
    cont_ q (a:as) = case B.splitAt 1 a of
      (x, y) -> let q' = B.append q x
        in if B.length q' == 2
          then
            let as' = if B.null y then as else y:as
            in q' : reChunk as'
          else cont_ q' as

-- | Write a byte array directly to Short bytestring
--
runShortST :: (forall s. ST s ByteArray) -> ShortByteString
runShortST enc = runRW# $ \s0 -> case enc of
  { ST g -> case g s0 of
    { (# _, ByteArray r #) -> SBS r
    }
  }
{-# INLINE runShortST #-}

-- | Used for writing 'ByteArray#'-based encodes
--
runDecodeST
    :: (forall s. ST s (Either Text ByteArray))
    -> Either Text ShortByteString
runDecodeST dec = runRW# $ \s0 -> case dec of
  { ST g -> case g s0 of
    { (# _, e #) -> case e of
      Left t -> Left t
      Right (ByteArray r) -> Right (SBS r)
    }
  }
{-# INLINE runDecodeST #-}

runDecodeST'
    :: (forall s. ST s ByteArray)
    -> ShortByteString
runDecodeST' dec = runRW# $ \s0 -> case dec of
  { ST g -> case g s0 of
      (# _, ByteArray r #) -> SBS r
  }
{-# inline runDecodeST' #-}
