{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module HaskellWorks.Data.Vector.AsVector8s
  ( AsVector8s(..)
  ) where

import Control.Monad.ST
import Data.Word
import Foreign.ForeignPtr

import qualified Data.ByteString              as BS
import qualified Data.ByteString.Internal     as BS
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Vector.Storable         as DVS
import qualified Data.Vector.Storable.Mutable as DVSM

#if !MIN_VERSION_base(4,13,0)
import Control.Applicative ((<$>)) -- Fix warning in ghc >= 9.2
#endif

class AsVector8s a where
  -- | Represent the value as a list of Vector of 'n' Word8 chunks.  The last chunk will
  -- also be of the specified chunk size filled with trailing zeros.
  asVector8s :: Int -> a -> [DVS.Vector Word8]

instance AsVector8s LBS.ByteString where
  asVector8s n = asVector8s n . LBS.toChunks
  {-# INLINE asVector8s #-}

instance AsVector8s [BS.ByteString] where
  asVector8s = bytestringsToVectors
  {-# INLINE asVector8s #-}

bytestringsToVectors :: Int -> [BS.ByteString] -> [DVS.Vector Word8]
bytestringsToVectors n = go
  where go :: [BS.ByteString] -> [DVS.Vector Word8]
        go bs = case DVS.createT (buildOneVector n bs) of
          (cs, ws) -> if DVS.length ws > 0
            then ws:go cs
            else []
{-# INLINE bytestringsToVectors #-}

buildOneVector :: forall s. Int -> [BS.ByteString] -> ST s ([BS.ByteString], DVS.MVector s Word8)
buildOneVector n ss = case dropWhile ((== 0) . BS.length) ss of
  [] -> ([],) <$> DVSM.new 0
  cs -> do
    v8 <- DVSM.unsafeNew n
    rs  <- go cs v8
    return (rs, v8)
  where go :: [BS.ByteString] -> DVSM.MVector s Word8 -> ST s [BS.ByteString]
        go ts v = if DVSM.length v > 0
          then case ts of
            (u:us) -> if BS.length u <= DVSM.length v
              then case DVSM.splitAt (BS.length u) v of
                (va, vb) -> do
                  DVSM.copy va (byteStringToVector8 u)
                  go us vb
              else case BS.splitAt (DVSM.length v) u of
                (ua, ub) -> do
                  DVSM.copy v (byteStringToVector8 ua)
                  return (ub:us)
            [] -> do
              DVSM.set v 0
              return []
          else return ts
        {-# INLINE go #-}
{-# INLINE buildOneVector #-}

byteStringToVector8 :: BS.ByteString -> DVSM.MVector s Word8
byteStringToVector8 bs = case BS.toForeignPtr bs of
  (fptr, off, len) -> DVSM.unsafeFromForeignPtr (castForeignPtr fptr) off len
{-# INLINE byteStringToVector8 #-}
