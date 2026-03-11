{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.ByteString
  ( chunkedBy
  , ToByteString(..)
  , ToByteStrings(..)
  , mmap
  , padded
  , rechunk
  , rechunkPadded
  , resegment
  , resegmentPadded
  , hGetContentsChunkedBy
  ) where

import Control.Monad.ST
import Data.Word
import Foreign.ForeignPtr

import qualified Control.Monad.ST.Unsafe       as ST
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Internal      as BSI
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Internal as LBSI
import qualified Data.Vector.Storable          as DVS
import qualified Data.Vector.Storable.Mutable  as DVSM
import qualified System.IO                     as IO
import qualified System.IO.MMap                as IO
import qualified System.IO.Unsafe              as IO

class ToByteString a where
  toByteString :: a -> BS.ByteString

instance ToByteString BS.ByteString where
  toByteString = id
  {-# INLINE toByteString #-}

instance ToByteString (DVS.Vector Word8) where
  toByteString v = case DVS.unsafeToForeignPtr v of
    (fptr, start, offset) -> BSI.fromForeignPtr fptr start offset
  {-# INLINE toByteString #-}

instance ToByteString (DVS.Vector Word16) where
  toByteString v = case DVS.unsafeToForeignPtr (DVS.unsafeCast v :: DVS.Vector Word8) of
    (fptr, start, offset) -> BSI.fromForeignPtr fptr start offset
  {-# INLINE toByteString #-}

instance ToByteString (DVS.Vector Word32) where
  toByteString v = case DVS.unsafeToForeignPtr (DVS.unsafeCast v :: DVS.Vector Word8) of
    (fptr, start, offset) -> BSI.fromForeignPtr fptr start offset
  {-# INLINE toByteString #-}

instance ToByteString (DVS.Vector Word64) where
  toByteString v = case DVS.unsafeToForeignPtr (DVS.unsafeCast v :: DVS.Vector Word8) of
    (fptr, start, offset) -> BSI.fromForeignPtr fptr start offset
  {-# INLINE toByteString #-}

instance ToByteString [Word64] where
  toByteString = toByteString . DVS.fromList
  {-# INLINE toByteString #-}

class ToByteStrings a where
  toByteStrings :: a -> [BS.ByteString]

instance ToByteStrings [BS.ByteString] where
  toByteStrings = id
  {-# INLINE toByteStrings #-}

instance ToByteStrings LBS.ByteString where
  toByteStrings = LBS.toChunks
  {-# INLINE toByteStrings #-}

instance ToByteStrings BS.ByteString where
  toByteStrings = (:[])
  {-# INLINE toByteStrings #-}

instance ToByteStrings (DVS.Vector Word8) where
  toByteStrings = (:[]) . toByteString
  {-# INLINE toByteStrings #-}

instance ToByteStrings (DVS.Vector Word16) where
  toByteStrings = (:[]) . toByteString
  {-# INLINE toByteStrings #-}

instance ToByteStrings (DVS.Vector Word32) where
  toByteStrings = (:[]) . toByteString
  {-# INLINE toByteStrings #-}

instance ToByteStrings (DVS.Vector Word64) where
  toByteStrings = (:[]) . toByteString
  {-# INLINE toByteStrings #-}

instance ToByteStrings [Word64] where
  toByteStrings ws = toByteString <$> go ws
    where go :: [Word64] -> [DVS.Vector Word64]
          go us = DVS.createT (goST us)
          goST :: [Word64] -> ST s [DVSM.MVector s Word64]
          goST us = do
            mv <- DVSM.new defaultChunkSize
            (i, ts) <- writeWords 0 defaultChunkSize us mv
            mvs <- ST.unsafeInterleaveST (goST ts)
            if null ts
              then return [DVSM.take i mv]
              else return (DVSM.take i mv:mvs)
          writeWords :: Int -> Int -> [Word64] -> DVSM.MVector s Word64 -> ST s (Int, [Word64])
          writeWords i n us mv = if i < n
            then case us of
              t:ts -> do
                DVSM.write mv i t
                writeWords (i + 1) n ts mv
              [] -> return (i, us)
            else return (i, us)
          defaultChunkSize = (LBSI.defaultChunkSize `div` 64) * 8
  {-# INLINE toByteStrings #-}

instance ToByteStrings [DVS.Vector Word8] where
  toByteStrings = (toByteString <$>)
  {-# INLINE toByteStrings #-}

instance ToByteStrings [DVS.Vector Word16] where
  toByteStrings = (toByteString <$>)
  {-# INLINE toByteStrings #-}

instance ToByteStrings [DVS.Vector Word32] where
  toByteStrings = (toByteString <$>)
  {-# INLINE toByteStrings #-}

instance ToByteStrings [DVS.Vector Word64] where
  toByteStrings = (toByteString <$>)
  {-# INLINE toByteStrings #-}

-- | Chunk a @bs into list of smaller byte strings of no more than @n elements
chunkedBy :: Int -> BS.ByteString -> [BS.ByteString]
chunkedBy n bs = if BS.length bs == 0
  then []
  else case BS.splitAt n bs of
    (as, zs) -> as : chunkedBy n zs
{-# INLINE chunkedBy #-}

rechunk :: Int -> [BS.ByteString] -> [BS.ByteString]
rechunk size = go
  where go (bs:bss) = let bsLen = BS.length bs in if bsLen > 0
          then if bsLen < size
            then let bsNeed = size - bsLen in case bss of
              (cs:css) -> case BS.length cs of
                csLen | csLen >  bsNeed -> (bs <> BS.take bsNeed cs ):go (BS.drop bsNeed cs:css)
                csLen | csLen == bsNeed -> (bs <> cs                ):go                    css
                _                       ->                            go ((bs <> cs)       :css)
              [] -> [bs]
            else if size == bsLen
              then bs:go bss
              else BS.take size bs:go (BS.drop size bs:bss)
          else go bss
        go [] = []

rechunkPadded :: Int -> [BS.ByteString] -> [BS.ByteString]
rechunkPadded size = go
  where go (bs:bss) = let bsLen = BS.length bs in if bsLen > 0
          then if bsLen < size
            then let bsNeed = size - bsLen in case bss of
              (cs:css) -> case BS.length cs of
                csLen | csLen >  bsNeed -> (bs <> BS.take bsNeed cs ):go (BS.drop bsNeed cs:css)
                csLen | csLen == bsNeed -> (bs <> cs                ):go                    css
                _                       ->                            go ((bs <> cs)       :css)
              [] -> [bs <> BS.replicate bsNeed 0]
            else if size == bsLen
              then bs:go bss
              else BS.take size bs:go (BS.drop size bs:bss)
          else go bss
        go [] = []

resegment :: Int -> [BS.ByteString] -> [BS.ByteString]
resegment size = go
  where go (bs:bss) = let bsLen = BS.length bs in if bsLen > 0
          then if bsLen < size
            then case bss of
              (cs:css) -> let bsNeed = size - bsLen; csLen = BS.length cs in if
                | csLen >  bsNeed -> (bs <> BS.take bsNeed cs ):go (BS.drop bsNeed cs:css)
                | csLen == bsNeed -> (bs <> cs                ):go                    css
                | otherwise       ->                            go ((bs <> cs)       :css)
              [] -> [bs]
            else let bsCroppedLen = (bsLen `div` size) * size in if bsCroppedLen == bsLen
              then bs:go bss
              else BS.take bsCroppedLen bs:go (BS.drop bsCroppedLen bs:bss)
          else go bss
        go [] = []

resegmentPadded :: Int -> [BS.ByteString] -> [BS.ByteString]
resegmentPadded size = go
  where go (bs:bss) = let bsLen = BS.length bs in if bsLen > 0
          then if bsLen < size
            then let bsNeed = size - bsLen in case bss of
              (cs:css) -> case BS.length cs of
                csLen | csLen >  bsNeed -> (bs <> BS.take bsNeed cs ):go (BS.drop bsNeed cs:css)
                csLen | csLen == bsNeed -> (bs <> cs                ):go                    css
                _                       ->                            go ((bs <> cs)       :css)
              [] -> [bs <> BS.replicate bsNeed 0]
            else case (bsLen `div` size) * size of
              bsCroppedLen -> if bsCroppedLen == bsLen
                then bs:go bss
                else BS.take bsCroppedLen bs:go (BS.drop bsCroppedLen bs:bss)
          else go bss
        go [] = []

hGetContentsChunkedBy :: Int -> IO.Handle -> IO [BS.ByteString]
hGetContentsChunkedBy k h = lazyRead
  where lazyRead = IO.unsafeInterleaveIO loop
        loop = do
          c <- BSI.createAndTrim k $ \p -> IO.hGetBuf h p k
          if BS.null c
            then IO.hClose h >> return []
            else (c:) <$> lazyRead

mmap :: FilePath -> IO BS.ByteString
mmap filepath = do
  (fptr :: ForeignPtr Word8, offset, size) <- IO.mmapFileForeignPtr filepath IO.ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  return bs

padded :: Int -> BS.ByteString -> BS.ByteString
padded n v = v <> BS.replicate ((n - BS.length v) `max` 0) 0
{-# INLINE padded #-}
