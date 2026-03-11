{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.C.ByteArray where

import Foreign.Ptr qualified as Ptr
import Foreign.Marshal.Alloc qualified as Alloc
import Foreign.C.String
import Data.Word
import Data.ByteString qualified as BS

type B = (Ptr.Ptr Word8, Int)

malloc :: Int -> IO B
malloc n = (, n) <$> Alloc.mallocBytes n

alloca :: Int -> (B -> IO a) -> IO a
alloca n a = Alloc.allocaBytes n $ a . (, n)

free :: B -> IO ()
free = Alloc.free . fst

splitAt :: Int -> B -> Maybe (B, B)
splitAt n (p, ln)
	| n < 0 || ln < n = Nothing
	| otherwise = Just ((p, n), (p `Ptr.plusPtr` n, ln - n))

fromCStringLen :: CStringLen -> B
fromCStringLen (p, n) = (Ptr.castPtr p, n)

toCStringLen :: B -> CStringLen
toCStringLen (p, n) = (Ptr.castPtr p, n)

packToByteString :: B -> IO BS.ByteString
packToByteString = BS.packCStringLen . toCStringLen

useAsFromByteString :: BS.ByteString -> (B -> IO a) -> IO a
useAsFromByteString bs f = BS.useAsCStringLen bs $ f . fromCStringLen
