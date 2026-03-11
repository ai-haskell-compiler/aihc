module PtrPoker.ByteString where

import Data.ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.Extra as Builder
import qualified Data.ByteString.Builder.Scientific as ScientificBuilder
import Data.ByteString.Internal
import qualified Data.ByteString.Lazy as Lazy
import qualified PtrPoker.Compat.Text as TextCompat
import qualified PtrPoker.Ffi as Ffi
import PtrPoker.Prelude hiding (empty)

builderWithStrategy :: Builder.AllocationStrategy -> Builder.Builder -> ByteString
builderWithStrategy strategy builder =
  builder
    & Builder.toLazyByteStringWith strategy Lazy.empty
    & Lazy.toStrict

scientific :: Scientific -> ByteString
scientific sci =
  sci
    & ScientificBuilder.scientificBuilder
    & builderWithStrategy (Builder.untrimmedStrategy 128 128)

double :: Double -> ByteString
double dbl =
  unsafeCreateUptoN
    25
    ( \ptr ->
        Ffi.pokeDouble dbl ptr
          & fmap fromIntegral
    )

unsafeCreateDownToN :: Int -> (Ptr Word8 -> IO Int) -> ByteString
unsafeCreateDownToN allocSize populate =
  unsafeDupablePerformIO $ do
    fp <- mallocByteString allocSize
    actualSize <- withForeignPtr fp (\p -> populate (plusPtr p (pred allocSize)))
    return $! PS fp (allocSize - actualSize) actualSize

{-# INLINE textUtf8 #-}
textUtf8 :: Text -> ByteString
textUtf8 = TextCompat.encodeInUtf8
