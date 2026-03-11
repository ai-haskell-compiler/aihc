# streamly-bytestring

Library for streamly and bytestring interoperation.

If you are writing code from scratch, please use `Streamly.Data.Array` which is
a generalization of `ByteString` and better integrated with streamly.

This library is to enable interoperation of streamly with existing code that
uses `ByteString`.

The package provides APIs to interconvert between strict `Bytestring` and
streamly `Array Word8` and between lazy `Bytestring` and stream of `Array
Word8`.

The interconversion in the case of strict `Bytestring` and streamly `Array
Word8` has no overhead for GHC allocated memory. For foreign allocator allocated
memory there is a copy involved.

## Usage

This is a dumb program that counts the number of bytes in a file.

```haskell
import qualified Streamly.Data.Stream as S
import qualified Streamly.Data.Fold as FL

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Streamly.External.ByteString as Strict
import qualified Streamly.External.ByteString.Lazy as Lazy

import System.IO (FilePath)

strictByteStringSize :: BS.ByteString -> IO Int
strictByteStringSize bs = S.fold FL.length $ S.unfold Strict.reader bs

lazyByteStringSize :: BSL.ByteString -> IO Int
lazyByteStringSize bsl =
    S.fold (FL.foldl' (+) 0)
        $ S.mapM strictByteStringSize
        $ fmap Strict.fromArray
        $ Lazy.toChunks bsl

fileSize :: FilePath -> IO Int
fileSize path = do
    bsl <- BSL.readFile path
    lazyByteStringSize bsl
```
