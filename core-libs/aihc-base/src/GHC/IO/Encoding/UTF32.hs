-- | The UTF-32 encodings, named as in 'GHC.IO.Encoding.UTF8'.
module GHC.IO.Encoding.UTF32
  ( utf32,
    mkUTF32,
    utf32le,
    mkUTF32le,
    utf32be,
    mkUTF32be,
  )
where

import GHC.Base ((++))
import GHC.IO.Encoding.Failure (CodingFailureMode (..), codingFailureModeSuffix)
import GHC.IO.Encoding.Types (TextEncoding)
import GHC.Internal.IO.Encoding.Codec (nameOnlyEncoding)

utf32 :: TextEncoding
utf32 = mkUTF32 ErrorOnCodingFailure

mkUTF32 :: CodingFailureMode -> TextEncoding
mkUTF32 failureMode = nameOnlyEncoding ("UTF-32" ++ codingFailureModeSuffix failureMode)

utf32le :: TextEncoding
utf32le = mkUTF32le ErrorOnCodingFailure

mkUTF32le :: CodingFailureMode -> TextEncoding
mkUTF32le failureMode = nameOnlyEncoding ("UTF-32LE" ++ codingFailureModeSuffix failureMode)

utf32be :: TextEncoding
utf32be = mkUTF32be ErrorOnCodingFailure

mkUTF32be :: CodingFailureMode -> TextEncoding
mkUTF32be failureMode = nameOnlyEncoding ("UTF-32BE" ++ codingFailureModeSuffix failureMode)
