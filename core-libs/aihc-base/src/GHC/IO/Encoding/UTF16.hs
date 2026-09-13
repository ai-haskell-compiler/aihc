-- | The UTF-16 encodings, named as in 'GHC.IO.Encoding.UTF8'.
module GHC.IO.Encoding.UTF16
  ( utf16,
    mkUTF16,
    utf16le,
    mkUTF16le,
    utf16be,
    mkUTF16be,
  )
where

import GHC.Base ((++))
import GHC.IO.Encoding.Failure (CodingFailureMode (..), codingFailureModeSuffix)
import GHC.IO.Encoding.Types (TextEncoding)
import GHC.Internal.IO.Encoding.Codec (nameOnlyEncoding)

utf16 :: TextEncoding
utf16 = mkUTF16 ErrorOnCodingFailure

mkUTF16 :: CodingFailureMode -> TextEncoding
mkUTF16 failureMode = nameOnlyEncoding ("UTF-16" ++ codingFailureModeSuffix failureMode)

utf16le :: TextEncoding
utf16le = mkUTF16le ErrorOnCodingFailure

mkUTF16le :: CodingFailureMode -> TextEncoding
mkUTF16le failureMode = nameOnlyEncoding ("UTF-16LE" ++ codingFailureModeSuffix failureMode)

utf16be :: TextEncoding
utf16be = mkUTF16be ErrorOnCodingFailure

mkUTF16be :: CodingFailureMode -> TextEncoding
mkUTF16be failureMode = nameOnlyEncoding ("UTF-16BE" ++ codingFailureModeSuffix failureMode)
