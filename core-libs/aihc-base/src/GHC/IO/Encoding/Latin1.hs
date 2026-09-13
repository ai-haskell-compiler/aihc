-- | The single-byte encodings, named as in 'GHC.IO.Encoding.UTF8'.
module GHC.IO.Encoding.Latin1
  ( latin1,
    mkLatin1,
    latin1_checked,
    mkLatin1_checked,
    ascii,
    mkAscii,
  )
where

import GHC.Base ((++))
import GHC.IO.Encoding.Failure (CodingFailureMode (..), codingFailureModeSuffix)
import GHC.IO.Encoding.Types (TextEncoding)
import GHC.Internal.IO.Encoding.Codec (nameOnlyEncoding)

latin1 :: TextEncoding
latin1 = mkLatin1 ErrorOnCodingFailure

mkLatin1 :: CodingFailureMode -> TextEncoding
mkLatin1 failureMode = nameOnlyEncoding ("ISO-8859-1" ++ codingFailureModeSuffix failureMode)

-- | Latin-1 that rejects the characters above U+00FF instead of
-- truncating them.
latin1_checked :: TextEncoding
latin1_checked = mkLatin1_checked ErrorOnCodingFailure

mkLatin1_checked :: CodingFailureMode -> TextEncoding
mkLatin1_checked failureMode = nameOnlyEncoding ("ISO-8859-1(checked)" ++ codingFailureModeSuffix failureMode)

ascii :: TextEncoding
ascii = mkAscii ErrorOnCodingFailure

mkAscii :: CodingFailureMode -> TextEncoding
mkAscii failureMode = nameOnlyEncoding ("ASCII" ++ codingFailureModeSuffix failureMode)
