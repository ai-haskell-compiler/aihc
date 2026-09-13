-- | The UTF-8 encodings. All text goes through UTF-8, so an encoding is
-- only its name.
module GHC.IO.Encoding.UTF8
  ( utf8,
    mkUTF8,
    utf8_bom,
    mkUTF8_bom,
  )
where

import GHC.Base ((++))
import GHC.IO.Encoding.Failure (CodingFailureMode (..), codingFailureModeSuffix)
import GHC.IO.Encoding.Types (TextEncoding (..))

utf8 :: TextEncoding
utf8 = mkUTF8 ErrorOnCodingFailure

mkUTF8 :: CodingFailureMode -> TextEncoding
mkUTF8 failureMode = TextEncoding ("UTF-8" ++ codingFailureModeSuffix failureMode)

utf8_bom :: TextEncoding
utf8_bom = mkUTF8_bom ErrorOnCodingFailure

mkUTF8_bom :: CodingFailureMode -> TextEncoding
mkUTF8_bom failureMode = TextEncoding ("UTF-8BOM" ++ codingFailureModeSuffix failureMode)
