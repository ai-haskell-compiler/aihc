-- Each mk function keeps its argument: the arity pass reads the arity from
-- the body, and an eta-reduced alias is not inlined. Thus the default
-- failure mode can select the name with no suffix.
{-# HLINT ignore "Eta reduce" #-}

-- | The UTF-8 encodings. All text goes through UTF-8, so an encoding is
-- only its name.
module GHC.IO.Encoding.UTF8
  ( utf8,
    mkUTF8,
    utf8_bom,
    mkUTF8_bom,
  )
where

import GHC.IO.Encoding.Failure (CodingFailureMode (..))
import GHC.IO.Encoding.Types (TextEncoding)
import GHC.Internal.IO.Encoding.FailureMode (failureModeEncoding)

utf8 :: TextEncoding
utf8 = mkUTF8 ErrorOnCodingFailure

mkUTF8 :: CodingFailureMode -> TextEncoding
mkUTF8 failureMode = failureModeEncoding "UTF-8" failureMode

utf8_bom :: TextEncoding
utf8_bom = mkUTF8_bom ErrorOnCodingFailure

mkUTF8_bom :: CodingFailureMode -> TextEncoding
mkUTF8_bom failureMode = failureModeEncoding "UTF-8BOM" failureMode
