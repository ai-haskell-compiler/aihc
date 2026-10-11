-- Each mk function keeps its argument: the arity pass reads the arity from
-- the body, and an eta-reduced alias is not inlined. Thus the default
-- failure mode can select the name with no suffix.
{-# HLINT ignore "Eta reduce" #-}

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

import GHC.IO.Encoding.Failure (CodingFailureMode (..))
import GHC.IO.Encoding.Types (TextEncoding)
import GHC.Internal.IO.Encoding.FailureMode (failureModeEncoding)

utf16 :: TextEncoding
utf16 = mkUTF16 ErrorOnCodingFailure

mkUTF16 :: CodingFailureMode -> TextEncoding
mkUTF16 failureMode = failureModeEncoding "UTF-16" failureMode

utf16le :: TextEncoding
utf16le = mkUTF16le ErrorOnCodingFailure

mkUTF16le :: CodingFailureMode -> TextEncoding
mkUTF16le failureMode = failureModeEncoding "UTF-16LE" failureMode

utf16be :: TextEncoding
utf16be = mkUTF16be ErrorOnCodingFailure

mkUTF16be :: CodingFailureMode -> TextEncoding
mkUTF16be failureMode = failureModeEncoding "UTF-16BE" failureMode
