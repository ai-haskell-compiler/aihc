-- Each mk function keeps its argument: the arity pass reads the arity from
-- the body, and an eta-reduced alias is not inlined. Thus the default
-- failure mode can select the name with no suffix.
{-# HLINT ignore "Eta reduce" #-}

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

import GHC.IO.Encoding.Failure (CodingFailureMode (..))
import GHC.IO.Encoding.Types (TextEncoding)
import GHC.Internal.IO.Encoding.FailureMode (failureModeEncoding)

latin1 :: TextEncoding
latin1 = mkLatin1 ErrorOnCodingFailure

mkLatin1 :: CodingFailureMode -> TextEncoding
mkLatin1 failureMode = failureModeEncoding "ISO-8859-1" failureMode

-- | Latin-1 that rejects the characters above U+00FF instead of
-- truncating them.
latin1_checked :: TextEncoding
latin1_checked = mkLatin1_checked ErrorOnCodingFailure

mkLatin1_checked :: CodingFailureMode -> TextEncoding
mkLatin1_checked failureMode = failureModeEncoding "ISO-8859-1(checked)" failureMode

ascii :: TextEncoding
ascii = mkAscii ErrorOnCodingFailure

mkAscii :: CodingFailureMode -> TextEncoding
mkAscii failureMode = failureModeEncoding "ASCII" failureMode
