-- Each mk function keeps its argument: the arity pass reads the arity from
-- the body, and an eta-reduced alias is not inlined. Thus the default
-- failure mode can select the name with no suffix.
{-# HLINT ignore "Eta reduce" #-}

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

import GHC.IO.Encoding.Failure (CodingFailureMode (..))
import GHC.IO.Encoding.Types (TextEncoding)
import GHC.Internal.IO.Encoding.FailureMode (failureModeEncoding)

utf32 :: TextEncoding
utf32 = mkUTF32 ErrorOnCodingFailure

mkUTF32 :: CodingFailureMode -> TextEncoding
mkUTF32 failureMode = failureModeEncoding "UTF-32" failureMode

utf32le :: TextEncoding
utf32le = mkUTF32le ErrorOnCodingFailure

mkUTF32le :: CodingFailureMode -> TextEncoding
mkUTF32le failureMode = failureModeEncoding "UTF-32LE" failureMode

utf32be :: TextEncoding
utf32be = mkUTF32be ErrorOnCodingFailure

mkUTF32be :: CodingFailureMode -> TextEncoding
mkUTF32be failureMode = failureModeEncoding "UTF-32BE" failureMode
