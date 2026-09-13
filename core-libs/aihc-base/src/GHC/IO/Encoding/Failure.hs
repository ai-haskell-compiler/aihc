-- | How a text codec reacts to a byte sequence or character it cannot
-- translate. Handles carry the name of their encoding only, so the mode
-- reaches the runtime as the suffix of that name.
module GHC.IO.Encoding.Failure
  ( CodingFailureMode (..),
    codingFailureModeSuffix,
  )
where

import GHC.Base (String)
import GHC.Show (Show (..))

data CodingFailureMode
  = -- | Throw an exception.
    ErrorOnCodingFailure
  | -- | Drop the offending input.
    IgnoreCodingFailure
  | -- | Replace the offending input with a substitute character.
    TransliterateCodingFailure
  | -- | Map undecodable bytes to the surrogate code points that GHC uses
    -- to carry them back out unchanged.
    RoundtripFailure
  deriving (Show)

-- | The suffix that names an encoding with this failure mode.
codingFailureModeSuffix :: CodingFailureMode -> String
codingFailureModeSuffix ErrorOnCodingFailure = ""
codingFailureModeSuffix IgnoreCodingFailure = "//IGNORE"
codingFailureModeSuffix TransliterateCodingFailure = "//TRANSLIT"
codingFailureModeSuffix RoundtripFailure = "//ROUNDTRIP"
