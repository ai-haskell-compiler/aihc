module YamlUnscrambler.Expectations
  ( Value (..),
    Scalar (..),
    Mapping (..),
    Sequence (..),
    String (..),
    ByKey (..),
    ByOrder (..),

    -- * --
    MaxInputSize (..),
    Signed (..),
    NumeralSystem (..),
    CaseSensitive (..),
  )
where

import YamlUnscrambler.Model
import YamlUnscrambler.Prelude hiding (String)
import qualified YamlUnscrambler.Util.Maybe as Maybe

-- * --

data Value
  = Value
      [Scalar]
      (Maybe Mapping)
      (Maybe Sequence)

data Scalar
  = StringScalar String
  | NullScalar
  | BoolScalar
  | ScientificScalar
  | DoubleScalar
  | RationalScalar MaxInputSize
  | BoundedIntegerScalar Signed NumeralSystem
  | UnboundedIntegerScalar MaxInputSize Signed NumeralSystem
  | Iso8601TimestampScalar
  | Iso8601DayScalar
  | Iso8601TimeScalar
  | UuidScalar
  | Base64BinaryScalar

data Mapping
  = MonomorphicMapping String Value
  | ByKeyMapping CaseSensitive (ByKey Text)

data Sequence
  = MonomorphicSequence Value
  | ByOrderSequence ByOrder
  | ByKeySequence (ByKey Int)

-- * --

data String
  = -- | Any string as it is.
    AnyString
  | -- | One of options. Suitable for enumerations.
    OneOfString
      CaseSensitive
      -- | Options.
      [Text]
  | -- | Must conform to a textually described format.
    FormattedString
      -- | Description of the format.
      Text

data ByKey key
  = AnyByKey
  | NoByKey
  | EitherByKey (ByKey key) (ByKey key)
  | BothByKey (ByKey key) (ByKey key)
  | LookupByKey
      -- | Keys to lookup.
      [key]
      Value

data ByOrder
  = AnyByOrder
  | BothByOrder ByOrder ByOrder
  | FetchByOrder Value

-- * --

instance Semigroup Value where
  (<>) (Value lScalars lMappings lSequences) (Value rScalars rMappings rSequences) =
    Value
      (lScalars <> rScalars)
      (Maybe.firstNonEmpty lMappings rMappings)
      (Maybe.firstNonEmpty lSequences rSequences)

instance Monoid Value where
  mempty =
    Value [] Nothing Nothing
