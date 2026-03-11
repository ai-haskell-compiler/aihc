{-# options_haddock prune #-}

-- | Description: Internal
module Exon.Class.ToSegment where

import GHC.TypeLits (ErrorMessage)

import Exon.Generic (IsNewtype, Unwrap (unwrap))

class NewtypeSegment (wrapped :: Maybe Type) a b where
  newtypeSegment :: a -> b

instance (
    Unwrap a b,
    ToSegment b c
  ) => NewtypeSegment ('Just b) a c where
    newtypeSegment = toSegment . unwrap

type family Q (a :: k) :: ErrorMessage where
  Q a = "‘" <> a <> "’"

type family NoGenericMessage (a :: Type) (b :: Type) :: Constraint where
  NoGenericMessage a a = a ~ a
  NoGenericMessage a b =
    TypeError (
      "Found an expression of type " <> Q a <> " in a quote of type " <> Q b <> "." %
      "If " <> Q a <> " is a newtype of " <> Q b <> " that should be converted automatically," %
      "you need to add " <> Q "deriving (Generic)" <> " to its declaration." %
      "You can also implement " <> Q ("instance ToSegment " <> a <> " " <> b) <> " for custom interpolation."
    )

instance (
    NoGenericMessage a b,
    a ~ b
  ) => NewtypeSegment 'Nothing a b where
    newtypeSegment = id

-- | This class determines how an expression is converted to an interpolation quote's result type.
--
-- For a quote like @[exon|a #{exp :: T} c|] :: R@, the instance @ToSegment T R@ is used to turn @T@ into @R@.
-- Aside from specialized instances for stringly types, the default implementation uses 'Generic' to unwrap newtypes
-- that either match the result type exactly, or uses 'fromString' for result types that implement 'IsString'.
--
-- So given:
--
-- >>> newtype T = T Text deriving newtype (Generic)
-- >>> newtype R = R Text deriving newtype (IsString, Semigroup, Monoid)
--
-- the quote from earlier would generically unwrap @T@ and use 'fromString' to construct an @R@.
class ToSegment a b where
  toSegment :: a -> b

instance {-# incoherent #-} (
    IsNewtype a wrapped,
    NewtypeSegment wrapped a b
  ) => ToSegment a b where
  toSegment =
    newtypeSegment @wrapped

instance {-# overlappable #-} (
    IsString a
  ) => ToSegment String a where
  toSegment =
    fromString . toString

instance {-# overlappable #-} (
    IsString a
  ) => ToSegment Text a where
  toSegment =
    fromString . toString

instance {-# overlappable #-} (
    IsString a
  ) => ToSegment LText a where
  toSegment =
    fromString . toString

instance {-# overlappable #-} (
    IsString a
  ) => ToSegment ByteString a where
  toSegment =
    fromString . decodeUtf8

instance {-# overlappable #-} (
    IsString a
  ) => ToSegment LByteString a where
  toSegment =
    fromString . decodeUtf8
