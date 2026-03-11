module LawfulConversions.Proxies.ViaIsSome where

import LawfulConversions.Classes
import LawfulConversions.Prelude
import qualified Test.QuickCheck as QuickCheck

-- |
-- Helper for deriving common instances on types which have an instance of @'IsSome' a@ using the @DerivingVia@ extension.
--
-- E.g.,
--
-- > newtype Percent = Percent Double
-- >   deriving newtype (Show, Eq, Ord)
-- >   deriving (Read, Arbitrary) via (ViaIsSome Double Percent)
-- >
-- > instance IsSome Double Percent where
-- >   to (Percent double) = double
-- >   maybeFrom double =
-- >     if double < 0 || double > 1
-- >       then Nothing
-- >       else Just (Percent double)
--
-- In the code above all the instances that are able to construct the values of 'Percent' are automatically derived based on the @IsSome Double Percent@ instance.
-- This guarantees that they only construct values that pass thru the checks defined in 'maybeFrom'.
newtype ViaIsSome a b = ViaIsSome b

instance (IsSome a b) => IsSome a (ViaIsSome a b) where
  to (ViaIsSome a) = to a
  maybeFrom = fmap ViaIsSome . maybeFrom

instance IsSome b (ViaIsSome a b) where
  to = coerce

instance IsSome (ViaIsSome a b) b where
  to = coerce

instance IsMany b (ViaIsSome a b)

instance IsMany (ViaIsSome a b) b

instance Is b (ViaIsSome a b)

instance Is (ViaIsSome a b) b

instance (IsSome a b, Show a) => Show (ViaIsSome a b) where
  show (ViaIsSome a) = show (to @a a)

instance (IsSome a b, Read a) => Read (ViaIsSome a b) where
  readPrec = do
    a <- readPrec
    case maybeFrom @a a of
      Just a -> pure (ViaIsSome a)
      Nothing -> fail "Value is not from the subset"

instance (IsSome a b, IsString a) => IsString (ViaIsSome a b) where
  fromString =
    maybe (error "Value is not from the subset") ViaIsSome . maybeFrom @a . fromString

instance (IsSome a b, Eq a) => Eq (ViaIsSome a b) where
  (==) = on (==) (to @a)

instance (IsSome a b, Ord a) => Ord (ViaIsSome a b) where
  compare = on compare (to @a)

instance (IsSome a b, QuickCheck.Arbitrary a) => QuickCheck.Arbitrary (ViaIsSome a b) where
  arbitrary =
    QuickCheck.suchThatMap QuickCheck.arbitrary (maybeFrom @a)
  shrink value = do
    shrunkValue <- QuickCheck.shrink (to @a value)
    shrunkValue
      & maybeFrom
      & maybeToList
