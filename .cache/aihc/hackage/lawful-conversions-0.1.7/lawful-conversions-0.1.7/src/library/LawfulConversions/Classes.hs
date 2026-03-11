module LawfulConversions.Classes where

import LawfulConversions.Prelude

-- |
-- Evidence that all values of type @b@ form a subset of all values of type @a@.
--
-- [From Wikipedia](https://en.wikipedia.org/wiki/Subset):
--
-- In mathematics, a set A is a subset of a set B if all elements of A are also elements of B; B is then a superset of A. It is possible for A and B to be equal; if they are unequal, then A is a proper subset of B. The relationship of one set being a subset of another is called inclusion (or sometimes containment). A is a subset of B may also be expressed as B includes (or contains) A or A is included (or contained) in B. A k-subset is a subset with k elements.
--
-- === Laws
--
-- ==== 'to' is [injective](https://en.wikipedia.org/wiki/Injective_function)
--
-- For every two values of type @b@ that are not equal converting with 'to' produces values that are not equal as well:
--
-- > \(b1, b2) -> b1 == b2 || to @a b1 /= to @a b2
--
-- ==== 'maybeFrom' is a [partial inverse](https://en.wikipedia.org/wiki/Inverse_function#Partial_inverses) of 'to'
--
-- For all values of @b@ converting to @a@ and then attempting to convert back to @b@ always succeeds and produces a value that is equal to the original:
--
-- > \b -> maybeFrom (to @a b) == Just b
--
-- === Testing
--
-- For testing whether your instances conform to these laws use 'LawfulConversions.isSomeProperties'.
class IsSome a b where
  -- |
  -- Convert a value of a subset type to a superset type.
  to :: b -> a

  -- |
  -- [Partial inverse](https://en.wikipedia.org/wiki/Inverse_function#Partial_inverses) of 'to'.
  maybeFrom :: a -> Maybe b

  -- |
  -- Requires the presence of 'IsSome' in reverse direction.
  default maybeFrom :: (IsSome b a) => a -> Maybe b
  maybeFrom = Just . to

-- | Every type is isomorphic to itself.
instance IsSome a a where
  to = id
  maybeFrom = Just . id

-- | The empty set has no elements, and therefore is vacuously a subset of any set.
instance IsSome a Void where
  to = absurd
  maybeFrom = const Nothing

-- |
-- Lossy or canonicalizing conversion.
-- Captures mappings from multiple alternative inputs into one output.
--
-- E.g.,
--
-- - `ByteString` can be decoded into `Text` with UTF-8 leniently, replacing the invalid chars with a default char.
--
-- - `String` has a wider range of supported chars than `Text`, so some chars get replaced too.
--
-- === Laws
--
-- ==== 'from' is an [inverse](https://en.wikipedia.org/wiki/Inverse_function) of 'to'
--
-- > \b -> b == from (to @a b)
--
-- === Testing
--
-- For testing whether your instances conform to these laws use 'LawfulConversions.isManyProperties'.
class (IsSome a b) => IsMany a b where
  -- |
  -- Possibly lossy inverse of 'to'.
  -- [Surjection](https://en.wikipedia.org/wiki/Surjective_function) from @a@ to @b@.
  --
  -- Particularly useful in combination with the @TypeApplications@ extension,
  -- where it allows to specify the input type, e.g.:
  --
  -- > fromText :: IsMany Text b => Text -> b
  -- > fromText = from @Text
  --
  -- The first type application of the 'to' function on the other hand specifies
  -- the output data type.
  --
  -- If you want to specify the output type instead, use 'onto'.
  from :: a -> b

  -- |
  -- Requires the presence of 'IsSome' in reverse direction.
  default from :: (IsSome b a) => a -> b
  from = to

-- |
-- Alias to 'from', which lets you specify the target type of the conversion first using @TypeApplications@.
--
-- In mathematics @onto@ is another name for [Surjective function](https://en.wikipedia.org/wiki/Surjective_function).
--
-- E.g.,
--
-- > lenientDecodeUtf8 = onto @Text
onto :: forall b a. (IsMany a b) => a -> b
onto = from

instance IsMany a a

-- | Bidirectional conversion between two types with no loss of information.
--
-- The bidirectionality is encoded via a recursive dependency with arguments
-- flipped.
--
-- You can read the signature @Is a b@ as \"/B/ is /A/\".
--
-- === Laws
--
-- ==== 'from' is an [inverse](https://en.wikipedia.org/wiki/Inverse_function) of 'to'
--
-- For all values of /b/ converting from /b/ to /a/ and then converting from /a/ to /b/ produces the original value:
--
-- > \b -> b == from (to @a b)
--
-- ==== 'to' is an [inverse](https://en.wikipedia.org/wiki/Inverse_function) of 'from'
--
-- For all values of /a/ converting from /a/ to /b/ and then converting from /b/ to /a/ produces the original value:
--
-- > \a -> a == to (from @a @b a)
--
-- === Testing
--
-- For testing whether your instances conform to these laws use 'LawfulConversions.isProperties'.
--
-- === Instance Definition
--
-- For each pair of isomorphic types (/A/ and /B/) the compiler will require you to define six instances, namely: @Is A B@ and @Is B A@, @IsMany A B@ and @IsMany B A@, @IsSome A B@ and @IsSome B A@.
--
-- Instances of @Is@ do not define any functions and serve merely as a statement that the laws are satisfied.
--
-- ==== Example: Lazy Text and Text
--
-- @
-- instance IsSome "Data.Text.Lazy.LazyText" "Data.Text.Text" where
--   to = LazyText.'Data.Text.Lazy.fromStrict'
--
-- instance IsSome "Data.Text.Text" "Data.Text.Lazy.LazyText" where
--   to = LazyText.'Data.Text.Lazy.toStrict'
--
-- instance IsMany "Data.Text.Lazy.LazyText" "Data.Text.Text"
--
-- instance IsMany "Data.Text.Text" "Data.Text.Lazy.LazyText"
--
-- instance Is "Data.Text.Lazy.LazyText" "Data.Text.Text"
--
-- instance Is "Data.Text.Text" "Data.Text.Lazy.LazyText"
-- @
class (IsMany a b, Is b a) => Is a b

-- | Any type is isomorphic to itself.
instance Is a a
