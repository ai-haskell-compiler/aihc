module Util.TestTrees where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid
import Data.Semigroup
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck
import Prelude

-- | Tests mapping from @a@ to @b@ to produce a valid 'Monoid'.
--
-- Tests the following properties:
--
-- [/Associative/]
--   @a '<>' (b '<>' c) ≡ (a '<>' b) '<>' c@
-- [/Semigroup Concatenation/]
--   @'sconcat' as ≡ 'foldr1' ('<>') as@
-- [/Times/]
--   @'stimes' n a ≡ 'foldr1' ('<>') ('replicate' n a)@
-- [/Left Identity/]
--   @mappend mempty a ≡ a@
-- [/Right Identity/]
--   @mappend a mempty ≡ a@
-- [/Monoid Concatenation/]
--   @mconcat as ≡ foldr mappend mempty as@
mapsToMonoid ::
  forall a b.
  (Arbitrary a, Monoid b, Eq b, Show b) =>
  -- | Embed in monoid.
  (a -> b) ->
  TestTree
mapsToMonoid embed =
  isMonoidWithCustomGen (embed <$> arbitrary)

-- | Tests mapping from @a@ to @b@ to produce a valid 'Monoid'.
--
-- Tests the following properties:
--
-- [/Associative/]
--   @a '<>' (b '<>' c) ≡ (a '<>' b) '<>' c@
-- [/Semigroup Concatenation/]
--   @'sconcat' as ≡ 'foldr1' ('<>') as@
-- [/Times/]
--   @'stimes' n a ≡ 'foldr1' ('<>') ('replicate' n a)@
-- [/Left Identity/]
--   @mappend mempty a ≡ a@
-- [/Right Identity/]
--   @mappend a mempty ≡ a@
-- [/Monoid Concatenation/]
--   @mconcat as ≡ foldr mappend mempty as@
isMonoidWithCustomGen ::
  (Monoid a, Eq a, Show a) =>
  Gen a ->
  TestTree
isMonoidWithCustomGen gen =
  testGroup
    "Monoid"
    [ testProperty "Is associative" do
        x <- gen
        y <- gen
        z <- gen
        pure (x <> (y <> z) === (x <> y) <> z),
      testProperty "Semigroup concatenation" do
        xs <- (:|) <$> gen <*> listOf gen
        pure (sconcat xs === foldr1 (<>) xs),
      testProperty "Times" do
        x <- gen
        Positive n <- arbitrary
        pure (stimes n x === foldr1 (<>) (replicate n x)),
      testProperty "Left identity" do
        x <- gen
        pure (mempty <> x === x),
      testProperty "Right identity" do
        x <- gen
        pure (x <> mempty === x),
      testProperty "Monoid concatenation" do
        xs <- listOf gen
        pure (mconcat xs === foldr mappend mempty xs)
    ]

followsLaws :: Laws -> TestTree
followsLaws Laws {..} =
  testProperties lawsTypeclass lawsProperties
