{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

#if MIN_VERSION_base(4,18,0)
# define HAS_FOLDABLE1_CONTAINERS   MIN_VERSION_containers(0,6,7)
# define HAS_FOLDABLE1_TRANSFORMERS MIN_VERSION_transformers(0,6,1)
#else
# define HAS_FOLDABLE1_CONTAINERS   1
# define HAS_FOLDABLE1_TRANSFORMERS 1
#endif

module Main (main) where

import Prelude hiding (foldl1, foldr1, head, last, maximum, minimum)

import Data.Functor.Compose                 (Compose (..))
import Data.Functor.Identity                (Identity (..))
import Data.Functor.Product                 (Product (..))
import Data.Functor.Sum                     (Sum (..))
import Data.List.NonEmpty                   (NonEmpty (..))
import Data.Semigroup
       (First (..), Last (..), Max (..), Min (..), Semigroup (..))
import Test.QuickCheck
       (Arbitrary, Fun, Property, Testable, applyFun, applyFun2, counterexample,
       mapSize, (===))
import Test.Tasty
       (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck                (testProperty)
import Test.QuickCheck.Poly                 (A, B, OrdA)

import Test.QuickCheck.Instances ()

import qualified Data.Foldable as F         (Foldable (foldMap))
import Data.Foldable                        (toList)
import Data.Foldable1

#if HAS_FOLDABLE1_CONTAINERS
import Data.Tree                            (Tree (..))
#endif

#if HASH_FOLDABLE1_TRANSFORMERS
import Data.Functor.Reverse                 (Reverse (..))
#endif

main :: IO ()
main = defaultMain $
  testGroup "foldable1-classes-compat"
    [ foldable1tests "NonEmpty"  (P1 :: P1 NonEmpty)
    , foldable1tests "foldMap1"  (P1 :: P1 NE1)
    , foldable1tests "foldrMap1" (P1 :: P1 NE3)
#if HAS_FOLDABLE1_CONTAINERS
    , foldable1tests "Tree"      (P1 :: P1 Tree)
#endif
    , foldable1tests "Identity"  (P1 :: P1 Identity)
    , foldable1tests "Compose"   (P1 :: P1 (Compose NonEmpty NonEmpty))
    , foldable1tests "Product"   (P1 :: P1 (Product NonEmpty NonEmpty))
#if HASH_FOLDABLE1_TRANSFORMERS
    , foldable1tests "Reverse"   (P1 :: P1 (Reverse NonEmpty))
#endif
    , foldable1tests "Sum"       (P1 :: P1 (Sum NonEmpty NonEmpty))
    ]

-------------------------------------------------------------------------------
-- tests
-------------------------------------------------------------------------------

foldable1tests
    :: forall f.
      ( Foldable1 f
      , Arbitrary (f A), Show (f A)
      , Arbitrary (f OrdA), Show (f OrdA)
      , Arbitrary (f B), Show (f B)
      , Arbitrary (f [B]), Show (f [B])
      )
    => TestName
    -> P1 f
    -> TestTree
foldable1tests name _p = testGroup name
    [ testProperty "foldMap1 ~= foldMap" coherentFoldMap
    , testProperty "toList . toNonEmpty ~= toList" coherentToNonEmpty

    , testProperty "foldl1 non/strict" $ smaller strictFoldl1
    , testProperty "foldr1 non/strict" $ smaller strictFoldr1
    , testProperty "foldlMap1 non/strict" $ smaller strictFoldl1Map
    , testProperty "foldrMap1 non/strict" $ smaller strictFoldr1Map

    -- test against default implementations
    , testProperty "foldMap1 default" defaultFoldMap
    , testProperty "foldrMap1 default" $ smaller defaultFoldr1Map
    , testProperty "foldlMap1 default" $ smaller defaultFoldl1Map
    , testProperty "toNonEmpty default" defaultToNonEmpty

    , testProperty "head default" defaultHead
    , testProperty "last default" defaultLast
    , testProperty "minimum default" defaultMinimum
    , testProperty "maximum default" defaultMaximum

    -- if we first convert to nonEmpty it should be the same
    , testProperty "foldMap via toNonEmpty" viaFoldMap
    , testProperty "foldr1 via toNonEmpty" $ smaller viaFoldr1
    , testProperty "foldl1 via toNonEmpty" $ smaller viaFoldl1
    , testProperty "foldr1' via toNonEmpty" $ smaller viaFoldr1'
    , testProperty "foldl1' via toNonEmpty" $ smaller viaFoldl1'
    , testProperty "head via toNonEmpty" viaHead
    , testProperty "last via toNonEmpty" viaLast
    , testProperty "minimum via toNonEmpty" viaMinimum
    , testProperty "maximum via toNonEmpty" viaMaximum
    ]
  where
    -- Things like Compose NonEmpty NonEmpty are big
    smaller :: Testable prop => prop -> Property
    smaller = mapSize (`div` 3)

    coherentFoldMap :: f A -> Fun A [B] -> Property
    coherentFoldMap xs f' = F.foldMap f xs === foldMap1 f xs where
        f = applyFun f'

    coherentToNonEmpty :: f A -> Property
    coherentToNonEmpty xs = toList (toNonEmpty xs) === toList xs

    strictFoldr1 :: f [B] -> Fun ([B], [B]) [B] -> Property
    strictFoldr1 xs g' = foldr1 g xs === foldr1' g xs where
        g = applyFun2 g'

    strictFoldl1 :: f [B] -> Fun ([B], [B]) [B] -> Property
    strictFoldl1 xs g' = foldl1 g xs === foldl1' g xs where
        g = applyFun2 g'


    strictFoldr1Map :: f A -> Fun A B -> Fun (A, B) B -> Property
    strictFoldr1Map xs f' g' = foldrMap1 f g xs === foldrMap1' f g xs where
        f = applyFun f'
        g = applyFun2 g'

    strictFoldl1Map :: f A -> Fun A B -> Fun (B, A) B -> Property
    strictFoldl1Map xs f' g' = foldlMap1 f g xs === foldlMap1' f g xs where
        f = applyFun f'
        g = applyFun2 g'

    defaultFoldMap :: f A -> Fun A [B] -> Property
    defaultFoldMap xs f' = F.foldMap f xs === foldrMap1 f (\a m -> f a Data.Semigroup.<> m) xs where
        f = applyFun f'

    defaultFoldr1Map :: f A -> Fun A [B] -> Fun (A, [B]) [B] -> Property
    defaultFoldr1Map xs f' g'
        = counterexample ("NE: " ++ show ys)
        $ foldrMap1 f g xs === foldrMap1 f g ys
      where
        f = applyFun f'
        g = applyFun2 g'
        ys = toNonEmpty xs

    defaultFoldl1Map :: f A -> Fun A [B] -> Fun ([B], A) [B] -> Property
    defaultFoldl1Map xs f' g'
        = counterexample ("NE: " ++ show ys)
        $ foldlMap1 f g xs === foldlMap1 f g ys
      where
        f = applyFun f'
        g = applyFun2 g'
        ys = toNonEmpty xs

    defaultToNonEmpty :: f A -> Property
    defaultToNonEmpty xs = toNonEmpty xs === foldMap1 (:|[]) xs

    defaultHead :: f A -> Property
    defaultHead xs = head xs === getFirst (foldMap1 First xs)

    defaultLast :: f A -> Property
    defaultLast xs = last xs === getLast (foldMap1 Last xs)

    defaultMinimum :: f OrdA -> Property
    defaultMinimum xs = minimum xs === getMin (foldMap1 Min xs)

    defaultMaximum :: f OrdA -> Property
    defaultMaximum xs = maximum xs === getMax (foldMap1 Max xs)

    viaFoldMap :: f A -> Fun A [B] -> Property
    viaFoldMap xs f' = F.foldMap f xs === F.foldMap f (toNonEmpty xs) where
        f = applyFun f'

    viaFoldr1 :: f [B] -> Fun ([B],[B]) [B] -> Property
    viaFoldr1 xs g' = foldr1 g xs === foldr1 g (toNonEmpty xs) where
        g = applyFun2 g'

    viaFoldr1' :: f [B] -> Fun ([B],[B]) [B] -> Property
    viaFoldr1' xs g' = foldr1' g xs === foldr1' g (toNonEmpty xs) where
        g = applyFun2 g'

    viaFoldl1 :: f [B] -> Fun ([B],[B]) [B] -> Property
    viaFoldl1 xs g' = foldl1 g xs === foldl1 g (toNonEmpty xs) where
        g = applyFun2 g'

    viaFoldl1' :: f [B] -> Fun ([B],[B]) [B] -> Property
    viaFoldl1' xs g' = foldl1' g xs === foldl1' g (toNonEmpty xs) where
        g = applyFun2 g'

    viaHead :: f A -> Property
    viaHead xs = head xs === head (toNonEmpty xs)

    viaLast :: f A -> Property
    viaLast xs = last xs === last (toNonEmpty xs)

    viaMinimum :: f OrdA -> Property
    viaMinimum xs = minimum xs === minimum (toNonEmpty xs)

    viaMaximum :: f OrdA -> Property
    viaMaximum xs = maximum xs === maximum (toNonEmpty xs)

-------------------------------------------------------------------------------
-- NonEmpty variants
-------------------------------------------------------------------------------

-- Using foldMap1 to define Foldable1
newtype NE1 a = NE1 (NonEmpty a)
  deriving (Eq, Show, Functor, F.Foldable, Arbitrary)

instance Foldable1 NE1 where
    foldMap1 f (NE1 xs) = foldMap1 f xs

-- Using foldrMap1 to define Foldable1
newtype NE3 a = NE3 (NonEmpty a)
  deriving (Eq, Show, Functor, F.Foldable, Arbitrary)

instance Foldable1 NE3 where
    foldrMap1 g f (NE3 xs) = foldrMap1 g f xs

-------------------------------------------------------------------------------
-- utilities
-------------------------------------------------------------------------------

-- Proxy of right kind
data P1 f
    = P1
    | Unused (f Int)

_unused :: P1 []
_unused = Unused []
