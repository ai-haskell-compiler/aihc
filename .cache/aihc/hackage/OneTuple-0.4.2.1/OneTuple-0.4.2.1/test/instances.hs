{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}
#else
{-# OPTIONS_GHC -fwarn-incomplete-patterns -Werror #-}
#endif
module Main where

import Control.Applicative  (Applicative (..))
import Control.Monad.Fix    (MonadFix (..))
import Data.Data            (Data)
import Data.Foldable        (Foldable (..))
import Data.Foldable1       (Foldable1)
import Data.Functor.Classes (Eq1, Ord1, Read1, Show1)
import Data.Hashable        (Hashable)
import Data.Hashable.Lifted (Hashable1)
import Data.Ix              (Ix)
import Data.Monoid          (Monoid (..))
import Data.Semigroup       (Semigroup (..))
import Data.Traversable     (Traversable (..))

#if MIN_VERSION_base(4,4,0)
import Control.Monad.Zip (MonadZip (..))
#endif

import Data.Tuple.Solo (Solo (..))

main :: IO ()
main = putStrLn "works"

-------------------------------------------------------------------------------
-- pattern match
-------------------------------------------------------------------------------

match :: Solo a -> a
match (MkSolo x) = x

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

tup1 :: Solo Char
tup1 = MkSolo 'x'

tup2 :: Solo String
tup2 = MkSolo "test"

hasEq :: Eq a => a -> a; hasEq x = x; testEq = hasEq tup1
hasOrd :: Ord a => a -> a; hasOrd x = x; testOrd = hasOrd tup1
hasBounded :: Bounded a => a -> a; hasBounded x = x; testBounded = hasBounded tup1
hasEnum :: Enum a => a -> a; hasEnum x = x; testEnum = hasEnum tup1
hasShow :: Show a => a -> a; hasShow x = x; testShow = hasShow tup1
hasRead :: Read a => a -> a; hasRead x = x; testRead = hasRead tup1
hasIx :: Ix a => a -> a; hasIx x = x; testIx = hasIx tup1

hasMonoid :: Monoid a => a -> a; hasMonoid x = x; testMonoid = hasMonoid tup2
hasSemigroup :: Semigroup a => a -> a; hasSemigroup x = x; testSemigroup = hasSemigroup tup2

hasData :: Data a => a -> a; hasData x = x; testData = hasData tup2

hasFunctor :: Functor f => f a -> f a; hasFunctor x = x; testFunctor = hasFunctor tup1
hasFoldable :: Foldable f => f a -> f a; hasFoldable x = x; testFoldable = hasFoldable tup1
hasFoldable1 :: Foldable1 f => f a -> f a; hasFoldable1 x = x; testFoldable1 = hasFoldable1 tup1
hasTraversable :: Traversable f => f a -> f a; hasTraversable x = x; testTraversable = hasTraversable tup1
hasApplicative :: Applicative f => f a -> f a; hasApplicative x = x; testApplicative = hasApplicative tup1
hasMonad :: Monad f => f a -> f a; hasMonad x = x; testMonad = hasMonad tup1
hasMonadFix :: MonadFix f => f a -> f a; hasMonadFix x = x; testMonadFix = hasMonadFix tup1

#if MIN_VERSION_base(4,4,0)
hasMonadZip :: MonadZip f => f a -> f a; hasMonadZip x = x; testMonadZip = hasMonadZip tup1
#endif

hasEq1 :: Eq1 f => f a -> f a; hasEq1 x = x; testEq1 = hasEq1 tup1
hasOrd1 :: Ord1 f => f a -> f a; hasOrd1 x = x; testOrd1 = hasOrd1 tup1
hasShow1 :: Show1 f => f a -> f a; hasShow1 x = x; testShow1 = hasShow1 tup1
hasRead1 :: Read1 f => f a -> f a; hasRead1 x = x; testRead1 = hasRead1 tup1

hasHashable :: Hashable a => a -> a; hasHashable x = x; testHashable = hasHashable tup1
hasHashable1 :: Hashable1 f => f a -> f a; hasHashable1 x = x; testHashable1 = hasHashable1 tup1
