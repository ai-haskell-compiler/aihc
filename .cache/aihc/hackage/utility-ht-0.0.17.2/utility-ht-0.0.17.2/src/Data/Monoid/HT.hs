module Data.Monoid.HT (cycle, (<>), when, power) where

import Data.Monoid (Monoid, mappend, mempty, )
import Data.Function (fix, )

import Prelude (Integer, Bool, Ordering(..), compare, divMod, error)


{- $setup
>>> import qualified Test.QuickCheck as QC
>>> import Control.Monad (mfilter)
>>> import Data.Function.HT (powerAssociative)
>>> import Data.Monoid (mconcat, mappend, mempty)
-}

{- |
Generalization of 'Data.List.cycle' to any monoid.
-}
cycle :: Monoid m => m -> m
cycle x =
   fix (mappend x)


infixr 6 <>

{- |
Infix synonym for 'mappend'.
-}
(<>) :: Monoid m => m -> m -> m
(<>) = mappend


{- |
prop> \b m -> when b m == mfilter (const b) (m::Maybe Ordering)
prop> \b m -> when b m == mfilter (const b) (m::String)
-}
when :: Monoid m => Bool -> m -> m
when b m = if b then m else mempty

{- |
prop> QC.forAll (QC.choose (0,20)) $ \k xs -> power (fromIntegral k) xs == mconcat (replicate k (xs::String))

In contrast to 'powerAssociative' the 'power' function
uses 'mempty' only for the zeroth power.

prop> QC.forAll (QC.choose (0,20)) $ \k xs -> power k xs == powerAssociative mappend mempty (xs::String) k
-}
power :: Monoid m => Integer -> m -> m
power k m =
   case compare k 0 of
      LT -> error "Monoid.power: negative exponent"
      EQ -> mempty
      GT ->
         let (k2,r) = divMod k 2
             p = power k2 m
             p2 = p<>p
         in case r of
               0 -> p2
               _ -> m<>p2
