module Test.Sound.Synthesizer.Plain.NonEmpty where

import Test.QuickCheck (Arbitrary, arbitrary, )
import Control.Monad (liftM2, )


data T a = Cons a [a]

toList :: T a -> [a]
toList (Cons x xs) =
   (x:xs)

toInfiniteList :: T a -> [a]
toInfiniteList =
   cycle . toList

instance Functor T where
   fmap f (Cons x xs) =
      Cons (f x) (map f xs)

instance Arbitrary a => Arbitrary (T a) where
   arbitrary = liftM2 Cons arbitrary arbitrary

instance Show a => Show (T a) where
   showsPrec p (Cons x xs) =
      showsPrec p (x:xs)

{-
instance Show a => Show (T a) where
   showsPrec p (Cons x xs) =
      showParen (p >= 10) $
      showString "cycle " .
      showsPrec 11 (x:xs)
-}
