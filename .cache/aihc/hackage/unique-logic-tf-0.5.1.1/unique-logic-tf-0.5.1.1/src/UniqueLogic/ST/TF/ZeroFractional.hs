{- |
This class provides short-cut multiplication
and division that checks for 0/0.
-}
module UniqueLogic.ST.TF.ZeroFractional (
   C, multiply, divide,
   multiplyDefault, divideDefault,
   ) where

import Data.Complex (Complex, )
import Data.Ratio (Ratio, )

import Data.Maybe.HT (toMaybe, )


class Fractional a => C a where
   multiply :: a -> Maybe a
   divide :: a -> a -> Maybe a

instance C Float where
   multiply = multiplyDefault
   divide = divideDefault

instance C Double where
   multiply = multiplyDefault
   divide = divideDefault

instance (Integral a) => C (Ratio a) where
   multiply = multiplyDefault
   divide = divideDefault

instance (RealFloat a) => C (Complex a) where
   multiply = multiplyDefault
   divide = divideDefault


multiplyDefault :: (Num a, Eq a) => a -> Maybe a
multiplyDefault x = toMaybe (x==0) 0

divideDefault :: (Fractional a, Eq a) => a -> a -> Maybe a
divideDefault z x = toMaybe (z/=0 || x/=0) (z/x)
