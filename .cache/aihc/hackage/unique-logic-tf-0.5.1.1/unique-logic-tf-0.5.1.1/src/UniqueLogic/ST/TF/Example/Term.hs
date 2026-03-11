{- |
This module is intended for documentation purposes. Do not import it!
-}
module UniqueLogic.ST.TF.Example.Term where

import qualified UniqueLogic.ST.TF.ZeroFractional as ZeroFractional

import Data.Maybe.HT (toMaybe, )


data T =
     Const Rational
   | Var Name
   | Max T T
   | Add T T
   | Sub T T
   | Mul T T
   | Div T T
   | Abs T
   | Signum T
   deriving (Show)

type Name = String


instance Num T where
   fromInteger n = Const $ fromInteger n
   (+) = Add
   (-) = Sub
   (*) = Mul
   abs = Abs
   signum = Signum

instance Fractional T where
   fromRational x = Const x
   (/) = Div

instance ZeroFractional.C T where
   multiply x = toMaybe (isZero x) 0
   divide z x = toMaybe (not (isZero z && isZero x)) (z/x)

isZero :: T -> Bool
isZero (Const x) = x==0
isZero _ = False
