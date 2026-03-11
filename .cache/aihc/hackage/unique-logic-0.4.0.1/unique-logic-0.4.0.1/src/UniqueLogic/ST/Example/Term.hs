{- |
This module is intended for documentation purposes. Do not import it!
-}
module UniqueLogic.ST.Example.Term where


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
