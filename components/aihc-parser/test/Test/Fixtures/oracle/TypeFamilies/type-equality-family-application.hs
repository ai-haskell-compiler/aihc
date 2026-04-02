{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}
module TypeEqualityFamilyApplication where

type family IsUpperCased a

data No
data Upper
data Cased a b

class Casing a

upperCased :: (Casing b, IsUpperCased a ~ No) => Cased a b -> Cased Upper b
upperCased = undefined
