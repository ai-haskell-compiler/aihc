
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}

module Data.Cased (
  Lower, Upper, Mixed, Yes, No,
  IsUpperCased, IsLowerCased,
  Cased(..),
  Casing(..),

  upperCased, lowerCased, mixedCased,
  force
) where

import qualified Data.Text.Lazy as LT
import qualified Data.Text as ST
import qualified Data.Char as C

data Lower
data Upper
data Mixed
data Yes
data No

newtype Cased a b = Cased { fromCased :: b }
                  deriving (Show, Eq, Ord)

class Casing a where
  toUpper :: a -> a
  toLower :: a -> a

instance Casing ST.Text where
  toUpper = ST.toUpper
  toLower = ST.toLower

instance Casing LT.Text where
  toUpper = LT.toUpper
  toLower = LT.toLower

instance Casing String where
  toUpper = map C.toUpper
  toLower = map C.toLower

type family IsLowerCased a :: *
type family IsUpperCased a :: *

type instance IsUpperCased Upper = Yes
type instance IsUpperCased Lower = No
type instance IsUpperCased Mixed = No

type instance IsLowerCased Lower = Yes
type instance IsLowerCased Upper = No
type instance IsLowerCased Mixed = No

force :: (Cased Mixed b -> c) -> b -> c
force f = f . mixedCased

mixedCased :: a -> Cased Mixed a
mixedCased = Cased

upperCased :: (Casing b, IsUpperCased a ~ No) => Cased a b -> Cased Upper b
upperCased = Cased . toUpper . fromCased

lowerCased :: (Casing b, IsLowerCased a ~ No) => Cased a b -> Cased Lower b
lowerCased = Cased . toLower . fromCased
