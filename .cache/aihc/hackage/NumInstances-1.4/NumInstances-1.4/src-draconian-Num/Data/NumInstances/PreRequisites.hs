module Data.NumInstances.PreRequisites where

import Control.Applicative

import Data.NumInstances.Util

noFun :: String -> a
noFun = noOv "function"

-- Eq & Show are prerequisites for Num, so they need to be faked here
instance Eq (a->b) where
  (==) = noFun "(==)"
  (/=) = noFun "(/=)"

instance Ord b => Ord (a->b) where
  min = liftA2 min
  max = liftA2 max

instance Show (a->b) where
  show      = noFun "show"
  showsPrec = noFun "showsPrec"
  showList  = noFun "showList"
