{-# LANGUAGE StandaloneDeriving #-}

module StandaloneDerivingQualified where

import qualified Data.Ord as Ord

data Box a = Box a

deriving instance Ord.Ord a => Ord.Ord (Box a)
