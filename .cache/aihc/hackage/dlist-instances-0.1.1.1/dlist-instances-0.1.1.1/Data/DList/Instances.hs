{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.DList.Instances () where

-- don't define instance for dlist>=0.8 && base>=4.9
#if !(MIN_VERSION_base(4,9,0)) || !(MIN_VERSION_dlist(0,8,0))
import Data.DList
import Data.Semigroup

instance Semigroup (DList a) where
  (<>) = append
#endif
