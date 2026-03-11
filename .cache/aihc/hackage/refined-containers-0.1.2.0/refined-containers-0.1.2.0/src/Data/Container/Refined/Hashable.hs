{-# LANGUAGE CPP #-}
module Data.Container.Refined.Hashable
  ( Hashable
  ) where

#if MIN_VERSION_hashable(1, 4, 0)
import           Data.Hashable (Hashable)
#else
import qualified Data.Hashable as Hashable


type Hashable a = (Eq a, Hashable.Hashable a)
#endif
