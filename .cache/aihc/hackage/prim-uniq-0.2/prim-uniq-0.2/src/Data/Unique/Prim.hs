{-# LANGUAGE Trustworthy #-}
module Data.Unique.Prim
    ( Uniq, getUniq, RealWorld
    ) where

import Unsafe.Unique.Prim
import Control.Monad.Primitive (RealWorld)