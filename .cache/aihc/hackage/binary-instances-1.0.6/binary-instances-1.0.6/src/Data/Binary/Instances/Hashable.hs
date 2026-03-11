{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Binary.Instances.Hashable where

import Data.Binary.Orphans ()

import           Data.Binary   (Binary, get, put)
import qualified Data.Hashable as Hashable

instance (Hashable.Hashable a, Binary a) => Binary (Hashable.Hashed a) where
    get = fmap Hashable.hashed get
    put = put . Hashable.unhashed
