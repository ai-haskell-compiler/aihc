{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Binary.Instances.CaseInsensitive where

import Data.Binary         (Binary, get, put)
import Data.Binary.Orphans ()

import qualified Data.CaseInsensitive as CI

instance (CI.FoldCase a, Binary a) => Binary (CI.CI a) where
    get = fmap CI.mk get
    put = put . CI.foldedCase
