{-# OPTIONS -fno-warn-orphans #-}

module Data.Function.Show
    (
    ) where

import Data.List
import Data.Searchable
import Prelude

instance (Show a, Finite a, Show b) => Show (a -> b) where
    show f = "{" ++ (intercalate "," (fmap (\a -> (show a) ++ "->" ++ (show (f a))) allValues)) ++ "}"
