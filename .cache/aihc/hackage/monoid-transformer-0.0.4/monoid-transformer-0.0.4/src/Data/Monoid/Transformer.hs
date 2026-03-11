module Data.Monoid.Transformer where

import Data.Monoid (Monoid, )

class C t where
   lift :: (Monoid m) => m -> t m
