
module Test.Feat.Class {-# DEPRECATED "Use Control.Enumerable instead" #-}
  ( Enumerable(..)
  , nullary
  , unary
  , funcurry
  , shared
  , consts
  , deriveEnumerable
  ) where

import Control.Enumerable

-- compatability
{-# DEPRECATED nullary "use c0 instead" #-}
nullary :: Sized f => a -> Shareable f a
nullary x = c0 x

{-# DEPRECATED unary "use c1 instead" #-}
unary :: (Sized f, Enumerable a, Typeable f) => (a -> x) -> Shareable f x
unary x = c1 x

{-# DEPRECATED shared "use access instead" #-}
shared :: (Sized f, Enumerable a, Typeable f) => Shareable f a
shared = access


funcurry :: (a -> b -> c) -> (a,b) -> c
funcurry = uncurry

{-# DEPRECATED consts "use datatype instead" #-}
consts :: (Sized f, Typeable f, Typeable a) => [Shareable f a] -> Shared f a
consts xs = datatype xs
