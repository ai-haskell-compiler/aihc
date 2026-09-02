{-# LANGUAGE RankNTypes #-}

module GHC.Base
  ( module GHC.Prim.Base,
    module GHC.Prim,
    unsafeChr,
    build,
    augment,
  )
where

import GHC.Char (unsafeChr)
import GHC.Prim
import GHC.Prim.Base

build :: (forall b. (a -> b -> b) -> b -> b) -> [a]
build generate = generate (:) []

augment :: (forall b. (a -> b -> b) -> b -> b) -> [a] -> [a]
augment generate = generate (:)
