{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Or (
	Or(..), or, lefts, rights, fromLeft, fromRight, partitionOrs ) where

import Prelude hiding (or)

import Control.Arrow (first, second, (***))

data Or a b = L a | R b | LR a b deriving (Show, Read, Eq, Ord)

or :: (a -> c) -> (b -> c) -> (a -> b -> c) -> Or a b -> c
or f g h = \case L x -> f x; R y -> g y; LR x y -> h x y

lefts :: [Or a b] -> [a]
lefts [] = []
lefts (L x : os) = x : lefts os
lefts (R _ : os) = lefts os
lefts (LR x _ : os) = x : lefts os

rights :: [Or a b] -> [b]
rights [] = []
rights (L _ : os) = rights os
rights (R y : os) = y : rights os
rights (LR _ y : os) = y : rights os

fromLeft :: a -> Or a b -> a
fromLeft _ (L x) = x
fromLeft d (R _) = d
fromLeft _ (LR x _) = x

fromRight :: b -> Or a b -> b
fromRight d (L _) = d
fromRight _ (R y) = y
fromRight _ (LR _ y) = y

partitionOrs :: [Or a b] -> ([a], [b])
partitionOrs [] = ([], [])
partitionOrs (L x : os) = (x :) `first` partitionOrs os
partitionOrs (R y : os) = (y :) `second` partitionOrs os
partitionOrs (LR x y : os) = (x :) *** (y :) $ partitionOrs os
