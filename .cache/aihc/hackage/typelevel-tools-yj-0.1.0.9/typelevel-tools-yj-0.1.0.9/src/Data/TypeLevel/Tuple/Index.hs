{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.TypeLevel.Tuple.Index where

import Data.TypeLevel.Tuple.Index.TH

uncurry mkI `mapM` reverse [ (i, n) | n <- [2 .. 15], i <- [0 .. n - 1] ]

uncurry mkITup `mapM`
	(concatMap (\n -> (, n) <$> reverse (mTupIndices n)) $ reverse [3 .. 8])
