-- |
-- Module      : Test.LeanCheck.Instances.Array
-- Copyright   : (c) 2019-2022 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Declares 'Listable' 'Array'
--
-- This 'Listable' instance imposes a 'Num' restriction on the index.
module Test.LeanCheck.Instances.Array () where

import Test.LeanCheck
import Data.Array

instance (Num i, Ix i, Listable i, Listable a) => Listable (Array i a) where
  tiers = cons2 mkArray
    where
    mkArray l xs = listArray (l, l + fromIntegral (length xs - 1)) xs
