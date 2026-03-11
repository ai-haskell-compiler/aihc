-- |
-- Module      : Test.LeanCheck.Instances.Natural
-- Copyright   : (c) 2018-2022 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Declares a 'Listable' 'Natural' instance.
module Test.LeanCheck.Instances.Natural () where

import Numeric.Natural
import Test.LeanCheck

instance Listable Natural where
  list  =  [0..]
