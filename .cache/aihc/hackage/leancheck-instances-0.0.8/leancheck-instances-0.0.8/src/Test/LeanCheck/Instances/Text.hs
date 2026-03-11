-- |
-- Module      : Test.LeanCheck.Instances.Text
-- Copyright   : (c) 2018-2022 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Declares 'Listable' 'Text' instances.
module Test.LeanCheck.Instances.Text () where

import Data.Text
import qualified Data.Text.Lazy as Lazy
import Test.LeanCheck

instance Listable Text where
  tiers = cons1 pack

instance Listable Lazy.Text where
  tiers = cons1 Lazy.pack
