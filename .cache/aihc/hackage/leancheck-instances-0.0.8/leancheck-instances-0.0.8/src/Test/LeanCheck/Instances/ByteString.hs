-- |
-- Module      : Test.LeanCheck.Instances.ByteString
-- Copyright   : (c) 2018-2022 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Instances of 'Listable' for 'ByteString'.
module Test.LeanCheck.Instances.ByteString () where

import Test.LeanCheck
import Data.ByteString.Lazy as Lazy
import Data.ByteString as Strict
import Data.Word

tiersBytes :: [[ Word8 ]]
tiersBytes = toTiers $ [0..127] +| [255,254..128]

tiersListBytes :: [[ [Word8] ]]
tiersListBytes = listsOf tiersBytes

instance Listable Lazy.ByteString where
  tiers = mapT Lazy.pack tiersListBytes

instance Listable Strict.ByteString where
  tiers = mapT Strict.pack tiersListBytes
