-- ASCIIArmor/Multipart.hs: OpenPGP (RFC4880) ASCII armor implementation
-- Copyright © 2012  Clint Adams
-- This software is released under the terms of the Expat license.
-- (See the LICENSE file).

module Codec.Encryption.OpenPGP.ASCIIArmor.Multipart (
   multipartMerge
) where

import Codec.Encryption.OpenPGP.ASCIIArmor.Types

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL

multipartMerge :: [Armor] -> Armor
multipartMerge as' = go as' (Armor ArmorMessage [] BL.empty)
    where
        go :: [Armor] -> Armor -> Armor
        go [] state = state
        go (Armor at hs bs:as) state = go as (go' at hs bs state)
        go _ _ = error "This shouldn't happen."
        go' :: ArmorType -> [(String,String)] -> ByteString -> Armor -> Armor
        go' (ArmorSplitMessage _ _) hs bs (Armor _ ohs obs) = Armor ArmorMessage (ohs ++ hs) (obs `BL.append` bs)
        go' (ArmorSplitMessageIndefinite _) hs bs (Armor _ ohs obs) = Armor ArmorMessage (ohs ++ hs) (obs `BL.append` bs)
        go' _ _ _ state = state
