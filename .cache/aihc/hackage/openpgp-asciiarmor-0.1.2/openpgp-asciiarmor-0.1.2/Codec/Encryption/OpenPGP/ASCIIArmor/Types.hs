-- ASCIIArmor/Decode.hs: OpenPGP (RFC4880) ASCII armor implementation
-- Copyright © 2012  Clint Adams
-- This software is released under the terms of the Expat license.
-- (See the LICENSE file).

module Codec.Encryption.OpenPGP.ASCIIArmor.Types (
   Armor(..)
 , ArmorType(..)
) where

import Data.ByteString.Lazy (ByteString)

data Armor = Armor ArmorType [(String, String)] ByteString
   | ClearSigned [(String, String)] ByteString Armor
    deriving (Show, Eq)

data ArmorType = ArmorMessage
               | ArmorPublicKeyBlock
               | ArmorPrivateKeyBlock
               | ArmorSplitMessage ByteString ByteString
               | ArmorSplitMessageIndefinite ByteString
               | ArmorSignature
    deriving (Show, Eq)
