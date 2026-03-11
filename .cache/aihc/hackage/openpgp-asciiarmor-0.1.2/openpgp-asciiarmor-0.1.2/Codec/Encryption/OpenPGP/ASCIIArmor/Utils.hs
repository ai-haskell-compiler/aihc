-- ASCIIArmor/Utils.hs: OpenPGP (RFC4880) ASCII armor implementation
-- Copyright © 2012  Clint Adams
-- This software is released under the terms of the Expat license.
-- (See the LICENSE file).

module Codec.Encryption.OpenPGP.ASCIIArmor.Utils (
   crlfUnlines
 , crlfUnlinesLazy
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC8
import Data.List (intersperse)

crlfUnlines :: [ByteString] -> ByteString
crlfUnlines [] = B.empty
crlfUnlines ss = B.concat $ intersperse (BC8.pack "\r\n") ss

crlfUnlinesLazy :: [BL.ByteString] -> BL.ByteString
crlfUnlinesLazy [] = BL.empty
crlfUnlinesLazy ss = BL.concat $ intersperse (BLC8.pack "\r\n") ss
