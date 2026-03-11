-- ASCIIArmor.hs: OpenPGP (RFC4880) ASCII armor implementation
-- Copyright © 2012  Clint Adams
-- This software is released under the terms of the Expat license.
-- (See the LICENSE file).

module Codec.Encryption.OpenPGP.ASCIIArmor (
   decode
 , decodeLazy
 , encode
 , encodeLazy
 , parseArmor
 , multipartMerge
) where

import Codec.Encryption.OpenPGP.ASCIIArmor.Decode (decode, decodeLazy, parseArmor)
import Codec.Encryption.OpenPGP.ASCIIArmor.Encode (encode, encodeLazy)
import Codec.Encryption.OpenPGP.ASCIIArmor.Multipart (multipartMerge)
