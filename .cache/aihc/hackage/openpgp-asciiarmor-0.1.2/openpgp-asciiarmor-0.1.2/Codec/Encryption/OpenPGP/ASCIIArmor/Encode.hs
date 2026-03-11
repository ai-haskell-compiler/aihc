-- ASCIIArmor/Encode.hs: OpenPGP (RFC4880) ASCII armor implementation
-- Copyright © 2012-2018  Clint Adams
-- This software is released under the terms of the Expat license.
-- (See the LICENSE file).

module Codec.Encryption.OpenPGP.ASCIIArmor.Encode (
   encode
 , encodeLazy
) where

import Codec.Encryption.OpenPGP.ASCIIArmor.Types
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC8
import qualified Data.ByteString.Base64 as Base64
import Data.Digest.CRC24 (crc24Lazy)
import Data.Binary.Put (runPut, putWord32be)

encode :: [Armor] -> B.ByteString
encode = B.concat . BL.toChunks . encodeLazy

encodeLazy :: [Armor] -> ByteString
encodeLazy = BL.concat . map armor

armor :: Armor -> ByteString
armor (Armor atype ahs bs) = beginLine atype `BL.append` armorHeaders ahs `BL.append` blankLine `BL.append` armorData bs `BL.append` armorChecksum bs `BL.append` endLine atype
armor (ClearSigned chs ctxt csig) = BLC8.pack "-----BEGIN PGP SIGNED MESSAGE-----\n" `BL.append` armorHeaders chs `BL.append` blankLine `BL.append` dashEscape ctxt `BL.append` armor csig

blankLine :: ByteString
blankLine = BLC8.singleton '\n'

beginLine :: ArmorType -> ByteString
beginLine atype = BLC8.pack "-----BEGIN PGP " `BL.append` aType atype `BL.append` BLC8.pack "-----\n"

endLine :: ArmorType -> ByteString
endLine atype = BLC8.pack "-----END PGP " `BL.append` aType atype `BL.append` BLC8.pack "-----\n"

aType :: ArmorType -> ByteString
aType ArmorMessage = BLC8.pack "MESSAGE"
aType ArmorPublicKeyBlock = BLC8.pack "PUBLIC KEY BLOCK"
aType ArmorPrivateKeyBlock = BLC8.pack "PRIVATE KEY BLOCK"
aType (ArmorSplitMessage x y) = BLC8.pack $ "MESSAGE, PART " ++ show x ++ "/" ++ show y
aType (ArmorSplitMessageIndefinite x) = BLC8.pack $ "MESSAGE, PART " ++ show x
aType ArmorSignature = BLC8.pack "SIGNATURE"

armorHeaders :: [(String, String)] -> ByteString
armorHeaders = BLC8.unlines . map armorHeader
    where
        armorHeader :: (String, String) -> ByteString
        armorHeader (k, v) = BLC8.pack k `BL.append` BLC8.pack ": " `BL.append` BLC8.pack v

armorData :: ByteString -> ByteString
armorData = BLC8.unlines . wordWrap 64 . BL.fromChunks . return . Base64.encode . B.concat . BL.toChunks

wordWrap :: Int -> ByteString -> [ByteString]
wordWrap lw bs
    | BL.null bs = []
    | lw < 1 || lw > 76 = wordWrap 76 bs
    | otherwise = BL.take (fromIntegral lw) bs : wordWrap lw (BL.drop (fromIntegral lw) bs)

armorChecksum :: ByteString -> ByteString
armorChecksum = BLC8.cons '=' . armorData . BL.tail . runPut . putWord32be . crc24Lazy

dashEscape :: ByteString -> ByteString
dashEscape = BLC8.unlines . map escapeLine . BLC8.lines
    where
        escapeLine :: ByteString -> ByteString
        escapeLine l
            | BLC8.singleton '-' `BL.isPrefixOf` l = BLC8.pack "- " `BL.append` l
            | BLC8.pack "From " `BL.isPrefixOf` l = BLC8.pack "- " `BL.append` l
            | otherwise = l
