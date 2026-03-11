{-# LANGUAGE OverloadedStrings #-}
-- ASCIIArmor/Decode.hs: OpenPGP (RFC4880) ASCII armor implementation
-- Copyright © 2012-2018  Clint Adams
-- This software is released under the terms of the Expat license.
-- (See the LICENSE file).

module Codec.Encryption.OpenPGP.ASCIIArmor.Decode (
   parseArmor
 , decode
 , decodeLazy
) where

import Codec.Encryption.OpenPGP.ASCIIArmor.Types
import Codec.Encryption.OpenPGP.ASCIIArmor.Utils
import Control.Applicative (many, (<|>), (<$>), (<*), (<*>), (*>), optional)
import Data.Attoparsec.ByteString (Parser, many1, string, inClass, notInClass, satisfy, word8, (<?>))
import qualified Data.Attoparsec.ByteString as AS
import qualified Data.Attoparsec.ByteString.Lazy as AL
import Data.Attoparsec.ByteString.Char8 (isDigit_w8, anyChar)
import Data.Bits (shiftL)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Base64 as Base64
import Data.Digest.CRC24 (crc24)
import Data.Binary.Get (Get, runGetOrFail, getWord8)
import Data.Functor (($>))
import Data.String (IsString, fromString)
import Data.Word (Word32)

decode :: IsString e => B.ByteString -> Either e [Armor]
decode bs = go (AS.parse parseArmors bs)
    where
        go (AS.Fail _ _ e) = Left (fromString e)
        go (AS.Partial cont) = go (cont B.empty)
        go (AS.Done _ r) = Right r

decodeLazy :: IsString e => BL.ByteString -> Either e [Armor]
decodeLazy bs = go (AL.parse parseArmors bs)
    where
        go (AL.Fail _ _ e) = Left (fromString e)
        go (AL.Done _ r) = Right r

parseArmors :: Parser [Armor]
parseArmors = many parseArmor

parseArmor :: Parser Armor
parseArmor = prefixed (clearsigned <|> armor) <?> "armor"

clearsigned :: Parser Armor
clearsigned = do
    _ <- string "-----BEGIN PGP SIGNED MESSAGE-----" <?> "clearsign header"
    _ <- lineEnding <?> "line ending"
    headers <- armorHeaders <?> "clearsign headers"
    _ <- blankishLine <?> "blank line"
    cleartext <- dashEscapedCleartext
    sig <- armor
    return $ ClearSigned headers cleartext sig

armor :: Parser Armor
armor = do
    atype <- beginLine <?> "begin line"
    headers <- armorHeaders <?> "headers"
    _ <- blankishLine <?> "blank line"
    payload <- base64Data <?> "base64 data"
    _ <- endLine atype <?> "end line"
    return $ Armor atype headers payload

beginLine :: Parser ArmorType
beginLine = do
    _ <- string "-----BEGIN PGP " <?> "leading minus-hyphens"
    atype <- pubkey <|> privkey <|> parts <|> message <|> signature
    _ <- string "-----" <?> "trailing minus-hyphens"
    _ <- many (satisfy (inClass " \t")) <?> "whitespace"
    _ <- lineEnding <?> "line ending"
    return atype
    where
        message = string "MESSAGE" $> ArmorMessage
        pubkey = string "PUBLIC KEY BLOCK" $> ArmorPublicKeyBlock
        privkey = string "PRIVATE KEY BLOCK" $> ArmorPrivateKeyBlock
        signature = string "SIGNATURE" $> ArmorSignature
        parts = string "MESSAGE, PART " *> (partsdef <|> partsindef)
        partsdef = do
            firstnum <- num
            _ <- word8 (fromIntegral . fromEnum $ '/')
            secondnum <- num
            return $ ArmorSplitMessage (BL.pack firstnum) (BL.pack secondnum)
        partsindef = ArmorSplitMessageIndefinite . BL.pack <$> num
        num = many1 (satisfy isDigit_w8) <?> "number"

lineEnding :: Parser B.ByteString
lineEnding = string "\n" <|> string "\r\n"

armorHeaders :: Parser [(String, String)]
armorHeaders = many armorHeader

armorHeader :: Parser (String, String)
armorHeader = do
    key <- many1 (satisfy (inClass "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))
    _ <- string ": "
    val <- many1 (satisfy (notInClass "\n\r"))
    _ <- lineEnding
    return (w8sToString key, w8sToString val)
    where
        w8sToString = BC8.unpack . B.pack

blankishLine ::  Parser B.ByteString
blankishLine = many (satisfy (inClass " \t")) *> lineEnding

endLine :: ArmorType -> Parser B.ByteString
endLine atype = do
    _ <- string $ "-----END PGP " `B.append` aType atype `B.append` "-----"
    lineEnding

aType :: ArmorType -> B.ByteString
aType ArmorMessage = BC8.pack "MESSAGE"
aType ArmorPublicKeyBlock = BC8.pack "PUBLIC KEY BLOCK"
aType ArmorPrivateKeyBlock = BC8.pack "PRIVATE KEY BLOCK"
aType (ArmorSplitMessage x y) = BC8.pack "MESSAGE, PART " `B.append` l2s x `B.append` BC8.singleton '/' `B.append` l2s y
aType (ArmorSplitMessageIndefinite x) = BC8.pack "MESSAGE, PART " `B.append` l2s x
aType ArmorSignature = BC8.pack "SIGNATURE"

l2s :: BL.ByteString -> B.ByteString
l2s = B.concat . BL.toChunks

base64Data :: Parser ByteString
base64Data = do
    ls <- many1 base64Line
    cksum <- checksumLine
    let payload = B.concat ls
    let ourcksum = crc24 payload
    case runGetOrFail d24 (BL.fromStrict cksum) of
        Left (_,_,err) -> fail err
        Right (_,_,theircksum) -> if theircksum == ourcksum then return (BL.fromStrict payload) else fail ("CRC24 mismatch: " ++ show (B.unpack cksum) ++ "/" ++ show theircksum ++ " vs. " ++ show ourcksum)
    where
        base64Line :: Parser B.ByteString
        base64Line = do
                        b64 <- many1 (satisfy (inClass "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
                        pad <- many (word8 (fromIntegral . fromEnum $ '='))
                        _ <- lineEnding
                        let line = B.pack b64 `B.append` B.pack pad
                        case Base64.decode line of
                            Left err -> fail err
                            Right bs -> return bs
        checksumLine :: Parser B.ByteString
        checksumLine = do
                        _ <- word8 (fromIntegral . fromEnum $ '=')
                        b64 <- many1 (satisfy (inClass "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
                        _ <- lineEnding
                        let line = B.pack b64
                        case Base64.decode line of
                            Left err -> fail err
                            Right bs -> return bs


d24 :: Get Word32
d24 = do
    a <- getWord8
    b <- getWord8
    c <- getWord8
    return $ shiftL (fromIntegral a :: Word32) 16 + shiftL (fromIntegral b :: Word32) 8 + (fromIntegral c :: Word32)

prefixed :: Parser a -> Parser a
prefixed end = end <|> anyChar *> prefixed end

dashEscapedCleartext :: Parser ByteString
dashEscapedCleartext = do
    ls <- many1 ((deLine <|> unescapedLine) <* lineEnding)
    return . BL.fromStrict $ crlfUnlines ls
    where
        deLine :: Parser B.ByteString
        deLine = B.pack <$> (string "- " *> many (satisfy (notInClass "\n\r")))
        unescapedLine :: Parser B.ByteString
        unescapedLine = maybe B.empty B.pack <$> optional ((:) <$> satisfy (notInClass "-\n\r") <*> many (satisfy (notInClass "\n\r")))
