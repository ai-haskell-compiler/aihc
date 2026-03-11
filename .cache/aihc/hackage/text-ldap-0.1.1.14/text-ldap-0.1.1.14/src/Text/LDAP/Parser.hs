{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Text.LDAP.Parser
-- Copyright   : 2014-2018 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
module Text.LDAP.Parser
       ( LdapParser, runLdapParser

       , dn, component, attribute

       , ldifDN, ldifAttr

       , openLdapEntry, openLdapData
       , openLdapDataBlocks

       , ldifDecodeAttrValue, ldifAttrValue

       , ldifDecodeB64Value
       ) where

import Control.Applicative
  ((<$>), pure, (<*>), (*>), (<*), (<|>), some, many)
import Numeric (readHex)
import Data.Monoid ((<>))
import Data.Word (Word8)
import Data.ByteString (ByteString, pack)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Attoparsec.ByteString.Char8
  (Parser, satisfy, isAlpha_ascii, char, char8, digit)
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.Attoparsec.ByteString as APW
import Data.Attoparsec.ByteString.Lazy (parse, eitherResult)
import Data.ByteArray.Encoding (Base (Base64), convertFromBase)

import Text.LDAP.Data
  (AttrType (..), AttrValue (..), Attribute, Component, DN, LdifAttrValue (..),
   ordW8, exact, inBounds, elem', notElem')
import qualified Text.LDAP.Data as Data
import Text.LDAP.InternalParser (satisfyW8, ldifSafeString)
import qualified Text.LDAP.InternalParser as Internal


-- | Parser context type for LDAP data stream
type LdapParser = Internal.LdapParser

-- | Run 'LdapParser' context.
runLdapParser :: Parser a -> LB.ByteString -> Either String a
runLdapParser p = eitherResult . parse (p <* AP.endOfInput)


spaces :: LdapParser ()
spaces =  many (char ' ') *> pure ()

alpha :: LdapParser Char
alpha =  satisfy isAlpha_ascii

alphaW8 :: LdapParser Word8
alphaW8 =  ordW8 <$> alpha

digitW8 :: LdapParser Word8
digitW8 =  ordW8 <$> digit

quotation :: LdapParser Word8
quotation =  APW.word8 Data.quotation

digits1' :: LdapParser ByteString
digits1' =  pack <$> some digitW8


-- DN
keychar :: LdapParser Word8
keychar =  alphaW8 <|> digitW8 <|> char8 '-'

quotechar :: LdapParser Word8
quotechar =  APW.satisfy (`notElem'` [ordW8 '\\', Data.quotation])

special :: LdapParser Word8
special =  APW.satisfy (`elem'` Data.specialChars)

stringchar :: LdapParser Word8
stringchar =  APW.satisfy (`notElem'` map ordW8 ['\r', '\n', '\\'] ++ Data.quotation : Data.specialChars)

hexchar :: LdapParser Char
hexchar =  digit <|> satisfy (`inBounds` [('a', 'f'), ('A', 'F')])


hexpair :: LdapParser Word8
hexpair =  (rh <$>) $ (:) <$> hexchar <*> ((:) <$> hexchar <*> pure [])  where
  rh s
    | rs == []   =  error $ "hexpair: BUG!: fail to read hex: " ++ s
    | otherwise  =  fst $ head rs
    where rs = readHex s

pair :: LdapParser Word8
pair =  char '\\' *> (
  special       <|>
  char8 '\\'    <|>
  quotation     <|>
  hexpair )


hexstring :: LdapParser ByteString
hexstring =  pack <$> some hexpair

string :: LdapParser ByteString
string =  pack <$> some (stringchar <|> pair)  <|>
          char '#' *> hexstring                <|>
          pack <$> (quotation *> many (quotechar <|> pair) <* quotation) {- Only for v2 -} <|>
          pure ""

_testString :: Either String ByteString
_testString =  runLdapParser string "\",\""

attrOid :: LdapParser AttrType
attrOid =  Data.attrOid <$> digits1' <*> many (char '.' *> digits1')

attrTypeStr :: LdapParser AttrType
attrTypeStr =  (Data.AttrType . pack <$>) $ (:) <$> alphaW8 <*> many keychar

attrType :: LdapParser AttrType
attrType =  attrTypeStr <|> attrOid

_testAT :: Either String AttrType
_testAT =  runLdapParser attrType "dc"

attrValueString :: LdapParser AttrValue
attrValueString =  AttrValue <$> string

_testAV :: Either String AttrValue
_testAV =  runLdapParser attrValueString "com"

-- | Parser of attribute pair string in RDN.
attribute :: LdapParser Attribute
attribute =  (,)
             <$> (attrType <* char '=')
             <*>  attrValueString

_testAttr :: Either String Attribute
_testAttr =  runLdapParser attribute "dc=com"

-- | Parser of RDN string.
component :: LdapParser Component
component =  Data.component <$> attribute <*> many (char '+' *> attribute)

comma :: LdapParser Char
comma =  spaces *> (char ',' <|> char ';') <* spaces

-- | Parser of DN string.
dn :: LdapParser DN
dn =  Data.consDN <$> component <*> many (comma *> component)



-- LDIF
fill :: LdapParser ()
fill =  spaces

base64Bounds :: [(Char, Char)]
base64Bounds =  [('A', 'Z'), ('a', 'z'), ('0', '9'), exact '+', exact '/', exact '=']

base64String :: LdapParser ByteString
base64String =  pack <$> many (satisfyW8 (`inBounds` base64Bounds))

padDecodeB64 :: ByteString -> Either String ByteString
padDecodeB64 s = fromB64 (s <> pad)  where
  pad = BS8.replicate ((- BS8.length s) `mod` 4) '='
  fromB64 "" = Right ""      -- avoid bug of older than memory-0.14.14
  fromB64 bs  = convertFromBase Base64 bs

eitherParser :: String -> Either String a -> LdapParser a
eitherParser s = either (fail . ((s ++ ": ") ++)) pure

decodeBase64 :: ByteString -> LdapParser ByteString
decodeBase64 =  eitherParser "internal decodeBase64" . padDecodeB64

parseDN :: ByteString -> LdapParser DN
parseDN s =
  eitherParser "internal parseDN"
    . runLdapParser dn $ LB.fromChunks [s]

-- | Parser of LDIF DN line.
ldifDN :: LdapParser DN
ldifDN =  AP.string "dn:" *> (
  fill *> dn                                              <|>
  char ':' *> fill *> (parseDN =<< decodeBase64 =<< base64String)
  )

-- | Parser of LDIF attribute value which may be base64 encoded.
--   Available combinator parser to pass 'ldifAttr' or 'openLdapEntry', etc ...
ldifAttrValue :: Parser LdifAttrValue
ldifAttrValue =
  fill             *> (LAttrValRaw    <$> ldifSafeString)  <|>
  char ':' *> fill *> (LAttrValBase64 <$> base64String)    <|>
  fill             *> pure (LAttrValRaw "")

-- | Decode value string of attribute pair after stream parsing.
ldifDecodeB64Value :: LdifAttrValue -> Either String AttrValue
ldifDecodeB64Value a = case a of
  LAttrValRaw    s -> Right $ AttrValue s
  LAttrValBase64 b -> AttrValue <$> padDecodeB64 b

-- | Parser of LDIF attribute value. This parser decodes base64 string.
--   Available combinator parser to pass 'ldifAttr' or 'openLdapEntry', etc ...
ldifDecodeAttrValue :: LdapParser AttrValue
ldifDecodeAttrValue =
  ldifAttrValue >>=
  eitherParser "internal ldifDecodeAttrValue" . ldifDecodeB64Value

-- | Parser of LDIF attribute pair line.
--   Use with 'ldifDecodeAttrValue' or 'ldifAttrValue' parser, like @ldifAttr ldifDecodeAttrValue@.
ldifAttr :: LdapParser a -> LdapParser (AttrType, a)
ldifAttr vp =
  (,)
  <$> (attrType <* char ':')
  <*> vp

newline :: LdapParser ByteString
newline =  AP.string "\n" <|> AP.string "\r\n"

-- | OpenLDAP data-stream block parser.
--   Use with 'ldifDecodeAttrValue' or 'ldifAttrValue' parser, like @openLdapEntry ldifDecodeAttrValue@.
openLdapEntry :: LdapParser a
              -> LdapParser (DN, [(AttrType, a)])
openLdapEntry dp =
  (,)
  <$> (ldifDN <* newline)
  <*> many (ldifAttr dp <* newline)

-- | OpenLDAP data-stream block list parser.
--   Use with 'ldifDecodeAttrValue' or 'ldifAttrValue' parser, like @openLdapData ldifDecodeAttrValue@.
openLdapData :: LdapParser a
             -> LdapParser [(DN, [(AttrType, a)])]
openLdapData dp =  many (openLdapEntry dp <* newline)

contLines :: [LB.ByteString] -> [LB.ByteString]
contLines =  d  where
  d  []    = []
  d (x:xs) = rec' x xs  where
    rec' a []     = [a]
    rec' a (y:ys)
      | hd == " "  =  rec' (a <> tl) ys
      | otherwise  =  a : rec' y ys
      where (hd, tl) = LB.splitAt 1 y

blocks :: [LB.ByteString] -> [[LB.ByteString]]
blocks =  d  where
  d     []     =  []
  d ls@(_:_)   =  hd : blocks (drop 1 tl)
    where  (hd,tl) = break (== "") ls

-- | Chunking lines of OpenLDAP data stream.
openLdapDataBlocks :: [LB.ByteString] -> [[LB.ByteString]]
openLdapDataBlocks =  map contLines . blocks

_test0 :: Either String DN
_test0 =  runLdapParser ldifDN "dn: cn=Slash\\\\The Post\\,ma\\=ster\\+\\<\\>\\#\\;,dc=example.sk,dc=com"
