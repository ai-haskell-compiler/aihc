{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Text.LDAP.Printer
-- Copyright   : 2014-2018 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
module Text.LDAP.Printer
       ( LdapPrinter, runLdapPrinter, LdapPutM

       , dn
       , component
       , attribute

       , ldifDN, ldifAttr

       , ldifAttrValue, ldifEncodeAttrValue

       , openLdapEntry, openLdapData
       ) where

import Prelude hiding (reverse)
import Data.Monoid (Endo (..))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Char (chr, isAscii, isPrint)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (ByteString, singleton)
import qualified Data.ByteString.Lazy as LB
import Control.Applicative ((<*))
import Control.Monad.Trans.Writer (Writer, tell, execWriter)
import Text.Printf (printf)
import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import Data.Attoparsec.ByteString (parseOnly, endOfInput)

import Text.LDAP.Data
  (AttrType (..), AttrValue (..), Attribute,
   Component (..), DN, unconsDN,
   LdifAttrValue (..),
   elem', ordW8)
import qualified Text.LDAP.Data as Data
import Text.LDAP.InternalParser (ldifSafeString)


-- | Printer context type for LDAP data stream
type LdapPutM = Writer (Endo [ByteString])

-- | 'LdapPrinter' 'a' print type 'a' into context
type LdapPrinter a = a -> LdapPutM ()

-- | Run 'LdapPrinter'
runLdapPrinter :: LdapPrinter a -> a -> LB.ByteString
runLdapPrinter p = LB.fromChunks . (`appEndo` []) . execWriter . p

string :: LdapPrinter ByteString
string =  tell . Endo . (:)

bslash :: Word8
bslash =  ordW8 '\\'

chrW8 :: Word8 -> Char
chrW8 =  chr . fromIntegral

escapeValueChar :: Word8 -> [Word8]
escapeValueChar w
  | not $ isAscii c                       =  hex
  | w `elem'` echars                      =  [bslash, w]
  | c == '\r' || c == '\n'                =  hex
  | isPrint c                             =  [w]
  | otherwise                             =  hex
  where c      = chrW8 w
        echars = bslash : Data.quotation : Data.specialChars
        hex    = (bslash :) . map ordW8 $ printf "%02x" w

_testEscape :: IO ()
_testEscape =
  putStr $ unlines [ show (w, map chrW8 $ escapeValueChar w) | w <- [0 .. 255 ] ]

escapeValueBS :: ByteString -> ByteString
escapeValueBS =  BS.pack . concatMap escapeValueChar . BS.unpack

char :: LdapPrinter Char
char =  string . singleton

newline :: LdapPutM ()
newline =  char '\n'

-- DN
attrType :: LdapPrinter AttrType
attrType =  d  where
  d (AttrType s)         =  string s
  d (AttrOid (x :| xs))  =  do
    string x
    mapM_ (\x' ->  char '.' >> string x') xs

attrValue :: LdapPrinter AttrValue
attrValue (AttrValue s) =  string . escapeValueBS $ s

-- | Printer of attribute pair string in RDN.
attribute :: LdapPrinter Attribute
attribute (t, v) =  do
  attrType  t
  char '='
  attrValue v

-- | Printer of RDN string.
component :: LdapPrinter Component
component =  d  where
  d (S a)          =  attribute a
  d (L (a :| as))  =  do
    attribute a
    mapM_ (\a' -> char '+' >> attribute a') as

-- | Printer of DN string.
dn :: LdapPrinter DN
dn =  d . unconsDN where
  d (c, cs)  =  do
    component c
    mapM_ (\c' -> char ',' >> component c') cs


-- LDIF

-- | Printer of LDIF DN line.
ldifDN :: LdapPrinter DN
ldifDN x = do
  string "dn: "
  dn x

-- | Printer of LDIF attribute value already encoded.
--   Available printer combinator to pass 'ldifAttr' or 'openLdapEntry', etc ...
ldifAttrValue :: LdapPrinter LdifAttrValue
ldifAttrValue = d  where
  d (LAttrValRaw s)    = do
    char ' '
    string s
  d (LAttrValBase64 s) = do
    string ": "
    string s

ldifToSafeAttrValue :: AttrValue -> LdifAttrValue
ldifToSafeAttrValue (AttrValue s) = do
  case parseOnly (ldifSafeString <* endOfInput) $ s of
    Right _    ->  LAttrValRaw s
    Left  _    ->  LAttrValBase64 $ convertToBase Base64 s

-- | Printer of LDIF attribute value with encode not safe string.
--   Available printer combinator to pass 'ldifAttr' or 'openLdapEntry', etc ...
ldifEncodeAttrValue :: LdapPrinter AttrValue
ldifEncodeAttrValue =  ldifAttrValue . ldifToSafeAttrValue

-- | Printer of LDIF attribute pair line.
--   Use with 'ldifAttrValue' or 'ldifEncodeAttrValue' printer, like @ldifAttr ldifEncodeAttrValue@.
ldifAttr :: LdapPrinter v -> LdapPrinter (AttrType, v)
ldifAttr vp (a, v) = do
  attrType a
  char ':'
  vp v

-- | OpenLDAP data-stream block printer.
--   Use with 'ldifAttrValue' or 'ldifEncodeAttrValue' printer, like @openLdapEntry ldifEncodeAttrValue@.
openLdapEntry :: LdapPrinter v -> LdapPrinter (DN, [(AttrType, v)])
openLdapEntry vp (x, as) = do
  ldifDN x
  newline
  mapM_ ((>> newline) . ldifAttr vp) as

-- | OpenLDAP data-stream block list printer.
--   Use with 'ldifAttrValue' or 'ldifEncodeAttrValue' printer, like @openLdapData ldifEncodeAttrValue@.
openLdapData :: LdapPrinter v -> LdapPrinter [(DN, [(AttrType, v)])]
openLdapData vp = mapM_ ((>> newline) . openLdapEntry vp)
