-- |
-- Module      : Text.LDAP.InternalParser
-- Copyright   : 2014 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- Module of internal share parsers.
module Text.LDAP.InternalParser
       ( LdapParser
       , satisfyW8

       , ldifSafeString
       ) where

import Control.Applicative ((<$>), (<*>), many)
import Data.Word (Word8)
import Data.ByteString (ByteString, pack)
import Data.Attoparsec.ByteString.Char8 (Parser, satisfy)

import Text.LDAP.Data (ordW8, inBounds)
import qualified Text.LDAP.Data as Data


type LdapParser = Parser

satisfyW8 :: (Char -> Bool) -> LdapParser Word8
satisfyW8 =  (ordW8 <$>) . satisfy

ldifSafeString :: LdapParser ByteString
ldifSafeString =
  (pack <$>)
  $ (:)
  <$> satisfyW8 (`inBounds` Data.ldifSafeInitBounds)
  <*> many (satisfyW8 (`inBounds` Data.ldifSafeBounds))
