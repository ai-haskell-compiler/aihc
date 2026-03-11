-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.ByteString.Conversion.From
    ( FromByteString (..)
    , fromByteString
    , fromByteString'
    , runParser
    , runParser'
    ) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (signed, decimal, double, hexadecimal)
import Data.Bits (Bits)
import Data.ByteString (ByteString, elem)
import Data.ByteString.Conversion.Internal
import Data.CaseInsensitive (CI, FoldCase, mk)
import Data.Int
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Word
import Prelude hiding (elem)

import qualified Data.Attoparsec.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy            as Lazy
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as TL
import qualified Data.Text.Lazy.Encoding         as TL

-- | Parse 'ByteString's.
class FromByteString a where
    parser :: Parser a

fromByteString :: FromByteString a => ByteString -> Maybe a
fromByteString = either (const Nothing) Just . runParser parser

fromByteString' :: FromByteString a => Lazy.ByteString -> Maybe a
fromByteString' = either (const Nothing) Just . runParser' parser

runParser :: Parser a -> ByteString -> Either String a
runParser p b = case feed (parse p b) "" of
    Done ""   r -> Right r
    Done _    _ -> Left "Trailing input"
    Fail _ [] m -> Left m
    Fail _ x  m -> Left (shows x . showString m $ "")
    Partial _   -> Left "Unexpected result: Partial"

runParser' :: Parser a -> Lazy.ByteString -> Either String a
runParser' p b = case Lazy.parse p b of
    Lazy.Done ""   r -> Right r
    Lazy.Done _    _ -> Left "Trailing input"
    Lazy.Fail _ [] m -> Left m
    Lazy.Fail _ x  m -> Left (shows x . showString m $ "")

-----------------------------------------------------------------------------
-- Instances

instance FromByteString ByteString where
    parser = takeByteString

instance FromByteString Lazy.ByteString where
    parser = takeLazyByteString

-- | A (flat) comma-separated list of values without spaces.
instance FromByteString a => FromByteString (List a) where
    parser = parseList

instance (FoldCase a, FromByteString a) => FromByteString (CI a) where
    parser = mk <$> parser

-- | UTF-8 is assumed as encoding format.
instance FromByteString Char where
    parser = do
        c <- text =<< takeByteString
        if T.length c /= 1
            then fail "Invalid Char"
            else return $ T.head c

-- | UTF-8 is assumed as encoding format.
instance FromByteString [Char] where
    parser = takeByteString >>= fmap T.unpack . text

-- | UTF-8 is assumed as encoding format.
instance FromByteString Text where
    parser = takeByteString >>= text

-- | UTF-8 is assumed as encoding format.
instance FromByteString TL.Text where
    parser = takeLazyByteString >>= ltext

instance FromByteString Bool where
    parser =
        satisfy (`elem` "tT") *> string "rue"  *> pure True  <|>
        satisfy (`elem` "fF") *> string "alse" *> pure False <|>
        fail "Invalid Bool"

instance FromByteString Double where
    parser = signed double <|> fail "Invalid Double"

instance FromByteString Integer where
    parser = signed decimal <|> fail "Invalid Integer"

instance FromByteString Int where
    parser = signed decimal <|> fail "Invalid Int"

instance FromByteString Int8 where
    parser = signed decimal <|> fail "Invalid Int8"

instance FromByteString Int16 where
    parser = signed decimal <|> fail "Invalid Int16"

instance FromByteString Int32 where
    parser = signed decimal <|> fail "Invalid Int32"

instance FromByteString Int64 where
    parser = signed decimal <|> fail "Invalid Int64"

instance FromByteString Word where
    parser = signed decimal <|> fail "Invalid Word"

instance FromByteString Word8 where
    parser = signed decimal <|> fail "Invalid Word8"

instance FromByteString Word16 where
    parser = signed decimal <|> fail "Invalid Word16"

instance FromByteString Word32 where
    parser = signed decimal <|> fail "Invalid Word32"

instance FromByteString Word64 where
    parser = signed decimal <|> fail "Invalid Word64"

instance (Integral a, Bits a) => FromByteString (Hex a) where
    parser = Hex <$> signed (optional prefix *> hexadecimal)
      where
        prefix = word8 0x30 *> satisfy (`elem` "xX")

-----------------------------------------------------------------------------
-- Implementation Helpers

parseList :: FromByteString a => Parser (List a)
parseList = atEnd >>= \e ->
    if e then return $ List []
         else List . reverse <$> go []
  where
    go acc = do
        x <- takeTill (== 0x2C)
        v <- case runParser parser x of
                Left  s -> fail s
                Right a -> return a
        c <- optional (word8 0x2C)
        e <- atEnd
        case (e, isJust c) of
            (True,  True)  -> fail "trailing comma"
            (True,  False) -> return (v:acc)
            (False, True)  -> go (v:acc)
            (False, False) -> fail "missing comma"

text :: ByteString -> Parser Text
text = either (fail . ("Invalid UTF-8: " ++) . show) return . decodeUtf8'
{-# INLINE text #-}

ltext :: Lazy.ByteString -> Parser TL.Text
ltext = either (fail . ("Invalid UTF-8: " ++) . show) return . TL.decodeUtf8'
{-# INLINE ltext #-}
