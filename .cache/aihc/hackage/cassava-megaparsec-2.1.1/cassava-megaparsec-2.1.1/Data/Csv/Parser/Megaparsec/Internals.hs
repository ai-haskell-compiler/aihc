-- |
-- Module      :  Data.Csv.Parser.Megaparsec.Internals
-- Copyright   :  © 2016–2021 Stack Builders
-- License     :  MIT
--

{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Data.Csv.Parser.Megaparsec.Internals 
    ( ConversionError (..)
    , Parser
    , csv
    , csvWithHeader
    , decodeWithC
    , toNamedRecord
    , header
    , name
    , record
    , field
    , escapedField
    , unescapedField)
where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Csv hiding
  ( Parser
  , record
  , header
  , toNamedRecord )
import Data.Data
import Data.Vector (Vector)
import Data.Word (Word8)
import Text.Megaparsec
import Text.Megaparsec.Byte
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv             as C
import qualified Data.HashMap.Strict  as H
import qualified Data.Vector          as V

----------------------------------------------------------------------------
-- Custom error component and other types

-- | Custom error component for CSV parsing. It allows typed reporting of
-- conversion errors.

newtype ConversionError = ConversionError String
  deriving (Eq, Data, Typeable, Ord, Read, Show)

instance ShowErrorComponent ConversionError where
  showErrorComponent (ConversionError msg) =
    "conversion error: " ++ msg

-- | Parser type that uses “custom error component” 'ConversionError'.

type Parser = Parsec ConversionError BL.ByteString

----------------------------------------------------------------------------
-- The parser

-- | Parse a CSV file that does not include a header.

csv :: FromRecord a
  => DecodeOptions     -- ^ Decoding options
  -> Parser (Vector a) -- ^ The parser that parses collection of records
csv DecodeOptions {..} = do
  xs <- sepEndBy1 (record decDelimiter parseRecord) eol
  eof
  return $! V.fromList xs

-- | Parse a CSV file that includes a header.

csvWithHeader :: FromNamedRecord a
  => DecodeOptions     -- ^ Decoding options
  -> Parser (Header, Vector a)
     -- ^ The parser that parser collection of named records
csvWithHeader DecodeOptions {..} = do
  !hdr <- header decDelimiter
  let f = parseNamedRecord . toNamedRecord hdr
  xs   <- sepEndBy1 (record decDelimiter f) eol
  eof
  return $ let !v = V.fromList xs in (hdr, v)

-- | Decode CSV data using the provided parser, skipping a leading header if
-- necessary.

decodeWithC
  :: (DecodeOptions -> Parser a)
     -- ^ Parsing function parametrized by 'DecodeOptions'
  -> DecodeOptions
     -- ^ Decoding options
  -> HasHeader
     -- ^ Whether to expect a header in the input
  -> FilePath
     -- ^ File name (only for displaying in parse error messages, use empty
     -- string if you have none)
  -> BL.ByteString
     -- ^ CSV data
  -> Either (ParseErrorBundle BL.ByteString ConversionError) a
decodeWithC p opts@DecodeOptions {..} hasHeader = parse parser
  where
    parser = case hasHeader of
      HasHeader -> header decDelimiter *> p opts
      NoHeader  -> p opts
{-# INLINE decodeWithC #-}

-- | Convert a 'Record' to a 'NamedRecord' by attaching column names. The
-- 'Header' and 'Record' must be of the same length.

toNamedRecord :: Header -> Record -> NamedRecord
toNamedRecord hdr v = H.fromList . V.toList $ V.zip hdr v
{-# INLINE toNamedRecord #-}

-- | Parse a header, including the terminating line separator.

header :: Word8 -> Parser Header
header del = V.fromList <$!> p <* eol
  where
    p = sepBy1 (name del) (void $ char del) <?> "file header"
{-# INLINE header #-}

-- | Parse a header name. Header names have the same format as regular
-- 'field's.

name :: Word8 -> Parser Name
name del = field del <?> "name in header"
{-# INLINE name #-}

-- | Parse a record, not including the terminating line separator. The
-- terminating line separate is not included as the last record in a CSV
-- file is allowed to not have a terminating line separator.

record
  :: Word8             -- ^ Field delimiter
  -> (Record -> C.Parser a)
     -- ^ How to “parse” record to get the data of interest
  -> Parser a
record del f = do
  notFollowedBy eof -- to prevent reading empty line at the end of file
  r <- V.fromList <$!> (sepBy1 (field del) (void $ char del) <?> "record")
  case C.runParser (f r) of
    Left msg -> customFailure (ConversionError msg)
    Right x  -> return x
{-# INLINE record #-}

-- | Parse a field. The field may be in either the escaped or non-escaped
-- format. The returned value is unescaped.

field :: Word8 -> Parser Field
field del = label "field" (escapedField <|> unescapedField del)
{-# INLINE field #-}

-- | Parse an escaped field.

escapedField :: Parser ByteString
escapedField =
  B.pack <$!> between (char 34) (char 34) (many $ normalChar <|> escapedDq)
  where
    normalChar = anySingleBut 34 <?> "unescaped character"
    escapedDq  = label "escaped double-quote" (34 <$ string "\"\"")
{-# INLINE escapedField #-}

-- | Parse an unescaped field.

unescapedField :: Word8 -> Parser ByteString
unescapedField del = BL.toStrict <$> takeWhileP (Just "unescaped character") f
  where
    f x = x /= del && x /= 34 && x /= 10 && x /= 13
{-# INLINE unescapedField #-}