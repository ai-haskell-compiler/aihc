-- |
-- Module      :  Data.Csv.Parser.Megaparsec
-- Copyright   :  © 2016–2021 Stack Builders
-- License     :  MIT
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- A CSV parser. The parser here is RFC 4180 compliant, with the following
-- extensions:
--
--     * Non-escaped fields may contain any characters except double-quotes,
--       commas (or generally delimiter characters), carriage returns, and
--       newlines.
--     * Escaped fields may contain any characters, but double-quotes need
--       to be escaped.
--
-- The parser provides better error messages than the parser that comes with
-- Cassava library, while being compatible with the rest of the library.

module Data.Csv.Parser.Megaparsec
  ( ConversionError (..)
  , decode
  , decodeWith
  , decodeByName
  , decodeByNameWith)
where

import Data.Csv hiding
  ( decode
  , decodeWith
  , decodeByName
  , decodeByNameWith )
import Data.Vector (Vector)
import Text.Megaparsec
import qualified Data.ByteString.Lazy as BL
import Data.Csv.Parser.Megaparsec.Internals
    ( ConversionError (..)
    , csv
    , csvWithHeader
    , decodeWithC)

----------------------------------------------------------------------------
-- Top level interface

-- | Deserialize CSV records form a lazy 'BL.ByteString'. If this fails due
-- to incomplete or invalid input, 'Left' is returned. Equivalent to
-- 'decodeWith' 'defaultDecodeOptions'.

decode :: FromRecord a
  => HasHeader
     -- ^ Whether the data contains header that should be skipped
  -> FilePath
     -- ^ File name (only for displaying in parse error messages, use empty
     -- string if you have none)
  -> BL.ByteString
     -- ^ CSV data
  -> Either (ParseErrorBundle BL.ByteString ConversionError) (Vector a)
decode = decodeWith defaultDecodeOptions
{-# INLINE decode #-}

-- | Like 'decode', but lets you customize how the CSV data is parsed.

decodeWith :: FromRecord a
  => DecodeOptions
     -- ^ Decoding options
  -> HasHeader
     -- ^ Whether the data contains header that should be skipped
  -> FilePath
     -- ^ File name (only for displaying in parse error messages, use empty
     -- string if you have none)
  -> BL.ByteString
     -- ^ CSV data
  -> Either (ParseErrorBundle BL.ByteString ConversionError) (Vector a)
decodeWith = decodeWithC csv
{-# INLINE decodeWith #-}

-- | Deserialize CSV records from a lazy 'BL.ByteString'. If this fails due
-- to incomplete or invalid input, 'Left' is returned. The data is assumed
-- to be preceded by a header. Equivalent to 'decodeByNameWith'
-- 'defaultDecodeOptions'.

decodeByName :: FromNamedRecord a
  => FilePath
     -- ^ File name (only for displaying in parse error messages, use empty
     -- string if you have none)
  -> BL.ByteString
     -- ^ CSV data
  -> Either (ParseErrorBundle BL.ByteString ConversionError) (Header, Vector a)
decodeByName = decodeByNameWith defaultDecodeOptions
{-# INLINE decodeByName #-}

-- | Like 'decodeByName', but lets you customize how the CSV data is parsed.

decodeByNameWith :: FromNamedRecord a
  => DecodeOptions
     -- ^ Decoding options
  -> FilePath
     -- ^ File name (only for displaying in parse error messages, use empty
     -- string if you have none)
  -> BL.ByteString
     -- ^ CSV data
  -> Either (ParseErrorBundle BL.ByteString ConversionError) (Header, Vector a)
decodeByNameWith opts = parse (csvWithHeader opts)
{-# INLINE decodeByNameWith #-}