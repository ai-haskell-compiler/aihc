-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Redis.Resp
    ( Resp (..)
    , resp
    , encode
    , decode
    ) where

import Control.Applicative
import Control.Monad (replicateM)
import Data.Attoparsec.ByteString.Char8 hiding (char8)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder
import Data.Int
import Data.Monoid
import Prelude hiding (take, takeWhile)

import qualified Data.Attoparsec.ByteString.Lazy as P
import qualified Data.ByteString.Lazy            as Lazy

-- | 'Resp' defines the various RESP constructors.
data Resp
    = Str   !ByteString -- ^ RESP simple strings
    | Err   !ByteString -- ^ RESP errors
    | Int   !Int64      -- ^ RESP integers
    | Bulk  !ByteString -- ^ RESP bulk strings
    | Array !Int [Resp] -- ^ RESP arrays
    | NullArray
    | NullBulk
    deriving (Eq, Ord, Show)

decode :: ByteString -> Either String Resp
decode = P.eitherResult . P.parse resp

encode :: Resp -> Lazy.ByteString
encode d = toLazyByteString (go d)
  where
    go (Str   x) = char8 '+' <> lazyByteString x <> crlf'
    go (Err   x) = char8 '-' <> lazyByteString x <> crlf'
    go (Int   x) = char8 ':' <> int64Dec x <> crlf'
    go (Bulk  x) = char8 '$'
        <> int64Dec (Lazy.length x)
        <> crlf'
        <> lazyByteString x
        <> crlf'
    go (Array n x) = char8 '*'
        <> intDec n
        <> crlf'
        <> foldr (<>) mempty (map go x)
    go NullArray = nullArray
    go NullBulk  = nullBulk

-----------------------------------------------------------------------------
-- Parsing

-- | An attoparsec parser to parse a single 'Resp' value.
resp :: Parser Resp
resp = do
    t <- anyChar
    case t of
        '+' -> Str `fmap` bytes       <* crlf
        '-' -> Err `fmap` bytes       <* crlf
        ':' -> Int <$> signed decimal <* crlf
        '$' -> bulk
        '*' -> array
        _   -> fail $ "invalid type tag: " ++ show t

bulk :: Parser Resp
bulk = do
    n <- signed decimal <* crlf
    if | n >=  0   -> Bulk . Lazy.fromStrict <$> take n <* crlf
       | n == -1   -> return NullBulk
       | otherwise -> fail "negative bulk length"

array :: Parser Resp
array = do
    n <- signed decimal <* crlf :: Parser Int
    if | n >=  0   -> Array n <$> replicateM n resp
       | n == -1   -> return NullArray
       | otherwise -> fail "negative array length"

bytes :: Parser ByteString
bytes = Lazy.fromStrict <$> takeWhile (/= '\r')

crlf :: Parser ()
crlf = string "\r\n" >> return ()

-----------------------------------------------------------------------------
-- Serialising

nullArray, nullBulk, crlf' :: Builder
nullArray = byteString "*-1\r\n"
nullBulk  = byteString "$-1\r\n"
crlf'     = byteString "\r\n"
