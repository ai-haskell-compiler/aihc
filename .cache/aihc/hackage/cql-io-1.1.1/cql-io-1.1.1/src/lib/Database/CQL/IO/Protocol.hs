-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module Database.CQL.IO.Protocol where

import Control.Monad.Catch
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Database.CQL.Protocol
import Database.CQL.IO.Exception

import qualified Data.Text.Lazy as LT

data Frame = Frame !Header !ByteString

-- | Parse a CQL protocol frame into a 'Response'.
parse :: (Tuple a, Tuple b, MonadThrow m)
    => Compression
    -> Frame
    -> m (Response k a b)
parse x (Frame h b) =
    case unpack x h b of
        Left  e -> throwM $ ParseError ("response body reading: " ++ e)
        Right r -> return r

-- | Serialise a 'Request' into a complete CQL protocol frame,
-- including header, length and body.
serialise :: (Tuple a, MonadThrow m)
    => Version
    -> Compression
    -> Request k a b
    -> Int
    -> m ByteString
serialise v f r i =
    let c = case getOpCode r of
                OcStartup -> noCompression
                OcOptions -> noCompression
                _         -> f
        s = mkStreamId i
    in either (throwM . SerialiseError) return (pack v c (isTracing r) s r)
  where
    isTracing :: Request k a b -> Bool
    isTracing (RqQuery (Query _ p))     = fromMaybe False $ enableTracing p
    isTracing (RqExecute (Execute _ p)) = fromMaybe False $ enableTracing p
    isTracing _                         = False

quoted :: LT.Text -> LT.Text
quoted s = "\"" <> LT.replace "\"" "\"\"" s <> "\""
