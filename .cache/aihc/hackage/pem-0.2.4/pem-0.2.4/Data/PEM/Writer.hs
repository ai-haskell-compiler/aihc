{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Data.PEM.Writer
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
module Data.PEM.Writer
    ( pemWriteLBS
    , pemWriteBS
    ) where

import Data.PEM.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import           Data.ByteArray.Encoding (Base(Base64), convertToBase)

-- | write a PEM structure to a builder
pemWrite :: PEM -> L.ByteString
pemWrite pem = L.fromChunks $ ([begin,header]++section++[end])
    where begin   = B.concat ["-----BEGIN ", sectionName, "-----\n"]
          end     = B.concat ["-----END ", sectionName, "-----\n" ]
          section :: [ByteString]
          section = map encodeLine $ splitChunks $ pemContent pem
          header :: ByteString
          header  = if null $ pemHeader pem
                        then B.empty
                        else B.concat ((concatMap toHeader (pemHeader pem)) ++ ["\n"])
          toHeader :: (String, ByteString) -> [ByteString]
          toHeader (k,v) = [ BC.pack k, ":", v, "\n" ]
          -- expect only ASCII. need to find a type to represent it.
          sectionName = BC.pack $ pemName pem
          encodeLine l = convertToBase Base64 l `B.append` "\n"

          splitChunks b
                  | B.length b > 48 = let (x,y) = B.splitAt 48 b in x : splitChunks y
                  | otherwise       = [b]

-- | convert a PEM structure to a bytestring
pemWriteBS :: PEM -> ByteString
pemWriteBS = B.concat . L.toChunks . pemWrite

-- | convert a PEM structure to a lazy bytestring
pemWriteLBS :: PEM -> L.ByteString
pemWriteLBS = pemWrite
