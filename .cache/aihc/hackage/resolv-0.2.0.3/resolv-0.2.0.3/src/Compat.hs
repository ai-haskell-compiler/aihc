{-# LANGUAGE CPP #-}

-- |
-- Copyright: © 2017 Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-2.0-or-later
module Compat
    ( toStrict
    , fromStrict
    , guard
    , replicateM
    , unless
    , when
    , A.Applicative(..)
    , (<$>)
    , Mon.Monoid(..)
    , Foldable
    , F.forM_
    , toList
    , traverse
    , T.Traversable
    , module Data.Word
    , module Data.Int
    , module Data.Maybe
    , putInt32be
    , getInt32be
    ) where

--import qualified Data.ByteString.Lazy as BSL

import           Control.Applicative  as A
import           Control.Monad        as M
import           Data.Binary.Get
import           Data.Binary.Put
#if MIN_VERSION_bytestring(0,10,0)
import           Data.ByteString.Lazy (fromStrict, toStrict)
#else
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BS.L
#endif
import           Data.Foldable        as F
import           Data.Int
import           Data.Maybe
import           Data.Monoid          as Mon (Monoid (..))
import           Data.Traversable     as T
import           Data.Word

#if !(MIN_VERSION_bytestring(0,10,0))
fromStrict :: BS.ByteString -> BS.L.ByteString
fromStrict = BS.L.fromChunks . (:[])

toStrict :: BS.L.ByteString -> BS.ByteString
toStrict = mconcat . BS.L.toChunks
#endif

#if !MIN_VERSION_binary(0,8,1)
putInt32be :: Int32 -> Put
putInt32be x = putWord32be (fromIntegral x)

getInt32be :: Get Int32
getInt32be = fromIntegral <$> getWord32be
#endif
