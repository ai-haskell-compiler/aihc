-- |
-- Module      : Data.Endian
-- Description : Newtypes for little- and big-endian values
-- Copyright   : (c) Aleksey Makarov, 2021
-- License     : BSD 3-Clause License
-- Maintainer  : aleksey.makarov@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Newtypes for little- and big-endian instances of `Binary`

{-# LANGUAGE FlexibleInstances #-}

module Data.Endian (Be(..), Le(..)) where

import Data.Binary.Put
import Data.Binary.Get
import Data.Binary

-- | @Be a@ is an instance of `Binary` such that @a@ is serialized as big-endian
newtype Be a = Be { fromBe :: a } deriving Eq

-- | @Le a@ is an instance of `Binary` such that @a@ is serialized as little-endian
newtype Le a = Le { fromLe :: a } deriving Eq

instance Binary (Be Word16) where
    put = putWord16be . fromBe
    get = Be <$> getWord16be

instance Binary (Le Word16) where
    put = putWord16le . fromLe
    get = Le <$> getWord16le

instance Binary (Be Word32) where
    put = putWord32be . fromBe
    get = Be <$> getWord32be

instance Binary (Le Word32) where
    put = putWord32le . fromLe
    get = Le <$> getWord32le

instance Binary (Be Word64) where
    put = putWord64be . fromBe
    get = Be <$> getWord64be

instance Binary (Le Word64) where
    put = putWord64le . fromLe
    get = Le <$> getWord64le
