{-# LANGUAGE TypeOperators, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE Safe #-}

{-| Description : Deriving strategies for making 'GHC.Generics.Generic'-based 'Binary' instances more expressive.

This module defines some types to be used with @DerivingVia@ when deriving 'Binary' instances.
-}

module Data.Binary.DerivingVia
( Alternatively(..)
) where

import Control.Applicative
import Data.Binary
import GHC.Generics

-- | Try to deserialize each constructor of @a@ in order.
--
-- For sum types, stock 'Binary' writes (and expects to read) an integer denoting the index of the constructor.
-- This isn't always what's needed. In the following example, the constructor is uniquely identified by the marker byte,
-- and its index in the Haskell ADT is irrelevant:
--
-- > data JfifSegment
-- >   = App0Segment (MatchByte "app0 segment" 0xe0, JfifApp0)
-- >   | DqtSegment  (MatchByte "dqt segment"  0xdb, QuantTable)
-- >   | SofSegment  (MatchByte "sof segment"  0xc0, SofInfo)
-- >   | DhtSegment  (MatchByte "dht segment"  0xc4, HuffmanTable)
-- >   | DriSegment  (MatchByte "dri segment"  0xdd, RestartInterval)
-- >   | SosSegment  (MatchByte "sos segment"  0xda, SosImage)
-- >   | UnknownSegment RawSegment
-- >   deriving Generic
-- >   deriving Binary via Alternatively JfifSegment

newtype Alternatively a = Alternatively { getAlt :: a }

class GAltBinary grecord where
  gAltGet :: Get (grecord p)
  gAltPut :: grecord p -> Put

instance Binary grecord => GAltBinary (K1 i grecord) where
  gAltGet = K1 <$> get
  gAltPut = put . unK1

instance GAltBinary grecord => GAltBinary (M1 i t grecord) where
  gAltGet = M1 <$> gAltGet
  gAltPut = gAltPut . unM1

instance (GAltBinary l, GAltBinary r) => GAltBinary (l :*: r) where
  gAltGet = (:*:) <$> gAltGet <*> gAltGet
  gAltPut (l :*: r) = gAltPut l <> gAltPut r

instance (GAltBinary l, GAltBinary r) => GAltBinary (l :+: r) where
  gAltGet = L1 <$> gAltGet
        <|> R1 <$> gAltGet
  gAltPut (L1 l) = gAltPut l
  gAltPut (R1 r) = gAltPut r

instance (Generic a, GAltBinary (Rep a)) => Binary (Alternatively a) where
  get = Alternatively . to <$> gAltGet
  put = gAltPut . from . getAlt
