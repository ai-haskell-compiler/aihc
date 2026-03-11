{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      :  ELynx.Tree.Name
-- Description :  Trees with named nodes
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jan 24 20:09:20 2019.
module ELynx.Tree.Name
  ( Name (..),
    HasName (..),
  )
where

import Control.DeepSeq
import Data.Aeson
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Default.Class
import Data.String

-- | Node name.
--
-- Use lazy byte strings because Newick strings are built using chunks.
newtype Name = Name {fromName :: BL.ByteString}
  deriving (Show, Read, Eq)
  deriving (Ord, Monoid, Semigroup, IsString, NFData) via BL.ByteString

instance Default Name where
  def = Name ""

-- XXX: This is pretty lame, but I need those instances. At the moment, I just
-- go via 'String', but this is certainly not the best solution.

instance ToJSON Name where
  toJSON = toJSON . BL.unpack . fromName
  toEncoding = toEncoding . BL.unpack . fromName

instance FromJSON Name where
  parseJSON = fmap (Name . BL.pack) . parseJSON

-- | Class of types having a name.
class HasName a where
  getName :: a -> Name

instance HasName Name where
  getName = id

instance HasName () where
  getName = const (Name BL.empty)

instance HasName Int where
  getName = Name . BB.toLazyByteString . BB.intDec

instance HasName Double where
  getName = Name . BB.toLazyByteString . BB.doubleDec

instance HasName Char where
  getName = Name . BB.toLazyByteString . BB.char8

instance (HasName a) => HasName [a] where
  getName = Name . BL.concat . map (fromName . getName)

instance HasName BL.ByteString where
  getName = Name

instance HasName BS.ByteString where
  getName = Name . BL.fromStrict
