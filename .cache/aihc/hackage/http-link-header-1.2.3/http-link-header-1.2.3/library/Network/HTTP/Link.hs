{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE Trustworthy, FlexibleInstances, UnicodeSyntax #-}

-- | This module exports all the things at the same time, plus a utility
-- function.
module Network.HTTP.Link (
  module Network.HTTP.Link.Types
, module Network.HTTP.Link.Writer
, module Network.HTTP.Link.Parser
, lnk
) where

import           Control.Error.Util (hush)
import           Data.Text (Text, pack)
import           Data.Text.Encoding (encodeUtf8)
import safe      Network.HTTP.Link.Parser
import safe      Network.HTTP.Link.Types
import safe      Network.HTTP.Link.Writer
import           Web.HttpApiData (ToHttpApiData(..))

instance (IsURI uri) ⇒ ToHttpApiData [Link uri] where
  toUrlPiece = toUrlPiece . writeLinkHeader
  toHeader = encodeUtf8 . writeLinkHeader

instance (IsURI uri) ⇒ ToHttpApiData (Link uri) where
  toUrlPiece = toUrlPiece . writeLink
  toHeader = encodeUtf8 . writeLink

-- | Construct a Link.
lnk ∷ (IsURI uri) ⇒ String → [(LinkParam, Text)] → Maybe (Link uri)
lnk u r = fmap (\x → Link x r) $ hush $ uriFromText $ pack u
