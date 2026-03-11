{- | Data types for working with the metadata of epub documents

   This contains the manifest information from an epub document.
-}
module Codec.Epub.Data.Manifest
   ( Manifest (..)
   , ManifestItem (..)
   )
   where

import Codec.Epub.Data.Common


-- | manifest attribute values
type MFItemHref = String
type MFItemMediaType = String


-- | package\/manifest\/item tag
data ManifestItem = ManifestItem
   { mfiId :: MFItemId  -- ^ id attribute
   , mfiHref :: MFItemHref  -- ^ href attribute
   , mfiMediaType :: MFItemMediaType  -- ^ media-type attribute
   }
   deriving (Eq, Show)


newtype Manifest = Manifest [ManifestItem]
   deriving (Eq, Show)
