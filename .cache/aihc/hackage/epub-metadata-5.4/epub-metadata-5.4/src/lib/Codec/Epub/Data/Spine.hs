{- | Data types for working with the metadata of epub documents

   The spine represents a default ordering of items in the manifest.
-}
module Codec.Epub.Data.Spine
   ( Spine (..)
   , SpineItemref (..)
   )
   where

import Codec.Epub.Data.Common


-- | package\/spine\/itemref tag
data SpineItemref = SpineItemref
   { siIdRef  :: MFItemId  -- ^ idref attribute. Must reference an item in the manifest
   , siLinear :: Maybe Bool  -- ^ linear attribute
   }
   deriving (Eq, Show)


-- | package\/spine tag
data Spine = Spine
   { spineToc    :: MFItemId  -- ^ toc attribute. Must reference the NCX in the manifest
   , spineItemrefs :: [SpineItemref] -- ^ at least one required
   }
   deriving (Eq, Show)
