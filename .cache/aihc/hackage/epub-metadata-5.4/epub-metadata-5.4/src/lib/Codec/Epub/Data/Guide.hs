{- | Data types for working with the metadata of epub documents

   This contains the guide section in an epub2 OPF document. It has been deprecated in the epub3 spec and should not be present in epub3 documents.
-}
module Codec.Epub.Data.Guide
   ( GuideRef (..)
   )
   where

import Codec.Epub.Data.Common


-- | package\/guide\/reference tag
data GuideRef = GuideRef
   { grType :: String  -- ^ type attribute. Must follow 13th edition of the Chicago Manual of Style
   , grTitle :: Maybe String  -- ^ title attribute
   , grHref :: MFItemId  -- ^ href attribute. Must reference an item in the manifest
   }
   deriving (Eq, Show)
