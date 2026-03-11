{- | Data types for working with the metadata of epub documents

   This module helps to avoid circular imports for definitions needed by two or more other modules in this library.
-}
module Codec.Epub.Data.Common
   where


-- | Attribute value used to relate things to each other in the OPF data
type MFItemId = String
