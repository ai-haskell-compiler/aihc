{- | Data type for working with epub package info

   This contains the package section from an epub document. In the
   OPF XML document, this tag contains the other tags (metadata,
   manifest...)
-}
module Codec.Epub.Data.Package
   ( Package (..)
   )
   where


{- | package tag
-}
data Package = Package
   { pkgVersion :: String  -- ^ version attribute. This contains which epub specification version (2.x or 3.x) this document conforms to.
   , pkgUniqueId :: String  -- ^ unique-identifier attribute. This should relate to one metaIdentifier in the Metadata and is intended to represent this book's unique identifier.
   }
   deriving (Eq, Show)
