{-# LANGUAGE TemplateHaskell #-}

module Debian.VendorURI
    ( VendorURI(..)
    , vendorURI
    , parseVendorURI
    ) where

import Control.Lens (makeLenses, review)
import Debian.TH (here)
import Debian.URI (parseURI, URI(uriPath))
import Distribution.Pretty (prettyShow)
import Language.Haskell.TH.Syntax (Loc)
import System.FilePath (splitDirectories)

newtype VendorURI = VendorURI {_vendorURI :: URI} deriving (Eq, Ord)

instance Show VendorURI where
    show (VendorURI uri) = "VendorURI (fromJust (parseURIReference " ++ show (show uri) ++ "))"

$(makeLenses ''VendorURI)

parseVendorURI :: [Loc] -> String -> Maybe VendorURI
parseVendorURI locs s = fmap (review vendorURI) (parseURI s)

-- toURI' :: VendorURI -> URI'
-- toURI' = URI' . show . view vendorURI
