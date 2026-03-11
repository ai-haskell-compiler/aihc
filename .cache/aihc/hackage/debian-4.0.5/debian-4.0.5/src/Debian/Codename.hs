-- | https://wiki.debian.org/DebianRepository/Format#Codename

{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Debian.Codename
    ( Codename(..)
    , codename
    , parseCodename
    ) where

import Data.Data (Data, Typeable)
import Debian.TH ({-instance Pretty Loc-})
import Network.URI (unEscapeString, escapeURIString, isAllowedInURI)
--import Text.PrettyPrint.HughesPJClass as PP (Pretty(pPrint), text)
import Text.PrettyPrint (text)
import Distribution.Pretty

data Codename = Codename String deriving (Eq, Ord, Read, Show, Data, Typeable)

parseCodename :: String -> Codename
parseCodename = Codename . unEscapeString

codename :: Codename -> String
codename (Codename s) = escapeURIString isAllowedInURI s

instance Pretty Codename where
    pretty (Codename s) = text s
