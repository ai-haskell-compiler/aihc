-- | This module name is spurious - "Release" is not an official term
-- in the debian documentation.

{-# LANGUAGE DeriveDataTypeable #-}

module Debian.Release
    ( Section(..)
    , SubSection(..)
    , sectionName
    , sectionName'
    , sectionNameOfSubSection
    , parseSection
    , parseSection'
    ) where

import Network.URI (unEscapeString, escapeURIString, isAllowedInURI)

-- |A section of a repository such as main, contrib, non-free,
-- restricted.  The indexes for a section are located below the
-- distribution directory.
newtype Section = Section String deriving (Read, Show, Eq, Ord)

-- |A package's subsection is only evident in its control information,
-- packages from different subsections all reside in the same index.
data SubSection = SubSection { section :: Section, subSectionName :: String } deriving (Read, Show, Eq, Ord)

sectionName :: SubSection -> String
sectionName (SubSection (Section "main") y) = y
sectionName (SubSection x y) = sectionName' x ++ "/" ++ y

sectionName' :: Section -> String
sectionName' (Section s) = escapeURIString isAllowedInURI s

sectionNameOfSubSection :: SubSection -> String
sectionNameOfSubSection = sectionName' . section

-- |Parse the value that appears in the @Section@ field of a .changes file.
-- (Does this need to be unesacped?)
parseSection :: String -> SubSection
parseSection s =
    case span (/= '/') s of
      (x, "") -> SubSection (Section "main") x
      ("main", y) -> SubSection (Section "main") y
      (x, y) -> SubSection (Section x) (tail y)

parseSection' :: String -> Section
parseSection' name =
    Section (unEscapeString name)
