-- |
-- Module      : Debian.Package.Data.Hackage
-- Copyright   : 2014-2015 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module provides data types of hackage meta information.
module Debian.Package.Data.Hackage
       ( HackageVersion, mkHackageVersion, mkHackageVersion', hackageVersionNumbers

       , Hackage, mkHackage, hackageName, hackageVersion
       , debianShortName, mkHackageDefault

       , NameRule (..), debianNamesFromSourceName

       , hackageLongName

       , hackageArchiveName, hackageArchive

       , ghcLibraryBinPackages, ghcLibraryDocPackage, ghcLibraryPackages
       ) where

import Data.List (stripPrefix)
import Data.Char (toLower)
import Data.Version (Version (Version), showVersion, parseVersion)
import Text.ParserCombinators.ReadP (readP_to_S)
import System.FilePath ((</>), (<.>))


-- | Hackage version type
newtype HackageVersion = HackageVersion (Version)

-- | Make 'HackageVersion'
mkHackageVersion' :: [Int] -> HackageVersion
mkHackageVersion' =  HackageVersion . (`Version` [])

-- | Make 'HackageVersion'
mkHackageVersion :: Int -> Int -> Int -> Int -> HackageVersion
mkHackageVersion v0 v1 v2 v3 = mkHackageVersion' [v0, v1, v2, v3]

-- | Extract hackage version numbers.
hackageVersionNumbers :: HackageVersion -> [Int]
hackageVersionNumbers (HackageVersion (Version ns _)) = ns

instance Show HackageVersion where
  show (HackageVersion v) = showVersion v

instance Read HackageVersion where
  readsPrec _ = map toH . filter h . readP_to_S parseVersion  where
    h (Version b t, _) = length b == 4 && null t
    toH (v, s) = (HackageVersion v, s)

-- | Hackage name and version type with debian short name. e.g. /src-ext/.
data Hackage = Hackage String HackageVersion String  deriving Show

-- | Make 'Hackage'
mkHackage :: String -> HackageVersion -> String -> Hackage
mkHackage =  Hackage

-- | Get package name of 'Hackage'
hackageName :: Hackage -> String
hackageName (Hackage n _ _) = n

-- | Get version of 'Hackage'
hackageVersion :: Hackage -> HackageVersion
hackageVersion (Hackage _ v _) = v

-- | Get debian short name of 'Hackage'
debianShortName :: Hackage -> String
debianShortName (Hackage _ _ sn) = sn

-- | Generate 'Hackage' type from package name and version
--   using 'NameRule'
mkHackageDefault :: NameRule       -- ^ Rule flag to generate names
                 -> String         -- ^ Hackage name string
                 -> HackageVersion -- ^ Version of hackage
                 -> Hackage        -- ^ Result hackage meta info
mkHackageDefault rule hname hver = mkHackage hname hver short  where
  (_ , short) = debianNamesFromSourceName rule hname

defaultHackageSrcPrefix :: String
defaultHackageSrcPrefix =  "haskell-"

-- | Debian short name generate rule
data NameRule = Suggest | Simple deriving (Eq, Show)

-- | Make debian short name from package name using 'NameRule'
debianNamesFromSourceName :: NameRule          -- ^ Rule flag to generate name
                          -> String            -- ^ Debian source name or Hackage name string
                          -> (String, String)  -- ^ Debian source package name and short name like ("haskell-src-exts", "src-exts")
debianNamesFromSourceName rule hname = d rule  where
  lh = map toLower hname
  d Suggest  =  rec' ["haskell-", "haskell"]
  d Simple   =  (defaultHackageSrcPrefix ++ lh, lh)
  rec' []     = (defaultHackageSrcPrefix ++ lh, lh)
  rec' (p:ps) = case stripPrefix p lh of
    Just s  -> (lh, s)
    Nothing -> rec' ps

-- | Package name string with version
hackageLongName :: Hackage -> String
hackageLongName hkg = hackageName hkg ++ '-' : show (hackageVersion hkg)

-- | Package archive basename
hackageArchiveName :: Hackage -> FilePath
hackageArchiveName hkg = hackageLongName hkg <.> "tar" <.> "gz"

distDir :: String
distDir =  "dist"

-- | Package archive pathname
hackageArchive :: Hackage -> FilePath
hackageArchive =  (distDir </>) . hackageArchiveName


-- | Debian library binary package names for GHC
ghcLibraryBinPackages :: Hackage -> [String]
ghcLibraryBinPackages hkg =
  [ concat ["libghc-", debianShortName hkg, '-' : suf]
  | suf <- ["dev", "prof"]
  ]

-- | Debian library document package name for GHC
ghcLibraryDocPackage :: Hackage -> String
ghcLibraryDocPackage hkg = concat ["libghc-", debianShortName hkg, "-doc"]

-- | Debian library package names for GHC
ghcLibraryPackages :: Hackage -> [String]
ghcLibraryPackages hkg = ghcLibraryDocPackage hkg : ghcLibraryBinPackages hkg
