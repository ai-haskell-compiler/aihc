{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, OverloadedStrings, TypeSynonymInstances #-}
module Debian.Relation.Common where

-- Standard GHC Modules

import Data.Data (Data)
import Data.List as List (map, intersperse)
import Data.Monoid (mconcat, (<>))
import Data.Function
import Data.Set as Set (Set, toList)
import Data.Typeable (Typeable)
import Debian.Arch (Arch, prettyArch)
import Debian.Pretty (PP(..))
import Prelude hiding (map)
import Text.ParserCombinators.Parsec
import Text.PrettyPrint (Doc, text, empty)
import Distribution.Pretty (Pretty(pretty))

-- Local Modules

import Debian.Version

-- Datatype for relations

type Relations = AndRelation
type AndRelation = [OrRelation]
type OrRelation = [Relation]

data Relation = Rel BinPkgName (Maybe VersionReq) (Maybe ArchitectureReq) deriving (Eq, Read, Show)

newtype SrcPkgName = SrcPkgName {unSrcPkgName :: String} deriving (Read, Show, Eq, Ord, Data, Typeable)
newtype BinPkgName = BinPkgName {unBinPkgName :: String} deriving (Read, Show, Eq, Ord, Data, Typeable)

class Pretty (PP a) => PkgName a where
    pkgNameFromString :: String -> a

instance PkgName BinPkgName where
    pkgNameFromString = BinPkgName

instance PkgName SrcPkgName where
    pkgNameFromString = SrcPkgName

class ParseRelations a where
    -- |'parseRelations' parse a debian relation (i.e. the value of a
    -- Depends field). Return a parsec error or a value of type
    -- 'Relations'
    parseRelations :: a -> Either ParseError Relations

-- | This needs to be indented for use in a control file: intercalate "\n     " . lines . show
prettyRelations :: [[Relation]] -> Doc
prettyRelations xss = mconcat . intersperse (text "\n, ") . List.map prettyOrRelation $ xss

prettyOrRelation :: [Relation] -> Doc
prettyOrRelation xs = mconcat . intersperse (text " | ") . List.map prettyRelation $ xs

prettyRelation :: Relation -> Doc
prettyRelation (Rel name ver arch) =
    pretty (PP name) <> maybe empty prettyVersionReq ver <> maybe empty prettyArchitectureReq arch

instance Ord Relation where
    compare (Rel pkgName1 mVerReq1 _mArch1) (Rel pkgName2 mVerReq2 _mArch2) =
        case compare pkgName1 pkgName2 of
             LT -> LT
             GT -> GT
             EQ -> compare mVerReq1 mVerReq2

data ArchitectureReq
    = ArchOnly (Set Arch)
    | ArchExcept (Set Arch)
    deriving (Eq, Ord, Read, Show)

prettyArchitectureReq :: ArchitectureReq -> Doc
prettyArchitectureReq (ArchOnly arch) = text " [" <> mconcat (List.map prettyArch (toList arch)) <> text "]"
prettyArchitectureReq (ArchExcept arch) = text " [" <> mconcat (List.map ((text "!") <>) (List.map prettyArch (toList arch))) <> text "]"

data VersionReq
    = SLT DebianVersion
    | LTE DebianVersion
    | EEQ  DebianVersion
    | GRE  DebianVersion
    | SGR DebianVersion
      deriving (Eq, Read, Show)

prettyVersionReq :: VersionReq -> Doc
prettyVersionReq (SLT v) = text " (<< " <> prettyDebianVersion v <> text ")"
prettyVersionReq (LTE v) = text " (<= " <> prettyDebianVersion v <> text ")"
prettyVersionReq (EEQ v) = text " (= " <> prettyDebianVersion v <> text ")"
prettyVersionReq (GRE v) = text " (>= " <> prettyDebianVersion v <> text ")"
prettyVersionReq (SGR v) = text " (>> " <> prettyDebianVersion v <> text ")"

-- |The sort order is based on version number first, then on the kind of
-- relation, sorting in the order <<, <= , ==, >= , >>
instance Ord VersionReq where
    compare = compare `on` extr
      where extr (SLT v) = (v,0 :: Int)
            extr (LTE v) = (v,1 :: Int)
            extr (EEQ v) = (v,2 :: Int)
            extr (GRE v) = (v,3 :: Int)
            extr (SGR v) = (v,4 :: Int)

-- |Check if a version number satisfies a version requirement.
checkVersionReq :: Maybe VersionReq -> Maybe DebianVersion -> Bool
checkVersionReq Nothing _ = True
checkVersionReq _ Nothing = False
checkVersionReq (Just (SLT v1)) (Just v2) = v2 < v1
checkVersionReq (Just (LTE v1)) (Just v2) = v2 <= v1
checkVersionReq (Just (EEQ v1)) (Just v2) = v2 == v1
checkVersionReq (Just (GRE v1)) (Just v2) = v2 >= v1
checkVersionReq (Just (SGR v1)) (Just v2) = v2 > v1

instance Pretty (PP BinPkgName) where
    pretty = text . unBinPkgName . unPP

instance Pretty (PP SrcPkgName) where
    pretty = text . unSrcPkgName . unPP

-- | Wrap `PP` around type synonyms that might overlap with the
-- `Pretty [a]` instance.
instance Pretty (PP Relations) where
    pretty = prettyRelations . unPP

instance Pretty (PP OrRelation) where
    pretty = prettyOrRelation . unPP

instance Pretty (PP Relation) where
    pretty = prettyRelation . unPP

instance Pretty (PP VersionReq) where
    pretty = prettyVersionReq . unPP

instance Pretty (PP ArchitectureReq) where
    pretty = prettyArchitectureReq . unPP
