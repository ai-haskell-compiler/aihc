-----------------------------------------------------------------------------
-- |
-- Module      :  Lentil.Types
-- Copyright   :  © 2015 Francesco Ariis
-- License     :  GPLv3 (see the LICENSE file)
--
-- Types descriptions
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}

module Lentil.Types where


import qualified Control.DeepSeq as DS
import qualified Data.List       as L
import qualified Data.Char       as C
import qualified GHC.Generics    as G


data Issue = Issue { iFile   :: FilePath,
                     iRow    :: Row,
                     iDesc   :: Maybe Description,
                     iTags   :: [Tag] }
             deriving (Eq, Show, G.Generic)

instance DS.NFData Issue

newtype Tag = Tag { tagString :: String }
            deriving (Show, Eq, Ord, G.Generic)

instance DS.NFData Tag

type Description = String
type Row = Int

-- extension alias, will treat *.fst files as if they were *.snd
type Alias = (String, String)

aliasSign :: String
aliasSign = "%"

-- Flagwords are string (TODO, FIXME, etc.)
type FlagWord = String

-- all to lower
normaliseFlagword :: FlagWord -> FlagWord
normaliseFlagword t = map C.toLower t


-- output format
data Format = TagPop | Pretty | File | Csv | Comp | Xml
            deriving (Show, Eq, Enum, Bounded)
    -- comp: as gcc/ghc would output warnings


-- tag delimiters
openDel, closeDel :: Char
openDel  = '['
closeDel = ']'

-- eliminate "./" at the beginning of fp if present
prettyFP :: String -> String
prettyFP fp | L.isPrefixOf "./" fp = drop 2 fp
            | otherwise            = fp

iPPDesc :: Issue -> String
iPPDesc i = maybe "<no description>" id . iDesc $ i

