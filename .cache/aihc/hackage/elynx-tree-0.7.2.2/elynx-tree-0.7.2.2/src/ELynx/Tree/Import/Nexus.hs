{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  ELynx.Tree.Import.Nexus
-- Description :  Import trees from Nexus files
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Apr 28 17:44:13 2020.
module ELynx.Tree.Import.Nexus
  ( nexusTrees,
  )
where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import ELynx.Import.Nexus
import ELynx.Tree.Import.Newick
import ELynx.Tree.Name
import ELynx.Tree.Phylogeny
import ELynx.Tree.Rooted
import Prelude hiding (takeWhile)

-- | Parse a Nexus files with a TREES block.
nexusTrees :: NewickFormat -> Parser [(BS.ByteString, Tree Phylo Name)]
nexusTrees = nexusBlock . trees

trees :: NewickFormat -> Block [(BS.ByteString, Tree Phylo Name)]
trees f = Block "TREES" (some $ namedNewick f)

namedNewick :: NewickFormat -> Parser (BS.ByteString, Tree Phylo Name)
namedNewick f = do
  _ <- skipWhile isSpace
  _ <- (stringCI "TREE" <|> stringCI "UTREE") <?> "namedNewickTreeStart"
  _ <- skipWhile isSpace
  n <- takeWhile1 (\x -> isAlpha_ascii x || isDigit x) <?> "namedNewickTreeName"
  _ <- skipWhile isSpace
  _ <- char '=' <?> "namedNewickEqual"
  _ <- skipWhile isSpace
  t <- newick f <?> "namedNewickTree"
  return (n, t)
