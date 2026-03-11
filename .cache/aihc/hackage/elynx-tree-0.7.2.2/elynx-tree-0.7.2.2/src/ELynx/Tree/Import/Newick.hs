{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  ELynx.Tree.Import.Newick
-- Description :  Import Newick trees
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jan 17 14:56:27 2019.
--
-- Some functions are inspired by
-- [Biobase.Newick.Import](https://hackage.haskell.org/package/BiobaseNewick).
--
-- [Specifications](http://evolution.genetics.washington.edu/phylip/newicktree.html)
--
-- In particular, no conversion from _ to (space) is done right now.
--
-- For a description of rooted 'Tree's, please see the 'ELynx.Tree.Rooted'
module ELynx.Tree.Import.Newick
  ( NewickFormat (..),
    newick,
    oneNewick,
    parseOneNewick,
    readOneNewick,
    someNewick,
    parseSomeNewick,
    readSomeNewick,
  )
where

import Control.Applicative
import Data.Aeson (FromJSON, ToJSON)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import ELynx.Tree.Length
import ELynx.Tree.Name
import ELynx.Tree.Phylogeny
import ELynx.Tree.Rooted hiding (forest, label)
import ELynx.Tree.Support
import GHC.Generics
import Prelude hiding (takeWhile)

-- IDEA: Key-value pairs in Newick files.
--
-- After some thinking I believe the best way to go is RevBayes-like key-value
-- pairs after branch lengths and node labels by default and the option to
-- import IqTree-like trees.
--
-- I can not really provide a general parser for key-value pairs, but I can
-- provide appropriate export functions for reasonably general key-value pairs
-- such as branch support values. This could look like so:
--
-- @
-- fromNewickG :: Parser (Tree (Maybe Length, BL.ByteString) (Name, BL.ByteString))
--
-- fromNewick :: Parser (Tree (Maybe Length) Name)
--
-- toNewickG :: Tree BL.ByteString BL.ByteString -> BL.ByteString
--
-- toNewick :: (HasMaybeLength e, HasMaybeSupport e, HasName a) => Tree e a -> BL.ByteString
-- @
--
-- In this case, I would also rename RevBayes to KeyVal (or provide a separate
-- function for IqTree-like trees). I would not ignore the key values but just
-- provide the whole string to be parsed by the user.

-- | Newick tree format.
--
-- - Standard: Branch support values are stored in square brackets after branch
--   lengths.
--
-- - IqTree: Branch support values are stored as node names after the closing
--   bracket of forests.
--
-- - RevBayes: Key-value pairs are provided in square brackets after node names
--   as well as branch lengths. NOTE: Key value pairs are ignored.
data NewickFormat = Standard | IqTree | RevBayes
  deriving (Eq, Show, Read, Bounded, Enum, Generic)

instance FromJSON NewickFormat

instance ToJSON NewickFormat

-- | Newick tree parser. Also succeeds when more trees follow.
newick :: NewickFormat -> Parser (Tree Phylo Name)
newick f = case f of
  Standard -> p tree
  IqTree -> p treeIqTree
  RevBayes -> newickRevBayes
  where
    p t = skipWhile isSpace *> t <* char ';' <* skipWhile isSpace <?> "newick"

-- | One Newick tree parser. Fails when end of input is not reached.
oneNewick :: NewickFormat -> Parser (Tree Phylo Name)
oneNewick f = newick f <* endOfInput <?> "oneNewick"

-- | See 'oneNewick'.
parseOneNewick :: NewickFormat -> BS.ByteString -> Either String (Tree Phylo Name)
parseOneNewick f = parseOnly (oneNewick f)

-- | See 'oneNewick'; may fail with 'error'.
readOneNewick :: NewickFormat -> FilePath -> IO (Tree Phylo Name)
readOneNewick f fn = BS.readFile fn >>= (either error pure . parseOneNewick f)

-- | One or more Newick trees parser.
someNewick :: NewickFormat -> Parser (Forest Phylo Name)
someNewick f = some (newick f) <* endOfInput <?> "someNewick"

-- | See 'someNewick'.
parseSomeNewick :: NewickFormat -> BS.ByteString -> Either String [Tree Phylo Name]
parseSomeNewick f = parseOnly (someNewick f)

-- | See 'someNewick'; may fail with 'error'.
readSomeNewick :: NewickFormat -> FilePath -> IO [Tree Phylo Name]
readSomeNewick f fn = BS.readFile fn >>= (either error pure . parseSomeNewick f)

tree :: Parser (Tree Phylo Name)
tree = branched <|> leaf <?> "tree"

branched :: Parser (Tree Phylo Name)
branched = (<?> "branched") $ do
  f <- forest
  n <- name
  p <- phylo
  return $ Node p n f

forestSep :: Parser ()
forestSep = char ',' *> skipWhile isSpace

-- A 'forest' is a set of trees separated by @,@ and enclosed by parentheses.
forest :: Parser (Forest Phylo Name)
forest = char '(' *> (tree `sepBy1` forestSep) <* char ')' <?> "forest"

-- A 'leaf' has a 'name' and a 'phylo' branch.
leaf :: Parser (Tree Phylo Name)
leaf = (<?> "leaf") $ do
  n <- name
  p <- phylo
  return $ Node p n []

nameChar :: Char -> Bool
nameChar c = c `notElem` " :;()[],"

-- A name can be any string of printable characters except blanks, colons,
-- semicolons, parentheses, and square brackets (and commas).
nameNotQuoted :: Parser Name
nameNotQuoted = Name . BL.fromStrict <$> takeWhile nameChar <?> "nameNoQuotes"

noClosingQuote :: Char -> Bool
noClosingQuote c = c /= '\''

nameQuoted :: Parser Name
nameQuoted = (<?> "nameQuotes") $ do
  _ <- char '\''
  n <- Name . BL.fromStrict <$> takeWhile noClosingQuote <?> "nameQuoted"
  _ <- char '\''
  pure n

-- A name can be any string of printable characters except blanks, colons,
-- semicolons, parentheses, and square brackets (and commas).
name :: Parser Name
name = skipWhile isSpace *> (nameQuoted <|> nameNotQuoted) <?> "name"

phylo :: Parser Phylo
phylo = Phylo <$> optional branchLengthStandard <*> optional branchSupportStandard <?> "phylo"

-- Branch length.
branchLengthSimple :: Parser Length
branchLengthSimple = (<?> "branchLengthSimple") $ do
  l <- double <?> "branchLengthSimple; double"
  case toLength l of
    Left e -> fail e
    Right pl -> pure pl

-- Branch length.
branchLengthStandard :: Parser Length
branchLengthStandard = do
  _ <- skipWhile isSpace
  _ <- char ':' <?> "branchLengthDelimiter"
  _ <- skipWhile isSpace
  branchLengthSimple

branchSupportSimple :: Parser Support
branchSupportSimple = (<?> "branchSupportSimple") $
  do
    s <- double <?> "branchSupportSimple; double"
    case toSupport s of
      Left e -> fail e
      Right ps -> pure ps

branchSupportStandard :: Parser Support
branchSupportStandard = (<?> "branchSupportStandard") $ do
  _ <- char '[' <?> "branchSupportBegin"
  s <- branchSupportSimple
  _ <- char ']' <?> "branchSupportEnd"
  return s

--------------------------------------------------------------------------------
-- IQ-TREE.
--
-- IQ-TREE stores the branch support as node names after the closing bracket of
-- a forest. Parse a single Newick tree. Also succeeds when more trees follow.

-- IQ-TREE stores the branch support as node names after the closing bracket of a forest.
treeIqTree :: Parser (Tree Phylo Name)
treeIqTree = branchedIqTree <|> leaf <?> "treeIqTree"

-- IQ-TREE stores the branch support as node names after the closing bracket of a forest.
branchedIqTree :: Parser (Tree Phylo Name)
branchedIqTree = (<?> "branchedIqTree") $ do
  f <- forestIqTree
  ms <- optional branchSupportSimple
  n <- name
  mb <- optional branchLengthStandard
  return $ Node (Phylo mb ms) n f

-- IQ-TREE stores the branch support as node names after the closing bracket of a forest.
forestIqTree :: Parser (Forest Phylo Name)
forestIqTree = (<?> "forestIqTree") $ do
  _ <- char '('
  f <- treeIqTree `sepBy1` forestSep
  _ <- char ')'
  return f

--------------------------------------------------------------------------------
-- RevBayes.

-- RevBayes uses square brackets and key-value pairs to define information
-- about nodes and branches.
--
-- Parse a single Newick tree. Also succeeds when more trees follow.
--
-- NOTE: Key value pairs are ignored. In my opinion, it is just not a good
-- option to import key values pairs in this form. Key value pairs can still be
-- exported by first converting them to a ByteString, and then performing a
-- normal export.
newickRevBayes :: Parser (Tree Phylo Name)
newickRevBayes =
  skipWhile isSpace
    *> optional brackets
    *> treeRevBayes
    <* char ';'
    <* skipWhile isSpace
    <?> "newickRevBayes"

treeRevBayes :: Parser (Tree Phylo Name)
treeRevBayes = branchedRevBayes <|> leafRevBayes <?> "treeRevBayes"

branchedRevBayes :: Parser (Tree Phylo Name)
branchedRevBayes = (<?> "branchedRevgBayes") $ do
  f <- forestRevBayes
  n <- nameRevBayes
  b <- optional branchLengthRevBayes
  return $ Node (Phylo b Nothing) n f

forestRevBayes :: Parser (Forest Phylo Name)
forestRevBayes = (<?> "forestRevBayes") $ do
  _ <- char '('
  f <- treeRevBayes `sepBy1` forestSep
  _ <- char ')'
  return f

nameRevBayes :: Parser Name
nameRevBayes = name <* optional brackets <?> "nameRevBayes"

branchLengthRevBayes :: Parser Length
branchLengthRevBayes = branchLengthStandard <* optional brackets <?> "branchLengthRevBayes"

leafRevBayes :: Parser (Tree Phylo Name)
leafRevBayes = (<?> "leafRevBayes") $ do
  n <- nameRevBayes
  b <- optional branchLengthRevBayes
  return $ Node (Phylo b Nothing) n []

-- NOTE: Drop anything between brackets.
brackets :: Parser ()
brackets = (<?> "brackets") $ do
  _ <- char '['
  _ <- takeWhile (/= ']')
  _ <- char ']'
  return ()
