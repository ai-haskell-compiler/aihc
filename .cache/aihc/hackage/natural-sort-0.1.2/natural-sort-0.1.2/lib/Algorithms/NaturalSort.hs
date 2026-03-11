{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      :  Algorithms.NaturalSort
-- Copyright   :  (c) 2010 John Millikin
-- License     :  BSD3
--
-- Maintainer  :  jmillikin@gmail.com
-- Portability :  portable
--
-- Human-friendly text collation
module Algorithms.NaturalSort
	( SortKey
	, NaturalSort (..)
	, compare
	) where

import           Prelude hiding (compare)
import qualified Prelude as Prelude
import           Data.Char (isDigit)
import           Data.Function (on)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import qualified Text.Parsec as P

data SortChunk
	= Integer Integer
	| Text T.Text
	| Collated B.ByteString (T.Text -> B.ByteString)

instance Show SortChunk where
	show (Integer x) = show x
	show (Text x) = show x
	show (Collated x _) = show x

instance Ord SortChunk where
	-- Basic comparisons
	compare (Integer x) (Integer y) = Prelude.compare x y
	compare (Text x) (Text y) = Prelude.compare x y
	compare (Collated x _) (Collated y _) = Prelude.compare x y
	
	-- Text <-> ByteString
	compare (Text x) (Collated y f) = Prelude.compare (f x) y
	compare (Collated x f) (Text y) = Prelude.compare x (f y)
	
	-- Integer < *
	compare (Integer _) _ = LT
	compare _ (Integer _) = GT

instance Eq SortChunk where
	(Integer x) == (Integer y) = x == y
	(Text x) == (Text y) = x == y
	(Collated x _) == (Collated y _) = x == y
	
	(Text x) == (Collated y f) = f x == y
	(Collated x f) == (Text y) = x == f y
	
	_ == _ = False

data SortKey = SortKey [SortChunk]
	deriving (Show, Eq, Ord)

class NaturalSort a where
	-- | Split a sortable type into textual and numeric sections, with no
	-- collation transformation.
	--
	-- If advanced collation is required, either pre-transform the input
	-- (using eg 'T.toLower') or use 'sortKeyCollated'.
	sortKey :: a -> SortKey
	
	-- | Split a sortable type into textual and numeric sections, using
	-- a custom collation transformation. This is useful for providing
	-- language- or use-specific ordering.
	sortKeyCollated :: (T.Text -> B.ByteString) -> a -> SortKey

instance NaturalSort String where
	sortKey = parseText Nothing
	sortKeyCollated f = parseText (Just f)

instance NaturalSort TL.Text where
	sortKey = sortKey . TL.unpack
	sortKeyCollated = (. TL.unpack) . sortKeyCollated

instance NaturalSort T.Text where
	sortKey = parseText Nothing . T.unpack
	sortKeyCollated = (. T.unpack) . sortKeyCollated

-- | Compare two values, using their natural ordering.
compare :: NaturalSort a => a -> a -> Ordering
compare = Prelude.compare `on` sortKey

parseText :: Maybe (T.Text -> B.ByteString) -> String -> SortKey
parseText toBytes string = parsed where
	parsed = case P.parse parser "" string of
		Right key -> key
		
		-- This should never happen; the parser has no failure
		-- conditions, unless somehow something broke within Parsec
		-- itself.
		Left err -> error ("sortKey failed: " ++ show err)
	
	parser = fmap SortKey (P.manyTill chunk P.eof) where
		chunk = P.choice [int, text]
		int = fmap (Integer . read) (P.many1 P.digit)
		text = fmap toText (P.many1 notDigit)
		notDigit = P.satisfy (not . isDigit)
	
	toText chars = let text = T.pack chars in case toBytes of
		Nothing -> Text text
		Just f -> Collated (f text) f
