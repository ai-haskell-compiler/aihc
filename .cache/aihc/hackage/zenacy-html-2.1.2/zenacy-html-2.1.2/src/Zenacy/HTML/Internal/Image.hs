{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines html image handling functions.
module Zenacy.HTML.Internal.Image
  ( HTMLSrcset(..)
  , HTMLSrcsetCandidate(..)
  , HTMLSrcsetDescriptor(..)
  , htmlSrcsetParse
  , htmlSrcsetParseCandidate
  , htmlSrcsetParseDescriptor
  , htmlSrcsetRender
  , htmlSrcsetRenderCandidate
  , htmlSrcsetRenderDescriptor
  , htmlSrcsetListURL
  , htmlSrcsetMapURL
  , htmlSrcsetImageMin
  , htmlSrcsetImageMax
  , htmlSrcsetDescriptorSize
  , htmlSrcsetCandidatePair
  , htmlSrcsetFilter
  ) where

import Zenacy.HTML.Internal.Core
import Control.Applicative
  ( (<|>)
  )
import qualified Data.IntMap as IntMap
  ( findMax
  , findMin
  , fromList
  )
import Data.Maybe
  ( catMaybes
  , fromJust
  )
import Data.Monoid
  ( (<>)
  )
import Data.Text
  ( Text
  )
import qualified Data.Text as T
  ( empty
  , intercalate
  , null
  , pack
  , splitOn
  , stripSuffix
  , words
  , unwords
  )

-- | Defines a srcset attribute value.
data HTMLSrcset = HTMLSrcset
  { htmlSrcsetCandidates :: ![HTMLSrcsetCandidate]
  } deriving (Show, Eq, Ord)

-- | Defines the image candidates.
data HTMLSrcsetCandidate = HTMLSrcsetCandidate
  { htmlSrcsetURL        :: !Text
  , htmlSrcsetDescriptor :: !HTMLSrcsetDescriptor
  } deriving (Show, Eq, Ord)

-- | Defines the srcset descriptor.
data HTMLSrcsetDescriptor
  = HTMLSrcsetWidth Int
  | HTMLSrcsetPixel Int
  | HTMLSrcsetNone
    deriving (Show, Eq, Ord)

-- | Parses a srcset attribute value.
htmlSrcsetParse :: Text -> HTMLSrcset
htmlSrcsetParse =
  ( HTMLSrcset
  . catMaybes
  . map htmlSrcsetParseCandidate
  . T.splitOn ","
  )

-- | Parses a srcset candidate value.
htmlSrcsetParseCandidate :: Text -> Maybe HTMLSrcsetCandidate
htmlSrcsetParseCandidate x =
  case T.words x of
    (u:d:[])   -> Just $ HTMLSrcsetCandidate u $ htmlSrcsetParseDescriptor d
    (u:[])     -> Just $ HTMLSrcsetCandidate u HTMLSrcsetNone
    _otherwise -> Nothing

-- | Parses a srcset descriptor value.
htmlSrcsetParseDescriptor :: Text -> HTMLSrcsetDescriptor
htmlSrcsetParseDescriptor x = fromJust $
  (HTMLSrcsetWidth <$> f "w")
  <|> (HTMLSrcsetPixel <$> f "x")
  <|> (Just HTMLSrcsetNone)
  where
    f s = T.stripSuffix s x >>= textReadDec

-- | Renders a srcset.
htmlSrcsetRender :: HTMLSrcset -> Text
htmlSrcsetRender =
  ( T.intercalate ","
  . map htmlSrcsetRenderCandidate
  . htmlSrcsetCandidates
  )

-- | Renders a srcset candidate.
htmlSrcsetRenderCandidate :: HTMLSrcsetCandidate -> Text
htmlSrcsetRenderCandidate (HTMLSrcsetCandidate u d) =
  T.unwords . filter (not . T.null) $ [ u, htmlSrcsetRenderDescriptor d ]

-- | Renders a srcset descriptor.
htmlSrcsetRenderDescriptor :: HTMLSrcsetDescriptor -> Text
htmlSrcsetRenderDescriptor = \case
  HTMLSrcsetWidth x -> T.pack $ show x <> "w"
  HTMLSrcsetPixel x -> T.pack $ show x <> "x"
  HTMLSrcsetNone -> T.empty

-- | Returns the URLs for a srcset.
htmlSrcsetListURL :: HTMLSrcset -> [Text]
htmlSrcsetListURL (HTMLSrcset c) =
  filter (not . T.null) $ map htmlSrcsetURL c

-- | Maps a function over the srcset URLs.
htmlSrcsetMapURL :: (Text -> Text) -> HTMLSrcset -> HTMLSrcset
htmlSrcsetMapURL f (HTMLSrcset c) = HTMLSrcset $ map g c
  where
    g (HTMLSrcsetCandidate u d) = HTMLSrcsetCandidate (f u) d

-- | Returns the smallest image in the srcset.
htmlSrcsetImageMin :: HTMLSrcset -> Text
htmlSrcsetImageMin (HTMLSrcset c) =
  ( snd
  . IntMap.findMin
  . IntMap.fromList
  . map htmlSrcsetCandidatePair
  ) c

-- | Returns the largest image in the srcset.
htmlSrcsetImageMax :: HTMLSrcset -> Text
htmlSrcsetImageMax (HTMLSrcset c) =
  ( snd
  . IntMap.findMax
  . IntMap.fromList
  . map htmlSrcsetCandidatePair
  ) c

-- | Gets the size of the descriptor.
htmlSrcsetDescriptorSize :: HTMLSrcsetDescriptor -> Int
htmlSrcsetDescriptorSize = \case
  HTMLSrcsetWidth x -> x
  HTMLSrcsetPixel x -> x
  HTMLSrcsetNone -> 1

-- | Converts a candidate to a pair.
htmlSrcsetCandidatePair :: HTMLSrcsetCandidate -> (Int, Text)
htmlSrcsetCandidatePair (HTMLSrcsetCandidate u d) =
  (htmlSrcsetDescriptorSize d, u)

-- | Filter candidates from a srcset.
htmlSrcsetFilter :: (HTMLSrcsetCandidate -> Bool) -> HTMLSrcset -> HTMLSrcset
htmlSrcsetFilter f (HTMLSrcset c) = HTMLSrcset $ filter f c
