{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Aihc.Haddock.Reference.Hoogle
-- Description : Parse a Hoogle text database into entries
--
-- Both the mainline Haddock output and the @aihc-haddock@ output go through
-- this parser, so the comparison sees the same structure for both.
module Aihc.Haddock.Reference.Hoogle
  ( HoogleFile (..),
    HoogleEntry (..),
    parseHoogleFile,
  )
where

import Data.List (mapAccumL)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

data HoogleEntry = HoogleEntry
  { hoogleEntryModule :: Text,
    -- | Documentation lines with the comment prefix removed.
    hoogleEntryDoc :: [Text],
    -- | The declaration line, such as @area :: Shape -> Double@.
    hoogleEntryDecl :: Text
  }
  deriving (Eq, Show)

data HoogleFile = HoogleFile
  { hooglePackage :: Maybe Text,
    hoogleVersion :: Maybe Text,
    hoogleEntries :: [HoogleEntry]
  }
  deriving (Eq, Show)

parseHoogleFile :: Text -> HoogleFile
parseHoogleFile text =
  let file = go (HoogleFile Nothing Nothing []) [] (dropHeader (T.lines text))
   in file {hoogleEntries = snd (mapAccumL attachModule "" (hoogleEntries file))}
  where
    attachModule current entry =
      let name = fromMaybe current (T.stripPrefix "module " (hoogleEntryDecl entry))
       in (name, entry {hoogleEntryModule = name})
    -- Everything before the first @package line is the generator's banner.
    dropHeader lines' =
      case break (T.isPrefixOf "@package") lines' of
        (_, rest@(_ : _)) -> rest
        _ -> lines'

    go file pendingDoc lines' =
      case lines' of
        [] -> file {hoogleEntries = reverse (hoogleEntries file)}
        line : rest
          | T.all (== ' ') line -> go file pendingDoc rest
          | Just package <- T.stripPrefix "@package " line -> go file {hooglePackage = Just (T.strip package)} pendingDoc rest
          | Just version <- T.stripPrefix "@version " line -> go file {hoogleVersion = Just (T.strip version)} pendingDoc rest
          | Just docLine <- commentLine line -> go file (pendingDoc <> [docLine]) rest
          | otherwise ->
              let entry = HoogleEntry {hoogleEntryModule = "", hoogleEntryDoc = pendingDoc, hoogleEntryDecl = T.strip line}
               in go file {hoogleEntries = entry : hoogleEntries file} [] rest

    commentLine line = do
      body <- T.stripPrefix "--" line
      pure $ case T.stripPrefix " | " body of
        Just content -> content
        Nothing -> fromMaybe body (T.stripPrefix "   " body)
