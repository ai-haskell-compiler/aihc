-- |
-- This module exports all relevant modules of the haskell-src-exts-simple package.
--
-- The most important module is "Language.Haskell.Exts.Simple.Syntax".

{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Exts.Simple (
    module Language.Haskell.Exts.Simple.Syntax,
    module Language.Haskell.Exts.Simple.Build,
    module Language.Haskell.Exts.Simple.Pretty,
    module Language.Haskell.Exts.Simple.Fixity,
    module Language.Haskell.Exts.Simple.Extension,
    module Language.Haskell.Exts.Simple.Parser,
    module Language.Haskell.Exts.SrcLoc,
    module Language.Haskell.Exts.Simple
) where

import qualified Language.Haskell.Exts as H
import Language.Haskell.Exts (
    readExtensions,
 )

import Language.Haskell.Exts.Simple.Syntax
import Language.Haskell.Exts.Simple.Build
import Language.Haskell.Exts.Simple.Pretty
import Language.Haskell.Exts.Simple.Fixity
import Language.Haskell.Exts.Simple.Extension
import Language.Haskell.Exts.Simple.Parser
import Language.Haskell.Exts.SrcLoc -- useful for interpreting parse errors

-- * Functions

parseFile :: FilePath -> IO (ParseResult Module)
parseFile = fmap (fmap (fmap (const ()))) . H.parseFile

parseFileWithExts :: [Extension] -> FilePath -> IO (ParseResult Module)
parseFileWithExts e = fmap (fmap (fmap (const ()))) . H.parseFileWithExts e


parseFileWithMode :: ParseMode -> FilePath -> IO (ParseResult Module)
parseFileWithMode m = fmap (fmap (fmap (const ()))) . H.parseFileWithMode m


parseFileContents :: String -> ParseResult Module
parseFileContents = fmap (fmap (const ())) . H.parseFileContents

parseFileContentsWithExts :: [Extension] -> String -> ParseResult Module
parseFileContentsWithExts e =
    fmap (fmap (const ())) . H.parseFileContentsWithExts e

parseFileContentsWithMode :: ParseMode -> String -> ParseResult Module
parseFileContentsWithMode m =
    fmap (fmap (const ())) . H.parseFileContentsWithMode m

-- omitted: parseFileWithComments :: ParseMode -> FilePath -> IO (ParseResult (Module, [Comment]))
-- omitted: parseFileWithCommentsAndPragmas :: ParseMode -> FilePath -> IO (ParseResult (Module, [Comment], [UnknownPragma]))
-- omitted: parseFileContentsWithComments :: ParseMode -> String -> ParseResult (Module, [Comment])
