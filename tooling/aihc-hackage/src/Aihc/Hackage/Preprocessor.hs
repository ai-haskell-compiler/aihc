{-# LANGUAGE OverloadedStrings #-}

-- | The source preprocessors Cabal runs by file suffix.
--
-- Cabal selects a preprocessor by the suffix of the file it finds for a
-- module, not by the @build-tool-depends@ field: a module found as
-- @Foo.hsc@ goes through hsc2hs whether or not the cabal file names the
-- tool. aihc does the same. The table here is the only place that knows
-- which tools exist; the install pipeline asks it which suffixes to look
-- for and which tool a suffix means, and everything after the preprocess
-- step sees ordinary Haskell sources.
module Aihc.Hackage.Preprocessor
  ( Preprocessor (..),
    preprocessorForExtension,
    preprocessorExtensions,
    preprocessorToolName,
    preprocessorEnvironmentVariable,
    lineDirectivesFromPragmas,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)

-- | A tool that turns a source file into a Haskell module.
data Preprocessor
  = -- | @.hsc@ files: C constants, structure sizes and field offsets
    -- computed with the C compiler of the target.
    Hsc2hs
  deriving (Eq, Ord, Show, Bounded, Enum)

-- | The preprocessor a source file suffix selects, without the leading dot.
preprocessorForExtension :: String -> Maybe Preprocessor
preprocessorForExtension extension =
  case extension of
    "hsc" -> Just Hsc2hs
    _ -> Nothing

-- | The suffixes that select a preprocessor, in the order Cabal tries them.
preprocessorExtensions :: [String]
preprocessorExtensions = ["hsc"]

-- | The name of the executable, which is also the name Cabal files use in
-- @build-tool-depends@.
preprocessorToolName :: Preprocessor -> String
preprocessorToolName preprocessor =
  case preprocessor of
    Hsc2hs -> "hsc2hs"

-- | The environment variable that names the executable instead of the
-- search path, in the manner of @AIHC_WASM_CLANG@.
preprocessorEnvironmentVariable :: Preprocessor -> String
preprocessorEnvironmentVariable preprocessor =
  case preprocessor of
    Hsc2hs -> "AIHC_HSC2HS"

-- | Rewrite the @{-# LINE n "file" #-}@ pragmas a preprocessor emits into
-- the equivalent @#line n "file"@ directives.
--
-- hsc2hs marks the generated module with the line and file of the @.hsc@
-- it came from, so that a compiler reports an error against the source the
-- author wrote rather than against the generated file. aihc's front end
-- understands both forms of line control, but only the @#line@ form all
-- the way: the pragma form sets the line and drops the file name, which
-- leaves a span carrying the generated file's path and the original file's
-- line number. Nothing downstream can reconcile those two, and the
-- excerpt a diagnostic prints comes out of a different file than the line
-- number it is labelled with.
--
-- Rewriting the pragmas closes that gap at the one point that owns the
-- generated file. The two forms mean the same thing, so nothing is lost,
-- and a span then names the @.hsc@ file at its own line and column.
--
-- Only a line that is nothing but such a pragma is rewritten, which is
-- what a preprocessor emits: a pragma sharing its line with code is left
-- alone, and so is anything that is not a well-formed line pragma with a
-- file name.
lineDirectivesFromPragmas :: ByteString -> ByteString
lineDirectivesFromPragmas source
  | not (BS8.isInfixOf "{-# LINE" source) = source
  | otherwise = BS8.intercalate "\n" (map rewrite (splitLines source))
  where
    rewrite line = fromMaybe line (linePragmaDirective line)

    -- A split that keeps every line, including the empty one a trailing
    -- newline leaves, so that rejoining reproduces the input byte for
    -- byte. Only ASCII is inspected, so the bytes of a module in any
    -- encoding pass through unchanged.
    splitLines = BS8.split '\n'

-- | The @#line@ directive a lone @{-# LINE n "file" #-}@ pragma stands
-- for, or 'Nothing' for a line that is not exactly such a pragma.
linePragmaDirective :: ByteString -> Maybe ByteString
linePragmaDirective line = do
  afterName <- BS8.stripPrefix "{-# LINE" line
  let afterSpace = BS8.dropWhile (== ' ') afterName
      (digits, afterDigits) = BS8.span isDigit afterSpace
  -- A pragma whose name is not followed by a space is a different pragma,
  -- and one with no line number is not line control.
  if BS8.null digits || BS8.length afterName == BS8.length afterSpace
    then Nothing
    else do
      quoted <- BS8.stripPrefix "\"" (BS8.dropWhile (== ' ') afterDigits)
      let (file, afterFile) = BS8.break (== '"') quoted
      afterQuote <- BS8.stripPrefix "\"" afterFile
      closing <- BS8.stripPrefix "#-}" (BS8.dropWhile (== ' ') afterQuote)
      if BS8.all (\byte -> byte == ' ' || byte == '\r') closing
        then Just ("#line " <> digits <> " \"" <> file <> "\"")
        else Nothing
