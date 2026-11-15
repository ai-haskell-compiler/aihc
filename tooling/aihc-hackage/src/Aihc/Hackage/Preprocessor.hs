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
    c2hsImports,
  )
where

import Data.Text (Text)
import Data.Text qualified as T

-- | A tool that turns a source file into a Haskell module.
data Preprocessor
  = -- | @.chs@ files: C bindings that c2hs writes as Haskell. c2hs reads
    -- the C headers with the C preprocessor of the target. A module can
    -- use the bindings of a different module with an @{\# import \#}@
    -- hook, and then c2hs reads the @.chi@ file of that module.
    C2hs
  | -- | @.hsc@ files: C constants, structure sizes and field offsets
    -- computed with the C compiler of the target.
    Hsc2hs
  | -- | @.x@ files: lexers that the alex lexer generator writes as
    -- Haskell.
    Alex
  | -- | @.y@ and @.ly@ files: parsers that the happy parser generator
    -- writes as Haskell.
    Happy
  deriving (Eq, Ord, Show, Bounded, Enum)

-- | The preprocessor a source file suffix selects, without the leading dot.
preprocessorForExtension :: String -> Maybe Preprocessor
preprocessorForExtension extension =
  case extension of
    "chs" -> Just C2hs
    "hsc" -> Just Hsc2hs
    "x" -> Just Alex
    "y" -> Just Happy
    "ly" -> Just Happy
    _ -> Nothing

-- | The suffixes that select a preprocessor, in the order Cabal tries them.
preprocessorExtensions :: [String]
preprocessorExtensions = ["chs", "hsc", "x", "y", "ly"]

-- | The name of the executable, which is also the name Cabal files use in
-- @build-tool-depends@.
preprocessorToolName :: Preprocessor -> String
preprocessorToolName preprocessor =
  case preprocessor of
    C2hs -> "c2hs"
    Hsc2hs -> "hsc2hs"
    Alex -> "alex"
    Happy -> "happy"

-- | The environment variable that names the executable instead of the
-- search path, in the manner of @AIHC_WASM_CLANG@.
preprocessorEnvironmentVariable :: Preprocessor -> String
preprocessorEnvironmentVariable preprocessor =
  case preprocessor of
    C2hs -> "AIHC_C2HS"
    Hsc2hs -> "AIHC_HSC2HS"
    Alex -> "AIHC_ALEX"
    Happy -> "AIHC_HAPPY"

-- | The modules that the @{\# import \#}@ hooks of a @.chs@ source name.
-- c2hs must process those modules first, because it reads their @.chi@
-- files.
c2hsImports :: Text -> [String]
c2hsImports source =
  [ T.unpack moduleName
  | hook <- drop 1 (T.splitOn "{#" source),
    moduleName <- case T.words (fst (T.breakOn "#}" hook)) of
      ["import", "qualified", name] -> [name]
      ["import", name] -> [name]
      _ -> []
  ]
