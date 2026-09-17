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
  )
where

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
