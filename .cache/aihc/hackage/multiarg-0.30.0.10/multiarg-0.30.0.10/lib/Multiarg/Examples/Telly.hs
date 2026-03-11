-- | Telly is a simple command-line program to test command-line
-- parsers that do not have multiple modes.  This includes most
-- command-line programs; you build parsers like this using
-- "Multiarg".  This module provides an example for documentation
-- purposes; it also provides fodder for the QuickCheck test cases.
-- You will want to look at the source code.

module Multiarg.Examples.Telly where

import Multiarg

-- | A data type to hold the result of command line parsing.
data Telly
  = PosArg String
  -- ^ Positional argument

  | Empty
  -- ^ @--empty@, @-e@
  | Single String
  -- ^ @--single@, @-s@
  | Double String String
  -- ^ @--double@, @-d@
  | Triple String String String
  -- ^ @--triple@, @-t@

  | Zero
  -- ^ @-0@
  | One String
  -- ^ @-1@
  | Two String String
  -- ^ @-2@
  | Three String String String
  -- ^ @-3@

  | Cero
  -- ^ @--cero@
  | Uno String
  -- ^ @--uno@
  | Dos String String
  -- ^ @--dos@
  | Tres String String String
  -- ^ @--tres@
  deriving (Eq, Ord, Show)

optSpecs :: [OptSpec Telly]
optSpecs =
  [ optSpec "e" ["empty"] (ZeroArg Empty)
  , optSpec "s" ["single"] (OneArg Single)
  , optSpec "d" ["double"] (TwoArg Double)
  , optSpec "t" ["triple"] (ThreeArg Triple)

  , optSpec "0" [] (ZeroArg Zero)
  , optSpec "1" [] (OneArg One)
  , optSpec "2" [] (TwoArg Two)
  , optSpec "3" [] (ThreeArg Three)

  , optSpec "" ["cero"] (ZeroArg Cero)
  , optSpec "" ["uno"] (OneArg Uno)
  , optSpec "" ["dos"] (TwoArg Dos)
  , optSpec "" ["tres"] (ThreeArg Tres)
  ]

help :: String -> String
help progName = unlines
  [ progName ++ " - simple program to test Multiarg."
  , "Parses command line and prints the results to standard output."
  , "Usage:"
  , progName ++ " [options] ARGUMENTS..."
  , ""
  , "Options:"
  , ""
  , "--empty, -e - option that takes no arguments"
  , "--single ARG, -s ARG - option that takes one argument"
  , "--double ARG1 ARG2, -d ARG1 ARG2 - option that takes two arguments"
  , "--triple ARG1 ARG2 ARG3, -t ARG1 ARG2 ARG3"
  , "  - option that takes three arguments"
  , ""
  , "--cero - same as --empty"
  , "--uno - same as --single"
  , "--dos - same as --double"
  , "--tres - same as --triple"
  , ""
  , "--help, -h - show help and exit"
  ]

parse :: IO [Telly]
parse = parseCommandLine help optSpecs PosArg

