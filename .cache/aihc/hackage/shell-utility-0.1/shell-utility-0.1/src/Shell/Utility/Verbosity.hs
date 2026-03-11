module Shell.Utility.Verbosity (
   Verbosity,
   silent, normal, verbose, deafening,
   parse,
   ) where

import qualified Shell.Utility.Exit as Exit

import Control.Applicative (pure)


data Verbosity = Silent | Normal | Verbose | Deafening
   deriving (Show, Read, Eq, Ord, Enum, Bounded)


-- | We shouldn't print /anything/ unless an error occurs in silent mode
silent :: Verbosity
silent = Silent

-- | Print stuff we want to see by default
normal :: Verbosity
normal = Normal

-- | Be more verbose about what's going on
verbose :: Verbosity
verbose = Verbose

{- |
Not only are we verbose ourselves
(perhaps even noisier than when being 'verbose'),
but we tell everything we run to be verbose too
-}
deafening :: Verbosity
deafening = Deafening

parse :: (Exit.Exit m) => String -> m Verbosity
parse "" = Exit.exitFailureMsg "empty verbosity identifier"
parse [c] =
   case c of
      '0' -> pure Silent
      '1' -> pure Normal
      '2' -> pure Verbose
      '3' -> pure Deafening
      _ -> Exit.exitFailureMsg "verbosity must be a number from [0..3]"
parse _ = Exit.exitFailureMsg "more than one character for verbosity"
