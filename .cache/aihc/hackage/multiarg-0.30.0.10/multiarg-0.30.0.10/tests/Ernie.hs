-- | Functions to assist with testing.
module Ernie where

import Control.Applicative
import Makeopt
import Multiarg.Types
import Test.QuickCheck

-- | Generates words that start with a single hyphen.
startsWithOneHyphen :: Gen String
startsWithOneHyphen = fmap ('-':) (listOf1 arbitrary)

-- | Generates words that start with two hyphens.
startsWithTwoHyphens :: Gen String
startsWithTwoHyphens = fmap ("--" ++) arbitrary

-- | Generates words that do not start with a hyphen.
startsWithNonHyphen :: Gen String
startsWithNonHyphen = (:) <$> (arbitrary `suchThat` (/= '-'))
  <*> arbitrary

-- | Generates words for option arguments.  Ensures that some start
-- with hyphens (these are valid option arguments.)
optArg :: Gen String
optArg = oneof [ startsWithOneHyphen, startsWithNonHyphen ]

short :: Char -> [String] -> [[String]]
short c os = case shortName c of
  Nothing -> error "Ernie.hs: error: could not create short name"
  Just o -> processShortOptions [] (o, os)

long :: String -> [String] -> [[String]]
long s os = case longName s of
  Nothing -> error "Ernie.hs: error: could not create long name"
  Just o -> processLongOption o os

pickItem :: [a] -> Gen a
pickItem a
  | null a = fail "pickItem: empty list"
  | otherwise = fmap (a !!) (choose (0, length a - 1))

-- | Generates non-option positional arguments that appear to the
-- right of the stopper.  This can be any word at all.
posArgRight :: Gen String
posArgRight = oneof
  [ arbitrary, startsWithOneHyphen, startsWithTwoHyphens ]

-- | Generates non-option positional arguments that appear to the left
-- of the stopper.  Cannot be preceded by a dash; can, however, be a
-- single hyphen only.
posArgLeft :: Gen String
posArgLeft =
  frequency [ (5, startsWithNonHyphen)
            , (1, return "-") ]

-- | Generates options, non-option positional arguments that are a
-- single hyphen only, and non-option positional arguments that do not
-- start with a hyphen; these may appear to the left of a stopper.
preStopper
  :: Gen a
  -- ^ Generates options
  -> (String -> a)
  -- ^ Creates non-option positional arguments
  -> Gen [a]
preStopper genOpt fPos =
  listOf (oneof [ genOpt, fmap fPos posArgLeft ])

-- | Generates any word at all, with a healthy mix of empty lists
-- (stoppers are unusual.)
postStopper
  :: (String -> a)
  -- ^ Creates non-option positional arguments
  -> Gen [a]
postStopper fPos =
  oneof [ return [], listOf (fmap fPos posArgRight) ]

-- | Generates a valid list of interspersed command-line options; that
-- is, a list that the user could have entered in the command line.
-- This list may be transformed into strings, which can then be parsed
-- and compared against this original value.
--
-- Returns a pair @(a, b)@, where @a@ is everything to the left of the
-- stopper, and @b@ (if non-empty) is everything to the right of the
-- stopper.

interspersedLine
  :: Gen a
  -- ^ Generates options
  -> (String -> a)
  -- ^ Creates non-option positional arguments
  -> Gen ([a], [a])
interspersedLine genOpt fPos =
  (,)
  <$> preStopper genOpt fPos
  <*> postStopper fPos


-- | Takes an interspersed line and creates a set of strings that
-- would, when parsed, yield the interspersed line.
interspersedLineToStrings

  :: ([a], [a])
  -- ^ @(a, b)@, where
  --
  -- @a@ is everything to the left of the stopper, and
  --
  -- @b@ is everything to the right of the stopper

  -> (a -> [[String]])
  -- ^ Converts a single item to a nested list of String.  Each list
  -- of String is a possible way to render this item.  This list must
  -- not be empty.

  -> Gen [String]

interspersedLineToStrings (left, right) fConv = do
  l <- fmap concat . mapM pickItem . map fConv $ left
  r <- fmap concat . mapM pickItem . map fConv $ right
  alwaysStopper <- arbitrary
  let stop | not (null r) || alwaysStopper = ["--"]
           | otherwise = []
  return $ l ++ stop ++ r
