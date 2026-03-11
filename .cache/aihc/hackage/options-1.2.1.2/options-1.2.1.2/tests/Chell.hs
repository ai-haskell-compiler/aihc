-- Excerpts copied out of the `chell` package and slightly modified
-- for use with `hspec`.
module Chell where

import Control.Monad (unless)
import Data.List (foldl', intercalate)
import Data.Maybe (isNothing)
import Patience qualified
import Test.Hspec (Expectation, expectationFailure)

assertBool :: Bool -> String -> Expectation
assertBool b s = unless b $ expectationFailure s

-- | Assert that some value is @Nothing@.
nothing :: Show a => Maybe a -> Expectation
nothing x =
  assertBool
    (isNothing x)
    ("nothing: received " ++ showsPrec 11 x "")

-- | Assert that some value is @Right@.
right :: Show a => Either a b -> Expectation
right (Right _) = pure ()
right (Left a) = expectationFailure ("right: received " ++ showsPrec 11 dummy "")
  where
    dummy = Left a `asTypeOf` Right ()

-- | Assert that some value is @Left@.
left :: Show b => Either a b -> Expectation
left (Left _) = pure ()
left (Right b) = expectationFailure ("left: received " ++ showsPrec 11 dummy "")
  where
    dummy = Right b `asTypeOf` Left ()

-- | Assert that two pieces of text are equal. This uses a diff algorithm
-- to check line-by-line, so the error message will be easier to read on
-- large inputs.
equalLines :: String -> String -> Expectation
equalLines x y = checkLinesDiff "equalLines" (lines x) (lines y)

checkLinesDiff :: String -> [String] -> [String] -> Expectation
checkLinesDiff label = go
  where
    go xs ys =
      case checkItems (Patience.diff xs ys) of
        (same, diff) -> assertBool same diff

    checkItems diffItems =
      case foldl' checkItem (True, []) diffItems of
        (same, diff) -> (same, errorMsg (intercalate "\n" (reverse diff)))

    checkItem (same, acc) item =
      case item of
        Patience.Old t -> (False, ("\t- " ++ t) : acc)
        Patience.New t -> (False, ("\t+ " ++ t) : acc)
        Patience.Both t _ -> (same, ("\t  " ++ t) : acc)

    errorMsg diff = label ++ ": lines differ\n" ++ diff
