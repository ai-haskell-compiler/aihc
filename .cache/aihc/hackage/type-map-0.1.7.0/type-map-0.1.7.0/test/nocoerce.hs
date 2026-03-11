{-# LANGUAGE
    DataKinds,
    TypeApplications #-}

-- Regression test for TypeList roles
-- https://github.com/Lysxia/type-map/issues/7

-- To test that a type error happens, we defer it to runtime where we can catch it.
{-# OPTIONS_GHC -fdefer-type-errors #-}

import Control.Exception (throw, try, evaluate, TypeError(..))
import Data.Coerce (coerce)
import Data.List (isInfixOf)

import Data.TypeMap.List (TypeList, empty, (<|), index)

e :: TypeList '[ '("a", Int) ]
e = 0 <| empty

e' :: TypeList '[ '("b", Int) ]
e' = coerce e

main :: IO ()
main = do
  ex <- try (evaluate (index @"b" e'))
  case ex of
    -- A minimal test to ensure the error didn't change too much.
    Left (TypeError err) | "coerce e" `isInfixOf` err -> pure ()
    Left tyerr -> throw tyerr
    Right _ -> fail "This test was expected to fail typechecking, but didn't."
