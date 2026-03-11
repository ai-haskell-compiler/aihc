{-# LANGUAGE LambdaCase #-}

module Scientist.Test
  ( module X
  , module Scientist.Test
  ) where

import Prelude as X

import Test.Hspec as X
import UnliftIO.Exception (StringException(..))

shouldSatisfyMaybe
  :: (HasCallStack, Show a) => Maybe a -> (a -> Bool) -> Expectation
x `shouldSatisfyMaybe` f = x `shouldSatisfy` maybe False f

infix 1 `shouldSatisfyMaybe`

isWithinOf :: (Num a, Ord a) => a -> a -> a -> Bool
isWithinOf tolerance desired = \case
  n | n >= desired + tolerance -> False
  n | n <= desired - tolerance -> False
  _ -> True

shouldThrowString :: HasCallStack => IO a -> String -> Expectation
f `shouldThrowString` msg = f `shouldThrow` isStringException msg

infix 1 `shouldThrowString`

isStringException :: String -> StringException -> Bool
isStringException a = \case
  StringException b _cs -> b == a
