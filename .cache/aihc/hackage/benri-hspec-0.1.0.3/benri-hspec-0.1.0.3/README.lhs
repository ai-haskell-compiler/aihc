# benri-hspec

[![GitHub CI](https://github.com/adetokunbo/benri-hspec/actions/workflows/ci.yml/badge.svg)](https://github.com/adetokunbo/benri-hspec/actions)
[![Stackage Nightly](http://stackage.org/package/benri-hspec/badge/nightly)](http://stackage.org/nightly/package/benri-hspec)
[![Hackage][hackage-badge]][hackage]
[![Hackage Dependencies][hackage-deps-badge]][hackage-deps]
[![BSD3](https://img.shields.io/badge/license-BSD3-green.svg?dummy)](https://github.com/adetokunbo/benri-hspec/blob/master/LICENSE)

`benri-hspec` is a small library of __convenient__ functions for writing hspec tests.

It's simplifies test code that returns `Either` or `Maybe` types from monadic code.

## Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

import System.Environment
  ( lookupEnv,
    setEnv,
    unsetEnv
   )
import Text.Read   (readEither)
import Test.Hspec
import Test.Hspec.Benri

spec :: Spec
spec = describe "Checking the functions in Test.Hspec.Benri" $ before_ clearIt $ do
  context "endsJust_" $ do
    it "should succeed if a Just is returned" $ do
      setIt
      endsJust_ getIt

  context "endsJust" $ do
    it "should match the Just value" $ do
      setIt
      getIt `endsJust` "1"

  context "endsNothing" $ do
    it "should succeed when the action returns Nothing" $ do
      setIt
      getIt `endsJust` "1"
      clearIt
      endsNothing getIt

  context "endsLeft_" $ do
    it "should succeed if a Left is returned" $ do
      setNotInt
      endsLeft_ getAsInt

  context "endsLeft" $ do
    it "should match the Left value" $ getAsInt `endsLeft` "not set!"

  context "endsRight_" $ do
    it "should succeed if a Right is returned" $ do
      setIt
      endsRight_ getAsInt

  context "endsRight" $ do
    it "should match the Right value" $ do
      setIt
      getAsInt `endsRight` 1

  context "endsThen" $ do
    it "should implement the behaviour of the other functions easily" $ do
      setIt
      getIt `endsThen` (== (Just "1"))
      clearIt
      getIt `endsThen` (== Nothing)
      getAsInt `endsThen` (== (Left "not set!"))
      setIt
      getAsInt `endsThen` (== (Right 1))

getIt :: IO (Maybe String)
getIt = lookupEnv envName

getAsInt :: IO (Either String Int)
getAsInt = maybe (Left "not set!") readEither <$> getIt

setIt :: IO ()
setIt = setEnv envName "1"

setNotInt :: IO ()
setNotInt = setEnv envName "foo"

clearIt :: IO ()
clearIt = unsetEnv envName

envName :: String
envName = "AN_ENV_VAR"

main :: IO ()
main = hspec spec

```

[hackage-deps-badge]: <https://img.shields.io/hackage-deps/v/benri-hspec.svg>
[hackage-deps]:       <http://packdeps.haskellers.com/feed?needle=benri-hspec>
[hackage-badge]:      <https://img.shields.io/hackage/v/benri-hspec.svg>
[hackage]:            <https://hackage.haskell.org/package/benri-hspec>
