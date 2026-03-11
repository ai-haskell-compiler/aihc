----------------------------------------------------------------------------
-- |
-- Module      :  TomlReaderTests
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module TomlReaderTests (main) where

import Control.Arrow (left)
import Data.Text (Text)
import Options.Applicative as Opts
import Prettyprinter
import Prettyprinter.Combinators
import Prettyprinter.Generics
import System.Exit
import System.IO

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Options qualified as Tasty
import Test.Tasty.Runners qualified as Tasty

import TOML as Toml
import TOML.Parse as Toml

data Config = Config
  { cfgTastyOpts :: !Tasty.OptionSet
  }

optsParser :: Opts.Parser Tasty.OptionSet -> Opts.Parser Config
optsParser tastyParser = do
  cfgTastyOpts <- tastyParser
  pure Config{..}

progInfo :: Opts.Parser Tasty.OptionSet -> Opts.ParserInfo Config
progInfo tastyParser = info
  (helper <*> optsParser tastyParser)
  (fullDesc <> header "My new shiny test suite!")

main :: IO ()
main = do
  Tasty.installSignalHandlers

  let allTests    = tests
      ingredients = defaultIngredients
      tastyParser = snd $ Tasty.suiteOptionParser ingredients allTests

  Config{cfgTastyOpts} <-
    customExecParser (prefs (showHelpOnEmpty <> noBacktrack <> multiSuffix "*")) (progInfo tastyParser)

  case Tasty.tryIngredients ingredients cfgTastyOpts allTests of
    Nothing -> do
      hPutStrLn stderr
        "No ingredients agreed to run. Something is wrong either with your ingredient set or the options."
      exitFailure
    Just act -> do
      ok <- act
      if ok then exitSuccess else exitFailure

data Sample1 = Sample1
  { _sample1Bar  :: String
  , _sample1Frob :: [Int]
  } deriving (Eq, Show)

instance FromToml Toml.Value Sample1 where
  fromToml x = do
    (foo :: L Table) <- x .: "foo"
    Sample1 <$> foo .: "bar" <*> foo .: "frob"

data Sample2 = Sample2
  { _sample2Bar  :: String
  , _sample2Frob :: [Int]
  } deriving (Eq, Show)

instance FromToml Toml.Value Sample2 where
  fromToml x = do
    foo <- x .: "foo" >>= pTableL
    Sample2 <$> foo .: "bar" <*> foo .: "frob"

newtype SpecialString = SpecialString { unSpecialString :: String }
  deriving (Eq, Show, FromToml Value)

data Sample3 = Sample3
  { _sample3Bar  :: SpecialString
  , _sample3Frob :: [Int]
  } deriving (Eq, Show)

instance FromToml Toml.Value Sample3 where
  fromToml x = do
    (foo :: L Table) <- x .: "foo"
    Sample3 <$> foo .: "bar" <*> foo .: "frob"

sample :: Text
sample =
  "\
  \[foo]\n\
  \bar = \"quux\"\n\
  \frob = [1, 2]\n\
  \"

deriving instance Generic Value
deriving instance Generic ContextItem
deriving instance Generic DecodeError
deriving instance Generic NormalizeError
deriving instance Generic TOMLError

instance Pretty Value where
  pretty = ppToml

instance Pretty ContextItem where
  pretty = ppGeneric

instance Pretty DecodeError where
  pretty = ppGeneric

instance Pretty NormalizeError where
  pretty = ppGeneric

instance Pretty TOMLError where
  pretty = ppGeneric

tests :: TestTree
tests = testGroup "Tests"
  [ testCase "Test 1" $
      (left renderString (left pretty (Toml.decode sample) >>= (\x -> Toml.runParser (x :: Value) Toml.fromToml))) @?=
        Right (Sample1 "quux" [1, 2])
  , testCase "Test 2" $
      (left renderString (left pretty (Toml.decode sample) >>= (\x -> Toml.runParser (x :: Value) Toml.fromToml))) @?=
        Right (Sample2 "quux" [1, 2])
  , testCase "Test 3" $
      (left renderString (left pretty (Toml.decode sample) >>= (\x -> Toml.runParser (x :: Value) Toml.fromToml))) @?=
        Right (Sample3 (SpecialString "quux") [1, 2])
  ]
