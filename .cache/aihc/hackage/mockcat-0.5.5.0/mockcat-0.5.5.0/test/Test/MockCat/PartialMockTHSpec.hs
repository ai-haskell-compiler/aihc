{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.MockCat.PartialMockTHSpec (spec) where

import Data.Text (pack)
import Test.Hspec (Spec, it, shouldBe, describe)
import Test.MockCat

import Test.MockCat.Definition
import Test.MockCat.Impl ()
import Prelude hiding (readFile, writeFile)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Reader (ReaderT(..))

newtype UserInput = UserInput String deriving (Show, Eq)

class Monad m => UserInputGetter m where
  getInput :: m String
  toUserInput :: String -> m (Maybe UserInput)

getUserInput :: UserInputGetter m => m (Maybe UserInput)
getUserInput = do
  i <- getInput
  toUserInput i

instance UserInputGetter IO where
  getInput = getLine
  toUserInput "" = pure Nothing
  toUserInput a = (pure . Just . UserInput) a

class Monad m => ExplicitlyReturnMonadicValuesPartialTest m where
  echo :: String -> m ()
  getBy :: String -> m Int

instance ExplicitlyReturnMonadicValuesPartialTest IO where
  echo _ = pure () 
  getBy s = pure $ length s
  
echoProgram :: ExplicitlyReturnMonadicValuesPartialTest m => String -> m ()
echoProgram s = do
  v <- getBy s
  echo $ show v

makePartialMock [t|UserInputGetter|]
makePartialMock [t|Finder|]
makePartialMock [t|FileOperation|]
makePartialMockWithOptions [t|ExplicitlyReturnMonadicValuesPartialTest|] options { implicitMonadicReturn = False }

spec :: Spec
spec = do
  it "Get user input (has input)" do
    a <- runMockT do
      _getInput "value"
      getUserInput
    a `shouldBe` Just (UserInput "value")

  it "Get user input (no input)" do
    a <- runMockT do
      _getInput ""
      getUserInput
    a `shouldBe` Nothing

  describe "Partial Mock Test (TH)" do
    it "MaybeT" do
      result <- runMaybeT do
        runMockT do
          _writeFile $ "output.text" |> pack "MaybeT content" |> ()
          program "input.txt" "output.text"

      result `shouldBe` Just ()

    it "IO" do
      result <- runMockT do
        _writeFile $ "output.text" |> pack "IO content" |> ()
        program "input.txt" "output.text"

      result `shouldBe` ()

    it "ReaderT" do
      result <- flip runReaderT "foo" do
        runMockT do
          _writeFile $ "output.text" |> pack "ReaderT content foo" |> ()
          program "input.txt" "output.text"

      result `shouldBe` ()

    describe "MultiParamType" do
      it "all real function" do
        values <- runMockT findValue
        values `shouldBe` ["{id: 1}", "{id: 2}", "{id: 3}"]

      it "partial findIds" do
        values <- runMockT  do
          _findIds [1 :: Int, 2]
          findValue
        values `shouldBe` ["{id: 1}", "{id: 2}"]

      it "partial findById" do
        values <- runMockT  do
          _findById $ do
            onCase $ (1 :: Int) |> "id1"
            onCase $ (2 :: Int) |> "id2"
            onCase $ (3 :: Int) |> "id3"
          findValue
        values `shouldBe` ["id1", "id2", "id3"]

    it "Return monadic value test" do
      result <- runMockT do
        _echo $ "3" |> pure @IO ()
        echoProgram "abc"

      result `shouldBe` ()