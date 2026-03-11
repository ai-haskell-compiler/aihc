{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Test.MockCat.PartialMockSpec (spec) where

import Data.Text (Text, pack)
import Test.Hspec (Spec, it, shouldBe, describe)
import Test.MockCat
import Test.MockCat.Definition
import Test.MockCat.Impl ()
import Prelude hiding (readFile, writeFile)
import Data.Data
import Data.List (find)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Unsafe.Coerce (unsafeCoerce)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader hiding (ask)

instance (MonadIO m, FileOperation m) => FileOperation (MockT m) where
  readFile path = MockT do
    defs <- getDefinitions
    case findParam (Proxy :: Proxy "readFile") defs of
      Just mock -> do
        let !result = stubFn mock path
        pure result
      Nothing -> lift $ readFile path

  writeFile path content = MockT do
    defs <- getDefinitions
    case findParam (Proxy :: Proxy "writeFile") defs of
      Just mock -> do
        let !result = stubFn mock path content
        pure result
      Nothing -> lift $ writeFile path content

_readFile :: (MockBuilder params (FilePath -> Text) (Param FilePath), MonadIO m) => params -> MockT m ()
_readFile p = MockT $ do
  mockInstance <- liftIO $ createNamedMock "readFile" p
  addDefinition (Definition (Proxy :: Proxy "readFile") mockInstance shouldApplyToAnything)

_writeFile :: (MockBuilder params (FilePath -> Text -> ()) (Param FilePath :> Param Text), MonadIO m) => params -> MockT m ()
_writeFile p = MockT $ do
  mockInstance <- liftIO $ createNamedMock "writeFile" p
  addDefinition (Definition (Proxy :: Proxy "writeFile") mockInstance shouldApplyToAnything)

findParam :: KnownSymbol sym => Proxy sym -> [Definition] -> Maybe a
findParam pa definitions = do
  let definition = find (\(Definition s _ _) -> symbolVal s == symbolVal pa) definitions
  fmap (\(Definition _ mock _) -> unsafeCoerce mock) definition

instance (MonadIO m, Finder a b m) => Finder a b (MockT m) where
  findIds :: (Finder a b m) => MockT m [a]
  findIds = MockT do
    defs <- getDefinitions
    case findParam (Proxy :: Proxy "_findIds") defs of
      Just mock -> do
        let !result = stubFn mock
        pure result
      Nothing -> lift findIds
  findById :: (Finder a b m) => a -> MockT m b
  findById id = MockT do
    defs <- getDefinitions
    case findParam (Proxy :: Proxy "_findById") defs of
      Just mock -> do
        let !result = stubFn mock id
        pure result
      Nothing -> lift $ findById id

_findIds :: MonadIO m => r -> MockT m ()
_findIds p = MockT $ do
  mockInstance <- liftIO $ createNamedConstantMock "_findIds" p
  addDefinition (Definition (Proxy :: Proxy "_findIds") mockInstance shouldApplyToAnything)

spec :: Spec
spec = do
  describe "Partial Mock Test" do
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

      it "partial 1" do
        values <- runMockT  do
          _findIds [1 :: Int, 2]
          findValue
        values `shouldBe` ["{id: 1}", "{id: 2}"]