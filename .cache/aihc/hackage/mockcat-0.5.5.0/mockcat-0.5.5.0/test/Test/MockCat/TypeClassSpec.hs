{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE TypeApplications #-}

module Test.MockCat.TypeClassSpec (spec) where

import Data.Text (Text, pack)
import Test.Hspec (Spec, it, shouldBe)
import Test.MockCat
import Prelude hiding (readFile, writeFile)
import Data.Data
import Data.List (find)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe (fromMaybe)
import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (ask, MonadReader (local))
import Control.Monad.Trans.Class (lift)

class Monad m => FileOperation m where
  readFile :: FilePath -> m Text
  writeFile :: FilePath -> Text -> m ()

class Monad m => ApiOperation m where
  post :: Text -> m ()

program ::
  (MonadReader String m, FileOperation m, ApiOperation m) =>
  FilePath ->
  FilePath ->
  (Text -> Text) ->
  m ()
program inputPath outputPath modifyText = do
  e <- ask
  content <- readFile inputPath
  let modifiedContent = modifyText content
  writeFile outputPath modifiedContent
  post $ modifiedContent <> pack ("+" <> e)

instance MonadIO m => FileOperation (MockT m) where
  readFile path = MockT do
    defs <- getDefinitions
    let
      mock = fromMaybe (error "no answer found stub function `readFile`.") $ findParam (Proxy :: Proxy "readFile") defs
      !result = stubFn mock path
    pure result

  writeFile path content = MockT do
    defs <- getDefinitions
    let
      mock = fromMaybe (error "no answer found stub function `writeFile`.") $ findParam (Proxy :: Proxy "writeFile") defs
      !result = stubFn mock path content
    pure result

instance MonadIO m => ApiOperation (MockT m) where
  post content = MockT do
    defs <- getDefinitions
    let
      mock = fromMaybe (error "no answer found stub function `post`.") $ findParam (Proxy :: Proxy "post") defs
      !result = stubFn mock content
    pure result

instance MonadIO m => MonadReader String (MockT m) where
  ask = MockT do
    defs <- getDefinitions
    let
      mock = fromMaybe (error "no answer found stub function `ask`.") $ findParam (Proxy :: Proxy "ask") defs
      !result = stubFn mock
    pure result
  local = undefined

_ask :: MonadIO m => params -> MockT m ()
_ask p = MockT $ do
  mockInstance <- liftIO $ createNamedConstantMock "ask" p
  addDefinition (Definition (Proxy :: Proxy "ask") mockInstance shouldApplyToAnything)

_readFile :: (MockBuilder params (FilePath -> Text) (Param FilePath), MonadIO m) => params -> MockT m ()
_readFile p = MockT $ do
  mockInstance <- liftIO $ createNamedMock "readFile" p
  addDefinition (Definition (Proxy :: Proxy "readFile") mockInstance shouldApplyToAnything)

_writeFile :: (MockBuilder params (FilePath -> Text -> ()) (Param FilePath :> Param Text), MonadIO m) => params -> MockT m ()
_writeFile p = MockT $ do
  mockInstance <- liftIO $ createNamedMock "writeFile" p
  addDefinition (Definition (Proxy :: Proxy "writeFile") mockInstance shouldApplyToAnything)

_post :: (MockBuilder params (Text -> ()) (Param Text), MonadIO m) => params -> MockT m ()
_post p = MockT $ do
  mockInstance <- liftIO $ createNamedMock "post" p
  addDefinition (Definition (Proxy :: Proxy "post") mockInstance shouldApplyToAnything)

findParam :: KnownSymbol sym => Proxy sym -> [Definition] -> Maybe a
findParam pa definitions = do
  let definition = find (\(Definition s _ _) -> symbolVal s == symbolVal pa) definitions
  fmap (\(Definition _ mock _) -> unsafeCoerce mock) definition

class Monad m => TestClass m where
  echo :: String -> m ()
  getBy :: String -> m Int

echoProgram :: MonadIO m => TestClass m => String -> m ()
echoProgram s = do
  v <- getBy s
  liftIO $ print v
  echo $ show v

instance MonadIO m => TestClass (MockT m) where
  getBy a = MockT do
    defs <- getDefinitions
    let
      mock = fromMaybe (error "no answer found stub function `_getBy`.") $ findParam (Proxy :: Proxy "_getBy") defs
      !result = stubFn mock a
    lift result

  echo a = MockT do
    defs <- getDefinitions
    let
      mock = fromMaybe (error "no answer found stub function `_echo`.") $ findParam (Proxy :: Proxy "_echo") defs
      !result = stubFn mock a
    lift result

_getBy :: (MockBuilder params (String -> m Int) (Param String), MonadIO m) => params -> MockT m ()
_getBy p = MockT $ do
  mockInstance <- liftIO $ createNamedMock "_getBy" p
  addDefinition (Definition (Proxy :: Proxy "_getBy") mockInstance shouldApplyToAnything)

_echo :: (MockBuilder params (String -> m ()) (Param String), MonadIO m) => params -> MockT m ()
_echo p = MockT $ do
  mockInstance <- liftIO $ createNamedMock "_echo" p
  addDefinition (Definition (Proxy :: Proxy "_echo") mockInstance shouldApplyToAnything)


class Monad m => Teletype m where
  readTTY :: m String
  writeTTY :: String -> m ()

echo2 :: Teletype m => m ()
echo2 = do
  i <- readTTY
  case i of
    "" -> pure ()
    _  -> writeTTY i >> echo2

instance MonadIO m => Teletype (MockT m) where
  readTTY = MockT do
    defs <- getDefinitions
    let
      mock = fromMaybe (error "no answer found stub function `_readTTY`.") $ findParam (Proxy :: Proxy "_readTTY") defs
      !result = stubFn mock
    lift result

  writeTTY a = MockT do
    defs <- getDefinitions
    let
      mock = fromMaybe (error "no answer found stub function `_writeTTY`.") $ findParam (Proxy :: Proxy "_writeTTY") defs
      !result = stubFn mock a
    lift result

_readTTY :: (MockBuilder params (m String) (), MonadIO m) => params -> MockT m ()
_readTTY p = MockT $ do
  mockInstance <- liftIO $ createNamedMock "_readTTY" p
  addDefinition (Definition (Proxy :: Proxy "_readTTY") mockInstance shouldApplyToAnything)

_writeTTY :: (MockBuilder params (String -> m ()) (Param String), MonadIO m) => params -> MockT m ()
_writeTTY p = MockT $ do
  mockInstance <- liftIO $ createNamedMock "_writeTTY" p
  addDefinition (Definition (Proxy :: Proxy "_writeTTY") mockInstance shouldApplyToAnything)

spec :: Spec
spec = do
  it "echo" do
    result <- runMockT do
      _readTTY $ casesIO [
        "a",
        ""
        ]
      _writeTTY $ "a" |> pure @IO ()
      echo2
    result `shouldBe` ()

  it "Read, edit, and output files" do
    modifyContentStub <- createStubFn $ pack "content" |> pack "modifiedContent"

    result <- runMockT do
      _ask "environment"
      _readFile ("input.txt" |> pack "content")
      _writeFile $ "output.text" |> pack "modifiedContent" |> ()
      _post $ pack "modifiedContent+environment" |> ()
      program "input.txt" "output.text" modifyContentStub

    result `shouldBe` ()

  it "return monadic value test" do
    result <- runMockT do
      _getBy $ "s" |> pure @IO (10 :: Int)
      _echo $ "10" |> pure @IO ()
      echoProgram "s"

    result `shouldBe` ()