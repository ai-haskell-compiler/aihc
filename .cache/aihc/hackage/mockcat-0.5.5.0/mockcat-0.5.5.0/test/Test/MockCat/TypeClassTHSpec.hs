{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE TypeApplications #-}

module Test.MockCat.TypeClassTHSpec (spec) where

import Prelude hiding (readFile, writeFile, any)
import Data.Text (Text, pack, isInfixOf)
import Test.Hspec
import Test.MockCat
import Control.Monad.State
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad (unless)
import Control.Monad.IO.Unlift (withRunInIO, MonadUnliftIO)
import Control.Concurrent.Async (async, wait)

class Monad m => FileOperation m where
  writeFile :: FilePath -> Text -> m ()
  readFile :: FilePath -> m Text

class Monad m => ApiOperation m where
  post :: Text -> m ()

operationProgram ::
  FileOperation m =>
  FilePath ->
  FilePath ->
  m ()
operationProgram inputPath outputPath = do
  content <- readFile inputPath
  unless (pack "ngWord" `isInfixOf` content) $
    writeFile outputPath content

operationProgram2 ::
  (FileOperation m, ApiOperation m) =>
  FilePath ->
  FilePath ->
  (Text -> Text) ->
  m ()
operationProgram2 inputPath outputPath modifyText = do
  content <- readFile inputPath
  let modifiedContent = modifyText content
  writeFile outputPath modifiedContent
  post modifiedContent

data Environment = Environment { inputPath :: String, outputPath :: String }

operationProgram3 ::
  MonadReader Environment m =>
  FileOperation m =>
  m ()
operationProgram3 = do
  (Environment inputPath outputPath) <- ask
  content <- readFile inputPath
  writeFile outputPath content

class (Eq s, Show s, MonadState s m) => MonadStateSub s m where
  fn_state :: Maybe s -> m s

class (MonadState String m) => MonadStateSub2 s m where
  fn_state2 :: String -> m ()

class Monad m => MonadVar2_1 m a where
class MonadVar2_1 m a => MonadVar2_1Sub m a where
  fn2_1Sub :: String -> m ()

class Monad m => MonadVar2_2 a m where
class MonadVar2_2 a m => MonadVar2_2Sub a m where
  fn2_2Sub :: String -> m ()

class MonadIO m => MonadVar3_1 m a b where
class MonadVar3_1 m a b => MonadVar3_1Sub m a b where
  fn3_1Sub :: String -> m ()

class MonadIO m => MonadVar3_2 a m b where
class MonadVar3_2 a m b => MonadVar3_2Sub a m b where
  fn3_2Sub :: String -> m ()

class MonadIO m => MonadVar3_3 a b m where
class MonadVar3_3 a b m => MonadVar3_3Sub a b m where
  fn3_3Sub :: String -> m ()

--makeMock [t|MonadReader Bool|]
makeMock [t|MonadReader Environment|]
makeMock [t|MonadVar2_1Sub|]
makeMock [t|MonadVar2_2Sub|]
makeMock [t|MonadVar3_1Sub|]
makeMock [t|MonadVar3_2Sub|]
makeMock [t|MonadVar3_3Sub|]
makeMock [t|FileOperation|]
makeMockWithOptions [t|ApiOperation|] options { prefix = "stub_", suffix = "_fn" }

class Monad m => ParamThreeMonad a b m | m -> a, m -> b where
  fnParam3_1 :: a -> b -> m String
  fnParam3_2 :: m a
  fnParam3_3 :: m b



class Monad m => MultiApplyTest m where
  getValueBy :: String -> m String

getValues :: MultiApplyTest m => [String] -> m [String]
getValues = mapM getValueBy

makeMock [t|MultiApplyTest|]

class Monad m => ExplicitlyReturnMonadicValuesTest m where
  echo :: String -> m ()
  getBy :: String -> m Int
  
echoProgram :: ExplicitlyReturnMonadicValuesTest m => String -> m ()
echoProgram s = do
  v <- getBy s
  echo $ show v

makeMockWithOptions [t|ExplicitlyReturnMonadicValuesTest|] options { implicitMonadicReturn = False }

class MonadUnliftIO m => MonadAsync m where
  mapConcurrently :: Traversable t => (a -> m b) -> t a -> m (t b)

instance (MonadUnliftIO m) => MonadAsync (MockT m) where
  mapConcurrently = traverse

processFiles :: MonadAsync m => FileOperation m => [FilePath] -> m [Text]
processFiles = mapConcurrently readFile

spec :: Spec
spec = do
  it "Read, and output files" do
    result <- runMockT do
      _readFile $ "input.txt" |> pack "content"
      _writeFile $ "output.txt" |> pack "content" |> ()
      operationProgram "input.txt" "output.txt"

    result `shouldBe` ()

  it "Read, and output files (contain ng word)" do
    result <- runMockT do
      _readFile ("input.txt" |> pack "contains ngWord")
      _writeFile ("output.txt" |> any |> ()) `applyTimesIs` 0
      operationProgram "input.txt" "output.txt"

    result `shouldBe` ()

  it "Read, and output files (contain ng word)2" do
    result <- runMockT do
      _readFile ("input.txt" |> pack "contains ngWord")
      neverApply $ _writeFile ("output.txt" |> any |> ())
      operationProgram "input.txt" "output.txt"

    result `shouldBe` ()

  it "Read, and output files (with MonadReader)" do
    r <- runMockT do
      _ask (Environment "input.txt" "output.txt")
      _readFile ("input.txt" |> pack "content")
      _writeFile ("output.txt" |> pack "content" |> ())
      operationProgram3
    r `shouldBe` ()


  it "Read, edit, and output files2" do
    modifyContentStub <- createStubFn $ pack "content" |> pack "modifiedContent"

    result <- runMockT do
      _readFile $ "input.txt" |> pack "content"
      _writeFile ("output.text" |> pack "modifiedContent" |> ()) 
      stub_post_fn (pack "modifiedContent" |> ())
      operationProgram2 "input.txt" "output.text" modifyContentStub

    result `shouldBe` ()
  
  
  it "Multi apply" do
    result <- runMockT do
      _getValueBy $ do
        onCase $ "a" |> "ax"
        onCase $ "b" |> "bx"
        onCase $ "c" |> "cx"
      getValues ["a", "b", "c"]
    result `shouldBe` ["ax", "bx", "cx"]

  it "Return monadic value test" do
    result <- runMockT do
      _getBy $ "s" |> pure @IO (10 :: Int)
      _echo $ "10" |> pure @IO ()
      echoProgram "s"

    result `shouldBe` ()

  it "MonadUnliftIO instance works correctly" do
    result <- runMockT do
      _readFile ("test.txt" |> pack "content")

      content <- withRunInIO $ \runInIO -> do
        asyncAction <- async $ runInIO (readFile "test.txt")
        wait asyncAction

      liftIO $ content `shouldBe` pack "content"
      pure content

    result `shouldBe` pack "content"

  it "MonadUnliftIO basic functionality" do
    result <- runMockT do
      _readFile ("test.txt" |> pack "test content")
      
      content <- withRunInIO $ \runInIO -> do
        runInIO (readFile "test.txt")
      
      liftIO $ content `shouldBe` pack "test content"
      pure content

    result `shouldBe` pack "test content"
  
  it "MonadAsync type class can be instantiated for MockT" do
    result <- runMockT do
      _readFile $ do
        onCase $ "file1.txt" |> pack "content1"
        onCase $ "file2.txt" |> pack "content2"

      processFiles ["file1.txt", "file2.txt"]
    result `shouldBe` [pack "content1", pack "content2"]

