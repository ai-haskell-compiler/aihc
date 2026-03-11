{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-
  This module tests the construction of some simple values
  using a registry
-}
module Test.Data.Registry.SmallExample where

import Data.Registry
import Data.Text (splitOn)
import Protolude as P
import Test.Tasty.Extensions hiding (run)

-- | Components of the application
--     - a Logger
--     - an interface to S3
--     - a lines counter
--     - the top level application
newtype Logger = Logger
  { info :: Text -> IO ()
  }
  deriving (Typeable)

newLogger :: Logger
newLogger = Logger print

noLogging = Logger (const (pure ()))

newtype LinesCounter = LinesCounter
  { count :: Text -> Int
  }
  deriving (Typeable)

newLinesCounter :: LinesCounter
newLinesCounter = LinesCounter $ \t -> length (splitOn "\n" t)

newtype S3 = S3
  { store :: Text -> IO ()
  }
  deriving (Typeable)

data S3Config = S3Config
  { bucket :: Text,
    key :: Text
  }
  deriving (Eq, Show, Typeable)

newS3 :: MonadIO m => S3Config -> Logger -> m S3
newS3 config logger =
  pure $
    S3 $
      const $ info logger $ "storing on S3 with config " <> P.show config

newtype Application = Application
  { run :: Text -> IO Int
  }
  deriving (Typeable)

newApplication :: MonadIO m => Logger -> LinesCounter -> S3 -> m Application
newApplication Logger {..} LinesCounter {..} S3 {..} = pure . Application $ \t -> do
  info "count lines"
  let n = count t

  info "store the lines on s3"
  store ("counted " <> P.show n <> " lines")
  pure n

-- | Create a registry for all constructors
registry =
  funTo @IO (newApplication @IO)
    <: funTo @IO (newS3 @IO)
    <: funTo @IO noLogging
    <: funTo @IO newLinesCounter
    <: valTo @IO (S3Config "bucket" "key")

-- | To create the application you call `make` for the `Application` type
--   with the registry above
--   Since the registry contains all functions and values necessary to create the application
--   Everything will work fine
createApplication :: IO Application
createApplication = make @(IO Application) (funTo @IO noLogging <: registry)

test_create = test "create the application" $ do
  app <- liftIO createApplication -- nothing should crash!
  r <- liftIO $ run app "hello\nworld"
  r === 2
