{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Test.MockCat.Impl (FileOperation, Finder) where

import Test.MockCat.Definition
import Data.Text (pack)
import Prelude hiding (readFile, writeFile)
import Control.Monad.Trans.Maybe
import Control.Monad.Reader

instance FileOperation IO where
  readFile _ = pure $ pack "IO content"
  writeFile _ _ = pure ()

instance Monad m => FileOperation (MaybeT m) where
  readFile _ = pure $ pack "MaybeT content"
  writeFile _ _ = undefined

instance Monad m => FileOperation (ReaderT String m) where
  readFile _ = do
    e <- ask
    pure $ pack "ReaderT content " <> pack e
  writeFile _ _ = undefined

instance Finder Int String IO where
  findIds = pure [1 :: Int, 2 :: Int, 3 :: Int]
  findById id = pure $ "{id: " <> show id <> "}"