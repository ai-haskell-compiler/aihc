{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.ByteString.Char8 qualified as C
import Data.Default (def)
import Data.Maybe
import Database.RocksDB
import Test.Hspec
import Text.Printf
import UnliftIO

conf :: Config
conf =
  def
    { createIfMissing = True,
      errorIfExists = True,
      bloomFilter = True,
      prefixLength = Just 3
    }

withTestDBCF :: (MonadUnliftIO m) => [String] -> (DB -> m a) -> m a
withTestDBCF cfs go =
  withSystemTempDirectory "rocksdb-tests-cf" $ \path ->
    withDBCF path conf (map (,conf) cfs) go

main :: IO ()
main = do
  hspec $ around (withTestDBCF ["one", "two", "tree"]) $ do
    describe "Database" $ do
      it "puts and gets an item" $ \db -> do
        put db "aaa" "zzz"
        get db "aaa" `shouldReturn` Just "zzz"
      it "puts and gets from different type families" $ \db -> do
        let two = head $ columnFamilies db
        put db "aaa_key" "aaa_value"
        get db "aaa_key" `shouldReturn` Just "aaa_value"
        getCF db two "aaa_key" `shouldReturn` Nothing
        putCF db two "two_key" "two_value"
        getCF db two "two_key" `shouldReturn` Just "two_value"
        get db "two_key" `shouldReturn` Nothing
    describe "Multithreading" $ do
      it "stores and retrieve items from multiple threads" $ \db -> do
        let key i = C.pack $ printf "key_%04d" i
            val i = C.pack $ printf "val_%04d" i
            indices = [0 .. 9999] :: [Int]
            keys = map key indices
            vals = map val indices
            kvs = zip keys vals
        was <- mapM (\(k, v) -> async $ put db k v) kvs
        mapM_ wait was
        ras <- mapM (async . get db) keys
        mapM wait ras `shouldReturn` map Just vals
    describe "Iterators" $ do
      it "retrieves entries using iterators" $ \db -> do
        let key i = C.pack $ printf "key_%03d" i
            val i = C.pack $ printf "val_%03d" i
            indices = [0 .. 999] :: [Int]
            keys = map key indices
            vals = map val indices
            kvs = zip keys vals
        was <- mapM (\(k, v) -> async $ put db k v) kvs
        mapM_ wait was
        kvs' <- withIter db $ \itr -> do
          let start = keys !! 500
          iterSeek itr start
          fmap catMaybes $ replicateM 500 $ do
            mkv <- iterEntry itr
            iterNext itr
            return mkv
        kvs' `shouldBe` drop 500 kvs
      it "walks back and forth" $ \db -> do
        withIter db $ \itr -> do
          iterSeek itr "hello"
          iterKey itr `shouldReturn` Nothing
          iterValue itr `shouldReturn` Nothing
          iterEntry itr `shouldReturn` Nothing
        put db "a" "aaa"
        put db "b" "bbb"
        put db "c" "ccc"
        withIter db $ \itr -> do
          iterSeek itr "b"
          iterKey itr `shouldReturn` Just "b"
          iterValue itr `shouldReturn` Just "bbb"
          iterNext itr
          iterKey itr `shouldReturn` Just "c"
          iterValue itr `shouldReturn` Just "ccc"
          iterNext itr -- After the last entry
          iterKey itr `shouldReturn` Nothing
          iterValue itr `shouldReturn` Nothing
          iterPrev itr -- It can't come back
          iterKey itr `shouldReturn` Nothing
          iterValue itr `shouldReturn` Nothing
        withIter db $ \itr -> do
          iterSeek itr "b"
          iterKey itr `shouldReturn` Just "b"
          iterValue itr `shouldReturn` Just "bbb"
          iterPrev itr
          iterKey itr `shouldReturn` Just "a"
          iterValue itr `shouldReturn` Just "aaa"
          iterPrev itr -- Invalid before lowest key
          iterKey itr `shouldReturn` Nothing
          iterValue itr `shouldReturn` Nothing
          iterNext itr -- But it remembers previous position
          iterKey itr `shouldReturn` Just "b"
          iterValue itr `shouldReturn` Just "bbb"
