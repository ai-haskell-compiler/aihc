{-# LANGUAGE ScopedTypeVariables #-}

module WVarSpec where

import qualified Test.Expectations as T
import qualified Test.WVar         as T

import qualified Test.Hspec      as HS
import qualified Test.QuickCheck as Q

import qualified Control.Concurrent.WVar as WV
import qualified Control.Monad           as M

takeWVarSpec :: HS.Spec
takeWVarSpec = HS.describe "takeWVar" $ do
    T.whenWVarIsFresh $ \ prepare -> do
        HS.it "takes the value without blocking" $ do
            (val, wv) <- prepare
            r <- WV.takeWVar wv `T.shouldNotBlock` 500000
            r `T.shouldBe` val
    T.whenWVarIsUpdating $ \ prepare -> do
        HS.it "blocks until WVar becomes fresh" $ do
            (val1 :: Int, wv) <- prepare
            val2 <- Q.generate $ Q.arbitrary `Q.suchThat` (/= val1)
            wait <- WV.takeWVar wv `T.shouldBlock` 500000
            M.void $ WV.putWVar wv val2 `T.shouldNotBlock` 500000
            r <- wait `T.shouldAwakeFinish` 500000
            r `T.shouldBe` val2

tryTakeWVarSpec :: HS.Spec
tryTakeWVarSpec = HS.describe "tryTakeWVar" $ do
    T.whenWVarIsFresh $ \ prepare -> do
        HS.it "takes the value without blocking" $ do
            (val :: Int, wv) <- prepare
            r <- WV.tryTakeWVar wv `T.shouldNotBlock` 500000
            r `T.shouldBe` (True, val)
    T.whenWVarIsUpdating $ \ prepare -> do
        HS.it "takes the latest value without blocking" $ do
            (val :: Int, wv) <- prepare
            r <- WV.tryTakeWVar wv `T.shouldNotBlock` 500000
            r `T.shouldBe` (False, val)

putWVarSpec :: HS.Spec
putWVarSpec = do
    T.whenWVarIsFresh $ \ prepare -> do
        HS.it "writes the value without blocking" $ do
            (val1 :: Int, wv) <- prepare
            val2 <- Q.generate $ Q.arbitrary `Q.suchThat` (/= val1)
            M.void $ WV.putWVar wv val2 `T.shouldNotBlock` 500000
            r <- WV.takeWVar wv `T.shouldNotBlock` 500000
            r `T.shouldBe` val2
    T.whenWVarIsUpdating $ \ prepare -> do
        HS.it "writes the value without blocking" $ do
            (val1 :: Int, wv) <- prepare
            val2 <- Q.generate $ Q.arbitrary `Q.suchThat` (/= val1)
            M.void $ WV.putWVar wv val2 `T.shouldNotBlock` 500000
            r <- WV.takeWVar wv `T.shouldNotBlock` 500000
            r `T.shouldBe` val2

readWVarSpec :: HS.Spec
readWVarSpec = HS.describe "readWVar" $ do
    T.whenWVarIsFresh $ \ prepare -> do
        HS.it "reads the value without blocking" $ do
            (val :: Int, wv) <- prepare
            r <- WV.readWVar wv `T.shouldNotBlock` 500000
            r `T.shouldBe` val
    T.whenWVarIsUpdating $ \ prepare -> do
        HS.it "reads the value without blocking" $ do
            (val :: Int, wv) <- prepare
            r <- WV.readWVar wv `T.shouldNotBlock` 500000
            r `T.shouldBe` val

readFreshWVarSpec :: HS.Spec
readFreshWVarSpec = HS.describe "readFreshWVar" $ do
    T.whenWVarIsFresh $ \ prepare -> do
        HS.it "reads the value without blocking" $ do
            (val :: Int, wv) <- prepare
            r <- WV.readFreshWVar wv `T.shouldNotBlock` 500000
            r `T.shouldBe` val
    T.whenWVarIsUpdating $ \ prepare -> do
        HS.it "blocks until WVar becomes fresh" $ do
            (val1 :: Int, wv) <- prepare
            val2 <- Q.generate $ Q.arbitrary `Q.suchThat` (/= val1)
            wait <- WV.readFreshWVar wv `T.shouldBlock` 500000
            M.void $ WV.putWVar wv val2 `T.shouldNotBlock` 500000
            r <- wait `T.shouldAwakeFinish` 500000
            r `T.shouldBe` val2

tryReadFreshWVarSpec :: HS.Spec
tryReadFreshWVarSpec = HS.describe "readFreshWVar" $ do
    T.whenWVarIsFresh $ \ prepare -> do
        HS.it "reads the value without blocking" $ do
            (val :: Int, wv) <- prepare
            r <- WV.tryReadFreshWVar wv `T.shouldNotBlock` 500000
            r `T.shouldBe` (True, val)
    T.whenWVarIsUpdating $ \ prepare -> do
        HS.it "reads the value without blocking" $ do
            (val :: Int, wv) <- prepare
            r <- WV.tryReadFreshWVar wv `T.shouldNotBlock` 500000
            r `T.shouldBe` (False, val)

takeWCachedSpec :: HS.Spec
takeWCachedSpec = HS.describe "takeWCached" $ do
    T.whenWVarIsFresh    $ T.withLatestCache takeValueWithoutBlocking
    T.whenWVarIsUpdating $ T.withLatestCache blocksUntilWVarBecomeFresh
    T.whenWVarIsFreshButCacheStaled    takeValueWithoutBlocking
    T.whenWVarIsUpdatingAndCacheStaled blocksUntilWVarBecomeFresh
    where
        takeValueWithoutBlocking prepare = do
            HS.it "takes the latest value without blocking" $ do
                (_, val :: Int, _, wc) <- prepare
                wt <- WV.takeWCached wc `T.shouldNotBlock` 500000
                WV.readWTicket wt `T.shouldBe` val
        blocksUntilWVarBecomeFresh prepare = do
            HS.it "blocks until WVar becomes fresh" $ do
                (_, val1 :: Int, wv, wc) <- prepare
                val2 <- Q.generate $ Q.arbitrary `Q.suchThat` (/= val1)
                wait <- WV.takeWCached wc `T.shouldBlock` 500000
                M.void $ WV.putWVar wv val2 `T.shouldNotBlock` 500000
                wt <- wait `T.shouldAwakeFinish` 500000
                WV.readWTicket wt `T.shouldBe` val2

tryTakeWCachedSpec :: HS.Spec
tryTakeWCachedSpec = HS.describe "tryTakeWCached" $ do
    T.whenWVarIsFresh $ \ prepare -> do
        HS.it "takes the value without blocking" $ do
            (val :: Int, wv) <- prepare
            wc <- WV.cacheWVar wv
            (ret, wt) <- WV.tryTakeWCached wc `T.shouldNotBlock` 500000
            ret `T.shouldBe` True
            WV.readWTicket wt `T.shouldBe` val
    T.whenWVarIsUpdating $ T.withLatestCache failToTakeButReadLatest
    T.whenWVarIsFreshButCacheStaled    failToTakeButReadLatest
    T.whenWVarIsUpdatingAndCacheStaled failToTakeButReadLatest
  where
    failToTakeButReadLatest prepare = do
        HS.it "fails but reads the latest value without blocking" $ do
            (_, val :: Int, _, wc) <- prepare
            (ret, wt) <- WV.tryTakeWCached wc `T.shouldNotBlock` 500000
            ret `T.shouldBe` False
            WV.readWTicket wt `T.shouldBe` val

putWCachedSpec :: HS.Spec
putWCachedSpec = HS.describe "putWCached" $ do
    T.whenWVarIsFresh    $ T.withLatestCache writeValueWithoutBlocking
    T.whenWVarIsUpdating $ T.withLatestCache writeValueWithoutBlocking
    T.whenWVarIsFreshButCacheStaled    writeValueWithoutBlocking
    T.whenWVarIsUpdatingAndCacheStaled writeValueWithoutBlocking
  where
    writeValueWithoutBlocking prepare = do
        HS.it "writes the value without blocking" $ do
            (_, val1 :: Int, _, wc) <- prepare
            val2 <- Q.generate $ Q.arbitrary `Q.suchThat` (/= val1)
            wt <- WV.putWCached wc val2 `T.shouldNotBlock` 500000
            WV.readWTicket wt `T.shouldBe` val2

tryPutWCachedSpec :: HS.Spec
tryPutWCachedSpec = HS.describe "tryPutWCached" $ do
    T.whenWVarIsFresh    $ T.withLatestCache writeValueWithoutBlocking
    T.whenWVarIsUpdating $ T.withLatestCache writeValueWithoutBlocking
    T.whenWVarIsFreshButCacheStaled    failToWrite
    T.whenWVarIsUpdatingAndCacheStaled failToWrite
  where
    writeValueWithoutBlocking prepare = do
        HS.it "writes the value without blocking" $ do
            (_, val1 :: Int, _, wc) <- prepare
            val2 <- Q.generate $ Q.arbitrary `Q.suchThat` (/= val1)
            (ret, wt) <- WV.tryPutWCached wc val2 `T.shouldNotBlock` 500000
            ret `T.shouldBe` True
            WV.readWTicket wt `T.shouldBe` val2
    failToWrite prepare = do
        HS.it "fails to write the value without blocking" $ do
            (_, val1 :: Int, _, wc) <- prepare
            val2 <- Q.generate $ Q.arbitrary `Q.suchThat` (/= val1)
            (ret, wt) <- WV.tryPutWCached wc val2 `T.shouldNotBlock` 500000
            ret `T.shouldBe` False
            WV.readWTicket wt `T.shouldBe` val1

readWCachedSpec :: HS.Spec
readWCachedSpec = HS.describe "readWCached" $ do
    T.whenWVarIsFresh    $ T.withLatestCache readLatestValue
    T.whenWVarIsUpdating $ T.withLatestCache readLatestValue
    T.whenWVarIsFreshButCacheStaled    readOldValue
    T.whenWVarIsUpdatingAndCacheStaled readOldValue
  where
    readLatestValue prepare = do
        HS.it "reads the latest value in the ticket" $ do
            (_, val :: Int, _, wc) <- prepare
            WV.readWCached wc `T.shouldBe` val
    readOldValue prepare = do
        HS.it "reads the old value in the ticket" $ do
            (val :: Int, _, _, wc) <- prepare
            WV.readWCached wc `T.shouldBe` val

readFreshWCachedSpec :: HS.Spec
readFreshWCachedSpec = HS.describe "readFreshWCached" $ do
    T.whenWVarIsFresh $ \ prepare -> do
        HS.it "reads the latest value without blocking" $ do
            (val :: Int, wv) <- prepare
            wc <- WV.cacheWVar wv
            wt <- WV.readFreshWCached wc `T.shouldNotBlock` 500000
            WV.readWTicket wt `T.shouldBe` val
    T.whenWVarIsUpdating $ \ prepare -> do
        HS.it "blocks until WVar becomes fresh" $ do
            (val1 :: Int, wv) <- prepare
            val2 <- Q.generate $ Q.arbitrary `Q.suchThat` (/= val1)
            wc <- WV.cacheWVar wv
            wait <- WV.readFreshWCached wc `T.shouldBlock` 500000
            M.void $ WV.putWVar wv val2 `T.shouldNotBlock` 500000
            wt <- wait `T.shouldAwakeFinish` 500000
            WV.readWTicket wt `T.shouldBe` val2
    T.whenWVarIsFreshButCacheStaled    readOldValue
    T.whenWVarIsUpdatingAndCacheStaled readOldValue
  where
    readOldValue prepare = do
        HS.it "reads the old value without blocking" $ do
            (val :: Int, _, _, wc) <- prepare
            wt <- WV.readFreshWCached wc `T.shouldNotBlock` 500000
            WV.readWTicket wt `T.shouldBe` val

tryReadFreshWCachedSpec :: HS.Spec
tryReadFreshWCachedSpec = HS.describe "tryReadFreshWCached" $ do
    T.whenWVarIsFresh $ \ prepare -> do
        HS.it "reads the value without blocking" $ do
            (val :: Int, wv) <- prepare
            wc <- WV.cacheWVar wv
            (ret, wt) <- WV.tryReadFreshWCached wc `T.shouldNotBlock` 500000
            ret `T.shouldBe` True
            WV.readWTicket wt `T.shouldBe` val
    T.whenWVarIsUpdating $ \ prepare -> do
        HS.it "reads the value without blocking" $ do
            (val :: Int, wv) <- prepare
            wc <- WV.cacheWVar wv
            (ret, wt) <- WV.tryReadFreshWCached wc `T.shouldNotBlock` 500000
            ret `T.shouldBe` False
            WV.readWTicket wt `T.shouldBe` val
    T.whenWVarIsFreshButCacheStaled $ \ prepare -> do
        HS.it "reads the old value without blocking" $ do
            (val :: Int, _, _, wc) <- prepare
            (ret, wt) <- WV.tryReadFreshWCached wc `T.shouldNotBlock` 500000
            ret `T.shouldBe` True
            WV.readWTicket wt `T.shouldBe` val
    T.whenWVarIsUpdatingAndCacheStaled $ \ prepare -> do
        HS.it "reads the old value without blocking" $ do
            (val :: Int, _, _, wc) <- prepare
            (ret, wt) <- WV.tryReadFreshWCached wc `T.shouldNotBlock` 500000
            ret `T.shouldBe` True
            WV.readWTicket wt `T.shouldBe` val

wvarSpec :: HS.Spec
wvarSpec = do
    takeWVarSpec
    tryTakeWVarSpec
    putWVarSpec
    readWVarSpec
    readFreshWVarSpec
    tryReadFreshWVarSpec

wcachedSpec :: HS.Spec
wcachedSpec = do
    takeWCachedSpec
    tryTakeWCachedSpec
    putWCachedSpec
    tryPutWCachedSpec
    readWCachedSpec
    readFreshWCachedSpec
    tryReadFreshWCachedSpec

spec :: HS.Spec
spec = HS.describe "WVar basic specs" $ do
    HS.describe "WVar"    wvarSpec
    HS.describe "WCached" wcachedSpec

