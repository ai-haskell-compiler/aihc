{-# LANGUAGE ScopedTypeVariables #-}

module WVarConcurrentSpec where

import qualified Test.Concurrent   as T
import qualified Test.Expectations as T
import qualified Test.Util         as T
import qualified Test.WVar         as T

import qualified Test.Hspec as HS

import qualified Control.Concurrent.WVar as WV
import qualified Control.Monad           as M

import qualified Data.List as L

takeWVarSeqSpec :: HS.Spec
takeWVarSeqSpec = HS.describe "takeWVar" $ do
    T.whenWVarIsFresh $ \ prepare -> do
        T.ioprop "takes the value before or after putWVar" $ do
            (val1, wv) <- prepare
            [val2] <- T.genSatisfy 1 (/= val1)
            M.void $ WV.putWVar wv val2 `T.concurrently` do
                ret <- WV.takeWVar wv `T.shouldNotBlock` 500000
                T.oneOf
                    [ ret `T.shouldBe` val1
                    , ret `T.shouldBe` val2
                    ]
        T.ioprop "take different value" $ do
            (val1, wv) <- prepare
            [val2, val3, val4] <- T.genSatisfy 3 (/= val1)
            ret <- T.mapConcurrently
                [ WV.takeWVar wv <* WV.putWVar wv val2
                , WV.takeWVar wv <* WV.putWVar wv val3
                , WV.takeWVar wv <* WV.putWVar wv val4
                ]
            T.oneOf
                [ ret `T.shouldBe` [val1,val2,val3]
                , ret `T.shouldBe` [val1,val4,val2]
                , ret `T.shouldBe` [val3,val1,val2]
                , ret `T.shouldBe` [val4,val1,val3]
                , ret `T.shouldBe` [val3,val4,val1]
                , ret `T.shouldBe` [val4,val2,val1]
                ]
    T.whenWVarIsUpdating $ \ prepare -> do
        T.ioprop "takes the value after putWVar" $ do
            (val1, wv) <- prepare
            [val2] <- T.genSatisfy 1 (/= val1)
            M.void $ WV.putWVar wv val2 `T.concurrently` do
                ret <- WV.takeWVar wv
                ret `T.shouldBe` val2
        T.ioprop "take different value" $ do
            (val1 :: Int, wv) <- prepare
            [val2, val3, val4, val5] <- T.genSatisfy 4 (/= val1)
            ret <- T.mapConcurrently
                [ WV.putWVar wv val2 >> return val2
                , WV.takeWVar wv <* WV.putWVar wv val3
                , WV.takeWVar wv <* WV.putWVar wv val4
                , WV.takeWVar wv <* WV.putWVar wv val5
                ]
            T.oneOf
                [ ret `T.shouldBe` [val2,val2,val3,val4]
                , ret `T.shouldBe` [val2,val2,val5,val3]
                , ret `T.shouldBe` [val2,val4,val2,val3]
                , ret `T.shouldBe` [val2,val5,val2,val4]
                , ret `T.shouldBe` [val2,val4,val5,val2]
                , ret `T.shouldBe` [val2,val5,val3,val2]
                ]

tryTakeWVarSeqSpec :: HS.Spec
tryTakeWVarSeqSpec = HS.describe "tryTakeWVar" $ do
    T.whenWVarIsFresh $ \ prepare -> do
        T.ioprop "takes the value before or after putWVar" $ do
            (val1 :: Int, wv) <- prepare
            [val2] <- T.genSatisfy 1 (/= val1)
            M.void $ WV.putWVar wv val2 `T.concurrently` do
                ret <- WV.tryTakeWVar wv `T.shouldNotBlock` 500000
                T.oneOf
                    [ ret `T.shouldBe` (True, val1)
                    , ret `T.shouldBe` (True, val2)
                    ]
        T.ioprop "all read same value but only one succeeded" $ do
            (val :: Int, wv) <- prepare
            ret <- T.mapConcurrently
                [ WV.tryTakeWVar wv
                , WV.tryTakeWVar wv
                , WV.tryTakeWVar wv
                ]
            T.oneOf
                [ ret `T.shouldBe` [(True,val),(False,val),(False,val)]
                , ret `T.shouldBe` [(False,val),(True,val),(False,val)]
                , ret `T.shouldBe` [(False,val),(False,val),(True,val)]
                ]
    T.whenWVarIsUpdating $ \ prepare -> do
        T.ioprop "takes the value before(failure) or after(success) putWVar" $ do
            (val1 :: Int, wv) <- prepare
            [val2] <- T.genSatisfy 1 (/= val1)
            M.void $ WV.putWVar wv val2 `T.concurrently` do
                ret <- WV.tryTakeWVar wv `T.shouldNotBlock` 500000
                T.oneOf
                    [ ret `T.shouldBe` (False, val1)
                    , ret `T.shouldBe` (True,  val2)
                    ]
        T.ioprop "all read same value and fail" $ do
            (val :: Int, wv) <- prepare
            ret <- T.mapConcurrently
                [ WV.tryTakeWVar wv
                , WV.tryTakeWVar wv
                , WV.tryTakeWVar wv
                ]
            ret `T.shouldBe` [(False,val),(False,val),(False,val)]

readFreshWVarSeqSpec :: HS.Spec
readFreshWVarSeqSpec = HS.describe "readFreshWVar" $ do
    T.whenWVarIsFresh $ \ prepare -> do
        T.ioprop "reads the value before or after putWVar" $ do
            (val1 :: Int, wv) <- prepare
            [val2] <- T.genSatisfy 1 (/= val1)
            M.void $ WV.putWVar wv val2 `T.concurrently` do
                ret <- WV.readFreshWVar wv `T.shouldNotBlock` 500000
                T.oneOf
                    [ ret `T.shouldBe` val1
                    , ret `T.shouldBe` val2
                    ]
        T.ioprop "read same value" $ do
            (val :: Int, wv) <- prepare
            ret <- T.mapConcurrently
                [ WV.readFreshWVar wv
                , WV.readFreshWVar wv
                , WV.readFreshWVar wv
                ]
            ret `T.shouldBe` [val,val,val]
    T.whenWVarIsUpdating $ \ prepare -> do
        T.ioprop "reads the value after putWVar" $ do
            (val1 :: Int, wv) <- prepare
            [val2] <- T.genSatisfy 1 (/= val1)
            M.void $ WV.putWVar wv val2 `T.concurrently` do
                ret <- WV.readFreshWVar wv
                ret `T.shouldBe` val2
        T.ioprop "read same value" $ do
            (val1 :: Int, wv) <- prepare
            [val2] <- T.genSatisfy 1 (/= val1)
            ret <- T.mapConcurrently
                [ WV.putWVar wv val2 >> return val2
                , WV.readFreshWVar wv
                , WV.readFreshWVar wv
                , WV.readFreshWVar wv
                ]
            ret `T.shouldBe` [val2,val2,val2,val2]

tryReadFreshWVarSeqSpec :: HS.Spec
tryReadFreshWVarSeqSpec = HS.describe "tryReadWVar" $ do
    T.whenWVarIsFresh $ \ prepare -> do
        T.ioprop "reads the old value" $ do
            (val1 :: Int, wv) <- prepare
            [val2] <- T.genSatisfy 1 (/= val1)
            M.void $ WV.putWVar wv val2 `T.concurrently` do
                ret <- WV.tryReadFreshWVar wv `T.shouldNotBlock` 500000
                T.oneOf
                    [ ret `T.shouldBe` (True, val1)
                    , ret `T.shouldBe` (True, val2)
                    ]
        T.ioprop "all read same value and succeed" $ do
            (val :: Int, wv) <- prepare
            ret <- T.mapConcurrently
                [ WV.tryReadFreshWVar wv
                , WV.tryReadFreshWVar wv
                , WV.tryReadFreshWVar wv
                ]
            ret `T.shouldBe` [(True,val),(True,val),(True,val)]
    T.whenWVarIsUpdating $ \ prepare -> do
        T.ioprop "reads the value before(failure) or after(success) putWVar" $ do
            (val1 :: Int, wv) <- prepare
            [val2] <- T.genSatisfy 1 (/= val1)
            M.void $ WV.putWVar wv val2 `T.concurrently` do
                ret <- WV.tryReadFreshWVar wv `T.shouldNotBlock` 500000
                T.oneOf
                    [ ret `T.shouldBe` (False, val1)
                    , ret `T.shouldBe` (True,  val2)
                    ]
        T.ioprop "all read same value and fail" $ do
            (val :: Int, wv) <- prepare
            ret <- T.mapConcurrently
                [ WV.tryReadFreshWVar wv
                , WV.tryReadFreshWVar wv
                , WV.tryReadFreshWVar wv
                ]
            ret `T.shouldBe` [(False,val),(False,val),(False,val)]

takeWCachedSeqSpec :: HS.Spec
takeWCachedSeqSpec = HS.describe "takeWCached" $ do
    T.whenWVarIsFresh . T.withLatestCache $ \ prepare -> do
        T.ioprop "takes the value before or after putWCached" $ do
            (_, val1 :: Int, _, wc) <- prepare
            [val2] <- T.genSatisfy 1 (/= val1)
            M.void $ WV.putWCached wc val2 `T.concurrently` do
                wt <- WV.takeWCached wc `T.shouldNotBlock` 500000
                T.oneOf
                    [ WV.readWTicket wt `T.shouldBe` val1
                    , WV.readWTicket wt `T.shouldBe` val2
                    ]
        T.ioprop "take different value" $ do
            (_, val1 :: Int, _, wc) <- prepare
            [val2, val3, val4] <- T.genSatisfy 3 (/= val1)
            ret <- fmap WV.readWTicket <$> T.mapConcurrently
                [ WV.takeWCached wc <* WV.putWCached wc val2
                , WV.takeWCached wc <* WV.putWCached wc val3
                , WV.takeWCached wc <* WV.putWCached wc val4
                ]
            T.oneOf
                [ ret `T.shouldBe` [val1,val2,val3]
                , ret `T.shouldBe` [val1,val4,val2]
                , ret `T.shouldBe` [val3,val1,val2]
                , ret `T.shouldBe` [val4,val1,val3]
                , ret `T.shouldBe` [val3,val4,val1]
                , ret `T.shouldBe` [val4,val2,val1]
                ]
    T.whenWVarIsUpdating . T.withLatestCache $ \ prepare -> do
        T.ioprop "takes the value after putWCached" $ do
            (_, val1 :: Int, _, wc) <- prepare
            [val2] <- T.genSatisfy 1 (/= val1)
            M.void $ WV.putWCached wc val2 `T.concurrently` do
                ret <- WV.takeWCached wc
                WV.readWTicket ret `T.shouldBe` val2
        T.ioprop "take different value" $ do
            (_, val1 :: Int, _, wc) <- prepare
            [val2, val3, val4, val5] <- T.genSatisfy 4 (/= val1)
            ret <- T.mapConcurrently
                [ WV.putWCached wc val2 >> return val2
                , WV.readWTicket <$> WV.takeWCached wc <* WV.putWCached wc val3
                , WV.readWTicket <$> WV.takeWCached wc <* WV.putWCached wc val4
                , WV.readWTicket <$> WV.takeWCached wc <* WV.putWCached wc val5
                ]
            T.oneOf
                [ ret `T.shouldBe` [val2,val2,val3,val4]
                , ret `T.shouldBe` [val2,val2,val5,val3]
                , ret `T.shouldBe` [val2,val4,val2,val3]
                , ret `T.shouldBe` [val2,val5,val2,val4]
                , ret `T.shouldBe` [val2,val4,val5,val2]
                , ret `T.shouldBe` [val2,val5,val3,val2]
                ]
    T.whenWVarIsFreshButCacheStaled $ \ prepare -> do
        T.ioprop "takes the value before or after putWCached" $ do
            (_, val1 :: Int, _, wc) <- prepare
            [val2] <- T.genSatisfy 1 (/= val1)
            M.void $ WV.putWCached wc val2 `T.concurrently` do
                wt <- WV.takeWCached wc `T.shouldNotBlock` 500000
                T.oneOf
                    [ WV.readWTicket wt `T.shouldBe` val1
                    , WV.readWTicket wt `T.shouldBe` val2
                    ]
        T.ioprop "take different value" $ do
            (_, val1 :: Int, _, wc) <- prepare
            [val2, val3, val4] <- T.genSatisfy 3 (/= val1)
            ret <- fmap WV.readWTicket <$> T.mapConcurrently
                [ WV.takeWCached wc <* WV.putWCached wc val2
                , WV.takeWCached wc <* WV.putWCached wc val3
                , WV.takeWCached wc <* WV.putWCached wc val4
                ]
            T.oneOf
                [ ret `T.shouldBe` [val1,val2,val3]
                , ret `T.shouldBe` [val1,val4,val2]
                , ret `T.shouldBe` [val3,val1,val2]
                , ret `T.shouldBe` [val4,val1,val3]
                , ret `T.shouldBe` [val3,val4,val1]
                , ret `T.shouldBe` [val4,val2,val1]
                ]
    T.whenWVarIsUpdatingAndCacheStaled $ \ prepare -> do
        T.ioprop "takes the value after putWCached" $ do
            (_, val1 :: Int, _, wc) <- prepare
            [val2] <- T.genSatisfy 1 (/= val1)
            M.void $ WV.putWCached wc val2 `T.concurrently` do
                ret <- WV.takeWCached wc
                WV.readWTicket ret `T.shouldBe` val2
        T.ioprop "take different value" $ do
            (_, val1 :: Int, _, wc) <- prepare
            [val2, val3, val4, val5] <- T.genSatisfy 4 (/= val1)
            ret <- T.mapConcurrently
                [ WV.putWCached wc val2 >> return val2
                , WV.readWTicket <$> WV.takeWCached wc <* WV.putWCached wc val3
                , WV.readWTicket <$> WV.takeWCached wc <* WV.putWCached wc val4
                , WV.readWTicket <$> WV.takeWCached wc <* WV.putWCached wc val5
                ]
            T.oneOf
                [ ret `T.shouldBe` [val2,val2,val3,val4]
                , ret `T.shouldBe` [val2,val2,val5,val3]
                , ret `T.shouldBe` [val2,val4,val2,val3]
                , ret `T.shouldBe` [val2,val5,val2,val4]
                , ret `T.shouldBe` [val2,val4,val5,val2]
                , ret `T.shouldBe` [val2,val5,val3,val2]
                ]

tryTakeWCachedSeqSpec :: HS.Spec
tryTakeWCachedSeqSpec = HS.describe "tryTakeWCached" $ do
    T.whenWVarIsFresh . T.withLatestCache $ \ prepare -> do
        T.ioprop "takes the value before(failure)/after(success) putWCached" $ do
            (_, val1 :: Int, _, wc) <- prepare
            [val2] <- T.genSatisfy 1 (/= val1)
            M.void $ WV.putWCached wc val2 `T.concurrently` do
                (ret, wt) <- WV.tryTakeWCached wc `T.shouldNotBlock` 500000
                T.oneOf
                    [ (ret, WV.readWTicket wt) `T.shouldBe` (True, val1)
                    , (ret, WV.readWTicket wt) `T.shouldBe` (False, val2)
                    ]
        T.ioprop "all read same value but only one succeeded" $ do
            (_, val1 :: Int, _, wc) <- prepare
            ret <- fmap (fmap WV.readWTicket) <$> T.mapConcurrently
                [ WV.tryTakeWCached wc
                , WV.tryTakeWCached wc
                , WV.tryTakeWCached wc
                ]
            T.oneOf
                [ ret `T.shouldBe` [(True,val1),(False,val1),(False,val1)]
                , ret `T.shouldBe` [(False,val1),(True,val1),(False,val1)]
                , ret `T.shouldBe` [(False,val1),(False,val1),(True,val1)]
                ]
    T.whenWVarIsUpdating . T.withLatestCache $ \ prepare -> do
        T.ioprop "takes the value before or after putWCached with failure" $ do
            (_, val1 :: Int, _, wc) <- prepare
            [val2] <- T.genSatisfy 1 (/= val1)
            M.void $ WV.putWCached wc val2 `T.concurrently` do
                (ret, wt) <- WV.tryTakeWCached wc `T.shouldNotBlock` 500000
                T.oneOf
                    [ (ret, WV.readWTicket wt) `T.shouldBe` (False, val1)
                    , (ret, WV.readWTicket wt) `T.shouldBe` (False, val2)
                    ]
        T.ioprop "all read same value and fail" $ do
            (_, val1 :: Int, _, wc) <- prepare
            ret <- fmap (fmap WV.readWTicket) <$> T.mapConcurrently
                [ WV.tryTakeWCached wc
                , WV.tryTakeWCached wc
                , WV.tryTakeWCached wc
                ]
            ret `T.shouldBe` [(False,val1),(False,val1),(False,val1)]
    T.whenWVarIsFreshButCacheStaled $ \ prepare -> do
        T.ioprop "takes the value before or after putWCached with failure" $ do
            (_, val1 :: Int, _, wc) <- prepare
            [val2] <- T.genSatisfy 1 (/= val1)
            M.void $ WV.putWCached wc val2 `T.concurrently` do
                (ret, wt) <- WV.tryTakeWCached wc `T.shouldNotBlock` 500000
                T.oneOf
                    [ (ret, WV.readWTicket wt) `T.shouldBe` (False, val1)
                    , (ret, WV.readWTicket wt) `T.shouldBe` (False, val2)
                    ]
        T.ioprop "all read same value and fail" $ do
            (_, val1 :: Int, _, wc) <- prepare
            ret <- fmap (fmap WV.readWTicket) <$> T.mapConcurrently
                [ WV.tryTakeWCached wc
                , WV.tryTakeWCached wc
                , WV.tryTakeWCached wc
                ]
            ret `T.shouldBe` [(False,val1),(False,val1),(False,val1)]
    T.whenWVarIsUpdatingAndCacheStaled $ \ prepare -> do
        T.ioprop "takes the value before or after putWCached with failure" $ do
            (_, val1 :: Int, _, wc) <- prepare
            [val2] <- T.genSatisfy 1 (/= val1)
            M.void $ WV.putWCached wc val2 `T.concurrently` do
                (ret, wt) <- WV.tryTakeWCached wc `T.shouldNotBlock` 500000
                T.oneOf
                    [ (ret, WV.readWTicket wt) `T.shouldBe` (False, val1)
                    , (ret, WV.readWTicket wt) `T.shouldBe` (False, val2)
                    ]
        T.ioprop "all read same value and fail" $ do
            (_, val1 :: Int, _, wc) <- prepare
            ret <- fmap (fmap WV.readWTicket) <$> T.mapConcurrently
                [ WV.tryTakeWCached wc
                , WV.tryTakeWCached wc
                , WV.tryTakeWCached wc
                ]
            ret `T.shouldBe` [(False,val1),(False,val1),(False,val1)]

readFreshWCachedSeqSpec :: HS.Spec
readFreshWCachedSeqSpec = HS.describe "readFreshWCached" $ do
    T.whenWVarIsFresh . T.withLatestCache $ \ prepare -> do
        T.ioprop "reads the value before or after putWCached" $ do
            (val1 :: Int, _, _, wc) <- prepare
            [val2] <- T.genSatisfy 1 (/= val1)
            M.void $ WV.putWCached wc val2 `T.concurrently` do
                wt <- WV.readFreshWCached wc `T.shouldNotBlock` 500000
                WV.readWTicket wt `T.shouldBe` val1
        T.ioprop "read same value" $ do
            (val1 :: Int, _, _, wc) <- prepare
            ret <- fmap WV.readWTicket <$> T.mapConcurrently
                [ WV.readFreshWCached wc
                , WV.readFreshWCached wc
                , WV.readFreshWCached wc
                ]
            ret `T.shouldBe` [val1,val1,val1]
    T.whenWVarIsUpdating . T.withLatestCache $ \ prepare -> do
        T.ioprop "reads the value after putWCached" $ do
            (val1 :: Int, _, _, wc) <- prepare
            [val2] <- T.genSatisfy 1 (/= val1)
            M.void $ WV.putWCached wc val2 `T.concurrently` do
                wt <- WV.readFreshWCached wc
                WV.readWTicket wt `T.shouldBe` val2
        T.ioprop "read same value" $ do
            (val1 :: Int, _, _, wc) <- prepare
            [val2] <- T.genSatisfy 1 (/= val1)
            ret <- T.mapConcurrently
                [ WV.putWCached wc val2 >> return val2
                , WV.readWTicket <$> WV.readFreshWCached wc
                , WV.readWTicket <$> WV.readFreshWCached wc
                , WV.readWTicket <$> WV.readFreshWCached wc
                ]
            ret `T.shouldBe` [val2,val2,val2,val2]
    T.whenWVarIsFreshButCacheStaled $ \ prepare -> do
        T.ioprop "reads the value before or after putWCached" $ do
            (val1 :: Int, _, _, wc) <- prepare
            [val2] <- T.genSatisfy 1 (/= val1)
            M.void $ WV.putWCached wc val2 `T.concurrently` do
                wt <- WV.readFreshWCached wc `T.shouldNotBlock` 500000
                WV.readWTicket wt `T.shouldBe` val1
        T.ioprop "read same value" $ do
            (val1 :: Int, _, _, wc) <- prepare
            ret <- fmap WV.readWTicket <$> T.mapConcurrently
                [ WV.readFreshWCached wc
                , WV.readFreshWCached wc
                , WV.readFreshWCached wc
                ]
            ret `T.shouldBe` [val1,val1,val1]
    T.whenWVarIsUpdatingAndCacheStaled $ \ prepare -> do
        T.ioprop "reads the value after putWCached" $ do
            (val1 :: Int, _, _, wc) <- prepare
            [val2] <- T.genSatisfy 1 (/= val1)
            M.void $ WV.putWCached wc val2 `T.concurrently` do
                ret <- WV.readWTicket <$> WV.readFreshWCached wc
                ret `T.shouldBe` val1
        T.ioprop "read same value" $ do
            (val1 :: Int, _, _, wc) <- prepare
            [val2] <- T.genSatisfy 1 (/= val1)
            ret <- T.mapConcurrently
                [ WV.putWCached wc val2 >> return val2
                , WV.readWTicket <$> WV.readFreshWCached wc
                , WV.readWTicket <$> WV.readFreshWCached wc
                , WV.readWTicket <$> WV.readFreshWCached wc
                ]
            ret `T.shouldBe` [val2,val1,val1,val1]

tryReadFreshWCachedSeqSpec :: HS.Spec
tryReadFreshWCachedSeqSpec = HS.describe "tryReadWCached" $ do
    T.whenWVarIsFresh . T.withLatestCache $ \ prepare -> do
        T.ioprop "reads the old value and succeed" $ do
            (val1 :: Int, _, _, wc) <- prepare
            [val2] <- T.genSatisfy 1 (/= val1)
            M.void $ WV.putWCached wc val2 `T.concurrently` do
                ret <- WV.tryReadFreshWCached wc `T.shouldNotBlock` 500000
                fmap WV.readWTicket ret `T.shouldBe` (True, val1)
        T.ioprop "all read same value and succeed" $ do
            (val1 :: Int, _, _, wc) <- prepare
            ret <- fmap (fmap WV.readWTicket) <$> T.mapConcurrently
                [ WV.tryReadFreshWCached wc
                , WV.tryReadFreshWCached wc
                , WV.tryReadFreshWCached wc
                ]
            ret `T.shouldBe` [(True,val1),(True,val1),(True,val1)]
    T.whenWVarIsUpdating . T.withLatestCache $ \ prepare -> do
        T.ioprop "reads the old value and fail" $ do
            (val1 :: Int, _, _, wc) <- prepare
            [val2] <- T.genSatisfy 1 (/= val1)
            M.void $ WV.putWCached wc val2 `T.concurrently` do
                ret <- WV.tryReadFreshWCached wc `T.shouldNotBlock` 500000
                fmap WV.readWTicket ret `T.shouldBe` (False, val1)
        T.ioprop "all read same value and fail" $ do
            (val1 :: Int, _, _, wc) <- prepare
            ret <- fmap (fmap WV.readWTicket) <$> T.mapConcurrently
                [ WV.tryReadFreshWCached wc
                , WV.tryReadFreshWCached wc
                , WV.tryReadFreshWCached wc
                ]
            ret `T.shouldBe` [(False,val1),(False,val1),(False,val1)]
    T.whenWVarIsFreshButCacheStaled $ \ prepare -> do
        T.ioprop "reads the old value and succeed" $ do
            (val1 :: Int, _, _, wc) <- prepare
            [val2] <- T.genSatisfy 1 (/= val1)
            M.void $ WV.putWCached wc val2 `T.concurrently` do
                ret <- WV.tryReadFreshWCached wc `T.shouldNotBlock` 500000
                fmap WV.readWTicket ret `T.shouldBe` (True, val1)
        T.ioprop "all read same value and succeed" $ do
            (val1 :: Int, _, _, wc) <- prepare
            ret <- fmap (fmap WV.readWTicket) <$> T.mapConcurrently
                [ WV.tryReadFreshWCached wc
                , WV.tryReadFreshWCached wc
                , WV.tryReadFreshWCached wc
                ]
            ret `T.shouldBe` [(True,val1),(True,val1),(True,val1)]
    T.whenWVarIsUpdating . T.withLatestCache $ \ prepare -> do
        T.ioprop "reads the old value and fail" $ do
            (val1 :: Int, _, _, wc) <- prepare
            [val2] <- T.genSatisfy 1 (/= val1)
            M.void $ WV.putWCached wc val2 `T.concurrently` do
                ret <- WV.tryReadFreshWCached wc `T.shouldNotBlock` 500000
                fmap WV.readWTicket ret `T.shouldBe` (False, val1)
        T.ioprop "all read same value and fail" $ do
            (val1 :: Int, _, _, wc) <- prepare
            ret <- fmap (fmap WV.readWTicket) <$> T.mapConcurrently
                [ WV.tryReadFreshWCached wc
                , WV.tryReadFreshWCached wc
                , WV.tryReadFreshWCached wc
                ]
            ret `T.shouldBe` [(False,val1),(False,val1),(False,val1)]

tryTakeAndPutCachedSeqSpec :: HS.Spec
tryTakeAndPutCachedSeqSpec =
    T.ioprop "tryTakeWCached and putWCached perform atomic modification" $ do
        wv <- WV.newWVar (0 :: Int)
        wc <- WV.cacheWVar wv
        ret <- L.sort . L.concat <$> T.mapConcurrently (countConc10 wc)
        ret `T.shouldBe` [1..1000]
  where
    countConc10 wc = L.replicate 10 $ count100 wc
    count100 wc = M.replicateM 100 $ countOne wc
    countOne wc = do
        (suc, wt1) <- WV.tryTakeWCached wc
        let val = WV.readWTicket wt1
        if suc
            then do
                wt2 <- WV.putWCached wc $ val + 1
                return $ WV.readWTicket wt2
            else do
                wt2 <- WV.readFreshWCached wc { WV.cachedTicket = wt1 }
                countOne wc { WV.cachedTicket = wt2 }

wvarSpec :: HS.Spec
wvarSpec = do
    takeWVarSeqSpec
    tryTakeWVarSeqSpec
    readFreshWVarSeqSpec
    tryReadFreshWVarSeqSpec

wcachedSpec :: HS.Spec
wcachedSpec = do
    takeWCachedSeqSpec
    tryTakeWCachedSeqSpec
    readFreshWCachedSeqSpec
    tryReadFreshWCachedSeqSpec

spec :: HS.Spec
spec = HS.describe "WVar concurrent specs" $ do
    HS.describe "WVar"    wvarSpec
    HS.describe "WCached" wcachedSpec
    HS.describe "combination" tryTakeAndPutCachedSeqSpec



