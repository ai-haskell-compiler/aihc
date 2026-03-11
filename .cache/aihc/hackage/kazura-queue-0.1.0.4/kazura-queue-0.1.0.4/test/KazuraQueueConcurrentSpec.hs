{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module KazuraQueueConcurrentSpec where

import qualified Test.Concurrent   as T
import qualified Test.Expectations as T
import qualified Test.KazuraQueue  as T
import qualified Test.Util         as T

import qualified Test.Hspec      as HS
import qualified Test.QuickCheck as Q

import qualified Control.Concurrent             as CC
import qualified Control.Concurrent.Async       as AS
import qualified Control.Concurrent.KazuraQueue as KQ
import qualified Control.Exception              as E
import qualified Control.Monad                  as M

import qualified Data.Foldable    as TF
import qualified Data.IORef       as Ref
import qualified Data.List        as L
import qualified Data.Map.Strict  as Map
import           Data.Monoid      ((<>))
import qualified Data.Set         as Set
import qualified Data.Traversable as TF

writeQueueSpec :: HS.Spec
writeQueueSpec = HS.describe "writeQueue" $ do
    T.whenItemsInQueue (1,10) $ \ prepare -> do
        T.ioprop "write and read values concurrently" . prepare $ \ (q, pre) -> do
            (val1 :: Int, val2, val3) <- Q.generate Q.arbitrary
            T.mapConcurrently_
                [ KQ.writeQueue q val1 `T.shouldNotBlock` 500000
                , KQ.writeQueue q val2 `T.shouldNotBlock` 500000
                , KQ.writeQueue q val3 `T.shouldNotBlock` 500000
                ]
            let len0 = length pre
            q `T.queueLengthShouldBeIn` (len0, 3+len0)
            ret1 <- M.replicateM len0 $
                KQ.readQueue q `T.shouldNotBlock` 500000
            ret2 <- M.replicateM 3 $
                KQ.readQueue q `T.shouldNotBlock` 500000
            let ret = ret1 ++ ret2
            T.oneOf
                [ ret `T.shouldBe` (pre ++ [val1, val2, val3])
                , ret `T.shouldBe` (pre ++ [val1, val3, val2])
                , ret `T.shouldBe` (pre ++ [val2, val1, val3])
                , ret `T.shouldBe` (pre ++ [val2, val3, val1])
                , ret `T.shouldBe` (pre ++ [val3, val1, val2])
                , ret `T.shouldBe` (pre ++ [val3, val2, val1])
                ]

readQueueSpec :: HS.Spec
readQueueSpec = HS.describe "readQueue" $ do
    T.whenQueueIsEmpty $ \ prepare -> do
        T.ioprop "values are read in order (thread awakes out of order)" . prepare $ \ q -> do
            waits0 <- M.replicateM 3 $ KQ.readQueue q `T.shouldBlock` 500000
            q `T.queueLengthShouldBeIn` (-3, 0)
            (val1 :: Int, val2, val3) <- Q.generate Q.arbitrary

            KQ.writeQueue q val1 `T.shouldNotBlock` 500000
            (r1, waits1) <- waits0 `T.onlyOneShouldAwakeFinish` 500000
            q `T.queueLengthShouldBeIn` (-3, 0)
            KQ.writeQueue q val2 `T.shouldNotBlock` 500000
            (r2, waits2) <- waits1 `T.onlyOneShouldAwakeFinish` 500000
            q `T.queueLengthShouldBeIn` (-3, 0)
            KQ.writeQueue q val3 `T.shouldNotBlock` 500000
            (r3, _)      <- waits2 `T.onlyOneShouldAwakeFinish` 500000
            q `T.queueLengthShouldBeIn` (-3, 0)
            (r1, r2, r3) `T.shouldBe` (val1, val2, val3)

tryReadQueueSpec :: HS.Spec
tryReadQueueSpec = HS.describe "tryReadQueue" $ do
    T.whenItemsInQueue (1,10) $ \ prepare -> do
        T.ioprop "values are read in order" . prepare $ \ (q, pre :: [Int]) -> do
            mrets <- M.replicateM 10 $ KQ.tryReadQueue q `T.shouldNotBlock` 1000000
            q `T.queueLengthShouldBeIn` (-10, 0)
            let nothings = L.replicate (10 - length pre) Nothing
                expected = (Just <$> pre) <> nothings
            mrets `T.shouldBe` expected
    T.whenQueueIsEmpty $ \ prepare -> do
        T.ioprop "read value after writing" . prepare $ \ q -> do
            (val1 :: Int, val2, val3, val4) <- Q.generate Q.arbitrary
            KQ.writeQueue q val1 `T.shouldNotBlock` 1000000
            mret1 <- KQ.tryReadQueue q `T.shouldNotBlock` 1000000
            mret1 `T.shouldBe` Just val1
            KQ.writeQueue q val2 `T.shouldNotBlock` 1000000
            mret2 <- KQ.tryReadQueue q `T.shouldNotBlock` 1000000
            mret2 `T.shouldBe` Just val2
            KQ.writeQueue q val3 `T.shouldNotBlock` 1000000
            mret3 <- KQ.tryReadQueue q `T.shouldNotBlock` 1000000
            mret3 `T.shouldBe` Just val3
            KQ.writeQueue q val4 `T.shouldNotBlock` 1000000
            mret4 <- KQ.tryReadQueue q `T.shouldNotBlock` 1000000
            mret4 `T.shouldBe` Just val4

readWriteQueueSpec :: HS.Spec
readWriteQueueSpec = HS.describe "readWriteQueueSpec" $ do
    T.whenQueueIsEmpty $ \ prepare -> do
        T.ioprop "read/write = 1/1" . prepare $ \ q -> do
            test (1,10000) (1,10000) q
        T.ioprop "read/write = 1/10" . prepare $ \ q -> do
            test (1,10000) (10,1000) q
        T.ioprop "read/write = 10/1" . prepare $ \ q -> do
            test (10,1000) (1,10000) q
        T.ioprop "read/write = 10/10" . prepare $ \ q -> do
            test (10,1000) (10,1000) q
  where
    test :: (Int,Int) -> (Int,Int) -> KQ.Queue (Int,Int) -> IO ()
    test readConfig writeConfig q = do
        (results, writtens) <-
            readConcurrent q readConfig
                `T.concurrently` writeConcurrent q writeConfig
        case checkEachResult results of
            Right _  -> return ()
            Left str -> T.assertFailure str
        let result  = L.concat results
            written = L.concat writtens
            resultSet  = Set.fromList result
            writtenSet = Set.fromList written
        length result `T.shouldBe` length written
        (writtenSet `Set.difference` resultSet) `T.shouldBe` Set.empty
    checkEachResult = TF.traverse checkEachItems
    checkEachItems  = L.foldl' checkItems $ Right Map.empty
    checkItems (Right mp) (thnum, num)
        | Map.lookup thnum mp < Just num = Right $ Map.insert thnum num mp
        | Map.lookup thnum mp > Just num = Left "broken order"
        | otherwise                      = Left "duplicated value"
    checkItems err _ = err
    readItems  q size  = M.replicateM size $ KQ.readQueue q
    writeItems q items = do
        TF.for_ items $ KQ.writeQueue q
        return items
    readConcurrent  q (thsize, itemsize) = do
        ass <- M.replicateM thsize . AS.async $ readItems q itemsize
        TF.for ass AS.wait
    writeConcurrent q (thsize, itemsize) = do
        ass <- TF.for [1..thsize] $ \ thnum ->
            AS.async . writeItems q $ fmap (thnum,) [1..itemsize]
        TF.for ass AS.wait

tryReadWriteQueueSpec :: HS.Spec
tryReadWriteQueueSpec = HS.describe "tryReadWriteQueueSpec" $ do
    T.whenQueueIsEmpty $ \ prepare -> do
        T.ioprop "read/write = 1/1" . prepare $ \ q -> do
            test (1,10000) (1,10000) q
        T.ioprop "read/write = 1/10" . prepare $ \ q -> do
            test (1,10000) (10,1000) q
        T.ioprop "read/write = 10/1" . prepare $ \ q -> do
            test (10,1000) (1,10000) q
        T.ioprop "read/write = 10/10" . prepare $ \ q -> do
            test (10,1000) (10,1000) q
  where
    test :: (Int,Int) -> (Int,Int) -> KQ.Queue (Int,Int) -> IO ()
    test readConfig writeConfig q = do
        (results, writtens) <-
            readConcurrent q readConfig
                `T.concurrently` writeConcurrent q writeConfig
        case checkEachResult results of
            Right _  -> return ()
            Left str -> T.assertFailure str
        let result  = L.concat results
            written = L.concat writtens
            resultSet  = Set.fromList result
            writtenSet = Set.fromList written
        length result `T.shouldBe` length written
        (writtenSet `Set.difference` resultSet) `T.shouldBe` Set.empty
    checkEachResult = TF.traverse checkEachItems
    checkEachItems  = L.foldl' checkItems $ Right Map.empty
    checkItems (Right mp) (thnum, num)
        | Map.lookup thnum mp < Just num = Right $ Map.insert thnum num mp
        | Map.lookup thnum mp > Just num = Left "broken order"
        | otherwise                      = Left "duplicated value"
    checkItems err _ = err
    readItem q = do
        mret <- KQ.tryReadQueue q
        case mret of
            Just ret -> return ret
            Nothing  -> do
                CC.yield
                readItem q
    readItems  q size  = M.replicateM size $ readItem q
    writeItems q items = do
        TF.for_ items $ KQ.writeQueue q
        return items
    readConcurrent  q (thsize, itemsize) = do
        ass <- M.replicateM thsize . AS.async $ readItems q itemsize
        TF.for ass AS.wait
    writeConcurrent q (thsize, itemsize) = do
        ass <- TF.for [1..thsize] $ \ thnum ->
            AS.async . writeItems q $ fmap (thnum,) [1..itemsize]
        TF.for ass AS.wait

readQueueWithExceptionSpec :: HS.Spec
readQueueWithExceptionSpec = HS.describe "readQueueWithExceptionSpec" $ do
    T.whenQueueIsEmpty $ \ prepare -> do
        T.ioprop "read/write = 1/1" . prepare $ \ q -> do
            test (1,10000) (1,10000) q
        T.ioprop "read/write = 1/10" . prepare $ \ q -> do
            test (1,10000) (10,1000) q
        T.ioprop "read/write = 10/1" . prepare $ \ q -> do
            test (10,1000) (1,10000) q
        T.ioprop "read/write = 10/10" . prepare $ \ q -> do
            test (10,1000) (10,1000) q
        T.ioprop "read/write ratio random 100000" . prepare $ \ q -> do
            let genthnum = Q.arbitrary `Q.suchThat` (> 0)
                                       `Q.suchThat` ((== 0).(100000 `mod`))
            rthnum <- Q.generate $ genthnum
            wthnum <- Q.generate $ genthnum
            let rnum = 100000 `div` rthnum
                wnum = 100000 `div` wthnum
            test (rthnum,rnum) (wthnum,wnum) q
  where
    test :: (Int,Int) -> (Int,Int) -> KQ.Queue (Int,Int) -> IO ()
    test readConfig writeConfig q = do
        (results, writtens) <- readConcurrent q readConfig
            `T.concurrently` writeConcurrent q writeConfig
        case checkEachResult results of
            Right _  -> return ()
            Left str -> T.assertFailure str
        let result  = L.concat results
            written = L.concat writtens
            resultSet  = Set.fromList result
            writtenSet = Set.fromList written
        length result `T.shouldBe` length written
        (writtenSet `Set.difference` resultSet) `T.shouldBe` Set.empty
    checkEachResult = TF.traverse checkEachItems
    checkEachItems  = L.foldl' checkItems $ Right Map.empty
    checkItems (Right mp) (thnum, num)
        | Map.lookup thnum mp < Just num = Right $ Map.insert thnum num mp
        | Map.lookup thnum mp > Just num = Left "broken order"
        | otherwise                      = Left "duplicated value"
    checkItems err _ = err
    readItem refItems refCount q = do
        r <- KQ.readQueueWithoutMask q
        Ref.modifyIORef refCount (1+)
        Ref.modifyIORef refItems (r:)
    tryReadItem refItems refCount q = do
        mr <- KQ.tryReadQueueWithoutMask q
        case mr of
            Just r  -> do
                Ref.modifyIORef refCount (1+)
                Ref.modifyIORef refItems (r:)
            Nothing -> do
                CC.yield
                tryReadItem refItems refCount q
    readItemOne refItems refCount q = E.mask_ $ do
        select <- Q.generate Q.arbitrary
        if select
            then readItem    refItems refCount q
            else tryReadItem refItems refCount q
    readItems refItems refCount q size restore !c = do
        M.void . T.ignoreException . restore $ readItemOne refItems refCount q
        count <- Ref.readIORef refCount
        M.when (count < size && c < size * 100) $
            readItems refItems refCount q size restore $ c + 1
    readConcurrent q (thsize, itemsize) = do
        ass <- E.mask $ \ restore ->
            M.replicateM thsize . AS.async $ do
                refItems <- Ref.newIORef []
                refCount <- Ref.newIORef 0
                readItems refItems refCount q itemsize restore (0 :: Int)
                reverse <$> Ref.readIORef refItems
        M.void . AS.async $ T.throwExceptionRandomly ass
        TF.for ass AS.wait
    writeItem refItems q = E.mask_ $ do
        items <- Ref.readIORef refItems
        case items of
            []     -> return Nothing
            v:next -> do
                KQ.writeQueueWithoutMask q v
                Ref.writeIORef refItems next
                return $ Just v
    writeItems refItems q restore !c = do
        mmwritten <- T.ignoreException . restore $ writeItem refItems q
        case mmwritten of
            Just Nothing -> return ()
            _            -> writeItems refItems q restore $ c + 1
    writeConcurrent q (thsize, itemsize) = do
        ass <- E.mask $ \ restore ->
            TF.for [1..thsize] $ \ thnum -> AS.async $ do
                let items = fmap (thnum,) [1..itemsize] :: [(Int, Int)]
                refItems <- Ref.newIORef items
                writeItems refItems q restore (0 :: Int)
                return items
        M.void . AS.async $ T.throwExceptionRandomly ass
        TF.for ass AS.wait

spec :: HS.Spec
spec = HS.describe "KazuraQueue concurrent specs" $ do
    writeQueueSpec
    readQueueSpec
    tryReadQueueSpec
    readWriteQueueSpec
    tryReadWriteQueueSpec
    readQueueWithExceptionSpec

