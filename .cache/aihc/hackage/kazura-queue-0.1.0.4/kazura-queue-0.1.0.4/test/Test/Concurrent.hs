{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Concurrent where

import qualified Test.Hspec            as HS
import qualified Test.Hspec.QuickCheck as HS
import qualified Test.QuickCheck       as Q

import qualified Control.Concurrent       as CC
import qualified Control.Concurrent.Async as AS
import qualified Control.Concurrent.MVar  as MV
import qualified Control.Exception        as E
import           Control.Monad            ((>=>))
import qualified Control.Monad            as M
import qualified GHC.Conc                 as CC

import qualified Data.Maybe       as MB
import qualified Data.Traversable as TF
import           Data.Typeable    (Typeable)

-- | Multiple times test enabled IO spec
ioprop :: (HS.HasCallStack, Q.Testable prop) => String -> prop -> HS.Spec
ioprop desc prop = HS.prop desc $ \ () -> prop

class HasThread th where
    threadId     :: th -> IO CC.ThreadId
    throwTo      :: E.Exception e => th -> e -> IO ()
    throwTo th e = threadId th >>= flip E.throwTo e

threadStatus :: HasThread th => th -> IO CC.ThreadStatus
threadStatus = threadId >=> CC.threadStatus

instance HasThread CC.ThreadId where
    threadId = return

instance HasThread (AS.Async x) where
    threadId = return . AS.asyncThreadId

isFinish :: CC.ThreadStatus -> Bool
isFinish CC.ThreadFinished = True
isFinish CC.ThreadDied     = True
isFinish _                 = False

isStop :: CC.ThreadStatus -> Bool
isStop CC.ThreadRunning = False
isStop _                = True

withWaitStart :: (IO () -> IO x) -> IO x
withWaitStart actf = do
    mv    <- MV.newEmptyMVar
    mdelay <- Q.generate $ arbitraryDelay 20000
    async <- AS.async . actf $ MV.readMVar mv
    case mdelay of
        Just delay -> CC.threadDelay delay
        Nothing    -> return ()
    CC.putMVar mv ()
    AS.wait async

concurrently :: IO a -> IO b -> IO (a, b)
concurrently act1 act2 = do
    mdelay1 <- Q.generate $ arbitraryDelay 20000
    mdelay2 <- Q.generate $ arbitraryDelay 20000
    withWaitStart $ \ wait ->
        wrap wait mdelay1 act1 `AS.concurrently` wrap wait mdelay2 act2
  where
    wrap :: IO () -> Maybe Int -> IO a -> IO a
    wrap wait mdelay act = wait >> TF.for mdelay CC.threadDelay >> act

mapConcurrently :: [IO a] -> IO [a]
mapConcurrently acts = do
    let len = length acts
    mds <- Q.generate . Q.vectorOf len $ fmap (`mod` 20000) <$> Q.arbitrary
    withWaitStart $ \ wait -> do
        AS.mapConcurrently id $ wrap wait <$> zip mds acts
  where
    wrap :: IO () -> (Maybe Int, IO a) -> IO a
    wrap wait (mdelay, act) = wait >> TF.for mdelay CC.threadDelay >> act

mapConcurrently_ :: [IO a] -> IO ()
mapConcurrently_ = M.void . mapConcurrently

waitStop :: HasThread th => th -> IO CC.ThreadStatus
waitStop th = snd . head <$> waitAny isStop [th]

waitFinish :: HasThread th => th -> IO CC.ThreadStatus
waitFinish th = snd . head <$> waitFinishAny [th]

waitFinishAny :: HasThread th => [th] -> IO [(Int, CC.ThreadStatus)]
waitFinishAny = waitAny isFinish

waitAny :: HasThread th =>
    (CC.ThreadStatus -> Bool) -> [th] -> IO [(Int, CC.ThreadStatus)]
waitAny = waitAnyAtLeast 1

waitAnyAtLeast :: HasThread th =>
    Int -> (CC.ThreadStatus -> Bool) -> [th] -> IO [(Int, CC.ThreadStatus)]
waitAnyAtLeast num f ths = go
  where
    go = do
        statuses <- M.sequence $ threadStatus <$> ths
        let satisfied = filter (f . snd) $ zip [0..] statuses
        if length satisfied >= num
            then return satisfied
            else CC.threadDelay 1 >> go

data RandomException = RandomException Int String
    deriving (Show, Typeable)
instance E.Exception RandomException

ignoreException :: IO a -> IO (Maybe a)
ignoreException act = (Just <$> act)
    `E.catch` \ (_err :: RandomException) -> do
--        E.uninterruptibleMask_ $ putStrLn $ "---- Exception throwed : " ++ show _err
        return Nothing

ignoreException_ :: IO a -> IO ()
ignoreException_ = M.void . ignoreException

runningThreadId :: HasThread th => th -> IO (Maybe CC.ThreadId)
runningThreadId th = do
    status <- threadStatus th
    if isFinish status
        then return Nothing
        else Just <$> threadId th

throwExceptionRandomly :: HasThread th => [th] -> IO ()
throwExceptionRandomly ths = go (1 :: Int)
  where
    getAlives = fmap MB.catMaybes . M.sequence $ runningThreadId <$> ths
    go !c = do
        mdelay <- Q.generate $ arbitraryDelay $ 20000 * c
        case mdelay of
            Just delay -> CC.threadDelay delay
            Nothing    -> return ()
        alives <- getAlives
        if length alives == 0
            then return ()
            else do
                alive <- Q.generate $ Q.elements alives
                throwTo alive . RandomException c $ show mdelay ++ " : " ++ show (length alives)
                go $ c+1

arbitraryDelay :: Int -> Q.Gen (Maybe Int)
arbitraryDelay limit = do
    mbase <- Q.arbitrary
    multi1 <- (+1) . abs <$> Q.arbitrary
    multi2 <- (+1) . abs <$> Q.arbitrary
    case mbase of
        Just base -> return . Just . (`mod` limit) $ base * multi1 * multi2
        Nothing   -> return Nothing

