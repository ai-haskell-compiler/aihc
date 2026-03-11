{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}

module Main where

import qualified Control.Concurrent.KazuraQueue as KZR

-- import qualified Control.Concurrent.Chan.Unagi as UNG

import qualified Control.Concurrent      as CC
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM  as STM
import qualified Control.Exception       as E
import qualified Control.Monad           as M

import qualified Criterion.Main  as CR
import qualified Criterion.Types as CR

async :: IO a -> IO (MVar.MVar a, CC.ThreadId)
async act = do
    mv <- MVar.newEmptyMVar
    thid <- CC.forkIO $ do
        M.void . M.forever . CC.threadDelay $ 1000000 * 1000000
        M.void $ act >>= MVar.tryPutMVar mv
    M.void . CC.forkIO $ do
        (act >>= MVar.putMVar mv) `E.finally` CC.killThread thid
    return (mv, thid)

wait :: (MVar.MVar a, CC.ThreadId) -> IO a
wait = MVar.readMVar . fst

type IterationSize  = Int
type QueueNum       = Int
type WriteAction    = Int -> IO ()
type ReadAction     = IO Int
type WriteThreadNum = Int
type ReadThreadNum  = Int
type ItemNum        = Int
type TestOne        = WriteThreadNum -> ReadThreadNum -> IO ()

data QueueActions v = forall q . QueueActions
    { newQueue   :: IO q
    , writeQueue :: q -> v -> IO ()
    , readQueue  :: q -> IO v
    }

------------------------

testSpeed :: ItemNum -> QueueActions Int -> TestOne
testSpeed inum (QueueActions newAct writeAct readAct) wth rth = go
    where
        wnum = inum `div` wth
        rnum = inum `div` rth
        ws = [0..(wnum-1)] :: [Int]
        asyncw = M.replicateM wth . async
        asyncr = M.replicateM rth . async
        go = do
            q <- newAct
            wws <- asyncw $ M.forM_ ws $ writeAct q
            rws <- asyncr . M.replicateM_ rnum $ readAct q
            M.forM_ wws wait
            M.forM_ rws wait


testCost :: IterationSize -> QueueNum -> ItemNum -> QueueActions Int -> TestOne
testCost itersize qnum inum (QueueActions newAct writeAct readAct) wth rth = do
    itrq <- STM.newTQueueIO
    M.replicateM_ itersize . STM.atomically $ STM.writeTQueue itrq ()
    qws <- M.replicateM qnum . async $ go itrq
    M.forM_ qws wait
    where
        wnum = inum `div` wth
        rnum = inum `div` rth
        ws = [0..(wnum-1)] :: [Int]
        asyncw = M.replicateM wth . async
        asyncr = M.replicateM rth . async
        go itrq = do
            ma <- STM.atomically $ STM.tryReadTQueue itrq
            case ma of
                Nothing -> return ()
                Just () -> do
                    q <- newAct
                    wws <- asyncw $ M.forM_ ws $ writeAct q
                    rws <- asyncr . M.replicateM_ rnum $ readAct q
                    M.forM_ wws wait
                    M.forM_ rws wait
                    go itrq

testChan :: QueueActions a
testChan = QueueActions
    { newQueue   = Chan.newChan
    , writeQueue = Chan.writeChan
    , readQueue  = Chan.readChan
    }

{-
testUChan :: QueueActions a
testUChan = QueueActions
    { newQueue   = UNG.newChan
    , writeQueue = UNG.writeChan . fst
    , readQueue  = UNG.readChan  . snd
    }
--}

testTChan :: QueueActions a
testTChan = QueueActions
    { newQueue   = STM.newTChanIO
    , writeQueue = \ q -> STM.atomically . STM.writeTChan q
    , readQueue  = STM.atomically . STM.readTChan
    }

testTQueue :: QueueActions a
testTQueue = QueueActions
    { newQueue   = STM.newTQueueIO
    , writeQueue = \ q -> STM.atomically . STM.writeTQueue q
    , readQueue  = STM.atomically . STM.readTQueue
    }

testKZRQueue :: QueueActions a
testKZRQueue = QueueActions
    { newQueue   = KZR.newQueue
    , writeQueue = KZR.writeQueue
    , readQueue  = KZR.readQueue
    }

main :: IO ()
main = do
    CR.defaultMainWith configSpeed
        [
          CR.bgroup "KazuraQueue" $ testcases $ testSpeed_ testKZRQueue
        -- , CR.bgroup "UnagiChan"   $ testcases $ testSpeed_ testUChan
        , CR.bgroup "Chan"        $ testcases $ testSpeed_ testChan
        , CR.bgroup "TQueue"      $ testcases $ testSpeed_ testTQueue
        , CR.bgroup "TChan"       $ testcases $ testSpeed_ testTChan
        ]
    CR.defaultMainWith configCost
        [
          CR.bgroup "KazuraQueue" $ testcases $ testCost_ testKZRQueue
        -- , CR.bgroup "UnagiChan"   $ testcases $ testCost_ testUChan
        , CR.bgroup "Chan"        $ testcases $ testCost_ testChan
        , CR.bgroup "TQueue"      $ testcases $ testCost_ testTQueue
        , CR.bgroup "TChan"       $ testcases $ testCost_ testTChan
        ]
    where
        configCost = CR.defaultConfig
            { CR.reportFile = Just "report_cost.html" }
        configSpeed = CR.defaultConfig
            { CR.reportFile = Just "report_speed.html" }
        testSpeed_ = testSpeed 90000
        testCost_ = testCost 20 10 45000
        testcases test =
            [ testcase 1 1 test
            , testcase 3 1 test
            , testcase 1 3 test
            ]
        testcase wth rth test =
            CR.bench (show wth ++ "." ++ show rth) $
                CR.nfIO $ test wth rth


