
module Test.KazuraQueue where

import qualified Test.Expectations as T

import qualified Test.Hspec      as HS
import qualified Test.QuickCheck as Q

import qualified Control.Concurrent.KazuraQueue as KQ

import qualified Control.Concurrent       as CC
import qualified Control.Concurrent.Async as AS
import qualified Control.Exception        as E
import qualified Control.Monad            as M

import qualified Data.Foldable as TF

import qualified System.Timeout as ST

timeout :: IO r -> IO r
timeout act = do
    mr <- ST.timeout (10 * 1000000) act
    case mr of
        Just r  -> return r
        Nothing -> T.assertFailure "timeout 10sec"

whenQueueIsEmpty :: (((KQ.Queue x -> IO r) -> IO r) -> HS.Spec) -> HS.Spec
whenQueueIsEmpty f = HS.describe "when Queue is empty" $ f prepare
  where
    prepare :: (KQ.Queue x -> IO r) -> IO r
    prepare iof = do
        q <- KQ.newQueue
        timeout . prependIndefiniteBlock q $ iof q

whenItemsInQueue :: Q.Arbitrary x =>
    (Int, Int) -> ((((KQ.Queue x, [x]) -> IO r) -> IO r) -> HS.Spec) -> HS.Spec
whenItemsInQueue range f = HS.describe "when some items in Queue" $ f prepare
  where
    (minSize, maxSize) = range
    len = maxSize - minSize + 1
    prepare :: Q.Arbitrary x => ((KQ.Queue x, [x]) -> IO r) -> IO r
    prepare iof = do
        num <- (+ minSize) . (`mod` len) . abs <$> Q.generate Q.arbitrary
        vals <- M.replicateM num $ Q.generate Q.arbitrary
        queue <- KQ.newQueue
        TF.for_ vals $ KQ.writeQueue queue
        timeout . prependIndefiniteBlock queue $ iof (queue, vals)

prependIndefiniteBlock :: KQ.Queue x -> IO r -> IO r
prependIndefiniteBlock queue io = do
    async <- AS.async $ do
        CC.threadDelay $ 50 * 1000000 -- 50 sec
        KQ.writeQueue queue $ error "indefinitly blocked"
    io `E.finally` AS.cancel async

--------------- expectations

queueLengthShouldBeIn :: KQ.Queue x -> (Int, Int) -> IO ()
queueLengthShouldBeIn q (minv, maxv) = do
    len <- KQ.lengthQueue q `T.shouldNotBlock` 500000
    len `T.shouldSatisfy` T.and [(>= minv), (<= maxv)]
    alen <- KQ.lengthQueue' q `T.shouldNotBlock` 500000
    alen `T.shouldSatisfy` T.and [(>= max minv 0), (<= max maxv 0)]

