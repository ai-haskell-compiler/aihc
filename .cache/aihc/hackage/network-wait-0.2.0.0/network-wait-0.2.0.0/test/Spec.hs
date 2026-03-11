-------------------------------------------------------------------------------
-- network-wait
-- Copyright 2022 Michael B. Gale (github@michael-gale.co.uk)
-------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE TypeApplications #-}

import Control.Concurrent
import Control.Monad.Catch
import Control.Retry
-- Only needed for base < 4.11, redundant otherwise
import Data.Semigroup

import Test.Tasty
import Test.Tasty.HUnit

import Network.Simple.TCP
import Network.Wait

-------------------------------------------------------------------------------

-- | Essentially the same as `retryPolicyDefault`, but here for compatibility
-- with older versions of `retry`.
testRetryPolicy :: Monad m => RetryPolicyM m
testRetryPolicy = constantDelay 50000 <> limitRetries 5

-- | `withServer` @delay action@ runs @action@ while asynchronously setting up
-- a TCP server on @localhost:5999@ with a @delay@ microseconds delay. The TCP
-- server will accept exactly one connection before shutting down.
withServer :: Int -> IO () -> IO ()
withServer delay k = do
    let initSocket = do
            (sock, _) <- bindSock (Host "localhost") "5999"
            listenSock sock 1
            pure sock

    tid <- forkIO $ do
        -- wait before starting the server
        threadDelay delay
        -- initialise the server, accept one connection, and shut down
        bracket initSocket closeSock $ \sock ->
            accept sock $ \(client,_) -> closeSock client

    -- run the computation that depends on the server
    k

    -- kill the server thread, if it is still alive
    killThread tid

tests :: TestTree
tests = testGroup "network-wait"
    [ testCase "Can't connect to service that doesn't exist" $ do
        res <- try @IO @SomeException $
            waitTcp testRetryPolicy "localhost" "5999"

        case res of
            Left _ -> pure ()
            Right _ -> assertFailure "`waitTcp` did not fail"
    , testCase "Can connect to service that does exist" $
        withServer 0 $ do
            res <- try @IO @SomeException $
                waitTcp testRetryPolicy "localhost" "5999"

            case res of
                Left ex -> assertFailure $
                    "`waitTcp` caused an exception: " <> show ex
                Right _ -> pure ()
    , testCase "Can connect to service after delay" $
        withServer 500000 $ do
            let policy = limitRetries 5 <> exponentialBackoff (60*1000)
            res <- try @IO @SomeException $
                waitTcp policy "localhost" "5999"

            case res of
                Left ex -> assertFailure $
                    "`waitTcp` caused an exception: " <> show ex
                Right _ -> pure ()
    ]

-- | `main` is the entry point to this test suite.
main :: IO ()
main = defaultMain tests

-------------------------------------------------------------------------------
