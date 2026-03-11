-- | This module implements the ZeroMQ reliable pub-sub (clone) pattern.
--
-- See
-- <http://zguide.zeromq.org/page:all#Reliable-Pub-Sub-Clone-Pattern
-- the ZeroMQ guide> for more information.
--
-- Example usage
--
-- @
-- import Control.Concurrent (threadDelay)
-- import Control.Concurrent.MVar
-- import Control.Concurrent.Async
--
-- import Control.Monad (forever)
--
-- import System.ZMQ4.Patterns.Clone (server, client)
--
-- -- | Produce an infinite stream of numbers
-- producer :: IO ()
-- producer = do
--     chan <- newEmptyMVar
--     withAsync (server "tcp:\/\/:5009" "tcp:\/\/:5010" chan) $ \\_ ->
--         let send i = putMVar chan i >> threadDelay (100*1000)
--         mapM_ send [(1 :: Integer)..]
--
-- -- | Consume the stream of numbers
-- consumer :: IO ()
-- consumer = do
--     chan <- newEmptyMVar
--     withAsync (client "tcp:\/\/127.0.0.1:5009" "tcp:\/\/127.0.0.1:5010" chan) $ \\_ ->
--         forever $ do
--             n <- takeMVar chan
--             print n
-- @
--
module System.ZMQ4.Patterns.Clone (
  -- * Server and client
  publisher
, subscriber
, queryLastState
) where

import System.ZMQ4.Patterns.Clone.Internal (publisher, subscriber, queryLastState)
