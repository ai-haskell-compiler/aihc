-- | Create two processes which bounce \"Ping\" and \"Pong\" messages back
--   and forth to each other

{-# LANGUAGE LambdaCase #-}

module Control.Concurrent.NanoErl.Examples.PingPong where

import Control.Concurrent.NanoErl
import Control.Monad

main :: IO ()
main = runNanoErl $ do
   pinger <- spawn pingPong
   ponger <- spawn pingPong
   pinger ! Pong ponger

data PingPongMsg
   = Ping (Pid PingPongMsg)
   | Pong (Pid PingPongMsg)
 deriving (Show)

pingPong :: Process PingPongMsg
pingPong self = do
   replicateM_ 5 $ receive self $ \case
      Ping pid -> do
         putStrLn $ show self ++ " got ping from " ++ show pid
         pid ! Pong self
      Pong pid -> do
         putStrLn $ show self ++ " got pong from " ++ show pid
         pid ! Ping self
   putStrLn "Ok got 5, I'm done!"
