{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Concurrent (Chan, forkIO, newChan, readChan)
import Control.Monad (void)
import OM.Fork (Responder, call, cast, respond)


{- | Messages that can be sent to the actor. -}
data MyMsg
  = ReverseEcho String (Responder String)
    {- ^ A blocking message. Responds with a String -}
  | Print String
    {- ^ A non-blocking message. -}
  | GetState (Responder Int)
    {- ^ Another blocking message. Responds with an Int -}


{- | The "state" is just a count of how many messages we've seen so far. -}
actorLoop :: Int -> Chan MyMsg -> IO void
actorLoop state chan = do
  readChan chan >>= \case
      ReverseEcho str responder ->
        void $ respond responder (reverse str)
      Print str ->
        putStrLn ("Printeded in background by actor thread: " <> str)
      GetState responder ->
        void $ respond responder state
  actorLoop (succ state) chan


main :: IO ()
main = do
  {- `Chan a` is an instance of `Actor` -}
  chan <- newChan
  void . forkIO $ actorLoop 0 chan

  {- | Notice that the result of this `call` is a String. -}
  putStrLn =<< call chan (ReverseEcho "hello world")
  cast chan (Print "foo")

  {- | Notice that the result of this `call` is an Int. -}
  actorState <- call chan GetState
  print (actorState * 2)


