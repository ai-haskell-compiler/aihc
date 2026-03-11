-- |
-- Module: Data.Conduit.FoldDebounce
-- Description: Regulate input traffic from conduit Source with Control.FoldDebounce
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- Synopsis:
--
-- > module Main (main) where
-- >
-- > import Data.Conduit (ConduitT, yield, runConduit, (.|))
-- > import qualified Data.Conduit.List as CL
-- > import Data.Void (Void)
-- > import Control.Concurrent (threadDelay)
-- > import Control.Monad.IO.Class (liftIO)
-- > import Control.Monad.Trans.Resource (ResourceT, runResourceT)
-- >
-- > import qualified Data.Conduit.FoldDebounce as F
-- >
-- > fastSource :: Int -> ConduitT () Int (ResourceT IO) ()
-- > fastSource max_num = fastStream' 0 where
-- >   fastStream' count = do
-- >     yield count
-- >     if count >= max_num
-- >       then return ()
-- >       else do
-- >         liftIO $ threadDelay 100000
-- >         fastStream' (count + 1)
-- >
-- > printSink :: Show a => ConduitT a Void (ResourceT IO) ()
-- > printSink = CL.mapM_ (liftIO . putStrLn . show)
-- >
-- > main :: IO ()
-- > main = do
-- >   putStrLn "-- Before debounce"
-- >   runResourceT $ runConduit $ fastSource 10 .| printSink
-- >   let debouncer = F.debounce F.Args { F.cb = undefined, -- anything will do
-- >                                       F.fold = (\list num -> list ++ [num]),
-- >                                       F.init = [] }
-- >                              F.def { F.delay = 500000 }
-- >   putStrLn "-- After debounce"
-- >   runResourceT $ runConduit $ debouncer (fastSource 10) .| printSink
--
-- Result:
--
-- > -- Before debounce
-- > 0
-- > 1
-- > 2
-- > 3
-- > 4
-- > 5
-- > 6
-- > 7
-- > 8
-- > 9
-- > 10
-- > -- After debounce
-- > [0,1,2,3,4]
-- > [5,6,7,8,9]
-- > [10]
--
-- This module regulates (slows down) data stream from conduit source
-- using "Control.FoldDebounce".
--
-- The data from the original source (type @i@) are pulled and folded
-- together to create an output data (type @o@). The output data then
-- comes out of the debounced source in a predefined interval
-- (specified by 'delay' option).
--
-- See "Control.FoldDebounce" for detail.
module Data.Conduit.FoldDebounce
    ( debounce
      -- * Re-exports
    , Args (..)
    , Opts
    , def
      -- ** Accessors for 'Opts'
    , delay
    , alwaysResetTimer
      -- * Preset parameters
    , forStack
    , forMonoid
    , forVoid
    ) where

import           Control.Monad                (void)
import           Data.Monoid                  (Monoid)
import           Data.Void                    (Void)
import           Prelude                      hiding (init)

import           Control.Concurrent.STM       (TVar, atomically, newTChanIO, newTVarIO, readTChan,
                                               readTVar, writeTChan, writeTVar)
import           Control.FoldDebounce         (Args (Args, cb, fold, init), Opts, alwaysResetTimer,
                                               def, delay)
import qualified Control.FoldDebounce         as F
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Resource (MonadResource, MonadUnliftIO, allocate, register,
                                               release, resourceForkIO, runResourceT)
import           Data.Conduit                 (ConduitT, await, bracketP, runConduit, yield, (.|))

-- | Debounce conduit source with "Control.FoldDebounce". The data
-- stream from the original source (type @i@) is debounced and folded
-- into the data stream of the type @o@.
--
-- Note that the original source is connected to a sink in another
-- thread. You may need some synchronization if the original source
-- has side-effects.
debounce :: (MonadResource m, MonadUnliftIO m)
            => Args i o -- ^ mandatory argument for FoldDebounce. 'cb'
                        -- field is ignored, so you can set anything
                        -- to that.
            -> Opts i o -- ^ optional argument for FoldDebounce
            -> ConduitT () i m () -- ^ original source
            -> ConduitT () o m () -- ^ debounced source
debounce args opts src = bracketP initOutTermed finishOutTermed debounceWith
  where
    initOutTermed = newTVarIO False
    finishOutTermed = atomically . flip writeTVar True
    debounceWith out_termed = do
      out_chan <- liftIO $ newTChanIO
      lift $ runResourceT $ do
        void $ register $ atomically $ writeTChan out_chan OutFinished
        (_, trig) <- allocate (F.new args { F.cb = atomically . writeTChan out_chan . OutData }
                                     opts)
                              (F.close)
        void $ resourceForkIO $ lift $ runConduit (src .| trigSink trig out_termed)
      keepYield out_chan
    keepYield out_chan = do
      mgot <- liftIO $ atomically $ readTChan out_chan
      case mgot of
       OutFinished -> return ()
       OutData got -> yield got >> keepYield out_chan

-- | Internal data type for output channel.
data OutData o
  = OutData o
  | OutFinished

trigSink :: (MonadIO m) => F.Trigger i o -> TVar Bool -> ConduitT i Void m ()
trigSink trig out_termed = trigSink' where
  trigSink' = do
    mgot <- await
    termed <- liftIO $ atomically $ readTVar out_termed
    case (termed, mgot) of
      (True, _) -> return ()
      (False, Nothing) -> return ()
      (False, Just got) -> do
        liftIO $ F.send trig got
        trigSink'


-- | 'Args' for stacks. Input events are accumulated in a stack, i.e.,
-- the last event is at the head of the list.
forStack :: Args i [i]
forStack = F.forStack undefined

-- | 'Args' for monoids. Input events are appended to the tail.
forMonoid :: Monoid i => Args i i
forMonoid = F.forMonoid undefined

-- | 'Args' that discards input events. The data stream from the
-- debounced source indicates the presence of data from the original
-- source.
forVoid :: Args i ()
forVoid = F.forVoid undefined
