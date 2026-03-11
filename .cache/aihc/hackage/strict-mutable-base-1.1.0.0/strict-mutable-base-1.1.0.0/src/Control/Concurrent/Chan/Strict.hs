-- | For full documentation please refer to "Control.Concurrent.Chan".
module Control.Concurrent.Chan.Strict
  ( Chan'

    -- * Operations
  , newChan'
  , writeChan'
  , readChan'
  , dupChan'
  , getChan'Contents
  , writeList2Chan'
  ) where

import Control.Exception (evaluate)
import qualified Control.Concurrent.Chan as Base

-- | A strict (WHNF) variant of 'Base.Chan'.
newtype Chan' a = Chan' (Base.Chan a)
  deriving Eq

-- | 'Base.newChan' for 'Chan''.
newChan' :: IO (Chan' a)
newChan' = Chan' <$> Base.newChan

-- | 'Base.writeChan' for 'Chan''.
--
-- Evaluates the value to WHNF.
writeChan' :: Chan' a -> a -> IO ()
writeChan' (Chan' chan) a = Base.writeChan chan =<< evaluate a

-- | 'Base.readChan' for 'Chan''.
readChan' :: Chan' a -> IO a
readChan' (Chan' chan) = Base.readChan chan

-- | 'Base.dupChan' for 'Chan''.
dupChan' :: Chan' a -> IO (Chan' a)
dupChan' (Chan' chan) = Chan' <$> Base.dupChan chan

-- | 'Base.getChanContents' for 'Chan''.
getChan'Contents :: Chan' a -> IO [a]
getChan'Contents (Chan' chan) = Base.getChanContents chan

-- | 'Base.writeList2Chan' for 'Chan''.
--
-- Evaluates the values to WHNF.
writeList2Chan' :: Chan' a -> [a] -> IO ()
writeList2Chan' = mapM_ . writeChan'
