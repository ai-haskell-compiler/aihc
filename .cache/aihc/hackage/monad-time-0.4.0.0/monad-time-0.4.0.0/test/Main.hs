module Main (main) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Time.Clock

import Control.Monad.Time

main :: IO ()
main = do
  currentTime >>= print
  -- Test that generic MonadTrans instance works.
  runReaderT currentTime 'x' >>= print
  evalStateT (runReaderT currentTime 'x') 'y' >>= print

  monotonicTime >>= print
  runReaderT monotonicTime 'x' >>= print
  evalStateT (runReaderT monotonicTime 'x') 'y' >>= print
