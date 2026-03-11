
-- | Pipes that introduce parallelism on different levels.

module Pipes.Parallel where

import Control.Monad.Codensity (lowerCodensity)
import Control.Monad (replicateM)
import Control.Parallel.Strategies (Strategy, parMap)
import Pipes



-- | Evaluates chunks of pipes elements in parallel with a pure function.

pipePar
  :: (Monad m)
  => Int
  -- ^ number of elements to evaluate in parallel
  -> Strategy b
  -- ^ with which strategy
  -> (a -> b)
  -- ^ function to be mapped in parallel
  -> Pipe a b m ()
pipePar n strat f = pipeParBA n strat f (\as -> return ((),as)) (\() bs -> return bs)
{-
  where
  go = do
    xs <- lowerCodensity . replicateM n $ lift await
    let ys = parMap strat f xs
    lowerCodensity $ mapM_ (lift . yield) ys
    go
-}

-- | Evaluates chunks of pipes elements in parallel with a pure function.
-- Before and after each parallel step, a monadic function is run. This
-- allows generation of certain statistics or information during runs.

pipeParBA
  :: (Monad m)
  => Int
  -- ^ number of elements to evaluate in parallel
  -> Strategy b
  -- ^ with which strategy
  -> (a -> b)
  -- ^ pure function to run in parallel
  -> ([a] -> m (x,[a]))
  -- ^ function to run before
  -> (x -> [b] -> m [b])
  -- ^ function to run after
  -> Pipe a b m ()
pipeParBA n strat f bef aft = go
  where
  go = do
    as' <- lowerCodensity . replicateM n $ lift await
    (x,as) <- lift $ bef as'
    let bs' = parMap strat f as
    bs <- lift $ aft x bs'
    lowerCodensity $ mapM_ (lift . yield) bs
    go

