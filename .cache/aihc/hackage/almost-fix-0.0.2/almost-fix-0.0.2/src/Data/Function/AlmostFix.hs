module Data.Function.AlmostFix where

import Control.Monad


-- | Applies the predicate to the input:
-- @almostFix (< 5) (+1) 0  =  4@
almostFix :: (a -> Bool) -> (a -> a) -> a -> a
almostFix p f x = if p x then almostFix p f (f x)
                         else x

-- | Use a monadic predicate for the control flow.
almostFixM :: Monad m => m Bool -> (a -> m a) -> a -> m a
almostFixM p f x = do
  p' <- p
  if p' then almostFixM p f =<< f x
        else return x
