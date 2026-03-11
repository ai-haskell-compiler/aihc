-- | Monad helper functions
module Numeric.Probability.Monad where

import Control.Monad.HT ((<=<), )
import Control.Monad (liftM, )
import Prelude hiding (iterate, )

-- | composition of a list of monadic functions
compose :: Monad m => [a -> m a] -> a -> m a
compose = foldl (flip (<=<)) return


iterate :: Monad m => Int -> (a -> m a) -> (a -> m a)
iterate n f = compose $ replicate n f


-- | like 'iterate' but returns all intermediate values
-- like Control.Monad.HT.iterateLimit, but counts differently.
-- Is it actually useful this way?
walk :: (Monad m) => Int -> (a -> m a) -> (a -> m [a])
walk n f =
   let recourse 0 _ = return []
       recourse m x = liftM (x:) (recourse (pred m) =<< f x)
   in  recourse n


{- |
While loop with a posteriori check.
Loops at least once.
-}
doWhile :: Monad m => (a -> Bool) -> (a -> m a) -> (a -> m a)
doWhile p t =
   let recourse x = t x >>= \l -> if p l then recourse l else return l
   in  recourse

{- |
While loop with a priori check.
Can loop zero times.
-}
while :: Monad m => (a -> Bool) -> (a -> m a) -> (a -> m a)
while p t =
   let recourse x = if p x then t x >>= recourse else return x
   in  recourse


whileTrace :: Monad m => (a -> m Bool) -> m a -> m [a]
whileTrace p t =
   do x <- t
      b <- p x
      liftM (x:) $
         if b
           then whileTrace p t
           else return []
