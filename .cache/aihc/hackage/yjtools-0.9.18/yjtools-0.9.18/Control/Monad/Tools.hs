module Control.Monad.Tools (
  ifM
, whenM
, unlessM
, doWhile_
, doWhile
, doUntil_
, doUntil
, for_
, for
, filterM
, repeatM
, repeatM_
, skipRet
) where

import Control.Monad         (when, unless)

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p t e = do b <- p
               if b then t else e

whenM, unlessM :: Monad m => m Bool -> m () -> m ()
whenM p t   = p >>= flip when t
unlessM p e = p >>= flip unless e

doWhile_, doUntil_ :: Monad m => m Bool -> m ()
doWhile_ act = do p <- act
                  when p $ doWhile_ act
doUntil_ act = do p <- act
                  unless p $ doUntil_ act

doWhile, doUntil :: Monad m => a -> (a -> m (a, Bool)) -> m a
doWhile i act = do (r, p) <- act i
                   if p then doWhile r act
		        else return r
doUntil i act = do (r, p) <- act i
                   if p then return r
		        else doUntil r act

for_ :: Monad m => a -> (a -> Bool) -> (a -> a) -> (a -> m b) -> m ()
for_ i test next act
  | test i    = act i >> for_ (next i) test next act
  | otherwise = return ()

for :: Monad m => a -> (a -> Bool) -> (a -> a) -> b -> (a -> b -> m b) -> m b
for i test next j act
  | test i    = act i j >>= flip (for (next i) test next) act
  | otherwise = return j

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM _ []     = return []
filterM p (x:xs) = do
  b  <- p x
  rs <- filterM p xs
  if b then return $ x:rs
       else return     rs

repeatM :: Monad m => m a -> m [ a ]
repeatM = sequence . repeat

repeatM_ :: Monad m => m a -> m ()
repeatM_ = sequence_ . repeat

skipRet :: Monad m => m b -> a -> m a
skipRet p x = p >> return x
