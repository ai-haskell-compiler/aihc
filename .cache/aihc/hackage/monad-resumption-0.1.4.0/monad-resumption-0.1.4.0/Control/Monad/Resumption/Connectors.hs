{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Resumption.Connectors where

import Control.Monad.Resumption.Reactive

-- | The parallel operator for combining computations in ReacT that share 
--   the same underlying monad and halting types.  No guarantees are given to 
--   which device's halting message will be seen by a handler.
(<||>) :: (Monad m) => ReacT i1 o1 m a -> ReacT i2 o2 m a -> ReacT (i1,i2) (o1,o2) m a
(<||>) (ReacT l) (ReacT r) = ReacT $ do
                                      l' <- l
                                      r' <- r
                                      case (l',r') of
                                          (Left a,_)                        -> return $ Left a
                                          (_,Left a)                        -> return $ Left a
                                          (Right (o1,res1),Right (o2,res2)) -> return $ Right ((o1,o2),\(i1,i2) -> (res1 i1) <||> (res2 i2))

-- | The refoldT operator is a refold where the input-modifying function yields a type in Maybe 
refoldT :: forall o1 o2 i1 i2 m a . Monad m => (o1 -> o2)
                                               -> (o1 -> i2 -> Maybe i1)
                                               -> ReacT i1 o1 m a
                                               -> ReacT i2 o2 m a
refoldT fo fi re = ReacT $ do
                             p <- deReacT re
                             case p of
                                Left a           -> return $ Left a
                                Right (o,resume) -> return $ Right (fo o, dispatch o resume)
    where
      dispatch :: o1 -> (i1 -> ReacT i1 o1 m a) -> (i2 -> ReacT i2 o2 m a)
      dispatch o1 resume = \i2 -> case fi o1 i2 of
                                        Nothing -> ReacT $ return $ Right (fo o1, dispatch o1 resume)
                                        Just x  -> refoldT fo fi (resume x)

-- | The refold operator changes the output and input types of a reactive resumption
refold :: (Monad m) => (o1 -> o2) -> (o1 -> i2 -> i1) -> ReacT i1 o1 m a -> ReacT i2 o2 m a
refold otpt inpt (ReacT r) = ReacT $ do
                                        r' <- r
                                        case r' of
                                          Left a          -> return $ Left a
                                          Right (o1,res1) -> return $ Right (otpt o1, \i2 -> refold otpt inpt (res1 (inpt o1 i2)))

-- | Chains two reactive resumptions together in a pipelined fashioned.  That is, inputs
--   and outputs are passed along between devices "tickwise".
pipeline :: (Monad m) => ReacT i z m a -> ReacT z o m a -> ReacT i o m a
pipeline r1 r2 = let r' = r1 <||> r2
                  in refold snd pipe r'
  where
    pipe oldout newinp = (newinp,(fst oldout))
