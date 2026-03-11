{-#LANGUAGE BangPatterns #-}
-- | A reactive resumption monad transformer, based on the formulation in
-- the article <http://people.cs.missouri.edu/~harrisonwl/drafts/CheapThreads.pdf Cheap (But Functional) Threads>
-- by William L. Harrison and Adam Procter.
module Control.Monad.Resumption.Reactive where

import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import qualified Control.Monad.Morph as Morph
import qualified Control.Monad.Trans as Trans
import Control.Monad.Resumption

-- | Reactive resumption monad transformer.
newtype ReacT input output m a = 
        ReacT { deReacT :: m (Either a (output, input -> ReacT input output m a)) }

instance Monad m => Monad (ReacT input output m) where
  return = ReacT . return . Left
  ReacT comp >>= f = ReacT $ do
                                inner <- comp
                                case inner of
                                    Left v                 -> deReacT (f v)
                                    Right (output, resume) -> return (Right (output,\ p -> resume p >>= f))

instance Trans.MonadTrans (ReacT input output) where
  lift m = ReacT $ m >>= return . Left

--instance Morph.MonadTrans (ReacT input output) where
--  lift m = ReacT $ m >>= return . Left

instance Monad m => Functor (ReacT input output m) where
  fmap f (ReacT m) = ReacT (m >>= \ r -> case r of
                                           Left v      -> return (Left (f v))
                                           Right (o,k) -> return (Right (o,\ i -> fmap f (k i))))

instance Monad m => Applicative (ReacT input output m) where
  pure  = return
  (<*>) = ap

instance MonadIO m => MonadIO (ReacT input output m) where
  liftIO = Trans.lift . liftIO

instance Morph.MFunctor (ReacT i o) where
  hoist f = ReacT . f . liftM (fmap (fmap (fmap (Morph.hoist f)))) . deReacT



-- | Outputs its argument, then waits for the next input and returns it.
signal :: Monad m => output -> ReacT input output m input
signal o = ReacT (return (Right (o,return)))

-- | Tennis operator.
(<~>) :: Monad m => ReacT i o m a -> ReacT o i m b -> ResT m (Either a b)
m1 <~> m2 = do r1 <- Trans.lift (deReacT m1)
               case r1 of
                 Left v        -> return (Left v)
                 Right (o1,k1) -> do
                   r2 <- Trans.lift (deReacT m2)
                   case r2 of
                     Left v        -> return (Right v)
                     Right (o2,k2) -> k1 o2 <~> k2 o1

-- | A basic runner function.  Provide the ReacT and a handler in the underlying monad to run.
runReacT :: Monad m => ReacT input output m a -> (output -> m input) -> m a
runReacT (ReacT r) handler = do
                        inner <- r
                        case inner of
                          Left a -> return a
                          Right (!output,!fr) -> do
                                                  next_input <- handler output
                                                  runReacT (fr next_input) handler
-- | A function for lifting pure functions to Reactive Resumptions.  Initial output is provided
-- as the second argument.
iter :: Monad m => (i -> o) -> o -> ReacT i o m a
iter f init_o = do
                  i <- signal init_o
                  iter' f i
  where
    iter' f i = do
                  i' <- signal (f i)
                  iter' f i'
