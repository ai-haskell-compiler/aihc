{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module PromptExamples where
import Control.Monad.Prompt
import Control.Monad.Cont  (MonadCont(..))
import Control.Monad.State (MonadState(..))
import Control.Monad       (MonadPlus(..))
import Control.Monad.ST    (ST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- Some standard monads implemented with Prompt:

-- State
data SP s a where
    Get :: SP s s
    Put :: s -> SP s ()

type PState s = Prompt (SP s)

instance MonadState s (Prompt (SP s)) where
    get = prompt Get
    put = prompt . Put

runPState :: forall r s. PState s r -> s -> (r, s)
runPState = runPromptC ret prm
  where
    ret :: r -> s -> (r,s)
    ret a s = (a, s)

    prm :: forall a. SP s a -> (a -> s -> (r,s)) -> s -> (r,s)
    prm Get      k st = k st st
    prm (Put st) k __ = k () st

testS :: PState Int Int
testS = do x <- get
           put (x+1)
           y <- get
           return (y*2)

-- StateT using PromptT
type PStateT s = PromptT (SP s)

instance MonadState s (PromptT (SP s) m) where
    get = prompt $ Get
    put = prompt . Put

runPStateT :: forall m r s. Monad m => PStateT s m r -> s -> m (r, s)
runPStateT = runPromptT ret prm lft
  where
    ret :: r -> s -> m (r,s)
    ret r s = return (r,s)

    prm :: forall a. SP s a -> (a -> s -> m (r,s)) -> s -> m (r,s)
    prm Get      k st = k st st
    prm (Put st) k __ = k () st

    lft :: forall a. m a -> (a -> s -> m (r,s)) -> s -> m (r,s)
    lft m        k st = m >>= \a -> k a st

-- MonadPlus with observation functions for "Maybe a" and "[a]"
data PP m a where
    PZero :: PP m a
    PPlus :: m a -> m a -> PP m a
type PPlus = RecPrompt PP

instance MonadPlus (RecPrompt PP) where
    mzero = prompt PZero
    mplus x y = prompt $ PPlus x y

runPPlus :: forall r m. (MonadPlus m) => PPlus r -> m r
runPPlus = runRecPromptM prm
  where prm :: forall a. PP PPlus a -> m a
        prm PZero       = mzero
        prm (PPlus x y) = runPPlus x `mplus` runPPlus y

runPPlusL :: forall r. PPlus r -> [r]
runPPlusL = runRecPromptC ret prm
  where ret x = [x]
        prm :: forall a. PP PPlus a -> (a -> [r]) -> [r]
        prm PZero k       = []
        prm (PPlus x y) k = concatMap k (runPPlusL x ++ runPPlusL y)

runPPlusM :: forall r. PPlus r -> Maybe r
runPPlusM = runRecPromptC ret prm
  where
    ret :: r -> Maybe r
    ret = Just
    prm :: forall a. PP PPlus a -> (a -> Maybe r) -> Maybe r
    prm PZero       _ = Nothing
    prm (PPlus x y) k = case (runPPlusM x, runPPlusM y) of
        (Just a, _) -> k a
        (_, Just a) -> k a
        _           -> Nothing

testP :: PPlus Int
testP = do x <- mplus (mplus (return 1) (return 2)) (mplus (return 3) (return 4))
           if x `div` 2 == 0 then mzero else return (x+5)

-- References, with observation functions in ST and IO
data PR ref a where
    NewRef   :: a -> PR ref (ref a)
    ReadRef  :: ref a -> PR ref a
    WriteRef :: ref a -> a -> PR ref ()
type PRef a = forall ref. Prompt (PR ref) a

runPRefST :: forall s r. PRef r -> ST s r
runPRefST m = runPromptM interp m where
    interp :: forall a. PR (STRef s) a -> ST s a
    interp (NewRef a)     = newSTRef a
    interp (ReadRef r)    = readSTRef r
    interp (WriteRef r a) = writeSTRef r a

runPRefIO :: forall r. PRef r -> IO r
runPRefIO m = runPromptM interp m where
    interp :: forall a. PR IORef a -> IO a
    interp (NewRef a)     = newIORef a
    interp (ReadRef r)    = readIORef r
    interp (WriteRef r a) = writeIORef r a

-- MonadCont
--
-- Implementation idea taken from the Unimo paper.
-- Is there a simpler way to do this?  It seems like there
-- should be, since we are representing the computation as
-- a continuation already.
data PromptCC r m a where
    CallCC :: ((a -> m b) -> m a) -> PromptCC r m a
    Apply :: r -> PromptCC r m a
type CallCC r = RecPrompt (PromptCC r)

instance MonadCont (RecPrompt (PromptCC r)) where
    callCC = prompt . CallCC

runContP :: forall ans r. CallCC ans r -> (r -> ans) -> ans
runContP = runPromptC ret prm . unRecPrompt
  where
    ret :: r -> (r -> ans) -> ans
    ret r f = f r

    prm :: forall a. PromptCC ans (CallCC ans) a -> (a -> (r -> ans) -> ans)
                      -> (r -> ans) -> ans
    prm (Apply r)  _ _  = r
    prm (CallCC f) k k2 = runContP (f cont) (\a -> k a k2)
       where cont a = prompt $ Apply (k a k2)
