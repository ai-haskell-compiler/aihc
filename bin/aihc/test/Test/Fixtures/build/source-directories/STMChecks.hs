{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module STMChecks (stmChecks) where
import GHC.Conc
import Control.Exception
import GHC.IO (IO(..))
import GHC.Prim
import GHC.Types (isTrue#)
import GHC.Weak
import Data.IORef

stmChecks :: IO Bool
stmChecks = do
  var <- newTVarIO (1 :: Int)
  other <- newTVarIO (1 :: Int)
  first <- atomically $ do
    writeTVar var 2
    readTVar var
  second <- atomically $
    (writeTVar var 3 >> retry) `orElse` readTVar var
  third <- atomically $ do
    writeTVar var 4
    catchSTM (writeTVar var 5 >> throwSTM Overflow)
      (\Overflow -> readTVar var)
  catch (atomically (writeTVar var 6 >> throwSTM Overflow))
    (\Overflow -> pure ())
  fourth <- readTVarIO var
  fifth <- atomically $
    catchSTM (writeTVar var 7 >> retry) (\Overflow -> pure 0)
      `orElse` readTVar var
  sixth <- catch (atomically (retry :: STM Bool))
    (\BlockedIndefinitelyOnSTM -> pure True)
  array <- arrayChecks
  weak <- weakChecks
  delay <- delayChecks
  pure (isTrue# (1# ==# 2# -# 1#) && array == 42 && weak && delay && first == 2 && second == 2 && third == 4 && fourth == 4
    && fifth == 4 && sixth && var == var && var /= other)


arrayChecks :: IO Int
arrayChecks = atomically $ STM $ \state ->
  case newTVar# (10 :: Int) state of
    (# state1, first #) -> case newArray# 2# first state1 of
      (# state2, array #) ->
        let fill token = case newTVar# (32 :: Int) token of
              (# next, second #) -> writeArray# array 1# second next
        in case unsafeFreezeArray# array (fill state2) of
          (# state3, frozen #) -> case indexArray# frozen 0# of
            (# left #) -> case indexArray# frozen 1# of
              (# right #) -> case readTVar# left state3 of
                (# state4, a #) -> case readTVar# right state4 of
                  (# state5, b #) -> (# state5, a + b #)


make :: TVar a -> IO () -> IO (Weak (TVar a))
make value@(TVar variable) (IO finalizer) = IO $ \state ->
  case mkWeak# variable value finalizer state of
    (# next, weak #) -> (# next, Weak weak #)

weakChecks :: IO Bool
weakChecks = do
  value <- newTVarIO (42 :: Int)
  count <- newIORef (0 :: Int)
  weak <- make value (modifyIORef count (+ 1))
  before <- deRefWeak weak
  finalize weak
  finalize weak
  after <- deRefWeak weak
  total <- readIORef count
  pure (before == Just value && after == Nothing && total == 1)


delayChecks :: IO Bool
delayChecks = do
  immediate <- registerDelay 0
  first <- readTVarIO immediate
  delayed <- registerDelay 1000
  second <- atomically $ do
    ready <- readTVar delayed
    if ready then pure ready else retry
  pure (first && second)
