{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.ST
  ( ST (..),
    STret (..),
    STRep,
    runST,
    liftST,
    unsafeInterleaveST,
    unsafeDupableInterleaveST,
  )
where

import GHC.Base (Applicative (..), Functor (..), Monad (..))
import GHC.Err (errorWithoutStackTrace)
import GHC.IO (ST (..))
import GHC.Prim (RealWorld, State#, newMutVar#, noDuplicate#, readMutVar#, realWorld#, writeMutVar#)
import GHC.Prim.MonadFix (MonadFix (..))
import GHC.Show (Show (..), showString)

-- | The strict state-thread monad.
-- | The unwrapped representation of an 'ST' computation.
type STRep s a = State# s -> (# State# s, a #)

instance Functor (ST s) where
  fmap f (ST action) =
    ST
      ( \state ->
          case action state of
            (# nextState, value #) -> (# nextState, f value #)
      )

instance Applicative (ST s) where
  pure value = ST (returnSTState value)

  ST function <*> ST argument =
    ST
      ( \state ->
          case function state of
            (# functionState, f #) ->
              case argument functionState of
                (# resultState, value #) -> (# resultState, f value #)
      )

instance Monad (ST s) where
  ST action >>= next =
    ST
      ( \state ->
          case action state of
            (# nextState, value #) ->
              case next value of
                ST nextAction -> nextAction nextState
      )

  ST action >> ST nextAction =
    ST
      ( \state ->
          case action state of
            (# nextState, _ #) -> nextAction nextState
      )

  return = pure

returnSTState :: a -> STRep s a
returnSTState value state = (# state, value #)

-- | A reference holds the result, as in the IO instance. The function gets a
-- lazy read of the reference.
instance MonadFix (ST s) where
  mfix k =
    ST
      ( \state ->
          case newMutVar# (errorWithoutStackTrace "fixST: the result is not available yet") state of
            (# state1, ref #) ->
              let ans =
                    case readMutVar# ref state1 of
                      (# _, value #) -> value
               in case k ans of
                    ST action ->
                      case action state1 of
                        (# state2, result #) ->
                          case writeMutVar# ref result state2 of
                            state3 -> (# state3, result #)
      )

-- | A lifted result from an 'ST' computation.
data STret s a = STret (State# s) a

instance Show (ST s a) where
  showsPrec _ _ = showString "<<ST action>>"

-- | Run an 'ST' computation while retaining its final state token.
liftST :: ST s a -> State# s -> STret s a
liftST (ST action) state =
  case action state of
    (# nextState, value #) -> STret nextState value

-- | Defer an 'ST' computation until its result is demanded.
unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST action = unsafeDupableInterleaveST (noDuplicateST >> action)

noDuplicateST :: ST s ()
noDuplicateST = ST (\state -> (# noDuplicate# state, () #))

-- | Defer an 'ST' computation without preventing duplicate evaluation.
unsafeDupableInterleaveST :: ST s a -> ST s a
unsafeDupableInterleaveST (ST action) =
  ST
    ( \state ->
        let result =
              case action state of
                (# _, value #) -> value
         in (# state, result #)
    )

-- | Return the value computed by a state thread.
runST :: (forall s. ST s a) -> a
runST action = runSTRealWorld (instantiateST action)

instantiateST :: (forall s. ST s a) -> ST RealWorld a
instantiateST action = action

runSTRealWorld :: ST RealWorld a -> a
runSTRealWorld (ST action) =
  case action realWorld# of
    (# _, value #) -> value
