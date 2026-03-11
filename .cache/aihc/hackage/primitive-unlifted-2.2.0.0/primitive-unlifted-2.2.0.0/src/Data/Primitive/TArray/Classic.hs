{-# language MagicHash #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language BangPatterns #-}
{-# language FlexibleInstances #-}
{-# language RoleAnnotations #-}
{- OPTIONS_GHC -ddump-simpl #-}


{- |
This module is a drop-in replacement for @Control.Concurrent.STM.TArray@
in the @stm@ package. It has the same fundamental inefficiency of the
classic @TArray@, but it's a /little/ faster and more compact.
Specifically, this implementation uses two fewer words of memory
and one fewer indirection per element.
We also add an 'MArray' instance for working in 'IO' that the 'stm'
version lacks.
Finally, the 'Eq' instance for the official @TArray@ is currently a little broken
thanks to a bug in the instance for @Data.Array.Array@ (See GHC Gitlab issue
#18700). We fix that bug here.
-}

module Data.Primitive.TArray.Classic (TArray) where
import GHC.Conc (STM, TVar, newTVar, readTVar, writeTVar
                , newTVarIO, readTVarIO, atomically)
import Data.Primitive.Unlifted.Array
import Data.Array.Base (MArray (..))
import Data.Ix (Ix, rangeSize)
import GHC.Exts (TVar#, RealWorld)

data TArray i a = TArray {
    _lb :: !i         -- the lower bound
  , _ub :: !i         -- the upper bound
  , range :: !Int    -- A cache of (rangeSize (l, u))
                     -- used to make sure an index is really in range
  , arr :: !(UnliftedArray_ (TVar# RealWorld a) (TVar a))
  }
type role TArray nominal representational

instance Eq i => Eq (TArray i a) where
  -- There's no way for TVars to move from one TArray to another, so two of
  -- them are equal iff they're both empty, with the same bounds, or they're
  -- actually the same array. There's no "safe" way to check if they're the
  -- same array (though we can use `unsafeCoerce#` with
  -- `sameMutableUnliftedArray#` if we want to). But we can just do a quick size
  -- check and then look at the first TVar of each.
  --
  -- Note: The instance in stm leans on the instance for @Array@ in @base@. As
  -- of base-4.14.0.0, that instance is broken. See GHC Gitlab issue #18700. It
  -- looks like that's probably going to get fixed, so we fix it here.
  TArray lb1 ub1 range1 arr1 == TArray lb2 ub2 range2 arr2
    | range1 /= range2 = False
      -- If the arrays are both empty, then they may still have been
      -- created with different bounds (e.g., (2,1) and (1,0)), so we
      -- check.
    | range1 == 0 = lb1 == lb2 && ub1 == ub2
      -- If the arrays are not empty, but the first TVar of each is the
      -- same, then they must have been created by the *same* newArray
      -- action. Therefore they are sure to have the same bounds, and
      -- are equal.
    | otherwise = indexUnliftedArray arr1 0 == indexUnliftedArray arr2 0

instance MArray TArray e STM where
  getBounds (TArray l u _ _) = pure (l, u)
  newArray b e = do
    tvs <- rep (rangeSize b) (newTVar e)
    return $ listTArray b tvs
  -- The stm version defines newArray_, but the default does the
  -- same thing.
  unsafeRead tarr i = readTVar $ indexUnliftedArray (arr tarr) i
  unsafeWrite tarr i e = writeTVar (indexUnliftedArray (arr tarr) i) e
  getNumElements !tarr = pure (range tarr)

-- | Writes are slow in 'IO'.
instance MArray TArray e IO where
  getBounds (TArray l u _ _) = pure (l, u)
  newArray b e = do
    tvs <- rep (rangeSize b) (newTVarIO e)
    return $ listTArray b tvs
  -- The stm version defines newArray_, but the default does the
  -- same thing.
  unsafeRead tarr i = readTVarIO $ indexUnliftedArray (arr tarr) i
  unsafeWrite tarr i e = atomically $ writeTVar (indexUnliftedArray (arr tarr) i) e
  getNumElements !tarr = pure (range tarr)

-- | Stolen from stm:
-- Like 'replicateM' but uses an accumulator to prevent stack overflows.
-- Unlike 'replicateM' the returned list is in reversed order.
-- This doesn't matter though since this function is only used to create
-- arrays with identical elements.
--
-- TODO: For `IO`, we should surely build the array directly, rather
-- than first making a list. For STM, I'm *guessing* this would be a
-- safe place to use unsafeIOtoSTM to do the same.
rep :: Monad m => Int -> m a -> m [a]
rep n m = go n []
    where
      go 0 xs = return xs
      go i xs = do
          x <- m
          go (i-1) (x:xs)

listTArray :: Ix i => (i, i) -> [TVar e] -> TArray i e
listTArray (l, u) tvs = TArray l u n (unliftedArrayFromListN n tvs)
  where
    !n = rangeSize (l, u)
