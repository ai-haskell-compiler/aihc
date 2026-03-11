{-# language UnboxedTuples #-}
{-# language UnboxedSums #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
{-# language BangPatterns #-}
{- options_ghc -ddump-simpl #-}

-- | This module includes all the features of "Control.Concurrent.MVar", except
-- that the functions in "Data.Primitive.Unlifted.Weak" subsume the functionality
-- of @mkWeakMV@ and @addMVarFinalizer@, so we do not include analogues of those
-- functions.
module Data.Primitive.Unlifted.MVar.ST
  ( UnliftedMVar_ (..)
  , UnliftedMVar
  , newUnliftedMVar
  , newEmptyUnliftedMVar
  , takeUnliftedMVar
  , tryTakeUnliftedMVar
  , putUnliftedMVar
  , tryPutUnliftedMVar
  , readUnliftedMVar
  , tryReadUnliftedMVar
  , isEmptyUnliftedMVar
  , swapUnliftedMVar
  , withUnliftedMVar
  , withUnliftedMVarMasked
  , modifyUnliftedMVar
  , modifyUnliftedMVar_
  , modifyUnliftedMVarMasked
  , modifyUnliftedMVarMasked_
  ) where
import Data.Primitive.Unlifted.Class (PrimUnlifted (..))
import Data.Primitive.Unlifted.MVar.Primops
import Data.Primitive.Unlifted.Box
import GHC.Exts (isTrue#, State#, RealWorld)
import GHC.ST (ST (..))
import GHC.IO (IO (..))
import qualified Control.Exception as E -- (mask, mask_, onException)
import Control.Monad.Primitive (primToST, stToPrim)
import Data.Coerce (coerce)

mask :: ((forall a. ST RealWorld a -> ST RealWorld a) -> ST RealWorld b) -> ST RealWorld b
{-# INLINE mask #-}
mask f = primToST $ E.mask (\restore -> stToPrim $ f (primToST . restore . stToPrim))

mask_ :: ST RealWorld a -> ST RealWorld a
{-# INLINE mask_ #-}
mask_ f = mask $ \_ -> f

primitive_ :: (State# s -> State# s) -> ST s ()
{-# INLINE primitive_ #-}
primitive_ f = ST (\s -> (# f s, () #))

onException :: forall a b. ST RealWorld a -> ST RealWorld b -> ST RealWorld a
{-# INLINE onException #-}
onException = coerce (E.onException :: IO a -> IO b -> IO a)

data UnliftedMVar_ s a unlifted_a
  = UnliftedMVar (UnliftedMVar# s unlifted_a)
type role UnliftedMVar_ nominal phantom representational

type UnliftedMVar s a = UnliftedMVar_ s a (Unlifted a)

instance unlifted_a ~ Unlifted a => PrimUnlifted (UnliftedMVar_ s a unlifted_a) where
  {-# INLINE toUnlifted# #-}
  {-# INLINE fromUnlifted# #-}
  type Unlifted (UnliftedMVar_ s _ unlifted_a) = UnliftedMVar# s unlifted_a
  toUnlifted# (UnliftedMVar mv) = mv
  fromUnlifted# mv = UnliftedMVar mv

instance unlifted_a ~ Unlifted a => Eq (UnliftedMVar_ s a unlifted_a) where
  {-# INLINE (==) #-}
  UnliftedMVar mv1 == UnliftedMVar mv2
    = isTrue# (sameUnliftedMVar# mv1 mv2)

newUnliftedMVar
  :: PrimUnlifted a
  => a -> ST s (UnliftedMVar s a)
newUnliftedMVar a = do
  mv <- newEmptyUnliftedMVar
  putUnliftedMVar mv a
  pure mv

newEmptyUnliftedMVar :: ST s (UnliftedMVar s a)
{-# INLINE newEmptyUnliftedMVar #-}
newEmptyUnliftedMVar = ST $ \s -> case newUnliftedMVar# s of
  (# s', mv #) -> (# s', UnliftedMVar mv #)

takeUnliftedMVar
  :: PrimUnlifted a
  => UnliftedMVar s a -> ST s a
{-# INLINE takeUnliftedMVar #-}
takeUnliftedMVar = takeUnliftedMVar_

takeUnliftedMVarBox
  :: UnliftedMVar_ s x unlifted_a -> ST s (Box unlifted_a)
{-# INLINE takeUnliftedMVarBox #-}
takeUnliftedMVarBox = takeUnliftedMVar_

-- A version of takeUnliftedMVar that doesn't care about the
-- lifted type. We use this to specialize to Box so things can
-- get simplified more aggressively. This also avoids any
-- risk of exceptions happening in unexpected places in case
-- @toUnlifted#@ or @fromUnlifted#@ should fail.
takeUnliftedMVar_
  :: PrimUnlifted a
  => UnliftedMVar_ s x (Unlifted a) -> ST s a
{-# INLINE takeUnliftedMVar_ #-}
takeUnliftedMVar_ (UnliftedMVar mv) = ST $ \s ->
  case takeUnliftedMVar# mv s of
    (# s', a #) -> (# s', fromUnlifted# a #)

tryTakeUnliftedMVar
  :: PrimUnlifted a
  => UnliftedMVar s a -> ST s (Maybe a)
{-# INLINE tryTakeUnliftedMVar #-}
tryTakeUnliftedMVar (UnliftedMVar mv) = ST $ \s ->
  case tryTakeUnliftedMVar# mv s of
    (# s', (# | a #) #) -> (# s', Just (fromUnlifted# a) #)
    (# s', (# (##) | #) #) -> (# s', Nothing #)

putUnliftedMVar
  :: PrimUnlifted a
  => UnliftedMVar s a -> a -> ST s ()
{-# INLINE putUnliftedMVar #-}
putUnliftedMVar = putUnliftedMVar_

putUnliftedMVarBox
  :: UnliftedMVar_ s x unlifted_a -> Box unlifted_a -> ST s ()
{-# INLINE putUnliftedMVarBox #-}
putUnliftedMVarBox = putUnliftedMVar_

-- A version of putUnliftedMVar that doesn't care about the
-- lifted type. We use this to specialize to Box so things can
-- get simplified more aggressively. This also avoids any
-- risk of exceptions happening in unexpected places in case
-- @toUnlifted#@ or @fromUnlifted#@ should fail.
putUnliftedMVar_
  :: PrimUnlifted a
  => UnliftedMVar_ s x (Unlifted a) -> a -> ST s ()
{-# INLINE putUnliftedMVar_ #-}
putUnliftedMVar_ (UnliftedMVar mv) a = primitive_ $
  putUnliftedMVar# mv (toUnlifted# a)

tryPutUnliftedMVar
  :: PrimUnlifted a
  => UnliftedMVar s a -> a -> ST s Bool
{-# INLINE tryPutUnliftedMVar #-}
tryPutUnliftedMVar (UnliftedMVar mv) a = ST $ \s ->
  case tryPutUnliftedMVar# mv (toUnlifted# a) s of
    (# s', 0# #) -> (# s', False #)
    (# s', _ #) -> (# s', True #)

readUnliftedMVar
  :: PrimUnlifted a
  => UnliftedMVar s a -> ST s a
{-# INLINE readUnliftedMVar #-}
readUnliftedMVar (UnliftedMVar mv) = ST $ \s ->
  case readUnliftedMVar# mv s of
    (# s', a #) -> (# s', fromUnlifted# a #)

tryReadUnliftedMVar
  :: PrimUnlifted a
  => UnliftedMVar s a -> ST s (Maybe a)
{-# INLINE tryReadUnliftedMVar #-}
tryReadUnliftedMVar (UnliftedMVar mv) = ST $ \s ->
  case tryReadUnliftedMVar# mv s of
    (# s', (# (##) | #) #) -> (# s', Nothing #)
    (# s', (# | a #) #) -> (# s', Just (fromUnlifted# a) #)

isEmptyUnliftedMVar
  :: UnliftedMVar s a -> ST s Bool
{-# INLINE isEmptyUnliftedMVar #-}
isEmptyUnliftedMVar (UnliftedMVar mv) = ST $ \s ->
  case isEmptyUnliftedMVar# mv s of
    (# s', 0# #) -> (# s', False #)
    (# s', _ #) -> (# s', True #)

swapUnliftedMVar
  :: PrimUnlifted a
  => UnliftedMVar RealWorld a -> a -> ST RealWorld a
{-# INLINE swapUnliftedMVar #-}
swapUnliftedMVar mvar new =
  fromBox <$> (mask_ $ do
     old <- takeUnliftedMVarBox mvar
     putUnliftedMVarBox mvar new_box
     pure old)
  where !new_box = toBox new

withUnliftedMVar
  :: PrimUnlifted a
  => UnliftedMVar RealWorld a -> (a -> ST RealWorld b) -> ST RealWorld b
{-# INLINE withUnliftedMVar #-}
withUnliftedMVar m f =
  mask $ \restore -> do
    a <- takeUnliftedMVarBox m
    b <- restore (f (fromBox a)) `onException` putUnliftedMVarBox m a
    putUnliftedMVarBox m a
    pure b

withUnliftedMVarMasked
  :: PrimUnlifted a
  => UnliftedMVar RealWorld a -> (a -> ST RealWorld b) -> ST RealWorld b
{-# INLINE withUnliftedMVarMasked #-}
withUnliftedMVarMasked m st =
  mask_ $ do
    a <- takeUnliftedMVarBox m
    b <- st (fromBox a) `onException` putUnliftedMVarBox m a
    putUnliftedMVarBox m a
    pure b

data HalfUnlifted a b = HalfUnlifted !(Box (Unlifted a)) b

-- Note:
-- Except in the "masked" functions, we are careful not to use
-- toUnlifted# or fromUnlifted# with exceptions masked. In theory, those
-- operations could be slow.
--
-- mask, mask_, and onException deal in lifted values, which is a bit
-- annoying. The underlying primops do too. I wonder if that's essential.
-- Could we unsafeCoerce# our way to glory and let these functions return
-- unlifted pointers and even actions producing unboxed tuples?

modifyUnliftedMVar
  :: forall a b. PrimUnlifted a
  => UnliftedMVar RealWorld a -> (a -> ST RealWorld (a, b)) -> ST RealWorld b
{-# INLINE modifyUnliftedMVar #-}
modifyUnliftedMVar m st =
  mask $ \restore -> do
    a <- takeUnliftedMVarBox m
    HalfUnlifted a' b :: HalfUnlifted a b <- restore
      (do
         (a', b) <- st (fromBox a)
         pure $! HalfUnlifted (toBox a') b) `onException` putUnliftedMVarBox m a
    putUnliftedMVarBox m a'
    pure b

modifyUnliftedMVar_
  :: PrimUnlifted a
  => UnliftedMVar RealWorld a -> (a -> ST RealWorld a) -> ST RealWorld ()
{-# INLINE modifyUnliftedMVar_ #-}
modifyUnliftedMVar_ m st =
  mask $ \restore -> do
    a  <- takeUnliftedMVarBox m
    a' <- restore (toBox <$> st (fromBox a)) `onException` putUnliftedMVarBox m a
    putUnliftedMVarBox m a'

modifyUnliftedMVarMasked
  :: forall a b. PrimUnlifted a
  => UnliftedMVar RealWorld a -> (a -> ST RealWorld (a, b)) -> ST RealWorld b
{-# INLINE modifyUnliftedMVarMasked #-}
modifyUnliftedMVarMasked m st =
  mask_ $ do
    a <- takeUnliftedMVarBox m
    HalfUnlifted a' b :: HalfUnlifted a b <-
      (do
         (a', b) <- st (fromBox a)
         pure $! HalfUnlifted (toBox a') b) `onException` putUnliftedMVarBox m a
    putUnliftedMVarBox m a'
    pure b

modifyUnliftedMVarMasked_
  :: PrimUnlifted a
  => UnliftedMVar RealWorld a -> (a -> ST RealWorld a) -> ST RealWorld ()
{-# INLINE modifyUnliftedMVarMasked_ #-}
modifyUnliftedMVarMasked_ m st =
  mask_ $ do
    a  <- takeUnliftedMVarBox m
    a' <- (toBox <$> st (fromBox a)) `onException` putUnliftedMVarBox m a
    putUnliftedMVarBox m a'
