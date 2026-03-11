{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language DataKinds #-}
{-# language PolyKinds #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language DataKinds #-}

-- | A version of "Data.Primitive.Unlifted.Weak" specialized to the 'IO' type.
module Data.Primitive.Unlifted.Weak.IO
  ( UnliftedWeak_ (..)
  , UnliftedWeak
  , mkWeakFromUnliftedToUnlifted
  , mkWeakToUnlifted
  , mkWeakFromUnlifted
  , deRefUnliftedWeak
  , finalizeUnlifted
  , mkUnliftedWeakPtr
  , addFinalizerUnlifted
  , addCFinalizerToUnliftedWeak1
  , addCFinalizerToUnliftedWeak2
  , touchUnlifted
  ) where

import GHC.Exts ( mkWeak#, mkWeakNoFinalizer# )
import Data.Primitive.Unlifted.Class (PrimUnlifted (..))
import Data.Primitive.Unlifted.Weak.Primops
import GHC.IO (IO (..))
import qualified GHC.Weak
import GHC.Ptr (Ptr (..), FunPtr (..))
import qualified GHC.Exts as Exts

import Data.Primitive.Unlifted.Type

-- | A weak pointer from a key (which may be lifted or unlifted)
-- to an unlifted value. In @UnliftedWeak_ a unlifted_a@, it is generally
-- expected that @unlifted_a ~ 'Unlifted' a@, but enforcing that here
-- would lead to unfortunate type roles. See "System.Mem.Weak" for detailed
-- information about weak references, including the notes at the end of that
-- module.
data UnliftedWeak_ a (unlifted_a :: UnliftedType) = UnliftedWeak (UnliftedWeak# unlifted_a)
type role UnliftedWeak_ phantom representational

-- | A type synonym for an 'UnliftedWeak_' containing lifted values of
-- a particular type. As a general rule, this type synonym should not be used in
-- class instances—use 'UnliftedWeak_' with an equality constraint instead.
-- It also should not be used when defining newtypes or datatypes, unless those
-- will have restrictive type roles regardless—use 'UnliftedWeak_' instead.
type UnliftedWeak a = UnliftedWeak_ a (Unlifted a)

instance unlifted_a ~ Unlifted a => PrimUnlifted (UnliftedWeak_ a unlifted_a) where
  {-# INLINE toUnlifted# #-}
  {-# INLINE fromUnlifted# #-}
  type Unlifted (UnliftedWeak_ _ unlifted_a) = UnliftedWeak# unlifted_a
  toUnlifted# (UnliftedWeak w) = w
  fromUnlifted# w = UnliftedWeak w

-- | Establishes a weak pointer from an unlifted value @k@ to an
-- unlifted value @v@ with an optional finalizer.
mkWeakFromUnliftedToUnlifted
  :: (PrimUnlifted k, PrimUnlifted v)
  => k -> v -> Maybe (IO ()) -> IO (UnliftedWeak v)
{-# INLINE mkWeakFromUnliftedToUnlifted #-}
mkWeakFromUnliftedToUnlifted k v (Just (IO finalizer)) = IO $ \s ->
  case mkWeakFromUnliftedToUnlifted# (toUnlifted# k) (toUnlifted# v) finalizer s of
    (# s', w #) -> (# s', UnliftedWeak w #)
mkWeakFromUnliftedToUnlifted k v Nothing = IO $ \s ->
  case mkWeakFromUnliftedToUnliftedNoFinalizer# (toUnlifted# k) (toUnlifted# v) s of
    (# s', w #) -> (# s', UnliftedWeak w #)

-- | Establishes a weak pointer from a lifted value @k@ to an
-- unlifted value @v@ with an optional finalizer.
mkWeakToUnlifted
  :: PrimUnlifted v
  => k -> v -> Maybe (IO ()) -> IO (UnliftedWeak v)
{-# INLINE mkWeakToUnlifted #-}
mkWeakToUnlifted k v (Just (IO finalizer)) = IO $ \s ->
  case mkWeakToUnlifted# k (toUnlifted# v) finalizer s of
    (# s', w #) -> (# s', UnliftedWeak w #)
mkWeakToUnlifted k v Nothing = IO $ \s ->
  case mkWeakToUnliftedNoFinalizer# k (toUnlifted# v) s of
    (# s', w #) -> (# s', UnliftedWeak w #)

-- | Establishes a weak pointer from an unlifted value @k@ to a
-- lifted value @v@ with an optional finalizer.
mkWeakFromUnlifted
  :: PrimUnlifted k
  => k -> v -> Maybe (IO ()) -> IO (GHC.Weak.Weak v)
{-# INLINE mkWeakFromUnlifted #-}
mkWeakFromUnlifted k v (Just (IO finalizer)) = IO $ \s ->
  case mkWeak# (toUnlifted# k) v finalizer s of
    (# s', w #) -> (# s', GHC.Weak.Weak w #)
mkWeakFromUnlifted k v Nothing = IO $ \s ->
  case mkWeakNoFinalizer# (toUnlifted# k) v s of
    (# s', w #) -> (# s', GHC.Weak.Weak w #)

-- | Derefences a weak pointer. If the key is still alive and the
-- pointer has not been finalized with 'finalizeUnlifted', then
-- @Just v@ is returned, where @v@ is the /value/ in the weak
-- pointer. Otherwise, @Nothing@ is returned.
deRefUnliftedWeak :: PrimUnlifted v => UnliftedWeak v -> IO (Maybe v)
{-# INLINE deRefUnliftedWeak #-}
deRefUnliftedWeak (UnliftedWeak w) = IO $ \s ->
  case deRefUnliftedWeak# w s of
    (# s', res #) -> case res of
      (# (# #) | #) -> (# s', Nothing #)
      (# | p #)  -> (# s', Just (fromUnlifted# p) #)

-- | Immediately finalize a weak pointer.
finalizeUnlifted :: UnliftedWeak v -> IO ()
{-# INLINE finalizeUnlifted #-}
finalizeUnlifted (UnliftedWeak w) = IO $ \s ->
  case finalizeUnliftedWeak# w s of
    (# s', (# (# #) | #) #) -> (# s', () #) -- already dead, or no finalizer
    (# s', (# | f #) #) -> f s'

-- | Make a weak pointer from an unlifted value to itself.
--
-- Note: This should generally be preferred to @Data.IORef.mkWeakIORef@
-- and similar for making weak pointers to @IORef@s, @MVar@s, @TVar@s,
-- etc, as the values are stored more directly and compactly this way.
mkUnliftedWeakPtr :: PrimUnlifted k => k -> Maybe (IO ()) -> IO (UnliftedWeak k)
{-# INLINE mkUnliftedWeakPtr #-}
mkUnliftedWeakPtr k fin = mkWeakFromUnliftedToUnlifted k k fin

-- | A specialised version of @mkUnliftedWeakPtr@, where the @UnliftedWeak@
-- object returned is simply thrown away (however the finalizer will be
-- remembered by the garbage collector, and will still be run when the key
-- becomes unreachable).
addFinalizerUnlifted :: PrimUnlifted k => k -> IO () -> IO ()
{-# INLINE addFinalizerUnlifted #-}
addFinalizerUnlifted k fin = do
  _ <- mkUnliftedWeakPtr k (Just fin) -- throw it away
  pure ()

-- | Add a finalizer written in C to an 'UnliftedWeak'. Takes a pointer to a C
-- function of one argument and an argument to call it with. Returns 'True'
-- on success, or 'False' if the 'UnliftedWeak' is already dead.
addCFinalizerToUnliftedWeak1 :: FunPtr (a -> IO ()) -> Ptr a -> UnliftedWeak b -> IO Bool
{-# INLINE addCFinalizerToUnliftedWeak1 #-}
addCFinalizerToUnliftedWeak1 (FunPtr f) (Ptr a) (UnliftedWeak w) =
  IO $ \s -> case addCFinalizerToUnliftedWeak1# f a w s of
    (# s', 0# #) -> (# s', False #)
    (# s', _ #) -> (# s', True #)

-- | Add a finalizer written in C to an 'UnliftedWeak'. Takes a pointer to a C
-- function of two arguments and arguments to call it with. Returns 'True'
-- on success, or 'False' if the 'UnliftedWeak' is already dead.
addCFinalizerToUnliftedWeak2 :: FunPtr (a -> b -> IO ()) -> Ptr a -> Ptr b -> UnliftedWeak c -> IO Bool
{-# INLINE addCFinalizerToUnliftedWeak2 #-}
addCFinalizerToUnliftedWeak2 (FunPtr f) (Ptr a) (Ptr b) (UnliftedWeak w) =
  IO $ \s -> case addCFinalizerToUnliftedWeak2# f a b w s of
    (# s', 0# #) -> (# s', False #)
    (# s', _ #) -> (# s', True #)

-- | Ensure that a value is considered live by the garbage collector at a
-- particular point in the program. Typically, this is used to prevent foreign
-- resources from being finalized while they are still being used.
--
-- Considerable care is required when using this operation (see GHC ticket
-- 14346). In particular, if GHC sees that an action @m@ will never complete
-- normally, then it will simplify @m >> touchUnlifted a@ to @m@, allowing @a@
-- to die prematurely. For now, functions using @touchUnlifted@ may require
-- careful use of @NOINLINE@ to work around this; in the future, GHC will
-- probably provide a more robust operation for keeping values alive.
touchUnlifted
  :: PrimUnlifted a
  => a -> IO ()
touchUnlifted a = IO $ \s ->
  (# Exts.touch# (toUnlifted# a) s, () #)
