{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language DataKinds #-}
{-# language PolyKinds #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}

-- | "System.Mem.Weak" provides weak references from lifted keys to lifted
-- values. "Data.IORef", "Control.Concurrent.MVar", and
-- @Control.Concurrent.STM.TVar@ provide operations for producing weak
-- references from unlifted keys /of specific types/ to lifted values.
--
-- This module fills in the gaps. It offers a type ('UnliftedWeak') for weak
-- references from (lifted or unlifted) keys to unlifted values. It also
-- provides fully general operations for producing weak references from
-- unlifted keys to lifted values.
--
-- Usage note: Weak references /from/ lifted types can be fragile in the face
-- of GHC's unboxing optimizations. Weak references from unlifted types are
-- much more reliable. Weak references /to/ boxed types that wrap unlifted
-- types tend to be inefficient, because they keep not only the actual value
-- alive but also its box. Unless it's necessary to create a 'SMW.Weak'
-- reference to an unevaluated thunk, it's generally best to create an
-- 'UnliftedWeak' reference to the unlifted value instead.
module Data.Primitive.Unlifted.Weak
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
import Control.Monad.Primitive (PrimMonad,PrimState,ioToPrim)
import GHC.Exts (RealWorld)
import Data.Primitive.Unlifted.Class (PrimUnlifted)
import Data.Primitive.Unlifted.Weak.IO (UnliftedWeak_ (..), UnliftedWeak)
import qualified Data.Primitive.Unlifted.Weak.IO as W
import qualified System.Mem.Weak as SMW
import Foreign.Ptr (Ptr, FunPtr)

-- | Establishes a weak pointer from an unlifted value @k@ to an
-- unlifted value @v@ with an optional finalizer.
mkWeakFromUnliftedToUnlifted
  :: (PrimUnlifted k, PrimUnlifted v, PrimMonad m, PrimState m ~ RealWorld)
  => k -> v -> Maybe (IO ()) -> m (UnliftedWeak v)
{-# INLINE mkWeakFromUnliftedToUnlifted #-}
-- Why do we insist on an IO argument and not just a PrimBase one?
-- No particular reason. But that seems likely to make the type
-- harder to read without much practical benefit. Users can always use
-- primToIO if necessary to write their finalizers.
mkWeakFromUnliftedToUnlifted k v mf = ioToPrim $ W.mkWeakFromUnliftedToUnlifted k v mf

-- | Establishes a weak pointer from a lifted value @k@ to an
-- unlifted value @v@ with an optional finalizer.
mkWeakToUnlifted
  :: (PrimUnlifted v, PrimMonad m, PrimState m ~ RealWorld)
  => k -> v -> Maybe (IO ()) -> m (UnliftedWeak v)
{-# INLINE mkWeakToUnlifted #-}
mkWeakToUnlifted k v mf = ioToPrim $ W.mkWeakToUnlifted k v mf

-- | Establishes a weak pointer from an unlifted value @k@ to a
-- lifted value @v@ with an optional finalizer.
mkWeakFromUnlifted
  :: (PrimUnlifted k, PrimMonad m, PrimState m ~ RealWorld)
  => k -> v -> Maybe (IO ()) -> m (SMW.Weak v)
{-# INLINE mkWeakFromUnlifted #-}
mkWeakFromUnlifted k v mf = ioToPrim $ W.mkWeakFromUnlifted k v mf

-- | Derefences a weak pointer. If the key is still alive and the
-- pointer has not been finalized with 'finalizeUnlifted', then
-- @Just v@ is returned, where @v@ is the /value/ in the weak
-- pointer. Otherwise, @Nothing@ is returned.
deRefUnliftedWeak
  :: (PrimUnlifted v, PrimMonad m, PrimState m ~ RealWorld)
   => UnliftedWeak v -> m (Maybe v)
{-# INLINE deRefUnliftedWeak #-}
deRefUnliftedWeak w = ioToPrim $ W.deRefUnliftedWeak w

-- | Immediately finalize a weak pointer.
finalizeUnlifted
  :: (PrimMonad m, PrimState m ~ RealWorld)
  => UnliftedWeak v -> m ()
{-# INLINE finalizeUnlifted #-}
finalizeUnlifted w = ioToPrim $ W.finalizeUnlifted w

-- | Make a weak pointer from an unlifted value to itself.
--
-- Note: This should generally be preferred to @Data.IORef.mkWeakIORef@
-- and similar for making weak pointers to @IORef@s, @MVar@s, @TVar@s,
-- etc, as the values are stored more directly and compactly this way.
mkUnliftedWeakPtr
  :: (PrimUnlifted k, PrimMonad m, PrimState m ~ RealWorld)
  => k -> Maybe (IO ()) -> m (UnliftedWeak k)
{-# INLINE mkUnliftedWeakPtr #-}
mkUnliftedWeakPtr k fin = ioToPrim $ W.mkUnliftedWeakPtr k fin

-- | A specialised version of @mkUnliftedWeakPtr@, where the @UnliftedWeak@
-- object returned is simply thrown away (however the finalizer will be
-- remembered by the garbage collector, and will still be run when the key
-- becomes unreachable).
addFinalizerUnlifted
  :: (PrimUnlifted k, PrimMonad m, PrimState m ~ RealWorld)
   => k -> IO () -> m ()
{-# INLINE addFinalizerUnlifted #-}
addFinalizerUnlifted k fin = ioToPrim $ W.addFinalizerUnlifted k fin

-- | Add a finalizer written in C to an 'UnliftedWeak'. Takes a pointer to a C
-- function of one argument and an argument to call it with. Returns 'True'
-- on success, or 'False' if the 'UnliftedWeak' is already dead.
addCFinalizerToUnliftedWeak1
  :: (PrimMonad m, PrimState m ~ RealWorld)
  => FunPtr (a -> IO ()) -> Ptr a -> UnliftedWeak b -> m Bool
{-# INLINE addCFinalizerToUnliftedWeak1 #-}
addCFinalizerToUnliftedWeak1 f a w = ioToPrim $ W.addCFinalizerToUnliftedWeak1 f a w

-- | Add a finalizer written in C to an 'UnliftedWeak'. Takes a pointer to a C
-- function of two arguments and arguments to call it with. Returns 'True'
-- on success, or 'False' if the 'UnliftedWeak' is already dead.
addCFinalizerToUnliftedWeak2
  :: (PrimMonad m, PrimState m ~ RealWorld)
  => FunPtr (a -> b -> IO ()) -> Ptr a -> Ptr b -> UnliftedWeak c -> m Bool
{-# INLINE addCFinalizerToUnliftedWeak2 #-}
addCFinalizerToUnliftedWeak2 f a b w = ioToPrim $ W.addCFinalizerToUnliftedWeak2 f a b w

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
  :: (PrimUnlifted a, PrimMonad m, PrimState m ~ RealWorld)
  => a -> m ()
touchUnlifted a = ioToPrim $ W.touchUnlifted a
