{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language UnboxedSums #-}
{-# language DataKinds #-}
{-# language PolyKinds #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language UnliftedNewtypes #-}

-- | "Primops" for weak references from (lifted or unlifted) values
-- to unlifted values. Several of these use a slightly different
-- interface than the underlying GHC primops. I have a GHC proposal
-- in progress (https://github.com/ghc-proposals/ghc-proposals/pull/367)
-- to make GHC match this interface. Note that the GHC primops work
-- just fine with unlifted types as /keys/, so we only need to fake
-- our own to use unlifted types as /values/.
module Data.Primitive.Unlifted.Weak.Primops
  ( UnliftedWeak#
  , mkWeakFromUnliftedToUnlifted#
  , mkWeakFromUnliftedToUnliftedNoFinalizer#
  , mkWeakToUnlifted#
  , mkWeakToUnliftedNoFinalizer#
  , addCFinalizerToUnliftedWeak1#
  , addCFinalizerToUnliftedWeak2#
  , deRefUnliftedWeak#
  , finalizeUnliftedWeak#
  ) where
import Data.Coerce (coerce)
import GHC.Exts
  ( RealWorld, State#
  , Weak#, mkWeak#, mkWeakNoFinalizer#, deRefWeak#, finalizeWeak#, Addr#
  , Int#, nullAddr#, addCFinalizerToWeak#)

import Data.Primitive.Unlifted.Type

-- | A weak pointer from a key (which may be lifted or unlifted)
-- to an unlifted value.
newtype UnliftedWeak# (a :: UnliftedType) = UnliftedWeak# (Weak# a)
type role UnliftedWeak# representational

-- The primops in GHC.Prim are "open kinded". They don't care if the
-- key is lifted or unlifted. But that sort of magic isn't available
-- to us, so we use separate primops for lifted and unlifted keys.

-- | @mkWeakFromUnliftedToUnlifted# k v finalizer s@ creates a weak reference
-- from an unlifted value @k@ to some unlifted value @v@. If @k@ is still alive
-- then @v@ can be retrieved using @deRefUnliftedWeak#@.
mkWeakFromUnliftedToUnlifted#
  :: forall (k :: UnliftedType) (v :: UnliftedType) c.
     k -> v -> (State# RealWorld -> (# State# RealWorld, c #))
  -> State# RealWorld -> (# State# RealWorld, UnliftedWeak# v #)
{-# INLINE mkWeakFromUnliftedToUnlifted# #-}
mkWeakFromUnliftedToUnlifted# k v finalizer s = coerce (mkWeak# k v finalizer s)

-- | The same as 'mkWeakFromUnliftedToUnlifted#' but without a finalizer.
mkWeakFromUnliftedToUnliftedNoFinalizer#
  :: forall (k :: UnliftedType) (v :: UnliftedType).
     k -> v -> State# RealWorld -> (# State# RealWorld, UnliftedWeak# v #)
{-# INLINE mkWeakFromUnliftedToUnliftedNoFinalizer# #-}
mkWeakFromUnliftedToUnliftedNoFinalizer# k v s = coerce (mkWeakNoFinalizer# k v s)

-- | @mkWeakToUnlifted# k v finalizer s@ creates a weak reference from a lifted
-- value @k@ to some unlifted value @v@. If @k@ is still alive then @v@ can be
-- retrieved using @deRefUnliftedWeak#@.
mkWeakToUnlifted#
  :: forall k (v :: UnliftedType) c.
     k -> v -> (State# RealWorld -> (# State# RealWorld, c #))
  -> State# RealWorld -> (# State# RealWorld, UnliftedWeak# v #)
{-# INLINE mkWeakToUnlifted# #-}
mkWeakToUnlifted# k v finalizer s = coerce (mkWeak# k v finalizer s)

-- | The same as 'mkWeakToUnlifted#' but without a finalizer.
mkWeakToUnliftedNoFinalizer#
  :: forall k (v :: UnliftedType).
     k -> v -> State# RealWorld -> (# State# RealWorld, UnliftedWeak# v #)
{-# INLINE mkWeakToUnliftedNoFinalizer# #-}
mkWeakToUnliftedNoFinalizer# k v s = coerce (mkWeakNoFinalizer# k v s)

-- | @addCFinalizerToUnliftedWeak1# fptr ptr w@ attaches a C function pointer
-- @fptr@ to a weak pointer @w@ as a finalizer. @ptr@ is an argument to be
-- passed to @fptr@.  @addCFinalizerToWeak1#@ returns @1#@ on success, or @0#@
-- if @w@ is already dead.
addCFinalizerToUnliftedWeak1# :: Addr# -> Addr# -> UnliftedWeak# b -> State# RealWorld -> (# State# RealWorld, Int# #)
{-# INLINE addCFinalizerToUnliftedWeak1# #-}
addCFinalizerToUnliftedWeak1# fptr ptr (UnliftedWeak# w)
  = addCFinalizerToWeak# fptr ptr 0# nullAddr# w

-- | @addCFinalizerToUnliftedWeak2# fptr eptr ptr w@ attaches a C function
-- pointer @fptr@ to a weak pointer @w@ as a finalizer. @eptr@ and @ptr@ are
-- arguments which will be passed to @fptr@ in order.  @addCFinalizerToWeak2#@
-- returns @1#@ on success, or @0#@ if @w@ is already dead.
addCFinalizerToUnliftedWeak2# :: Addr# -> Addr# -> Addr# -> UnliftedWeak# b -> State# RealWorld -> (# State# RealWorld, Int# #)
{-# INLINE addCFinalizerToUnliftedWeak2# #-}
-- Note: the underlying primop takes the function arguments in *reverse* order.
-- We fix that up here.
addCFinalizerToUnliftedWeak2# fptr eptr ptr (UnliftedWeak# w)
  = addCFinalizerToWeak# fptr ptr 1# eptr w

-- | Dereference an 'UnliftedWeak#'. If the pointer is already dead, returns
-- @(#(##) | #)@. Otherwise returns @(# | v #)@, where @v@ is the target of
-- the weak pointer.
deRefUnliftedWeak#
  :: UnliftedWeak# v
  -> State# RealWorld
  -> (# State# RealWorld, (# (##) | v #) #)
{-# INLINE deRefUnliftedWeak# #-}
deRefUnliftedWeak# (UnliftedWeak# w) s =
  case deRefWeak# w s of
    (# s', flag, p #) -> case flag of
                           0# -> (# s', (# (##) | #) #)
                           _  -> (# s', (# | p #) #)

-- | @finalizeUnliftedWeak#@ attempts to finalize an 'UnliftedWeak#'. If the
-- weak pointer is already dead, or it has no Haskell finalizer, it returns
-- @(#(##) | #)@. Otherwise, it returns @(# | f #)@, where @f@ is the Haskell
-- finalization action. The return value @b@ from the finalizer should be
-- ignored.  @finalizeUnliftedWeak#@ breaks the connection the @UnliftedWeak#@
-- has maintained between key and value and runs any C finalizers. After
-- finalization, @deRefUnliftedWeak#@ will return @(#(##) | #)@.
finalizeUnliftedWeak#
  :: UnliftedWeak# v
  -> State# RealWorld
  -> (# State# RealWorld, (# (##) | State# RealWorld -> (# State# RealWorld, b #) #) #)
{-# INLINE finalizeUnliftedWeak# #-}
finalizeUnliftedWeak# (UnliftedWeak# w) s =
  case finalizeWeak# w s of
    (# s', 0#, _ #) -> (# s', (# (##) | #) #) -- already dead, or no Haskell finalizer
    (# s', _, f #) -> (# s', (# | f #) #)
