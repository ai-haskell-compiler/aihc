{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}

module Arithmetic.Fin
  ( -- * Modification
    incrementL
  , incrementL#
  , incrementR
  , incrementR#
  , weaken
  , weaken#
  , weakenL
  , weakenL#
  , weakenR
  , weakenR#
  , succ
  , succ#

    -- * Traverse

    -- | These use the terms @ascend@ and @descend@ rather than the
    -- more popular @l@ (left) and @r@ (right) that pervade the Haskell
    -- ecosystem. The general rule is that ascending functions pair
    -- the initial accumulator with zero with descending functions
    -- pair the initial accumulator with the last index.
  , ascend
  , ascend'
  , ascendFrom'
  , ascendFrom'#
  , ascendM
  , ascendM#
  , ascendM_
  , ascendM_#
  , ascendFromToM_#
  , descend
  , descend#
  , descend'
  , descendM
  , descendM_
  , ascending
  , descending
  , ascendingSlice
  , descendingSlice

    -- * Absurdities
  , absurd

    -- * Demote
  , demote
  , demote#
  , demote32#

    -- * Deconstruct
  , with
  , with#

    -- * Construct
  , construct#
  , nativeTo32#
  , nativeFrom32#
  , remInt#
  , remWord#
  , fromInt
  , fromInt#
  , constant#
  , greatest#

    -- * Compare
  , equals#

    -- * Substitute Bound
  , substitute#

    -- * Lift and Unlift
  , lift
  , unlift
  ) where

import Prelude hiding (last, succ)

import Arithmetic.Nat ((<?),(<?#))
import Arithmetic.Types (Difference (..), Fin (..), Nat, Nat#, pattern MaybeFinJust#, pattern MaybeFinNothing#, type (:=:), type (<), type (<#), type (<=), (:=:#))
import Arithmetic.Types (type (<=#))
import Arithmetic.Unsafe (Fin# (Fin#), MaybeFin#, Nat# (Nat#), Fin32#(Fin32#))
import Data.Maybe.Void (pattern JustVoid#)
import GHC.Exts (Int (I#), Int32#, Int#, Word#, (+#), (==#))
import GHC.TypeNats (CmpNat, type (+))

import qualified Arithmetic.Equal as Eq
import qualified Arithmetic.Lt as Lt
import qualified Arithmetic.Lte as Lte
import qualified Arithmetic.Nat as Nat
import qualified Arithmetic.Plus as Plus
import qualified Arithmetic.Unsafe as Unsafe
import qualified GHC.Exts as Exts
import qualified GHC.TypeNats as GHC

{- | Raise the index by @m@ and weaken the bound by @m@, adding
@m@ to the right-hand side of @n@.
-}
incrementR :: forall n m. Nat m -> Fin n -> Fin (n + m)
{-# INLINE incrementR #-}
incrementR m (Fin i pf) = Fin (Nat.plus i m) (Lt.incrementR @m pf)

incrementR# :: forall n m. Nat# m -> Fin# n -> Fin# (n + m)
{-# INLINE incrementR# #-}
incrementR# (Nat# n) (Fin# i) = Fin# (n +# i)

{- | Raise the index by @m@ and weaken the bound by @m@, adding
@m@ to the left-hand side of @n@.
-}
incrementL :: forall n m. Nat m -> Fin n -> Fin (m + n)
{-# INLINE incrementL #-}
incrementL m (Fin i pf) = Fin (Nat.plus m i) (Lt.incrementL @m pf)

incrementL# :: forall n m. Nat# m -> Fin# n -> Fin# (m + n)
{-# INLINE incrementL# #-}
incrementL# (Nat# n) (Fin# i) = Fin# (n +# i)

{- | Weaken the bound by @m@, adding it to the left-hand side of
the existing bound. This does not change the index.
-}
weakenL :: forall n m. Fin n -> Fin (m + n)
{-# INLINE weakenL #-}
weakenL (Fin i pf) =
  Fin
    i
    ( Lt.substituteR
        (Plus.commutative @n @m)
        (Lt.plus pf (Lte.zero @m))
    )

{- | Unboxed variant of 'weakenL'.
-}
weakenL# :: forall n m. Fin# n -> Fin# (m + n)
{-# INLINE weakenL# #-}
weakenL# (Fin# i) = Fin# i

{- | Weaken the bound by @m@, adding it to the right-hand side of
the existing bound. This does not change the index.
-}
weakenR :: forall n m. Fin n -> Fin (n + m)
{-# INLINE weakenR #-}
weakenR (Fin i pf) = Fin i (Lt.plus pf Lte.zero)

{- | Unboxed variant of 'weakenR'.
-}
weakenR# :: forall n m. Fin# n -> Fin# (n + m)
{-# INLINE weakenR# #-}
weakenR# (Fin# i) = Fin# i

{- | Weaken the bound, replacing it by another number greater than
or equal to itself. This does not change the index.
-}
weaken :: forall n m. (n <= m) -> Fin n -> Fin m
{-# INLINE weaken #-}
weaken lt (Fin i pf) = Fin i (Lt.transitiveNonstrictR pf lt)

weaken# :: forall n m. (n <=# m) -> Fin# n -> Fin# m
{-# INLINE weaken# #-}
weaken# _ (Fin# x) = Fin# x

-- | A finite set of no values is impossible.
absurd :: Fin 0 -> void
{-# INLINE absurd #-}
absurd (Fin _ pf) = Lt.absurd pf

{- | Fold over the numbers bounded by @n@ in descending
order. This is lazy in the accumulator. For convenince,
this differs from @foldr@ in the order of the parameters.

> descend 4 z f = f 0 (f 1 (f 2 (f 3 z)))
-}
descend ::
  forall a n.
  -- | Upper bound
  Nat n ->
  -- | Initial accumulator
  a ->
  -- | Update accumulator
  (Fin n -> a -> a) ->
  a
{-# INLINE descend #-}
descend !n b0 f = go Nat.zero
 where
  go :: Nat m -> a
  go !m = case m <? n of
    Nothing -> b0
    Just lt -> f (Fin m lt) (go (Nat.succ m))

descend# ::
  forall a n.
  -- | Upper bound
  Nat# n ->
  -- | Initial accumulator
  a ->
  -- | Update accumulator
  (Fin# n -> a -> a) ->
  a
{-# INLINE descend# #-}
descend# !n b0 f = descend (Nat.lift n) b0 (\ix a -> f (unlift ix) a)

{- | Fold over the numbers bounded by @n@ in descending
order. This is strict in the accumulator. For convenince,
this differs from @foldr'@ in the order of the parameters.

> descend 4 z f = f 0 (f 1 (f 2 (f 3 z)))
-}
descend' ::
  forall a n.
  -- | Upper bound
  Nat n ->
  -- | Initial accumulator
  a ->
  -- | Update accumulator
  (Fin n -> a -> a) ->
  a
{-# INLINE descend' #-}
descend' !n !b0 f = go n Lte.reflexive b0
 where
  go :: Nat p -> p <= n -> a -> a
  go !m pLteEn !b = case Nat.monus m Nat.one of
    Nothing -> b
    Just (Difference (mpred :: Nat c) cPlusOneEqP) ->
      let !cLtEn = descendLemma cPlusOneEqP pLteEn
       in go mpred (Lte.fromStrict cLtEn) (f (Fin mpred cLtEn) b)

{- | Fold over the numbers bounded by @n@ in ascending order. This
is lazy in the accumulator.

> ascend 4 z f = f 3 (f 2 (f 1 (f 0 z)))
-}
ascend ::
  forall a n.
  Nat n ->
  a ->
  (Fin n -> a -> a) ->
  a
{-# INLINE ascend #-}
ascend !n !b0 f = go n Lte.reflexive
 where
  go :: Nat p -> (p <= n) -> a
  go !m pLteEn = case Nat.monus m Nat.one of
    Nothing -> b0
    Just (Difference (mpred :: Nat c) cPlusOneEqP) ->
      let !cLtEn = descendLemma cPlusOneEqP pLteEn
       in f (Fin mpred cLtEn) (go mpred (Lte.fromStrict cLtEn))

{- | Strict fold over the numbers bounded by @n@ in ascending
order. For convenince, this differs from @foldl'@ in the
order of the parameters.

> ascend' 4 z f = f 3 (f 2 (f 1 (f 0 z)))
-}
ascend' ::
  forall a n.
  -- | Upper bound
  Nat n ->
  -- | Initial accumulator
  a ->
  -- | Update accumulator
  (Fin n -> a -> a) ->
  a
{-# INLINE ascend' #-}
ascend' !n !b0 f = go Nat.zero b0
 where
  go :: Nat m -> a -> a
  go !m !b = case m <? n of
    Nothing -> b
    Just lt -> go (Nat.succ m) (f (Fin m lt) b)

{- | Generalization of @ascend'@ that lets the caller pick the starting index:

> ascend' === ascendFrom' 0
-}
ascendFrom' ::
  forall a m n.
  -- | Index to start at
  Nat m ->
  -- | Number of steps to take
  Nat n ->
  -- | Initial accumulator
  a ->
  -- | Update accumulator
  (Fin (m + n) -> a -> a) ->
  a
{-# INLINE ascendFrom' #-}
ascendFrom' !m0 !n !b0 f = go m0 b0
 where
  end = Nat.plus m0 n
  go :: Nat k -> a -> a
  go !m !b = case m <? end of
    Nothing -> b
    Just lt -> go (Nat.succ m) (f (Fin m lt) b)

-- | Variant of @ascendFrom'@ with unboxed arguments.
ascendFrom'# ::
  forall a m n.
  -- | Index to start at
  Nat# m ->
  -- | Number of steps to take
  Nat# n ->
  -- | Initial accumulator
  a ->
  -- | Update accumulator
  (Fin# (m + n) -> a -> a) ->
  a
{-# INLINE ascendFrom'# #-}
ascendFrom'# !m0 !n !b0 f = ascendFrom' (Nat.lift m0) (Nat.lift n) b0 (\ix -> f (unlift ix))

{- | Strict monadic left fold over the numbers bounded by @n@
in ascending order. Roughly:

> ascendM 4 z0 f =
>   f 0 z0 >>= \z1 ->
>   f 1 z1 >>= \z2 ->
>   f 2 z2 >>= \z3 ->
>   f 3 z3
-}
ascendM ::
  forall m a n.
  (Monad m) =>
  -- | Upper bound
  Nat n ->
  -- | Initial accumulator
  a ->
  -- | Update accumulator
  (Fin n -> a -> m a) ->
  m a
{-# INLINE ascendM #-}
ascendM !n !b0 f = go Nat.zero b0
 where
  go :: Nat p -> a -> m a
  go !m !b = case m <? n of
    Nothing -> pure b
    Just lt -> go (Nat.succ m) =<< f (Fin m lt) b

{- | Variant of @ascendM@ that takes an unboxed Nat and provides
an unboxed Fin to the callback.
-}
ascendM# ::
  forall m a n.
  (Monad m) =>
  -- | Upper bound
  Nat# n ->
  -- | Initial accumulator
  a ->
  -- | Update accumulator
  (Fin# n -> a -> m a) ->
  m a
{-# INLINE ascendM# #-}
ascendM# n !a0 f = ascendM (Nat.lift n) a0 (\ix a -> f (unlift ix) a)

{- | Monadic traversal of the numbers bounded by @n@
in ascending order.

> ascendM_ 4 f = f 0 *> f 1 *> f 2 *> f 3
-}
ascendM_ ::
  forall m a n.
  (Applicative m) =>
  -- | Upper bound
  Nat n ->
  -- | Effectful interpretion
  (Fin n -> m a) ->
  m ()
{-# INLINE ascendM_ #-}
ascendM_ !n f = go Nat.zero
 where
  go :: Nat p -> m ()
  go !m = case m <? n of
    Nothing -> pure ()
    Just lt -> f (Fin m lt) *> go (Nat.succ m)

ascendFromToM_# ::
  forall m a i n.
  (Monad m) =>
  -- | Index to start at (inclusive)
  Nat# i ->
  -- | Upper bound (exclusive)
  Nat# n ->
  -- | Update accumulator
  (Fin# n -> m a) ->
  m ()
ascendFromToM_# m0 end f = go m0
  where
  go :: forall k. Nat# k -> m ()
  go m = case m <?# end of
    JustVoid# lt -> f (construct# lt m) *> go (Nat.succ# m)
    _ -> pure ()

{- | Variant of @ascendM_@ that takes an unboxed Nat and provides
an unboxed Fin to the callback.
-}
ascendM_# ::
  forall m a n.
  (Monad m) =>
  -- | Upper bound
  Nat# n ->
  -- | Update accumulator
  (Fin# n -> m a) ->
  m ()
{-# INLINE ascendM_# #-}
ascendM_# n f = ascendM_ (Nat.lift n) (\ix -> f (unlift ix))

descendLemma :: forall a b c. a + 1 :=: b -> b <= c -> a < c
{-# INLINE descendLemma #-}
descendLemma !aPlusOneEqB !bLteC =
  id
    $ Lt.transitiveNonstrictR
      ( Lt.substituteR
          (Plus.commutative @1 @a)
          (Lt.plus Lt.zero Lte.reflexive)
      )
    $ Lte.substituteL (Eq.symmetric aPlusOneEqB) bLteC

{- | Strict monadic left fold over the numbers bounded by @n@
in descending order. Roughly:

> descendM 4 z f =
>   f 3 z0 >>= \z1 ->
>   f 2 z1 >>= \z2 ->
>   f 1 z2 >>= \z3 ->
>   f 0 z3
-}
descendM ::
  forall m a n.
  (Monad m) =>
  Nat n ->
  a ->
  (Fin n -> a -> m a) ->
  m a
{-# INLINE descendM #-}
descendM !n !b0 f = go n Lte.reflexive b0
 where
  go :: Nat p -> p <= n -> a -> m a
  go !m pLteEn !b = case Nat.monus m Nat.one of
    Nothing -> pure b
    Just (Difference (mpred :: Nat c) cPlusOneEqP) ->
      let !cLtEn = descendLemma cPlusOneEqP pLteEn
       in go mpred (Lte.fromStrict cLtEn) =<< f (Fin mpred cLtEn) b

{- | Monadic traversal of the numbers bounded by @n@
in descending order.

> descendM_ 4 f = f 3 *> f 2 *> f 1 *> f 0
-}
descendM_ ::
  forall m a n.
  (Applicative m) =>
  -- | Upper bound
  Nat n ->
  -- | Effectful interpretion
  (Fin n -> m a) ->
  m ()
{-# INLINE descendM_ #-}
descendM_ !n f = go n Lte.reflexive
 where
  go :: Nat p -> p <= n -> m ()
  go !m !pLteEn = case Nat.monus m Nat.one of
    Nothing -> pure ()
    Just (Difference (mpred :: Nat c) cPlusOneEqP) ->
      let !cLtEn = descendLemma cPlusOneEqP pLteEn
       in f (Fin mpred cLtEn) *> go mpred (Lte.fromStrict cLtEn)

{- | Generate all values of a finite set in ascending order.

>>> ascending (Nat.constant @3)
[Fin 0,Fin 1,Fin 2]
-}
ascending :: forall n. Nat n -> [Fin n]
ascending !n = go Nat.zero
 where
  go :: Nat m -> [Fin n]
  go !m = case m <? n of
    Nothing -> []
    Just lt -> Fin m lt : go (Nat.succ m)

{- | Generate all values of a finite set in descending order.

>>> descending (Nat.constant @3)
[Fin 2,Fin 1,Fin 0]
-}
descending :: forall n. Nat n -> [Fin n]
descending !n = go n Lte.reflexive
 where
  go :: Nat p -> (p <= n) -> [Fin n]
  go !m !pLteEn = case Nat.monus m Nat.one of
    Nothing -> []
    Just (Difference (mpred :: Nat c) cPlusOneEqP) ->
      let !cLtEn = descendLemma cPlusOneEqP pLteEn
       in Fin mpred cLtEn : go mpred (Lte.fromStrict cLtEn)

{- | Generate 'len' values starting from 'off' in ascending order.

>>> ascendingSlice (Nat.constant @2) (Nat.constant @3) (Lte.constant @_ @6)
[Fin 2,Fin 3,Fin 4]
-}
ascendingSlice ::
  forall n off len.
  Nat off ->
  Nat len ->
  off + len <= n ->
  [Fin n]
{-# INLINE ascendingSlice #-}
ascendingSlice off len !offPlusLenLteEn = go Nat.zero
 where
  go :: Nat m -> [Fin n]
  go !m = case m <? len of
    Nothing -> []
    Just emLtLen ->
      let !offPlusEmLtOffPlusLen = Lt.incrementL @off emLtLen
          !offPlusEmLtEn = Lt.transitiveNonstrictR offPlusEmLtOffPlusLen offPlusLenLteEn
       in Fin (Nat.plus off m) offPlusEmLtEn : go (Nat.succ m)

{- | Generate 'len' values starting from 'off + len - 1' in descending order.

>>> descendingSlice (Nat.constant @2) (Nat.constant @3) (Lt.constant @6)
[Fin 4,Fin 3,Fin 2]
-}
descendingSlice ::
  forall n off len.
  Nat off ->
  Nat len ->
  off + len <= n ->
  [Fin n]
{-# INLINE descendingSlice #-}
descendingSlice !off !len !offPlusLenLteEn =
  go len Lte.reflexive
 where
  go :: Nat m -> m <= len -> [Fin n]
  go !m !mLteEn = case Nat.monus m Nat.one of
    Nothing -> []
    Just (Difference (mpred :: Nat c) cPlusOneEqEm) ->
      let !cLtLen =
            Lt.transitiveNonstrictR
              (Lt.substituteR (Plus.commutative @1 @c) (Lt.plus Lt.zero Lte.reflexive))
              -- c < c + 1
              (Lte.substituteL (Eq.symmetric cPlusOneEqEm) mLteEn)
          -- c + 1 <= len
          !cPlusOffLtEn =
            Lt.transitiveNonstrictR
              ( Lt.substituteR
                  (Plus.commutative @len @off)
                  (Lt.plus cLtLen (Lte.reflexive @off))
              )
              -- c + off < off + len
              offPlusLenLteEn
       in Fin (mpred `Nat.plus` off) cPlusOffLtEn : go mpred (Lte.fromStrict cLtLen)

{- | Extract the 'Int' from a 'Fin n'. This is intended to be used
at a boundary where a safe interface meets the unsafe primitives
on top of which it is built.
-}
demote :: Fin n -> Int
{-# INLINE demote #-}
demote (Fin i _) = Nat.demote i

demote# :: Fin# n -> Int#
{-# INLINE demote# #-}
demote# (Fin# i) = i

demote32# :: Fin32# n -> Int32#
{-# INLINE demote32# #-}
demote32# (Fin32# i) = i

lift :: Unsafe.Fin# n -> Fin n
{-# INLINE lift #-}
lift (Unsafe.Fin# i) = Fin (Unsafe.Nat (I# i)) Unsafe.Lt

unlift :: Fin n -> Unsafe.Fin# n
{-# INLINE unlift #-}
unlift (Fin (Unsafe.Nat (I# i)) _) = Unsafe.Fin# i

-- | Consume the natural number and the proof in the Fin.
with :: Fin n -> (forall i. (i < n) -> Nat i -> a) -> a
{-# INLINE with #-}
with (Fin i lt) f = f lt i

-- | Variant of 'with' for unboxed argument and result types.
with# :: Fin# n -> (forall i. (i <# n) -> Nat# i -> a) -> a
{-# INLINE with# #-}
with# (Unsafe.Fin# i) f = f (Unsafe.Lt# (# #)) (Unsafe.Nat# i)

construct# :: (i <# n) -> Nat# i -> Fin# n
{-# INLINE construct# #-}
construct# _ (Unsafe.Nat# x) = Unsafe.Fin# x

{- | Return the successor of the Fin or return nothing if the
argument is the greatest inhabitant.
-}
succ :: Nat n -> Fin n -> Maybe (Fin n)
{-# INLINE succ #-}
succ n (Fin ix _) = case ix' <? n of
  Nothing -> Nothing
  Just lt -> Just (Fin ix' lt)
 where
  ix' = Nat.succ ix

-- | Variant of 'succ' for unlifted finite numbers.
succ# :: Nat# n -> Fin# n -> MaybeFin# n
{-# INLINE succ# #-}
succ# (Nat# n) (Fin# ix) = case ix' Exts.<# n of
  0# -> MaybeFinNothing#
  _ -> MaybeFinJust# (Fin# ix')
 where
  !ix' = ix +# 1#

{- | Convert an Int to a finite number, testing that it is
less than the upper bound. This crashes with an uncatchable
exception when given a negative number.
-}
fromInt ::
  -- | exclusive upper bound
  Nat n ->
  Int ->
  Maybe (Fin n)
{-# INLINE fromInt #-}
fromInt bound i
  | i < 0 = errorWithoutStackTrace "Arithmetic.Fin.fromInt: negative argument"
  | otherwise = Nat.with i $ \number -> case number <? bound of
      Just lt -> Just (Fin number lt)
      Nothing -> Nothing

-- | Unboxed variant of 'fromInt'.
fromInt# ::
  -- | exclusive upper bound
  Nat# n ->
  Int# ->
  MaybeFin# n
{-# INLINE fromInt# #-}
fromInt# (Nat# n) i
  | Exts.isTrue# (i Exts.<# 0#) =
      errorWithoutStackTrace "Arithmetic.Fin.fromInt#: negative argument"
  | Exts.isTrue# (i Exts.<# n) = MaybeFinJust# (Fin# i)
  | otherwise = MaybeFinNothing#

{- | This crashes if @n = 0@. Divides @i@ by @n@ and takes
the remainder.
-}
remInt# :: Int# -> Nat# n -> Fin# n
remInt# i (Nat# n) = case n of
  0# -> errorWithoutStackTrace "Arithmetic.Fin.remInt#: cannot divide by zero"
  _ -> Fin# (Exts.remInt# i n)

{- | This crashes if @n = 0@. Divides @i@ by @n@ and takes
the remainder.
-}
remWord# :: Word# -> Nat# n -> Fin# n
remWord# w (Nat# n) = case n of
  0# -> errorWithoutStackTrace "Arithmetic.Fin.remWord#: cannot divide by zero"
  _ -> Fin# (Exts.word2Int# (Exts.remWord# w (Exts.int2Word# n)))

nativeTo32# :: (n <=# 2147483648) -> Fin# n -> Fin32# n
{-# inline nativeTo32# #-}
nativeTo32# _ (Fin# x) = Fin32# (Exts.intToInt32# x)

nativeFrom32# :: Fin32# n -> Fin# n
{-# inline nativeFrom32# #-}
nativeFrom32# (Fin32# x) = Fin# (Exts.int32ToInt# x)

{- | Create an unlifted finite number from an unlifted natural number.
The upper bound is the first type argument so that user can use
type applications to clarify when it is helpful. For example:

>>> Fin.constant# @10 N4#
-}
constant# :: forall (b :: GHC.Nat) (a :: GHC.Nat). (CmpNat a b ~ 'LT) => Nat# a -> Fin# b
constant# (Nat# i) = Fin# i

equals# :: Fin# n -> Fin# n -> Bool
equals# (Fin# a) (Fin# b) = Exts.isTrue# (a ==# b)

substitute# :: (m :=:# n) -> Fin# m -> Fin# n
substitute# _ (Fin# x) = Fin# x

greatest# :: Nat# n -> Fin# (n + 1)
greatest# (Nat# i) = Fin# i
