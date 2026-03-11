{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ < 806
{-# LANGUAGE TypeInType #-}
#endif

{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Exports orphan instances that mimic instances available in later versions of @base@.
To use them, simply @import Data.Orphans ()@.
-}
module Data.Orphans () where

#ifdef __GLASGOW_HASKELL__
# if !(MIN_VERSION_base(4,11,0))
import qualified Control.Monad.Fail as Fail (MonadFail(..))
# endif

# if !(MIN_VERSION_base(4,16,0))
import qualified Data.Functor.Product as Functor
# endif

# if !(MIN_VERSION_base(4,10,0))
import           Data.Data as Data
# endif

# if !(MIN_VERSION_base(4,11,0))
import           Control.Monad.ST as Strict
# endif

# if MIN_VERSION_base(4,11,0) && !(MIN_VERSION_base(4,21,0))
import           GHC.Read (readField)
# endif

# if !(MIN_VERSION_base(4,12,0))
import qualified Data.Foldable as F (Foldable(..))
import qualified Data.Traversable as T (Traversable(..))
# endif

# if MIN_VERSION_base(4,15,0) && !(MIN_VERSION_base(4,16,0))
import           GHC.Tuple (Solo(..))
# endif

# if !(MIN_VERSION_base(4,21,0))
import           Data.Orphans.Prelude
import           GHC.Generics as Generics hiding (prec)
# endif

#include "HsBaseConfig.h"

-------------------------------------------------------------------------------

# if !(MIN_VERSION_base(4,10,0))
deriving instance (Typeable k, Data a, Typeable (b :: k)) => Data (Const a b)

instance Eq1 NonEmpty where
  liftEq eq (a :| as) (b :| bs) = eq a b && liftEq eq as bs

instance Ord1 NonEmpty where
  liftCompare cmp (a :| as) (b :| bs) = cmp a b <> liftCompare cmp as bs

instance Read1 NonEmpty where
  liftReadsPrec rdP rdL p s = readParen (p > 5) (\s' -> do
    (a, s'') <- rdP 6 s'
    (":|", s''') <- lex s''
    (as, s'''') <- rdL s'''
    return (a :| as, s'''')) s

instance Show1 NonEmpty where
  liftShowsPrec shwP shwL p (a :| as) = showParen (p > 5) $
    shwP 6 a . showString " :| " . shwL as

instance Semigroup a => Semigroup (IO a) where
    (<>) = liftA2 (<>)

#  if !defined(mingw32_HOST_OS) && !defined(ghcjs_HOST_OS)
instance Semigroup Event where
    (<>) = mappend
    stimes = stimesMonoid

instance Semigroup Lifetime where
    (<>) = mappend
    stimes = stimesMonoid
#  endif
# endif

# if !(MIN_VERSION_base(4,11,0))
instance Alternative ZipList where
  empty = ZipList []
  ZipList xs <|> ZipList ys = ZipList (xs ++ drop (length xs) ys)

deriving instance Monoid a => Monoid (Down a)
deriving instance Num a => Num (Down a)

instance Functor Down where
    fmap = coerce

instance Applicative Down where
    pure = Down
    (<*>) = coerce

instance Monad Down where
    return = Down
    Down a >>= k = k a

instance Monoid a => Monoid (Strict.ST s a) where
    mempty = pure mempty
    mappend = liftA2 mappend

instance Fail.MonadFail (Strict.ST s) where
    fail s = errorWithoutStackTrace s

deriving instance Semigroup a => Semigroup (Down a)

instance Semigroup a => Semigroup (Strict.ST s a) where
    (<>) = liftA2 (<>)

#  if MIN_VERSION_base(4,10,0)
deriving instance Data IntPtr
deriving instance Data WordPtr
#  else
-- The constructors for IntPtr and WordPtr aren't exposed on older versions
-- of base, so we're forced to hand-roll the Data instances here
instance Data IntPtr where
  gfoldl k z iptr = z intPtr `k` unIntPtr iptr
  gunfold k z _ = k (z intPtr)
  toConstr !_ = cIntPtr
  dataTypeOf _ = tIntPtr

intPtr :: Int -> IntPtr
intPtr = unsafeCoerce

unIntPtr :: IntPtr -> Int
unIntPtr = unsafeCoerce

tIntPtr :: DataType
tIntPtr = mkDataType "IntPtr" [cIntPtr]

cIntPtr :: Constr
cIntPtr = mkConstr tIntPtr "IntPtr" [] Data.Prefix

instance Data WordPtr where
  gfoldl k z wptr = z wordPtr `k` unWordPtr wptr
  gunfold k z _ = k (z wordPtr)
  toConstr !_ = cWordPtr
  dataTypeOf _ = tWordPtr

wordPtr :: Word -> WordPtr
wordPtr = unsafeCoerce

unWordPtr :: WordPtr -> Word
unWordPtr = unsafeCoerce

tWordPtr :: DataType
tWordPtr = mkDataType "WordPtr" [cWordPtr]

cWordPtr :: Constr
cWordPtr = mkConstr tWordPtr "WordPtr" [] Data.Prefix
#  endif
# endif

# if !(MIN_VERSION_base(4,12,0))
instance MonadFix Down where
    mfix f = Down (fix (getDown . f))
      where getDown (Down x) = x
deriving instance Data a => Data (Down a)
deriving instance F.Foldable Down
deriving instance T.Traversable Down
instance MonadZip Down where
    mzipWith = liftM2
instance Eq1 Down where
    liftEq eq (Down x) (Down y) = eq x y
instance Ord1 Down where
    liftCompare comp (Down x) (Down y) = case comp x y of
        LT -> GT
        EQ -> EQ
        GT -> LT
instance Read1 Down where
    liftReadsPrec rp _ = readsData $
         readsUnaryWith rp "Down" Down
instance Show1 Down where
    liftShowsPrec sp _ d (Down x) = showsUnaryWith sp "Down" d x

instance Monoid c => Applicative (K1 i c) where
  pure _ = K1 mempty
  (<*>) = coerce (mappend :: c -> c -> c)
#  if MIN_VERSION_base(4,10,0)
  liftA2 = \_ -> coerce (mappend :: c -> c -> c)
#  endif

instance Monoid (U1 p) where
  mempty = U1
#  if !(MIN_VERSION_base(4,11,0))
  _ `mappend` _ = U1
#  endif
deriving instance Monoid p => Monoid (Par1 p)
deriving instance Monoid (f p) => Monoid (Rec1 f p)
deriving instance Monoid c => Monoid (K1 i c p)
deriving instance Monoid (f p) => Monoid (M1 i c f p)
instance (Monoid (f p), Monoid (g p)) => Monoid ((f :*: g) p) where
  mempty = mempty :*: mempty
#  if !(MIN_VERSION_base(4,11,0))
  (x1 :*: y1) `mappend` (x2 :*: y2) = (x1 `mappend` x2) :*: (y1 `mappend` y2)
#  endif
deriving instance Monoid (f (g p)) => Monoid ((f :.: g) p)

instance Semigroup (V1 p) where
  v <> _ = v
instance Semigroup (U1 p) where
  _ <> _ = U1
deriving instance Semigroup p => Semigroup (Par1 p)
deriving instance Semigroup (f p) => Semigroup (Rec1 f p)
deriving instance Semigroup c => Semigroup (K1 i c p)
deriving instance Semigroup (f p) => Semigroup (M1 i c f p)
instance (Semigroup (f p), Semigroup (g p)) => Semigroup ((f :*: g) p) where
  (x1 :*: y1) <> (x2 :*: y2) = (x1 <> x2) :*: (y1 <> y2)
deriving instance Semigroup (f (g p)) => Semigroup ((f :.: g) p)

deriving instance Foldable f => Foldable (Alt f)
deriving instance Traversable f => Traversable (Alt f)
# endif

# if !(MIN_VERSION_base(4,14,0))
instance Functor ((,,) a b) where
    fmap f (a, b, c) = (a, b, f c)

instance (Monoid a, Monoid b) => Applicative ((,,) a b) where
    pure x = (mempty, mempty, x)
    (a, b, f) <*> (a', b', x) = (a `mappend` a', b `mappend` b', f x)

instance (Monoid a, Monoid b) => Monad ((,,) a b) where
    (u, v, a) >>= k =
      case k a of
        (u', v', b) ->
          (u `mappend` u', v `mappend` v', b)

instance Functor ((,,,) a b c) where
    fmap f (a, b, c, d) = (a, b, c, f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative ((,,,) a b c) where
    pure x = (mempty, mempty, mempty, x)
    (a, b, c, f) <*> (a', b', c', x) =
      (a `mappend` a', b `mappend` b', c `mappend` c', f x)

instance (Monoid a, Monoid b, Monoid c) => Monad ((,,,) a b c) where
    (u, v, w, a) >>= k =
      case k a of
        (u', v', w', b) ->
          (u `mappend` u', v `mappend` v', w `mappend` w', b)

deriving instance Functor m => Functor (Kleisli m a)

instance Applicative m => Applicative (Kleisli m a) where
  pure = Kleisli . const . pure
  {-# INLINE pure #-}
  Kleisli f <*> Kleisli g = Kleisli $ \x -> f x <*> g x
  {-# INLINE (<*>) #-}
  Kleisli f *> Kleisli g = Kleisli $ \x -> f x *> g x
  {-# INLINE (*>) #-}
  Kleisli f <* Kleisli g = Kleisli $ \x -> f x <* g x
  {-# INLINE (<*) #-}

instance Alternative m => Alternative (Kleisli m a) where
  empty = Kleisli $ const empty
  {-# INLINE empty #-}
  Kleisli f <|> Kleisli g = Kleisli $ \x -> f x <|> g x
  {-# INLINE (<|>) #-}

instance Monad m => Monad (Kleisli m a) where
  Kleisli f >>= k = Kleisli $ \x -> f x >>= \a -> runKleisli (k a) x
  {-# INLINE (>>=) #-}

instance MonadPlus m => MonadPlus (Kleisli m a) where
  mzero = Kleisli $ const mzero
  {-# INLINE mzero #-}
  Kleisli f `mplus` Kleisli g = Kleisli $ \x -> f x `mplus` g x
  {-# INLINE mplus #-}

-- | Swaps @'minBound'@ and @'maxBound'@ of the underlying type.
instance Bounded a => Bounded (Down a) where
    minBound = Down maxBound
    maxBound = Down minBound

deriving instance Bits       a => Bits       (Down a)
deriving instance Floating   a => Floating   (Down a)
deriving instance Fractional a => Fractional (Down a)
deriving instance Ix         a => Ix         (Down a)
deriving instance Real       a => Real       (Down a)
deriving instance RealFrac   a => RealFrac   (Down a)
deriving instance RealFloat  a => RealFloat  (Down a)
deriving instance Storable   a => Storable   (Down a)
deriving instance FiniteBits a => FiniteBits (Down a)

deriving instance (Typeable2 a, Typeable b, Typeable c, Data (a b c))
                         => Data (WrappedArrow a b c)
deriving instance (Typeable1 m, Typeable a, Data (m a))
                         => Data (WrappedMonad m a)
deriving instance Data a => Data (ZipList a)

instance IsList (ZipList a) where
  type Item (ZipList a) = a
  fromList = ZipList
  toList = getZipList

instance (TestEquality f) => TestEquality (Compose f g) where
  testEquality (Compose x) (Compose y) =
    case testEquality x y of -- :: Maybe (g x :~: g y)
      Just Refl -> Just Refl -- :: Maybe (x :~: y)
      Nothing   -> Nothing
# endif

# if !(MIN_VERSION_base(4,15,0))
instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6) =>
      Ix (a1,a2,a3,a4,a5,a6)  where
    range ((l1,l2,l3,l4,l5,l6),(u1,u2,u3,u4,u5,u6)) =
      [(i1,i2,i3,i4,i5,i6) | i1 <- range (l1,u1),
                             i2 <- range (l2,u2),
                             i3 <- range (l3,u3),
                             i4 <- range (l4,u4),
                             i5 <- range (l5,u5),
                             i6 <- range (l6,u6)]

    unsafeIndex ((l1,l2,l3,l4,l5,l6),(u1,u2,u3,u4,u5,u6)) (i1,i2,i3,i4,i5,i6) =
      unsafeIndex (l6,u6) i6 + unsafeRangeSize (l6,u6) * (
      unsafeIndex (l5,u5) i5 + unsafeRangeSize (l5,u5) * (
      unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
      unsafeIndex (l1,u1) i1)))))

    inRange ((l1,l2,l3,l4,l5,l6),(u1,u2,u3,u4,u5,u6)) (i1,i2,i3,i4,i5,i6) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3 && inRange (l4,u4) i4 &&
      inRange (l5,u5) i5 && inRange (l6,u6) i6

    -- Default method for index

instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7) =>
      Ix (a1,a2,a3,a4,a5,a6,a7)  where
    range ((l1,l2,l3,l4,l5,l6,l7),(u1,u2,u3,u4,u5,u6,u7)) =
      [(i1,i2,i3,i4,i5,i6,i7) | i1 <- range (l1,u1),
                                i2 <- range (l2,u2),
                                i3 <- range (l3,u3),
                                i4 <- range (l4,u4),
                                i5 <- range (l5,u5),
                                i6 <- range (l6,u6),
                                i7 <- range (l7,u7)]

    unsafeIndex ((l1,l2,l3,l4,l5,l6,l7),(u1,u2,u3,u4,u5,u6,u7))
        (i1,i2,i3,i4,i5,i6,i7) =
      unsafeIndex (l7,u7) i7 + unsafeRangeSize (l7,u7) * (
      unsafeIndex (l6,u6) i6 + unsafeRangeSize (l6,u6) * (
      unsafeIndex (l5,u5) i5 + unsafeRangeSize (l5,u5) * (
      unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
      unsafeIndex (l1,u1) i1))))))

    inRange ((l1,l2,l3,l4,l5,l6,l7),(u1,u2,u3,u4,u5,u6,u7))
        (i1,i2,i3,i4,i5,i6,i7) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3 && inRange (l4,u4) i4 &&
      inRange (l5,u5) i5 && inRange (l6,u6) i6 &&
      inRange (l7,u7) i7

    -- Default method for index

instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7, Ix a8) =>
      Ix (a1,a2,a3,a4,a5,a6,a7,a8)  where
    range ((l1,l2,l3,l4,l5,l6,l7,l8),(u1,u2,u3,u4,u5,u6,u7,u8)) =
      [(i1,i2,i3,i4,i5,i6,i7,i8) | i1 <- range (l1,u1),
                                   i2 <- range (l2,u2),
                                   i3 <- range (l3,u3),
                                   i4 <- range (l4,u4),
                                   i5 <- range (l5,u5),
                                   i6 <- range (l6,u6),
                                   i7 <- range (l7,u7),
                                   i8 <- range (l8,u8)]

    unsafeIndex ((l1,l2,l3,l4,l5,l6,l7,l8),(u1,u2,u3,u4,u5,u6,u7,u8))
        (i1,i2,i3,i4,i5,i6,i7,i8) =
      unsafeIndex (l8,u8) i8 + unsafeRangeSize (l8,u8) * (
      unsafeIndex (l7,u7) i7 + unsafeRangeSize (l7,u7) * (
      unsafeIndex (l6,u6) i6 + unsafeRangeSize (l6,u6) * (
      unsafeIndex (l5,u5) i5 + unsafeRangeSize (l5,u5) * (
      unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
      unsafeIndex (l1,u1) i1)))))))

    inRange ((l1,l2,l3,l4,l5,l6,l7,l8),(u1,u2,u3,u4,u5,u6,u7,u8))
        (i1,i2,i3,i4,i5,i6,i7,i8) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3 && inRange (l4,u4) i4 &&
      inRange (l5,u5) i5 && inRange (l6,u6) i6 &&
      inRange (l7,u7) i7 && inRange (l8,u8) i8

    -- Default method for index

instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7, Ix a8, Ix a9) =>
      Ix (a1,a2,a3,a4,a5,a6,a7,a8,a9)  where
    range ((l1,l2,l3,l4,l5,l6,l7,l8,l9),(u1,u2,u3,u4,u5,u6,u7,u8,u9)) =
      [(i1,i2,i3,i4,i5,i6,i7,i8,i9) | i1 <- range (l1,u1),
                                      i2 <- range (l2,u2),
                                      i3 <- range (l3,u3),
                                      i4 <- range (l4,u4),
                                      i5 <- range (l5,u5),
                                      i6 <- range (l6,u6),
                                      i7 <- range (l7,u7),
                                      i8 <- range (l8,u8),
                                      i9 <- range (l9,u9)]

    unsafeIndex ((l1,l2,l3,l4,l5,l6,l7,l8,l9),(u1,u2,u3,u4,u5,u6,u7,u8,u9))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9) =
      unsafeIndex (l9,u9) i9 + unsafeRangeSize (l9,u9) * (
      unsafeIndex (l8,u8) i8 + unsafeRangeSize (l8,u8) * (
      unsafeIndex (l7,u7) i7 + unsafeRangeSize (l7,u7) * (
      unsafeIndex (l6,u6) i6 + unsafeRangeSize (l6,u6) * (
      unsafeIndex (l5,u5) i5 + unsafeRangeSize (l5,u5) * (
      unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
      unsafeIndex (l1,u1) i1))))))))

    inRange ((l1,l2,l3,l4,l5,l6,l7,l8,l9),(u1,u2,u3,u4,u5,u6,u7,u8,u9))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3 && inRange (l4,u4) i4 &&
      inRange (l5,u5) i5 && inRange (l6,u6) i6 &&
      inRange (l7,u7) i7 && inRange (l8,u8) i8 &&
      inRange (l9,u9) i9

    -- Default method for index

instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7, Ix a8, Ix a9,
           Ix aA) =>
      Ix (a1,a2,a3,a4,a5,a6,a7,a8,a9,aA)  where
    range ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA),(u1,u2,u3,u4,u5,u6,u7,u8,u9,uA)) =
      [(i1,i2,i3,i4,i5,i6,i7,i8,i9,iA) | i1 <- range (l1,u1),
                                         i2 <- range (l2,u2),
                                         i3 <- range (l3,u3),
                                         i4 <- range (l4,u4),
                                         i5 <- range (l5,u5),
                                         i6 <- range (l6,u6),
                                         i7 <- range (l7,u7),
                                         i8 <- range (l8,u8),
                                         i9 <- range (l9,u9),
                                         iA <- range (lA,uA)]

    unsafeIndex ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA),
                 (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9,iA) =
      unsafeIndex (lA,uA) iA + unsafeRangeSize (lA,uA) * (
      unsafeIndex (l9,u9) i9 + unsafeRangeSize (l9,u9) * (
      unsafeIndex (l8,u8) i8 + unsafeRangeSize (l8,u8) * (
      unsafeIndex (l7,u7) i7 + unsafeRangeSize (l7,u7) * (
      unsafeIndex (l6,u6) i6 + unsafeRangeSize (l6,u6) * (
      unsafeIndex (l5,u5) i5 + unsafeRangeSize (l5,u5) * (
      unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
      unsafeIndex (l1,u1) i1)))))))))

    inRange ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA),(u1,u2,u3,u4,u5,u6,u7,u8,u9,uA))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9,iA) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3 && inRange (l4,u4) i4 &&
      inRange (l5,u5) i5 && inRange (l6,u6) i6 &&
      inRange (l7,u7) i7 && inRange (l8,u8) i8 &&
      inRange (l9,u9) i9 && inRange (lA,uA) iA

    -- Default method for index

instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7, Ix a8, Ix a9,
           Ix aA, Ix aB) =>
      Ix (a1,a2,a3,a4,a5,a6,a7,a8,a9,aA,aB)  where
    range ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB),
           (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB)) =
      [(i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB) | i1 <- range (l1,u1),
                                            i2 <- range (l2,u2),
                                            i3 <- range (l3,u3),
                                            i4 <- range (l4,u4),
                                            i5 <- range (l5,u5),
                                            i6 <- range (l6,u6),
                                            i7 <- range (l7,u7),
                                            i8 <- range (l8,u8),
                                            i9 <- range (l9,u9),
                                            iA <- range (lA,uA),
                                            iB <- range (lB,uB)]

    unsafeIndex ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB),
                 (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB) =
      unsafeIndex (lB,uB) iB + unsafeRangeSize (lB,uB) * (
      unsafeIndex (lA,uA) iA + unsafeRangeSize (lA,uA) * (
      unsafeIndex (l9,u9) i9 + unsafeRangeSize (l9,u9) * (
      unsafeIndex (l8,u8) i8 + unsafeRangeSize (l8,u8) * (
      unsafeIndex (l7,u7) i7 + unsafeRangeSize (l7,u7) * (
      unsafeIndex (l6,u6) i6 + unsafeRangeSize (l6,u6) * (
      unsafeIndex (l5,u5) i5 + unsafeRangeSize (l5,u5) * (
      unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
      unsafeIndex (l1,u1) i1))))))))))

    inRange ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB),
             (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3 && inRange (l4,u4) i4 &&
      inRange (l5,u5) i5 && inRange (l6,u6) i6 &&
      inRange (l7,u7) i7 && inRange (l8,u8) i8 &&
      inRange (l9,u9) i9 && inRange (lA,uA) iA &&
      inRange (lB,uB) iB

    -- Default method for index

instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7, Ix a8, Ix a9,
           Ix aA, Ix aB, Ix aC) =>
      Ix (a1,a2,a3,a4,a5,a6,a7,a8,a9,aA,aB,aC)  where
    range ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB,lC),
           (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB,uC)) =
      [(i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB,iC) | i1 <- range (l1,u1),
                                               i2 <- range (l2,u2),
                                               i3 <- range (l3,u3),
                                               i4 <- range (l4,u4),
                                               i5 <- range (l5,u5),
                                               i6 <- range (l6,u6),
                                               i7 <- range (l7,u7),
                                               i8 <- range (l8,u8),
                                               i9 <- range (l9,u9),
                                               iA <- range (lA,uA),
                                               iB <- range (lB,uB),
                                               iC <- range (lC,uC)]

    unsafeIndex ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB,lC),
                 (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB,uC))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB,iC) =
      unsafeIndex (lC,uC) iC + unsafeRangeSize (lC,uC) * (
      unsafeIndex (lB,uB) iB + unsafeRangeSize (lB,uB) * (
      unsafeIndex (lA,uA) iA + unsafeRangeSize (lA,uA) * (
      unsafeIndex (l9,u9) i9 + unsafeRangeSize (l9,u9) * (
      unsafeIndex (l8,u8) i8 + unsafeRangeSize (l8,u8) * (
      unsafeIndex (l7,u7) i7 + unsafeRangeSize (l7,u7) * (
      unsafeIndex (l6,u6) i6 + unsafeRangeSize (l6,u6) * (
      unsafeIndex (l5,u5) i5 + unsafeRangeSize (l5,u5) * (
      unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
      unsafeIndex (l1,u1) i1)))))))))))

    inRange ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB,lC),
             (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB,uC))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB,iC) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3 && inRange (l4,u4) i4 &&
      inRange (l5,u5) i5 && inRange (l6,u6) i6 &&
      inRange (l7,u7) i7 && inRange (l8,u8) i8 &&
      inRange (l9,u9) i9 && inRange (lA,uA) iA &&
      inRange (lB,uB) iB && inRange (lC,uC) iC

    -- Default method for index

instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7, Ix a8, Ix a9,
           Ix aA, Ix aB, Ix aC, Ix aD) =>
      Ix (a1,a2,a3,a4,a5,a6,a7,a8,a9,aA,aB,aC,aD)  where
    range ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB,lC,lD),
           (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB,uC,uD)) =
      [(i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB,iC,iD) | i1 <- range (l1,u1),
                                                  i2 <- range (l2,u2),
                                                  i3 <- range (l3,u3),
                                                  i4 <- range (l4,u4),
                                                  i5 <- range (l5,u5),
                                                  i6 <- range (l6,u6),
                                                  i7 <- range (l7,u7),
                                                  i8 <- range (l8,u8),
                                                  i9 <- range (l9,u9),
                                                  iA <- range (lA,uA),
                                                  iB <- range (lB,uB),
                                                  iC <- range (lC,uC),
                                                  iD <- range (lD,uD)]

    unsafeIndex ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB,lC,lD),
                 (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB,uC,uD))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB,iC,iD) =
      unsafeIndex (lD,uD) iD + unsafeRangeSize (lD,uD) * (
      unsafeIndex (lC,uC) iC + unsafeRangeSize (lC,uC) * (
      unsafeIndex (lB,uB) iB + unsafeRangeSize (lB,uB) * (
      unsafeIndex (lA,uA) iA + unsafeRangeSize (lA,uA) * (
      unsafeIndex (l9,u9) i9 + unsafeRangeSize (l9,u9) * (
      unsafeIndex (l8,u8) i8 + unsafeRangeSize (l8,u8) * (
      unsafeIndex (l7,u7) i7 + unsafeRangeSize (l7,u7) * (
      unsafeIndex (l6,u6) i6 + unsafeRangeSize (l6,u6) * (
      unsafeIndex (l5,u5) i5 + unsafeRangeSize (l5,u5) * (
      unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
      unsafeIndex (l1,u1) i1))))))))))))

    inRange ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB,lC,lD),
             (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB,uC,uD))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB,iC,iD) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3 && inRange (l4,u4) i4 &&
      inRange (l5,u5) i5 && inRange (l6,u6) i6 &&
      inRange (l7,u7) i7 && inRange (l8,u8) i8 &&
      inRange (l9,u9) i9 && inRange (lA,uA) iA &&
      inRange (lB,uB) iB && inRange (lC,uC) iC &&
      inRange (lD,uD) iD

    -- Default method for index

instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7, Ix a8, Ix a9,
           Ix aA, Ix aB, Ix aC, Ix aD, Ix aE) =>
      Ix (a1,a2,a3,a4,a5,a6,a7,a8,a9,aA,aB,aC,aD,aE)  where
    range ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB,lC,lD,lE),
           (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB,uC,uD,uE)) =
      [(i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB,iC,iD,iE) | i1 <- range (l1,u1),
                                                     i2 <- range (l2,u2),
                                                     i3 <- range (l3,u3),
                                                     i4 <- range (l4,u4),
                                                     i5 <- range (l5,u5),
                                                     i6 <- range (l6,u6),
                                                     i7 <- range (l7,u7),
                                                     i8 <- range (l8,u8),
                                                     i9 <- range (l9,u9),
                                                     iA <- range (lA,uA),
                                                     iB <- range (lB,uB),
                                                     iC <- range (lC,uC),
                                                     iD <- range (lD,uD),
                                                     iE <- range (lE,uE)]

    unsafeIndex ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB,lC,lD,lE),
                 (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB,uC,uD,uE))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB,iC,iD,iE) =
      unsafeIndex (lE,uE) iE + unsafeRangeSize (lE,uE) * (
      unsafeIndex (lD,uD) iD + unsafeRangeSize (lD,uD) * (
      unsafeIndex (lC,uC) iC + unsafeRangeSize (lC,uC) * (
      unsafeIndex (lB,uB) iB + unsafeRangeSize (lB,uB) * (
      unsafeIndex (lA,uA) iA + unsafeRangeSize (lA,uA) * (
      unsafeIndex (l9,u9) i9 + unsafeRangeSize (l9,u9) * (
      unsafeIndex (l8,u8) i8 + unsafeRangeSize (l8,u8) * (
      unsafeIndex (l7,u7) i7 + unsafeRangeSize (l7,u7) * (
      unsafeIndex (l6,u6) i6 + unsafeRangeSize (l6,u6) * (
      unsafeIndex (l5,u5) i5 + unsafeRangeSize (l5,u5) * (
      unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
      unsafeIndex (l1,u1) i1)))))))))))))

    inRange ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB,lC,lD,lE),
             (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB,uC,uD,uE))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB,iC,iD,iE) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3 && inRange (l4,u4) i4 &&
      inRange (l5,u5) i5 && inRange (l6,u6) i6 &&
      inRange (l7,u7) i7 && inRange (l8,u8) i8 &&
      inRange (l9,u9) i9 && inRange (lA,uA) iA &&
      inRange (lB,uB) iB && inRange (lC,uC) iC &&
      inRange (lD,uD) iD && inRange (lE,uE) iE

    -- Default method for index

instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5, Ix a6, Ix a7, Ix a8, Ix a9,
           Ix aA, Ix aB, Ix aC, Ix aD, Ix aE, Ix aF) =>
      Ix (a1,a2,a3,a4,a5,a6,a7,a8,a9,aA,aB,aC,aD,aE,aF)  where
    range ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB,lC,lD,lE,lF),
           (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB,uC,uD,uE,uF)) =
      [(i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB,iC,iD,iE,iF) | i1 <- range (l1,u1),
                                                        i2 <- range (l2,u2),
                                                        i3 <- range (l3,u3),
                                                        i4 <- range (l4,u4),
                                                        i5 <- range (l5,u5),
                                                        i6 <- range (l6,u6),
                                                        i7 <- range (l7,u7),
                                                        i8 <- range (l8,u8),
                                                        i9 <- range (l9,u9),
                                                        iA <- range (lA,uA),
                                                        iB <- range (lB,uB),
                                                        iC <- range (lC,uC),
                                                        iD <- range (lD,uD),
                                                        iE <- range (lE,uE),
                                                        iF <- range (lF,uF)]

    unsafeIndex ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB,lC,lD,lE,lF),
                 (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB,uC,uD,uE,uF))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB,iC,iD,iE,iF) =
      unsafeIndex (lF,uF) iF + unsafeRangeSize (lF,uF) * (
      unsafeIndex (lE,uE) iE + unsafeRangeSize (lE,uE) * (
      unsafeIndex (lD,uD) iD + unsafeRangeSize (lD,uD) * (
      unsafeIndex (lC,uC) iC + unsafeRangeSize (lC,uC) * (
      unsafeIndex (lB,uB) iB + unsafeRangeSize (lB,uB) * (
      unsafeIndex (lA,uA) iA + unsafeRangeSize (lA,uA) * (
      unsafeIndex (l9,u9) i9 + unsafeRangeSize (l9,u9) * (
      unsafeIndex (l8,u8) i8 + unsafeRangeSize (l8,u8) * (
      unsafeIndex (l7,u7) i7 + unsafeRangeSize (l7,u7) * (
      unsafeIndex (l6,u6) i6 + unsafeRangeSize (l6,u6) * (
      unsafeIndex (l5,u5) i5 + unsafeRangeSize (l5,u5) * (
      unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
      unsafeIndex (l1,u1) i1))))))))))))))

    inRange ((l1,l2,l3,l4,l5,l6,l7,l8,l9,lA,lB,lC,lD,lE,lF),
             (u1,u2,u3,u4,u5,u6,u7,u8,u9,uA,uB,uC,uD,uE,uF))
        (i1,i2,i3,i4,i5,i6,i7,i8,i9,iA,iB,iC,iD,iE,iF) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3 && inRange (l4,u4) i4 &&
      inRange (l5,u5) i5 && inRange (l6,u6) i6 &&
      inRange (l7,u7) i7 && inRange (l8,u8) i8 &&
      inRange (l9,u9) i9 && inRange (lA,uA) iA &&
      inRange (lB,uB) iB && inRange (lC,uC) iC &&
      inRange (lD,uD) iD && inRange (lE,uE) iE &&
      inRange (lF,uF) iF

    -- Default method for index

instance MonadZip Complex where
  mzipWith = liftA2

instance MonadFix Complex where
  mfix f = (let a :+ _ = f a in a) :+ (let _ :+ a = f a in a)
# endif

# if !(MIN_VERSION_base(4,16,0))
instance Eq1 Complex where
    liftEq eq (x :+ y) (u :+ v) = eq x u && eq y v

instance Read1 Complex where
#  if MIN_VERSION_base(4,10,0)
    liftReadPrec rp _  = parens $ prec complexPrec $ do
        x <- step rp
        expectP (Symbol ":+")
        y <- step rp
        return (x :+ y)
      where
        complexPrec = 6

    liftReadListPrec = liftReadListPrecDefault
    liftReadList     = liftReadListDefault
#  else
    liftReadsPrec rdP _ p s = readParen (p > complexPrec) (\s' -> do
      (x, s'')     <- rdP (complexPrec+1) s'
      (":+", s''') <- lex s''
      (y, s'''')   <- rdP (complexPrec+1) s'''
      return (x :+ y, s'''')) s
      where
        complexPrec = 6
#  endif

instance Show1 Complex where
    liftShowsPrec sp _ d (x :+ y) = showParen (d > complexPrec) $
        sp (complexPrec+1) x . showString " :+ " . sp (complexPrec+1) y
      where
        complexPrec = 6

instance Eq a => Eq2 ((,,) a) where
    liftEq2 e1 e2 (u1, x1, y1) (v1, x2, y2) =
        u1 == v1 &&
        e1 x1 x2 && e2 y1 y2

instance Ord a => Ord2 ((,,) a) where
    liftCompare2 comp1 comp2 (u1, x1, y1) (v1, x2, y2) =
        compare u1 v1 `mappend`
        comp1 x1 x2 `mappend` comp2 y1 y2

instance Read a => Read2 ((,,) a) where
#  if MIN_VERSION_base(4,10,0)
    liftReadPrec2 rp1 _ rp2 _ = parens $ paren $ do
        x1 <- readPrec
        expectP (Punc ",")
        y1 <- rp1
        expectP (Punc ",")
        y2 <- rp2
        return (x1,y1,y2)

    liftReadListPrec2 = liftReadListPrec2Default
    liftReadList2     = liftReadList2Default
#  else
    liftReadsPrec2 rp1 _ rp2 _ _ = readParen False $ \ r ->
        [((e1,e2,e3), y) | ("(",s) <- lex r,
                           (e1,t)  <- readsPrec 0 s,
                           (",",u) <- lex t,
                           (e2,v)  <- rp1 0 u,
                           (",",w) <- lex v,
                           (e3,x)  <- rp2 0 w,
                           (")",y) <- lex x]
#  endif

instance Show a => Show2 ((,,) a) where
    liftShowsPrec2 sp1 _ sp2 _ _ (x1,y1,y2)
        = showChar '(' . showsPrec 0 x1
        . showChar ',' . sp1 0 y1
        . showChar ',' . sp2 0 y2
        . showChar ')'

instance (Eq a, Eq b) => Eq1 ((,,) a b) where
    liftEq = liftEq2 (==)

instance (Ord a, Ord b) => Ord1 ((,,) a b) where
    liftCompare = liftCompare2 compare

instance (Read a, Read b) => Read1 ((,,) a b) where
#  if MIN_VERSION_base(4,10,0)
    liftReadPrec = liftReadPrec2 readPrec readListPrec

    liftReadListPrec = liftReadListPrecDefault
    liftReadList     = liftReadListDefault
#  else
    liftReadsPrec = liftReadsPrec2 readsPrec readList
#  endif

instance (Show a, Show b) => Show1 ((,,) a b) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Eq a, Eq b) => Eq2 ((,,,) a b) where
    liftEq2 e1 e2 (u1, u2, x1, y1) (v1, v2, x2, y2) =
        u1 == v1 &&
        u2 == v2 &&
        e1 x1 x2 && e2 y1 y2

instance (Ord a, Ord b) => Ord2 ((,,,) a b) where
    liftCompare2 comp1 comp2 (u1, u2, x1, y1) (v1, v2, x2, y2) =
        compare u1 v1 `mappend`
        compare u2 v2 `mappend`
        comp1 x1 x2 `mappend` comp2 y1 y2

instance (Read a, Read b) => Read2 ((,,,) a b) where
#  if MIN_VERSION_base(4,10,0)
    liftReadPrec2 rp1 _ rp2 _ = parens $ paren $ do
        x1 <- readPrec
        expectP (Punc ",")
        x2 <- readPrec
        expectP (Punc ",")
        y1 <- rp1
        expectP (Punc ",")
        y2 <- rp2
        return (x1,x2,y1,y2)

    liftReadListPrec2 = liftReadListPrec2Default
    liftReadList2     = liftReadList2Default
#  else
    liftReadsPrec2 rp1 _ rp2 _ _ = readParen False $ \ r ->
        [((e1,e2,e3,e4), s9) | ("(",s1) <- lex r,
                               (e1,s2)  <- readsPrec 0 s1,
                               (",",s3) <- lex s2,
                               (e2,s4)  <- readsPrec 0 s3,
                               (",",s5) <- lex s4,
                               (e3,s6)  <- rp1 0 s5,
                               (",",s7) <- lex s6,
                               (e4,s8)  <- rp2 0 s7,
                               (")",s9) <- lex s8]
#  endif

instance (Show a, Show b) => Show2 ((,,,) a b) where
    liftShowsPrec2 sp1 _ sp2 _ _ (x1,x2,y1,y2)
        = showChar '(' . showsPrec 0 x1
        . showChar ',' . showsPrec 0 x2
        . showChar ',' . sp1 0 y1
        . showChar ',' . sp2 0 y2
        . showChar ')'

instance (Eq a, Eq b, Eq c) => Eq1 ((,,,) a b c) where
    liftEq = liftEq2 (==)

instance (Ord a, Ord b, Ord c) => Ord1 ((,,,) a b c) where
    liftCompare = liftCompare2 compare

instance (Read a, Read b, Read c) => Read1 ((,,,) a b c) where
#  if MIN_VERSION_base(4,10,0)
    liftReadPrec = liftReadPrec2 readPrec readListPrec

    liftReadListPrec = liftReadListPrecDefault
    liftReadList     = liftReadListDefault
#  else
    liftReadsPrec = liftReadsPrec2 readsPrec readList
#  endif

instance (Show a, Show b, Show c) => Show1 ((,,,) a b c) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

deriving instance Semigroup (f (g a)) => Semigroup (Compose f g a)
deriving instance Monoid    (f (g a)) => Monoid    (Compose f g a)

instance (Semigroup (f a), Semigroup (g a)) => Semigroup (Functor.Product f g a) where
    Functor.Pair x1 y1 <> Functor.Pair x2 y2 = Functor.Pair (x1 <> x2) (y1 <> y2)

instance (Monoid (f a), Monoid (g a)) => Monoid (Functor.Product f g a) where
    mempty = Functor.Pair mempty mempty
#  if !(MIN_VERSION_base(4,11,0))
    Functor.Pair x1 y1 `mappend` Functor.Pair x2 y2 = Functor.Pair (x1 `mappend` x2) (y1 `mappend` y2)
#  endif

#  if MIN_VERSION_base(4,15,0)
instance Enum a => Enum (Solo a) where
    succ (Solo a) = Solo (succ a)
    pred (Solo a) = Solo (pred a)

    toEnum x = Solo (toEnum x)

    fromEnum (Solo x) = fromEnum x
    enumFrom (Solo x) = [Solo a | a <- enumFrom x]
    enumFromThen (Solo x) (Solo y) =
      [Solo a | a <- enumFromThen x y]
    enumFromTo (Solo x) (Solo y) =
      [Solo a | a <- enumFromTo x y]
    enumFromThenTo (Solo x) (Solo y) (Solo z) =
      [Solo a | a <- enumFromThenTo x y z]

deriving instance Eq a => Eq (Solo a)
deriving instance Ord a => Ord (Solo a)
deriving instance Bounded a => Bounded (Solo a)

instance Ix a => Ix (Solo a) where -- as derived
    {-# SPECIALISE instance Ix (Solo Int) #-}

    {-# INLINE range #-}
    range (Solo l, Solo u) =
      [ Solo i | i <- range (l,u) ]

    {-# INLINE unsafeIndex #-}
    unsafeIndex (Solo l, Solo u) (Solo i) =
      unsafeIndex (l,u) i

    {-# INLINE inRange #-}
    inRange (Solo l, Solo u) (Solo i) =
      inRange (l, u) i

    -- Default method for index
#  endif
# endif

# if !(MIN_VERSION_base(4,16,1))
deriving instance Ix CChar
deriving instance Ix CSChar
deriving instance Ix CUChar
deriving instance Ix CShort
deriving instance Ix CUShort
deriving instance Ix CInt
deriving instance Ix CUInt
deriving instance Ix CLong
deriving instance Ix CULong
deriving instance Ix CLLong
deriving instance Ix CULLong
deriving instance Ix CPtrdiff
deriving instance Ix CSize
deriving instance Ix CWchar
deriving instance Ix CSigAtomic
deriving instance Ix CIntPtr
deriving instance Ix CUIntPtr
deriving instance Ix CIntMax
deriving instance Ix CUIntMax
#  if MIN_VERSION_base(4,10,0)
deriving instance Ix CBool
#  endif

#  if MIN_VERSION_base(4,10,0)
-- These are guarded on base-4.10.0 because that was the first version which
-- exported their constructors, which is necessary to use
-- GeneralizedNewtypeDeriving. See
-- https://gitlab.haskell.org/ghc/ghc/-/issues/11983.
deriving instance Ix WordPtr
deriving instance Ix IntPtr
#  endif

#  if defined(HTYPE_DEV_T)
deriving instance Ix CDev
#  endif
#  if defined(HTYPE_INO_T)
deriving instance Ix CIno
#  endif
#  if defined(HTYPE_MODE_T)
deriving instance Ix CMode
#  endif
#  if defined(HTYPE_OFF_T)
deriving instance Ix COff
#  endif
#  if defined(HTYPE_PID_T)
deriving instance Ix CPid
#  endif
#  if defined(HTYPE_SSIZE_T)
deriving instance Ix CSsize
#  endif
#  if defined(HTYPE_GID_T)
deriving instance Ix CGid
#  endif
#  if defined(HTYPE_NLINK_T)
deriving instance Ix CNlink
#  endif
#  if defined(HTYPE_UID_T)
deriving instance Ix CUid
#  endif
#  if defined(HTYPE_CC_T)
deriving instance Ix CCc
#  endif
#  if defined(HTYPE_SPEED_T)
deriving instance Ix CSpeed
#  endif
#  if defined(HTYPE_TCFLAG_T)
deriving instance Ix CTcflag
#  endif
#  if defined(HTYPE_RLIM_T)
deriving instance Ix CRLim
#  endif
deriving instance Ix Fd

#  if MIN_VERSION_base(4,10,0)
#   if defined(HTYPE_BLKSIZE_T)
deriving instance Ix CBlkSize
#   endif
#   if defined(HTYPE_BLKCNT_T)
deriving instance Ix CBlkCnt
#   endif
#   if defined(HTYPE_CLOCKID_T)
deriving instance Ix CClockId
#   endif
#   if defined(HTYPE_FSBLKCNT_T)
deriving instance Ix CFsBlkCnt
#   endif
#   if defined(HTYPE_FSFILCNT_T)
deriving instance Ix CFsFilCnt
#   endif
#   if defined(HTYPE_ID_T)
deriving instance Ix CId
#   endif
#   if defined(HTYPE_KEY_T)
deriving instance Ix CKey
#   endif
#   if defined(HTYPE_SOCKLEN_T)
deriving instance Ix CSocklen
#   endif
#   if defined(HTYPE_NFDS_T)
deriving instance Ix CNfds
#   endif
#  endif
# endif

# if !(MIN_VERSION_base(4,18,0))
instance Functor ((,,,,) a b c d) where
    fmap f (a, b, c, d, e) = (a, b, c, d, f e)
instance Functor ((,,,,,) a b c d e) where
    fmap fun (a, b, c, d, e, f) = (a, b, c, d, e, fun f)
instance Functor ((,,,,,,) a b c d e f) where
    fmap fun (a, b, c, d, e, f, g) = (a, b, c, d, e, f, fun g)

#  if !(MIN_VERSION_base(4,14,0)) || MIN_VERSION_base(4,15,0)
-- | Swaps @'succ'@ and @'pred'@ of the underlying type.
instance (Enum a, Bounded a, Eq a) => Enum (Down a) where
    succ = fmap pred
    pred = fmap succ

    -- Here we use the fact that 'comparing (complement @Int)' behaves
    -- as an order-swapping `compare @Int`.
    fromEnum (Down x) = complement $ fromEnum x
    toEnum = Down . toEnum . complement

    enumFrom (Down x)
        | x == minBound
        = [Down x] -- We can't rely on 'enumFromThen _ (pred @a minBound)` behaving nicely,
                   -- since 'enumFromThen _' might be strict and 'pred minBound' might throw
        | otherwise
        = coerce $ enumFromThen x (pred x)
    enumFromThen (Down x) (Down y) = coerce $ enumFromThen x y
#  endif

#  if MIN_VERSION_base(4,17,0)
instance (Generic1 f, Eq (Rep1 f a)) => Eq (Generically1 f a) where
   Generically1 x == Generically1 y = from1 x == from1 y
   Generically1 x /= Generically1 y = from1 x /= from1 y

instance (Generic1 f, Ord (Rep1 f a)) => Ord (Generically1 f a) where
   Generically1 x `compare` Generically1 y = from1 x `compare` from1 y
#  endif
# endif

# if !(MIN_VERSION_base(4,19,0))
deriving instance Enum (f (g a)) => Enum (Compose f g a)
deriving instance Bounded (f (g a)) => Bounded (Compose f g a)
deriving instance Num (f (g a)) => Num (Compose f g a)

-- In base-4.18.0.0, the Ord instance for Compose was simplified to:
--
--   instance Ord (f (g a)) => Ord (Compose f g a)
--
-- Before that, the Ord instance was defined as:
--
--   instance (Ord1 f, Ord1 g, Ord a) => Ord (Compose f g a)
--
-- This makes deriving Real and Integral instances slightly more complicated for
-- these older versions of base, as there are no Real1 or Integral1 classes. We
-- opt for making the instance contexts more complicated instead.
#  if MIN_VERSION_base(4,18,0)
deriving instance Real (f (g a)) => Real (Compose f g a)
deriving instance Integral (f (g a)) => Integral (Compose f g a)
#  else
deriving instance (Real (f (g a)), Ord1 f, Ord1 g, Ord a) => Real (Compose f g a)
deriving instance (Integral (f (g a)), Ord1 f, Ord1 g, Ord a) => Integral (Compose f g a)
#  endif

#  if MIN_VERSION_base(4,18,0)
instance Eq (SChar c) where
  _ == _ = True
instance Ord (SChar c) where
  compare _ _ = EQ

instance Eq (SNat n) where
  _ == _ = True
instance Ord (SNat n) where
  compare _ _ = EQ

instance Eq (SSymbol s) where
  _ == _ = True
instance Ord (SSymbol s) where
  compare _ _ = EQ
#  endif
# endif

# if !(MIN_VERSION_base(4,20,0))
deriving instance Fractional (f (g a)) => Fractional (Compose f g a)
deriving instance Floating (f (g a)) => Floating (Compose f g a)

-- RealFrac and RealFloat both have Ord as a superclass. For the reasons stated
-- above (near the Real/Integral instances for Compose), these
-- RealFrace/RealFloat instances are slightly more complicated for older
-- versions of base.
#  if MIN_VERSION_base(4,18,0)
deriving instance RealFrac (f (g a)) => RealFrac (Compose f g a)
deriving instance RealFloat (f (g a)) => RealFloat (Compose f g a)
#  else
deriving instance (RealFrac (f (g a)), Ord1 f, Ord1 g, Ord a) => RealFrac (Compose f g a)
deriving instance (RealFloat (f (g a)), Ord1 f, Ord1 g, Ord a) => RealFloat (Compose f g a)
#  endif
# endif

# if !(MIN_VERSION_base(4,21,0))
instance Monoid a => MonadFix ((,) a) where
    -- See the CLC proposal thread for discussion and proofs of the laws: https://github.com/haskell/core-libraries-committee/issues/238
    mfix f = let a = f (snd a) in a

instance Eq1 V1 where
  liftEq _ = \_ _ -> True

instance Ord1 V1 where
  liftCompare _ = \_ _ -> EQ

instance Show1 V1 where
  liftShowsPrec _ _ _ = \_ -> showString "V1"

instance Read1 V1 where
  liftReadsPrec _ _ = readPrec_to_S pfail

#  if MIN_VERSION_base(4,10,0)
  liftReadListPrec  = liftReadListPrecDefault
  liftReadList      = liftReadListDefault
#  endif

instance Eq1 U1 where
  liftEq _ = \_ _ -> True

instance Ord1 U1 where
  liftCompare _ = \_ _ -> EQ

instance Show1 U1 where
  liftShowsPrec _ _ _ = \U1 -> showString "U1"

instance Read1 U1 where
#  if MIN_VERSION_base(4,10,0)
  liftReadPrec _ _ =
    parens (expectP (Ident "U1") *> pure U1)

  liftReadListPrec  = liftReadListPrecDefault
  liftReadList      = liftReadListDefault
#  else
  liftReadsPrec _ _ =
    readPrec_to_S $
    parens (expectP (Ident "U1") *> pure U1)
#  endif

instance Eq1 Par1 where
  liftEq eq = \(Par1 a) (Par1 a') -> eq a a'

instance Ord1 Par1 where
  liftCompare cmp = \(Par1 a) (Par1 a') -> cmp a a'

instance Show1 Par1 where
  liftShowsPrec sp _ d = \(Par1 { unPar1 = a }) ->
    showsSingleFieldRecordWith sp "Par1" "unPar1" d a

instance Read1 Par1 where
#  if MIN_VERSION_base(4,10,0)
  liftReadPrec rp _ =
    readSingleFieldRecordWith rp "Par1" "unPar1" Par1

  liftReadListPrec  = liftReadListPrecDefault
  liftReadList      = liftReadListDefault
#  else
  liftReadsPrec rp _ =
    readPrec_to_S $
    readSingleFieldRecordWith (readS_to_Prec rp) "Par1" "unPar1" Par1
#  endif

instance Eq1 f => Eq1 (Rec1 f) where
  liftEq eq = \(Rec1 a) (Rec1 a') -> liftEq eq a a'

instance Ord1 f => Ord1 (Rec1 f) where
  liftCompare cmp = \(Rec1 a) (Rec1 a') -> liftCompare cmp a a'

instance Show1 f => Show1 (Rec1 f) where
  liftShowsPrec sp sl d = \(Rec1 { unRec1 = a }) ->
    showsSingleFieldRecordWith (liftShowsPrec sp sl) "Rec1" "unRec1" d a

instance Read1 f => Read1 (Rec1 f) where
#  if MIN_VERSION_base(4,10,0)
  liftReadPrec rp rl =
    readSingleFieldRecordWith (liftReadPrec rp rl) "Rec1" "unRec1" Rec1

  liftReadListPrec   = liftReadListPrecDefault
  liftReadList       = liftReadListDefault
#  else
  liftReadsPrec rp rl =
    readPrec_to_S $
    readSingleFieldRecordWith
      (readS_to_Prec (liftReadsPrec rp rl))
      "Rec1"
      "unRec1"
      Rec1
#  endif

instance Eq c => Eq1 (K1 i c) where
  liftEq _ = \(K1 a) (K1 a') -> a == a'

instance Ord c => Ord1 (K1 i c) where
  liftCompare _ = \(K1 a) (K1 a') -> compare a a'

instance Show c => Show1 (K1 i c) where
  liftShowsPrec _ _ d = \(K1 { unK1 = a }) ->
    showsSingleFieldRecordWith showsPrec "K1" "unK1" d a

instance Read c => Read1 (K1 i c) where
#  if MIN_VERSION_base(4,10,0)
  liftReadPrec _ _ = readData $
    readSingleFieldRecordWith readPrec "K1" "unK1" K1

  liftReadListPrec  = liftReadListPrecDefault
  liftReadList      = liftReadListDefault
#  else
  liftReadsPrec _ _ =
    readPrec_to_S $
    readData $
    readSingleFieldRecordWith readPrec "K1" "unK1" K1
#  endif

instance Eq1 f => Eq1 (M1 i c f) where
  liftEq eq = \(M1 a) (M1 a') -> liftEq eq a a'

instance Ord1 f => Ord1 (M1 i c f) where
  liftCompare cmp = \(M1 a) (M1 a') -> liftCompare cmp a a'

instance Show1 f => Show1 (M1 i c f) where
  liftShowsPrec sp sl d = \(M1 { unM1 = a }) ->
    showsSingleFieldRecordWith (liftShowsPrec sp sl) "M1" "unM1" d a

instance Read1 f => Read1 (M1 i c f) where
#  if MIN_VERSION_base(4,10,0)
  liftReadPrec rp rl = readData $
    readSingleFieldRecordWith (liftReadPrec rp rl) "M1" "unM1" M1

  liftReadListPrec  = liftReadListPrecDefault
  liftReadList      = liftReadListDefault
#  else
  liftReadsPrec rp rl =
    readPrec_to_S $
    readData $
    readSingleFieldRecordWith
      (readS_to_Prec (liftReadsPrec rp rl))
      "M1"
      "unM1"
      M1
#  endif

instance (Eq1 f, Eq1 g) => Eq1 (f :+: g) where
  liftEq eq = \lhs rhs -> case (lhs, rhs) of
    (L1 a, L1 a') -> liftEq eq a a'
    (R1 b, R1 b') -> liftEq eq b b'
    _           -> False

instance (Ord1 f, Ord1 g) => Ord1 (f :+: g) where
  liftCompare cmp = \lhs rhs -> case (lhs, rhs) of
    (L1 _, R1 _)  -> LT
    (R1 _, L1 _)  -> GT
    (L1 a, L1 a') -> liftCompare cmp a a'
    (R1 b, R1 b') -> liftCompare cmp b b'

instance (Show1 f, Show1 g) => Show1 (f :+: g) where
  liftShowsPrec sp sl d = \x -> case x of
    L1 a -> showsUnaryWith (liftShowsPrec sp sl) "L1" d a
    R1 b -> showsUnaryWith (liftShowsPrec sp sl) "R1" d b

instance (Read1 f, Read1 g) => Read1 (f :+: g) where
#  if MIN_VERSION_base(4,10,0)
  liftReadPrec rp rl = readData $
    readUnaryWith (liftReadPrec rp rl) "L1" L1 <|>
    readUnaryWith (liftReadPrec rp rl) "R1" R1

  liftReadListPrec  = liftReadListPrecDefault
  liftReadList      = liftReadListDefault
#  else
  liftReadsPrec rp rl =
    readPrec_to_S $
    readData $
    readUnaryWith (readS_to_Prec (liftReadsPrec rp rl)) "L1" L1 <|>
    readUnaryWith (readS_to_Prec (liftReadsPrec rp rl)) "R1" R1
#  endif

instance (Eq1 f, Eq1 g) => Eq1 (f :*: g) where
  liftEq eq = \(f :*: g) (f' :*: g') -> liftEq eq f f' && liftEq eq g g'

instance (Ord1 f, Ord1 g) => Ord1 (f :*: g) where
  liftCompare cmp = \(f :*: g) (f' :*: g') -> liftCompare cmp f f' <> liftCompare cmp g g'

instance (Show1 f, Show1 g) => Show1 (f :*: g) where
  liftShowsPrec sp sl d = \(a :*: b) ->
    showsBinaryOpWith
      (liftShowsPrec sp sl)
      (liftShowsPrec sp sl)
      7
      ":*:"
      d
      a
      b

instance (Read1 f, Read1 g) => Read1 (f :*: g) where
#  if MIN_VERSION_base(4,10,0)
  liftReadPrec rp rl = parens $ prec 6 $
    readBinaryOpWith (liftReadPrec rp rl) (liftReadPrec rp rl) ":*:" (:*:)

  liftReadListPrec  = liftReadListPrecDefault
  liftReadList      = liftReadListDefault
#  else
  liftReadsPrec rp rl =
    readPrec_to_S $
    parens $ prec 6 $
    readBinaryOpWith
      (readS_to_Prec (liftReadsPrec rp rl))
      (readS_to_Prec (liftReadsPrec rp rl))
      ":*:"
      (:*:)
#  endif

instance (Eq1 f, Eq1 g) => Eq1 (f :.: g) where
  liftEq eq = \(Comp1 a) (Comp1 a') -> liftEq (liftEq eq) a a'

instance (Ord1 f, Ord1 g) => Ord1 (f :.: g) where
  liftCompare cmp = \(Comp1 a) (Comp1 a') -> liftCompare (liftCompare cmp) a a'

instance (Show1 f, Show1 g) => Show1 (f :.: g) where
  liftShowsPrec sp sl d = \(Comp1 { unComp1 = a }) ->
    showsSingleFieldRecordWith
      (liftShowsPrec (liftShowsPrec sp sl) (liftShowList sp sl))
      "Comp1"
      "unComp1"
      d
      a

instance (Read1 f, Read1 g) => Read1 (f :.: g) where
#  if MIN_VERSION_base(4,10,0)
  liftReadPrec rp rl = readData $
    readSingleFieldRecordWith
      (liftReadPrec (liftReadPrec rp rl) (liftReadListPrec rp rl))
      "Comp1"
      "unComp1"
      Comp1

  liftReadListPrec  = liftReadListPrecDefault
  liftReadList      = liftReadListDefault
#  else
  liftReadsPrec rp rl =
    readPrec_to_S $
    readData $
    readSingleFieldRecordWith
      (readS_to_Prec (liftReadsPrec (liftReadsPrec rp rl) (liftReadList rp rl)))
      "Comp1"
      "unComp1"
      Comp1
#  endif

instance Eq1 UAddr where
  -- NB cannot use eqAddr# because its module isn't safe
  liftEq _ = \(UAddr a) (UAddr b) -> UAddr a == UAddr b

instance Ord1 UAddr where
  liftCompare _ = \(UAddr a) (UAddr b) -> compare (UAddr a) (UAddr b)

instance Show1 UAddr where
  liftShowsPrec _ _ = showsPrec

-- NB no Read1 for URec (Ptr ()) because there's no Read for Ptr.

instance Eq1 UChar where
  liftEq _ = \(UChar a) (UChar b) -> UChar a == UChar b

instance Ord1 UChar where
  liftCompare _ = \(UChar a) (UChar b) -> compare (UChar a) (UChar b)

instance Show1 UChar where
  liftShowsPrec _ _ = showsPrec

instance Eq1 UDouble where
  liftEq _ = \(UDouble a) (UDouble b) -> UDouble a == UDouble b

instance Ord1 UDouble where
  liftCompare _ = \(UDouble a) (UDouble b) -> compare (UDouble a) (UDouble b)

instance Show1 UDouble where
  liftShowsPrec _ _ = showsPrec

instance Eq1 UFloat where
  liftEq _ = \(UFloat a) (UFloat b) -> UFloat a == UFloat b

instance Ord1 UFloat where
  liftCompare _ = \(UFloat a) (UFloat b) -> compare (UFloat a) (UFloat b)

instance Show1 UFloat where
  liftShowsPrec _ _ = showsPrec

instance Eq1 UInt where
  liftEq _ = \(UInt a) (UInt b) -> UInt a == UInt b

instance Ord1 UInt where
  liftCompare _ = \(UInt a) (UInt b) -> compare (UInt a) (UInt b)

instance Show1 UInt where
  liftShowsPrec _ _ = showsPrec

instance Eq1 UWord where
  liftEq _ = \(UWord a) (UWord b) -> UWord a == UWord b

instance Ord1 UWord where
  liftCompare _ = \(UWord a) (UWord b) -> compare (UWord a) (UWord b)

instance Show1 UWord where
  liftShowsPrec _ _ = showsPrec

readSingleFieldRecordWith :: ReadPrec a -> String -> String -> (a -> t) -> ReadPrec t
readSingleFieldRecordWith rp name field cons = parens $ prec 11 $ do
  expectP $ Ident name
  expectP $ Punc "{"
  x <- readField field $ reset rp
  expectP $ Punc "}"
  pure $ cons x

readBinaryOpWith
  :: ReadPrec a
  -> ReadPrec b
  -> String
  -> (a -> b -> t)
  -> ReadPrec t
readBinaryOpWith rp1 rp2 name cons =
  cons <$> step rp1 <* expectP (Symbol name) <*> step rp2

#  if !(MIN_VERSION_base(4,10,0))
readData :: ReadPrec a -> ReadPrec a
readData reader = parens $ prec 10 reader

readUnaryWith :: ReadPrec a -> String -> (a -> t) -> ReadPrec t
readUnaryWith rp name cons = do
    expectP $ Ident name
    x <- step rp
    return $ cons x
#  endif

#  if !(MIN_VERSION_base(4,11,0))
readField :: String -> ReadPrec a -> ReadPrec a
readField fieldName readVal = do
        expectP (Ident fieldName)
        expectP (Punc "=")
        readVal
{-# NOINLINE readField #-}
#  endif

showsSingleFieldRecordWith :: (Int -> a -> ShowS) -> String -> String -> Int -> a -> ShowS
showsSingleFieldRecordWith sp name field d x =
  showParen (d > appPrec) $
    showString name . showString " {" . showString field . showString " = " . sp 0 x . showChar '}'

showsBinaryOpWith
  :: (Int -> a -> ShowS)
  -> (Int -> b -> ShowS)
  -> Int
  -> String
  -> Int
  -> a
  -> b
  -> ShowS
showsBinaryOpWith sp1 sp2 opPrec name d x y = showParen (d >= opPrec) $
  sp1 opPrec x . showChar ' ' . showString name . showChar ' ' . sp2 opPrec y

instance Show (UAddr p) where
  -- This Show instance would be equivalent to what deriving Show would generate,
  -- but because deriving Show doesn't support Addr# fields we define it manually.
  showsPrec d (UAddr x) =
    showParen (d > appPrec)
      (\y -> showString "UAddr {uAddr# = " (showsPrec 0 (Ptr x) (showChar '}' y)))
# endif
#endif
