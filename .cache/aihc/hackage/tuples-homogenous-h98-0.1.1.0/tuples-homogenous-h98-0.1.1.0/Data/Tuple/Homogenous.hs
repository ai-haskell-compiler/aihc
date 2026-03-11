-- | Provides wrappers for homogenous tuples defined as
--
-- @
-- newtype TupleN a = TupleN (a,...,a)
-- @
--
-- together with helper methods
--
-- @
-- tupleN :: a -> ... -> a -> TupleN a
-- @
--
-- and instances for
--
--   * 'Functor' applies a given function to all elements of a tuple.
--
--   * 'Applicative' zips two tuples, applying /i/-th function of the first to
--     /i/-th element of the second.
--
--   * 'Monad' where /i/-th element of @x >>= f@ is the result of applying @f@
--     to the /i/-th element of @x@ and taking its /i/-th result. In other words,
--     @join :: Tupple N (TuppleN a) -> TuppleN a@ returns the diagonal of the
--     /NxN/ matrix.
--
--   * 'Foldable' and 'Traversable' folds\/traverses over the /N/ elements.
--
module Data.Tuple.Homogenous where

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.Monoid

newtype Tuple0 a = Tuple0 { untuple0 :: () }
  deriving (Show, Read, Ord, Eq, Bounded)

tuple0 :: Tuple0 a
tuple0 = Tuple0 ()
{-# INLINE tuple0 #-}

instance Functor Tuple0 where
    fmap _ _ = tuple0
    {-# INLINE fmap #-}
instance Applicative Tuple0 where
    pure _ = tuple0
    {-# INLINE pure #-}
    _ <*> _ = tuple0
    {-# INLINE (<*>) #-}
    _  *> _ = tuple0
    {-# INLINE (*>) #-}
    _ <*  _ = tuple0
    {-# INLINE (<*) #-}
instance Monad Tuple0 where
    return = pure
    {-# INLINE return #-}
    _ >>= _ = tuple0
    {-# INLINE (>>=) #-}
    (>>)    = (*>)
    {-# INLINE (>>) #-}
instance Foldable Tuple0 where
    foldMap _ _ = mempty
    {-# INLINE foldMap #-}
instance Traversable Tuple0 where
    traverse _ _ = pure tuple0

-- -----------------------------------------------------------------

-- | This @newtype@ intentionally omits the @untuple1@ accessor as there is no
-- 1-tuple in Haskell.
newtype Tuple1 a = Tuple1 a
  deriving (Show, Read, Ord, Eq, Bounded)

tuple1 :: a -> Tuple1 a
tuple1 = Tuple1
{-# INLINE tuple1 #-}

instance Functor Tuple1 where
    fmap f (Tuple1 x) = Tuple1 (f x)
    {-# INLINE fmap #-}
instance Applicative Tuple1 where
    pure = tuple1
    {-# INLINE pure #-}
    (Tuple1 f) <*> (Tuple1 k) = tuple1 (f k)
    {-# INLINE (<*>) #-}
    _  *> k = k
    {-# INLINE (*>) #-}
    k <*  _ = k
    {-# INLINE (<*) #-}
instance Monad Tuple1 where
    return = pure
    {-# INLINE return #-}
    (Tuple1 k) >>= f = f k
    {-# INLINE (>>=) #-}
    (>>)    = (*>)
    {-# INLINE (>>) #-}
instance Foldable Tuple1 where
    foldMap f (Tuple1 x) = f x
    {-# INLINE foldMap #-}
instance Traversable Tuple1 where
    traverse f (Tuple1 x) = Tuple1 <$> f x

-- Auto-generated: --------------------------------------------------------

newtype Tuple2 a
  = Tuple2 {untuple2 :: ((a, a))}
    deriving (Eq, Ord, Bounded, Show, Read)
tuple2 x_0 x_1 = Tuple2 (x_0, x_1)
{-# INLINE tuple2 #-}
instance Functor Tuple2
    where fmap f (Tuple2 (x1, x2)) = Tuple2 (f x1, f x2)
          {-# INLINABLE fmap #-}
instance Applicative Tuple2
    where pure x = Tuple2 (x, x)
          {-# INLINE pure #-}
          (<*>) (Tuple2 (f1, f2)) (Tuple2 (x1,
                                                             x2)) = Tuple2 (f1 x1, f2 x2)
          {-# INLINEABLE (<*>) #-}
          (<*) x _ = x
          {-# INLINE (<*) #-}
          (*>) _ y = y
          {-# INLINE (*>) #-}
instance Monad Tuple2
    where return = pure
          {-# INLINE return #-}
          (>>=) (Tuple2 (x1, x2)) f = Tuple2 (case f x1 of
                                                         Tuple2 (y, _) -> y,
                                                     case f x2 of
                                                         Tuple2 (_, y) -> y)
          {-# INLINABLE (>>=) #-}
          (>>) = (*>)
          {-# INLINE (>>) #-}
instance Foldable Tuple2
    where foldr f z (Tuple2 (x1, x2)) = f x1 (f x2 z)
          {-# INLINABLE foldr #-}
          foldl f z (Tuple2 (x1, x2)) = f (f z x1) x2
          {-# INLINABLE foldl #-}
instance Traversable Tuple2
    where traverse f (Tuple2 (x1,
                                               x2)) = (<*>) (fmap tuple2 (f x1)) (f x2)
          {-# INLINABLE traverse #-}
newtype Tuple3 a
  = Tuple3 {untuple3 :: ((a, a, a))}
    deriving (Eq, Ord, Bounded, Show, Read)
tuple3 x_2 x_3 x_4 = Tuple3 (x_2, x_3, x_4)
{-# INLINE tuple3 #-}
instance Functor Tuple3
    where fmap f (Tuple3 (x1, x2, x3)) = Tuple3 (f x1,
                                                          f x2,
                                                          f x3)
          {-# INLINABLE fmap #-}
instance Applicative Tuple3
    where pure x = Tuple3 (x, x, x)
          {-# INLINE pure #-}
          (<*>) (Tuple3 (f1, f2, f3)) (Tuple3 (x1,
                                                                 x2,
                                                                 x3)) = Tuple3 (f1 x1, f2 x2, f3 x3)
          {-# INLINEABLE (<*>) #-}
          (<*) x _ = x
          {-# INLINE (<*) #-}
          (*>) _ y = y
          {-# INLINE (*>) #-}
instance Monad Tuple3
    where return = pure
          {-# INLINE return #-}
          (>>=) (Tuple3 (x1, x2, x3)) f = Tuple3 (case f x1 of
                                                             Tuple3 (y, _, _) -> y,
                                                         case f x2 of
                                                             Tuple3 (_, y, _) -> y,
                                                         case f x3 of
                                                             Tuple3 (_, _, y) -> y)
          {-# INLINABLE (>>=) #-}
          (>>) = (*>)
          {-# INLINE (>>) #-}
instance Foldable Tuple3
    where foldr f z (Tuple3 (x1,
                                           x2,
                                           x3)) = f x1 (f x2 (f x3 z))
          {-# INLINABLE foldr #-}
          foldl f z (Tuple3 (x1,
                                           x2,
                                           x3)) = f (f (f z x1) x2) x3
          {-# INLINABLE foldl #-}
instance Traversable Tuple3
    where traverse f (Tuple3 (x1,
                                               x2,
                                               x3)) = (<*>) ((<*>) (fmap tuple3 (f x1)) (f x2)) (f x3)
          {-# INLINABLE traverse #-}
newtype Tuple4 a
  = Tuple4 {untuple4 :: ((a, a, a, a))}
    deriving (Eq, Ord, Bounded, Show, Read)
tuple4 x_5 x_6 x_7 x_8 = Tuple4 (x_5, x_6, x_7, x_8)
{-# INLINE tuple4 #-}
instance Functor Tuple4
    where fmap f (Tuple4 (x1, x2, x3, x4)) = Tuple4 (f x1,
                                                              f x2,
                                                              f x3,
                                                              f x4)
          {-# INLINABLE fmap #-}
instance Applicative Tuple4
    where pure x = Tuple4 (x, x, x, x)
          {-# INLINE pure #-}
          (<*>) (Tuple4 (f1, f2, f3, f4)) (Tuple4 (x1,
                                                                     x2,
                                                                     x3,
                                                                     x4)) = Tuple4 (f1 x1,
                                                                                    f2 x2,
                                                                                    f3 x3,
                                                                                    f4 x4)
          {-# INLINEABLE (<*>) #-}
          (<*) x _ = x
          {-# INLINE (<*) #-}
          (*>) _ y = y
          {-# INLINE (*>) #-}
instance Monad Tuple4
    where return = pure
          {-# INLINE return #-}
          (>>=) (Tuple4 (x1, x2, x3, x4)) f = Tuple4 (case f x1 of
                                                                 Tuple4 (y, _, _, _) -> y,
                                                             case f x2 of
                                                                 Tuple4 (_, y, _, _) -> y,
                                                             case f x3 of
                                                                 Tuple4 (_, _, y, _) -> y,
                                                             case f x4 of
                                                                 Tuple4 (_, _, _, y) -> y)
          {-# INLINABLE (>>=) #-}
          (>>) = (*>)
          {-# INLINE (>>) #-}
instance Foldable Tuple4
    where foldr f z (Tuple4 (x1,
                                           x2,
                                           x3,
                                           x4)) = f x1 (f x2 (f x3 (f x4 z)))
          {-# INLINABLE foldr #-}
          foldl f z (Tuple4 (x1,
                                           x2,
                                           x3,
                                           x4)) = f (f (f (f z x1) x2) x3) x4
          {-# INLINABLE foldl #-}
instance Traversable Tuple4
    where traverse f (Tuple4 (x1,
                                               x2,
                                               x3,
                                               x4)) = (<*>) ((<*>) ((<*>) (fmap tuple4 (f x1)) (f x2)) (f x3)) (f x4)
          {-# INLINABLE traverse #-}
newtype Tuple5 a
  = Tuple5 {untuple5 :: ((a, a, a, a, a))}
    deriving (Eq, Ord, Bounded, Show, Read)
tuple5 x_9 x_10 x_11 x_12 x_13 = Tuple5 (x_9,
                                         x_10,
                                         x_11,
                                         x_12,
                                         x_13)
{-# INLINE tuple5 #-}
instance Functor Tuple5
    where fmap f (Tuple5 (x1, x2, x3, x4, x5)) = Tuple5 (f x1,
                                                                  f x2,
                                                                  f x3,
                                                                  f x4,
                                                                  f x5)
          {-# INLINABLE fmap #-}
instance Applicative Tuple5
    where pure x = Tuple5 (x, x, x, x, x)
          {-# INLINE pure #-}
          (<*>) (Tuple5 (f1, f2, f3, f4, f5)) (Tuple5 (x1,
                                                                         x2,
                                                                         x3,
                                                                         x4,
                                                                         x5)) = Tuple5 (f1 x1,
                                                                                        f2 x2,
                                                                                        f3 x3,
                                                                                        f4 x4,
                                                                                        f5 x5)
          {-# INLINEABLE (<*>) #-}
          (<*) x _ = x
          {-# INLINE (<*) #-}
          (*>) _ y = y
          {-# INLINE (*>) #-}
instance Monad Tuple5
    where return = pure
          {-# INLINE return #-}
          (>>=) (Tuple5 (x1, x2, x3, x4, x5)) f = Tuple5 (case f x1 of
                                                                     Tuple5 (y, _, _, _, _) -> y,
                                                                 case f x2 of
                                                                     Tuple5 (_, y, _, _, _) -> y,
                                                                 case f x3 of
                                                                     Tuple5 (_, _, y, _, _) -> y,
                                                                 case f x4 of
                                                                     Tuple5 (_, _, _, y, _) -> y,
                                                                 case f x5 of
                                                                     Tuple5 (_, _, _, _, y) -> y)
          {-# INLINABLE (>>=) #-}
          (>>) = (*>)
          {-# INLINE (>>) #-}
instance Foldable Tuple5
    where foldr f z (Tuple5 (x1,
                                           x2,
                                           x3,
                                           x4,
                                           x5)) = f x1 (f x2 (f x3 (f x4 (f x5 z))))
          {-# INLINABLE foldr #-}
          foldl f z (Tuple5 (x1,
                                           x2,
                                           x3,
                                           x4,
                                           x5)) = f (f (f (f (f z x1) x2) x3) x4) x5
          {-# INLINABLE foldl #-}
instance Traversable Tuple5
    where traverse f (Tuple5 (x1,
                                               x2,
                                               x3,
                                               x4,
                                               x5)) = (<*>) ((<*>) ((<*>) ((<*>) (fmap tuple5 (f x1)) (f x2)) (f x3)) (f x4)) (f x5)
          {-# INLINABLE traverse #-}
newtype Tuple6 a
  = Tuple6 {untuple6 :: ((a, a, a, a, a, a))}
    deriving (Eq, Ord, Bounded, Show, Read)
tuple6 x_14 x_15 x_16 x_17 x_18 x_19 = Tuple6 (x_14,
                                               x_15,
                                               x_16,
                                               x_17,
                                               x_18,
                                               x_19)
{-# INLINE tuple6 #-}
instance Functor Tuple6
    where fmap f (Tuple6 (x1,
                                   x2,
                                   x3,
                                   x4,
                                   x5,
                                   x6)) = Tuple6 (f x1, f x2, f x3, f x4, f x5, f x6)
          {-# INLINABLE fmap #-}
instance Applicative Tuple6
    where pure x = Tuple6 (x, x, x, x, x, x)
          {-# INLINE pure #-}
          (<*>) (Tuple6 (f1,
                                           f2,
                                           f3,
                                           f4,
                                           f5,
                                           f6)) (Tuple6 (x1, x2, x3, x4, x5, x6)) = Tuple6 (f1 x1,
                                                                                            f2 x2,
                                                                                            f3 x3,
                                                                                            f4 x4,
                                                                                            f5 x5,
                                                                                            f6 x6)
          {-# INLINABLE (<*>) #-}
          (<*) x _ = x
          {-# INLINE (<*) #-}
          (*>) _ y = y
          {-# INLINE (*>) #-}
instance Monad Tuple6
    where return = pure
          {-# INLINE return #-}
          (>>=) (Tuple6 (x1,
                                x2,
                                x3,
                                x4,
                                x5,
                                x6)) f = Tuple6 (case f x1 of
                                                     Tuple6 (y, _, _, _, _, _) -> y,
                                                 case f x2 of
                                                     Tuple6 (_, y, _, _, _, _) -> y,
                                                 case f x3 of
                                                     Tuple6 (_, _, y, _, _, _) -> y,
                                                 case f x4 of
                                                     Tuple6 (_, _, _, y, _, _) -> y,
                                                 case f x5 of
                                                     Tuple6 (_, _, _, _, y, _) -> y,
                                                 case f x6 of
                                                     Tuple6 (_, _, _, _, _, y) -> y)
          {-# INLINABLE (>>=) #-}
          (>>) = (*>)
          {-# INLINE (>>) #-}
instance Foldable Tuple6
    where foldr f z (Tuple6 (x1,
                                           x2,
                                           x3,
                                           x4,
                                           x5,
                                           x6)) = f x1 (f x2 (f x3 (f x4 (f x5 (f x6 z)))))
          {-# INLINABLE foldr #-}
          foldl f z (Tuple6 (x1,
                                           x2,
                                           x3,
                                           x4,
                                           x5,
                                           x6)) = f (f (f (f (f (f z x1) x2) x3) x4) x5) x6
          {-# INLINABLE foldl #-}
instance Traversable Tuple6
    where traverse f (Tuple6 (x1,
                                               x2,
                                               x3,
                                               x4,
                                               x5,
                                               x6)) = (<*>) ((<*>) ((<*>) ((<*>) ((<*>) (fmap tuple6 (f x1)) (f x2)) (f x3)) (f x4)) (f x5)) (f x6)
          {-# INLINABLE traverse #-}
newtype Tuple7 a
  = Tuple7 {untuple7 :: ((a, a, a, a, a, a, a))}
    deriving (Eq, Ord, Bounded, Show, Read)
tuple7 x_20 x_21 x_22 x_23 x_24 x_25 x_26 = Tuple7 (x_20,
                                                    x_21,
                                                    x_22,
                                                    x_23,
                                                    x_24,
                                                    x_25,
                                                    x_26)
{-# INLINE tuple7 #-}
instance Functor Tuple7
    where fmap f (Tuple7 (x1,
                                   x2,
                                   x3,
                                   x4,
                                   x5,
                                   x6,
                                   x7)) = Tuple7 (f x1, f x2, f x3, f x4, f x5, f x6, f x7)
          {-# INLINABLE fmap #-}
instance Applicative Tuple7
    where pure x = Tuple7 (x, x, x, x, x, x, x)
          {-# INLINE pure #-}
          (<*>) (Tuple7 (f1,
                                           f2,
                                           f3,
                                           f4,
                                           f5,
                                           f6,
                                           f7)) (Tuple7 (x1,
                                                         x2,
                                                         x3,
                                                         x4,
                                                         x5,
                                                         x6,
                                                         x7)) = Tuple7 (f1 x1,
                                                                        f2 x2,
                                                                        f3 x3,
                                                                        f4 x4,
                                                                        f5 x5,
                                                                        f6 x6,
                                                                        f7 x7)
          {-# INLINABLE (<*>) #-}
          (<*) x _ = x
          {-# INLINE (<*) #-}
          (*>) _ y = y
          {-# INLINE (*>) #-}
instance Monad Tuple7
    where return = pure
          {-# INLINE return #-}
          (>>=) (Tuple7 (x1,
                                x2,
                                x3,
                                x4,
                                x5,
                                x6,
                                x7)) f = Tuple7 (case f x1 of
                                                     Tuple7 (y, _, _, _, _, _, _) -> y,
                                                 case f x2 of
                                                     Tuple7 (_, y, _, _, _, _, _) -> y,
                                                 case f x3 of
                                                     Tuple7 (_, _, y, _, _, _, _) -> y,
                                                 case f x4 of
                                                     Tuple7 (_, _, _, y, _, _, _) -> y,
                                                 case f x5 of
                                                     Tuple7 (_, _, _, _, y, _, _) -> y,
                                                 case f x6 of
                                                     Tuple7 (_, _, _, _, _, y, _) -> y,
                                                 case f x7 of
                                                     Tuple7 (_, _, _, _, _, _, y) -> y)
          {-# INLINABLE (>>=) #-}
          (>>) = (*>)
          {-# INLINE (>>) #-}
instance Foldable Tuple7
    where foldr f z (Tuple7 (x1,
                                           x2,
                                           x3,
                                           x4,
                                           x5,
                                           x6,
                                           x7)) = f x1 (f x2 (f x3 (f x4 (f x5 (f x6 (f x7 z))))))
          {-# INLINABLE foldr #-}
          foldl f z (Tuple7 (x1,
                                           x2,
                                           x3,
                                           x4,
                                           x5,
                                           x6,
                                           x7)) = f (f (f (f (f (f (f z x1) x2) x3) x4) x5) x6) x7
          {-# INLINABLE foldl #-}
instance Traversable Tuple7
    where traverse f (Tuple7 (x1,
                                               x2,
                                               x3,
                                               x4,
                                               x5,
                                               x6,
                                               x7)) = (<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) (fmap tuple7 (f x1)) (f x2)) (f x3)) (f x4)) (f x5)) (f x6)) (f x7)
          {-# INLINABLE traverse #-}
newtype Tuple8 a
  = Tuple8 {untuple8 :: ((a, a, a, a, a, a, a, a))}
    deriving (Eq, Ord, Bounded, Show, Read)
tuple8 x_27 x_28 x_29 x_30 x_31 x_32 x_33 x_34 = Tuple8 (x_27,
                                                         x_28,
                                                         x_29,
                                                         x_30,
                                                         x_31,
                                                         x_32,
                                                         x_33,
                                                         x_34)
{-# INLINE tuple8 #-}
instance Functor Tuple8
    where fmap f (Tuple8 (x1,
                                   x2,
                                   x3,
                                   x4,
                                   x5,
                                   x6,
                                   x7,
                                   x8)) = Tuple8 (f x1, f x2, f x3, f x4, f x5, f x6, f x7, f x8)
          {-# INLINABLE fmap #-}
instance Applicative Tuple8
    where pure x = Tuple8 (x, x, x, x, x, x, x, x)
          {-# INLINE pure #-}
          (<*>) (Tuple8 (f1,
                                           f2,
                                           f3,
                                           f4,
                                           f5,
                                           f6,
                                           f7,
                                           f8)) (Tuple8 (x1,
                                                         x2,
                                                         x3,
                                                         x4,
                                                         x5,
                                                         x6,
                                                         x7,
                                                         x8)) = Tuple8 (f1 x1,
                                                                        f2 x2,
                                                                        f3 x3,
                                                                        f4 x4,
                                                                        f5 x5,
                                                                        f6 x6,
                                                                        f7 x7,
                                                                        f8 x8)
          {-# INLINABLE (<*>) #-}
          (<*) x _ = x
          {-# INLINE (<*) #-}
          (*>) _ y = y
          {-# INLINE (*>) #-}
instance Monad Tuple8
    where return = pure
          {-# INLINE return #-}
          (>>=) (Tuple8 (x1,
                                x2,
                                x3,
                                x4,
                                x5,
                                x6,
                                x7,
                                x8)) f = Tuple8 (case f x1 of
                                                     Tuple8 (y, _, _, _, _, _, _, _) -> y,
                                                 case f x2 of
                                                     Tuple8 (_, y, _, _, _, _, _, _) -> y,
                                                 case f x3 of
                                                     Tuple8 (_, _, y, _, _, _, _, _) -> y,
                                                 case f x4 of
                                                     Tuple8 (_, _, _, y, _, _, _, _) -> y,
                                                 case f x5 of
                                                     Tuple8 (_, _, _, _, y, _, _, _) -> y,
                                                 case f x6 of
                                                     Tuple8 (_, _, _, _, _, y, _, _) -> y,
                                                 case f x7 of
                                                     Tuple8 (_, _, _, _, _, _, y, _) -> y,
                                                 case f x8 of
                                                     Tuple8 (_, _, _, _, _, _, _, y) -> y)
          {-# INLINABLE (>>=) #-}
          (>>) = (*>)
          {-# INLINE (>>) #-}
instance Foldable Tuple8
    where foldr f z (Tuple8 (x1,
                                           x2,
                                           x3,
                                           x4,
                                           x5,
                                           x6,
                                           x7,
                                           x8)) = f x1 (f x2 (f x3 (f x4 (f x5 (f x6 (f x7 (f x8 z)))))))
          {-# INLINABLE foldr #-}
          foldl f z (Tuple8 (x1,
                                           x2,
                                           x3,
                                           x4,
                                           x5,
                                           x6,
                                           x7,
                                           x8)) = f (f (f (f (f (f (f (f z x1) x2) x3) x4) x5) x6) x7) x8
          {-# INLINABLE foldl #-}
instance Traversable Tuple8
    where traverse f (Tuple8 (x1,
                                               x2,
                                               x3,
                                               x4,
                                               x5,
                                               x6,
                                               x7,
                                               x8)) = (<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) (fmap tuple8 (f x1)) (f x2)) (f x3)) (f x4)) (f x5)) (f x6)) (f x7)) (f x8)
          {-# INLINABLE traverse #-}
newtype Tuple9 a
  = Tuple9 {untuple9 :: ((a, a, a, a, a, a, a, a, a))}
    deriving (Eq, Ord, Bounded, Show, Read)
tuple9 x_35 x_36 x_37 x_38 x_39 x_40 x_41 x_42 x_43 = Tuple9 (x_35,
                                                              x_36,
                                                              x_37,
                                                              x_38,
                                                              x_39,
                                                              x_40,
                                                              x_41,
                                                              x_42,
                                                              x_43)
{-# INLINE tuple9 #-}
instance Functor Tuple9
    where fmap f (Tuple9 (x1,
                                   x2,
                                   x3,
                                   x4,
                                   x5,
                                   x6,
                                   x7,
                                   x8,
                                   x9)) = Tuple9 (f x1,
                                                  f x2,
                                                  f x3,
                                                  f x4,
                                                  f x5,
                                                  f x6,
                                                  f x7,
                                                  f x8,
                                                  f x9)
          {-# INLINABLE fmap #-}
instance Applicative Tuple9
    where pure x = Tuple9 (x,
                                               x,
                                               x,
                                               x,
                                               x,
                                               x,
                                               x,
                                               x,
                                               x)
          {-# INLINE pure #-}
          (<*>) (Tuple9 (f1,
                                           f2,
                                           f3,
                                           f4,
                                           f5,
                                           f6,
                                           f7,
                                           f8,
                                           f9)) (Tuple9 (x1,
                                                         x2,
                                                         x3,
                                                         x4,
                                                         x5,
                                                         x6,
                                                         x7,
                                                         x8,
                                                         x9)) = Tuple9 (f1 x1,
                                                                        f2 x2,
                                                                        f3 x3,
                                                                        f4 x4,
                                                                        f5 x5,
                                                                        f6 x6,
                                                                        f7 x7,
                                                                        f8 x8,
                                                                        f9 x9)
          {-# INLINABLE (<*>) #-}
          (<*) x _ = x
          {-# INLINE (<*) #-}
          (*>) _ y = y
          {-# INLINE (*>) #-}
instance Monad Tuple9
    where return = pure
          {-# INLINE return #-}
          (>>=) (Tuple9 (x1,
                                x2,
                                x3,
                                x4,
                                x5,
                                x6,
                                x7,
                                x8,
                                x9)) f = Tuple9 (case f x1 of
                                                     Tuple9 (y, _, _, _, _, _, _, _, _) -> y,
                                                 case f x2 of
                                                     Tuple9 (_, y, _, _, _, _, _, _, _) -> y,
                                                 case f x3 of
                                                     Tuple9 (_, _, y, _, _, _, _, _, _) -> y,
                                                 case f x4 of
                                                     Tuple9 (_, _, _, y, _, _, _, _, _) -> y,
                                                 case f x5 of
                                                     Tuple9 (_, _, _, _, y, _, _, _, _) -> y,
                                                 case f x6 of
                                                     Tuple9 (_, _, _, _, _, y, _, _, _) -> y,
                                                 case f x7 of
                                                     Tuple9 (_, _, _, _, _, _, y, _, _) -> y,
                                                 case f x8 of
                                                     Tuple9 (_, _, _, _, _, _, _, y, _) -> y,
                                                 case f x9 of
                                                     Tuple9 (_, _, _, _, _, _, _, _, y) -> y)
          {-# INLINABLE (>>=) #-}
          (>>) = (*>)
          {-# INLINE (>>) #-}
instance Foldable Tuple9
    where foldr f z (Tuple9 (x1,
                                           x2,
                                           x3,
                                           x4,
                                           x5,
                                           x6,
                                           x7,
                                           x8,
                                           x9)) = f x1 (f x2 (f x3 (f x4 (f x5 (f x6 (f x7 (f x8 (f x9 z))))))))
          {-# INLINABLE foldr #-}
          foldl f z (Tuple9 (x1,
                                           x2,
                                           x3,
                                           x4,
                                           x5,
                                           x6,
                                           x7,
                                           x8,
                                           x9)) = f (f (f (f (f (f (f (f (f z x1) x2) x3) x4) x5) x6) x7) x8) x9
          {-# INLINABLE foldl #-}
instance Traversable Tuple9
    where traverse f (Tuple9 (x1,
                                               x2,
                                               x3,
                                               x4,
                                               x5,
                                               x6,
                                               x7,
                                               x8,
                                               x9)) = (<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) (fmap tuple9 (f x1)) (f x2)) (f x3)) (f x4)) (f x5)) (f x6)) (f x7)) (f x8)) (f x9)
          {-# INLINABLE traverse #-}
newtype Tuple10 a
  = Tuple10 {untuple10 :: ((a, a, a, a, a, a, a, a, a, a))}
    deriving (Eq, Ord, Bounded, Show, Read)
tuple10 x_44 x_45 x_46 x_47 x_48 x_49 x_50 x_51 x_52 x_53 = Tuple10 (x_44,
                                                                     x_45,
                                                                     x_46,
                                                                     x_47,
                                                                     x_48,
                                                                     x_49,
                                                                     x_50,
                                                                     x_51,
                                                                     x_52,
                                                                     x_53)
{-# INLINE tuple10 #-}
instance Functor Tuple10
    where fmap f (Tuple10 (x1,
                                    x2,
                                    x3,
                                    x4,
                                    x5,
                                    x6,
                                    x7,
                                    x8,
                                    x9,
                                    x10)) = Tuple10 (f x1,
                                                     f x2,
                                                     f x3,
                                                     f x4,
                                                     f x5,
                                                     f x6,
                                                     f x7,
                                                     f x8,
                                                     f x9,
                                                     f x10)
          {-# INLINABLE fmap #-}
instance Applicative Tuple10
    where pure x = Tuple10 (x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x)
          {-# INLINE pure #-}
          (<*>) (Tuple10 (f1,
                                            f2,
                                            f3,
                                            f4,
                                            f5,
                                            f6,
                                            f7,
                                            f8,
                                            f9,
                                            f10)) (Tuple10 (x1,
                                                            x2,
                                                            x3,
                                                            x4,
                                                            x5,
                                                            x6,
                                                            x7,
                                                            x8,
                                                            x9,
                                                            x10)) = Tuple10 (f1 x1,
                                                                             f2 x2,
                                                                             f3 x3,
                                                                             f4 x4,
                                                                             f5 x5,
                                                                             f6 x6,
                                                                             f7 x7,
                                                                             f8 x8,
                                                                             f9 x9,
                                                                             f10 x10)
          {-# INLINABLE (<*>) #-}
          (<*) x _ = x
          {-# INLINE (<*) #-}
          (*>) _ y = y
          {-# INLINE (*>) #-}
instance Monad Tuple10
    where return = pure
          {-# INLINE return #-}
          (>>=) (Tuple10 (x1,
                                 x2,
                                 x3,
                                 x4,
                                 x5,
                                 x6,
                                 x7,
                                 x8,
                                 x9,
                                 x10)) f = Tuple10 (case f x1 of
                                                        Tuple10 (y, _, _, _, _, _, _, _, _, _) -> y,
                                                    case f x2 of
                                                        Tuple10 (_, y, _, _, _, _, _, _, _, _) -> y,
                                                    case f x3 of
                                                        Tuple10 (_, _, y, _, _, _, _, _, _, _) -> y,
                                                    case f x4 of
                                                        Tuple10 (_, _, _, y, _, _, _, _, _, _) -> y,
                                                    case f x5 of
                                                        Tuple10 (_, _, _, _, y, _, _, _, _, _) -> y,
                                                    case f x6 of
                                                        Tuple10 (_, _, _, _, _, y, _, _, _, _) -> y,
                                                    case f x7 of
                                                        Tuple10 (_, _, _, _, _, _, y, _, _, _) -> y,
                                                    case f x8 of
                                                        Tuple10 (_, _, _, _, _, _, _, y, _, _) -> y,
                                                    case f x9 of
                                                        Tuple10 (_, _, _, _, _, _, _, _, y, _) -> y,
                                                    case f x10 of
                                                        Tuple10 (_, _, _, _, _, _, _, _, _, y) -> y)
          {-# INLINABLE (>>=) #-}
          (>>) = (*>)
          {-# INLINE (>>) #-}
instance Foldable Tuple10
    where foldr f z (Tuple10 (x1,
                                            x2,
                                            x3,
                                            x4,
                                            x5,
                                            x6,
                                            x7,
                                            x8,
                                            x9,
                                            x10)) = f x1 (f x2 (f x3 (f x4 (f x5 (f x6 (f x7 (f x8 (f x9 (f x10 z)))))))))
          {-# INLINABLE foldr #-}
          foldl f z (Tuple10 (x1,
                                            x2,
                                            x3,
                                            x4,
                                            x5,
                                            x6,
                                            x7,
                                            x8,
                                            x9,
                                            x10)) = f (f (f (f (f (f (f (f (f (f z x1) x2) x3) x4) x5) x6) x7) x8) x9) x10
          {-# INLINABLE foldl #-}
instance Traversable Tuple10
    where traverse f (Tuple10 (x1,
                                                x2,
                                                x3,
                                                x4,
                                                x5,
                                                x6,
                                                x7,
                                                x8,
                                                x9,
                                                x10)) = (<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) (fmap tuple10 (f x1)) (f x2)) (f x3)) (f x4)) (f x5)) (f x6)) (f x7)) (f x8)) (f x9)) (f x10)
          {-# INLINABLE traverse #-}
newtype Tuple11 a
  = Tuple11 {untuple11 :: ((a, a, a, a, a, a, a, a, a, a, a))}
    deriving (Eq, Ord, Bounded, Show, Read)
tuple11 x_54 x_55 x_56 x_57 x_58 x_59 x_60 x_61 x_62 x_63 x_64 = Tuple11 (x_54,
                                                                          x_55,
                                                                          x_56,
                                                                          x_57,
                                                                          x_58,
                                                                          x_59,
                                                                          x_60,
                                                                          x_61,
                                                                          x_62,
                                                                          x_63,
                                                                          x_64)
{-# INLINE tuple11 #-}
instance Functor Tuple11
    where fmap f (Tuple11 (x1,
                                    x2,
                                    x3,
                                    x4,
                                    x5,
                                    x6,
                                    x7,
                                    x8,
                                    x9,
                                    x10,
                                    x11)) = Tuple11 (f x1,
                                                     f x2,
                                                     f x3,
                                                     f x4,
                                                     f x5,
                                                     f x6,
                                                     f x7,
                                                     f x8,
                                                     f x9,
                                                     f x10,
                                                     f x11)
          {-# INLINABLE fmap #-}
instance Applicative Tuple11
    where pure x = Tuple11 (x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x)
          {-# INLINE pure #-}
          (<*>) (Tuple11 (f1,
                                            f2,
                                            f3,
                                            f4,
                                            f5,
                                            f6,
                                            f7,
                                            f8,
                                            f9,
                                            f10,
                                            f11)) (Tuple11 (x1,
                                                            x2,
                                                            x3,
                                                            x4,
                                                            x5,
                                                            x6,
                                                            x7,
                                                            x8,
                                                            x9,
                                                            x10,
                                                            x11)) = Tuple11 (f1 x1,
                                                                             f2 x2,
                                                                             f3 x3,
                                                                             f4 x4,
                                                                             f5 x5,
                                                                             f6 x6,
                                                                             f7 x7,
                                                                             f8 x8,
                                                                             f9 x9,
                                                                             f10 x10,
                                                                             f11 x11)
          {-# INLINABLE (<*>) #-}
          (<*) x _ = x
          {-# INLINE (<*) #-}
          (*>) _ y = y
          {-# INLINE (*>) #-}
instance Monad Tuple11
    where return = pure
          {-# INLINE return #-}
          (>>=) (Tuple11 (x1,
                                 x2,
                                 x3,
                                 x4,
                                 x5,
                                 x6,
                                 x7,
                                 x8,
                                 x9,
                                 x10,
                                 x11)) f = Tuple11 (case f x1 of
                                                        Tuple11 (y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x2 of
                                                        Tuple11 (_,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x3 of
                                                        Tuple11 (_,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x4 of
                                                        Tuple11 (_,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x5 of
                                                        Tuple11 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x6 of
                                                        Tuple11 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x7 of
                                                        Tuple11 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x8 of
                                                        Tuple11 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x9 of
                                                        Tuple11 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _) -> y,
                                                    case f x10 of
                                                        Tuple11 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _) -> y,
                                                    case f x11 of
                                                        Tuple11 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y) -> y)
          {-# INLINABLE (>>=) #-}
          (>>) = (*>)
          {-# INLINE (>>) #-}
instance Foldable Tuple11
    where foldr f z (Tuple11 (x1,
                                            x2,
                                            x3,
                                            x4,
                                            x5,
                                            x6,
                                            x7,
                                            x8,
                                            x9,
                                            x10,
                                            x11)) = f x1 (f x2 (f x3 (f x4 (f x5 (f x6 (f x7 (f x8 (f x9 (f x10 (f x11 z))))))))))
          {-# INLINABLE foldr #-}
          foldl f z (Tuple11 (x1,
                                            x2,
                                            x3,
                                            x4,
                                            x5,
                                            x6,
                                            x7,
                                            x8,
                                            x9,
                                            x10,
                                            x11)) = f (f (f (f (f (f (f (f (f (f (f z x1) x2) x3) x4) x5) x6) x7) x8) x9) x10) x11
          {-# INLINABLE foldl #-}
instance Traversable Tuple11
    where traverse f (Tuple11 (x1,
                                                x2,
                                                x3,
                                                x4,
                                                x5,
                                                x6,
                                                x7,
                                                x8,
                                                x9,
                                                x10,
                                                x11)) = (<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) (fmap tuple11 (f x1)) (f x2)) (f x3)) (f x4)) (f x5)) (f x6)) (f x7)) (f x8)) (f x9)) (f x10)) (f x11)
          {-# INLINABLE traverse #-}
newtype Tuple12 a
  = Tuple12 {untuple12 :: ((a, a, a, a, a, a, a, a, a, a, a, a))}
    deriving (Eq, Ord, Bounded, Show, Read)
tuple12 x_65 x_66 x_67 x_68 x_69 x_70 x_71 x_72 x_73 x_74 x_75 x_76 = Tuple12 (x_65,
                                                                               x_66,
                                                                               x_67,
                                                                               x_68,
                                                                               x_69,
                                                                               x_70,
                                                                               x_71,
                                                                               x_72,
                                                                               x_73,
                                                                               x_74,
                                                                               x_75,
                                                                               x_76)
{-# INLINE tuple12 #-}
instance Functor Tuple12
    where fmap f (Tuple12 (x1,
                                    x2,
                                    x3,
                                    x4,
                                    x5,
                                    x6,
                                    x7,
                                    x8,
                                    x9,
                                    x10,
                                    x11,
                                    x12)) = Tuple12 (f x1,
                                                     f x2,
                                                     f x3,
                                                     f x4,
                                                     f x5,
                                                     f x6,
                                                     f x7,
                                                     f x8,
                                                     f x9,
                                                     f x10,
                                                     f x11,
                                                     f x12)
          {-# INLINABLE fmap #-}
instance Applicative Tuple12
    where pure x = Tuple12 (x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x)
          {-# INLINE pure #-}
          (<*>) (Tuple12 (f1,
                                            f2,
                                            f3,
                                            f4,
                                            f5,
                                            f6,
                                            f7,
                                            f8,
                                            f9,
                                            f10,
                                            f11,
                                            f12)) (Tuple12 (x1,
                                                            x2,
                                                            x3,
                                                            x4,
                                                            x5,
                                                            x6,
                                                            x7,
                                                            x8,
                                                            x9,
                                                            x10,
                                                            x11,
                                                            x12)) = Tuple12 (f1 x1,
                                                                             f2 x2,
                                                                             f3 x3,
                                                                             f4 x4,
                                                                             f5 x5,
                                                                             f6 x6,
                                                                             f7 x7,
                                                                             f8 x8,
                                                                             f9 x9,
                                                                             f10 x10,
                                                                             f11 x11,
                                                                             f12 x12)
          {-# INLINABLE (<*>) #-}
          (<*) x _ = x
          {-# INLINE (<*) #-}
          (*>) _ y = y
          {-# INLINE (*>) #-}
instance Monad Tuple12
    where return = pure
          {-# INLINE return #-}
          (>>=) (Tuple12 (x1,
                                 x2,
                                 x3,
                                 x4,
                                 x5,
                                 x6,
                                 x7,
                                 x8,
                                 x9,
                                 x10,
                                 x11,
                                 x12)) f = Tuple12 (case f x1 of
                                                        Tuple12 (y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x2 of
                                                        Tuple12 (_,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x3 of
                                                        Tuple12 (_,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x4 of
                                                        Tuple12 (_,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x5 of
                                                        Tuple12 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x6 of
                                                        Tuple12 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x7 of
                                                        Tuple12 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x8 of
                                                        Tuple12 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x9 of
                                                        Tuple12 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x10 of
                                                        Tuple12 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _) -> y,
                                                    case f x11 of
                                                        Tuple12 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _) -> y,
                                                    case f x12 of
                                                        Tuple12 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y) -> y)
          {-# INLINABLE (>>=) #-}
          (>>) = (*>)
          {-# INLINE (>>) #-}
instance Foldable Tuple12
    where foldr f z (Tuple12 (x1,
                                            x2,
                                            x3,
                                            x4,
                                            x5,
                                            x6,
                                            x7,
                                            x8,
                                            x9,
                                            x10,
                                            x11,
                                            x12)) = f x1 (f x2 (f x3 (f x4 (f x5 (f x6 (f x7 (f x8 (f x9 (f x10 (f x11 (f x12 z)))))))))))
          {-# INLINABLE foldr #-}
          foldl f z (Tuple12 (x1,
                                            x2,
                                            x3,
                                            x4,
                                            x5,
                                            x6,
                                            x7,
                                            x8,
                                            x9,
                                            x10,
                                            x11,
                                            x12)) = f (f (f (f (f (f (f (f (f (f (f (f z x1) x2) x3) x4) x5) x6) x7) x8) x9) x10) x11) x12
          {-# INLINABLE foldl #-}
instance Traversable Tuple12
    where traverse f (Tuple12 (x1,
                                                x2,
                                                x3,
                                                x4,
                                                x5,
                                                x6,
                                                x7,
                                                x8,
                                                x9,
                                                x10,
                                                x11,
                                                x12)) = (<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) (fmap tuple12 (f x1)) (f x2)) (f x3)) (f x4)) (f x5)) (f x6)) (f x7)) (f x8)) (f x9)) (f x10)) (f x11)) (f x12)
          {-# INLINABLE traverse #-}
newtype Tuple13 a
  = Tuple13 {untuple13 :: ((a, a, a, a, a, a, a, a, a, a, a, a, a))}
    deriving (Eq, Ord, Bounded, Show, Read)
tuple13 x_77 x_78 x_79 x_80 x_81 x_82 x_83 x_84 x_85 x_86 x_87 x_88 x_89 = Tuple13 (x_77,
                                                                                    x_78,
                                                                                    x_79,
                                                                                    x_80,
                                                                                    x_81,
                                                                                    x_82,
                                                                                    x_83,
                                                                                    x_84,
                                                                                    x_85,
                                                                                    x_86,
                                                                                    x_87,
                                                                                    x_88,
                                                                                    x_89)
{-# INLINE tuple13 #-}
instance Functor Tuple13
    where fmap f (Tuple13 (x1,
                                    x2,
                                    x3,
                                    x4,
                                    x5,
                                    x6,
                                    x7,
                                    x8,
                                    x9,
                                    x10,
                                    x11,
                                    x12,
                                    x13)) = Tuple13 (f x1,
                                                     f x2,
                                                     f x3,
                                                     f x4,
                                                     f x5,
                                                     f x6,
                                                     f x7,
                                                     f x8,
                                                     f x9,
                                                     f x10,
                                                     f x11,
                                                     f x12,
                                                     f x13)
          {-# INLINABLE fmap #-}
instance Applicative Tuple13
    where pure x = Tuple13 (x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x)
          {-# INLINE pure #-}
          (<*>) (Tuple13 (f1,
                                            f2,
                                            f3,
                                            f4,
                                            f5,
                                            f6,
                                            f7,
                                            f8,
                                            f9,
                                            f10,
                                            f11,
                                            f12,
                                            f13)) (Tuple13 (x1,
                                                            x2,
                                                            x3,
                                                            x4,
                                                            x5,
                                                            x6,
                                                            x7,
                                                            x8,
                                                            x9,
                                                            x10,
                                                            x11,
                                                            x12,
                                                            x13)) = Tuple13 (f1 x1,
                                                                             f2 x2,
                                                                             f3 x3,
                                                                             f4 x4,
                                                                             f5 x5,
                                                                             f6 x6,
                                                                             f7 x7,
                                                                             f8 x8,
                                                                             f9 x9,
                                                                             f10 x10,
                                                                             f11 x11,
                                                                             f12 x12,
                                                                             f13 x13)
          {-# INLINABLE (<*>) #-}
          (<*) x _ = x
          {-# INLINE (<*) #-}
          (*>) _ y = y
          {-# INLINE (*>) #-}
instance Monad Tuple13
    where return = pure
          {-# INLINE return #-}
          (>>=) (Tuple13 (x1,
                                 x2,
                                 x3,
                                 x4,
                                 x5,
                                 x6,
                                 x7,
                                 x8,
                                 x9,
                                 x10,
                                 x11,
                                 x12,
                                 x13)) f = Tuple13 (case f x1 of
                                                        Tuple13 (y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x2 of
                                                        Tuple13 (_,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x3 of
                                                        Tuple13 (_,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x4 of
                                                        Tuple13 (_,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x5 of
                                                        Tuple13 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x6 of
                                                        Tuple13 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x7 of
                                                        Tuple13 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x8 of
                                                        Tuple13 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x9 of
                                                        Tuple13 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x10 of
                                                        Tuple13 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x11 of
                                                        Tuple13 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _) -> y,
                                                    case f x12 of
                                                        Tuple13 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _) -> y,
                                                    case f x13 of
                                                        Tuple13 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y) -> y)
          {-# INLINABLE (>>=) #-}
          (>>) = (*>)
          {-# INLINE (>>) #-}
instance Foldable Tuple13
    where foldr f z (Tuple13 (x1,
                                            x2,
                                            x3,
                                            x4,
                                            x5,
                                            x6,
                                            x7,
                                            x8,
                                            x9,
                                            x10,
                                            x11,
                                            x12,
                                            x13)) = f x1 (f x2 (f x3 (f x4 (f x5 (f x6 (f x7 (f x8 (f x9 (f x10 (f x11 (f x12 (f x13 z))))))))))))
          {-# INLINABLE foldr #-}
          foldl f z (Tuple13 (x1,
                                            x2,
                                            x3,
                                            x4,
                                            x5,
                                            x6,
                                            x7,
                                            x8,
                                            x9,
                                            x10,
                                            x11,
                                            x12,
                                            x13)) = f (f (f (f (f (f (f (f (f (f (f (f (f z x1) x2) x3) x4) x5) x6) x7) x8) x9) x10) x11) x12) x13
          {-# INLINABLE foldl #-}
instance Traversable Tuple13
    where traverse f (Tuple13 (x1,
                                                x2,
                                                x3,
                                                x4,
                                                x5,
                                                x6,
                                                x7,
                                                x8,
                                                x9,
                                                x10,
                                                x11,
                                                x12,
                                                x13)) = (<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) (fmap tuple13 (f x1)) (f x2)) (f x3)) (f x4)) (f x5)) (f x6)) (f x7)) (f x8)) (f x9)) (f x10)) (f x11)) (f x12)) (f x13)
          {-# INLINABLE traverse #-}
newtype Tuple14 a
  = Tuple14 {untuple14 :: ((a,
                            a,
                            a,
                            a,
                            a,
                            a,
                            a,
                            a,
                            a,
                            a,
                            a,
                            a,
                            a,
                            a))}
    deriving (Eq, Ord, Bounded, Show, Read)
tuple14 x_90 x_91 x_92 x_93 x_94 x_95 x_96 x_97 x_98 x_99 x_100 x_101 x_102 x_103 = Tuple14 (x_90,
                                                                                             x_91,
                                                                                             x_92,
                                                                                             x_93,
                                                                                             x_94,
                                                                                             x_95,
                                                                                             x_96,
                                                                                             x_97,
                                                                                             x_98,
                                                                                             x_99,
                                                                                             x_100,
                                                                                             x_101,
                                                                                             x_102,
                                                                                             x_103)
{-# INLINE tuple14 #-}
instance Functor Tuple14
    where fmap f (Tuple14 (x1,
                                    x2,
                                    x3,
                                    x4,
                                    x5,
                                    x6,
                                    x7,
                                    x8,
                                    x9,
                                    x10,
                                    x11,
                                    x12,
                                    x13,
                                    x14)) = Tuple14 (f x1,
                                                     f x2,
                                                     f x3,
                                                     f x4,
                                                     f x5,
                                                     f x6,
                                                     f x7,
                                                     f x8,
                                                     f x9,
                                                     f x10,
                                                     f x11,
                                                     f x12,
                                                     f x13,
                                                     f x14)
          {-# INLINABLE fmap #-}
instance Applicative Tuple14
    where pure x = Tuple14 (x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x)
          {-# INLINE pure #-}
          (<*>) (Tuple14 (f1,
                                            f2,
                                            f3,
                                            f4,
                                            f5,
                                            f6,
                                            f7,
                                            f8,
                                            f9,
                                            f10,
                                            f11,
                                            f12,
                                            f13,
                                            f14)) (Tuple14 (x1,
                                                            x2,
                                                            x3,
                                                            x4,
                                                            x5,
                                                            x6,
                                                            x7,
                                                            x8,
                                                            x9,
                                                            x10,
                                                            x11,
                                                            x12,
                                                            x13,
                                                            x14)) = Tuple14 (f1 x1,
                                                                             f2 x2,
                                                                             f3 x3,
                                                                             f4 x4,
                                                                             f5 x5,
                                                                             f6 x6,
                                                                             f7 x7,
                                                                             f8 x8,
                                                                             f9 x9,
                                                                             f10 x10,
                                                                             f11 x11,
                                                                             f12 x12,
                                                                             f13 x13,
                                                                             f14 x14)
          {-# INLINABLE (<*>) #-}
          (<*) x _ = x
          {-# INLINE (<*) #-}
          (*>) _ y = y
          {-# INLINE (*>) #-}
instance Monad Tuple14
    where return = pure
          {-# INLINE return #-}
          (>>=) (Tuple14 (x1,
                                 x2,
                                 x3,
                                 x4,
                                 x5,
                                 x6,
                                 x7,
                                 x8,
                                 x9,
                                 x10,
                                 x11,
                                 x12,
                                 x13,
                                 x14)) f = Tuple14 (case f x1 of
                                                        Tuple14 (y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x2 of
                                                        Tuple14 (_,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x3 of
                                                        Tuple14 (_,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x4 of
                                                        Tuple14 (_,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x5 of
                                                        Tuple14 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x6 of
                                                        Tuple14 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x7 of
                                                        Tuple14 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x8 of
                                                        Tuple14 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x9 of
                                                        Tuple14 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x10 of
                                                        Tuple14 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x11 of
                                                        Tuple14 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x12 of
                                                        Tuple14 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _) -> y,
                                                    case f x13 of
                                                        Tuple14 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _) -> y,
                                                    case f x14 of
                                                        Tuple14 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y) -> y)
          {-# INLINABLE (>>=) #-}
          (>>) = (*>)
          {-# INLINE (>>) #-}
instance Foldable Tuple14
    where foldr f z (Tuple14 (x1,
                                            x2,
                                            x3,
                                            x4,
                                            x5,
                                            x6,
                                            x7,
                                            x8,
                                            x9,
                                            x10,
                                            x11,
                                            x12,
                                            x13,
                                            x14)) = f x1 (f x2 (f x3 (f x4 (f x5 (f x6 (f x7 (f x8 (f x9 (f x10 (f x11 (f x12 (f x13 (f x14 z)))))))))))))
          {-# INLINABLE foldr #-}
          foldl f z (Tuple14 (x1,
                                            x2,
                                            x3,
                                            x4,
                                            x5,
                                            x6,
                                            x7,
                                            x8,
                                            x9,
                                            x10,
                                            x11,
                                            x12,
                                            x13,
                                            x14)) = f (f (f (f (f (f (f (f (f (f (f (f (f (f z x1) x2) x3) x4) x5) x6) x7) x8) x9) x10) x11) x12) x13) x14
          {-# INLINABLE foldl #-}
instance Traversable Tuple14
    where traverse f (Tuple14 (x1,
                                                x2,
                                                x3,
                                                x4,
                                                x5,
                                                x6,
                                                x7,
                                                x8,
                                                x9,
                                                x10,
                                                x11,
                                                x12,
                                                x13,
                                                x14)) = (<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) (fmap tuple14 (f x1)) (f x2)) (f x3)) (f x4)) (f x5)) (f x6)) (f x7)) (f x8)) (f x9)) (f x10)) (f x11)) (f x12)) (f x13)) (f x14)
          {-# INLINABLE traverse #-}
newtype Tuple15 a
  = Tuple15 {untuple15 :: ((a,
                            a,
                            a,
                            a,
                            a,
                            a,
                            a,
                            a,
                            a,
                            a,
                            a,
                            a,
                            a,
                            a,
                            a))}
    deriving (Eq, Ord, Bounded, Show, Read)
tuple15 x_104 x_105 x_106 x_107 x_108 x_109 x_110 x_111 x_112 x_113 x_114 x_115 x_116 x_117 x_118 = Tuple15 (x_104,
                                                                                                             x_105,
                                                                                                             x_106,
                                                                                                             x_107,
                                                                                                             x_108,
                                                                                                             x_109,
                                                                                                             x_110,
                                                                                                             x_111,
                                                                                                             x_112,
                                                                                                             x_113,
                                                                                                             x_114,
                                                                                                             x_115,
                                                                                                             x_116,
                                                                                                             x_117,
                                                                                                             x_118)
{-# INLINE tuple15 #-}
instance Functor Tuple15
    where fmap f (Tuple15 (x1,
                                    x2,
                                    x3,
                                    x4,
                                    x5,
                                    x6,
                                    x7,
                                    x8,
                                    x9,
                                    x10,
                                    x11,
                                    x12,
                                    x13,
                                    x14,
                                    x15)) = Tuple15 (f x1,
                                                     f x2,
                                                     f x3,
                                                     f x4,
                                                     f x5,
                                                     f x6,
                                                     f x7,
                                                     f x8,
                                                     f x9,
                                                     f x10,
                                                     f x11,
                                                     f x12,
                                                     f x13,
                                                     f x14,
                                                     f x15)
          {-# INLINABLE fmap #-}
instance Applicative Tuple15
    where pure x = Tuple15 (x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x,
                                                x)
          {-# INLINE pure #-}
          (<*>) (Tuple15 (f1,
                                            f2,
                                            f3,
                                            f4,
                                            f5,
                                            f6,
                                            f7,
                                            f8,
                                            f9,
                                            f10,
                                            f11,
                                            f12,
                                            f13,
                                            f14,
                                            f15)) (Tuple15 (x1,
                                                            x2,
                                                            x3,
                                                            x4,
                                                            x5,
                                                            x6,
                                                            x7,
                                                            x8,
                                                            x9,
                                                            x10,
                                                            x11,
                                                            x12,
                                                            x13,
                                                            x14,
                                                            x15)) = Tuple15 (f1 x1,
                                                                             f2 x2,
                                                                             f3 x3,
                                                                             f4 x4,
                                                                             f5 x5,
                                                                             f6 x6,
                                                                             f7 x7,
                                                                             f8 x8,
                                                                             f9 x9,
                                                                             f10 x10,
                                                                             f11 x11,
                                                                             f12 x12,
                                                                             f13 x13,
                                                                             f14 x14,
                                                                             f15 x15)
          {-# INLINABLE (<*>) #-}
          (<*) x _ = x
          {-# INLINE (<*) #-}
          (*>) _ y = y
          {-# INLINE (*>) #-}
instance Monad Tuple15
    where return = pure
          {-# INLINE return #-}
          (>>=) (Tuple15 (x1,
                                 x2,
                                 x3,
                                 x4,
                                 x5,
                                 x6,
                                 x7,
                                 x8,
                                 x9,
                                 x10,
                                 x11,
                                 x12,
                                 x13,
                                 x14,
                                 x15)) f = Tuple15 (case f x1 of
                                                        Tuple15 (y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x2 of
                                                        Tuple15 (_,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x3 of
                                                        Tuple15 (_,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x4 of
                                                        Tuple15 (_,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x5 of
                                                        Tuple15 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x6 of
                                                        Tuple15 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x7 of
                                                        Tuple15 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x8 of
                                                        Tuple15 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x9 of
                                                        Tuple15 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x10 of
                                                        Tuple15 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x11 of
                                                        Tuple15 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x12 of
                                                        Tuple15 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _,
                                                                 _) -> y,
                                                    case f x13 of
                                                        Tuple15 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _,
                                                                 _) -> y,
                                                    case f x14 of
                                                        Tuple15 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y,
                                                                 _) -> y,
                                                    case f x15 of
                                                        Tuple15 (_,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 _,
                                                                 y) -> y)
          {-# INLINABLE (>>=) #-}
          (>>) = (*>)
          {-# INLINE (>>) #-}
instance Foldable Tuple15
    where foldr f z (Tuple15 (x1,
                                            x2,
                                            x3,
                                            x4,
                                            x5,
                                            x6,
                                            x7,
                                            x8,
                                            x9,
                                            x10,
                                            x11,
                                            x12,
                                            x13,
                                            x14,
                                            x15)) = f x1 (f x2 (f x3 (f x4 (f x5 (f x6 (f x7 (f x8 (f x9 (f x10 (f x11 (f x12 (f x13 (f x14 (f x15 z))))))))))))))
          {-# INLINABLE foldr #-}
          foldl f z (Tuple15 (x1,
                                            x2,
                                            x3,
                                            x4,
                                            x5,
                                            x6,
                                            x7,
                                            x8,
                                            x9,
                                            x10,
                                            x11,
                                            x12,
                                            x13,
                                            x14,
                                            x15)) = f (f (f (f (f (f (f (f (f (f (f (f (f (f (f z x1) x2) x3) x4) x5) x6) x7) x8) x9) x10) x11) x12) x13) x14) x15
          {-# INLINABLE foldl #-}
instance Traversable Tuple15
    where traverse f (Tuple15 (x1,
                                                x2,
                                                x3,
                                                x4,
                                                x5,
                                                x6,
                                                x7,
                                                x8,
                                                x9,
                                                x10,
                                                x11,
                                                x12,
                                                x13,
                                                x14,
                                                x15)) = (<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) ((<*>) (fmap tuple15 (f x1)) (f x2)) (f x3)) (f x4)) (f x5)) (f x6)) (f x7)) (f x8)) (f x9)) (f x10)) (f x11)) (f x12)) (f x13)) (f x14)) (f x15)
          {-# INLINABLE traverse #-}

