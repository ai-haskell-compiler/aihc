{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}
module Data.Vector.Unboxing.Internal
  (Unboxable(Rep)
  ,Vector(UnboxingVector)
  ,MVector(UnboxingMVector)
  ,Generics(..)
  ,Enum(..)
  ,EnumRep(..)
  ,coerceVector
  ,liftCoercion
  ,vectorCoercion
  ,toUnboxedVector
  ,fromUnboxedVector
  ,coercionWithUnboxedVector
  ,coerceMVector
  ,liftCoercionM
  ,mVectorCoercion
  ,toUnboxedMVector
  ,fromUnboxedMVector
  ,coercionWithUnboxedMVector
  ) where
import Prelude hiding (Enum)
import qualified Prelude
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Fusion.Bundle as Bundle
import Data.Coerce
import Data.Type.Coercion
import Data.Int
import Data.Word
import qualified GHC.Generics
import Data.Type.Bool
import qualified Data.Complex
import qualified Data.Functor.Identity
import qualified Data.Functor.Const
import qualified Data.Functor.Compose
import qualified Data.Ord
import qualified Data.Semigroup
import qualified Data.Monoid
import GHC.Exts (IsList(..))
import Control.DeepSeq (NFData(..))
import Text.Read (Read(..),readListPrecDefault)
import GHC.TypeLits (TypeError,ErrorMessage(Text))

newtype Vector a = UnboxingVector (U.Vector (Rep a))
newtype MVector s a = UnboxingMVector (UM.MVector s (Rep a))

type instance G.Mutable Vector = MVector

-- | Types that can be stored in unboxed vectors ('Vector' and 'MVector').
--
-- You can define instances of this class like:
--
-- > newtype Foo = Foo Int
-- > instance Unboxable Foo where
-- >   type Rep Foo = Int
--
-- The type specified by 'Rep' needs to be an instance of 'U.Unbox',
-- and coercion must be possible between the two types.
--
-- Instances can also be derived with @GeneralizedNewtypeDeriving@.
-- GND always works if the base type is an instance of 'Unboxable'.
--
-- If you want to have non-trivial correspondence between the type and the representation,
-- use 'Generics' wrapper with @DerivingVia@.
--
-- Note that @UndecidableInstances@ is needed if you use GND or @DerivingVia@ to derive instances.
class U.Unbox (Rep a) => Unboxable a where
  -- | The underlying type of @a@.  Must be an instance of 'U.Unbox'.
  type Rep a

  -- Hidden members:

  -- Used by 'coerceVector'
  type CoercibleRep a
  type CoercibleRep a = Rep a

  -- True if both 'unboxingFrom and 'unboxingTo' are just 'coerce'
  type IsTrivial a :: Bool
  type IsTrivial a = 'True
  -- TODO: Use ConstraintKinds?

  unboxingFrom :: a -> Rep a
  default unboxingFrom :: Coercible a (Rep a) => a -> Rep a
  unboxingFrom = coerce
  {-# INLINE unboxingFrom #-}

  unboxingTo :: Rep a -> a
  default unboxingTo :: Coercible a (Rep a) => Rep a -> a
  unboxingTo = coerce
  {-# INLINE unboxingTo #-}

coerceVector :: (Coercible a b, Unboxable a, Unboxable b, CoercibleRep a ~ CoercibleRep b, Rep a ~ Rep b) => Vector a -> Vector b
coerceVector = coerce
{-# INLINE coerceVector #-}

liftCoercion :: (Unboxable a, Unboxable b, CoercibleRep a ~ CoercibleRep b, Rep a ~ Rep b) => Coercion a b -> Coercion (Vector a) (Vector b)
liftCoercion Coercion = Coercion
{-# INLINE liftCoercion #-}

vectorCoercion :: (Coercible a b, Unboxable a, Unboxable b, CoercibleRep a ~ CoercibleRep b, Rep a ~ Rep b) => Coercion (Vector a) (Vector b)
vectorCoercion = Coercion
{-# INLINE vectorCoercion #-}

toUnboxedVector :: (Unboxable a, Rep a ~ a, IsTrivial a ~ 'True) => Vector a -> U.Vector a
toUnboxedVector = coerce
{-# INLINE toUnboxedVector #-}

fromUnboxedVector :: (Unboxable a, Rep a ~ a, IsTrivial a ~ 'True) => U.Vector a -> Vector a
fromUnboxedVector = coerce
{-# INLINE fromUnboxedVector #-}

coerceMVector :: (Coercible a b, Unboxable a, Unboxable b, CoercibleRep a ~ CoercibleRep b, Rep a ~ Rep b) => MVector s a -> MVector s b
coerceMVector = coerce
{-# INLINE coerceMVector #-}

liftCoercionM :: (Unboxable a, Unboxable b, CoercibleRep a ~ CoercibleRep b, Rep a ~ Rep b) => Coercion a b -> Coercion (MVector s a) (MVector s b)
liftCoercionM Coercion = Coercion
{-# INLINE liftCoercionM #-}

mVectorCoercion :: (Coercible a b, Unboxable a, Unboxable b, CoercibleRep a ~ CoercibleRep b, Rep a ~ Rep b) => Coercion (MVector s a) (MVector s b)
mVectorCoercion = Coercion
{-# INLINE mVectorCoercion #-}

toUnboxedMVector :: (Unboxable a, Rep a ~ a, IsTrivial a ~ 'True) => MVector s a -> UM.MVector s a
toUnboxedMVector = coerce
{-# INLINE toUnboxedMVector #-}

fromUnboxedMVector :: (Unboxable a, Rep a ~ a, IsTrivial a ~ 'True) => UM.MVector s a -> MVector s a
fromUnboxedMVector = coerce
{-# INLINE fromUnboxedMVector #-}

coercionWithUnboxedVector :: (Unboxable a, Rep a ~ a, IsTrivial a ~ 'True) => Coercion (Vector a) (U.Vector a)
coercionWithUnboxedVector = Coercion
{-# INLINE coercionWithUnboxedVector #-}

coercionWithUnboxedMVector :: (Unboxable a, Rep a ~ a, IsTrivial a ~ 'True) => Coercion (MVector s a) (U.MVector s a)
coercionWithUnboxedMVector = Coercion
{-# INLINE coercionWithUnboxedMVector #-}

--
-- Generics
--

-- | A newtype wrapper to be used with @DerivingVia@.
--
-- Usage:
--
-- > data Bar = Bar !Int !Int
-- >   deriving Generic
-- >   deriving Unboxable via Generics Bar
newtype Generics a = Generics a

instance (GHC.Generics.Generic a, U.Unbox (Rep' (GHC.Generics.Rep a)), Unboxable' (GHC.Generics.Rep a)) => Unboxable (Generics a) where
  type Rep (Generics a) = Rep' (GHC.Generics.Rep a)
  type CoercibleRep (Generics a) = a
  type IsTrivial (Generics a) = 'False
  unboxingFrom (Generics x) = from' (GHC.Generics.from x)
  {-# INLINE unboxingFrom #-}
  unboxingTo y = Generics (GHC.Generics.to (to' y))
  {-# INLINE unboxingTo #-}

class Unboxable' f where
  type Rep' f
  from' :: f x -> Rep' f
  to' :: Rep' f -> f x
instance Unboxable' GHC.Generics.U1 where
  type Rep' GHC.Generics.U1 = ()
  from' _ = ()
  to' _ = GHC.Generics.U1
  {-# INLINE from' #-}
  {-# INLINE to' #-}
instance Unboxable c => Unboxable' (GHC.Generics.K1 i c) where
  type Rep' (GHC.Generics.K1 i c) = Rep c
  from' = {- from . GHC.Generics.unK1 -} coerce (unboxingFrom :: c -> Rep c)
  to' = {- GHC.Generics.K1 . to -} coerce (unboxingTo :: Rep c -> c)
  {-# INLINE from' #-}
  {-# INLINE to' #-}
instance Unboxable' f => Unboxable' (GHC.Generics.M1 i c f) where
  type Rep' (GHC.Generics.M1 i c f) = Rep' f
  from' = from' . GHC.Generics.unM1
  to' = GHC.Generics.M1 . to'
  {-# INLINE from' #-}
  {-# INLINE to' #-}
instance (Unboxable' f, Unboxable' g) => Unboxable' (f GHC.Generics.:*: g) where
  type Rep' (f GHC.Generics.:*: g) = (Rep' f, Rep' g)
  from' (x GHC.Generics.:*: y) = (from' x, from' y)
  to' (x, y) = (to' x GHC.Generics.:*: to' y)
  {-# INLINE from' #-}
  {-# INLINE to' #-}
instance Unboxable' (f GHC.Generics.:+: g) where
  type Rep' (f GHC.Generics.:+: g) = TypeError ('Text "Cannot derive Unboxable instance for a sum type.")
  from' = undefined
  to' = undefined

--
-- Enum
--

-- | A newtype wrapper to be used with @DerivingVia@.
-- The value will be stored as 'Int', via `fromEnum`/`toEnum`.
--
-- Usage:
--
-- > data Direction = North | South | East | West
-- >   deriving Enum
-- >   deriving Data.Vector.Unboxing.Unboxable via Data.Vector.Unboxing.Enum Bar
newtype Enum a = Enum a
instance (Prelude.Enum a) => Unboxable (Enum a) where
  type Rep (Enum a) = Int
  type CoercibleRep (Enum a) = a
  type IsTrivial (Enum a) = 'False
  unboxingFrom (Enum x) = fromEnum x
  {-# INLINE unboxingFrom #-}
  unboxingTo y = Enum (toEnum y)
  {-# INLINE unboxingTo #-}

-- | A newtype wrapper to be used with @DerivingVia@.
--
-- Usage:
--
-- > data Direction = North | South | East | West
-- >   deriving Enum
-- >   deriving Data.Vector.Unboxing.Unboxable via Data.Vector.Unboxing.EnumRep Int8 Bar
newtype EnumRep rep a = EnumRep a
instance (Prelude.Enum a, Integral rep, U.Unbox rep) => Unboxable (EnumRep rep a) where
  type Rep (EnumRep rep a) = rep
  type CoercibleRep (EnumRep rep a) = a
  type IsTrivial (EnumRep rep a) = 'False
  unboxingFrom (EnumRep x) = fromIntegral (fromEnum x)
  {-# INLINE unboxingFrom #-}
  unboxingTo y = EnumRep (toEnum (fromIntegral y))
  {-# INLINE unboxingTo #-}

--
-- Instances
--

instance (Unboxable a) => IsList (Vector a) where
  type Item (Vector a) = a
  fromList = G.fromList
  fromListN = G.fromListN
  toList = G.toList
  {-# INLINE fromList #-}
  {-# INLINE fromListN #-}
  {-# INLINE toList #-}

instance (Eq a, Unboxable a) => Eq (Vector a) where
  xs == ys = Bundle.eq (G.stream xs) (G.stream ys)
  xs /= ys = not (Bundle.eq (G.stream xs) (G.stream ys))
  {-# INLINE (==) #-}
  {-# INLINE (/=) #-}

instance (Ord a, Unboxable a) => Ord (Vector a) where
  compare xs ys = Bundle.cmp (G.stream xs) (G.stream ys)
  {-# INLINE compare #-}

instance (Show a, Unboxable a) => Show (Vector a) where
  showsPrec = G.showsPrec
  {-# INLINE showsPrec #-}

instance (Read a, Unboxable a) => Read (Vector a) where
  readPrec = G.readPrec
  readListPrec = readListPrecDefault
  {-# INLINE readPrec #-}
  {-# INLINE readListPrec #-}

instance (Unboxable a) => Data.Semigroup.Semigroup (Vector a) where
  (<>) = (G.++)
  sconcat = G.concatNE
  {-# INLINE (<>) #-}
  {-# INLINE sconcat #-}

instance (Unboxable a) => Data.Monoid.Monoid (Vector a) where
  mempty = G.empty
  mappend = (Data.Semigroup.<>)
  mconcat = G.concat
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}
  {-# INLINE mconcat #-}

instance NFData (Vector a) where
  rnf !_ = () -- the content is unboxed

instance (Unboxable a) => GM.MVector MVector a where
  basicLength (UnboxingMVector mv)                           = GM.basicLength mv
  basicUnsafeSlice i l (UnboxingMVector mv)                  = UnboxingMVector (GM.basicUnsafeSlice i l mv)
  basicOverlaps (UnboxingMVector mv) (UnboxingMVector mv')   = GM.basicOverlaps mv mv'
  basicUnsafeNew l                                           = UnboxingMVector <$> GM.basicUnsafeNew l
  basicInitialize (UnboxingMVector mv)                       = GM.basicInitialize mv
  basicUnsafeReplicate i x                                   = UnboxingMVector <$> GM.basicUnsafeReplicate i (unboxingFrom x)
  basicUnsafeRead (UnboxingMVector mv) i                     = unboxingTo <$> GM.basicUnsafeRead mv i
  basicUnsafeWrite (UnboxingMVector mv) i x                  = GM.basicUnsafeWrite mv i (unboxingFrom x)
  basicClear (UnboxingMVector mv)                            = GM.basicClear mv
  basicSet (UnboxingMVector mv) x                            = GM.basicSet mv (unboxingFrom x)
  basicUnsafeCopy (UnboxingMVector mv) (UnboxingMVector mv') = GM.basicUnsafeCopy mv mv'
  basicUnsafeMove (UnboxingMVector mv) (UnboxingMVector mv') = GM.basicUnsafeMove mv mv'
  basicUnsafeGrow (UnboxingMVector mv) n                     = UnboxingMVector <$> GM.basicUnsafeGrow mv n
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeMove #-}
  {-# INLINE basicUnsafeGrow #-}

instance (Unboxable a) => G.Vector Vector a where
  basicUnsafeFreeze (UnboxingMVector mv)                  = UnboxingVector <$> G.basicUnsafeFreeze mv
  basicUnsafeThaw (UnboxingVector v)                      = UnboxingMVector <$> G.basicUnsafeThaw v
  basicLength (UnboxingVector v)                          = G.basicLength v
  basicUnsafeSlice i l (UnboxingVector v)                 = UnboxingVector (G.basicUnsafeSlice i l v)
  basicUnsafeIndexM (UnboxingVector v) i                  = unboxingTo <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (UnboxingMVector mv) (UnboxingVector v) = G.basicUnsafeCopy mv v
  elemseq (UnboxingVector v) x y                          = G.elemseq v (unboxingFrom x) y -- ?
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE elemseq #-}

--
-- Unboxable instances
--

instance Unboxable Bool where   type Rep Bool = Bool
instance Unboxable Char where   type Rep Char = Char
instance Unboxable Double where type Rep Double = Double
instance Unboxable Float where  type Rep Float = Float
instance Unboxable Int where    type Rep Int = Int
instance Unboxable Int8 where   type Rep Int8 = Int8
instance Unboxable Int16 where  type Rep Int16 = Int16
instance Unboxable Int32 where  type Rep Int32 = Int32
instance Unboxable Int64 where  type Rep Int64 = Int64
instance Unboxable Word where   type Rep Word = Word
instance Unboxable Word8 where  type Rep Word8 = Word8
instance Unboxable Word16 where type Rep Word16 = Word16
instance Unboxable Word32 where type Rep Word32 = Word32
instance Unboxable Word64 where type Rep Word64 = Word64
instance Unboxable () where     type Rep () = ()

instance (Unboxable a) => Unboxable (Data.Complex.Complex a) where
  type Rep (Data.Complex.Complex a) = Data.Complex.Complex (Rep a)
  type CoercibleRep (Data.Complex.Complex a) = Data.Complex.Complex (CoercibleRep a)
  type IsTrivial (Data.Complex.Complex a) = IsTrivial a
  unboxingFrom = fmap unboxingFrom
  unboxingTo = fmap unboxingTo
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}

instance (Unboxable a, Unboxable b) => Unboxable (a, b) where
  type Rep (a, b) = (Rep a, Rep b)
  type CoercibleRep (a, b) = (CoercibleRep a, CoercibleRep b)
  type IsTrivial (a, b) = IsTrivial a && IsTrivial b
  unboxingFrom (a, b) = (unboxingFrom a, unboxingFrom b)
  unboxingTo (a, b) = (unboxingTo a, unboxingTo b)
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}

instance (Unboxable a, Unboxable b, Unboxable c) => Unboxable (a, b, c) where
  type Rep (a, b, c) = (Rep a, Rep b, Rep c)
  type CoercibleRep (a, b, c) = (CoercibleRep a, CoercibleRep b, CoercibleRep c)
  type IsTrivial (a, b, c) = IsTrivial a && IsTrivial b && IsTrivial c
  unboxingFrom (a, b, c) = (unboxingFrom a, unboxingFrom b, unboxingFrom c)
  unboxingTo (a, b, c) = (unboxingTo a, unboxingTo b, unboxingTo c)
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}

instance (Unboxable a, Unboxable b, Unboxable c, Unboxable d) => Unboxable (a, b, c, d) where
  type Rep (a, b, c, d) = (Rep a, Rep b, Rep c, Rep d)
  type CoercibleRep (a, b, c, d) = (CoercibleRep a, CoercibleRep b, CoercibleRep c, CoercibleRep d)
  type IsTrivial (a, b, c, d) = IsTrivial a && IsTrivial b && IsTrivial c && IsTrivial d
  unboxingFrom (a, b, c, d) = (unboxingFrom a, unboxingFrom b, unboxingFrom c, unboxingFrom d)
  unboxingTo (a, b, c, d) = (unboxingTo a, unboxingTo b, unboxingTo c, unboxingTo d)
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}

instance (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e) => Unboxable (a, b, c, d, e) where
  type Rep (a, b, c, d, e) = (Rep a, Rep b, Rep c, Rep d, Rep e)
  type CoercibleRep (a, b, c, d, e) = (CoercibleRep a, CoercibleRep b, CoercibleRep c, CoercibleRep d, CoercibleRep e)
  type IsTrivial (a, b, c, d, e) = IsTrivial a && IsTrivial b && IsTrivial c && IsTrivial d && IsTrivial e
  unboxingFrom (a, b, c, d, e) = (unboxingFrom a, unboxingFrom b, unboxingFrom c, unboxingFrom d, unboxingFrom e)
  unboxingTo (a, b, c, d, e) = (unboxingTo a, unboxingTo b, unboxingTo c, unboxingTo d, unboxingTo e)
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}

instance (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e, Unboxable f) => Unboxable (a, b, c, d, e, f) where
  type Rep (a, b, c, d, e, f) = (Rep a, Rep b, Rep c, Rep d, Rep e, Rep f)
  type CoercibleRep (a, b, c, d, e, f) = (CoercibleRep a, CoercibleRep b, CoercibleRep c, CoercibleRep d, CoercibleRep e, CoercibleRep f)
  type IsTrivial (a, b, c, d, e, f) = IsTrivial a && IsTrivial b && IsTrivial c && IsTrivial d && IsTrivial e && IsTrivial f
  unboxingFrom (a, b, c, d, e, f) = (unboxingFrom a, unboxingFrom b, unboxingFrom c, unboxingFrom d, unboxingFrom e, unboxingFrom f)
  unboxingTo (a, b, c, d, e, f) = (unboxingTo a, unboxingTo b, unboxingTo c, unboxingTo d, unboxingTo e, unboxingTo f)
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}

{-
GND for type classes with associated type families is not available until GHC 8.2.
With GHC 8.2 or later, one can derive these instances like:

> deriving instance Unboxable a => Unboxable (Data.Functor.Identity.Identity a)
> deriving instance Unboxable a => Unboxable (Data.Functor.Const.Const a b)
> deriving instance Unboxable a => Unboxable (Data.Semigroup.Min a)
> deriving instance Unboxable a => Unboxable (Data.Semigroup.Max a)
> deriving instance Unboxable a => Unboxable (Data.Semigroup.First a)
> deriving instance Unboxable a => Unboxable (Data.Semigroup.Last a)
> deriving instance Unboxable a => Unboxable (Data.Semigroup.WrappedMonoid a)
> deriving instance Unboxable a => Unboxable (Data.Monoid.Dual a)
> deriving instance Unboxable Data.Monoid.All
> deriving instance Unboxable Data.Monoid.Any
> deriving instance Unboxable a => Unboxable (Data.Monoid.Sum a)
> deriving instance Unboxable a => Unboxable (Data.Monoid.Product a)
> deriving instance Unboxable a => Unboxable (Data.Ord.Down a)
-}

instance Unboxable a => Unboxable (Data.Functor.Identity.Identity a) where
  type Rep (Data.Functor.Identity.Identity a) = Rep a
  unboxingFrom = unboxingFrom .# Data.Functor.Identity.runIdentity
  unboxingTo = Data.Functor.Identity.Identity #. unboxingTo
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}

instance Unboxable a => Unboxable (Data.Functor.Const.Const a b) where
  type Rep (Data.Functor.Const.Const a b) = Rep a
  unboxingFrom = unboxingFrom .# Data.Functor.Const.getConst
  unboxingTo = Data.Functor.Const.Const #. unboxingTo
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}

instance Unboxable a => Unboxable (Data.Semigroup.Min a) where
  type Rep (Data.Semigroup.Min a) = Rep a
  unboxingFrom = unboxingFrom .# Data.Semigroup.getMin
  unboxingTo = Data.Semigroup.Min #. unboxingTo
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}

instance Unboxable a => Unboxable (Data.Semigroup.Max a) where
  type Rep (Data.Semigroup.Max a) = Rep a
  unboxingFrom = unboxingFrom .# Data.Semigroup.getMax
  unboxingTo = Data.Semigroup.Max #. unboxingTo
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}

instance Unboxable a => Unboxable (Data.Semigroup.First a) where
  type Rep (Data.Semigroup.First a) = Rep a
  unboxingFrom = unboxingFrom .# Data.Semigroup.getFirst
  unboxingTo = Data.Semigroup.First #. unboxingTo
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}

instance Unboxable a => Unboxable (Data.Semigroup.Last a) where
  type Rep (Data.Semigroup.Last a) = Rep a
  unboxingFrom = unboxingFrom .# Data.Semigroup.getLast
  unboxingTo = Data.Semigroup.Last #. unboxingTo
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}

instance Unboxable a => Unboxable (Data.Semigroup.WrappedMonoid a) where
  type Rep (Data.Semigroup.WrappedMonoid a) = Rep a
  unboxingFrom = unboxingFrom .# Data.Semigroup.unwrapMonoid
  unboxingTo = Data.Semigroup.WrapMonoid #. unboxingTo
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}

instance Unboxable a => Unboxable (Data.Monoid.Dual a) where
  type Rep (Data.Monoid.Dual a) = Rep a
  unboxingFrom = unboxingFrom .# Data.Monoid.getDual
  unboxingTo = Data.Monoid.Dual #. unboxingTo
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}

instance Unboxable Data.Monoid.All where
  type Rep Data.Monoid.All = Bool

instance Unboxable Data.Monoid.Any where
  type Rep Data.Monoid.Any = Bool

instance Unboxable a => Unboxable (Data.Monoid.Sum a) where
  type Rep (Data.Monoid.Sum a) = Rep a
  unboxingFrom = unboxingFrom .# Data.Monoid.getSum
  unboxingTo = Data.Monoid.Sum #. unboxingTo
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}

instance Unboxable a => Unboxable (Data.Monoid.Product a) where
  type Rep (Data.Monoid.Product a) = Rep a
  unboxingFrom = unboxingFrom .# Data.Monoid.getProduct
  unboxingTo = Data.Monoid.Product #. unboxingTo
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}

instance Unboxable a => Unboxable (Data.Ord.Down a) where
  type Rep (Data.Ord.Down a) = Rep a
  unboxingFrom = unboxingFrom .# (\(Data.Ord.Down x) -> x)
  unboxingTo = Data.Ord.Down #. unboxingTo
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}

instance Unboxable Ordering where
  type Rep Ordering = Int8
  unboxingFrom x = fromIntegral (fromEnum x)
  unboxingTo y = toEnum (fromIntegral y)
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}

instance (Unboxable a, Unboxable b) => Unboxable (Data.Semigroup.Arg a b) where
  type Rep (Data.Semigroup.Arg a b) = (Rep a, Rep b)
  unboxingFrom (Data.Semigroup.Arg x y) = (unboxingFrom x, unboxingFrom y)
  unboxingTo (x, y) = Data.Semigroup.Arg (unboxingTo x) (unboxingTo y)
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}

instance Unboxable (f a) => Unboxable (Data.Monoid.Alt f a) where
  type Rep (Data.Monoid.Alt f a) = Rep (f a)
  unboxingFrom = unboxingFrom .# Data.Monoid.getAlt
  unboxingTo = Data.Monoid.Alt #. unboxingTo
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}

{-
-- Since base-4.12.0.0
instance Unboxable (f a) => Unboxable (Data.Monoid.Ap f a) where
  type Rep (Data.Monoid.Ap f a) = Rep (f a)
  unboxingFrom = unboxingFrom .# Data.Monoid.getAp
  unboxingTo = Data.Monoid.Ap #. unboxingTo
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}
-}

instance Unboxable (f (g a)) => Unboxable (Data.Functor.Compose.Compose f g a) where
  type Rep (Data.Functor.Compose.Compose f g a) = Rep (f (g a))
  unboxingFrom = unboxingFrom .# Data.Functor.Compose.getCompose
  unboxingTo = Data.Functor.Compose.Compose #. unboxingTo
  {-# INLINE unboxingFrom #-}
  {-# INLINE unboxingTo #-}

infixr 9 #.
infixl 8 .#

(#.) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
_ #. f = coerce f
{-# INLINE (#.) #-}

(.#) :: Coercible a b => (b -> c) -> (a -> b) -> a -> c
f .# _ = coerce f
{-# INLINE (.#) #-}
