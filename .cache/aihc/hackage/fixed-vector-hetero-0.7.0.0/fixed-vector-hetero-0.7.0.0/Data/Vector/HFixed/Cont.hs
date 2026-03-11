{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- CPS encoded heterogeneous vectors.
module Data.Vector.HFixed.Cont (
    -- * CPS-encoded vector
    -- ** Type classes
    Fn
  , Fun
  , TFun(..)
  , Arity(..)
  , HVector(..)
  , tupleSize
  , HVectorF(..)
  , tupleSizeF
  , ValueAt
  , Index
    -- ** CPS-encoded vector
  , ContVec
  , ContVecF(..)
    -- ** Other data types
  , VecList(..)
  , VecListF(..)
    -- * Conversion to/from vector
  , cvec
  , vector
  , cvecF
  , vectorF
    -- * Generic API for tuples
    -- ** Position based functions
  , head
  , tail
  , cons
  , consF
  , concat
    -- ** Indexing
  , index
  , set
  , tyLookup
  , tyLookupF
    -- ** Folds and unfolds
  , foldlF
  , foldrF
  , foldMapF
  , foldlNatF
  , foldrNatF
  , foldMapNatF
  , unfoldrF
    -- ** Replicate variants
  , replicateF
  , replicateNatF
    -- ** Zip variants
  , zipWithF
  , zipWithNatF
  , zipFoldF
    -- ** Monomorphization of vectors
  , monomorphizeF
    -- ** Manipulation with type constructor
  , map
  , mapNat
  , sequenceF
  , distributeF
  ) where

import Control.Applicative   (Applicative(..),Const(..))
import Data.Monoid           (Monoid(..),(<>))
import Data.Functor.Compose  (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.Kind             (Type)
import qualified Data.Vector.Fixed.Cont as F
import Prelude               (Functor(..),id,(.),($))
#if MIN_VERSION_base(4,17,0)
import Prelude (type(~))
#endif
import Data.Vector.HFixed.Class



----------------------------------------------------------------
-- Conversions between vectors
----------------------------------------------------------------

-- | Convert heterogeneous vector to CPS form
cvec :: (HVector v, Elems v ~ xs) => v -> ContVec xs
cvec v = ContVecF (inspect v)
{-# INLINE cvec #-}

-- | Convert CPS-vector to heterogeneous vector
vector :: (HVector v, Elems v ~ xs) => ContVec xs -> v
vector (ContVecF cont) = cont construct
{-# INLINE vector #-}

cvecF :: HVectorF v => v f -> ContVecF (ElemsF v) f
cvecF v = ContVecF (inspectF v)
{-# INLINE cvecF #-}

vectorF :: HVectorF v => ContVecF (ElemsF v) f -> v f
vectorF (ContVecF cont) = cont constructF
{-# INLINE vectorF #-}



----------------------------------------------------------------
-- Position based functions
----------------------------------------------------------------

-- | Head of vector
head :: Arity xs => ContVec (x : xs) -> x
head v = inspect v (uncurryFun pure)
{-# INLINE head #-}

-- | Tail of CPS-encoded vector
tail :: ContVec (x : xs) -> ContVec xs
tail (ContVecF cont) = ContVecF $ cont . constFun
{-# INLINE tail #-}

-- | Concatenate two vectors
concat :: Arity xs => ContVec xs -> ContVec ys -> ContVec (xs ++ ys)
concat (ContVecF contX) (ContVecF contY) = ContVecF $ contY . contX . curryMany
{-# INLINE concat #-}

-- | Get value at @n@th position.
index :: Index n xs => ContVec xs -> proxy n -> ValueAt n xs
index (ContVecF cont) = cont . getF
{-# INLINE index #-}

-- | Set value on nth position.
set :: Index n xs => proxy n -> ValueAt n xs -> ContVec xs -> ContVec xs
set n x (ContVecF cont) = ContVecF $ cont . putF n x
{-# INLINE set #-}

-- | Lookup value by its type
tyLookup :: TyLookup a xs => ContVec xs -> a
tyLookup (ContVecF cont) = runIdentity $ cont lookupTFun
{-# INLINE tyLookup #-}

-- | Lookup value by its type
tyLookupF :: TyLookup a xs => ContVecF xs f -> f a
tyLookupF (ContVecF cont) = cont lookupTFun
{-# INLINE tyLookupF #-}


----------------------------------------------------------------
-- Monadic/applicative API
----------------------------------------------------------------

-- | Apply transformation to every element of the tuple.
map :: (ArityC c xs)
    => Proxy c
    -> (forall a. c a => f a -> g a)
    -> ContVecF xs f
    -> ContVecF xs g
map cls f (ContVecF cont) = ContVecF $ cont . mapF cls f
{-# INLINE map #-}

mapF :: forall c f g r xs. (ArityC c xs)
     => Proxy c
     -> (forall a. c a => f a -> g a)
     -> TFun g xs r
     -> TFun f xs r
mapF cls g (TFun f0) = accumC cls
  (\(TF_map f) a -> TF_map $ f (g a))
  (\(TF_map r)   -> r)
  (TF_map f0 :: TF_map r g xs)

-- | Apply natural transformation to every element of the tuple.
mapNat :: (Arity xs)
       => (forall a. f a -> g a)
       -> ContVecF xs f
       -> ContVecF xs g
mapNat f (ContVecF cont) = ContVecF $ cont . mapFF f
{-# INLINE mapNat #-}

mapFF :: (Arity xs)
      => (forall a. f a -> g a) -> TFun g xs r -> TFun f xs r
{-# INLINE mapFF #-}
mapFF g (TFun f0) = accum
  (\(TF_map f) a -> TF_map $ f (g a))
  (\(TF_map r)   -> r)
  (TF_map f0)

newtype TF_map r g xs = TF_map (Fn g xs r)


-- | Apply sequence to outer level of parametrized tuple elements.
sequenceF :: (Arity xs, Applicative f)
          => ContVecF xs (f `Compose` g) -> f (ContVecF xs g)
sequenceF (ContVecF cont)
  = cont $ sequenceF_F constructF
{-# INLINE sequenceF #-}

sequenceF_F :: (Applicative f, Arity xs)
            => TFun g xs r -> TFun (f `Compose` g) xs (f r)
{-# INLINE sequenceF_F #-}
sequenceF_F (TFun f) =
  accum (\(T_seq2 m) (Compose a) -> T_seq2 $ m <*> a)
        (\(T_seq2 m)             -> m)
        (T_seq2 (pure f))

newtype T_seq2 f g r xs = T_seq2 (f (Fn g xs r))



distributeF :: forall f g xs. (Arity xs, Functor f)
            => f (ContVecF xs g)
            -> ContVecF xs (f `Compose` g)
{-# INLINE distributeF #-}
distributeF f0
  = apply step start
  where
    step :: forall a as. T_distributeF f g (a : as) -> ((Compose f g) a, T_distributeF f g as)
    step (T_distributeF v) = ( Compose $ fmap (\(ConsF x _) -> x) v
                             , T_distributeF $ fmap (\(ConsF _ x) -> x) v
                             )
    start :: T_distributeF f g xs
    start = T_distributeF $ fmap vectorF f0

newtype T_distributeF f g xs = T_distributeF (f (VecListF xs g))



----------------------------------------------------------------
-- Other vectors
----------------------------------------------------------------

-- | List like heterogeneous vector.
data VecList :: [Type] -> Type where
  Nil  :: VecList '[]
  Cons :: x -> VecList xs -> VecList (x : xs)

instance Arity xs => HVector (VecList xs) where
  type Elems (VecList xs) = xs
  construct = accum
    (\(T_List f) (Identity a) -> T_List (f . Cons a))
    (\(T_List f)              -> f Nil)
    (T_List id)
  inspect f = runContVecF (apply step f)
    where
      step :: VecList (a : as) -> (Identity a, VecList as)
      step (Cons a xs) = (Identity a, xs)
  {-# INLINE construct #-}
  {-# INLINE inspect   #-}

newtype T_List all xs = T_List (VecList xs -> VecList all)


-- | List-like vector
data VecListF (xs :: [α]) (f :: α -> Type) where
  NilF  :: VecListF '[] f
  ConsF :: f x -> VecListF xs f -> VecListF (x : xs) f

instance Arity xs => HVectorF (VecListF xs) where
  type ElemsF (VecListF xs) = xs
  constructF   = conVecF
  inspectF   v = inspectF (apply step (TF_insp v))
    where
      step :: TF_insp f (a : as) -> (f a, TF_insp f as)
      step (TF_insp (ConsF a xs)) = (a, TF_insp xs)
  {-# INLINE constructF #-}
  {-# INLINE inspectF   #-}

conVecF :: (Arity xs) => TFun f xs (VecListF xs f)
{-# INLINE conVecF #-}
conVecF = accum (\(TF_List f) a -> TF_List (f . ConsF a))
                (\(TF_List f)   -> f NilF)
                (TF_List id)

newtype TF_insp f     xs = TF_insp (VecListF xs f)
newtype TF_List f all xs = TF_List (VecListF xs f -> VecListF all f)



----------------------------------------------------------------
-- More combinators
----------------------------------------------------------------

replicateNatF :: Arity xs => (forall a. f a) -> ContVecF xs f
{-# INLINE replicateNatF #-}
replicateNatF f = apply (\Proxy -> (f, Proxy)) (Proxy)

replicateF :: ArityC c xs => Proxy c -> (forall a. c a => f a) -> ContVecF xs f
{-# INLINE replicateF #-}
replicateF cls f = applyC cls (\Proxy -> (f,Proxy)) Proxy



----------------------------------------------------------------
-- Folds
----------------------------------------------------------------

-- | Monoidal fold over vector
foldMapNatF
  :: (Monoid m, Arity xs)
  => (forall a. f a -> m) -> ContVecF xs f -> m
{-# INLINE foldMapNatF #-}
foldMapNatF f v
  = inspectF v
  $ accum (\(Const m) a -> Const (m <> f a))
          (\(Const m)   -> m)
          (Const mempty)

-- | Monoidal fold over vector
foldMapF
  :: (Monoid m, ArityC c xs)
  => Proxy c -> (forall a. c a => f a -> m) -> ContVecF xs f -> m
{-# INLINE foldMapF #-}
foldMapF cls f v
  = inspectF v
  $ accumC cls (\(Const m) a -> Const (m <> f a))
               (\(Const m)   -> m)
               (Const mempty)

-- | Right fold over vector
foldrF :: (ArityC c xs)
       => Proxy c -> (forall a. c a => f a -> b -> b) -> b -> ContVecF xs f -> b
{-# INLINE foldrF #-}
foldrF cls f b0 v
  = inspectF v
  $ accumC cls (\(Const b) a -> Const (b . f a))
               (\(Const b)   -> b b0)
               (Const id)

-- | Left fold over vector
foldlF :: (ArityC c xs)
       => Proxy c -> (forall a. c a => b -> f a -> b) -> b -> ContVecF xs f -> b
{-# INLINE foldlF #-}
foldlF cls f b0 v
  = inspectF v
  $ accumC cls (\(Const b) a -> Const (f b a))
               (\(Const b)   -> b)
               (Const b0)

-- | Right fold over vector
foldrNatF :: (Arity xs)
       => (forall a. f a -> b -> b) -> b -> ContVecF xs f -> b
{-# INLINE foldrNatF #-}
foldrNatF f b0 v
  = inspectF v
  $ accum (\(Const b) a -> Const (b . f a))
          (\(Const b)   -> b b0)
          (Const id)

-- | Left fold over vector
foldlNatF :: (Arity xs)
          => (forall a. b -> f a -> b) -> b -> ContVecF xs f -> b
{-# INLINE foldlNatF #-}
foldlNatF f b0 v
  = inspectF v
  $ accum (\(Const b) a -> Const (f b a))
          (\(Const b)   -> b)
          (Const b0)

-- | Convert heterogeneous vector to homogeneous
monomorphizeF :: forall c xs a f. ( ArityC c xs )
              => Proxy c -> (forall x. c x => f x -> a)
              -> ContVecF xs f -> F.ContVec (Len xs) a
{-# INLINE monomorphizeF #-}
monomorphizeF cls f v
  = inspectF v
  $ accumC cls (\(T_mono cont) a -> T_mono (cont . F.consPeano (f a)))
               (\(T_mono cont)   -> cont (F.ContVec F.unFun))
               (T_mono id :: T_mono a xs xs)

data T_mono a all xs = T_mono (F.ContVec (Len xs) a -> F.ContVec (Len all) a)


-- | Unfold vector.
unfoldrF :: (ArityC c xs)
         => Proxy c
         -> (forall a. c a => b -> (f a, b))
         -> b
         -> ContVecF xs f
{-# INLINE unfoldrF #-}
unfoldrF cls f b0 = applyC cls
  (\(Const b) -> let (a,b') = f b in (a, Const b'))
  (Const b0)



----------------------------------------------------------------
-- Zip variants
----------------------------------------------------------------

-- | Zip two heterogeneous vectors
zipWithF :: (ArityC c xs)
         => Proxy c
         -> (forall a. c a => f a -> g a -> h a)
         -> ContVecF xs f
         -> ContVecF xs g
         -> ContVecF xs h
{-# INLINE zipWithF #-}
zipWithF cls f cvecA cvecB = applyC cls
  (\(T_zipWithF (ConsF a va) (ConsF b vb)) ->
      (f a b, T_zipWithF va vb))
  (T_zipWithF (vectorF cvecA) (vectorF cvecB))

data T_zipWithF f g xs = T_zipWithF (VecListF xs f) (VecListF xs g)


-- | Zip vector and fold result using monoid
zipFoldF :: forall xs c m f. (ArityC c xs, Monoid m)
         => Proxy c
         -> (forall a. c a => f a -> f a -> m)
         -> ContVecF xs f
         -> ContVecF xs f
         -> m
{-# INLINE zipFoldF #-}
zipFoldF cls f cvecA cvecB
  = inspectF cvecB zipF
  where
    zipF :: TFun f xs m
    zipF = accumC cls
      (\(T_zipFoldF (ConsF a va) m) b -> T_zipFoldF va (m <> f a b))
      (\(T_zipFoldF _            m)   -> m)
      (T_zipFoldF (vectorF cvecA) mempty :: T_zipFoldF m f xs)

data T_zipFoldF m f xs = T_zipFoldF (VecListF xs f) m


-- | Zip two heterogeneous vectors
zipWithNatF :: (Arity xs)
            => (forall a. f a -> g a -> h a)
            -> ContVecF xs f
            -> ContVecF xs g
            -> ContVecF xs h
{-# INLINE zipWithNatF #-}
zipWithNatF f cvecA cvecB
  = apply (\(T_zipNatF (ConsF a va) (ConsF b vb)) -> (f a b, T_zipNatF va vb))
          (T_zipNatF (vectorF cvecA) (vectorF cvecB))

data T_zipNatF f g xs = T_zipNatF (VecListF xs f) (VecListF xs g)
