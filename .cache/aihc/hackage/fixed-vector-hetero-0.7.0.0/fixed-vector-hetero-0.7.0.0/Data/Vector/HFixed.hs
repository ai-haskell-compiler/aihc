{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- This module provides function for working with product types and
-- comes in two variants. First works with plain product, types like
-- @(a,b)@ or @data Prod = Prod A B@, etc. Second one is for
-- parameterized products (it seems there's no standard name for
-- them), that is types like: @data ProdF f = ProdF (f Int) (f Char)@.
--
-- Most examples in this module use tuple but library is not limited
-- to them in any way. They're just in base and convenient to work
-- with.
module Data.Vector.HFixed (
    -- * HVector type classes
    HVector(..)
  , tupleSize
  , HVectorF(..)
  , tupleSizeF
  , ContVec
  , ContVecF(..)
  , asCVec
  , asCVecF
    -- * Plain product types
    -- ** Construction
    -- *** Simple constructor
    -- $construction
  , mk0
  , mk1
  , mk2
  , mk3
  , mk4
  , mk5
    -- *** Unfoldr & replicate
  , unfoldr
  , replicate
  , replicateM
  -- ** Position based functions
  , convert
  , head
  , tail
  , cons
  , concat
    -- *** Indexing
  , ValueAt
  , Index
  , index
  , set
  , element
  , elementCh
  , tyLookup
  , tyLookupF
    -- ** Folds & unfolds
  , foldr
  , foldl
  , foldMap
  , mapM_
    -- ** Zips
  , zipWith
  , zipFold
    -- ** Specializations
  , eq
  , compare
  , rnf
    -- * Parametrized products
    -- ** Construction
    -- *** Simple constructors
    -- $construction_F
  , mk0F
  , mk1F
  , mk2F
  , mk3F
  , mk4F
  , mk5F
    -- *** Unfoldr & replicate
  , unfoldrF
  , replicateF
  , replicateNatF
    -- ** Conversion to\/from products
  , wrap
  , unwrap
  , monomorphize
  , monomorphizeF
    -- ** Functor\/Applicative like
  , map
  , mapNat
  , sequence
  , sequence_
  , sequenceF
  , distribute
  , distributeF
    -- ** Folds and unfolds
  , foldrF
  , foldlF
  , foldMapF
  , foldrNatF
  , foldlNatF
  , foldMapNatF
    -- ** Zips
  , zipWithF
  , zipWithNatF
  , zipFoldF
    -- ** Reexports
  , Arity
  , ArityC
  , Proxy(..)
  ) where

import Control.Applicative  (Applicative(..),(<$>))
import qualified Control.DeepSeq as NF

import Data.Coerce           (coerce)
import Data.Functor.Compose  (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.Monoid           (Monoid,All(..))
import Prelude ( Functor(..),Eq(..),Ord,Bool,Ordering
               , id,(.),($),seq)
import qualified Prelude
#if MIN_VERSION_base(4,17,0)
import Prelude (type(~))
#endif

import           Data.Vector.HFixed.Class hiding (cons,consF)
import           Data.Vector.Fixed.Cont       (Peano)
import qualified Data.Vector.Fixed          as F
import qualified Data.Vector.HFixed.Cont    as C


----------------------------------------------------------------
-- Generic API
----------------------------------------------------------------

-- | Restrict type of vector to 'ContVec'. This function is useful for
--   resolving type ambiguity when composing functions. For example
--   following code would not compile because intermediate type is
--   ambiguous:
--
-- > cons 'a' . tail
--
--   GHC cannot guess what type should be produced by @tail@. However
--   we can fix type of intermediate vector with @asCVec@, so code
--   below will work just fine:
--
-- > cons 'a' . asCVec . tail
asCVec :: ContVec xs -> ContVec xs
asCVec = id

asCVecF :: ContVecF f xs -> ContVecF f xs
asCVecF = id

-- | We can convert between any two vector which have same
--   structure but different representations.
--
-- >>> convert (1 :+ 2) :: (Double,Double)
-- (1.0,2.0)
convert :: (HVector v, HVector w, Elems v ~ Elems w)
        => v -> w
{-# INLINE convert #-}
convert v = inspect v construct

-- | Tail of the vector. Note that in the example we only tell GHC
--   that resulting value is 2-tuple via pattern matching and let
--   typechecker figure out the rest.
--
-- >>> case tail ('a',"aa",()) of x@(_,_) -> x
-- ("aa",())
tail :: (HVector v, HVector w, (a : Elems w) ~ Elems v)
     => v -> w
{-# INLINE tail #-}
tail = C.vector . C.tail . C.cvec


-- | Head of the vector
--
-- >>> head ('a',"ABC")
-- 'a'
head :: (HVector v, Elems v ~ (a : as), Arity as)
     => v -> a
{-# INLINE head #-}
head = C.head . C.cvec

-- | Prepend element to the product.
--
-- >>> cons 'c' ('d','e') :: (Char,Char,Char)
-- ('c','d','e')
cons :: (HVector v, HVector w, Elems w ~ (a : Elems v))
     => a -> v -> w
{-# INLINE cons #-}
cons a = C.vector . C.cons a . C.cvec

-- | Concatenate two vectors
--
-- >>> concat ('c','d') ('e','f') :: (Char,Char,Char,Char)
-- ('c','d','e','f')
concat :: ( HVector v, HVector u, HVector w
          , Elems w ~ (Elems v ++ Elems u)
          )
       => v -> u -> w
concat v u = C.vector $ C.concat (C.cvec v) (C.cvec u)
{-# INLINE concat #-}



----------------------------------------------------------------
-- Indexing
----------------------------------------------------------------

-- | Index heterogeneous vector.
--
-- >>> index (Proxy @0) ('c',"str")
-- 'c'
-- >>> index (Proxy @1) ('c',"str")
-- "str"
index
  :: forall n v proxy. (Index (Peano n) (Elems v), HVector v)
  => proxy n                     -- ^ Type level index
  -> v                           -- ^ Vector to index
  -> ValueAt (Peano n) (Elems v)
{-# INLINE index #-}
index _ v = C.index (C.cvec v) (Proxy @(Peano n))


-- | Set element in the vector
--
-- >>> set (Proxy @0) 'X' ('_',"str")
-- ('X',"str")
set :: forall n v proxy. (Index (Peano n) (Elems v), HVector v)
    => proxy n                     -- ^ Type level index
    -> ValueAt (Peano n) (Elems v) -- ^ New value at index
    -> v
    -> v
{-# INLINE set #-}
set _ x = C.vector
        . C.set (Proxy @(Peano n)) x
        . C.cvec

-- | Twan van Laarhoven's lens for i'th element.
element :: forall n v proxy.
           ( Index (Peano n) (Elems v)
           , HVector v
           )
        => proxy n              -- ^ Type level index
        -> Lens' v (ValueAt (Peano n) (Elems v))
{-# INLINE element #-}
element _ f v = inspect v
              $ lensF (Proxy @(Peano n)) f construct

-- | Type changing Twan van Laarhoven's lens for i'th element.
elementCh :: forall n v w a b proxy.
             ( Index   (Peano n) (Elems v)
             , ValueAt (Peano n) (Elems v) ~ a
             , HVector v
             , HVector w
             , Elems w ~ NewElems (Peano n) (Elems v) b
             )
          => proxy n            -- ^ Type level index
          -> Lens v w a b
{-# INLINE elementCh #-}
elementCh _ f v = inspect v
                $ lensChF (Proxy @(Peano n)) f construct


-- | Lookup field from product by its type. Product must contain one
--   and only one field of type @a@
--
-- >>> tyLookup ('c',"str") :: Char
-- 'c'
--
-- >>> tyLookup ('c',"str") :: Int
-- ...
--     • Cannot find type:
--       Int
-- ... • In the expression: tyLookup ('c', "str") :: Int
-- ...   In an equation for ‘it’: it = tyLookup ('c', "str") :: Int
-- ...
--
-- >>> tyLookup ('c','c') :: Char
-- ...
--     • Duplicate type found:
--       Char
-- ... • In the expression: tyLookup ('c', 'c') :: Char
-- ...   In an equation for ‘it’: it = tyLookup ('c', 'c') :: Char
-- ...
tyLookup :: (HVector v, TyLookup a (Elems v)) => v -> a
tyLookup = C.tyLookup . C.cvec
{-# INLINE tyLookup #-}

-- | Analog of 'tyLookup' for @HVectorF@
tyLookupF :: (HVectorF v, TyLookup a (ElemsF v)) => v f -> f a
tyLookupF = C.tyLookupF . C.cvecF
{-# INLINE tyLookupF #-}


----------------------------------------------------------------
-- Folds over vector
----------------------------------------------------------------

-- | Right fold over heterogeneous vector
--
-- >>> foldr (Proxy @Show) (\x str -> show x : str) [] (12,'c')
-- ["12","'c'"]
foldr :: (HVector v, ArityC c (Elems v))
      => Proxy c -> (forall a. c a => a -> b -> b) -> b -> v -> b
{-# INLINE foldr #-}
foldr c f b0 = C.foldrF c (\(Identity a) b -> f a b) b0 . C.cvec

-- | Left fold over heterogeneous vector
foldl :: (HVector v, ArityC c (Elems v))
      => Proxy c -> (forall a. c a => b -> a -> b) -> b -> v -> b
{-# INLINE foldl #-}
foldl c f b0 = C.foldlF c (\b (Identity a) -> f b a) b0 . C.cvec

-- | Monoidal fold over heterogeneuous vector
--
-- >>> foldMap (Proxy @Show) show (12,'c',"str")
-- "12'c'\"str\""
foldMap
  :: (HVector v, ArityC c (Elems v), Monoid m)
  => Proxy c -> (forall a. c a => a -> m) -> v -> m
{-# INLINE foldMap #-}
foldMap c f = C.foldMapF c (\(Identity a) -> f a) . C.cvec

-- | Right fold over heterogeneous vector
foldrF :: (HVectorF v, ArityC c (ElemsF v))
       => Proxy c -> (forall a. c a => f a -> b -> b) -> b -> v f -> b
{-# INLINE foldrF #-}
foldrF c f b0 = C.foldrF c f b0 . C.cvecF

-- | Left fold over heterogeneous vector
foldlF :: (HVectorF v, ArityC c (ElemsF v))
       => Proxy c -> (forall a. c a => b -> f a -> b) -> b -> v f -> b
{-# INLINE foldlF #-}
foldlF c f b0 = C.foldlF c f b0 . C.cvecF

-- | Monoidal fold over heterogeneous vector
--
-- >>> foldMapF (Proxy @Show) show (mk2F (Just 1) Nothing :: HVecF '[Int,Char] Maybe)
-- "Just 1Nothing"
foldMapF :: (HVectorF v, ArityC c (ElemsF v), Monoid m)
         => Proxy c -> (forall a. c a => f a -> m) -> v f -> m
{-# INLINE foldMapF #-}
foldMapF c f = C.foldMapF c f . C.cvecF

-- | Right fold over heterogeneous vector
foldrNatF :: (HVectorF v)
          => (forall a. f a -> b -> b) -> b -> v f -> b
{-# INLINE foldrNatF #-}
foldrNatF f b0 = C.foldrNatF f b0 . C.cvecF

-- | Left fold over heterogeneous vector
foldlNatF :: (HVectorF v)
          => (forall a. b -> f a -> b) -> b -> v f -> b
{-# INLINE foldlNatF #-}
foldlNatF f b0 = C.foldlNatF f b0 . C.cvecF

-- | Monoidal fold over heterogeneous vector
--
-- >>> foldMapNatF (Sum . getConst) (mk2F (Const 1) (Const 2) :: HVecF '[Char,String] (Const Int))
-- Sum {getSum = 3}
foldMapNatF :: (HVectorF v, Monoid m)
            => (forall a. f a -> m) -> v f -> m
{-# INLINE foldMapNatF #-}
foldMapNatF f = C.foldMapNatF f . C.cvecF

-- | Apply monadic action to every element in the vector
mapM_ :: (HVector v, ArityC c (Elems v), Applicative f)
      => Proxy c -> (forall a. c a => a -> f ()) -> v -> f ()
{-# INLINE mapM_ #-}
mapM_ c f = foldl c (\m a -> m *> f a) (pure ())

-- | Unfold vector.
unfoldr :: (HVector v, ArityC c (Elems v))
        => Proxy c -> (forall a. c a => b -> (a,b)) -> b -> v
{-# INLINE unfoldr #-}
unfoldr c f = C.vector . C.unfoldrF c (\b -> let (a,b') = f b in (Identity a, b'))

-- | Unfold vector.
unfoldrF :: (HVectorF v, ArityC c (ElemsF v))
        => Proxy c -> (forall a. c a => b -> (f a,b)) -> b -> v f
{-# INLINE unfoldrF #-}
unfoldrF c f = C.vectorF . C.unfoldrF c f



----------------------------------------------------------------
-- Constructors
----------------------------------------------------------------

-- $construction
--
-- Functions below allow to construct products up to 5 elements. Here
-- are example for product types from base:
--
-- >>> mk0 :: ()
-- ()
--
-- >>> mk3 12 'x' "xyz" :: (Int,Char,String)
-- (12,'x',"xyz")
--
-- >>> mk2 0 1 :: Complex Double
-- 0.0 :+ 1.0

mk0 :: forall v. (HVector v, Elems v ~ '[]) => v
mk0 = coerce (construct :: Fun '[] v)
{-# INLINE mk0 #-}

mk1 :: forall v a. (HVector v, Elems v ~ '[a])
    => a -> v
mk1 = coerce (construct :: Fun '[a] v)
{-# INLINE mk1 #-}

mk2 :: forall v a b. (HVector v, Elems v ~ '[a,b])
    => a -> b -> v
mk2 = coerce (construct :: Fun '[a,b] v)
{-# INLINE mk2 #-}

mk3 :: forall v a b c. (HVector v, Elems v ~ '[a,b,c])
    => a -> b -> c -> v
mk3 = coerce (construct :: Fun '[a,b,c] v)
{-# INLINE mk3 #-}

mk4 :: forall v a b c d. (HVector v, Elems v ~ '[a,b,c,d])
    => a -> b -> c -> d -> v
mk4 = coerce (construct :: Fun '[a,b,c,d] v)
{-# INLINE mk4 #-}

mk5 :: forall v a b c d e. (HVector v, Elems v ~ '[a,b,c,d,e])
    => a -> b -> c -> d -> e -> v
mk5 = coerce (construct :: Fun '[a,b,c,d,e] v)
{-# INLINE mk5 #-}


-- $construction_F
--
-- Construction function for parametrized products are fully
-- analogous to plain products:
--
-- >>>mk2F (Identity 'c') (Identity 1) :: HVecF '[Char, Int] Identity
-- [Identity 'c',Identity 1]
--
-- >>>mk2F (Nothing) (Just 1) :: HVecF '[Char, Int] Maybe
-- [Nothing,Just 1]

mk0F :: forall f v. (HVectorF v, ElemsF v ~ '[]) => v f
mk0F = coerce (constructF :: TFun f '[] (v f))
{-# INLINE mk0F #-}

mk1F :: forall f v a. (HVectorF v, ElemsF v ~ '[a])
     => f a -> v f
mk1F = coerce (constructF :: TFun f '[a] (v f))
{-# INLINE mk1F #-}

mk2F :: forall f v a b. (HVectorF v, ElemsF v ~ '[a,b])
     => f a -> f b -> v f
mk2F = coerce (constructF :: TFun f '[a,b] (v f))
{-# INLINE mk2F #-}

mk3F :: forall f v a b c. (HVectorF v, ElemsF v ~ '[a,b,c])
     => f a -> f b -> f c -> v f
mk3F = coerce (constructF :: TFun f '[a,b,c] (v f))
{-# INLINE mk3F #-}

mk4F :: forall f v a b c d. (HVectorF v, ElemsF v ~ '[a,b,c,d])
     => f a -> f b -> f c -> f d -> v f
mk4F = coerce (constructF :: TFun f '[a,b,c,d] (v f))
{-# INLINE mk4F #-}

mk5F :: forall f v a b c d e. (HVectorF v, ElemsF v ~ '[a,b,c,d,e])
     => f a -> f b -> f c -> f d -> f e -> v f
mk5F = coerce (constructF :: TFun f '[a,b,c,d,e] (v f))
{-# INLINE mk5F #-}



----------------------------------------------------------------
-- Collective operations
----------------------------------------------------------------

-- | Apply function to every value of parametrized product.
--
-- >>> map (Proxy @Num) (Identity . fromMaybe 0) (mk2F (Just 12) Nothing :: HVecF '[Double, Int] Maybe)
-- [Identity 12.0,Identity 0]
map :: (HVectorF v, ArityC c (ElemsF v))
    => Proxy c -> (forall a. c a => f a -> g a) -> v f -> v g
{-# INLINE map #-}
map cls f = C.vectorF . C.map cls f . C.cvecF

-- | Apply natural transformation to every element of the tuple.
--
-- >>> mapNat (Just . runIdentity) (mk2F (pure 'c') (pure 1) :: HVecF '[Char, Int] Identity)
-- [Just 'c',Just 1]
mapNat :: (HVectorF v)
       => (forall a. f a -> g a) -> v f -> v g
{-# INLINE mapNat #-}
mapNat f = C.vectorF . C.mapNat f . C.cvecF

-- | Sequence effects for every element in the vector
--
-- >>> sequence (mk2F [1,2] "ab" :: HVecF '[Int,Char] []) :: [(Int,Char)]
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
sequence
  :: ( Applicative f, HVectorF v, HVector w, ElemsF v ~ Elems w )
  => v f -> f w
{-# INLINE sequence #-}
sequence
  = fmap C.vector
  . C.sequenceF
  . C.mapNat (Compose . fmap Identity)
  . C.cvecF

-- | Sequence effects for every element in the vector
sequence_ :: (Applicative f, HVectorF v) => v f -> f ()
{-# INLINE sequence_ #-}
sequence_ = foldlNatF (\m a -> m <* a) (pure ())

-- | Sequence effects for every element in the vector
sequenceF :: ( Applicative f, HVectorF v) => v (f `Compose` g) -> f (v g)
{-# INLINE sequenceF #-}
sequenceF v = C.vectorF <$> C.sequenceF (C.cvecF v)

-- | Wrap every value in the vector into type constructor.
wrap :: ( HVector v, HVectorF w, Elems v ~ ElemsF w )
     => (forall a. a -> f a) -> v -> w f
{-# INLINE wrap #-}
wrap f = C.vectorF . C.mapNat (f . runIdentity) . C.cvec

-- | Unwrap every value in the vector from the type constructor.
unwrap :: ( HVectorF v, HVector w, ElemsF v ~ Elems w )
       => (forall a. f a -> a) -> v f -> w
{-# INLINE unwrap #-}
unwrap  f = C.vector . C.mapNat (Identity . f) . C.cvecF

-- | Analog of /distribute/ from /Distributive/ type class.
distribute
  :: ( Functor f, HVector v, HVectorF w,  Elems v ~ ElemsF w )
  => f v -> w f
{-# INLINE distribute #-}
distribute
  = C.vectorF
  . mapNat (fmap runIdentity . getCompose)
  . C.distributeF
  . fmap C.cvec

-- | Analog of /distribute/ from /Distributive/ type class.
distributeF
  :: ( Functor f, HVectorF v)
  => f (v g) -> v (f `Compose` g)
{-# INLINE distributeF #-}
distributeF = C.vectorF . C.distributeF . fmap C.cvecF



----------------------------------------------------------------
-- Type class based ops
----------------------------------------------------------------

-- | Replicate polymorphic value n times. Concrete instance for every
--   element is determined by their respective types.
--
-- >>> replicate (Proxy :: Proxy Monoid) mempty :: ((),String)
-- ((),"")
--
-- Or a bit contrived example which illustrate what how to call
-- function that require multiple type class constraints:
--
-- >>> replicate (Proxy @(Monoid :&&: Num)) (mempty * 10) :: (Product Int, Sum Int)
-- (Product {getProduct = 10},Sum {getSum = 0})
replicate :: (HVector v, ArityC c (Elems v))
          => Proxy c -> (forall x. c x => x) -> v
{-# INLINE replicate #-}
replicate c x = C.vector $ C.replicateF c (Identity x)

-- | Replicate monadic action n times. Example below is a bit awkward does convey what's
--
-- >>> :{
--   Prelude.mapM_ print
--     (replicateM (Proxy @(Monoid :&&: Num)) [mempty+1, mempty * 10] :: [(Product Int, Sum Int)])
-- :}
-- (Product {getProduct = 2},Sum {getSum = 1})
-- (Product {getProduct = 2},Sum {getSum = 0})
-- (Product {getProduct = 10},Sum {getSum = 1})
-- (Product {getProduct = 10},Sum {getSum = 0})
replicateM :: (HVector v, Applicative f, ArityC c (Elems v))
           => Proxy c -> (forall a. c a => f a) -> f v
{-# INLINE replicateM #-}
replicateM c x
  = fmap C.vector
  $ C.sequenceF
  $ C.replicateF c (Compose $ fmap Identity x)

-- | Replicate value @f a@ which is valid for every type a n times.
--
-- >>> replicateNatF Nothing :: HVecF '[Char,Int] Maybe
-- [Nothing,Nothing]
replicateNatF :: (HVectorF v)
           => (forall a. f a) -> v f
{-# INLINE replicateNatF #-}
replicateNatF x = C.vectorF $ C.replicateNatF x

-- | Replicate polymorphic value n times:
--
-- >>> replicateF (Proxy @Num) (Just 0) :: HVecF '[Double,Int] Maybe
-- [Just 0.0,Just 0]
replicateF :: (HVectorF v, ArityC c (ElemsF v))
            => Proxy c -> (forall a. c a => f a) -> v f
{-# INLINE replicateF #-}
replicateF c x = C.vectorF $ C.replicateF c x



----------------------------------------------------------------
-- Zipping of vectors
----------------------------------------------------------------

-- | Zip two heterogeneous vectors
--
-- >>> zipWith (Proxy @Num) (+) (0, 1.2) (1, 10) :: (Int,Double)
-- (1,11.2)
zipWith :: (HVector v, ArityC c (Elems v))
        => Proxy c -> (forall a. c a => a -> a -> a) -> v -> v -> v
{-# INLINE zipWith #-}
zipWith c f v u
  = C.vector
  $ C.zipWithF c (\(Identity a) (Identity b) -> Identity (f a b)) (C.cvec v) (C.cvec u)

-- | Zip two heterogeneous vectors
zipWithF :: (HVectorF v, ArityC c (ElemsF v))
         => Proxy c -> (forall a. c a => f a -> g a -> h a) -> v f -> v g -> v h
{-# INLINE zipWithF #-}
zipWithF c f v u
  = C.vectorF $ C.zipWithF c f (C.cvecF v) (C.cvecF u)

-- | Zip two heterogeneous vectors
zipWithNatF :: (HVectorF v)
        => (forall a. f a -> g a -> h a) -> v f -> v g -> v h
{-# INLINE zipWithNatF #-}
zipWithNatF f v u
  = C.vectorF $ C.zipWithNatF f (C.cvecF v) (C.cvecF u)

-- | Zip two heterogeneous vectors and immediately fold resulting
--   value.
--
-- >>> zipFold (Proxy @Show) (\a b -> show (a,b)) ((),'c',10) ((),'D',1)
-- "((),())('c','D')(10,1)"
zipFold :: (HVector v, ArityC c (Elems v), Monoid m)
        => Proxy c -> (forall a. c a => a -> a -> m) -> v -> v -> m
{-# INLINE zipFold #-}
zipFold c f v u
  = C.zipFoldF c (\(Identity a) (Identity b) -> f a b) (C.cvec v) (C.cvec u)

zipFoldF :: (HVectorF v, ArityC c (ElemsF v), Monoid m)
        => Proxy c -> (forall a. c a => f a -> f a -> m) -> v f -> v f -> m
{-# INLINE zipFoldF #-}
zipFoldF c f v u
  = C.zipFoldF c f (C.cvecF v) (C.cvecF u)

-- | Convert heterogeneous vector to homogeneous
monomorphize :: ( HVector v
                , ArityC c (Elems v))
             => Proxy c -> (forall a. c a => a -> x)
             -> v -> F.ContVec (Len (Elems v)) x
{-# INLINE monomorphize #-}
monomorphize c f = C.monomorphizeF c (f . runIdentity) . C.cvec

-- | Convert heterogeneous vector to homogeneous
monomorphizeF :: ( HVectorF v
                 , ArityC c (ElemsF v)
                 )
             => Proxy c -> (forall a. c a => f a -> x)
             -> v f -> F.ContVec (Len (ElemsF v)) x
{-# INLINE monomorphizeF #-}
monomorphizeF c f = C.monomorphizeF c f . C.cvecF


-- | Generic equality for heterogeneous vectors
--
-- >>> data A = A Int Char deriving Generic
-- >>> instance HVector A
-- >>> eq (A 1 'c') (A 2 'c')
-- False
eq :: (HVector v, ArityC Eq (Elems v)) => v -> v -> Bool
eq v u = getAll $ zipFold (Proxy :: Proxy Eq) (\x y -> All (x == y)) v u
{-# INLINE eq #-}

-- | Generic comparison for heterogeneous vectors. It works same way
--   as Ord instance for tuples.
--
-- >>> data A = A Int Char deriving Generic
-- >>> instance HVector A
-- >>> compare (A 1 'c') (A 2 'c')
-- LT
compare :: (HVector v, ArityC Ord (Elems v)) => v -> v -> Ordering
compare = zipFold (Proxy :: Proxy Ord) Prelude.compare
{-# INLINE compare #-}

-- | Reduce vector to normal form
rnf :: (HVector v, ArityC NF.NFData (Elems v)) => v -> ()
rnf = foldl (Proxy :: Proxy NF.NFData) (\r a -> NF.rnf a `seq` r) ()
{-# INLINE rnf #-}


----------------------------------------------------------------
-- Doctest
----------------------------------------------------------------

-- $setup
--
-- >>> :set -XDeriveGeneric
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XDataKinds
-- >>> import Prelude (Int,Double,String,Char,IO,(++),Maybe(..))
-- >>> import Prelude (Show(..),Read(..),read,Num(..),Monoid(..))
-- >>> import Prelude (print)
-- >>> import Control.Applicative     (Const(..))
-- >>> import Data.Complex            (Complex(..))
-- >>> import Data.Monoid             (Sum(..),Product(..))
-- >>> import Data.Maybe              (fromMaybe)
-- >>> import Data.Vector.HFixed.HVec (HVec,HVecF)
-- >>> import GHC.Generics            (Generic)
