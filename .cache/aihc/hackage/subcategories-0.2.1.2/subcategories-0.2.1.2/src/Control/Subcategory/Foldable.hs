{-# LANGUAGE BangPatterns, CPP, DefaultSignatures, DerivingVia, LambdaCase #-}
{-# LANGUAGE OverloadedStrings, QuantifiedConstraints, StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell, TypeOperators                                #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Control.Subcategory.Foldable
  ( CFoldable(..),
    ctoList,
    CTraversable(..),
    CFreeMonoid(..),
    cfromList,
    cfolded, cfolding,
    cctraverseFreeMonoid,
    cctraverseZipFreeMonoid
  ) where
import           Control.Applicative                  (ZipList, getZipList)
import           Control.Arrow                        (first, second, (***))
import qualified Control.Foldl                        as L
import           Control.Monad                        (forM)
import           Control.Subcategory.Applicative
import           Control.Subcategory.Functor
import           Control.Subcategory.Pointed
import           Control.Subcategory.Wrapper.Internal
import           Control.Subcategory.Zip
import           Data.Coerce
import           Data.Complex                         (Complex)
import           Data.Foldable
import           Data.Functor.Const                   (Const)
import           Data.Functor.Contravariant           (Contravariant, contramap,
                                                       phantom)
import           Data.Functor.Identity                (Identity)
import qualified Data.Functor.Product                 as SOP
import qualified Data.Functor.Sum                     as SOP
import qualified Data.HashMap.Strict                  as HM
import qualified Data.HashSet                         as HS
import qualified Data.IntMap.Strict                   as IM
import qualified Data.IntSet                          as IS
import           Data.Kind                            (Type)
import           Data.List                            (uncons)
import           Data.List                            (intersperse)
import           Data.List                            (nub)
import qualified Data.List                            as List
import           Data.List.NonEmpty                   (NonEmpty)
import qualified Data.List.NonEmpty                   as NE
import qualified Data.Map                             as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Monoid                          as Mon
import           Data.MonoTraversable                 hiding (WrappedMono,
                                                       unwrapMono)
import           Data.Ord                             (Down)
import qualified Data.Primitive.Array                 as A
import qualified Data.Primitive.PrimArray             as PA
import qualified Data.Primitive.SmallArray            as SA
import           Data.Proxy                           (Proxy)
import           Data.Semigroup                       (Arg, Max (..), Min (..))
import qualified Data.Semigroup                       as Sem
import qualified Data.Sequence                        as Seq
import           Data.Sequences                       (IsSequence (indexEx))
import qualified Data.Sequences                       as MT
import qualified Data.Set                             as Set
import qualified Data.Text                            as T
import qualified Data.Vector                          as V
import qualified Data.Vector.Algorithms.Intro         as AI
import qualified Data.Vector.Primitive                as P
import qualified Data.Vector.Storable                 as S
import qualified Data.Vector.Unboxed                  as U
import           Foreign.Ptr                          (Ptr)
import qualified GHC.Exts                             as GHC
import           GHC.Generics
import           Language.Haskell.TH                  hiding (Type)
import           Language.Haskell.TH.Syntax           hiding (Type)
import qualified VectorBuilder.Builder                as VB
import qualified VectorBuilder.Vector                 as VB

-- See Note [Function coercion]
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
{-# INLINE (#.) #-}

ctoList :: (CFoldable f, Dom f a) => f a -> [a]
{-# INLINE [1] ctoList #-}
ctoList = cbasicToList

cfromList :: (CFreeMonoid f, Dom f a) => [a] -> f a
{-# INLINE [1] cfromList #-}
cfromList = cbasicFromList


-- | Fold-optic for 'CFoldable' instances.
--   In the terminology of lens, cfolded is a constrained
--   variant of @folded@ optic.
--
--  @
--    cfolded :: (CFoldable t, Dom t a) => Fold (t a) a
--  @
cfolded
  :: (CFoldable t, Dom t a)
  => forall f. (Contravariant f, Applicative f) => (a -> f a) -> t a -> f (t a)
{-# INLINE cfolded #-}
cfolded = (contramap (const ()) .) . ctraverse_

class Constrained f => CFoldable f where
  {-# MINIMAL cfoldMap | cfoldr #-}
  cfoldMap :: (Dom f a, Monoid w) => (a -> w) -> f a -> w
  {-# INLINE [1] cfoldMap #-}
  cfoldMap f = cfoldr (mappend . f) mempty

  cfoldMap' :: (Dom f a, Monoid m) => (a -> m) -> f a -> m
  {-# INLINE [1] cfoldMap' #-}
  cfoldMap' f = cfoldl' (\ acc a -> acc <> f a) mempty

  cfold :: (Dom f w, Monoid w) => f w -> w
  cfold = cfoldMap id

  {-# INLINE [1] cfold #-}
  cfoldr :: (Dom f a) => (a -> b -> b) -> b -> f a -> b
  {-# INLINE [1] cfoldr #-}
  cfoldr f z t = appEndo (cfoldMap (Endo #. f) t) z

  cfoldlM
    :: (Monad m, Dom f b)
    => (a -> b -> m a) -> a -> f b -> m a
  {-# INLINE [1] cfoldlM #-}
  cfoldlM f z0 xs = cfoldr f' return xs z0
    where f' x k z = f z x >>= k

  cfoldlM'
    :: (Monad m, Dom f b)
    => (a -> b -> m a) -> a -> f b -> m a
  {-# INLINE [1] cfoldlM' #-}
  cfoldlM' f z0 xs = cfoldr' f' return xs z0
    where f' !x k z = do
            !i <- f z x
            k i

  cfoldrM
    :: (Monad m, Dom f a)
    => (a -> b -> m b) -> b -> f a -> m b
  {-# INLINE [1] cfoldrM #-}
  cfoldrM f z0 xs = cfoldl c return xs z0
    where c k x z = f x z >>= k

  cfoldrM'
    :: (Monad m, Dom f a)
    => (a -> b -> m b) -> b -> f a -> m b
  {-# INLINE [1] cfoldrM' #-}
  cfoldrM' f z0 xs = cfoldl' c return xs z0
    where c k !x z = do
            !i <- f x z
            k i
  cfoldl
      :: (Dom f a)
      => (b -> a -> b) -> b -> f a -> b
  {-# INLINE [1] cfoldl #-}
  cfoldl f z t = appEndo (getDual (cfoldMap (Dual . Endo . flip f) t)) z

  cfoldr' :: (Dom f a) => (a -> b -> b) -> b -> f a -> b
  {-# INLINE [1] cfoldr' #-}
  cfoldr' f z0 xs = cfoldl f' id xs z0
      where f' k x z = k $! f x z

  cfoldl' :: Dom f a => (b -> a -> b) -> b -> f a -> b
  {-# INLINE [1] cfoldl' #-}
  cfoldl' f z0 xs = cfoldr f' id xs z0
    where f' x k z = k $! f z x

  cbasicToList :: Dom f a => f a -> [a]
  {-# INLINE cbasicToList #-}
  cbasicToList = cfoldr (:) []

  cfoldr1 :: Dom f a => (a -> a -> a) -> f a -> a
  {-# INLINE [1] cfoldr1 #-}
  cfoldr1 f xs = fromMaybe (errorWithoutStackTrace "cfoldr1: empty structure")
                    (cfoldr mf Nothing xs)
      where
        mf x m = Just $
          case m of
            Nothing -> x
            Just y  -> f x y



  cfoldl1 :: Dom f a => (a -> a -> a) -> f a -> a
  {-# INLINE [1] cfoldl1 #-}
  cfoldl1 f xs = fromMaybe (errorWithoutStackTrace "cfoldl1: empty structure")
                  (cfoldl mf Nothing xs)
    where
      mf m y = Just $
        case m of
          Nothing -> y
          Just x  -> f x y

  cindex :: Dom f a => f a -> Int -> a
  cindex xs n = case cfoldl' go (Left' 0) xs of
    Right' x -> x
    Left'{} -> errorWithoutStackTrace $ "cindex: index out of bound " ++ show n
    where
      go (Left' i) x
        | i == n = Right' x
        | otherwise = Left' (i + 1)
      go r@Right'{} _ = r

  cnull :: Dom f a => f a -> Bool
  cnull = cfoldr (const $ const False) True

  clength :: Dom f a => f a -> Int
  {-# INLINE [1] clength #-}
  clength = cfoldl' (\c _ -> c + 1) 0

  cany :: Dom f a => (a -> Bool) -> f a -> Bool
  {-# INLINE [1] cany #-}
  cany p = cfoldl' (\b -> (||) b . p) False

  call :: Dom f a => (a -> Bool) -> f a -> Bool
  {-# INLINE [1] call #-}
  call p = cfoldl' (\b -> (&&) b . p) True

  celem :: (Eq a, Dom f a) => a -> f a -> Bool
  {-# INLINE [1] celem #-}
  celem = cany . (==)

  cnotElem :: (Eq a, Dom f a) => a -> f a -> Bool
  {-# INLINE [1] cnotElem #-}
  cnotElem = call . (/=)

  cminimum :: (Ord a, Dom f a) => f a -> a
  {-# INLINE [1] cminimum #-}
  cminimum =
    getMin
    . fromMaybe (errorWithoutStackTrace "minimum: empty structure")
    . cfoldMap (Just . Min)

  cmaximum :: (Ord a, Dom f a) => f a -> a
  {-# INLINE [1] cmaximum #-}
  cmaximum =
    getMax
    . fromMaybe (errorWithoutStackTrace "cmaximum: empty structure")
    . cfoldMap (Just . Max)

  csum :: (Num a, Dom f a) => f a -> a
  {-# INLINE [1] csum #-}
  csum = getSum #. cfoldMap Sum

  cproduct :: (Num a, Dom f a) => f a -> a
  {-# INLINE [1] cproduct #-}
  cproduct = getProduct #. cfoldMap Product

  cctraverse_
    :: (CApplicative g, CPointed g, Dom g (), Dom f a, Dom g b)
    => (a -> g b)
    -> f a -> g ()
  {-# INLINE [1] cctraverse_ #-}
  cctraverse_ f = cfoldr c (cpure ())
    where
      {-# INLINE c #-}
      c x k = f x .> k

  ctraverse_
    :: (Applicative g, Dom f a)
    => (a -> g b)
    -> f a -> g ()
  {-# INLINE [1] ctraverse_ #-}
  ctraverse_ f = cfoldr c (pure ())
    where
      {-# INLINE c #-}
      c x k = f x *> k

  clast :: Dom f a => f a -> a
  {-# INLINE [1] clast #-}
  clast = fromJust . L.foldOver cfolded L.last

  chead :: Dom f a => f a -> a
  {-# INLINE [1] chead #-}
  chead = fromJust . L.foldOver cfolded L.head

  cfind :: Dom f a => (a -> Bool) -> f a -> Maybe a
  {-# INLINE [1] cfind #-}
  cfind = \p -> getFirst . cfoldMap (\x -> First $ if p x then Just x else Nothing)

  cfindIndex :: Dom f a => (a -> Bool) -> f a -> Maybe Int
  {-# INLINE [1] cfindIndex #-}
  cfindIndex = L.foldOver cfolded . L.findIndex

  cfindIndices :: Dom f a => (a -> Bool) -> f a -> [Int]
  {-# INLINE [1] cfindIndices #-}
  cfindIndices = \p -> List.findIndices p . ctoList

  celemIndex :: (Dom f a, Eq a) => a -> f a -> Maybe Int
  {-# INLINE [0] celemIndex #-}
  celemIndex = cfindIndex . (==)

  celemIndices :: (Dom f a, Eq a) => a -> f a -> [Int]
  {-# INLINE [0] celemIndices #-}
  celemIndices = cfindIndices . (==)

data Eith' a b = Left' !a | Right' !b

instance Traversable f => CTraversable (WrapFunctor f) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}

instance Foldable f => CFoldable (WrapFunctor f) where
  cfoldMap = foldMap
  {-# INLINE [1] cfoldMap #-}
  cfoldMap' = foldMap'
  {-# INLINE [1] cfoldMap' #-}
  cfold = fold
  {-# INLINE [1] cfold #-}
  cfoldr = foldr
  {-# INLINE [1] cfoldr #-}
  cfoldr' = foldr'
  {-# INLINE [1] cfoldr' #-}
  cfoldl = foldl
  {-# INLINE [1] cfoldl #-}
  cfoldl' = foldl'
  {-# INLINE [1] cfoldl' #-}
  cbasicToList = toList
  {-# INLINE [1] cbasicToList #-}
  cfoldr1 = foldr1
  {-# INLINE [1] cfoldr1 #-}
  cfoldl1 = foldl1
  {-# INLINE [1] cfoldl1 #-}
  cfoldlM = foldlM
  {-# INLINE [1] cfoldlM #-}
  cfoldrM = foldrM
  {-# INLINE [1] cfoldrM #-}
  cnull = null
  {-# INLINE [1] cnull #-}
  clength = length
  {-# INLINE [1] clength #-}
  cany = any
  {-# INLINE [1] cany #-}
  call = all
  {-# INLINE [1] call #-}
  celem = elem
  {-# INLINE [1] celem #-}
  cnotElem = notElem
  {-# INLINE [1] cnotElem #-}
  cminimum = minimum
  {-# INLINE [1] cminimum #-}
  cmaximum = maximum
  {-# INLINE [1] cmaximum #-}
  csum = sum
  {-# INLINE [1] csum #-}
  cproduct = product
  {-# INLINE [1] cproduct #-}
  ctraverse_ = traverse_
  {-# INLINE [1] ctraverse_ #-}
  cfind = find
  {-# INLINE [1] cfind #-}
  cfindIndex = L.fold . L.findIndex
  {-# INLINE [1] cfindIndex #-}
  celemIndex = L.fold . L.elemIndex
  {-# INLINE [1] celemIndex #-}

{-# RULES
"cfind/List"
  cfind = find @[]

"cfindIndex/List"
  cfindIndex = List.findIndex

"cfindIndices/List"
  cfindIndices = List.findIndices

"celemIndex/List"
  celemIndex = List.elemIndex

"celemIndices/List"
  celemIndices = List.elemIndices

"cfindIndex/List"
  cfindIndex = Seq.findIndexL

"cfindIndices/Seq"
  cfindIndices = Seq.findIndicesL

"celemIndex/Seq"
  celemIndex = Seq.elemIndexL

"celemIndices/Seq"
  celemIndices = Seq.elemIndicesL

  #-}

{-# RULES
"cctraverse_/traverse_"
  forall (f :: Applicative f => a -> f b) (tx :: Foldable t => t a).
  cctraverse_ f tx = traverse_ f tx
  #-}

{-# RULES
"cindex/List"
  cindex = (!!)
  #-}

class (CFunctor f, CFoldable f) => CTraversable f where
  -- | __N.B.__ If we require @g@ to be 'CApplicative'
  --   we cannot directly lift plain 'Traversable' to 'CTraversable'.
  --   This is rather annoying, so we require the strongest possible
  --   constraint to @g@ here.
  ctraverse
    :: (Dom f a, Dom f b, Applicative g)
    => (a -> g b) -> f a -> g (f b)

deriving via WrapFunctor []
  instance CFoldable []
{-# RULES
"ctoList/List"
  ctoList = id
"cfromList/List"
  cbasicFromList = id
"clast/List"
  clast = last
"chead/List"
  chead = head
  #-}

instance CTraversable [] where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor Maybe
  instance CFoldable Maybe
instance CTraversable Maybe where
  ctraverse = traverse
deriving via WrapFunctor (Either e)
  instance CFoldable (Either e)
instance CTraversable (Either e) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor IM.IntMap
  instance CFoldable IM.IntMap
instance CTraversable IM.IntMap where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor (M.Map k)
  instance CFoldable (M.Map k)
instance Ord k => CTraversable (M.Map k) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor (HM.HashMap k)
  instance CFoldable (HM.HashMap k)
instance CTraversable (HM.HashMap k) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor Seq.Seq
  instance CFoldable Seq.Seq
instance CTraversable Seq.Seq where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
{-# RULES
"cindex/Seq"
  cindex = Seq.index
  #-}

deriving via WrapFunctor Par1
  instance CFoldable Par1
instance CTraversable Par1 where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor NonEmpty
  instance CFoldable NonEmpty
instance CTraversable NonEmpty where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
{-# RULES
"cindex/NonEmpty"
  cindex = (NE.!!)
  #-}

deriving via WrapFunctor Down
  instance CFoldable Down
instance CTraversable Down where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor Mon.Last
  instance CFoldable Mon.Last
instance CTraversable Mon.Last where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor Mon.First
  instance CFoldable Mon.First
instance CTraversable Mon.First where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor Sem.Last
  instance CFoldable Sem.Last
instance CTraversable Sem.Last where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor Sem.First
  instance CFoldable Sem.First
instance CTraversable Sem.First where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor Identity
  instance CFoldable Identity
instance CTraversable Identity where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor ZipList
  instance CFoldable ZipList
instance CTraversable ZipList where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
{-# RULES
"cindex/ZipList"
  cindex = (!!) . getZipList
  #-}

#if !MIN_VERSION_base(4,16,0)
deriving via WrapFunctor Sem.Option
  instance CFoldable Sem.Option
instance CTraversable Sem.Option where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
#endif

deriving via WrapFunctor Min
  instance CFoldable Min
instance CTraversable Min where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor Max
  instance CFoldable Max
instance CTraversable Max where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor Complex
  instance CFoldable Complex
instance CTraversable Complex where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor (V1 :: Type -> Type)
  instance CFoldable (V1 :: Type -> Type)
instance CTraversable (V1 :: Type -> Type) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor (U1 :: Type -> Type)
  instance CFoldable (U1 :: Type -> Type)
instance CTraversable (U1 :: Type -> Type) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor ((,) a)
  instance CFoldable ((,) a)
instance CTraversable ((,) a) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor (Proxy :: Type -> Type)
  instance CFoldable (Proxy :: Type -> Type)
instance CTraversable (Proxy :: Type -> Type) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor (Arg a)
  instance CFoldable (Arg a)
instance CTraversable (Arg a) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor (Rec1 (f :: Type -> Type))
  instance Foldable f => CFoldable (Rec1 (f :: Type -> Type))
deriving via WrapFunctor (URec Char :: Type -> Type)
  instance CFoldable (URec Char :: Type -> Type)
instance CTraversable (URec Char :: Type -> Type) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor (URec Double :: Type -> Type)
  instance CFoldable (URec Double :: Type -> Type)
instance CTraversable (URec Double :: Type -> Type) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor (URec Float :: Type -> Type)
  instance CFoldable (URec Float :: Type -> Type)
instance CTraversable (URec Float :: Type -> Type) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor (URec Int :: Type -> Type)
  instance CFoldable (URec Int :: Type -> Type)
instance CTraversable (URec Int :: Type -> Type) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor (URec Word :: Type -> Type)
  instance CFoldable (URec Word :: Type -> Type)
instance CTraversable (URec Word :: Type -> Type) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor (URec (Ptr ()) :: Type -> Type)
  instance CFoldable (URec (Ptr ()) :: Type -> Type)
instance CTraversable (URec (Ptr ()) :: Type -> Type) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving newtype
  instance CFoldable f => CFoldable (Alt f)
deriving newtype
  instance CFoldable f => CFoldable (Ap f)
deriving via WrapFunctor (Const m :: Type -> Type)
  instance CFoldable (Const m :: Type -> Type)
instance CTraversable (Const m :: Type -> Type) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}
deriving via WrapFunctor (K1 i c :: Type -> Type)
  instance CFoldable (K1 i c :: Type -> Type)
instance CTraversable (K1 i c :: Type -> Type) where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}

instance (CFoldable f, CFoldable g) => CFoldable (f :+: g) where
  {-# INLINE [1] cfoldMap #-}
  cfoldMap f = \case
    L1 x -> cfoldMap f x
    R1 x -> cfoldMap f x

  {-# INLINE [1] cfoldr #-}
  cfoldr f z = \case
    L1 x -> cfoldr f z x
    R1 x -> cfoldr f z x

  cfoldMap' = \f -> \case
    L1 x -> cfoldMap' f x
    R1 x -> cfoldMap' f x
  {-# INLINE [1] cfoldMap' #-}
  cfold = \case
    L1 x -> cfold x
    R1 x -> cfold x
  {-# INLINE [1] cfold #-}
  cfoldr' = \f z -> \case
    L1 x -> cfoldr' f z x
    R1 x -> cfoldr' f z x
  {-# INLINE [1] cfoldr' #-}
  cfoldl = \f z -> \case
    L1 x -> cfoldl f z x
    R1 x -> cfoldl f z x
  {-# INLINE [1] cfoldl #-}
  cfoldl' = \f z -> \case
    L1 x -> cfoldl' f z x
    R1 x -> cfoldl' f z x
  {-# INLINE [1] cfoldl' #-}
  cbasicToList = \case
    L1 x -> ctoList x
    R1 x -> ctoList x
  {-# INLINE cbasicToList #-}
  cfoldr1 = \f -> \case
    L1 x -> cfoldr1 f x
    R1 x -> cfoldr1 f x
  {-# INLINE [1] cfoldr1 #-}
  cfoldl1 = \f -> \case
    L1 x -> cfoldl1 f x
    R1 x -> cfoldl1 f x
  {-# INLINE [1] cfoldl1 #-}
  cnull = \case
    L1 x -> cnull x
    R1 x -> cnull x
  {-# INLINE [1] cnull #-}
  clength = \case
    L1 x -> clength x
    R1 x -> clength x
  {-# INLINE [1] clength #-}
  cany = \f -> \case
    L1 x -> cany f x
    R1 x -> cany f x
  {-# INLINE [1] cany #-}
  call = \f -> \case
    L1 x -> call f x
    R1 x -> call f x
  {-# INLINE [1] call #-}
  celem = \x -> \case
    L1 xs -> celem x xs
    R1 xs -> celem x xs
  {-# INLINE [1] celem #-}
  cminimum = \case
    L1 xs -> cminimum xs
    R1 xs -> cminimum xs
  {-# INLINE [1] cminimum #-}
  cmaximum = \case
    L1 xs -> cmaximum xs
    R1 xs -> cmaximum xs
  {-# INLINE [1] cmaximum #-}
  csum = \case
    L1 xs -> csum xs
    R1 xs -> csum xs
  {-# INLINE [1] csum #-}
  cproduct = \case
    L1 xs -> cproduct xs
    R1 xs -> cproduct xs
  {-# INLINE [1] cproduct #-}
  ctraverse_ f = \case
    L1 xs -> ctraverse_ f xs
    R1 xs -> ctraverse_ f xs
  {-# INLINE [1] ctraverse_ #-}

instance (CTraversable f, CTraversable g) => CTraversable (f :+: g) where
  ctraverse f = \case
    L1 xs -> L1 <$> ctraverse f xs
    R1 xs -> R1 <$> ctraverse f xs
  {-# INLINE [1] ctraverse #-}

instance (CFoldable f, CFoldable g) => CFoldable (f :*: g) where
  {-# INLINE [1] cfoldMap #-}
  cfoldMap f (l :*: r) = cfoldMap f l <> cfoldMap f r

  cfoldMap' f (l :*: r) = cfoldMap' f l <> cfoldMap' f r
  {-# INLINE [1] cfoldMap' #-}
  cfold (l :*: r) = cfold l <> cfold r
  {-# INLINE [1] cfold #-}
  cnull (l :*: r) = cnull l && cnull r
  {-# INLINE [1] cnull #-}
  clength (l :*: r) = clength l + clength r
  {-# INLINE [1] clength #-}
  cany f (l :*: r) = cany f l || cany f r
  {-# INLINE [1] cany #-}
  call f (l :*: r) = call f l && call f r
  {-# INLINE [1] call #-}
  celem x (l :*: r) = celem x l || celem x r
  {-# INLINE [1] celem #-}
  csum (l :*: r) = csum l + csum r
  {-# INLINE [1] csum #-}
  cproduct (l :*: r) = cproduct l * cproduct r
  {-# INLINE [1] cproduct #-}
  ctraverse_ f (l :*: r) = ctraverse_ f l *> ctraverse_ f r
  {-# INLINE [1] ctraverse_ #-}

instance (CTraversable f, CTraversable g) => CTraversable (f :*: g) where
  ctraverse f (l :*: r) =
    (:*:) <$> ctraverse f l <*> ctraverse f r

instance (CFoldable f, CFoldable g) => CFoldable (SOP.Sum f g) where
  {-# INLINE [1] cfoldMap #-}
  cfoldMap f = \case
    SOP.InL x -> cfoldMap f x
    SOP.InR x -> cfoldMap f x

  {-# INLINE [1] cfoldr #-}
  cfoldr f z = \case
    SOP.InL x -> cfoldr f z x
    SOP.InR x -> cfoldr f z x

  cfoldMap' = \f -> \case
    SOP.InL x -> cfoldMap' f x
    SOP.InR x -> cfoldMap' f x
  {-# INLINE [1] cfoldMap' #-}
  cfold = \case
    SOP.InL x -> cfold x
    SOP.InR x -> cfold x
  {-# INLINE [1] cfold #-}
  cfoldr' = \f z -> \case
    SOP.InL x -> cfoldr' f z x
    SOP.InR x -> cfoldr' f z x
  {-# INLINE [1] cfoldr' #-}
  cfoldl = \f z -> \case
    SOP.InL x -> cfoldl f z x
    SOP.InR x -> cfoldl f z x
  {-# INLINE [1] cfoldl #-}
  cfoldl' = \f z -> \case
    SOP.InL x -> cfoldl' f z x
    SOP.InR x -> cfoldl' f z x
  {-# INLINE [1] cfoldl' #-}
  cbasicToList = \case
    SOP.InL x -> ctoList x
    SOP.InR x -> ctoList x
  {-# INLINE cbasicToList #-}
  cfoldr1 = \f -> \case
    SOP.InL x -> cfoldr1 f x
    SOP.InR x -> cfoldr1 f x
  {-# INLINE [1] cfoldr1 #-}
  cfoldl1 = \f -> \case
    SOP.InL x -> cfoldl1 f x
    SOP.InR x -> cfoldl1 f x
  {-# INLINE [1] cfoldl1 #-}
  cnull = \case
    SOP.InL x -> cnull x
    SOP.InR x -> cnull x
  {-# INLINE [1] cnull #-}
  clength = \case
    SOP.InL x -> clength x
    SOP.InR x -> clength x
  {-# INLINE [1] clength #-}
  cany = \f -> \case
    SOP.InL x -> cany f x
    SOP.InR x -> cany f x
  {-# INLINE [1] cany #-}
  call = \f -> \case
    SOP.InL x -> call f x
    SOP.InR x -> call f x
  {-# INLINE [1] call #-}
  celem = \x -> \case
    SOP.InL xs -> celem x xs
    SOP.InR xs -> celem x xs
  {-# INLINE [1] celem #-}
  cminimum = \case
    SOP.InL xs -> cminimum xs
    SOP.InR xs -> cminimum xs
  {-# INLINE [1] cminimum #-}
  cmaximum = \case
    SOP.InL xs -> cmaximum xs
    SOP.InR xs -> cmaximum xs
  {-# INLINE [1] cmaximum #-}
  csum = \case
    SOP.InL xs -> csum xs
    SOP.InR xs -> csum xs
  {-# INLINE [1] csum #-}
  cproduct = \case
    SOP.InL xs -> cproduct xs
    SOP.InR xs -> cproduct xs
  {-# INLINE [1] cproduct #-}
  ctraverse_ f = \case
    SOP.InL xs -> ctraverse_ f xs
    SOP.InR xs -> ctraverse_ f xs
  {-# INLINE [1] ctraverse_ #-}

instance (CTraversable f, CTraversable g) => CTraversable (SOP.Sum f g) where
  ctraverse f = \case
    SOP.InL xs -> SOP.InL <$> ctraverse f xs
    SOP.InR xs -> SOP.InR <$> ctraverse f xs
  {-# INLINE [1] ctraverse #-}

instance (CFoldable f, CFoldable g) => CFoldable (SOP.Product f g) where
  {-# INLINE [1] cfoldMap #-}
  cfoldMap f (SOP.Pair l r) = cfoldMap f l <> cfoldMap f r

  cfoldMap' f (SOP.Pair l r) = cfoldMap' f l <> cfoldMap' f r
  {-# INLINE [1] cfoldMap' #-}
  cfold (SOP.Pair l r) = cfold l <> cfold r
  {-# INLINE [1] cfold #-}
  cnull (SOP.Pair l r) = cnull l && cnull r
  {-# INLINE [1] cnull #-}
  clength (SOP.Pair l r) = clength l + clength r
  {-# INLINE [1] clength #-}
  cany f (SOP.Pair l r) = cany f l || cany f r
  {-# INLINE [1] cany #-}
  call f (SOP.Pair l r) = call f l && call f r
  {-# INLINE [1] call #-}
  celem x (SOP.Pair l r) = celem x l || celem x r
  {-# INLINE [1] celem #-}
  csum (SOP.Pair l r) = csum l + csum r
  {-# INLINE [1] csum #-}
  cproduct (SOP.Pair l r) = cproduct l * cproduct r
  {-# INLINE [1] cproduct #-}
  ctraverse_ f (SOP.Pair l r) =
    ctraverse_ f l *> ctraverse_ f r
  {-# INLINE ctraverse_ #-}

deriving via WrapFunctor SA.SmallArray instance CFoldable SA.SmallArray
deriving via WrapFunctor A.Array instance CFoldable A.Array

instance CFoldable PA.PrimArray where
  cfoldr = PA.foldrPrimArray
  {-# INLINE [1] cfoldr #-}
  cfoldl' = PA.foldlPrimArray'
  {-# INLINE [1] cfoldl' #-}
  cfoldlM' = PA.foldlPrimArrayM'
  {-# INLINE [1] cfoldlM' #-}
  cfoldl = PA.foldlPrimArray
  {-# INLINE [1] cfoldl #-}
  clength = PA.sizeofPrimArray
  {-# INLINE [1] clength #-}
  csum = PA.foldlPrimArray' (+) 0
  {-# INLINE [1] csum #-}
  cproduct = PA.foldlPrimArray' (*) 1
  {-# INLINE [1] cproduct #-}
  ctraverse_ = PA.traversePrimArray_
  {-# INLINE [1] ctraverse_ #-}

instance CTraversable PA.PrimArray where
  ctraverse = PA.traversePrimArray
  {-# INLINE [1] ctraverse #-}

instance CTraversable SA.SmallArray where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}

instance CTraversable A.Array where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}

instance (CTraversable f, CTraversable g) => CTraversable (SOP.Product f g) where
  {-# INLINE [1] ctraverse #-}
  ctraverse f (SOP.Pair l r) =
    SOP.Pair <$> ctraverse f l <*> ctraverse f r

instance CFoldable Set.Set where
  cfoldMap = ofoldMap
  {-# INLINE [1] cfoldMap #-}
  cfoldr = Set.foldr
  {-# INLINE [1] cfoldr #-}
  cfoldl = Set.foldl
  {-# INLINE [1] cfoldl #-}
  cfoldr' = Set.foldr'
  {-# INLINE [1] cfoldr' #-}
  cfoldl' = Set.foldl'
  {-# INLINE [1] cfoldl' #-}
  cminimum = Set.findMin
  {-# INLINE [1] cminimum #-}
  cmaximum = Set.findMax
  {-# INLINE [1] cmaximum #-}
  celem = Set.member
  {-# INLINE [1] celem #-}
  cnotElem = Set.notMember
  {-# INLINE [1] cnotElem #-}
  cbasicToList = Set.toList
  {-# INLINE cbasicToList #-}
  celemIndex = Set.lookupIndex
  {-# INLINE [1] celemIndex #-}
  cindex = flip Set.elemAt
  {-# INLINE [1] cindex #-}

instance CTraversable Set.Set where
  -- TODO: more efficient implementation
  ctraverse f =
      fmap Set.fromList
    . traverse f
    . Set.toList
  {-# INLINE [1] ctraverse #-}

instance CFoldable HS.HashSet where
  cfoldMap = ofoldMap
  {-# INLINE [1] cfoldMap #-}
  cfoldr = HS.foldr
  {-# INLINE [1] cfoldr #-}
  cfoldl' = HS.foldl'
  {-# INLINE [1] cfoldl' #-}
  celem = HS.member
  {-# INLINE [1] celem #-}
  cbasicToList = HS.toList
  {-# INLINE cbasicToList #-}

instance CTraversable HS.HashSet where
  -- TODO: more efficient implementation
  ctraverse f =
      fmap HS.fromList
    . traverse f
    . HS.toList
  {-# INLINE [1] ctraverse #-}

{-# RULES
"celem/IntSet"
  celem = coerce
    @(Int -> IS.IntSet -> Bool)
    @(Int -> WrapMono IS.IntSet Int -> Bool)
    IS.member
"cnotElem/IntSet"
  cnotElem = coerce
    @(Int -> IS.IntSet -> Bool)
    @(Int -> WrapMono IS.IntSet Int -> Bool)
    IS.notMember
"cmaximum/IntSet"
  cmaximum = coerce @_ @(WrapMono IS.IntSet Int -> Int)
    IS.findMax
"cminimum/IntSet"
  cminimum = coerce @(IS.IntSet -> Int) @(WrapMono IS.IntSet Int -> Int)
    IS.findMin
  #-}

instance MonoFoldable mono => CFoldable (WrapMono mono) where
  cfoldMap = ofoldMap
  {-# INLINE [1] cfoldMap #-}
  cfold = ofold
  {-# INLINE [1] cfold #-}
  cfoldr = ofoldr
  {-# INLINE [1] cfoldr #-}
  cfoldl' = ofoldl'
  {-# INLINE [1] cfoldl' #-}
  cfoldlM = ofoldlM
  {-# INLINE [1] cfoldlM #-}
  cbasicToList = otoList
  {-# INLINE cbasicToList #-}
  cfoldr1 = ofoldr1Ex
  {-# INLINE [1] cfoldr1 #-}
  cnull = onull
  {-# INLINE [1] cnull #-}
  clength = olength
  {-# INLINE [1] clength #-}
  cany = oany
  {-# INLINE [1] cany #-}
  call = oall
  {-# INLINE [1] call #-}
  celem = oelem
  {-# INLINE [1] celem #-}
  cnotElem = onotElem
  {-# INLINE [1] cnotElem #-}
  cminimum = minimumEx
  {-# INLINE [1] cminimum #-}
  cmaximum = maximumEx
  {-# INLINE [1] cmaximum #-}
  csum = osum
  {-# INLINE [1] csum #-}
  cproduct = oproduct
  {-# INLINE [1] cproduct #-}
  ctraverse_ = otraverse_
  {-# INLINE [1] ctraverse_ #-}

instance MonoTraversable mono => CTraversable (WrapMono mono) where
  ctraverse = \f -> fmap WrapMono . otraverse f . unwrapMono

instance CFoldable V.Vector where
  {-# INLINE [1] cfoldMap #-}
  cfoldMap = foldMap
  {-# INLINE [1] cfoldr #-}
  cfoldr = V.foldr
  {-# INLINE [1] cfoldr' #-}
  cfoldr' = V.foldr'
  {-# INLINE [1] cfoldl #-}
  cfoldl = V.foldl
  {-# INLINE [1] cfoldl' #-}
  cfoldl' = V.foldl'
  {-# INLINE cfoldlM #-}
  cfoldlM = V.foldM
  {-# INLINE cfoldlM' #-}
  cfoldlM' = V.foldM'
  {-# INLINE [1] cindex #-}
  cindex = (V.!)
  {-# INLINE [1] celem #-}
  celem = V.elem
  {-# INLINE [1] cnotElem #-}
  cnotElem = V.notElem
  {-# INLINE [1] cany #-}
  cany = V.any
  {-# INLINE [1] call #-}
  call = V.all
  {-# INLINE [1] cfoldl1 #-}
  cfoldl1 = V.foldl1
  {-# INLINE [1] cfoldr1 #-}
  cfoldr1 = V.foldr1
  {-# INLINE [1] csum #-}
  csum = V.sum
  {-# INLINE [1] cproduct #-}
  cproduct = V.product
  {-# INLINE [1] cmaximum #-}
  cmaximum = V.maximum
  {-# INLINE [1] cminimum #-}
  cminimum = V.minimum
  {-# INLINE cbasicToList #-}
  cbasicToList = V.toList
  {-# INLINE [1] clast #-}
  clast = V.last
  {-# INLINE [1] chead #-}
  chead = V.head
  {-# INLINE [1] cfind #-}
  cfind = V.find
  {-# INLINE [1] cfindIndex #-}
  cfindIndex = V.findIndex
  {-# INLINE [1] cfindIndices #-}
  cfindIndices = fmap V.toList . V.findIndices
  {-# INLINE [1] celemIndex #-}
  celemIndex = V.elemIndex
  {-# INLINE [1] celemIndices #-}
  celemIndices = fmap V.toList . V.elemIndices

instance CFoldable U.Vector where
  {-# INLINE [1] cfoldMap #-}
  cfoldMap = ofoldMap
  {-# INLINE [1] cfoldr #-}
  cfoldr = U.foldr
  {-# INLINE [1] cfoldr' #-}
  cfoldr' = U.foldr'
  {-# INLINE [1] cfoldl #-}
  cfoldl = U.foldl
  {-# INLINE [1] cfoldl' #-}
  cfoldl' = U.foldl'
  {-# INLINE cfoldlM #-}
  cfoldlM = U.foldM
  {-# INLINE cfoldlM' #-}
  cfoldlM' = U.foldM'
  {-# INLINE [1] cindex #-}
  cindex = (U.!)
  {-# INLINE [1] celem #-}
  celem = U.elem
  {-# INLINE [1] cnotElem #-}
  cnotElem = U.notElem
  {-# INLINE [1] cany #-}
  cany = U.any
  {-# INLINE [1] call #-}
  call = U.all
  {-# INLINE [1] cfoldl1 #-}
  cfoldl1 = U.foldl1
  {-# INLINE [1] cfoldr1 #-}
  cfoldr1 = U.foldr1
  {-# INLINE [1] csum #-}
  csum = U.sum
  {-# INLINE [1] cproduct #-}
  cproduct = U.product
  {-# INLINE [1] cmaximum #-}
  cmaximum = U.maximum
  {-# INLINE [1] cminimum #-}
  cminimum = U.minimum
  {-# INLINE cbasicToList #-}
  cbasicToList = U.toList
  {-# INLINE [1] clast #-}
  clast = U.last
  {-# INLINE [1] chead #-}
  chead = U.head
  {-# INLINE [1] cfind #-}
  cfind = U.find
  {-# INLINE [1] cfindIndex #-}
  cfindIndex = U.findIndex
  {-# INLINE [1] cfindIndices #-}
  cfindIndices = fmap U.toList . U.findIndices
  {-# INLINE [1] celemIndex #-}
  celemIndex = U.elemIndex
  {-# INLINE [1] celemIndices #-}
  celemIndices = fmap U.toList . U.elemIndices

instance CFoldable S.Vector where
  {-# INLINE [1] cfoldr #-}
  cfoldr = S.foldr
  {-# INLINE [1] cfoldr' #-}
  cfoldr' = S.foldr'
  {-# INLINE [1] cfoldl #-}
  cfoldl = S.foldl
  {-# INLINE [1] cfoldl' #-}
  cfoldl' = S.foldl'
  {-# INLINE cfoldlM #-}
  cfoldlM = S.foldM
  {-# INLINE cfoldlM' #-}
  cfoldlM' = S.foldM'
  {-# INLINE [1] cindex #-}
  cindex = (S.!)
  {-# INLINE [1] celem #-}
  celem = S.elem
  {-# INLINE [1] cnotElem #-}
  cnotElem = S.notElem
  {-# INLINE [1] cany #-}
  cany = S.any
  {-# INLINE [1] call #-}
  call = S.all
  {-# INLINE [1] cfoldl1 #-}
  cfoldl1 = S.foldl1
  {-# INLINE [1] cfoldr1 #-}
  cfoldr1 = S.foldr1
  {-# INLINE [1] csum #-}
  csum = S.sum
  {-# INLINE [1] cproduct #-}
  cproduct = S.product
  {-# INLINE [1] cmaximum #-}
  cmaximum = S.maximum
  {-# INLINE [1] cminimum #-}
  cminimum = S.minimum
  {-# INLINE cbasicToList #-}
  cbasicToList = S.toList
  {-# INLINE [1] clast #-}
  clast = S.last
  {-# INLINE [1] chead #-}
  chead = S.head
  {-# INLINE [1] cfind #-}
  cfind = S.find
  {-# INLINE [1] cfindIndex #-}
  cfindIndex = S.findIndex
  {-# INLINE [1] cfindIndices #-}
  cfindIndices = fmap S.toList . S.findIndices
  {-# INLINE [1] celemIndex #-}
  celemIndex = S.elemIndex
  {-# INLINE [1] celemIndices #-}
  celemIndices = fmap S.toList . S.elemIndices

instance CFoldable P.Vector where
  {-# INLINE [1] cfoldr #-}
  cfoldr = P.foldr
  {-# INLINE [1] cfoldr' #-}
  cfoldr' = P.foldr'
  {-# INLINE [1] cfoldl #-}
  cfoldl = P.foldl
  {-# INLINE [1] cfoldl' #-}
  cfoldl' = P.foldl'
  {-# INLINE cfoldlM #-}
  cfoldlM = P.foldM
  {-# INLINE cfoldlM' #-}
  cfoldlM' = P.foldM'
  {-# INLINE [1] cindex #-}
  cindex = (P.!)
  {-# INLINE [1] celem #-}
  celem = P.elem
  {-# INLINE [1] cnotElem #-}
  cnotElem = P.notElem
  {-# INLINE [1] cany #-}
  cany = P.any
  {-# INLINE [1] call #-}
  call = P.all
  {-# INLINE [1] cfoldl1 #-}
  cfoldl1 = P.foldl1
  {-# INLINE [1] cfoldr1 #-}
  cfoldr1 = P.foldr1
  {-# INLINE [1] csum #-}
  csum = P.sum
  {-# INLINE [1] cproduct #-}
  cproduct = P.product
  {-# INLINE [1] cmaximum #-}
  cmaximum = P.maximum
  {-# INLINE [1] cminimum #-}
  cminimum = P.minimum
  {-# INLINE cbasicToList #-}
  cbasicToList = P.toList
  {-# INLINE [1] clast #-}
  clast = P.last
  {-# INLINE [1] chead #-}
  chead = P.head
  {-# INLINE [1] cfind #-}
  cfind = P.find
  {-# INLINE [1] cfindIndex #-}
  cfindIndex = P.findIndex
  {-# INLINE [1] cfindIndices #-}
  cfindIndices = fmap P.toList . P.findIndices
  {-# INLINE [1] celemIndex #-}
  celemIndex = P.elemIndex
  {-# INLINE [1] celemIndices #-}
  celemIndices = fmap P.toList . P.elemIndices

instance CTraversable V.Vector where
  ctraverse = traverse
  {-# INLINE [1] ctraverse #-}

instance CTraversable U.Vector where
  ctraverse = \f -> fmap S.convert . traverse f . U.convert @_ @_ @V.Vector
  {-# INLINE [1] ctraverse #-}

instance CTraversable S.Vector where
  ctraverse = \f -> fmap S.convert . traverse f . U.convert @_ @_ @V.Vector
  {-# INLINE [1] ctraverse #-}

instance CTraversable P.Vector where
  ctraverse = \f -> fmap P.convert . traverse f . U.convert @_ @_ @V.Vector
  {-# INLINE [1] ctraverse #-}

{-# RULES
"cindex/IsSequence" forall (xs :: (MT.Index mono ~ Int, IsSequence mono) => WrapMono mono b).
  cindex xs = withMonoCoercible (coerce @(mono -> Int -> Element mono) indexEx xs)
  #-}

{-# RULES
"cfromList/ctoList" [~1]
  cfromList . ctoList = id
"cfromList/ctoList" [~1] forall xs.
  cfromList (ctoList xs) = xs
  #-}

{-# RULES
"ctoList/cfromList" [~1]
  ctoList . cfromList = id
"ctoList/cfromList" forall xs.
  ctoList (cfromList xs) = xs
  #-}
-- | Free monoid functor from fullsubcategory.
--   It must be a pointed foldable functor with the property
--   that for any 'Monoid' @w@ and @f :: a -> w@,
--   @'cfoldMap' f@ must be a monoid homomorphism and the following
--   must be hold:
--
--    @
--       'cfoldMap' f . 'cpure' == f
--    @
--
--   Hence, @'Set's@ cannot be a free monoid functor;
class (CFunctor f, forall x. Dom f x => Monoid (f x), CPointed f, CFoldable f)
  => CFreeMonoid f where
  cbasicFromList :: Dom f a => [a] -> f a
  cbasicFromList = foldr ((<>) . cpure) mempty
  {-# INLINE cbasicFromList #-}

  ccons :: Dom f a => a -> f a -> f a
  {-# INLINE [1] ccons #-}
  ccons = (<>) . cpure

  csnoc :: Dom f a => f a -> a -> f a
  {-# INLINE [1] csnoc #-}
  csnoc = (. cpure) . (<>)

  {- |
    The 'cfromListN' function takes the input list's length as a hint. Its behaviour should be equivalent to 'cfromList'. The hint can be used to construct the structure l more efficiently compared to 'cfromList'.
    If the given hint does not equal to the input list's length the behaviour of fromListN is not specified.
  -}
  cfromListN :: Dom f a => Int -> [a] -> f a
  cfromListN = const cfromList
  {-# INLINE [1] cfromListN #-}

  ctake :: Dom f a => Int -> f a -> f a
  {-# INLINE [1] ctake #-}
  ctake n = cfromList . take n . ctoList

  cdrop :: Dom f a => Int -> f a -> f a
  {-# INLINE [1] cdrop #-}
  cdrop n = cfromList . drop n . ctoList

  cinit :: Dom f a => f a -> f a
  {-# INLINE [1] cinit #-}
  cinit = cfromList . init . ctoList

  ctail :: Dom f a => f a -> f a
  ctail = cfromList . tail . ctoList

  csplitAt :: Dom f a => Int -> f a -> (f a, f a)
  {-# INLINE [1] csplitAt #-}
  csplitAt n = (\(a, b) -> (cfromList a, cfromList b)) . splitAt n . ctoList

  creplicate :: Dom f a => Int -> a -> f a
  {-# INLINE [1] creplicate #-}
  creplicate n = cfromList . replicate n

  cgenerate :: Dom f a => Int -> (Int -> a) -> f a
  {-# INLINE [1] cgenerate #-}
  cgenerate = \n f ->
    cfromList [f i | i <- [0.. n - 1]]

  cgenerateM :: (Dom f a, Monad m) => Int -> (Int -> m a) -> m (f a)
  {-# INLINE [1] cgenerateM #-}
  cgenerateM = \n f ->
    cfromList <$> mapM f [0..n-1]

  cgenerateA :: (Dom f a, Applicative g) => Int -> (Int -> g a) -> g (f a)
  {-# INLINE [1] cgenerateA #-}
  cgenerateA = \n f ->
    cfromList <$> traverse f [0..n-1]

  cuncons :: Dom f a => f a -> Maybe (a, f a)
  {-# INLINE [1] cuncons #-}
  cuncons = fmap (second cfromList) . uncons . ctoList

  cunsnoc :: Dom f a => f a -> Maybe (f a, a)
  {-# INLINE [1] cunsnoc #-}
  cunsnoc = fmap (first cfromList) . MT.unsnoc . ctoList

  creverse :: Dom f a => f a -> f a
  {-# INLINE [1] creverse #-}
  creverse = cfromList . reverse . ctoList

  cintersperse :: Dom f a => a -> f a -> f a
  cintersperse = \a -> cfromList . intersperse a . ctoList

  cnub :: (Dom f a, Eq a) => f a -> f a
  {-# INLINE [1] cnub #-}
  cnub = cfromList . nub . ctoList

  cnubOrd :: (Dom f a, Ord a) => f a -> f a
  {-# INLINE [1] cnubOrd #-}
  cnubOrd = cfromList . L.foldOver cfolded L.nub

  csort :: (Dom f a, Ord a) => f a -> f a
  {-# INLINE [1] csort #-}
  csort = cfromList . List.sort . ctoList

  csortBy :: (Dom f a) => (a -> a -> Ordering) -> f a -> f a
  {-# INLINE [1] csortBy #-}
  csortBy = \f -> cfromList . List.sortBy f . ctoList

  cinsert :: (Dom f a, Ord a) => a -> f a -> f a
  {-# INLINE [1] cinsert #-}
  cinsert = \a -> cfromList . List.insert a . ctoList

  cinsertBy :: (Dom f a) => (a -> a -> Ordering) -> a -> f a -> f a
  {-# INLINE [1] cinsertBy #-}
  cinsertBy = \f a -> cfromList . List.insertBy f a . ctoList

  ctakeWhile :: Dom f a => (a -> Bool) -> f a -> f a
  {-# INLINE [1] ctakeWhile #-}
  ctakeWhile = \f -> cfromList . takeWhile f . ctoList

  cdropWhile :: Dom f a => (a -> Bool) -> f a -> f a
  {-# INLINE [1] cdropWhile #-}
  cdropWhile = \f -> cfromList . dropWhile f . ctoList

  cspan :: Dom f a => (a -> Bool) -> f a -> (f a, f a)
  {-# INLINE [1] cspan #-}
  cspan = \f -> (cfromList *** cfromList) . span f . ctoList

  cbreak :: Dom f a => (a -> Bool) -> f a -> (f a, f a)
  {-# INLINE [1] cbreak #-}
  cbreak = \f -> (cfromList *** cfromList) . break f . ctoList

  cfilter :: Dom f a => (a -> Bool) -> f a -> f a
  {-# INLINE [1] cfilter #-}
  cfilter = \f -> cfromList . filter f . ctoList

  cpartition :: Dom f a => (a -> Bool) -> f a -> (f a, f a)
  {-# INLINE [1] cpartition #-}
  cpartition = \f -> (cfromList *** cfromList) . List.partition f . ctoList

  -- TODO: more ListLike equivalent functions here

instance CFreeMonoid [] where
  cbasicFromList = id
  {-# INLINE cbasicFromList #-}
  cfromListN = take
  {-# INLINE [1] cfromListN #-}
  ccons = (:)
  {-# INLINE [1] ccons #-}
  csnoc = \xs x -> xs ++ [x]
  {-# INLINE [1] csnoc #-}
  ctake = take
  {-# INLINE [1] ctake #-}
  cdrop = drop
  {-# INLINE [1] cdrop #-}
  cinit = init
  {-# INLINE [1] cinit #-}
  ctail = tail
  {-# INLINE [1] ctail #-}
  csplitAt = splitAt
  {-# INLINE [1] csplitAt #-}
  creplicate = replicate
  {-# INLINE [1] creplicate #-}
  cgenerateM = \n f -> mapM f [0..n-1]
  {-# INLINE [1] cgenerateM #-}
  cgenerateA = \n f -> traverse f [0..n-1]
  {-# INLINE [1] cgenerateA #-}
  cuncons = uncons
  {-# INLINE [1] cuncons #-}
  cunsnoc = MT.unsnoc
  {-# INLINE [1] cunsnoc #-}
  creverse = reverse
  {-# INLINE [1] creverse #-}
  cintersperse = intersperse
  {-# INLINE [1] cintersperse #-}
  cnub = cnub
  {-# INLINE [1] cnub #-}
  csort = List.sort
  {-# INLINE [1] csort #-}
  csortBy = List.sortBy
  {-# INLINE [1] csortBy #-}
  ctakeWhile = takeWhile
  {-# INLINE [1] ctakeWhile #-}
  cdropWhile = dropWhile
  {-# INLINE [1] cdropWhile #-}
  cspan = span
  {-# INLINE [1] cspan #-}
  cbreak = break
  {-# INLINE [1] cbreak #-}
  cfilter = filter
  {-# INLINE [1] cfilter #-}
  cpartition = List.partition
  {-# INLINE [1] cpartition #-}

fmap concat $ forM
  [''V.Vector, ''U.Vector, ''S.Vector, ''P.Vector]
  $ \vecTy@(Name _ (NameG _ pkg modl0@(ModName mn))) ->
    let modl = maybe modl0 (ModName . T.unpack)
          $ T.stripSuffix ".Base" $ T.pack mn
        modFun fun = varE $
          Name (OccName fun) (NameG VarName pkg modl)
    in [d|
    instance CFreeMonoid $(conT vecTy) where
      cbasicFromList = $(modFun "fromList")
      {-# INLINE cbasicFromList #-}
      cfromListN = $(modFun "fromListN")
      {-# INLINE [1] cfromListN #-}
      ccons = $(modFun "cons")
      {-# INLINE [1] ccons #-}
      csnoc = $(modFun "snoc")
      {-# INLINE [1] csnoc #-}
      ctake = $(modFun "take")
      {-# INLINE [1] ctake #-}
      cdrop = $(modFun "drop")
      {-# INLINE [1] cdrop #-}
      cinit = $(modFun "init")
      {-# INLINE [1] cinit #-}
      ctail = $(modFun "tail")
      {-# INLINE [1] ctail #-}
      csplitAt = $(modFun "splitAt")
      {-# INLINE [1] csplitAt #-}
      creplicate = $(modFun "replicate")
      {-# INLINE [1] creplicate #-}
      cgenerate = $(modFun "generate")
      {-# INLINE [1] cgenerate #-}
      cgenerateM = $(modFun "generateM")
      {-# INLINE [1] cgenerateM #-}
      cgenerateA = \n f ->
        fmap VB.build
        $ getAp $ foldMap (Ap . fmap VB.singleton . f) [0..n-1]
      {-# INLINE [1] cgenerateA #-}
      cuncons = \xs ->
        if $(modFun "null") xs
        then Nothing
        else Just ($(modFun "head") xs, $(modFun "tail") xs)
      {-# INLINE [1] cuncons #-}
      cunsnoc = \xs ->
        if $(modFun "null") xs
        then Nothing
        else Just ($(modFun "init") xs, $(modFun "last") xs)
      {-# INLINE [1] cunsnoc #-}
      creverse = $(modFun "reverse")
      {-# INLINE [1] creverse #-}
      cnubOrd = $(modFun "uniq") . $(modFun "modify") AI.sort
      {-# INLINE cnubOrd #-}
      csort = $(modFun "modify") AI.sort
      {-# INLINE [1] csort #-}
      csortBy = \f -> $(modFun "modify") $ AI.sortBy f
      {-# INLINE [1] csortBy #-}
      ctakeWhile = $(modFun "takeWhile")
      {-# INLINE [1] ctakeWhile #-}
      cdropWhile = $(modFun "dropWhile")
      {-# INLINE [1] cdropWhile #-}
      cspan = $(modFun "span")
      {-# INLINE [1] cspan #-}
      cbreak = $(modFun "break")
      {-# INLINE [1] cbreak #-}
      cfilter = $(modFun "filter")
      {-# INLINE [1] cfilter #-}
      cpartition = $(modFun "partition")
      {-# INLINE [1] cpartition #-}
    |]

instance CFreeMonoid PA.PrimArray where
  cbasicFromList = PA.primArrayFromList
  {-# INLINE cbasicFromList #-}
  cfromListN = PA.primArrayFromListN
  {-# INLINE [1] cfromListN #-}
  cgenerate = PA.generatePrimArray
  {-# INLINE [1] cgenerate #-}
  cgenerateM = PA.generatePrimArrayA
  {-# INLINE [1] cgenerateM #-}
  cgenerateA = PA.generatePrimArrayA
  {-# INLINE [1] cgenerateA #-}
  cfilter = PA.filterPrimArray
  {-# INLINE [1] cfilter #-}
  creplicate = PA.replicatePrimArray
  {-# INLINE [1] creplicate #-}

instance CFreeMonoid SA.SmallArray where
  cbasicFromList = SA.smallArrayFromList
  {-# INLINE cbasicFromList #-}
  cfromListN = SA.smallArrayFromListN
  {-# INLINE [1] cfromListN #-}

instance CFreeMonoid A.Array where
#if MIN_VERSION_primitive(0,9,0)
  cbasicFromList = A.arrayFromList
  {-# INLINE cbasicFromList #-}
  cfromListN = A.arrayFromListN
  {-# INLINE [1] cfromListN #-}
#else
  cbasicFromList = A.fromList
  {-# INLINE cbasicFromList #-}
  cfromListN = A.fromListN
  {-# INLINE [1] cfromListN #-}
#endif

instance CFreeMonoid Seq.Seq where
  cbasicFromList = Seq.fromList
  {-# INLINE cbasicFromList #-}
  cfromListN = GHC.fromListN
  {-# INLINE [1] cfromListN #-}

instance MT.IsSequence mono
      => CFreeMonoid (WrapMono mono) where
  cbasicFromList = coerce $ MT.fromList @mono
  {-# INLINE cbasicFromList #-}
  cfromListN = \n -> coerce $ MT.take (fromIntegral n) . MT.fromList @mono
  {-# INLINE [1] cfromListN #-}
  ctake = coerce . MT.take @mono . fromIntegral
  {-# INLINE [1] ctake #-}
  cdrop = coerce . MT.drop @mono . fromIntegral
  {-# INLINE [1] cdrop #-}
  ccons = coerce $ MT.cons @mono
  {-# INLINE ccons #-}
  csnoc = coerce $ MT.snoc @mono
  {-# INLINE [1] csnoc #-}
  cuncons = coerce $ MT.uncons @mono
  {-# INLINE [1] cuncons #-}
  cunsnoc = coerce $ MT.unsnoc @mono
  {-# INLINE [1] cunsnoc #-}
  ctail = coerce $ MT.tailEx @mono
  {-# INLINE [1] ctail #-}
  cinit = coerce $ MT.initEx @mono
  {-# INLINE [1] cinit #-}
  csplitAt = coerce $ \(n :: Int) ->
      MT.splitAt @mono (fromIntegral n :: MT.Index mono)
  {-# INLINE [1] csplitAt #-}
  creplicate = coerce $ \(n :: Int) ->
      MT.replicate @mono (fromIntegral n :: MT.Index mono)
  {-# INLINE [1] creplicate #-}
  creverse = coerce $ MT.reverse @mono
  {-# INLINE [1] creverse #-}
  cintersperse = coerce $ MT.intersperse @mono
  {-# INLINE [1] cintersperse #-}
  csort = coerce $ MT.sort @mono
  {-# INLINE [1] csort #-}
  csortBy = coerce $ MT.sortBy @mono
  {-# INLINE [1] csortBy #-}
  ctakeWhile = coerce $ MT.takeWhile @mono
  {-# INLINE [1] ctakeWhile #-}
  cdropWhile = coerce $ MT.dropWhile @mono
  {-# INLINE [1] cdropWhile #-}
  cbreak = coerce $ MT.break @mono
  {-# INLINE [1] cbreak #-}
  cspan = coerce $ MT.span @mono
  {-# INLINE [1] cspan #-}
  cfilter = coerce $ MT.filter @mono
  {-# INLINE [1] cfilter #-}
  cpartition = coerce $ MT.partition @mono
  {-# INLINE [1] cpartition #-}

cctraverseFreeMonoid
  ::  ( CFreeMonoid t, CApplicative f, CPointed f,
        Dom t a, Dom f (t b), Dom f b, Dom t b,
        Dom f (t b, t b)
      )
  => (a -> f b) -> t a -> f (t b)
cctraverseFreeMonoid f =
  runCApp . cfoldMap (CApp . cmap cpure . f)

cctraverseZipFreeMonoid
  :: ( CFreeMonoid t, CRepeat f,
        Dom t a, Dom f (t b), Dom f b, Dom t b,
        Dom f (t b, t b)
      )
  => (a -> f b) -> t a -> f (t b)
cctraverseZipFreeMonoid f =
  runCZippy . cfoldMap (CZippy . cmap cpure . f)

-- | Lifts 'CFoldable' along given function.
--
--  @
--    cfolding :: (CFoldable t, Dom t a) => (s -> t a) -> Fold s a
--  @
cfolding
  :: (CFoldable t, Dom t a, Contravariant f, Applicative f)
  => (s -> t a)
  -> (a -> f a) -> s -> f s
{-# INLINE cfolding #-}
cfolding = \sfa agb -> phantom . ctraverse_ agb . sfa
