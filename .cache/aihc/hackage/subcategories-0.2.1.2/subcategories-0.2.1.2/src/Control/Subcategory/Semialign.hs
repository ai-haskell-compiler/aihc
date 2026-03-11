{-# LANGUAGE BangPatterns, CPP, DerivingStrategies, DerivingVia         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiWayIf, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators                                              #-}
module Control.Subcategory.Semialign
  ( CSemialign(..), CAlign(..),
    csalign, cpadZip, cpadZipWith
  ) where
import           Control.Applicative                  (ZipList)
import           Control.Monad                        (forM_)
import           Control.Monad.ST.Strict              (runST)
import           Control.Subcategory.Functor
import           Control.Subcategory.Wrapper.Internal
import           Data.Bifunctor                       (Bifunctor (bimap))
import           Data.Coerce
import           Data.Containers
import           Data.Functor.Compose                 (Compose (..))
import           Data.Functor.Identity                (Identity)
import qualified Data.Functor.Product                 as SOP
import           Data.Hashable                        (Hashable)
import           Data.HashMap.Strict                  (HashMap)
import           Data.IntMap.Strict                   (IntMap)
import qualified Data.IntSet                          as IS
import           Data.List.NonEmpty                   (NonEmpty)
import           Data.Map                             (Map)
import           Data.MonoTraversable
import qualified Data.Primitive.Array                 as A
import qualified Data.Primitive.PrimArray             as PA
import qualified Data.Primitive.SmallArray            as SA
import           Data.Proxy                           (Proxy)
import           Data.Semialign
#if !MIN_VERSION_base(4,16,0)
import           Data.Semigroup                       (Option (..))
#endif
import           Data.Sequence                        (Seq)
import qualified Data.Sequences                       as MT
import           Data.These                           (These (..), fromThese,
                                                       mergeThese)
import           Data.Tree                            (Tree)
import qualified Data.Vector                          as V
import qualified Data.Vector.Primitive                as P
import qualified Data.Vector.Storable                 as S
import qualified Data.Vector.Unboxed                  as U
import           GHC.Generics                         ((:*:) (..), (:.:) (..))

class CFunctor f => CSemialign f where
  {-# MINIMAL calignWith #-}
  calignWith
    :: (Dom f a, Dom f b, Dom f c)
    => (These a b -> c) -> f a -> f b -> f c
  calign
    :: (Dom f a, Dom f b, Dom f (These a b))
    => f a -> f b -> f (These a b)
  {-# INLINE [1] calign #-}
  calign = calignWith id

instance Semialign f => CSemialign (WrapFunctor f) where
  calignWith = alignWith
  {-# INLINE [1] calignWith #-}
  calign = align
  {-# INLINE [1] calign #-}

instance {-# OVERLAPPING #-}  CSemialign (WrapMono IS.IntSet) where
  calignWith f = withMonoCoercible @IS.IntSet $
    coerce @(IS.IntSet -> IS.IntSet -> IS.IntSet) $ \ l r ->
    let ints = l `IS.intersection` r
    in IS.unions
          [ IS.map (f . This) l
          , IS.map (f . That) r
          , IS.map (\x -> f $ These x x) ints
          ]
  {-# INLINE [1] calignWith #-}

class CSemialign f => CAlign f where
  cnil :: Dom f a => f a

instance Align f => CAlign (WrapFunctor f) where
  cnil = WrapFunctor nil
  {-# INLINE [1] cnil #-}

deriving via WrapFunctor [] instance CSemialign []
deriving via WrapFunctor [] instance CAlign []
deriving via WrapFunctor Maybe instance CSemialign Maybe
deriving via WrapFunctor Maybe instance CAlign Maybe
#if !MIN_VERSION_base(4,16,0)
#if MIN_VERSION_semialign(1,1,0)
deriving via WrapFunctor Option instance CSemialign Option
deriving via WrapFunctor Option instance CAlign Option
#else
deriving newtype instance CSemialign Option
deriving newtype instance CAlign Option
#endif
#endif

deriving via WrapFunctor ZipList instance CSemialign ZipList
deriving via WrapFunctor ZipList instance CAlign ZipList
deriving via WrapFunctor Identity instance CSemialign Identity
deriving via WrapFunctor NonEmpty instance CSemialign NonEmpty
deriving via WrapFunctor IntMap instance CSemialign IntMap
deriving via WrapFunctor IntMap instance CAlign IntMap
deriving via WrapFunctor Tree instance CSemialign Tree
deriving via WrapFunctor Seq instance CSemialign Seq
deriving via WrapFunctor Seq instance CAlign Seq
deriving via WrapFunctor V.Vector instance CSemialign V.Vector
deriving via WrapFunctor V.Vector instance CAlign V.Vector
deriving via WrapFunctor Proxy instance CSemialign Proxy
deriving via WrapFunctor Proxy instance CAlign Proxy
deriving via WrapFunctor (Map k) instance Ord k => CSemialign (Map k)
deriving via WrapFunctor (Map k) instance Ord k => CAlign (Map k)
deriving via WrapFunctor (HashMap k)
  instance (Eq k, Hashable k) => CSemialign (HashMap k)
deriving via WrapFunctor (HashMap k)
  instance (Eq k, Hashable k) => CAlign (HashMap k)
deriving via WrapFunctor ((->) s) instance CSemialign ((->) s)

instance (CSemialign f, CSemialign g) => CSemialign (SOP.Product f g) where
  calign (SOP.Pair a b) (SOP.Pair c d) = SOP.Pair (calign a c) (calign b d)
  {-# INLINE [1] calign #-}
  calignWith f (SOP.Pair a b) (SOP.Pair c d) =
    SOP.Pair (calignWith f a c) (calignWith f b d)
  {-# INLINE [1] calignWith #-}

instance (CAlign f, CAlign g) => CAlign (SOP.Product f g) where
  cnil = SOP.Pair cnil cnil
  {-# INLINE [1] cnil #-}

instance (CSemialign f, CSemialign g) => CSemialign (f :*: g) where
  calign ((:*:) a b) ((:*:) c d) = (:*:) (calign a c) (calign b d)
  {-# INLINE [1] calign #-}
  calignWith f ((:*:) a b) ((:*:) c d) =
    (:*:) (calignWith f a c) (calignWith f b d)
  {-# INLINE [1] calignWith #-}

instance (CAlign f, CAlign g) => CAlign (f :*: g) where
  cnil = cnil :*: cnil
  {-# INLINE [1] cnil #-}

instance (CSemialign f, CSemialign g) => CSemialign (Compose f g) where
  calignWith f (Compose x) (Compose y) = Compose (calignWith g x y)
    where
      g (This ga)     = cmap (f . This) ga
      g (That gb)     = cmap (f . That) gb
      g (These ga gb) = calignWith f ga gb
  {-# INLINE [1] calignWith #-}

instance (CAlign f, CSemialign g) => CAlign (Compose f g) where
  cnil = Compose cnil
  {-# INLINE [1] cnil #-}

instance (CSemialign f, CSemialign g) => CSemialign ((:.:) f g) where
  calignWith f (Comp1 x) (Comp1 y) = Comp1 (calignWith g x y)
    where
      g (This ga)     = cmap (f . This) ga
      g (That gb)     = cmap (f . That) gb
      g (These ga gb) = calignWith f ga gb
  {-# INLINE [1] calignWith #-}

instance (CAlign f, CSemialign g) => CAlign ((:.:) f g) where
  cnil = Comp1 cnil
  {-# INLINE [1] cnil #-}

instance CSemialign U.Vector where
  calignWith = alignVectorWith
  {-# INLINE [1] calignWith #-}

instance CAlign U.Vector where
  cnil = U.empty
  {-# INLINE [1] cnil #-}

instance CSemialign S.Vector where
  calignWith = alignVectorWith
  {-# INLINE [1] calignWith #-}

instance CAlign S.Vector where
  cnil = S.empty
  {-# INLINE [1] cnil #-}

instance CSemialign P.Vector where
  calignWith = alignVectorWith
  {-# INLINE [1] calignWith #-}

instance CAlign P.Vector where
  cnil = P.empty
  {-# INLINE [1] cnil #-}

instance CSemialign SA.SmallArray where
  calignWith f l r = runST $ do
    let !lenL = length l
        !lenR = length r
        (isLftShort, thresh, len)
          | lenL < lenR = (True, lenL, lenR)
          | otherwise = (False, lenR, lenL)
    sa <- SA.newSmallArray len (error "Uninitialised element")
    forM_ [0..len-1] $ \n ->
      if  | n == len -> pure ()
          | n < thresh ->
            SA.writeSmallArray sa n
            $ f $ These
              (SA.indexSmallArray l n)
              (SA.indexSmallArray r n)
          | isLftShort ->
            SA.writeSmallArray sa n
            $ f $ That $ SA.indexSmallArray r n
          | otherwise ->
            SA.writeSmallArray sa n
            $ f $ This $ SA.indexSmallArray l n
    SA.unsafeFreezeSmallArray sa
  {-# INLINE [1] calignWith #-}

instance CAlign SA.SmallArray where
  cnil = SA.smallArrayFromListN 0 []
  {-# INLINE [1] cnil #-}

instance CSemialign A.Array where
  calignWith f l r = runST $ do
    let !lenL = length l
        !lenR = length r
        (isLftShort, thresh, len)
          | lenL < lenR = (True, lenL, lenR)
          | otherwise = (False, lenR, lenL)
    sa <- A.newArray len (error "Uninitialised element")
    forM_ [0..len-1] $ \n ->
      if  | n == len -> pure ()
          | n < thresh ->
            A.writeArray sa n
            $ f $ These
              (A.indexArray l n)
              (A.indexArray r n)
          | isLftShort ->
            A.writeArray sa n
            $ f $ That $ A.indexArray r n
          | otherwise ->
            A.writeArray sa n
            $ f $ This $ A.indexArray l n
    A.unsafeFreezeArray sa
  {-# INLINE [1] calignWith #-}

instance CAlign A.Array where
#if MIN_VERSION_primitive(0,9,0)
  cnil = A.arrayFromListN 0 []
#else
  cnil = A.fromListN 0 []
#endif
  {-# INLINE [1] cnil #-}

instance CSemialign PA.PrimArray where
  calignWith f l r = runST $ do
    let !lenL = PA.sizeofPrimArray l
        !lenR = PA.sizeofPrimArray r
        (isLftShort, thresh, len)
          | lenL < lenR = (True, lenL, lenR)
          | otherwise = (False, lenR, lenL)
    sa <- PA.newPrimArray len
    forM_ [0..len-1] $ \n ->
      if  | n == len -> pure ()
          | n < thresh ->
            PA.writePrimArray sa n
            $ f $ These
              (PA.indexPrimArray l n)
              (PA.indexPrimArray r n)
          | isLftShort ->
            PA.writePrimArray sa n
            $ f $ That $ PA.indexPrimArray r n
          | otherwise ->
            PA.writePrimArray sa n
            $ f $ This $ PA.indexPrimArray l n
    PA.unsafeFreezePrimArray sa
  {-# INLINE [1] calignWith #-}

instance CAlign PA.PrimArray where
  cnil = PA.primArrayFromListN 0 []
  {-# INLINE [1] cnil #-}

instance (MT.IsSequence mono, MonoZip mono)
  => CSemialign (WrapMono mono) where
  calignWith f = coerce go
    where
      go :: mono -> mono -> mono
      go ls rs
        | lenL == lenR = ozipWith (fmap f . These) ls rs
        | lenL < lenR  =
            ozipWith (fmap f . These) ls rs
            <> omap (f . That) (MT.drop (fromIntegral lenL) rs)
        | otherwise  =
            ozipWith (fmap f . These) ls rs
            <> omap (f . This) (MT.drop (fromIntegral lenL) ls)
        where lenL = olength ls
              lenR = olength rs

instance (MT.IsSequence mono, MonoZip mono)
  => CAlign (WrapMono mono) where
  cnil = WrapMono mempty

csalign :: (CSemialign f, Dom f a, Semigroup a)
  => f a -> f a -> f a
{-# INLINE [1] csalign #-}
csalign = calignWith $ mergeThese (<>)

cpadZip
  :: (CSemialign f, Dom f a, Dom f b, Dom f (Maybe a, Maybe b))
  => f a -> f b -> f (Maybe a, Maybe b)
{-# INLINE [1] cpadZip #-}
cpadZip = calignWith (fromThese Nothing Nothing . bimap Just Just)

cpadZipWith
  :: (CSemialign f, Dom f a, Dom f b, Dom f c)
  => (Maybe a -> Maybe b -> c)
  -> f a -> f b -> f c
{-# INLINE [1] cpadZipWith #-}
cpadZipWith f = calignWith $
  uncurry f . fromThese Nothing Nothing . bimap Just Just
