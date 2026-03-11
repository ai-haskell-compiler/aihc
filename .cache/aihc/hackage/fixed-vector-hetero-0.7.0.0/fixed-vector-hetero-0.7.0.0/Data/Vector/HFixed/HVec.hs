{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Heterogeneous vector parametric in its elements
module Data.Vector.HFixed.HVec (
    -- * Generic heterogeneous vector
    HVec
  , HVecF
  ) where

import Control.Monad.ST        (ST,runST)
import Data.Functor.Identity   (Identity(..))
import Data.Functor.Classes
import Control.DeepSeq         (NFData(..))
#if !MIN_VERSION_base(4,13,0)
import Data.Semigroup          (Semigroup(..))
#endif
import Data.Monoid             (All(..))
import Data.List               (intersperse,intercalate)
import Data.Kind               (Type)
import Data.Primitive.SmallArray ( SmallArray, SmallMutableArray, newSmallArray
                                 , writeSmallArray, indexSmallArray
                                 , unsafeFreezeSmallArray)
import GHC.Exts                (Any)
import Unsafe.Coerce           (unsafeCoerce)

import qualified Data.Vector.HFixed     as H
import Data.Vector.HFixed.Class



----------------------------------------------------------------
-- HVecF
----------------------------------------------------------------

-- | Heterogeneous vector parametrized by common type constructor.
newtype HVecF (xs :: [Type]) (f :: Type -> Type) = HVecF (SmallArray Any)

instance Arity xs => HVectorF (HVecF xs) where
  type ElemsF (HVecF xs) = xs
  inspectF (HVecF arr)
    = runContVecF
    $ apply (\(T_insp i a) -> ( unsafeCoerce $ indexSmallArray a i
                              , T_insp (i+1) a))
            (T_insp 0 arr)
  {-# INLINE inspectF #-}
  constructF = accum
    (\(T_con i box) a -> T_con (i+1) (writeToBox (unsafeCoerce a) i box))
    (\(T_con _ box)   -> HVecF $ runBox len box)
    (T_con 0 (Box $ \_ -> return ()))
    where
    len = arity (Proxy @xs)
  {-# INLINE constructF #-}

data T_insp (xs :: [Type]) = T_insp Int (SmallArray Any)
data T_con  (xs :: [Type]) = T_con  Int (Box Any)

-- Helper data type for creating of array
newtype Box a = Box (forall s. SmallMutableArray s a -> ST s ())

writeToBox :: a -> Int -> Box a -> Box a
{-# INLINE writeToBox #-}
writeToBox a i (Box f) = Box $ \arr -> f arr >> (writeSmallArray arr i $! a)

runBox :: Int -> Box a -> SmallArray a
{-# INLINE runBox #-}
runBox size (Box f) = runST $ do arr <- newSmallArray size uninitialised
                                 f arr
                                 unsafeFreezeSmallArray arr

uninitialised :: a
uninitialised = error "Data.Vector.HFixed: uninitialised element"


instance (Show1 f, ArityC Show xs) => Show (HVecF xs f) where
  showsPrec _ v = showChar '['
                . ( foldr (.) id
                  $ intersperse (showChar ',')
                  $ H.foldrF (Proxy @Show) (\x xs -> showsPrec1 0 x : xs) [] v
                  )
                . showChar ']'
instance (Eq1 f, ArityC Eq xs) => Eq (HVecF xs f) where
  v == u = getAll $ H.zipFoldF (Proxy @Eq) (\x y -> All (eq1 x y)) v u
instance (Ord1 f, ArityC Eq xs, ArityC Ord xs) => Ord (HVecF xs f) where
  compare = H.zipFoldF (Proxy :: Proxy Ord) compare1

----------------------------------------------------------------
-- HVec
----------------------------------------------------------------

-- | Generic heterogeneous vector
newtype HVec (xs :: [Type]) = HVec (HVecF xs Identity)

instance Arity xs => HVector (HVec xs) where
  type Elems (HVec xs) = xs
  inspect (HVec v) = inspectF v
  construct = HVec <$> constructF
  {-# INLINE inspect   #-}
  {-# INLINE construct #-}

instance (ArityC Show xs) => Show (HVec xs) where
  show v
    = "[" ++ intercalate ", " (H.foldr (Proxy :: Proxy Show) (\x xs -> show x : xs) [] v) ++ "]"

instance (ArityC Eq xs) => Eq (HVec xs) where
  (==) = H.eq
  {-# INLINE (==) #-}

-- NOTE: We need to add `Eq (HVec xs)' since GHC cannot deduce that
--       `ArityC Ord xs => ArityC Eq xs' for all xs
instance (ArityC Ord xs, ArityC Eq xs) => Ord (HVec xs) where
  compare = H.compare
  {-# INLINE compare #-}

instance ( ArityC Monoid xs
-- NOTE: Sadly we cannot infer `ArityC Semigroup' xs from `ArityC Monoid xs'
--       Thus we have to specify both
         , ArityC Semigroup xs
         ) => Monoid (HVec xs) where
  mempty  = H.replicate (Proxy @Monoid) mempty
  {-# INLINE mempty  #-}

instance (ArityC Semigroup xs) => Semigroup (HVec xs) where
  (<>) = H.zipWith   (Proxy @Semigroup) (<>)
  {-# INLINE (<>) #-}

instance (ArityC NFData xs) => NFData (HVec xs) where
  rnf = H.rnf
  {-# INLINE rnf #-}
