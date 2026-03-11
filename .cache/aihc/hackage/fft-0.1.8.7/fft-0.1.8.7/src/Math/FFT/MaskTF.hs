{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
module Math.FFT.MaskTF (
   Mask, Complex, Real,
   IODim(..), TSpec,
   keep, trans, realTrans,
   mask2, mask3, (<++>),
   example,
   ) where

import qualified Data.Array.IArray as IArray
import qualified Data.Ix as Ix
import Data.Array.IArray (IArray)
import Data.Ix (Ix)

import qualified Control.Monad.Trans.State as MS
import Control.Applicative (Applicative, liftA2)
import Data.Functor.Reverse (Reverse(Reverse), getReverse)

import qualified Data.Complex as Cpl
import Prelude hiding (Real)


data IODim = IODim {ioDimNum, ioDimStride :: Int}
type TSpec typ = (([IODim], Box typ), [IODim])
newtype Mask typ ix = Mask ((ix,ix) -> Reverse (MS.State Int) (TSpec typ))


data Real = Real IODim
data Complex = Complex
data Invalid = Invalid

type family Combine typ0 typ1
type instance Combine Real Real = Invalid
type instance Combine Real Complex = Real
type instance Combine Complex Real = Real
type instance Combine Complex Complex = Complex
type instance Combine Invalid Real = Invalid
type instance Combine Invalid Complex = Invalid
type instance Combine Real Invalid = Invalid
type instance Combine Complex Invalid = Invalid
type instance Combine Invalid Invalid = Invalid

class Type typ where
   switch :: f Complex -> f Real -> f Invalid -> f typ

instance Type Complex where switch x _ _ = x
instance Type Real    where switch _ x _ = x
instance Type Invalid where switch _ _ x = x

newtype
   CombineType0 typ1 typ0 =
      CombineType0 {
         runCombineType0 :: Box typ0 -> CombineType1 typ0 typ1
      }

newtype
   CombineType1 typ0 typ1 =
      CombineType1 {
         runCombineType1 :: Box typ1 -> Box (Combine typ0 typ1)
      }

data Box typ = (Type typ) => Box typ


keepFirst ::
   (Combine typ0 typ1 ~ typ0) =>
   Box typ0 -> CombineType1 typ0 typ1
keepFirst x = CombineType1 (const x)

keepSecond ::
   (Combine typ0 typ1 ~ typ1) =>
   Box typ0 -> CombineType1 typ0 typ1
keepSecond _x = CombineType1 id

resultInvalid ::
   (Combine typ0 typ1 ~ Invalid) =>
   Box typ0 -> CombineType1 typ0 typ1
resultInvalid _x = CombineType1 (const $ Box Invalid)

combineType :: Box typ0 -> Box typ1 -> Box (Combine typ0 typ1)
combineType typ0@(Box _) typ1@(Box _) =
   flip runCombineType1 typ1 $
   flip runCombineType0 typ0 $
   switch
      (CombineType0 $ \x ->
         switch (keepSecond x) (keepSecond x) (keepSecond x))
      (CombineType0 $ \x ->
         switch (keepFirst x) (resultInvalid x) (keepSecond x))
      (CombineType0 $ \x ->
         switch (keepFirst x) (keepFirst x) (keepFirst x))


dim :: (Ix ix) => (ix,ix) -> MS.State Int IODim
dim bnds = do
   stride <- MS.get
   let num = Ix.rangeSize bnds
   MS.put (num*stride)
   return $ IODim num stride

makeMask ::
   (Ix ix, Type typ) =>
   (IODim -> (([IODim], typ), [IODim])) -> Mask typ ix
makeMask f =
   Mask $ \bnds ->
      Reverse $
      fmap
         (\((transComplex, transReal), keepDim) ->
            ((transComplex, Box transReal), keepDim)) $
      fmap f $ dim bnds

keep :: (Ix ix) => Mask Complex ix
keep = makeMask $ \d -> (([], Complex), [d])

trans :: Mask Complex Int
trans = makeMask $ \d -> (([d], Complex), [])

realTrans :: Mask Real Int
realTrans = makeMask $ \d -> (([], Real d), [])


combineTSpec :: TSpec typ0 -> TSpec typ1 -> TSpec (Combine typ0 typ1)
combineTSpec ((dims0, halfDim0), hdims0) ((dims1, halfDim1), hdims1) =
   ((dims0++dims1, combineType halfDim0 halfDim1), hdims0++hdims1)


infixl 6 <++>

(<++>) ::
   (Applicative m) =>
   m (TSpec typ0) -> m (TSpec typ1) -> m (TSpec (Combine typ0 typ1))
(<++>) = liftA2 combineTSpec

mask2 :: Mask typ0 ix0 -> Mask typ1 ix1 -> Mask (Combine typ0 typ1) (ix0,ix1)
mask2 (Mask op0) (Mask op1) =
   Mask $ \((l0,l1), (r0,r1)) -> op0 (l0,r0) <++> op1 (l1,r1)

mask3 ::
   Mask typ0 ix0 -> Mask typ1 ix1 -> Mask typ2 ix2 ->
   Mask (Combine (Combine typ0 typ1) typ2) (ix0,ix1,ix2)
mask3 (Mask op0) (Mask op1) (Mask op2) =
   Mask $ \((l0,l1,l2), (r0,r1,r2)) ->
            op0 (l0,r0) <++> op1 (l1,r1) <++> op2 (l2,r2)

dftRCN ::
   (Ix i, IArray array a) =>
   Mask Real i -> array i a -> array i (Cpl.Complex a)
dftRCN (Mask op) arr =
   undefined $ MS.evalState (getReverse $ op (IArray.bounds arr)) 1


example ::
   (IArray array a) =>
   array (Char, Int, Integer) a ->
   array (Char, Int, Integer) (Cpl.Complex a)
example = dftRCN (mask3 keep realTrans keep)
