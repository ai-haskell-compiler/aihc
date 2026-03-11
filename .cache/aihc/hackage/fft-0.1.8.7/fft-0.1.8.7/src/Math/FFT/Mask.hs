module Math.FFT.Mask (
   Mask, Complex, Real,
   IODim(..), TSpec,
   keep, trans, realTrans,
   mask2, mask3, unnest3, (<<+>), (<+>>),
   outermostReal,
   example,
   ) where

import qualified Data.Array.IArray as IArray
import qualified Data.Ix as Ix
import Data.Array.IArray (IArray)
import Data.Ix (Ix)

import qualified Control.Monad.Trans.State as MS
import Control.Applicative (liftA2)
import Data.Functor.Reverse (Reverse(Reverse), getReverse)

import qualified Data.Complex as Cpl
import Prelude hiding (Real)


data IODim = IODim {ioDimNum, ioDimStride :: Int}
type TSpec typ = (([IODim], typ), [IODim])
newtype Mask typ ix = Mask ((ix,ix) -> Reverse (MS.State Int) (TSpec typ))


data Real = Real IODim
data Complex = Complex


dim :: (Ix ix) => (ix,ix) -> MS.State Int IODim
dim bnds = do
   stride <- MS.get
   let num = Ix.rangeSize bnds
   MS.put (num*stride)
   return $ IODim num stride

makeMask :: (Ix ix) => (IODim -> TSpec typ) -> Mask typ ix
makeMask f =
   Mask $ \bnds -> Reverse $ fmap f $ dim bnds

keep :: (Ix ix) => Mask Complex ix
keep = makeMask $ \d -> (([], Complex), [d])

trans :: Mask Complex Int
trans = makeMask $ \d -> (([d], Complex), [])

realTrans :: Mask Real Int
realTrans = makeMask $ \d -> (([], Real d), [])


combineTSpec :: (typ0 -> typ1 -> typ2) -> TSpec typ0 -> TSpec typ1 -> TSpec typ2
combineTSpec f ((dims0, halfDim0), hdims0) ((dims1, halfDim1), hdims1) =
   ((dims0++dims1, f halfDim0 halfDim1), hdims0++hdims1)


infixl 6 <<+>, <+>>

(<<+>) ::
   Mask typ ix0 -> Mask Complex ix1 -> Mask typ (ix0,ix1)
(<<+>) (Mask op0) (Mask op1) =
   Mask $ \((l0,l1), (r0,r1)) ->
      liftA2 (combineTSpec const) (op0 (l0,r0)) (op1 (l1,r1))

(<+>>) ::
   Mask Complex ix0 -> Mask typ ix1 -> Mask typ (ix0,ix1)
(<+>>) (Mask op0) (Mask op1) =
   Mask $ \((l0,l1), (r0,r1)) ->
      liftA2 (combineTSpec (flip const)) (op0 (l0,r0)) (op1 (l1,r1))

unnest3 :: Mask typ ((ix0, ix1), ix2) -> Mask typ (ix0,ix1,ix2)
unnest3 (Mask op) =
   Mask $ \((l0,l1,l2), (r0,r1,r2)) -> op (((l0,l1),l2), ((r0,r1),r2))


mask2 :: Mask typ ix0 -> Mask Complex ix1 -> Mask typ (ix0,ix1)
mask2 = (<<+>)

mask3 ::
   Mask typ ix0 -> Mask Complex ix1 -> Mask Complex ix2 ->
   Mask typ (ix0,ix1,ix2)
mask3 m0 m1 m2 = unnest3 $ m0 <<+> m1 <<+> m2

{- |
Turn the outermost transformed dimension into one for real transformation.
It is undefined if there is no transformed dimension.
-}
outermostReal :: Mask Complex ix -> Mask Real ix
outermostReal (Mask op) =
   Mask $ \bnds ->
      fmap
         (\((d:dims, Complex), hdims) ->
            ((dims, Real d), hdims)) $
      op bnds


dftRCN ::
   (Ix i, IArray array a) =>
   Mask Real i -> array i a -> array i (Cpl.Complex a)
dftRCN (Mask op) arr =
   undefined $ MS.evalState (getReverse $ op (IArray.bounds arr)) 1


example ::
   (IArray array a) =>
   array (Char, Int, Integer) a ->
   array (Char, Int, Integer) (Cpl.Complex a)
example = dftRCN (unnest3 $ keep <+>> realTrans <<+> keep)
