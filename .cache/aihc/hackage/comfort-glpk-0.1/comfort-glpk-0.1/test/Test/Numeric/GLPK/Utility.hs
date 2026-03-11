{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Test.Numeric.GLPK.Utility where

import qualified Numeric.GLPK as LP
import Numeric.LinearProgramming.Test (approxReal)

import qualified Test.QuickCheck as QC

import qualified Data.Array.Comfort.Shape as Shape
import qualified Data.NonEmpty as NonEmpty
import qualified Data.List.HT as ListHT
import Data.Array.Comfort.Storable (Array)
import Data.Tuple.HT (double)
import Data.Traversable (Traversable, traverse)
import Data.Foldable (Foldable, traverse_)

import qualified Control.Monad.Trans.State as MS

import Foreign.Storable (Storable)


approxSuccession ::
   (Shape.C sh, Show sh, Show a, Ord a, Num a, Storable a) =>
   a ->
   Either LP.FailureType
      (NonEmpty.T [] (LP.SolutionType, (a, Array sh a))) ->
   Either LP.FailureType
      (NonEmpty.T [] (LP.SolutionType, (a, Array sh a))) ->
   QC.Property
approxSuccession tol x y =
   QC.counterexample (show x) $
   QC.counterexample (show y) $
   case (x,y) of
      (Left sx, Left sy) -> sx==sy
      (Right (NonEmpty.Cons xh xs), Right (NonEmpty.Cons yh ys)) ->
         let equalSol (solX, (optX, _)) (solY, (optY, _)) =
               solX == solY && approxReal tol optX optY
         in equalSol xh yh  &&  ListHT.equalWith equalSol xs ys
      _ -> False


traverse_Lag ::
   (Foldable t, Monad m) =>
   b -> (b -> a -> m b) -> t a -> m ()
traverse_Lag b0 f =
   flip MS.evalStateT b0 .
   traverse_ (\a -> MS.StateT $ \b -> fmap double $ f b a)

traverseLag ::
   (Traversable t, Monad m) =>
   b -> (b -> a -> m b) -> t a -> m (t b)
traverseLag b0 f =
   flip MS.evalStateT b0 .
   traverse (\a -> MS.StateT $ \b -> fmap double $ f b a)
