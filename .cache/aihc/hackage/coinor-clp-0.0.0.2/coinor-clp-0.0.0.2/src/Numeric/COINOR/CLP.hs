{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Numeric.COINOR.CLP (
   simplex,
   LP.Direction(..),
   PlusMinusOne(..),
   Term(..), (LP..*),
   Constraints,
   LP.free, (LP.<=.), (LP.>=.), (LP.==.), (LP.>=<.),
   Method, Priv.dual, Priv.primal,
   Priv.initialSolve, Priv.initialDualSolve, Priv.initialPrimalSolve,
   Priv.initialBarrierSolve, Priv.initialBarrierNoCrossSolve,
   FailureType(..),
   Result,
   ) where

import qualified Numeric.COINOR.CLP.FFI as FFI
import qualified Numeric.COINOR.CLP.Debug as Debug
import qualified Numeric.COINOR.CLP.Private as Priv
import Numeric.COINOR.CLP.Private
         (Method(runMethod), Result, FailureType(..),
          runContT, withBuffer, false,
          storeBounds, prepareRowBoundsArrays, prepareColumnBoundsArrays,
          storeConstraints, prepareConstraints,
          setOptimizationDirection, examineStatus)

import qualified Numeric.LinearProgramming.Common as LP
import Numeric.LinearProgramming.Common
         (Inequality(Inequality), Bounds,
          Term(Term), Constraints, Direction(..), Objective)

import qualified Data.Array.Comfort.Storable as Array
import qualified Data.Array.Comfort.Shape as Shape
import qualified Data.List.HT as ListHT

import qualified Control.Monad.Trans.Cont as MC
import Control.Monad.IO.Class (liftIO)
import Control.Exception (bracket)

import System.IO.Unsafe (unsafePerformIO)

import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.Types (CDouble)


{- $setup
>>> import qualified Numeric.COINOR.CLP as LP
>>> import qualified Numeric.LinearProgramming.Test as TestLP
>>> import Numeric.COINOR.CLP
>>>    (PlusMinusOne(..), (.*), (==.), (<=.), (>=.), (>=<.))
>>>
>>> import qualified Data.Array.Comfort.Storable as Array
>>> import qualified Data.Array.Comfort.Shape as Shape
>>>
>>> import Data.Either.HT (mapRight)
>>> import Data.Tuple.HT (mapSnd)
>>>
>>> import qualified Test.QuickCheck as QC
>>> import Test.QuickCheck ((===), (.&&.), (.||.))
>>>
>>> type X = Shape.Element
>>> type PairShape = Shape.NestedTuple Shape.TupleIndex (X,X)
>>> type TripletShape = Shape.NestedTuple Shape.TupleIndex (X,X,X)
>>>
>>> pairShape :: PairShape
>>> pairShape = Shape.static
>>>
>>> tripletShape :: TripletShape
>>> tripletShape = Shape.static
>>>
>>> approxReal :: (Ord a, Num a) => a -> a -> a -> Bool
>>> approxReal tol x y = abs (x-y) <= tol
>>>
>>> genMethod :: QC.Gen (String, LP.Method)
>>> genMethod = QC.elements $
>>>    ("dual", LP.dual) :
>>>    ("primal", LP.primal) :
>>>    ("initialSolve", LP.initialSolve) :
>>>    ("initialDualSolve", LP.initialDualSolve) :
>>>    ("initialPrimalSolve", LP.initialPrimalSolve) :
>>>    ("initialBarrierSolve", LP.initialBarrierSolve) :
>>>    -- let tests fail
>>>    -- ("initialBarrierNoCrossSolve", LP.initialBarrierNoCrossSolve) :
>>>    []
>>>
>>> forAllMethod ::
>>>    (QC.Testable prop) => (LP.Method -> prop) -> QC.Property
>>> forAllMethod prop = QC.forAllShow genMethod fst (prop . snd)
-}



data PlusMinusOne = MinusOne | PlusOne deriving (Eq, Show)


class Coefficient a where
   loadProblem ::
      (Shape.Indexed sh, Shape.Index sh ~ ix) =>
      sh ->
      Constraints a ix ->
      Ptr FFI.Simplex ->
      Ptr CDouble -> Ptr CDouble ->
      Ptr CDouble ->
      Ptr CDouble -> Ptr CDouble ->
      MC.ContT () IO ()

instance Coefficient Double where
   loadProblem shape constrs lp collbPtr colubPtr objPtr rowlbPtr rowubPtr = do
      let (coefficients, indices, rowStarts) = prepareConstraints shape constrs
      (coefficientsPtr, indexPtr, startPtr)
         <- storeConstraints (coefficients, indices, rowStarts)
      let createMatrix =
            FFI.newCoinPackedMatrix
               false
               (fromIntegral $ Shape.size shape)
               (fromIntegral $ length constrs)
               (fromIntegral $ Shape.size $ Array.shape coefficients)
               coefficientsPtr
               indexPtr
               startPtr
               nullPtr
      matrix <- MC.ContT $ bracket createMatrix FFI.deleteCoinPackedMatrix
      liftIO $
         FFI.loadProblemFromCoinMatrix lp matrix
            collbPtr colubPtr
            objPtr
            rowlbPtr rowubPtr
            nullPtr

instance Coefficient PlusMinusOne where
   loadProblem shape constrs lp collbPtr colubPtr objPtr rowlbPtr rowubPtr = do
      let shapeOffset = Shape.offset shape
      let coefficients =
            map
               (\(Inequality terms _bnd) ->
                  ListHT.partition (\(Term c _) -> c == PlusOne) terms)
               constrs
      indexPtr <-
         withBuffer $ Array.vectorFromList $
         concatMap
            (\(positive,negative) ->
               map fromIntegral $
                  map (\(Term _ ix) -> shapeOffset ix) positive
                  ++
                  map (\(Term _ ix) -> shapeOffset ix) negative)
            coefficients
      let rowStarts =
            scanl (+) 0 $
            map (\(Inequality terms _bnd) -> length terms) constrs
      startPositivePtr <-
         withBuffer $ Array.vectorFromList $ map fromIntegral rowStarts
      startNegativePtr <-
         withBuffer $ Array.vectorFromList $
         zipWith (\k (pos,_neg) -> fromIntegral $ k + length pos)
            rowStarts coefficients
      let createMatrix =
            FFI.newPlusMinusOneMatrix
               (fromIntegral $ length constrs)
               (fromIntegral $ Shape.size shape)
               (toEnum $ fromEnum False)
               indexPtr startPositivePtr startNegativePtr
      matrix <- MC.ContT $ bracket createMatrix FFI.deletePlusMinusOneMatrix
      liftIO $
         FFI.loadProblemFromMatrix lp matrix
            collbPtr colubPtr
            objPtr
            rowlbPtr rowubPtr
            nullPtr


{- |
>>> :{
   case Shape.indexTupleFromShape tripletShape of
      (x,y,z) ->
         mapSnd Array.toTuple <$>
         LP.simplex LP.dual []
            [[2.*x, 1.*y] <=. 10, [1.*y, (5::Double).*z] <=. 20]
            (LP.Maximize, Array.fromTuple (4,-3,2)
               :: Array.Array TripletShape Double)
:}
Right (28.0,(5.0,0.0,4.0))

>>> :{
   case Shape.indexTupleFromShape tripletShape of
      (x,y,z) ->
         mapSnd Array.toTuple <$>
         LP.simplex LP.primal [y >=<. (-12,12)]
            [[1.*x, (-1).*y] <=. 10, [(-1).*y, (1::Double).*z] <=. 20]
            (LP.Maximize, Array.fromTuple (4,-3,2)
               :: Array.Array TripletShape Double)
:}
Right (116.0,(22.0,12.0,32.0))

>>> :{
   case Shape.indexTupleFromShape tripletShape of
      (x,y,z) ->
         mapSnd Array.toTuple <$>
         LP.simplex LP.primal [y >=<. (-12,12)]
            [[PlusOne .* x, MinusOne .* y] <=. 10,
             [MinusOne .* y, PlusOne .* z] <=. 20]
            (LP.Maximize, Array.fromTuple (4,-3,2)
               :: Array.Array TripletShape Double)
:}
Right (116.0,(22.0,12.0,32.0))

>>> :{
   case Shape.indexTupleFromShape tripletShape of
      (x,y,z) ->
         mapSnd Array.toTuple <$>
         LP.simplex LP.primal [y >=<. (-12,12)]
            [[1.*x, 1.*y] <=. 10, [1.*y, (-1::Double).*z] >=. 20]
            (LP.Maximize, Array.fromTuple (4,3,2)
               :: Array.Array TripletShape Double)
:}
Left PrimalInfeasible

>>> :{
   case Shape.indexTupleFromShape tripletShape of
      (x,y,z) ->
         mapSnd Array.toTuple <$>
         LP.simplex LP.primal [y >=<. (-12,12)]
            [[1.*x, 1.*y] <=. 10, [1.*y, (1::Double).*z] >=. 20]
            (LP.Maximize, Array.fromTuple (4,3,2)
               :: Array.Array TripletShape Double)
:}
Left DualInfeasible

prop> :{
   forAllMethod $ \method (QC.Positive posWeight) (QC.Positive negWeight) target ->
   case Shape.indexTupleFromShape pairShape of
      (pos,neg) ->
         case mapSnd Array.toTuple <$>
               LP.simplex method []
                  [[1.*pos, (-1::Double).*neg] ==. target]
                  (LP.Minimize, Array.fromTuple (posWeight,negWeight)
                     :: Array.Array PairShape Double) of
            Left _ -> QC.property False
            Right (absol,(posResult,negResult)) ->
               QC.property (absol>=0)
               .&&.
               (posResult === 0 .||. negResult === 0)
:}
prop> :{
   forAllMethod $ \method target ->
   case Shape.indexTupleFromShape pairShape of
      (pos,neg) ->
         case mapSnd Array.toTuple <$>
               LP.simplex method []
                  [[1.*pos, (-1::Double).*neg] ==. target]
                  (LP.Minimize, Array.fromTuple (1,1)
                     :: Array.Array PairShape Double) of
            Left _ -> QC.property False
            Right (absol,(posResult,negResult)) ->
               QC.counterexample (show(absol,(posResult,negResult))) $
               QC.property (approxReal 0.001 absol (abs target))
               .&&.
               (posResult === 0 .||. negResult === 0)
:}

prop> :{
   forAllMethod $ \method ->
   TestLP.forAllOrigin $ \origin ->
   TestLP.forAllProblem origin $ \bounds constrs ->
   QC.forAll (TestLP.genObjective origin) $ \(dir,obj) ->
   case LP.simplex method bounds constrs (dir,obj) of
      Left _ -> False
      Right _ -> True
:}
prop> :{
   forAllMethod $ \method ->
   TestLP.forAllOrigin $ \origin ->
   TestLP.forAllProblem origin $ \bounds constrs ->
   QC.forAll (TestLP.genObjective origin) $ \(dir,obj) ->
   case LP.simplex method bounds constrs (dir,obj) of
      Left _ -> QC.property False
      Right (_,sol) -> TestLP.checkFeasibility 0.1 bounds constrs sol
:}
prop> :{
   forAllMethod $ \method ->
   TestLP.forAllOrigin $ \origin ->
   TestLP.forAllProblem origin $ \bounds constrs ->
   QC.forAll (TestLP.genObjective origin) $ \(dir,obj) ->
   case LP.simplex method bounds constrs (dir,obj) of
      Left _ -> QC.property False
      Right (_,sol) ->
         QC.forAll (QC.choose (0,1)) $ \lambda ->
         TestLP.checkFeasibility 0.1 bounds constrs $
         TestLP.affineCombination lambda sol (Array.map fromIntegral origin)
:}
prop> :{
   forAllMethod $ \method ->
   TestLP.forAllOrigin $ \origin ->
   TestLP.forAllProblem origin $ \bounds constrs ->
   QC.forAll (TestLP.genObjective origin) $ \(dir,obj) ->
   case LP.simplex method bounds constrs (dir,obj) of
      Left _ -> QC.property False
      Right (opt,sol) ->
         QC.forAll (QC.choose (0,1)) $ \lambda ->
            let val = TestLP.scalarProduct obj $
                        TestLP.affineCombination lambda sol (Array.map fromIntegral origin)
            in case dir of
                  LP.Minimize -> opt-0.01 <= val
                  LP.Maximize -> opt+0.01 >= val
:}
prop> :{
   forAllMethod $ \method ->
   TestLP.forAllOrigin $ \origin ->
   TestLP.forAllBoundedProblem origin $ \bounds constrs ->
   QC.forAll (TestLP.genObjective origin) $ \dirObjA ->
   QC.forAll (TestLP.genObjective origin) $ \dirObjB ->
   let solA = LP.simplex method bounds constrs dirObjA in
   let solB = LP.simplex method bounds constrs dirObjB in
   QC.counterexample (show (mapRight fst solA, mapRight fst solB)) $
   case (solA, solB) of
      (Right _, Right _) -> True
      (Left _, Left _) -> True
      _ -> False
:}
prop> :{
   forAllMethod $ \method ->
   TestLP.forAllOrigin $ \origin ->
   TestLP.forAllProblem origin $ \bounds constrs ->
   QC.forAll (TestLP.genObjective origin) $ \(_dir,obj) ->
   case (LP.simplex method bounds constrs (LP.Minimize,obj),
         LP.simplex method bounds constrs (LP.Maximize,obj)) of
      (Right (optMin,_), Right (optMax,_)) ->
         QC.counterexample (show (optMin, optMax)) $ optMin <= optMax + 0.01
      _ -> QC.property False
:}
prop> :{
   forAllMethod $ \method ->
   TestLP.forAllOrigin $ \origin ->
   TestLP.forAllProblem origin $ \bounds allConstrs ->
   QC.forAll (QC.sublistOf allConstrs) $ \someConstrs ->
   QC.forAll (TestLP.genObjective origin) $ \(dir,obj) ->
   case (LP.simplex method bounds allConstrs (dir,obj),
         LP.simplex method bounds someConstrs (dir,obj)) of
      (Right (optAll,_), Right (optSome,_)) ->
         QC.counterexample (show (optAll, optSome)) $
         case dir of
            LP.Minimize -> optAll >= optSome-0.01
            LP.Maximize -> optAll <= optSome+0.01
      _ -> QC.property False
:}
prop> :{
   forAllMethod $ \methodA ->
   forAllMethod $ \methodB ->
   TestLP.forAllOrigin $ \origin ->
   TestLP.forAllProblem origin $ \bounds constrs ->
   QC.forAll (TestLP.genObjective origin) $ \dirObj ->
   case (LP.simplex methodA bounds constrs dirObj,
         LP.simplex methodB bounds constrs dirObj) of
      (Right (optA,_), Right (optB,_)) ->
         QC.counterexample (show (optA, optB)) $
         approxReal 0.01 optA optB
      _ -> QC.property False
:}
-}
simplex ::
   (Coefficient a, Shape.Indexed sh, Shape.Index sh ~ ix) =>
   Method -> Bounds ix -> Constraints a ix ->
   (Direction, Objective sh) -> Result sh
simplex method bounds constrs (dir,obj) =
   unsafePerformIO $
   bracket FFI.newModel FFI.deleteModel $ \lp -> do

   Debug.initLog lp
   let shape = Array.shape obj
   runContT $ do
      objPtr <- withBuffer $ Array.map realToFrac obj
      (collbPtr,colubPtr) <-
         storeBounds $ prepareColumnBoundsArrays shape bounds
      (rowlbPtr,rowubPtr) <- storeBounds $ prepareRowBoundsArrays constrs
      loadProblem shape constrs lp collbPtr colubPtr objPtr rowlbPtr rowubPtr
   setOptimizationDirection lp dir
   runMethod method lp
   examineStatus shape lp
