{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Numeric.LinearProgramming.Test (
   Element,
   forAllOrigin,
   forAllProblem,
   forAllBoundedProblem,
   genObjective,
   forAllObjectives,
   successiveObjectives,
   approxReal,
   approx,
   checkFeasibility,
   affineCombination,
   scalarProduct,
   ) where

import qualified Numeric.LinearProgramming.Common as LP
import Numeric.LinearProgramming.Common ((<=.), (>=.), (.*))

import qualified Test.QuickCheck as QC
import Test.QuickCheck ((.&&.))
import System.Random (Random)

import qualified Data.Array.Comfort.Boxed as BoxedArray
import qualified Data.Array.Comfort.Storable as Array
import qualified Data.Array.Comfort.Shape as Shape
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Ix as Ix
import Data.Array.Comfort.Storable (Array, (!))
import Data.Traversable (sequenceA, for)
import Data.Tuple.HT (mapSnd)
import Data.Maybe (fromMaybe)
import Data.Int (Int64)

import Control.Applicative (liftA2, (<$>))

import Text.Printf (PrintfArg, printf)

import Foreign.Storable (Storable)



type Term = LP.Term Double
type Constraints ix = LP.Constraints Double ix


{- |
Generate constraints in the form of a polyhedron
which contains warrantedly the zero vector.
That is, there is an admissible solution.
In order to assert that the polyhedron is closed,
we bound all variables by a hypercube.
-}
genProblem ::
   (Shape.Indexed sh, Shape.Index sh ~ ix, Element a) =>
   Array sh a -> QC.Gen (LP.Bounds ix, Constraints ix)
genProblem origin =
   liftA2 (,)
      (genBounds origin)
      (do
         numConstraints <- QC.choose (1,20)
         QC.vectorOf numConstraints $ do
            ixs <- QC.sublistOf $ Shape.indices $ Array.shape origin
            terms <- for ixs $ \ix -> do
               coeff <- QC.choose (-10,10)
               return (coeff, ix)
            let offset = scalarProductTerms terms origin
            let deviation = 25
            LP.Inequality (map (uncurry ((.*) . doubleFromElement)) terms)
               <$>
               QC.oneof (
                  (do bound <- QC.choose (offset-deviation, offset+deviation)
                      return $
                         if bound > offset
                            then LP.LessEqual    $ doubleFromElement bound
                            else LP.GreaterEqual $ doubleFromElement bound) :
                  (liftA2 LP.Between
                     (doubleFromElement <$>
                        QC.choose (offset-deviation, offset))
                     (doubleFromElement <$>
                        QC.choose (offset, offset+deviation))) :
                  []))

scalarProductTerms ::
   (Shape.Indexed sh, Shape.Index sh ~ ix, Storable a, Num a) =>
   [(a,ix)] -> Array sh a -> a
scalarProductTerms terms origin =
   sum $ map (\(coeff, ix) -> coeff * origin!ix) terms

{- |
Generates bounded, but maybe infeasible problems.
-}
genBoundedProblem ::
   (Shape.Indexed sh, Shape.Index sh ~ ix, Element a) =>
   Array sh a -> QC.Gen (LP.Bounds ix, Constraints ix)
genBoundedProblem origin =
   liftA2 (,)
      (genBounds origin)
      (do
         numConstraints <- QC.choose (1,20)
         QC.vectorOf numConstraints $ do
            ixs <- QC.sublistOf $ Shape.indices $ Array.shape origin
            terms <- for ixs $ \ix -> do
               coeff <- QC.choose (-10, 10)
               return (coeff, ix)
            let doubleFromElem :: (Element a) => f a -> a -> Double
                doubleFromElem _ = doubleFromElement
            let choose = doubleFromElem origin <$> QC.choose (-100, 100)
            LP.Inequality (map (uncurry ((.*) . doubleFromElem origin)) terms)
               <$>
               QC.oneof (
                  (LP.LessEqual    <$> choose) :
                  (LP.GreaterEqual <$> choose) :
                  (liftA2
                     (\x y -> LP.Between (min x y) (max x y))
                     choose choose) :
                  []))

genBounds ::
   (Shape.Indexed sh, Element a) =>
   Array sh a -> QC.Gen [LP.Inequality (Shape.Index sh)]
genBounds origin =
   for (Array.toAssociations origin) $ \(ix,x) ->
      LP.Inequality ix <$>
      liftA2 LP.Between
         (doubleFromElement . (x+) <$> QC.choose (-100,-50))
         (doubleFromElement . (x+) <$> QC.choose (50,100))

genVarShape :: QC.Gen (Shape.Range Char)
genVarShape = Shape.Range 'a' <$> QC.choose ('a','j')

genOrigin :: QC.Gen (Array (Shape.Range Char) Int64)
genOrigin = genVector =<< genVarShape

_genOrigin :: QC.Gen (Array (Shape.Range Char) Double)
_genOrigin = genVector =<< genVarShape


_shrinkVarShape :: Shape.Range Char -> [Shape.Range Char]
_shrinkVarShape (Shape.Range from to) =
   if from<to then [Shape.Range from (pred to)] else []

shrinkOrigin ::
   (Storable a) => Array (Shape.Range Char) a -> [Array (Shape.Range Char) a]
shrinkOrigin vec =
   case Array.shape vec of
      Shape.Range from to ->
         if from<to
            then [Array.sample (Shape.Range from (pred to)) (vec!)]
            else []


forAllOrigin ::
   (QC.Testable prop) =>
   (Array (Shape.Range Char) Int64 -> prop) -> QC.Property
forAllOrigin = QC.forAllShrink genOrigin shrinkOrigin


class (Storable a, Random a, Num a, Ord a) => Element a where
   doubleFromElement :: a -> Double

instance Element Double where
   doubleFromElement = id

instance Element Int64 where
   doubleFromElement = fromIntegral

genObjective ::
   (Shape.Indexed sh, Shape.Index sh ~ ix, Element a) =>
   Array sh a -> QC.Gen (LP.Direction, LP.Objective sh)
genObjective origin =
   liftA2 (,) QC.arbitraryBoundedEnum
      (fmap (Array.map doubleFromElement . flip asTypeOf origin) $
       genVector $ Array.shape origin)

genVector :: (Shape.Indexed sh, Element a) => sh -> QC.Gen (Array sh a)
genVector shape =
   fmap Array.fromBoxed $ sequenceA $
   BoxedArray.fromAssociations (QC.choose (-10,10)) shape []
--    BoxedArray.constant shape (QC.choose (-10,10))

shrinkProblem ::
   (LP.Bounds ix, Constraints ix) ->
   [(LP.Bounds ix, Constraints ix)]
shrinkProblem (bounds, constraints) =
   map (\shrinked -> (bounds, shrinked)) $
   filter (not . null) $ QC.shrinkList (const []) constraints

forAllProblem ::
   (Shape.Indexed sh, Shape.Index sh ~ ix, Show ix) =>
   (QC.Testable prop, Element a) =>
   Array sh a -> (LP.Bounds ix -> Constraints ix -> prop) -> QC.Property
forAllProblem origin =
   QC.forAllShrink (genProblem origin) shrinkProblem . uncurry

forAllBoundedProblem ::
   (Shape.Indexed sh, Shape.Index sh ~ ix, Show ix) =>
   (QC.Testable prop, Element a) =>
   Array sh a -> (LP.Bounds ix -> Constraints ix -> prop) -> QC.Property
forAllBoundedProblem origin =
   QC.forAllShrink (genBoundedProblem origin) shrinkProblem . uncurry


genObjectives ::
   (Shape.Indexed sh, Shape.Index sh ~ ix, Element a) =>
   Array sh a -> QC.Gen (NonEmpty.T [] (LP.Direction, [Term ix]))
genObjectives origin = do
   let shape = Array.shape origin
   let stageRange :: (Int,Int)
       stageRange = (0,3)
   stages <- for (Shape.indices shape) $ \ix -> (,) ix <$> QC.choose stageRange
   let varSets =
         fromMaybe (error "there should be at least one stage") $
         NonEmpty.fetch $
         filter (not . null) $
         map (\k -> map fst $ filter ((k==) . snd) stages) $
         Ix.range stageRange
   let asTypeOfElement :: a -> f a -> a
       asTypeOfElement = const
   for varSets $ \varSet ->
      liftA2 (,)
         QC.arbitraryBoundedEnum
         (for varSet $ \ix ->
            (.*ix) . doubleFromElement
               <$> QC.choose (-10, 10 `asTypeOfElement` origin))

shrinkObjectives ::
   NonEmpty.T [] (LP.Direction, [Term ix]) ->
   [NonEmpty.T [] (LP.Direction, [Term ix])]
shrinkObjectives (NonEmpty.Cons obj objs) =
   map (NonEmpty.Cons obj) $
   QC.shrinkList
      (\(dir,terms) ->
         map ((,) dir) $ filter (not . null) $
         QC.shrinkList (const []) terms)
      objs

forAllObjectives ::
   (Shape.Indexed sh, Shape.Index sh ~ ix, Show ix) =>
   (QC.Testable prop, Element a) =>
   Array sh a ->
   (NonEmpty.T [] (LP.Direction, [Term (Shape.Index sh)]) -> prop) ->
   QC.Property
forAllObjectives origin =
   QC.forAllShrink (genObjectives origin) shrinkObjectives

constraintsFromSolution ::
   Double -> (LP.Direction, x) -> Double -> [LP.Inequality x]
constraintsFromSolution tol (dir,obj) opt =
   case dir of
      LP.Minimize -> [obj <=. opt + tol]
      LP.Maximize -> [obj >=. opt - tol]

successiveObjectives ::
   (Shape.Indexed sh, Shape.Index sh ~ ix) =>
   Array sh a -> Double ->
   NonEmpty.T [] (LP.Direction, [Term ix]) ->
   ((LP.Direction, LP.Objective sh),
    [(Double -> Constraints ix, (LP.Direction, LP.Objective sh))])
successiveObjectives origin tol xs =
   let shape = Array.shape origin in
   (mapSnd (LP.objectiveFromTerms shape) $ NonEmpty.head xs,
    NonEmpty.mapAdjacent
      (\(dir,obj) y1 ->
         (constraintsFromSolution tol (dir,obj),
          mapSnd (LP.objectiveFromTerms shape) y1))
      xs)


approxReal :: (Ord a, Num a) => a -> a -> a -> Bool
approxReal tol x y = abs (x-y) <= tol

approx :: (PrintfArg a, Ord a, Num a) => String -> a -> a -> a -> QC.Property
approx name tol x y =
   QC.counterexample (printf "%s: %f - %f" name x y) (approxReal tol x y)



checkBound :: Double -> LP.Bound -> Double -> QC.Property
checkBound tol bound x =
   QC.counterexample (show (x, bound)) $
   case bound of
      LP.LessEqual up -> x<=up+tol
      LP.GreaterEqual lo -> x>=lo-tol
      LP.Between lo up -> lo-tol<=x && x<=up+tol
      LP.Equal y -> approxReal tol x y
      LP.Free -> True

checkBounds ::
   (Shape.Indexed sh, Shape.Index sh ~ ix) =>
   Double -> LP.Bounds ix -> Array sh Double -> QC.Property
checkBounds tol bounds sol =
   QC.conjoin $ map (\(ix,bnd) -> checkBound tol bnd (sol!ix)) $
   BoxedArray.toAssociations $
   BoxedArray.fromAssociations (LP.GreaterEqual 0) (Array.shape sol) $
   map (\(LP.Inequality ix bnd) -> (ix,bnd)) bounds

checkContraint ::
   (Shape.Indexed sh, Shape.Index sh ~ ix) =>
   Double -> LP.Inequality [LP.Term Double ix] -> Array sh Double -> QC.Property
checkContraint tol (LP.Inequality terms bnd) sol =
   checkBound tol bnd $
   scalarProductTerms (map (\(LP.Term c ix) -> (c,ix)) terms) sol

checkFeasibility ::
   (Shape.Indexed sh, Shape.Index sh ~ ix) =>
   Double -> LP.Bounds ix -> Constraints ix -> Array sh Double -> QC.Property
checkFeasibility tol bounds constrs sol =
   checkBounds tol bounds sol
   .&&.
   QC.conjoin (map (flip (checkContraint tol) sol) constrs)


affineCombination ::
   (Shape.C sh, Eq sh, Storable a, Num a) =>
   a -> Array sh a -> Array sh a -> Array sh a
affineCombination c x y =
   Array.zipWith (+) (Array.map ((1-c)*) x) (Array.map (c*) y)

scalarProduct ::
   (Shape.C sh, Eq sh, Storable a, Num a) =>
   Array sh a -> Array sh a -> a
scalarProduct x y = Array.sum $ Array.zipWith (*) x y
