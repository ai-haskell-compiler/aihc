{-# LANGUAGE RebindableSyntax #-}
{- |
Tools for creating a data base of physical units
and for extracting data from it
-}

module Number.Physical.UnitDatabase where

import qualified Number.Physical.Unit as Unit
import qualified Algebra.Field as Field

import Algebra.NormedSpace.Sum(norm)

import Data.Maybe.HT (toMaybe)
import Data.List (findIndices, partition, unfoldr, find, minimumBy)

import NumericPrelude.Base
import NumericPrelude.Numeric

type T i a = [UnitSet i a]

-- since field names are reused for accessor functions
-- they are global identifiers and can't be reused
data InitUnitSet i a =
  InitUnitSet {
    initUnit        :: Unit.T i,
    initIndependent :: Bool,
    initScales      :: [InitScale a]
  }

data InitScale a =
  InitScale {
    initSymbol  :: String,
    initMag     :: a,
    initIsUnit  :: Bool,
    initDefault :: Bool
  }

-- | An entry for a unit and there scalings.
data UnitSet i a =
  UnitSet {
    unit        :: Unit.T i,
    independent :: Bool,
    defScaleIx  :: Int,
    reciprocal  :: Bool,  {-^ If True the symbols must be preceded with a '/'.
                              Though it sounds like an attribute of Scale
                              it must be the same for all scales and we need it
                              to sort positive powered unitsets to the front
                              of the list of unit components. -}
    scales      :: [Scale a]
  }
  deriving Show

-- | A common scaling for a unit.
data Scale a =
  Scale {
    symbol     :: String,
    magnitude  :: a
  }
  deriving Show


-- extract the element from a list containing exact one element
-- fails if there are zero or more than one element
-- 'head' fails only if there are zero elements
extractOne :: [a] -> a
extractOne (x:[]) = x
extractOne _      = error "There must be exactly one default unit in the data base."

initScale   :: String -> a -> Bool -> Bool -> InitScale a
initScale   = InitScale
initUnitSet :: Unit.T i -> Bool -> [InitScale a] -> InitUnitSet i a
initUnitSet = InitUnitSet

createScale :: InitScale a -> Scale a
createScale (InitScale sym mg _ _) = (Scale sym mg)

createUnitSet :: InitUnitSet i a -> UnitSet i a
createUnitSet (InitUnitSet u ind scs) = (UnitSet u ind
    (extractOne (findIndices initDefault scs))
    False
    (map createScale scs)
  )

{- Filter out all scales intended for showing.
   If there is none return Nothing. -}
showableUnit :: InitUnitSet i a -> Maybe (InitUnitSet i a)
showableUnit (InitUnitSet u ind scs) =
   let sscs = filter initIsUnit scs
   in  toMaybe (not (null sscs)) (InitUnitSet u ind sscs)


{- | Raise all scales of a unit and the unit itself to the n-th power -}
powerOfUnitSet :: (Ord i, Field.C a) => Int -> UnitSet i a -> UnitSet i a
powerOfUnitSet n us@UnitSet { unit = u, reciprocal = rec, scales = scs } =
   us { unit = n *> u,
        reciprocal = rec == (n>0),  -- flip sign
        scales = map (powerOfScale n) scs }


powerOfScale :: Field.C a => Int -> Scale a -> Scale a
powerOfScale n Scale { symbol = sym, magnitude = mag } =
   if n>0
   then Scale { symbol = sym ++ showExp   n,  magnitude = ringPower  n mag }
   else Scale { symbol = sym ++ showExp (-n), magnitude = fieldPower n mag }

showExp :: Int -> String
showExp 1    = ""
--showExp 2    = "²"
--showExp 3    = "³"
showExp expo = "^" ++ show expo


{- | Reorder the unit components in a way
     that the units with positive exponents lead the list. -}
positiveToFront :: [UnitSet i a] -> [UnitSet i a]
positiveToFront = uncurry (++) . partition (not . reciprocal)

-- | Decompose a complex unit into common ones
decompose :: (Ord i, Field.C a) => Unit.T i -> T i a -> [UnitSet i a]
decompose u db =
   case (findIndep u db) of
      Just us -> [us]
      Nothing ->
        unfoldr (\urem ->
          toMaybe (not (Unit.isScalar urem))
                  (let us = findClosest urem db
                   in  (us, subtract (unit us) urem))
        ) u

findIndep :: (Eq i) => Unit.T i -> T i a -> Maybe (UnitSet i a)
findIndep u = find (\UnitSet {unit=un} -> u==un) . filter independent

findClosest :: (Ord i, Field.C a) => Unit.T i -> T i a -> UnitSet i a
findClosest u =
   fst . minimumBy (\(_,dist0) (_,dist1) -> compare dist0 dist1) .
            evalDist u . filter (not.independent)

evalDist :: (Ord i, Field.C a)
   => Unit.T i
   -> T i a
   -> [(UnitSet i a, Int)] {-^ (UnitSet,distance)   the UnitSet may contain powered units -}
evalDist target = map (\us->
    let (expo,dist)=findBestExp target (unit us)
    in  (powerOfUnitSet expo us, dist)
  )

findBestExp :: (Ord i) => Unit.T i -> Unit.T i -> (Int, Int)
findBestExp target u =
  let bestl = findMinExp (distances target (listMultiples (subtract u) (-1)))
      bestr = findMinExp (distances target (listMultiples ((+)      u)   1 ))
  in  if distLE bestl bestr
      then bestl
      else bestr

{-|
  Find the exponent that lead to minimal distance
  Since the list is infinite 'maximum' will fail
  but the sequence is convex
  and thus we can abort when the distance stop falling
-}
findMinExp :: [(Int, Int)] -> (Int, Int)
findMinExp (x0:x1:rest) =
  if distLE x0 x1
  then x0
  else findMinExp (x1:rest)
findMinExp _ = error "List of unit approximations with respect to the unit exponent must be infinite."

distLE :: (Int, Int) -> (Int, Int) -> Bool
distLE (_,dist0) (_,dist1) = dist0<=dist1
--distLE (exp0,dist0) (exp1,dist1) = (dist0<dist1) || (dist0==dist1 && (abs exp0) <= (abs exp1))

-- [(exponent,unit)] -> [(exponent,distance)]
distances :: (Ord i) => Unit.T i -> [(Int, Unit.T i)] -> [(Int, Int)]
distances targetu = map (\(expo,u)->(expo, norm (subtract u targetu)))

listMultiples :: (Unit.T i -> Unit.T i) -> Int -> [(Int, Unit.T i)]
listMultiples f dir = iterate (\(expo,u)->(expo+dir,f u)) (0,Unit.scalar)
