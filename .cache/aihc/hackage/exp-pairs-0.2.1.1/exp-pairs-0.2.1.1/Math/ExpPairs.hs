{-|
Module      : Math.ExpPairs
Copyright   : (c) Andrew Lelechenko, 2014-2020
License     : GPL-3
Maintainer  : andrew.lelechenko@gmail.com

Linear programming over exponent pairs

Package implements an algorithm to minimize the maximum of a list of rational objective functions over the set of exponent pairs. See full description in
A. V. Lelechenko, Linear programming over exponent pairs. Acta Univ. Sapientiae, Inform. 5, No. 2, 271-287 (2013).
<http://www.acta.sapientia.ro/acta-info/C5-2/info52-7.pdf>

A set of useful applications can be found in
"Math.ExpPairs.Ivic", "Math.ExpPairs.Kratzel" and "Math.ExpPairs.MenzerNowak".
-}

{-# LANGUAGE PatternSynonyms #-}

module Math.ExpPairs
  ( optimize
  , OptimizeResult
  , optimalValue
  , optimalPair
  , optimalPath
  , simulateOptimize
  , simulateOptimize'
  , LinearForm
  , RationalForm (..)
  , IneqType
  , Constraint
  , InitPair
  , Path
  , RatioInf (..)
  , RationalInf
  , pattern K
  , pattern L
  , pattern M
  , (>.), (>=.), (<.), (<=.)
  , scaleLF
  ) where

import Control.Arrow hiding ((<+>))
import Data.Function (on)
import Data.Ord      (comparing)
import Data.List     (minimumBy)
import Data.Ratio
import Prettyprinter hiding ((<>))
import qualified Prettyprinter as PP
import Text.Printf

import Math.ExpPairs.LinearForm
import Math.ExpPairs.Process
import Math.ExpPairs.Pair
import Math.ExpPairs.RatioInf

-- | For a given @c@ returns linear form @c * k@
pattern K :: (Eq a, Num a) => a -> LinearForm a
pattern K n = LinearForm n 0 0

-- | For a given @c@ returns linear form @c * l@
pattern L :: (Eq a, Num a) => a -> LinearForm a
pattern L n = LinearForm 0 n 0

-- | For a given @c@ returns linear form @c * m@
pattern M :: (Eq a, Num a) => a -> LinearForm a
pattern M n = LinearForm 0 0 n

-- | Build a constraint, which states that the value of the first linear form is greater than the value of the second one.
(>.) :: Num t => LinearForm t -> LinearForm t -> Constraint t
lf1 >. lf2  = Constraint (lf1 - lf2) Strict
infix 5 >.

-- | Build a constraint, which states that the value of the first linear form is greater or equal to the value of the second one.
(>=.) :: Num t => LinearForm t -> LinearForm t -> Constraint t
lf1 >=. lf2 = Constraint (lf1 - lf2) NonStrict
infix 5 >=.

-- | Build a constraint, which states that the value of the first linear form is less than the value of the second one.
(<.) :: Num t => LinearForm t -> LinearForm t -> Constraint t
lf1 <. lf2  = Constraint (lf2 - lf1) Strict
infix 5 <.

-- | Build a constraint, which states that the value of the first linear form is less or equal to the value of the second one.
(<=.) :: Num t => LinearForm t -> LinearForm t -> Constraint t
lf1 <=. lf2 = Constraint (lf2 - lf1) NonStrict
infix 5 <=.

evalFunctional :: [InitPair] -> [InitPair] -> [RationalForm Rational] -> [Constraint Rational] -> Path -> (RationalInf, InitPair)
evalFunctional corners interiors rfs cons path = case rs of
  [] -> (InfPlus, error $ "evalFunctional: cannot find any exponential pair, which satisfies constraints " ++ show (map pretty cons))
  _  -> minimumBy (comparing fst) rs
  where
    applyPath  = map (evalPath path . initPairToProjValue &&& id)
    corners'   = applyPath corners
    interiors' = applyPath interiors

    predicate (p, _) = all (checkConstraint p) cons
    qs
      | all predicate corners' = corners'
      | any predicate corners' = filter predicate interiors'
      | otherwise              = []

    rs = map (first $ \p -> maximum (map (evalRF p) rfs)) qs

checkMConstraints :: Path -> [Constraint Rational] -> Bool
checkMConstraints path = all (\con -> any (\p -> checkConstraint (evalPath path p) con) triangleT) where
  triangleT = [(0, 1, 1), (0, 1, 2), (1, 1, 2)]

-- |Container for the result of optimization.
data OptimizeResult = OptimizeResult {
  -- | The minimal value of objective function.
  optimalValue :: RationalInf,
  -- | The initial exponent pair, on which minimal value was achieved.
  optimalPair  :: InitPair,
  -- | The sequence of processes, after which minimal value was
  -- achieved.
  optimalPath  :: Path
  }
  deriving (Show)

instance Pretty OptimizeResult where
  pretty (OptimizeResult r' ip p) = pretty1 r' <> PP.line <>
    (parens (pretty (k%m) PP.<> comma PP.<> pretty (l%m)) <+> equals <+> pretty p <> softline <> pretty ip)
    where
      pretty1 r@(Finite rr) = pretty (printf "%.6f" (fromRational rr :: Double) :: String) <+> equals <+> pretty r
      pretty1 r = pretty r

      (k, l, m) = evalPath p $ initPairToProjValue ip


instance Eq OptimizeResult where
  (==) = (==) `on` optimalValue

instance Ord OptimizeResult where
  compare = compare `on` optimalValue

-- |Wrap 'Rational' into 'OptimizeResult'.
simulateOptimize :: Rational -> OptimizeResult
simulateOptimize r = OptimizeResult (Finite r) Corput01 mempty

-- |Wrap 'RationalInf' into 'OptimizeResult'.
simulateOptimize' :: RationalInf -> OptimizeResult
simulateOptimize' r = OptimizeResult r Corput01 mempty

-- |This function takes a list of rational forms and a list
-- of constraints and returns an exponent pair, which satisfies
-- all constraints and minimizes the maximum of all rational forms.
optimize :: [RationalForm Rational] -> [Constraint Rational] -> OptimizeResult
optimize rfs cons = optimize' rfs cons (OptimizeResult r0 ip0 mempty) where
  (r0, ip0) = evalFunctional [Corput01, Corput12] [Corput01, Corput12] rfs cons mempty

optimize' :: [RationalForm Rational] -> [Constraint Rational] -> OptimizeResult -> OptimizeResult
optimize' rfs cons ret@(OptimizeResult r _ path)
  | lengthPath path > 100 = ret
  | otherwise = retBA where
    ret0@(OptimizeResult r0 ip0 _) = if r0' < r then OptimizeResult r0' ip0' path else ret where
      (r0', ip0') = evalFunctional corners interiors rfs cons path
      corners = [Mix 1 0, Mix 0 1, Mix 0 0]
      interiors = initPairs

    cons0 = if r0==InfPlus then cons else cons ++ map (consBuilder r0) rfs

    retA@(OptimizeResult r1 ip1 _) = if checkMConstraints patha cons0 && r1' < r0 then branchA else ret0 where
      patha  = path <> aPath
      branchA@(OptimizeResult r1' _ _) = optimize' rfs cons (OptimizeResult r0 ip0 patha)

    cons1 = if r1==r0  then cons0 else cons ++ map (consBuilder r1) rfs

    retBA = if checkMConstraints pathba cons1 && r2' < r1 then branchB else retA where
      pathba  = path <> baPath
      branchB@(OptimizeResult r2' _ _) = optimize' rfs cons (OptimizeResult r1 ip1 pathba)

    consBuilder rr (num :/: den) = substituteLF (num, den, 1) (L (toRational rr) - K 1) >. 0

