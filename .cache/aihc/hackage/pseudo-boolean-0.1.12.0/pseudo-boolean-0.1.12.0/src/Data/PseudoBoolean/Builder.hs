{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.PseudoBoolean.Builder
-- Copyright   :  (c) Masahiro Sakai 2011-2015
-- License     :  BSD-style
-- 
-- Maintainer  :  masahiro.sakai@gmail.com
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Data.PseudoBoolean.Builder
  (
  -- * Builder for String-like Monoid
    opbBuilder
  , wboBuilder

  -- * String generation
  , toOPBString
  , toWBOString
  ) where

import qualified Prelude
import Prelude hiding (sum)
import qualified Data.DList as DList
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Data.List (sortBy)
import Data.Maybe (maybeToList)
import Data.Monoid hiding (Sum (..))
import Data.Ord
import Data.String
import Math.NumberTheory.Logarithms (integerLog2)
import Text.Printf
import Data.PseudoBoolean.Types

-- | A builder which renders a OPB format in any String-like 'Monoid'.
opbBuilder :: (Monoid a, IsString a) => Formula -> a
opbBuilder opb = (size <> part1 <> part2)
  where
    nv = pbNumVars opb
    nc = pbNumConstraints opb
    neq = length [() | (_lhs, Eq, _rhs) <- pbConstraints opb]
    intsize = maximum $ 0 :
      [ if tmp == 0 then 0 else 1 + integerLog2 tmp
      | (ts, d) <- [(ts, 0) | ts <- maybeToList (pbObjectiveFunction opb)] ++ [(lhs,rhs) | (lhs,_op,rhs) <- pbConstraints opb]
      , let tmp = abs d + Prelude.sum [abs c | (c,_) <- ts]
      ]
    p = pbProducts opb
    np = Set.size p
    sp = Prelude.sum [IntSet.size tm | tm <- Set.toList p]
    size = fromString (printf "* #variable= %d #constraint= %d #equal= %d intsize= %d" nv nc neq intsize)
         <> (if np >= 1 then fromString (printf " #product= %d sizeproduct= %d" np sp) else mempty)
         <> fromString "\n"
    part1 = 
      case pbObjectiveFunction opb of
        Nothing -> mempty
        Just o -> fromString "min: " <> showSum o <> fromString ";\n"
    part2 = mconcat $ map showConstraint (pbConstraints opb)

-- | A builder which renders a WBO format in any String-like 'Monoid'.
wboBuilder :: (Monoid a, IsString a) => SoftFormula -> a
wboBuilder wbo = size <> part1 <> part2
  where
    nv = wboNumVars wbo
    nc = wboNumConstraints wbo
    neq = length [() | (_, (_lhs, Eq, _rhs)) <- wboConstraints wbo]
    p = wboProducts wbo
    np = Set.size p
    sp = Prelude.sum [IntSet.size tm | tm <- Set.toList p]
    mincost =
      case [c | (Just c, _) <- wboConstraints wbo] of
        [] -> 1 -- this should not happen
        cs -> minimum cs
    maxcost = maximum $ 0 : [c | (Just c, _) <- wboConstraints wbo]
    sumcost = Prelude.sum [c | (Just c, _) <- wboConstraints wbo]
    intsize = maximum $ 0 :
      [ if tmp == 0 then 0 else 1 + integerLog2 tmp
      | (cs, d) <- ([sumcost], 0) : [(map fst lhs, rhs) | (_,(lhs,_op,rhs)) <- wboConstraints wbo]
      , let tmp = abs d + Prelude.sum [abs c | c <- cs]
      ]
    size = fromString (printf "* #variable= %d #constraint= %d #equal= %d intsize= %d" nv nc neq intsize)
         <> (if np >= 1 then fromString (printf " #product= %d sizeproduct= %d" np sp) else mempty)
         <> fromString (printf " #soft= %d" (wboNumSoft wbo))
         <> fromString (printf " mincost= %d maxcost= %d sumcost= %d" mincost maxcost sumcost)
         <> fromString "\n"
    part1 = 
      case wboTopCost wbo of
        Nothing -> fromString "soft: ;\n"
        Just t -> fromString "soft: " <> fromString (show t) <> fromString ";\n"
    part2 = mconcat $ map showSoftConstraint (wboConstraints wbo)

showSum :: (Monoid a, IsString a) => Sum -> a
showSum = mconcat . map showWeightedTerm

showWeightedTerm :: (Monoid a, IsString a) => WeightedTerm -> a
showWeightedTerm (c, lits) = foldr (\f g -> f <> fromString " " <> g) mempty (x:xs)
  where
    x = if c >= 0 then fromString "+" <> fromString (show c) else fromString (show c)
    xs = map showLit $ sortBy (comparing abs) lits

showLit :: (Monoid a, IsString a) => Lit -> a
showLit lit = if lit > 0 then v else fromString "~" <> v
  where
    v = fromString "x" <> fromString (show (abs lit))

showConstraint :: (Monoid a, IsString a) => Constraint -> a
showConstraint (lhs, op, rhs) =
  showSum lhs <> f op <>  fromString " " <> fromString (show rhs) <> fromString ";\n"
  where
    f Eq = fromString "="
    f Ge = fromString ">="

showSoftConstraint :: (Monoid a, IsString a) => SoftConstraint -> a
showSoftConstraint (cost, constr) =
  case cost of
    Nothing -> showConstraint constr
    Just c -> fromString "[" <> fromString (show c) <> fromString "] " <> showConstraint constr


-- | Generate a OPB format string containing pseudo boolean problem.
toOPBString :: Formula -> String
toOPBString opb = DList.apply (opbBuilder opb) ""

-- | Generate a WBO format string containing weighted boolean optimization problem.
toWBOString :: SoftFormula -> String
toWBOString wbo = DList.apply (wboBuilder wbo) ""
