{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.PseudoBoolean.Types
-- Copyright   :  (c) Masahiro Sakai 2011-2015
-- License     :  BSD-style
-- 
-- Maintainer  :  masahiro.sakai@gmail.com
-- Portability :  non-portable (BangPatterns, DeriveDataTypeable, DeriveGeneric)
-- 
-- References:
--
-- * Input/Output Format and Solver Requirements for the Competitions of
--   Pseudo-Boolean Solvers
--   <http://www.cril.univ-artois.fr/PB11/format.pdf>
--
-----------------------------------------------------------------------------

module Data.PseudoBoolean.Types
  (
  -- * Abstract Syntax
    Formula (..)
  , Constraint
  , Op (..)
  , SoftFormula (..)
  , SoftConstraint
  , Sum
  , WeightedTerm
  , Term
  , Lit
  , Var

  -- * Internal utilities
  , pbComputeNumVars
  , pbProducts
  , wboComputeNumVars
  , wboProducts
  , wboNumSoft
  ) where

import GHC.Generics (Generic)
import Control.Monad
import Control.DeepSeq
import Data.Data
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Hashable
import Data.Maybe

-- | Pair of /objective function/ and a list of constraints.
data Formula
  = Formula
  { pbObjectiveFunction :: Maybe Sum
  , pbConstraints :: [Constraint]
  , pbNumVars :: !Int
  , pbNumConstraints :: !Int
  }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

instance NFData Formula
instance Hashable Formula

-- | Lhs, relational operator and rhs.
type Constraint = (Sum, Op, Integer)

-- | Relational operators
data Op
  = Ge -- ^ /greater than or equal/
  | Eq -- ^ /equal/
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data, Generic)

instance NFData Op
instance Hashable Op

-- | A pair of /top cost/ and a list of soft constraints.
data SoftFormula
  = SoftFormula
  { wboTopCost :: Maybe Integer
  , wboConstraints :: [SoftConstraint]
  , wboNumVars :: !Int
  , wboNumConstraints :: !Int
  }
  deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

instance NFData SoftFormula
instance Hashable SoftFormula

-- | A pair of weight and constraint.
type SoftConstraint = (Maybe Integer, Constraint)

-- | Sum of 'WeightedTerm'
type Sum = [WeightedTerm]

-- | Coefficient and 'Term'
type WeightedTerm = (Integer, Term)

-- | List of variables interpreted as products
type Term = [Lit]

-- | Positive (resp. negative) literals are represented as positive (resp. negative) integers.
type Lit = Int

-- | Variable are repserented as positive integers.
type Var = Int

-- | Utility function for computing number of variables in given objective function and constraints.
pbComputeNumVars :: Maybe Sum -> [Constraint] -> Int
pbComputeNumVars obj cs = maximum (0 : vs)
  where
    vs = do
      s <- maybeToList obj ++ [s | (s,_,_) <- cs]
      (_, tm) <- s
      lit <- tm
      return $ abs lit

-- | Utility function for computing number of variables in given objective function and constraints.
wboComputeNumVars :: [SoftConstraint] -> Int
wboComputeNumVars cs = maximum (0 : vs)
  where
    vs = do
      s <- [s | (_, (s,_,_)) <- cs]
      (_, tm) <- s
      lit <- tm
      return $ abs lit

pbProducts :: Formula -> Set IntSet
pbProducts formula = Set.fromList $ do  
  s <- maybeToList (pbObjectiveFunction formula) ++ [s | (s,_,_) <- pbConstraints formula]
  (_, tm)  <- s
  let tm2 = IntSet.fromList tm
  guard $ IntSet.size tm2 > 1
  return tm2

wboProducts :: SoftFormula -> Set IntSet
wboProducts softformula = Set.fromList $ do
  (_,(s,_,_)) <- wboConstraints softformula
  (_, tm) <- s
  let tm2 = IntSet.fromList tm
  guard $ IntSet.size tm2 > 1
  return tm2

wboNumSoft :: SoftFormula -> Int
wboNumSoft softformula = length [() | (Just _, _) <- wboConstraints softformula]
