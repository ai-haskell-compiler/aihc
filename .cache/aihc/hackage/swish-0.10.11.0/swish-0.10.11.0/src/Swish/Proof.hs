{-# LANGUAGE CPP #-}

#if (__GLASGOW_HASKELL__ >= 802)
{-# LANGUAGE DerivingStrategies #-}
#endif

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Proof
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--                 2011, 2016, 2022, 2024 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  CPP, DerivingStrategies
--
--  This module defines a framework for constructing proofs
--  over some expression form.  It is intended to be used
--  with RDF graphs, but the structures aim to be quite
--  generic with respect to the expression forms allowed.
--
--  It does not define any proof-finding strategy.
--
--------------------------------------------------------------------------------

module Swish.Proof
    ( Proof(..), Step(..)
    , checkProof, explainProof, checkStep, showProof, showsProof, showsFormula )
where

import qualified Data.Set as S

import Data.Foldable(Foldable(..))
import Data.List (union, intersect, intercalate)
import Data.Maybe (catMaybes, isNothing)
import Data.String.ShowLines (ShowLines(..))

-- Avoid messages added in GHC 9.10 about foldl' import from Data.List
-- being redundant.
import Prelude hiding (Foldable(..))

import Swish.Rule (Expression(..), Formula(..), Rule(..))
import Swish.Rule (showsFormula, showsFormulae)
import Swish.Ruleset (Ruleset(..))

------------------------------------------------------------
--  Proof framework
------------------------------------------------------------

-- |Step in proof chain
--
--  The display name for a proof step comes from the display name of its
--  consequence formula.
data Step ex = Step
    { stepRule :: Rule ex           -- ^ Inference rule used
    , stepAnt  :: [Formula ex]      -- ^ Antecedents of inference rule
    , stepCon  :: Formula ex        -- ^ Named consequence of inference rule
    } deriving
#if (__GLASGOW_HASKELL__ >= 802)
    stock
#endif
    Show

-- |Proof is a structure that presents a chain of rule applications
--  that yield a result expression from a given expression
data Proof ex = Proof
    { proofContext :: [Ruleset ex]  -- ^ Proof context:  list of rulesets,
                                    --   each of which provides a number of
                                    --   axioms and rules.
    , proofInput   :: Formula ex    -- ^ Given expression
    , proofResult  :: Formula ex    -- ^ Result expression
    , proofChain   :: [Step ex]     -- ^ Chain of inference rule applications
                                    --   progressing from input to result
    }

-- |Return a list of axioms from all the rulesets in a proof
proofAxioms :: Proof a -> [Formula a]
proofAxioms = concatMap rsAxioms . proofContext

-- |Return a list of rules from all the rulesets in a proof
proofRules :: Proof a -> [Rule a]
proofRules = concatMap rsRules . proofContext

-- |Return list of axioms actually referenced by a proof
proofAxiomsUsed :: Proof ex -> [Formula ex]
proofAxiomsUsed proof = foldl' union [] $ map stepAxioms (proofChain proof)
    where
        stepAxioms st = stepAnt st `intersect` proofAxioms proof

-- |Check consistency of given proof.
--  The supplied rules and axioms are assumed to be correct.
checkProof :: 
    (Expression ex, Ord ex) 
    => Proof ex -> Bool
checkProof pr =
    checkProof1 (proofRules pr) initExpr (proofChain pr) goalExpr
    where
        initExpr = formExpr (proofInput pr) : map formExpr (proofAxioms pr)
        goalExpr = formExpr $ proofResult pr

checkProof1 :: 
    (Expression ex, Ord ex) 
    => [Rule ex] -> [ex] -> [Step ex] -> ex -> Bool
checkProof1 _     prev []       res = res `elem` prev
checkProof1 rules prev (st:steps) res =
    checkStep rules prev st &&
    checkProof1 rules (formExpr (stepCon st):prev) steps res

-- | A proof step is valid if rule is in list of rules
--  and the antecedents are sufficient to obtain the conclusion
--  and the antecedents are in the list of formulae already proven.
--
--  Note:  this function depends on the ruleName of any rule being
--  unique among all rules.  In particular the name of the step rule
--  being in correspondence with the name of one of the indicated
--  valid rules of inference.
checkStep :: 
  Ord ex
  => [Rule ex]   -- ^ rules
  -> [ex]        -- ^ antecedants
  -> Step ex     -- ^ the step to validate
  -> Bool        -- ^ @True@ if the step is valid

checkStep rules prev step = isNothing $ explainStep rules prev step

{-

Is the following an optimisation of the above?

checkStep rules prev step =
    -- Rule name is one of supplied rules, and
    (ruleName srul `elem` map ruleName rules) &&
    -- Antecedent expressions are all previously accepted expressions
    (sant `subset` prev)   &&
    -- Inference rule yields concequence from antecendents
    checkInference srul sant scon
    where
        --  Rule from proof step:
        srul = stepRule step
        --  Antecedent expressions from proof step:
        sant = map formExpr $ stepAnt step
        --  Consequentent expression from proof step:
        scon = formExpr $ stepCon step
-}


{-
    (formExpr (stepCon step) `elem` sfwd)
    -- (or $ map (`subset` sant) sbwd)
    where
        --  Rule from proof step:
        srul = stepRule step
        --  Antecedent expressions from proof step:
        sant = map formExpr $ stepAnt step
        --  Forward chaining from antecedents of proof step
        scon = map formExpr $ stepCon step
        --  Forward chaining from antecedents of proof step

        sfwd = fwdApply srul sant
        --  Backward chaining from consequent of proof step
        --  (Does not work because of introduction of existentials)
        sbwd = bwdApply srul (formExpr $ stepCon step)
-}

-- |Check proof. If there is an error then return information
-- about the failing step.
explainProof ::
    (Expression ex, Ord ex) => Proof ex -> Maybe String
explainProof pr =
    explainProof1 (proofRules pr) initExpr (proofChain pr) goalExpr
    where
        initExpr = formExpr (proofInput pr) : map formExpr (proofAxioms pr)
        goalExpr = formExpr $ proofResult pr

explainProof1 ::
    (Expression ex, Ord ex) 
    => [Rule ex] -> [ex] -> [Step ex] -> ex -> Maybe String
explainProof1 _     prev []       res   =
    if res `elem` prev then Nothing else Just "Result not demonstrated"
explainProof1 rules prev (st:steps) res =
    case explainStep rules prev st  of
        Nothing -> explainProof1 rules (formExpr (stepCon st):prev) steps res
        Just ex -> Just ("Invalid step: " ++ show (formName $ stepCon st) ++ ": " ++ ex)

-- | A proof step is valid if rule is in list of rules
--  and the antecedents are sufficient to obtain the conclusion
--  and the antecedents are in the list of formulae already proven.
--
--  Note:  this function depends on the ruleName of any rule being
--  unique among all rules.  In particular the name of the step rule
--  being in correspondence with the name of one of the indicated
--  valid rules of inference.
--
explainStep :: 
  Ord ex
  => [Rule ex]  -- ^ rules
  -> [ex]       -- ^ previous
  -> Step ex    -- ^ step
  -> Maybe String -- ^ @Nothing@ if step is okay, otherwise a string indicating the error
explainStep rules prev step =
        if null errors then Nothing else Just $ intercalate ", " errors
    where
        --  Rule from proof step:
        srul = stepRule step
        --  Antecedent expressions from proof step:
        sant = map formExpr $ stepAnt step
        --  Consequentent expression from proof step:
        scon = formExpr $ stepCon step
        --  Tests for step to be valid
        errors = catMaybes
            -- Rule name is one of supplied rules, and
            [ require (ruleName srul `elem` map ruleName rules)
                      ("rule " ++ show (ruleName srul) ++ " not present")
            -- Antecedent expressions are all previously accepted expressions
            , require (S.fromList sant `S.isSubsetOf` S.fromList prev) -- (sant `subset` prev)
                      "antecedent not axiom or previous result"
            -- Inference rule yields consequence from antecedents
            , require (checkInference srul sant scon)
                      "rule does not deduce consequence from antecedents"
            ]
        require b s = if b then Nothing else Just s

-- |Create a displayable form of a proof, returned as a `ShowS` value.
--
--  This function is intended to allow the calling function some control
--  of multiline displays by providing:
--
--  (1) the first line of the proof is not preceded by any text, so
--      it may be appended to some preceding text on the same line,
--
--  (2) the supplied newline string is used to separate lines of the
--      formatted text, and may include any desired indentation, and
--
--  (3) no newline is output following the final line of text.
showsProof :: 
  (ShowLines ex) 
  => String    -- ^ newline string
  -> Proof ex 
  -> ShowS
showsProof newline proof =
    if null axioms then shProof else shAxioms . shProof
    where
        axioms = proofAxiomsUsed proof
        shAxioms =
            showString    ("Axioms:" ++ newline) .
            showsFormulae newline (proofAxiomsUsed proof) newline
        shProof =
            showString    ("Input:" ++ newline) .
            showsFormula  newline (proofInput  proof) .
            showString    (newline ++ "Proof:" ++ newline) .
            showsSteps    newline (proofChain  proof)

-- |Returns a simple string representation of a proof.
showProof :: 
  (ShowLines ex) 
  => String    -- ^ newline string
  -> Proof ex 
  -> String
showProof newline proof = showsProof newline proof ""

-- |Create a displayable form of a list of labelled proof steps
showsSteps :: (ShowLines ex) => String -> [Step ex] -> ShowS
showsSteps _       []     = id
showsSteps newline [s]    = showsStep  newline s
showsSteps newline (s:ss) = showsStep  newline s .
                            showString newline .
                            showsSteps newline ss

-- |Create a displayable form of a labelled proof step.
showsStep :: (ShowLines ex) => String -> Step ex -> ShowS
showsStep newline s = showsFormula newline (stepCon s) .
                      showString newline .
                      showString ("  (by [" ++ rulename ++ "] from " ++ antnames ++ ")")
    where
        rulename = show . ruleName $ stepRule s
        antnames = showNames $ map (show . formName) (stepAnt s)

-- |Return a string containing a list of names.
showNames :: [String] -> String
showNames []      = "<nothing>"
showNames [n]     = showName n
showNames [n1,n2] = showName n1 ++ " and " ++ showName n2
showNames (n1:ns) = showName n1 ++ ", " ++ showNames ns

-- |Return a string representing a single name.
showName :: String -> String
showName n = "[" ++ n ++ "]"

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2016, 2022 Douglas Burke
--  All rights reserved.
--
--  This file is part of Swish.
--
--  Swish is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  Swish is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Swish; if not, write to:
--    The Free Software Foundation, Inc.,
--    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--
--------------------------------------------------------------------------------
