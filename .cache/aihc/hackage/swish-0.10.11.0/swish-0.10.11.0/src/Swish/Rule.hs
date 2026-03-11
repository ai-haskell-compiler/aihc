{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

#if (__GLASGOW_HASKELL__ >= 802)
{-# LANGUAGE DerivingStrategies #-}
#endif

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Rule
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012, 2022 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  CPP, DerivingStrategies, OverloadedStrings
--
--  This module defines a framework for defining inference rules
--  over some expression form.  It is intended to be used with
--  RDF graphs, but the structures aim to be quite generic with
--  respect to the expression forms allowed.
--
--------------------------------------------------------------------------------

module Swish.Rule
       ( Expression(..), Formula(..), Rule(..), RuleMap
       , nullScope, nullSN, nullFormula, nullRule
       , fwdCheckInference, bwdCheckInference
       , showsFormula, showsFormulae, showsWidth
       )
       where

import Swish.Namespace (Namespace, ScopedName)
import Swish.Namespace (makeNamespace, makeNSScopedName)
import Swish.QName (LName)

import Data.Maybe (fromJust)
import Data.String.ShowLines (ShowLines(..))

import Network.URI (URI, parseURI)

import qualified Data.Map as M

------------------------------------------------------------
--  Expressions
------------------------------------------------------------

-- |Expression is a type class for values over which proofs
--  may be constructed.
class (Eq ex) => Expression ex where
    -- |Is expression true in all interpretations?
    --  If so, then its truth is assumed without justification.
    isValid :: ex -> Bool

------------------------------------------------------------
--  Formula:  a named expression
------------------------------------------------------------

-- | A Formula is a named expression.
data Formula ex = Formula
    { formName :: ScopedName        -- ^ Name used for formula in proof chain
    , formExpr :: ex                -- ^ Named formula value
    } deriving
#if (__GLASGOW_HASKELL__ >= 802)
    stock
#endif
    Show

-- |Define equality of formulae as equality of formula names
instance Eq (Formula ex) where
    f1 == f2 = formName f1 == formName f2

-- |Define ordering of formulae based on formula names
instance Ord (Formula ex) where
    f1 <= f2 = formName f1 <= formName f2

-- | The namespace @http:\/\/id.ninebynine.org\/2003\/Ruleset\/null@ with the prefix @null:@.
nullScope :: Namespace
nullScope = makeNamespace (Just "null") nullScopeURI

-- | Create a scoped name with the null namespace.
nullSN :: 
  LName -- ^ local name.
  -> ScopedName
nullSN = makeNSScopedName nullScope

tU :: String -> URI
tU = fromJust . parseURI

nullScopeURI :: URI
nullScopeURI = tU "http://id.ninebynine.org/2003/Ruleset/null"

-- | The null formula.
nullFormula :: Formula ex
nullFormula = Formula
    { formName = makeNSScopedName nullScope "nullFormula"
    , formExpr = error "Null formula"
    }

-- testf1 = Formula "f1" ('f',1)
-- testf2 = Formula "f2" ('f',2)

-- |Return a displayable form of a list of labelled formulae
showsFormulae :: 
  (ShowLines ex) 
  => String        -- ^ newline
  -> [Formula ex]  -- ^ the formulae to show
  -> String        -- ^ text to be placed after the formulae
  -> ShowS
showsFormulae _       []     _     = id
showsFormulae newline [f]    after = showsFormula  newline f .
                                     showString    after
showsFormulae newline (f:fs) after = showsFormula  newline f .
                                     showString    newline .
                                     showsFormulae newline fs after

-- |Create a displayable form of a labelled formula
showsFormula :: 
  (ShowLines ex) 
  => String      -- ^ newline
  -> Formula ex  -- ^ formula
  -> ShowS
showsFormula newline f =
    showsWidth 16 ("[" ++ show (formName f) ++ "] ") .
    showls (newline ++ replicate 16 ' ') (formExpr f)

------------------------------------------------------------
--  Rule
------------------------------------------------------------

-- |Rule is a data type for inference rules that can be used
--  to construct a step in a proof.
data Rule ex = Rule
    {
      -- |Name of rule, for use when displaying a proof
      ruleName :: ScopedName,
      
      -- |Forward application of a rule, takes a list of
      --  expressions and returns a list (possibly empty)
      --  of forward applications of the rule to combinations
      --  of the antecedent expressions.
      --  Note that all of the results returned can be assumed to
      --  be (simultaneously) true, given the antecedents provided.
      fwdApply :: [ex] -> [ex],
      
      -- |Backward application of a rule, takes an expression
      --  and returns a list of alternative antecedents, each of
      --  which is a list of expressions that jointly yield the
      --  given consequence through application of the inference
      --  rule.  An empty list is returned if no antecedents
      --  will allow the consequence to be inferred.
      bwdApply :: ex -> [[ex]],
      
      -- |Inference check.  Takes a list of antecedent expressions
      --  and a consequent expression, returning True if the
      --  consequence can be obtained from the antecedents by
      --  application of the rule.  When the antecedents and
      --  consequent are both given, this is generally more efficient
      --  that using either forward or backward chaining.
      --  Also, a particular rule may not fully support either
      --  forward or backward chaining, but all rules are required
      --  to fully support this function.
      --
      --  A default implementation based on forward chaining is
      --  given below.
      checkInference :: [ex] -> ex -> Bool 
    }

-- |Define equality of rules as equality of the rule names.
instance Eq (Rule ex) where
    r1 == r2 = ruleName r1 == ruleName r2

-- |Define ordering of rules based on the rule names.
instance Ord (Rule ex) where
    r1 <= r2 = ruleName r1 <= ruleName r2

instance Show (Rule ex) where
    show rl = "Rule " ++ show (ruleName rl)

-- | A set of rules labelled with their name.
type RuleMap ex = M.Map ScopedName (Rule ex)

-- | Checks that consequence is a result of
-- applying the rule to the antecedants.
fwdCheckInference :: 
  (Eq ex) 
  => Rule ex   -- ^ rule
  -> [ex]      -- ^ antecedants
  -> ex        -- ^ consequence
  -> Bool
fwdCheckInference rule ante cons =
    cons `elem` fwdApply rule ante

-- | Checks that the antecedants are all required
-- to create the consequence using the given rule.
bwdCheckInference ::
  (Eq ex) 
  => Rule ex   -- ^ rule
  -> [ex]      -- ^ antecedants
  -> ex        -- ^ consequence
  -> Bool
bwdCheckInference rule ante cons = any checkAnts (bwdApply rule cons)
    where
        checkAnts = all (`elem` ante)

-- | The null rule.
nullRule :: Rule ex
nullRule = Rule
    { ruleName = makeNSScopedName nullScope "nullRule"
    , fwdApply = const []
    , bwdApply = const []
    , checkInference = \ _ _ -> False
    }

------------------------------------------------------------
--  Shows formatting support functions
-----------------------------------------------------------

-- |Show a string left justified in a field of at least the specified
--  number of characters width.
showsWidth :: Int -> String -> ShowS
showsWidth wid str more = str ++ replicate pad ' ' ++ more
    where
        pad = wid - length str


--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011, 2012, 2022 Douglas Burke
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
