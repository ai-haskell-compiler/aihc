{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Ruleset
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--                 2011, 2012, 2014, 2016, 2024 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  CPP, OverloadedStrings
--
--  This module defines some datatypes and functions that are
--  used to define rules and rulesets over RDF graphs.
--
--  For the routines that accept a graph in N3 format, the following
--  namespaces are pre-defined for use by the graph:
--     @rdf:@ and @rdfs:@.
--
--------------------------------------------------------------------------------

module Swish.RDF.Ruleset
    (
     -- * Data types for RDF Ruleset
     RDFFormula, RDFRule, RDFRuleMap
    , RDFClosure, RDFRuleset, RDFRulesetMap
    , nullRDFFormula
    , GraphClosure(..), makeGraphClosureRule
    , makeRDFGraphFromN3Builder
    , makeRDFFormula
    , makeRDFClosureRule
      -- * Create rules using Notation3 statements
    , makeN3ClosureRule
    , makeN3ClosureSimpleRule
    , makeN3ClosureModifyRule
    , makeN3ClosureAllocatorRule
    , makeNodeAllocTo
      -- * Debugging
    , graphClosureFwdApply, graphClosureBwdApply
    )
where

import Swish.Namespace (Namespace, ScopedName)
import Swish.Namespace (makeNSScopedName, namespaceToBuilder)
import Swish.QName (LName)
import Swish.Rule (Formula(..), Rule(..), RuleMap)
import Swish.Rule (fwdCheckInference, nullSN)
import Swish.Ruleset (Ruleset(..), RulesetMap)
import Swish.GraphClass (Label(..), ArcSet, LDGraph(..))
import Swish.VarBinding (VarBindingModify(..))
import Swish.VarBinding (makeVarBinding, applyVarBinding, joinVarBindings, vbmCompose, varBindingId)

import Swish.RDF.Query
    ( rdfQueryFind
    , rdfQueryBack, rdfQueryBackModify
    , rdfQuerySubs
    , rdfQuerySubsBlank
    )

import Swish.RDF.Graph
    ( RDFLabel(..), RDFGraph, RDFArcSet
    , makeBlank, newNodes
    , merge, allLabels
    , toRDFGraph)

import Swish.RDF.VarBinding (RDFVarBinding, RDFVarBindingModify)
import Swish.RDF.Parser.N3 (parseN3)

import Swish.RDF.Vocabulary (swishName, namespaceRDF, namespaceRDFS)

import Swish.Utils.ListHelpers (flist)

import Data.List (nub)
import Data.Maybe (fromMaybe)

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Data.Monoid (Monoid(..))
#endif

import qualified Data.Set as S
import qualified Data.Text.Lazy.Builder as B

------------------------------------------------------------
--  Datatypes for RDF ruleset
------------------------------------------------------------

-- | A named formula expressed as a RDF Graph.
type RDFFormula     = Formula RDFGraph

-- | A named inference rule expressed in RDF.
type RDFRule        = Rule RDFGraph

-- | A map for 'RDFRule' rules.
type RDFRuleMap     = RuleMap RDFGraph

-- | A 'GraphClosure' for RDF statements.
type RDFClosure     = GraphClosure RDFLabel

-- | A 'Ruleset' for RDF.
type RDFRuleset     = Ruleset RDFGraph

-- | A map for 'RDFRuleset'.
type RDFRulesetMap  = RulesetMap RDFGraph

------------------------------------------------------------
--  Declare null RDF formula
------------------------------------------------------------

-- | The null RDF formula.
nullRDFFormula :: Formula RDFGraph
nullRDFFormula = Formula
    { formName = nullSN "nullRDFGraph"
    , formExpr = mempty
    }

------------------------------------------------------------
--  Datatype for graph closure rule
------------------------------------------------------------

-- |Datatype for constructing a graph closure rule
data GraphClosure lb = GraphClosure
    { nameGraphRule :: ScopedName   -- ^ Name of rule for proof display
    , ruleAnt       :: ArcSet lb    -- ^ Antecedent triples pattern
                                    --   (may include variable nodes)
    , ruleCon       :: ArcSet lb    -- ^ Consequent triples pattern
                                    --   (may include variable nodes)
    , ruleModify    :: VarBindingModify lb lb
                                    -- ^ Structure that defines additional
                                    --   constraints and/or variable
                                    --   bindings based on other matched
                                    --   query variables.  Matching the
                                    --   antecedents.  Use 'varBindingId' if
                                    --   no additional variable constraints
                                    --   or bindings are added beyond those
                                    --   arising from graph queries.
    }

-- | Equality is based on the closure rule, anrecedents and
--   consequents.
instance (Label lb) => Eq (GraphClosure lb) where
    c1 == c2 = nameGraphRule c1 == nameGraphRule c2 &&
               ruleAnt c1 == ruleAnt c2 &&
               ruleCon c1 == ruleCon c2

instance Show (GraphClosure lb) where
    show c = "GraphClosure " ++ show (nameGraphRule c)

------------------------------------------------------------
--  Define inference rule based on RDF graph closure rule
------------------------------------------------------------

-- |Define a value of type Rule based on an RDFClosure value.
makeGraphClosureRule :: GraphClosure RDFLabel -> Rule RDFGraph
makeGraphClosureRule grc = newrule
    where
        newrule = Rule
            { ruleName       = nameGraphRule grc
            , fwdApply       = graphClosureFwdApply grc
            , bwdApply       = graphClosureBwdApply grc
            , checkInference = fwdCheckInference newrule
            }

-- | Forward chaining function based on RDF graph closure description
--
--  Note:  antecedents here are presumed to share bnodes.
--
graphClosureFwdApply :: 
  GraphClosure RDFLabel 
  -> [RDFGraph] 
  -> [RDFGraph]
graphClosureFwdApply grc grs =
    let gr   = if null grs then mempty else foldl1 addGraphs grs
        vars = queryFind (ruleAnt grc) gr
        varm = vbmApply (ruleModify grc) vars
        cons = querySubs varm (ruleCon grc)
    in
        {-
        seq cons $
        seq (trace "\ngraphClosureFwdApply") $
        seq (traceShow "\nvars: " vars) $
        seq (traceShow "\nvarm: " varm) $
        seq (traceShow "\ncons: " cons) $
        seq (trace "\n") $
        -}
        --  Return null list or single result graph that is the union
        --  (not merge) of individual results:
        if null cons then [] else [foldl1 addGraphs cons]
        -- cons {- don't merge results -}

-- | Backward chaining function based on RDF graph closure description
graphClosureBwdApply :: GraphClosure RDFLabel -> RDFGraph -> [[RDFGraph]]
graphClosureBwdApply grc gr =
    let vars = rdfQueryBackModify (ruleModify grc) $
               queryBack (ruleCon grc) gr
        --  This next function eliminates duplicate variable bindings.
        --  It is strictly redundant, but comparing variable
        --  bindings is much cheaper than comparing graphs.
        --  I don't know if many duplicate graphs will be result
        --  of exact duplicate variable bindings, so this may be
        --  not very effective.
        varn = map nub vars
    in
        --  The 'nub ante' below eliminates duplicate antecedent graphs,
        --  based on graph matching, which tests for equivalence under
        --  bnode renaming, with a view to reducing redundant arcs in
        --  the merged antecedent graph, hence less to prove in
        --  subsequent back-chaining steps.
        --
        --  Each antecedent is reduced to a single RDF graph, when
        --  bwdApply specifies a list of expressions corresponding to
        --  each antecedent.
        [ [foldl1 merge (nub ante)]
          | vs <- varn
          , let ante = querySubsBlank vs (ruleAnt grc) ]

------------------------------------------------------------
--  RDF graph query and substitution support functions
------------------------------------------------------------

queryFind :: RDFArcSet -> RDFGraph -> [RDFVarBinding]
queryFind qas = rdfQueryFind (toRDFGraph qas)

queryBack :: RDFArcSet -> RDFGraph -> [[RDFVarBinding]]
queryBack qas = rdfQueryBack (toRDFGraph qas)

querySubs :: [RDFVarBinding] -> RDFArcSet -> [RDFGraph]
querySubs vars = rdfQuerySubs vars . toRDFGraph

querySubsBlank :: [RDFVarBinding] -> RDFArcSet -> [RDFGraph]
querySubsBlank vars = rdfQuerySubsBlank vars . toRDFGraph

------------------------------------------------------------
--  Method for creating an RDF formula value from N3 text
------------------------------------------------------------

mkPrefix :: Namespace -> B.Builder
mkPrefix = namespaceToBuilder

prefixRDF :: B.Builder
prefixRDF = 
  mconcat 
  [ mkPrefix namespaceRDF
  , mkPrefix namespaceRDFS
    ]

-- |Helper function to parse a string containing Notation3
--  and return the corresponding RDFGraph value.
--
makeRDFGraphFromN3Builder :: B.Builder -> RDFGraph
makeRDFGraphFromN3Builder b = 
  let t = B.toLazyText (prefixRDF `mappend` b)
  in case parseN3 t Nothing of
    Left  msg -> error msg
    Right gr  -> gr

-- |Create an RDF formula.
makeRDFFormula ::
    Namespace     -- ^ namespace to which the formula is allocated
    -> LName      -- ^ local name for the formula in the namespace
    -> B.Builder  -- ^ graph in Notation 3 format
    -> RDFFormula
makeRDFFormula scope local gr = 
  Formula
    { formName = makeNSScopedName scope local
    , formExpr = makeRDFGraphFromN3Builder gr
    }

------------------------------------------------------------
--  Create an RDF closure rule from supplied graphs
------------------------------------------------------------

-- |Constructs an RDF graph closure rule.  That is, a rule that
--  given some set of antecedent statements returns new statements
--  that may be added to the graph.
--
makeRDFClosureRule ::
    ScopedName -- ^ scoped name for the new rule
    -> [RDFGraph] -- ^ RDFGraphs that are the entecedent of the rule.
                  --
                  -- (Note:  bnodes and variable names are assumed to be shared
                  -- by all the entecedent graphs supplied.  /is this right?/)
    -> RDFGraph   -- ^ the consequent graph
    -> RDFVarBindingModify -- ^ is a variable binding modifier value that may impose
    --          additional conditions on the variable bindings that
    --          can be used for this inference rule, or which may
    --          cause new values to be allocated for unbound variables.
    --          These modifiers allow for certain inference patterns
    --          that are not captured by simple "closure rules", such
    --          as the allocation of bnodes corresponding to literals,
    --          and are an extension point for incorporating datatypes
    --          into an inference process.
    --
    --          If no additional constraints or variable bindings are
    --          to be applied, use value 'varBindingId'
    --
    -> RDFRule
makeRDFClosureRule sname antgrs congr vmod = makeGraphClosureRule
    GraphClosure
        { nameGraphRule = sname
        , ruleAnt       = S.unions $ map getArcs antgrs
        , ruleCon       = getArcs congr
        , ruleModify    = vmod
        }

------------------------------------------------------------
--  Methods to create an RDF closure rule from N3 input
------------------------------------------------------------
--
--  These functions are used internally by Swish to construct
--  rules from textual descriptions.

-- |Constructs an RDF graph closure rule.  That is, a rule that
--  given some set of antecedent statements returns new statements
--  that may be added to the graph.  This is the basis for
--  implementation of most of the inference rules given in the
--  RDF formal semantics document.
--
makeN3ClosureRule ::
    Namespace -- ^ namespace to which the rule is allocated
    -> LName  -- ^ local name for the rule in the namespace
    -> B.Builder 
    -- ^ the Notation3 representation
    --   of the antecedent graph.  (Note: multiple antecedents
    --   can be handled by combining multiple graphs.)
    -> B.Builder -- ^ the Notation3 representation of the consequent graph.
    -> RDFVarBindingModify
    -- ^ a variable binding modifier value that may impose
    --   additional conditions on the variable bindings that
    --   can be used for this inference rule, or which may
    --   cause new values to be allocated for unbound variables.
    --   These modifiers allow for certain inference patterns
    --   that are not captured by simple closure rules, such
    --   as the allocation of bnodes corresponding to literals,
    --   and are an extension point for incorporating datatypes
    --   into an inference process.
    --
    --   If no additional constraints or variable bindings are
    --   to be applied, use a value of 'varBindingId', or use
    --   'makeN3ClosureSimpleRule'.
    -> RDFRule
makeN3ClosureRule scope local ant con =
    makeRDFClosureRule (makeNSScopedName scope local) [antgr] congr
    where
        antgr = makeRDFGraphFromN3Builder ant
        congr = makeRDFGraphFromN3Builder con

-- |Construct a simple RDF graph closure rule without
--  additional node allocations or variable binding constraints.
--
makeN3ClosureSimpleRule ::
    Namespace -- ^ namespace to which the rule is allocated
    -> LName  -- ^ local name for the rule in the namepace
    -> B.Builder
    -- ^ the Notation3 representation
    --   of the antecedent graph.  (Note: multiple antecedents
    --   can be handled by combining multiple graphs.)
    -> B.Builder  -- ^ the Notation3 representation of the consequent graph.
    -> RDFRule
makeN3ClosureSimpleRule scope local ant con =
    makeN3ClosureRule scope local ant con varBindingId

-- |Constructs an RDF graph closure rule that incorporates
--  a variable binding filter and a variable binding modifier.
--
makeN3ClosureModifyRule ::
    Namespace -- ^ namespace to which the rule is allocated
    -> LName  -- ^ local name for the rule in the given namespace
    -> B.Builder -- ^ the Notation3 representation
    --                of the antecedent graph.  (Note: multiple antecedents
    --                can be handled by combining multiple graphs.)
    -> B.Builder -- ^ the Notation3 representation of the consequent graph.
    -> RDFVarBindingModify
    -- ^ a variable binding modifier value that may impose
    --   additional conditions on the variable bindings that
    --   can be used for this inference rule (@vflt@).
    --
    --   These modifiers allow for certain inference patterns
    --   that are not captured by simple closure rules, such
    --   as deductions that pertain only to certain kinds of
    --   nodes in a graph.
    -> RDFVarBindingModify
    -- ^ a variable binding modifier that is applied to the
    --   variable bindings obtained, typically to create some
    --   additional variable bindings.  This is applied before
    --   the preceeding filter rule (@vflt@).
    -> RDFRule
makeN3ClosureModifyRule scope local ant con vflt vmod =
    makeN3ClosureRule scope local ant con modc
    where
        modc  = fromMaybe varBindingId $ vbmCompose vmod vflt

{-
    makeRDFClosureRule (ScopedName scope local) [antgr] congr modc
    where
        antgr = makeRDFGraphFromN3String ant
        congr = makeRDFGraphFromN3String con
        modc  = case vbmCompose vmod vflt of
            Just x  -> x
            Nothing -> varBindingId
-}

-- |Construct an RDF graph closure rule with a bnode allocator.
--
--  This function is rather like 'makeN3ClosureModifyRule', except that
--  the variable binding modifier is a function from the variables in
--  the variables and bnodes contained in the antecedent graph.
--
makeN3ClosureAllocatorRule ::
    Namespace -- ^ namespace to which the rule is allocated
    -> LName  -- ^ local name for the rule in the given namespace
    -> B.Builder -- ^ the Notation3 representation
    --                of the antecedent graph.  (Note: multiple antecedents
    --                can be handled by combining multiple graphs.)
    -> B.Builder -- ^ the Notation3 representation of the consequent graph.
    -> RDFVarBindingModify
    -- ^ variable binding modifier value that may impose
    --   additional conditions on the variable bindings that
    --   can be used for this inference rule (@vflt@).
    -> ( [RDFLabel] -> RDFVarBindingModify )
    -- ^ function applied to a list of nodes to yield a
    --   variable binding modifier value.
    --
    --   The supplied parameter is applied to a list of all of
    --   the variable nodes (including all blank nodes) in the
    --   antecedent graph, and then composed with the @vflt@
    --   value.  This allows any node allocation
    --   function to avoid allocating any blank nodes that
    --   are already used in the antecedent graph.
    --   (See 'makeNodeAllocTo').
    -> RDFRule
makeN3ClosureAllocatorRule scope local ant con vflt aloc =
    makeRDFClosureRule (makeNSScopedName scope local) [antgr] congr modc
    where
        antgr = makeRDFGraphFromN3Builder ant
        congr = makeRDFGraphFromN3Builder con
        vmod  = aloc $ S.toList (allLabels labelIsVar antgr)
        modc  = fromMaybe varBindingId $ vbmCompose vmod vflt

------------------------------------------------------------
--  Query binding modifier for "allocated to" logic
------------------------------------------------------------

-- |This function defines a variable binding modifier that
--  allocates a new blank node for each value bound to
--  a query variable, and binds it to another variable
--  in each query binding.
--
--  This provides a single binding for query variables that would
--  otherwise be unbound by a query.  For example, consider the
--  inference pattern:
--        
--  >  ?a hasUncle ?c => ?a hasFather ?b . ?b hasBrother ?c .
--        
--  For a given @?a@ and @?c@, there is insufficient information
--  here to instantiate a value for variable @?b@.  Using this
--  function as part of a graph instance closure rule allows
--  forward chaining to allocate a single bnode for each
--  occurrence of @?a@, so that given:
--        
--  >  Jimmy hasUncle Fred .
--  >  Jimmy hasUncle Bob .
--
--  leads to exactly one bnode inference of:
--
--  >  Jimmy hasFather _:f .
--
--  giving:
--
--  >  Jimmy hasFather _:f .
--  >  _:f   hasBrother Fred .
--  >  _:f   hasBrother Bob .
--
--  rather than:
--
--  >  Jimmy hasFather _:f1 .
--  >  _:f1  hasBrother Fred .
--  >  Jimmy hasFather _:f2 .
--  >  _:f2  hasBrother Bob .
--
--  This form of constrained allocation of bnodes is also required for
--  some of the inference patterns described by the RDF formal semantics,
--  particularly those where bnodes are substituted for URIs or literals.
--
makeNodeAllocTo ::
    RDFLabel      -- ^ variable node to which a new blank node is bound
    -> RDFLabel   -- ^ variable which is bound in each query to a graph
                  --  node to which new blank nodes are allocated.
    -> [RDFLabel]
    -> RDFVarBindingModify
makeNodeAllocTo bindvar alocvar exbnode = VarBindingModify
        { vbmName   = swishName "makeNodeAllocTo"
        , vbmApply  = applyNodeAllocTo bindvar alocvar exbnode
        , vbmVocab  = [alocvar,bindvar]
        , vbmUsage  = [[bindvar]]
        }

--  Auxiliary function that performs the node allocation defined
--  by makeNodeAllocTo.
--
--  bindvar is a variable node to which a new blank node is bound
--  alocvar is a variable which is bound in each query to a graph
--          node to which new blank nodes are allocated.
--  exbnode is a list of existing blank nodes, to be avoided by
--          the new blank node allocator.
--  vars    is a list of variable bindings to which new bnode
--          allocations for the indicated bindvar are to be added.
--
applyNodeAllocTo ::
    RDFLabel -> RDFLabel -> [RDFLabel] -> [RDFVarBinding] -> [RDFVarBinding]
applyNodeAllocTo bindvar alocvar exbnode vars =
    let
        app       = applyVarBinding
        alocnodes = zip (nub $ flist (map app vars) alocvar)
                        (newNodes (makeBlank bindvar) exbnode)


        tlist var = [ [(bindvar, b)] |
                      (v,b) <- alocnodes,
                      app var alocvar == v ]
        hlist var = case tlist var of
          (h:_) -> h
          _ -> error "internal error"

        newvb var = joinVarBindings (makeVarBinding (hlist var)) var

    in
        map newvb vars


--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011, 2012, 2014, 2016, 2024 Douglas Burke
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
