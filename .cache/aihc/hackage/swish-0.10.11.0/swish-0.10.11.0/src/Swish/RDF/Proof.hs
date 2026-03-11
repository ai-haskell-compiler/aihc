{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Proof
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--                 2011, 2012, 2014, 2016, 2024 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  CPP, FlexibleInstances, UndecidableInstances
--
--  This module instantiates the 'Proof' framework for
--  constructing proofs over 'RDFGraph' expressions.
--  The intent is that this can be used to test some
--  correspondences between the RDF Model theory and
--  corresponding proof theory based on closure rules
--  applied to the graph, per <http://www.w3.org/TR/rdf-mt/>.
--
--------------------------------------------------------------------------------

module Swish.RDF.Proof
    ( RDFProof, RDFProofStep
    , makeRDFProof, makeRDFProofStep
    , makeRdfInstanceEntailmentRule
    , makeRdfSubgraphEntailmentRule
    , makeRdfSimpleEntailmentRule )
where

import Swish.GraphClass (Label(..), LDGraph(..))
import Swish.Namespace (ScopedName)
import Swish.Proof (Proof(..), Step(..))
import Swish.Rule (Expression(..), Rule(..))
import Swish.VarBinding (makeVarBinding)

import Swish.RDF.Graph (RDFLabel(..), RDFGraph, fmapNSGraph)
import Swish.RDF.Graph (merge, allLabels, remapLabelList)
import Swish.RDF.Query (rdfQueryInstance, rdfQuerySubs)
import Swish.RDF.Ruleset (RDFFormula, RDFRule, RDFRuleset)

import Swish.Utils.ListHelpers (flist)

import Data.List (subsequences)

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Data.Monoid (Monoid(..))
#endif

import qualified Data.Map as M
import qualified Data.Set as S

------------------------------------------------------------
--  Type instantiation of Proof framework for RDFGraph data
------------------------------------------------------------
--
--  This is a partial instantiation of the proof framework.
--  Details for applying inference rules are specific to the
--  graph instance type.

------------------------------------------------------------
--  Proof datatypes for graph values
------------------------------------------------------------

-- The following is an orphan instance

-- |Instances of 'LDGraph' are also instance of the
--  @Expression@ class, for which proofs can be constructed.
--  The empty RDF graph is always @True@ (other enduring
--  truths are asserted as axioms).

instance (LDGraph lg lb, Eq (lg lb)) => Expression (lg lb) where
    isValid = S.null . getArcs 

------------------------------------------------------------
--  Define RDF-specific types for proof framework
------------------------------------------------------------

-- | An RDF proof.
type RDFProof     = Proof RDFGraph

-- | A step in an RDF proof.
type RDFProofStep = Step RDFGraph

------------------------------------------------------------
--  Helper functions for constructing proofs on RDF graphs
------------------------------------------------------------

-- |Make an RDF graph proof step.
--
makeRDFProofStep ::
    RDFRule  -- ^ rule to use for this step
    -> [RDFFormula] -- ^ antecedent RDF formulae for this step
    -> RDFFormula -- ^ RDF formula that is the consequent for this step 
    -> RDFProofStep
makeRDFProofStep rul ants con = Step
    { stepRule = rul
    , stepAnt  = ants
    , stepCon  = con
    }

-- |Make an RDF proof.
--
makeRDFProof ::
    [RDFRuleset]      -- ^ RDF rulesets that constitute a proof context for this proof
    -> RDFFormula     -- ^ initial statement from which the goal is claimed to be proven
    -> RDFFormula     -- ^ statement that is claimed to be proven
    -> [RDFProofStep] -- ^ the chain of inference rules in the proof.
    -> RDFProof
makeRDFProof rsets base goal steps = Proof
    { proofContext = rsets
    , proofInput   = base
    , proofResult  = goal
    , proofChain   = steps
    }

------------------------------------------------------------
--  RDF instance entailment inference rule
------------------------------------------------------------

-- |Make an inference rule dealing with RDF instance entailment;
--  i.e. entailments that are due to replacement of a URI or literal
--  node with a blank node.
--
--  The part of this rule expected to be useful is 'checkInference'.
--  The 'fwdApply' and 'bwdApply' functions defined here may return
--  rather large results if applied to graphs with many variables or
--  a large vocabulary, and are defined for experimentation.
--
--  Forward and backward chaining is performed with respect to a
--  specified vocabulary.  In the case of backward chaining, it would
--  otherwise be impossible to bound the options thus generated.
--  In the case of forward chaining, it is often not desirable to
--  have the properties generalized.  If forward or backward backward
--  chaining will not be used, supply an empty vocabulary.
--  Note:  graph method 'Swish.RDF.Graph.allNodes' can be used to obtain a list of all
--  the subjects and objects used in a  graph, not counting nested
--  formulae;  use a call of the form:
--
--  >  allNodes (not . labelIsVar) graph
--
makeRdfInstanceEntailmentRule :: 
  ScopedName     -- ^ name
  -> [RDFLabel]  -- ^ vocabulary
  -> RDFRule
makeRdfInstanceEntailmentRule name vocab = newrule
    where
        newrule = Rule
            { ruleName = name
            , fwdApply = rdfInstanceEntailFwdApply vocab
            , bwdApply = rdfInstanceEntailBwdApply vocab
            , checkInference = rdfInstanceEntailCheckInference
            }

--  Instance entailment forward chaining
--
--  Note:  unless the initial graph is small, the total result
--  here could be very large.  The existential generalizations are
--  sequenced in increasing number of substitutions applied.
--
--  The instances generated are all copies of the merge of the
--  supplied graphs, with some or all of the non-variable nodes
--  replaced by blank nodes.
rdfInstanceEntailFwdApply :: [RDFLabel] -> [RDFGraph] -> [RDFGraph]
rdfInstanceEntailFwdApply vocab ante =
    let
        --  Merge antecedents to single graph, renaming bnodes if needed.
        --  (Null test and using 'foldl1' to avoid merging if possible.)
        mergeGraph  = if null ante then mempty
                      else foldl1 merge ante

        --  Obtain lists of variable and non-variable nodes
        --  (was: nonvarNodes = allLabels (not . labelIsVar) mergeGraph)
        nonvarNodes = vocab
        varNodes    = S.toList $ allLabels labelIsVar mergeGraph

        --  Obtain list of possible remappings for non-variable nodes
        mapList     = remapLabelList nonvarNodes varNodes
        mapSubLists = (drop 1 . subsequences) mapList
        mapGr ls = fmapNSGraph 
                   (\l -> M.findWithDefault l l
                          (M.fromList ls))

    in
        --  Return all remappings of the original merged graph
        flist (map mapGr mapSubLists) mergeGraph

--  Instance entailment backward chaining (for specified vocabulary)
--
--  [[[TODO:  this is an incomplete implementation, there being no
--  provision for instantiating some variables and leaving others
--  alone.  This can be overcome in many cases by combining instance
--  and subgraph chaining.
--  Also, there is no provision for instantiating some variables in
--  a triple and leaving others alone.  This may be fixed later if
--  this function is really needed to be completely faithful to the
--  precise notion of instance entailment.]]]
rdfInstanceEntailBwdApply :: [RDFLabel] -> RDFGraph -> [[RDFGraph]]
rdfInstanceEntailBwdApply vocab cons =
    let
        --  Obtain list of variable nodes
        varNodes     = S.toList $ allLabels labelIsVar cons
        --  Generate a substitution for each combination of variable
        --  and vocabulary node.
        varBindings  = map (makeVarBinding . zip varNodes) vocSequences
        vocSequences = powerSequencesLen (length varNodes) vocab
    in
        --  Generate a substitution for each combination of variable
        --  and vocabulary:
        [ rdfQuerySubs [v] cons | v <- varBindings ]

--  Instance entailment inference checker
rdfInstanceEntailCheckInference :: [RDFGraph] -> RDFGraph -> Bool
rdfInstanceEntailCheckInference ante cons =
    let
        mante = if null ante then mempty        -- merged antecedents
                    else foldl1 merge ante
        qvars = rdfQueryInstance cons mante     -- all query matches
        bsubs = rdfQuerySubs qvars cons         -- all back substitutions
    in
        --  Return True if any back-substitution matches the original
        --  merged antecendent graph.
        elem mante bsubs

------------------------------------------------------------
--  Powersequence (?) -- all sequences from some base values
------------------------------------------------------------

-- |Construct list of lists of sequences of increasing length
powerSeqBylen :: [a] -> [[a]] -> [[[a]]]
powerSeqBylen rs ps = ps : powerSeqBylen rs (powerSeqNext rs ps)

-- |Return sequences of length n+1 given original sequence
--  and list of all sequences of length n
powerSeqNext :: [a] -> [[a]] -> [[a]]
powerSeqNext rs rss = [ h:t | t <- rss, h <- rs ]

-- |Return all powersequences of a given length
powerSequencesLen :: Int -> [a] -> [[a]]
powerSequencesLen len rs = powerSeqBylen rs [[]] !! len

--  Instance entailment notes.
--
--  Relation to simple entailment (s-entails):
--
--  (1) back-substitution yields original graph
--  ex:s1 ex:p1 ex:o1  s-entails  ex:s1 ex:p1 _:o1  by [_:o1/ex:o1]
--
--  (2) back-substitution yields original graph
--  ex:s1 ex:p1 ex:o1  s-entails  ex:s1 ex:p1 _:o2  by [_:o2/ex:o1]
--  ex:s1 ex:p1  _:o1             ex:s1 ex:p1 _:o3     [_:o3/_:o1]
--
--  (3) back-substitution does not yield original graph
--  ex:s1 ex:p1 ex:o1  s-entails  ex:s1 ex:p1 _:o2  by [_:o2/ex:o1]
--  ex:s1 ex:p1  _:o1             ex:s1 ex:p1 _:o3     [_:o3/ex:o1]
--
--  (4) consider
--  ex:s1 ex:p1 ex:o1  s-entails  ex:s1 ex:p1 ex:o1
--  ex:s1 ex:p1 ex:o2             ex:s1 ex:p1 ex:o2
--  ex:s1 ex:p1 ex:o3             ex:s1 ex:p1 _:o1
--                                ex:s1 ex:p1 _:o2
--  where [_:o1/ex:o1,_:o2/ex:o2] yields a simple entailment but not
--  an instance entailment, but [_:o1/ex:o3,_:o2/ex:o3] is also
--  (arguably) an instance entailment.  Therefore, it is not sufficient
--  to look only at the "largest" substitutions to determine instance
--  entailment.
--
--  All this means that when checking for instance entailment by
--  back substitution, all of the query results must be checked.
--  This seems clumsy.  If this function is heavily used with
--  multiple query matches, a modified query that uses each
--  triple of the target graph exactly once may be required.

------------------------------------------------------------
--  RDF subgraph entailment inference rule
------------------------------------------------------------

-- |Make an inference rule dealing with RDF subgraph entailment.
--  The part of this rule expected to be useful is 'checkInference'.
--  The 'fwdApply' function defined here may return rather large
--  results.  But in the name of completeness and experimentation
--  with the possibilities of lazy evaluation, it has been defined.
--
--  Backward chaining is not performed, as there is no reasonable way
--  to choose a meaningful supergraph of that supplied.
makeRdfSubgraphEntailmentRule :: ScopedName -> RDFRule
makeRdfSubgraphEntailmentRule name = newrule
    where
        newrule = Rule
            { ruleName = name
            , fwdApply = rdfSubgraphEntailFwdApply
            , bwdApply = const []
            , checkInference = rdfSubgraphEntailCheckInference
            }

--  Subgraph entailment forward chaining
--
--  Note:  unless the initial graph is small, the total result
--  here could be very large.  The subgraphs are sequenced in
--  increasing size of the sub graph.
rdfSubgraphEntailFwdApply :: [RDFGraph] -> [RDFGraph]
rdfSubgraphEntailFwdApply ante =
    let
        --  Merge antecedents to single graph, renaming bnodes if needed.
        --  (Null test and using 'foldl1' to avoid merging if possible.)
        mergeGraph  = if null ante then mempty
                      else foldl1 merge ante

        list = init $ drop 1 $ subsequences $ S.toList $ getArcs mergeGraph

    in
        --  Return all subgraphs of the full graph constructed above
        -- TODO: update to use sets as appropriate
        map (setArcs mergeGraph . S.fromList) list

--  Subgraph entailment inference checker
--
--  This is of dubious utiltiy, as it doesn't allow for node renaming.
--  The simple entailment inference rule is probably more useful here.
rdfSubgraphEntailCheckInference :: [RDFGraph] -> RDFGraph -> Bool
rdfSubgraphEntailCheckInference ante cons =
    let
        --  Combine antecedents to single graph, renaming bnodes if needed.
        --  (Null test and using 'foldl1' to avoid merging if possible.)
        fullGraph  = if null ante then mempty
                        else foldl1 addGraphs ante
    in
        --  Check each consequent graph arc is in the antecedent graph
        getArcs cons `S.isSubsetOf` getArcs fullGraph

------------------------------------------------------------
--  RDF simple entailment inference rule
------------------------------------------------------------

-- |Make an inference rule dealing with RDF simple entailment.
--  The part of this rule expected to be useful is 'checkInference'.
--  The 'fwdApply' and 'bwdApply' functions defined return null
--  results, indicating that they are not useful for the purposes
--  of proof discovery.
makeRdfSimpleEntailmentRule :: ScopedName -> RDFRule
makeRdfSimpleEntailmentRule name = newrule
    where
        newrule = Rule
            { ruleName = name
            , fwdApply = const []
            , bwdApply = const []
            , checkInference = rdfSimpleEntailCheckInference
            }

--  Simple entailment inference checker
--
--  Note:  antecedents here are presumed to share bnodes.
--         (Use 'merge' instead of 'add' for non-shared bnodes)
--
rdfSimpleEntailCheckInference :: [RDFGraph] -> RDFGraph -> Bool
rdfSimpleEntailCheckInference ante cons =
    let agr = if null ante then mempty else foldl1 addGraphs ante
    in not $ null $ rdfQueryInstance cons agr

{- original..
        not $ null $ rdfQueryInstance cons (foldl1 merge ante)
-}

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
