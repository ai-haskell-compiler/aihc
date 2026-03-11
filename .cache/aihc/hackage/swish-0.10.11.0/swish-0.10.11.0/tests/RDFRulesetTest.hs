{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  RDFRulesetTest
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012, 2013, 2014 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  CPP, OverloadedStrings
--
--  This module contains test cases for ruleset data.
--
--  Note that the proof-related methods defined in RDFRuleset are tested
--  by RDFProofTest and/or RDFProofCheck.
--

module Main where

import qualified Data.Set as S
import qualified Data.Text.Lazy.Builder as B

import qualified Test.Framework as TF

import qualified TestHelpers as TH

import Swish.Namespace (Namespace, makeNamespace, getNamespaceTuple, getNamespaceURI, ScopedName, makeScopedName, makeNSScopedName, namespaceToBuilder)
import Swish.QName (LName)
import Swish.Rule (Formula(..), Rule(..), fwdCheckInference )
import Swish.Ruleset (makeRuleset, getRulesetNamespace, getRulesetAxioms)
import Swish.Ruleset (getRulesetRules, getRulesetAxiom, getRulesetRule)
import Swish.VarBinding (makeVarBinding, vbmCompose, makeVarFilterModify)

import Swish.RDF.Ruleset
    ( RDFFormula, RDFRule, RDFClosure, RDFRuleset
    , GraphClosure(..)
    , makeRDFGraphFromN3Builder
    , makeRDFFormula
    , makeN3ClosureSimpleRule
    , makeNodeAllocTo
    , graphClosureFwdApply, graphClosureBwdApply
    )

import Swish.RDF.Query (rdfQueryBack, rdfQueryBackModify)

import Swish.RDF.VarBinding
    ( RDFVarBinding
    , RDFVarBindingModify
    , RDFVarBindingFilter
    , rdfVarBindingXMLLiteral
    )

import Swish.RDF.Graph
    ( Label (..), RDFLabel(..), RDFGraph
    , RDFArcSet
    , getArcs
    , allLabels
    , toRDFGraph
    )
    
import Swish.RDF.Vocabulary (namespaceRDF, namespaceRDFS, namespaceOWL, scopeRDF)

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Data.Monoid (Monoid(..))
#endif

import Data.List (sort)
import Data.Maybe (isJust, fromJust)
import Data.Text (Text)

import Test.HUnit
    ( Test(TestCase,TestList)
    , assertBool
    )

import Network.URI (URI, parseURI)

import TestHelpers ( conv, test
                   , testLe
                   , testCompare
                   , testCompareEq
                   , testMaker
                   )

------------------------------------------------------------
--  Test case helpers
------------------------------------------------------------

testVal :: (Eq a, Show a) => String -> a -> a -> Test
testVal = testCompare "testVal:"

testEq :: (Eq a, Show a) => String -> Bool -> a -> a -> Test
testEq = testCompareEq "testIsEq:"

testEqual :: (Eq a, Show a) => String -> a -> a -> Test
testEqual = TH.testEq

testStringEq :: String -> String -> String -> Test
testStringEq = testCompare "testStringEq:"

testSameNamespace :: String -> Namespace -> Namespace -> Test
testSameNamespace = testMaker getNamespaceTuple "testSameNamespace:"

testScopedNameEq :: String -> Bool -> ScopedName -> ScopedName -> Test
testScopedNameEq = testCompareEq "testScopedNameEq:"

testSameAs :: (Ord a) => String -> String -> [a] -> [a] -> Test
testSameAs l1 l2 x y =
  let z = sort x == sort y
  in TestCase (assertBool ("testSameAs:" ++ l1 ++ ":" ++ l2) z)
     
testSameAxioms :: String -> [RDFFormula] -> [RDFFormula] -> Test
testSameAxioms = testSameAs "Axioms"

testSameRules :: String -> [RDFRule] -> [RDFRule] -> Test
testSameRules = testSameAs "Rules"

------------------------------------------------------------
--  Common values
------------------------------------------------------------

pref_rdf, pref_owl :: URI
pref_rdf = getNamespaceURI namespaceRDF
pref_owl = getNamespaceURI namespaceOWL

toURI :: String -> URI
toURI = fromJust . parseURI

toNS :: Maybe Text -> String -> Namespace
toNS p = makeNamespace p . toURI

------------------------------------------------------------
--  Define and manipulate rulesets
------------------------------------------------------------
--
--  A ruleset is essentially a collection of axioms and rules
--  associated with a namespace.
--
--  Rulesets for RDF, RDFS and basic datatyping are predefined:
--  see RDFRuleset, RDFSRuleset and RDFDRuleset.
--  Additional rulesets may be defined for specific datatypes.
--
--  A proof context is a list of rulesets,
--  which may be cited by a proof.

rn1 :: Namespace
rn1  = toNS (Just "r1") "http://id.ninebynine.org/wip/2003/rulesettest/r1"

-- Common prefix declarations for graph expressions

mkPrefix :: Namespace -> B.Builder
mkPrefix = namespaceToBuilder

prefix :: B.Builder
prefix =
  mconcat 
  [ mkPrefix namespaceRDF
  , mkPrefix namespaceRDFS
  , mkPrefix (toNS (Just "ex") "http://example.org/")
  ]

a11, a12 :: RDFFormula
a11  = makeRDFFormula rn1 "a11" $ prefix `mappend` "ex:R1 rdf:type ex:C1 ."
a12  = makeRDFFormula rn1 "a12" $ prefix `mappend` "ex:R2 rdf:type ex:C2 ."

r11, r12 :: RDFRule
r11  = makeN3ClosureSimpleRule rn1 "r11"
            ( prefix `mappend` "?r1 rdf:type ex:C1 . ?r2 rdf:type ex:C2 ." )
            ( prefix `mappend` "?r1 ex:P1 ?r2 ." )

r12  = makeN3ClosureSimpleRule rn1 "r12"
            ( prefix `mappend` "?r1 rdf:type ex:C1 . ?r2 rdf:type ex:C2 ." )
            ( prefix `mappend` "?r2 ex:P2 ?r1 ." )

--  Basic formula and rule comparison tests
--  (tests support code added in module Proof.hs)

testFormulaSuite :: Test
testFormulaSuite =
  TestList
  [ testEq "testCmpAX01" True  a11 a11
  , testEq "testCmpAX02" False a11 a12
  , testLe "testCmpAX03" True  a11 a11
  , testLe "testCmpAX04" True  a11 a12
  , testLe "testCmpAX05" False a12 a11
  ]

testRuleSuite :: Test
testRuleSuite =
  TestList
  [ testEq "testCmpRU01" True  r11 r11
  , testEq "testCmpRU02" False r11 r12
  , testLe "testCmpRU03" True  r11 r11
  , testLe "testCmpRU04" True  r11 r12
  , testLe "testCmpRU05" False r12 r11
  ]

--  Test simple ruleset construction and access

a1s :: [RDFFormula]
a1s  = [ a11, a12 ]

r1s :: [RDFRule]
r1s  = [ r11, r12 ]

r1 :: RDFRuleset
r1 = makeRuleset rn1 a1s r1s

testRulesetSuite :: Test
testRulesetSuite = 
  TestList
  [ testSameNamespace "testNS01" rn1 (getRulesetNamespace r1)
  , testSameAxioms    "testAX01" a1s (getRulesetAxioms r1)
  , testSameRules     "testRU01" r1s (getRulesetRules r1)
  , testEqual "testGeta11" (Just a11) $
      getRulesetAxiom (makeNSScopedName rn1 "a11") r1
  , testEqual "testGeta11" (Just a12) $
      getRulesetAxiom (makeNSScopedName rn1 "a12") r1
  , testEqual "testGetr11" (Just r11) $
      getRulesetRule (makeNSScopedName rn1 "r11") r1
  , testEqual "testGetr12" (Just r12) $
      getRulesetRule (makeNSScopedName rn1 "r12") r1
  , testEqual "testGetnone" Nothing $
      getRulesetRule (makeNSScopedName rn1 "none") r1
  ]

------------------------------------------------------------
--  Component tests for RDF proof context
------------------------------------------------------------

scopeex :: Namespace
scopeex = toNS (Just "ex") "http://id.ninebynine.org/wip/2003/RDFProofCheck#"

makeFormula :: Namespace -> LName -> B.Builder -> RDFFormula
makeFormula scope local gr =
    makeRDFFormula scope local $ prefix `mappend` gr

allocateTo :: String -> String -> [RDFLabel] -> RDFVarBindingModify
allocateTo bv av = makeNodeAllocTo (Var bv) (Var av)

isXMLLit :: String -> RDFVarBindingFilter
isXMLLit = rdfVarBindingXMLLiteral . Var

queryBack :: RDFArcSet -> RDFGraph -> [[RDFVarBinding]]
queryBack qas = rdfQueryBack (toRDFGraph qas)

-- Backward chaining rdf:r2

rdfr2ant, rdfr2con :: RDFGraph
rdfr2ant  = makeRDFGraphFromN3Builder "?x ?a ?l . "
rdfr2con  = makeRDFGraphFromN3Builder "?x ?a ?b . ?b rdf:type rdf:XMLLiteral ."

rdfr2modv :: RDFVarBindingModify
rdfr2modv = allocateTo "b" "l" $ S.toList $ allLabels labelIsVar rdfr2ant

rdfr2modc :: Maybe RDFVarBindingModify
rdfr2modc = vbmCompose (makeVarFilterModify $ isXMLLit "l") rdfr2modv

rdfr2grc :: RDFClosure 
rdfr2grc = GraphClosure
            { nameGraphRule = makeNSScopedName scopeRDF "r2"
            , ruleAnt       = getArcs rdfr2ant
            , ruleCon       = getArcs rdfr2con
            , ruleModify    = fromJust rdfr2modc
            }

rdfr2rul :: RDFRule
rdfr2rul = Rule
            { ruleName       = nameGraphRule rdfr2grc
            , fwdApply       = graphClosureFwdApply rdfr2grc
            , bwdApply       = graphClosureBwdApply rdfr2grc
            , checkInference = fwdCheckInference rdfr2rul
            }

con03 :: RDFGraph
con03 = formExpr $ makeFormula scopeex "con03" $
    "ex:s ex:p1 _:l1 ; ex:p2a _:l2; ex:p2b _:l2 ." `mappend`
    "_:l1 rdf:type rdf:XMLLiteral ." `mappend`
    "_:l2 rdf:type rdf:XMLLiteral ."

v_a, v_b, v_x :: RDFLabel
v_a   = Var "a"
v_b   = Var "b"
v_x   = Var "x"

exURI :: URI
exURI = toURI "http://example.org/"

u_s, u_p1, u_p2a, u_p2b, u_rt, u_rx :: RDFLabel
u_s   = Res $ makeScopedName Nothing exURI "s"
u_p1  = Res $ makeScopedName Nothing exURI "p1"
u_p2a = Res $ makeScopedName Nothing exURI "p2a"
u_p2b = Res $ makeScopedName Nothing exURI "p2b"
u_rt  = Res $ makeScopedName Nothing pref_rdf "type"
u_rx  = Res $ makeScopedName Nothing pref_rdf "XMLLiteral"

b_l1, b_l2 :: RDFLabel
b_l1  = Blank "l1"
b_l2  = Blank "l2"

-- could look at S.Set (S.Set RDFVarBinding) which would make
-- comparison easier to write
--
rdfr2v1, rdfr2b1, rdfr2v2 :: [[RDFVarBinding]]
rdfr2v1 = queryBack (ruleCon rdfr2grc) con03

rdfr2b1 = [ [ makeVarBinding [ (v_b,b_l2) ]
            , makeVarBinding [ (v_b,b_l1) ]
            , makeVarBinding [ (v_x,u_s), (v_a,u_p2b), (v_b,b_l2) ]
            , makeVarBinding [ (v_x,u_s), (v_a,u_p2a), (v_b,b_l2) ]
            , makeVarBinding [ (v_x,u_s), (v_a,u_p1),  (v_b,b_l1) ]
            ]
          , [ makeVarBinding [ (v_x,b_l2), (v_a,u_rt),  (v_b,u_rx) ]
            , makeVarBinding [ (v_b,b_l1) ]
            , makeVarBinding [ (v_x,u_s),  (v_a,u_p2b), (v_b,b_l2) ]
            , makeVarBinding [ (v_x,u_s),  (v_a,u_p2a), (v_b,b_l2) ]
            , makeVarBinding [ (v_x,u_s),  (v_a,u_p1),  (v_b,b_l1) ]
            ]
          , [ makeVarBinding [ (v_b,b_l2) ]
            , makeVarBinding [ (v_x,b_l1), (v_a,u_rt),  (v_b,u_rx) ]
            , makeVarBinding [ (v_x,u_s),  (v_a,u_p2b), (v_b,b_l2) ]
            , makeVarBinding [ (v_x,u_s),  (v_a,u_p2a), (v_b,b_l2) ]
            , makeVarBinding [ (v_x,u_s),  (v_a,u_p1),  (v_b,b_l1) ]
            ]
          , [ makeVarBinding [ (v_x,b_l2), (v_a,u_rt),  (v_b,u_rx) ]
            , makeVarBinding [ (v_x,b_l1), (v_a,u_rt),  (v_b,u_rx) ]
            , makeVarBinding [ (v_x,u_s),  (v_a,u_p2b), (v_b,b_l2) ]
            , makeVarBinding [ (v_x,u_s),  (v_a,u_p2a), (v_b,b_l2) ]
            , makeVarBinding [ (v_x,u_s),  (v_a,u_p1),  (v_b,b_l1) ]
            ]
          ]

rdfr2v2 = rdfQueryBackModify (ruleModify rdfr2grc) rdfr2v1

-- as rdfr2v2 is empty no point in the following

-- rdfr2v3 :: [[RDFVarBinding]]
-- rdfr2v3 = map nub rdfr2v2

testRDFSuite :: Test
testRDFSuite = 
  TestList
  [ test    "testRDF01" (isJust rdfr2modc)
  , testVal "testRDF02" rdfr2b1 rdfr2v1
  , test "testRDF03" $ null rdfr2v2
  -- , test "testRDF04" $ null rdfr2v3
  , testEq "testRDF09" True [] $ bwdApply rdfr2rul con03
  ]

------------------------------------------------------------
--  All tests
------------------------------------------------------------

allTests :: [TF.Test]
allTests = 
  [ conv "Formula" testFormulaSuite
  , conv "Rule" testRuleSuite
  , conv "Ruleset" testRulesetSuite
  , conv "RDF" testRDFSuite
  ]

main :: IO ()
main = TF.defaultMain allTests

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011, 2012, 2013 Douglas Burke
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
