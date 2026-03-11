{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  RDFProofContextTest
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012, 2013, 2014, 2021 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  OverloadedStrings
--
--  This module contains RDF proof-checking test cases based on the RDF
--  semantics specifications, as capured in module RDFProofContext.
--
--------------------------------------------------------------------------------

module Main where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as B

import qualified Test.Framework as TF

import Swish.Namespace (Namespace, makeNamespace, ScopedName, makeNSScopedName, namespaceToBuilder)
import Swish.QName (LName, newLName)
import Swish.Proof (Step(..), checkProof, checkStep, explainProof)
import Swish.Rule (Formula(..), Rule(..), nullFormula, nullRule)
import Swish.Ruleset (getContextAxiom, getContextRule)

import Swish.RDF.BuiltIn (rdfRulesetMap, allRulesets)

import Swish.RDF.ProofContext (rulesetRDF, rulesetRDFS, rulesetRDFD)
import Swish.RDF.Proof (RDFProof, RDFProofStep, makeRDFProof, makeRDFProofStep )
import Swish.RDF.Ruleset
    ( RDFFormula, RDFRule, RDFRuleset
    , nullRDFFormula
    , makeRDFFormula )

import Swish.RDF.Graph (RDFGraph)
-- import Swish.RDF.GraphShowLines ()

import Swish.RDF.Vocabulary
    ( namespaceRDF
    , namespaceRDFS
    , namespaceRDFD
    , namespaceXSD
    , namespaceXsdType
    , scopeRDF
    , scopeRDFS
    , scopeRDFD
    )

import Data.Maybe (isNothing, fromJust, fromMaybe)

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Data.Monoid (Monoid(..))
#endif

import Network.URI (URI, parseURI)

import Test.HUnit
    ( Test(TestCase,TestList)
    , assertBool, assertEqual )
import TestHelpers ( conv, test
                     , testEq
                     , testElem
                     )

--  misc helpers

testGr :: String -> RDFGraph -> [RDFGraph] -> Test
testGr = testElem
-- testGr lab eg a = TestCase $ assertBool lab (eg `elem` a)

-- testProof "rdfProof01" True rdfProof01
testProof :: String -> Bool -> RDFProof -> Test
testProof lab valid proof = TestList
    [ TestCase $ assertEqual lab valid (checkProof proof)
    , TestCase $ assertBool  (lab++": "++ex) (valid == isNothing expl)
    ]
    where
        expl = explainProof proof
        ex   = fromMaybe "(Proof OK)" expl

-- testProofStep "rdfStep01" True [rules] [antes] rdfStep01
testProofStep ::
    String -> Bool -> [RDFRule] -> [RDFGraph] -> Step RDFGraph
    -> Test
testProofStep lab valid rules antes step =
    TestCase $ assertEqual lab valid (checkStep rules antes step)

--  Various support methods

makeFormula :: Namespace -> LName -> B.Builder -> RDFFormula
makeFormula scope local gr =
    makeRDFFormula scope local (prefix `mappend` gr)

getRule :: String -> RDFRule
getRule nam = getContextRule (makeSName nam) nullRule $
    rdfdContext++[rulesetXsdInt,rulesetXsdStr]

getAxiom :: String -> RDFFormula
getAxiom nam = getContextAxiom (makeSName nam) nullRDFFormula rdfdContext

makeSName :: String -> ScopedName
makeSName nam = makeNSScopedName ns (fromJust (newLName (T.pack loc)))
    where
      err = error $ "makeSName: Unrcognized prefix in rule name: " ++ nam
      (pre, post) = break (==':') nam
      loc = case post of
        "" -> err
        _:loc' -> loc'

      ns = case pre of
             "rs_rdf"  -> scopeRDF
             "rs_rdfs" -> scopeRDFS
             "rs_rdfd" -> scopeRDFD
             "xsd_integer" -> namespaceXsdType "integer"
             "xsd_string"  -> namespaceXsdType "string"
             _ -> err

--  Common definitions

toURI :: String -> URI
toURI = fromJust . parseURI

toNS :: Maybe T.Text -> String -> Namespace
toNS p = makeNamespace p . toURI

mkPrefix :: Namespace -> B.Builder
mkPrefix = namespaceToBuilder

prefix :: B.Builder
prefix =
  mconcat 
  [ mkPrefix namespaceRDF
  , mkPrefix namespaceRDFS
  , mkPrefix namespaceRDFD
  , mkPrefix namespaceXSD
    -- TODO: should the following use scopeex instead?
  , mkPrefix $ toNS (Just "ex") "http://example.org/"
  ]

scopeex :: Namespace
scopeex = toNS (Just "ex") "http://id.ninebynine.org/wip/2003/RDFProofCheck#"

rdfContext, rdfsContext, rdfdContext, xsdintContext,
  xsdstrContext :: [RDFRuleset]
rdfContext    = [ rulesetRDF ]
rdfsContext   = [ rulesetRDF, rulesetRDFS ]
rdfdContext   = [ rulesetRDF, rulesetRDFS, rulesetRDFD ]
xsdintContext = [ rulesetRDF, rulesetRDFS, rulesetRDFD, rulesetXsdInt ]
xsdstrContext = [ rulesetRDF, rulesetRDFS, rulesetRDFD, rulesetXsdStr ]

rulesetXsdInt, rulesetXsdStr :: RDFRuleset
rulesetXsdInt = fromJust $ M.lookup (namespaceXsdType "integer") rdfRulesetMap
rulesetXsdStr = fromJust $ M.lookup (namespaceXsdType "string") rdfRulesetMap

------------------------
--  RDF/S rule tests
------------------------
--
--  These tests aim to exercise the specific closure rule constructs
--  that are used by the RDF/S rules.  They have been prepared as a
--  regression test for a refactoring of the variable binding
--  filtering and modification logic.

--  Simple rule test - forward and backward chaining
--
--  rdfr1 = "?x ?a ?y ." => "?a rdf:type rdf:Property ."
--

rdfr1 :: RDFRule
rdfr1  = getRule "rs_rdf:r1"

ant01, con01, bwd01 :: RDFGraph
ant01  = formExpr $ makeFormula scopeex "ant01" 
    "ex:s ex:p1 ex:o1 ; ex:p2 ex:o2 ."
con01  = formExpr $ makeFormula scopeex "con01" $
    "ex:p1 rdf:type rdf:Property ." `mappend`
    "ex:p2 rdf:type rdf:Property ."
bwd01  = formExpr $ makeFormula scopeex "bwd01a" $
    "_:s1 ex:p1 _:o1 . " `mappend`
    "_:s2 ex:p2 _:o2 . "

--  Simple rule test - no match forward or backward
--
--  rdfsr2 = "?x ?a ?y . ?a rdfs:domain ?z ." => "?x rdf:type ?z ."
--

rdfsr2 :: RDFRule
rdfsr2 = getRule "rs_rdfs:r2"

ant02, con02 :: RDFGraph
ant02  = formExpr $ makeFormula scopeex "ant02" 
    "ex:s ex:p1 ex:o1 . ex:p2 rdfs:domain ex:d2 ."
con02  = formExpr $ makeFormula scopeex "con02" 
    "ex:s ex:p1 ex:o1 . ex:p2 rdfs:domain ex:d2 ."

--  Rule with literal test and "allocateTo":
--  match forward, but not backward
--
--  This is a two-part rule: (a) apply rdflg, (b) apply rdfr2
--
--  rdflg = "?x ?a ?l . " => "?x  ?a ?b . ?b rdf:_allocatedTo ?l ."
--  where:
--          (isLiteral "?l")
--          (allocateTo "?b" "?l")
--
--  rdfr2 = "?x ?a ?l . ?b rdf:_allocatedTo ?l . "
--          => "?b rdf:type rdf:XMLLiteral ."
--  where:
--          (isXMLLit "?l")
--

rdflg, rdfr2 :: RDFRule
rdflg  = getRule "rs_rdf:lg"
rdfr2  = getRule "rs_rdf:r2"

ant03, con03lg, con03r2 :: RDFGraph
ant03  = formExpr $ makeFormula scopeex "ant03" $
    "ex:s ex:p1  \"lit1\"^^rdf:XMLLiteral ; " `mappend`
    "     ex:p2a \"lit2\"^^rdf:XMLLiteral ; " `mappend`
    "     ex:p2b \"lit2\"^^rdf:XMLLiteral ."
con03lg  = formExpr $ makeFormula scopeex "con03" $
    "ex:s ex:p1 _:l1 ; ex:p2a _:l2; ex:p2b _:l2 ." `mappend`
    "_:l1 rdf:_allocatedTo \"lit1\"^^rdf:XMLLiteral ." `mappend`
    "_:l2 rdf:_allocatedTo \"lit2\"^^rdf:XMLLiteral ."
con03r2  = formExpr $ makeFormula scopeex "con03" $
    "_:l1 rdf:type rdf:XMLLiteral ." `mappend`
    "_:l2 rdf:type rdf:XMLLiteral ."

--  Rule with member property test, match forwards and backwards
--  rdfcp1 = "?x  ?c ?y . " => "?c rdf:type rdf:Property ."
--  where:
--          (isMemberProp "?c")
--

rdfcp :: RDFRule
rdfcp  = getRule "rs_rdf:cp1"

ant04, con04, bwd04 :: RDFGraph
ant04  = formExpr $ makeFormula scopeex "ant04" $
    "ex:s rdf:_123 ex:o1 ; " `mappend`
    "     rdf:_2   ex:o2 . "
con04  = formExpr $ makeFormula scopeex "con04" $
    "rdf:_123 rdf:type rdf:Property ." `mappend`
    "rdf:_2   rdf:type rdf:Property ."
bwd04  = formExpr $ makeFormula scopeex "bwd04a" $
    "_:s1 rdf:_123 _:o1 . " `mappend`
    "_:s2 rdf:_2   _:o2 . "

--  Rule with disjunction test, match forwards and backwards
--
--  rdfsr3 = "?x ?a ?u . ?a rdfs:range ?z ." => "?u rdf:type ?z ."
--  where:
--          (requireAny [isUriRef "?u",isBlank "?u"])
--

rdfsr3 :: RDFRule
rdfsr3 = getRule "rs_rdfs:r3"

ant05, con05, bwd05 :: RDFGraph
ant05  = formExpr $ makeFormula scopeex "ant05" $
    "ex:s ex:p1 ex:o1 ; "        `mappend`
    "     ex:p2 _:o2  . "        `mappend`
    "ex:p1 rdfs:range ex:pr1 . " `mappend`
    "ex:p2 rdfs:range ex:pr2 . "
con05  = formExpr $ makeFormula scopeex "con05" $
    "ex:o1 rdf:type ex:pr1 ." `mappend`
    "_:o2  rdf:type ex:pr2 ."
bwd05  = formExpr $ makeFormula scopeex "bwd05a" $
    "_:s1 _:p1 ex:o1 . "        `mappend`
    "_:s2 _:p2 _:o2  . "        `mappend`
    "_:p1 rdfs:range ex:pr1 . " `mappend`
    "_:p2 rdfs:range ex:pr2 . "

--  Rule with disjunction test, fail forwards
--
--  rdfsr3 = "?x ?a ?u . ?a rdfs:range ?z ." => "?u rdf:type ?z ."
--  where:
--          (requireAny [isUriRef "?u",isBlank "?u"])
--

ant06, con06, bwd06, chk06 :: RDFGraph
ant06  = formExpr $ makeFormula scopeex "ant06" $
    "ex:s ex:p1 \"lit1\" . "     `mappend`
    "ex:p1 rdfs:range ex:pr1 . "
con06  = formExpr $ makeFormula scopeex "con06" 
    "_:o1  rdf:type ex:pr1 ."
bwd06  = formExpr $ makeFormula scopeex "bwd06a" $
    "_:s1 _:p1 _:o1 . "      `mappend`
    "_:p1 rdfs:range ex:pr1 . "
chk06  = formExpr $ makeFormula scopeex "bwd06a" $
    "_:s1 _:p1 \"lit1\" . "      `mappend`
    "_:p1 rdfs:range ex:pr1 . "

--  Collected rule tests

testRules :: Test
testRules = 
  TestList
  [ testEq "testRule01a" "rs_rdf:r1" (show $ ruleName rdfr1)
  , testEq "testRule01b" [con01]     (fwdApply rdfr1 [ant01])
  , testEq "testRule01c" [[bwd01]]   (bwdApply rdfr1 con01)
  , test   "testRule01d" (checkInference rdfr1 [ant01] con01)
  , test   "testRule01e" (checkInference rdfr1 [bwd01] con01)
  , testEq "testRule02a" "rs_rdfs:r2" (show $ ruleName rdfsr2)
  , testEq "testRule02b" []           (fwdApply rdfsr2 [ant02])
  , testEq "testRule02c" []           (bwdApply rdfsr2 con02)
  , testEq "testRule02d" False (checkInference rdfsr2 [ant02] con02)
  , testEq "testRule03a" "rs_rdf:lg" (show $ ruleName rdflg)
  , testEq "testRule03b" "rs_rdf:r2" (show $ ruleName rdfr2)
  , testEq "testRule03c" [con03lg]   (fwdApply rdflg [ant03])
  , testEq "testRule03d" []          (bwdApply rdflg con03lg)
  , test   "testRule03e" (checkInference rdflg [ant03] con03lg)
  , testEq "testRule03f" [con03r2]   (fwdApply rdfr2 [con03lg])
  , testEq "testRule03g" []          (bwdApply rdfr2 con03r2)
  , test   "testRule03h" (checkInference rdfr2 [con03lg] con03r2)
  , testEq "testRule04a" "rs_rdf:cp1" (show $ ruleName rdfcp)
  , testEq "testRule04b" [con04]      (fwdApply rdfcp [ant04])
  , testEq "testRule04c" [[bwd04]]    (bwdApply rdfcp con04)
  , test   "testRule04d" (checkInference rdfcp [ant04] con04)
  , test   "testRule01e" (checkInference rdfcp [bwd04] con04)
  , testEq "testRule05a" "rs_rdfs:r3" (show $ ruleName rdfsr3)
  , testEq "testRule05b" [con05]      (fwdApply rdfsr3 [ant05])
  , testEq "testRule05c" [[bwd05]]    (bwdApply rdfsr3 con05)
  , test   "testRule05d" (checkInference rdfsr3 [ant05] con05)
  , test   "testRule01e" (checkInference rdfsr3 [bwd05] con05)
  , testEq "testRule06a" "rs_rdfs:r3" (show $ ruleName rdfsr3)
  , testEq "testRule06b" []           (fwdApply rdfsr3 [ant06])
  , testEq "testRule06c" [[bwd06]]    (bwdApply rdfsr3 con06)
  , testEq "testRule06d" False (checkInference rdfsr3 [ant06] con06)
  , testEq "testRule06e" True  (checkInference rdfsr3 [bwd06] con06)
  , testEq "testRule06e" False (checkInference rdfsr3 [chk06] con06)
  ]

------------------------
--  Complete proof tests
------------------------
--
--  These are a few tests of complete RDF proof chains based on the
--  RDF semantic rules.

--  RDF entailment proof checks

rdfBase01, rdfGoal01 :: RDFFormula
rdfBase01  = makeFormula scopeex "rdfBase01" "ex:s ex:p ex:o ."
rdfGoal01  = makeFormula scopeex "rdfGoal01" "ex:p rdf:type rdf:Property ."

rdfStep01a :: RDFProofStep
rdfStep01a = makeRDFProofStep (getRule "rs_rdf:r1") [rdfBase01] rdfGoal01

rdfProof01 :: RDFProof
rdfProof01 = makeRDFProof rdfsContext rdfBase01 rdfGoal01
                [ rdfStep01a ]

rdfBase02, rdfCon02a, rdfGoal02 :: RDFFormula
rdfBase02  = makeFormula scopeex "rdfBase02" 
                "ex:s ex:p \"l1\"^^rdf:XMLLiteral ."
rdfCon02a  = makeFormula scopeex "rdfStep02a" $
                "ex:s ex:p _:lll . "             `mappend`
                "_:lll rdf:_allocatedTo \"l1\"^^rdf:XMLLiteral . "
rdfGoal02  = makeFormula scopeex "rdfGoal02" 
                "_:lll rdf:type rdf:XMLLiteral . "
                
rdfStep02a, rdfStep02b :: RDFProofStep                
rdfStep02a = makeRDFProofStep (getRule "rs_rdf:lg") [rdfBase02] rdfCon02a
rdfStep02b = makeRDFProofStep (getRule "rs_rdf:r2") [rdfCon02a] rdfGoal02

rdfProof02 :: RDFProof
rdfProof02 = makeRDFProof rdfsContext rdfBase02 rdfGoal02
                [ rdfStep02a, rdfStep02b ]

rdfBase03, rdfCon03a, rdfGoal03 :: RDFFormula
rdfBase03  = makeFormula scopeex "rdfBase03" 
                "ex:s ex:p ex:o ."
rdfCon03a  = makeFormula scopeex "rdfStep03a" $
                "ex:s ex:p _:lll . "             `mappend`
                "_:lll rdf:_allocatedTo \"l1\"^^rdf:XMLLiteral . "
rdfGoal03  = makeFormula scopeex "rdfGoal03" 
                "_:lll rdf:type rdf:XMLLiteral . "

rdfStep03a, rdfStep03b :: RDFProofStep                
rdfStep03a = makeRDFProofStep (getRule "rs_rdf:lg") [rdfBase03] rdfCon03a
rdfStep03b = makeRDFProofStep (getRule "rs_rdf:r2") [rdfCon03a] rdfGoal03

rdfProof03 :: RDFProof
rdfProof03 = makeRDFProof rdfsContext rdfBase03 rdfGoal03
                [ rdfStep03a, rdfStep03b ]

rdfBase04, rdfGoal04 :: RDFFormula
rdfBase04  = makeFormula scopeex "rdfBase04" "ex:s ex:p ex:o ."
rdfGoal04  = makeFormula scopeex "rdfGoal04" "_:s  ex:p _:o ."

rdfStep04a :: RDFProofStep                
rdfStep04a = makeRDFProofStep (getRule "rs_rdf:se") [rdfBase04] rdfGoal04

rdfProof04 :: RDFProof
rdfProof04 = makeRDFProof rdfsContext rdfBase04 rdfGoal04
                [ rdfStep04a ]

rdfBase05 :: RDFFormula
rdfBase05  = makeFormula scopeex "rdfBase05" 
                "ex:s ex:p rdf:nil ."

rdfStep05a :: RDFProofStep
rdfStep05a = makeRDFProofStep (getRule "rs_rdf:r1") [rdfBase05]  rdfCons05a

rdfCons05a :: RDFFormula
rdfCons05a = makeFormula scopeex "rdfCons05a" 
                "ex:p  rdf:type rdf:Property ."
                
rdfStep05b :: RDFProofStep
rdfStep05b = makeRDFProofStep (getRule "rs_rdf:se")
             [rdfBase05, rdfCons05a, getAxiom "rs_rdf:a8"] 
             rdfGoal05

rdfGoal05 :: RDFFormula
rdfGoal05  = makeFormula scopeex "rdfGoal05" $
                "ex:s _:p _:n ."               `mappend`
                "_:p  rdf:type rdf:Property ." `mappend`
                "_:n  rdf:type rdf:List ."
                
rdfProof05 :: RDFProof
rdfProof05 = makeRDFProof rdfsContext rdfBase05 rdfGoal05
             [rdfStep05a, rdfStep05b]

--  Swap rdfProof05 proof steps:

rdfProof06 :: RDFProof
rdfProof06 = makeRDFProof rdfsContext rdfBase05 rdfGoal05
                [ rdfStep05b, rdfStep05a ]

--  Proof using rdfsr1 and rdfsub
--
--    ex:s1 ex:p1 "lll"
--    ex:s2 ex:p2 "lll"
--  =>
--    ex:s1 ex:p1 _:l
--    ex:s2 ex:p2 _:l
--    _:l   rdf:type rdfs:Literal

rdfBase07 :: RDFFormula
rdfBase07  = makeFormula scopeex "rdfBase07" $
                "ex:s1 ex:p1 \"lll\" ." `mappend`
                "ex:s2 ex:p2 \"lll\" ." `mappend`
                "ex:s3 ex:p3 \"mmm\" ."
                
rdfStep07a :: RDFProofStep                
rdfStep07a = makeRDFProofStep (getRule "rs_rdf:lg") [rdfBase07]  rdfCons07a

rdfCons07a :: RDFFormula
rdfCons07a = makeFormula scopeex "rdfCons07a" $
                "ex:s1 ex:p1 _:l ."              `mappend`
                "ex:s2 ex:p2 _:l ."              `mappend`
                "_:l rdf:_allocatedTo \"lll\" ." `mappend`
                "ex:s3 ex:p3 _:m ."              `mappend`
                "_:m rdf:_allocatedTo \"mmm\" ."
                
rdfStep07b :: RDFProofStep                
rdfStep07b = makeRDFProofStep (getRule "rs_rdfs:r1") [rdfCons07a]  rdfCons07b

rdfCons07b :: RDFFormula
rdfCons07b = makeFormula scopeex "rdfCons07a" $
                "_:l rdf:type rdfs:Literal ." `mappend`
                "_:m rdf:type rdfs:Literal ."

rdfStep07c :: RDFProofStep
rdfStep07c = makeRDFProofStep (getRule "rs_rdf:sub")
                [rdfCons07a,rdfCons07b] rdfGoal07

rdfGoal07 :: RDFFormula
rdfGoal07  = makeFormula scopeex "rdfGoal07" $
                "ex:s1 ex:p1 _:l ."           `mappend`
                "ex:s2 ex:p2 _:l ."           `mappend`
                "_:l rdf:type rdfs:Literal ."

rdfProof07 :: RDFProof
rdfProof07 = makeRDFProof rdfsContext rdfBase07 rdfGoal07
                [ rdfStep07a, rdfStep07b, rdfStep07c ]

--  Proof of:
--    rdf:_123 rdfs:supPropertyOf rdfs:member

rdfBase08 :: RDFFormula
rdfBase08  = makeFormula scopeex "rdfBase08" 
                "ex:s1 rdf:_123 ex:o ."
                
rdfStep08a :: RDFProofStep                
rdfStep08a = makeRDFProofStep (getRule "rs_rdfs:cp11") [rdfBase08]  rdfCons08a

rdfCons08a :: RDFFormula
rdfCons08a = makeFormula scopeex "rdfCons08a" 
                "rdf:_123 rdf:type rdfs:ContainerMembershipProperty ."
                
rdfStep08b :: RDFProofStep                
rdfStep08b = makeRDFProofStep (getRule "rs_rdfs:r12")  [rdfCons08a]  rdfGoal08

rdfGoal08 :: RDFFormula
rdfGoal08  = makeFormula scopeex "rdfCons08b" 
                "rdf:_123 rdfs:subPropertyOf rdfs:member ."
                
rdfProof08 :: RDFProof                
rdfProof08 = makeRDFProof rdfsContext rdfBase08 rdfGoal08
                [ rdfStep08a, rdfStep08b ]


--  Proof of:
--    ex:s ex:p "010"^^xsd:Integer .
--  =>
--    ex:s ex:p "10"^^xsd:Integer .
--    ex:s ex:p _:b
--    _:b rdf:type xsd:integer .

rdfAxiomIntDt :: RDFFormula
rdfAxiomIntDt = getContextAxiom
                    (makeNSScopedName (namespaceXsdType "integer") "dt")
                    nullFormula
                    allRulesets

rdfAxiom09 :: Test
rdfAxiom09 = testEq "rdfAxiom09" "xsd_integer:dt" $
                show (formName rdfAxiomIntDt)

rdfBase09 :: RDFFormula
rdfBase09  = makeFormula scopeex "rdfBase09" 
                "ex:s ex:p \"010\"^^xsd:integer ."
                
rdfStep09a :: RDFProofStep                
rdfStep09a = makeRDFProofStep (getRule "rs_rdfd:r2")
                [rdfAxiomIntDt,rdfBase09]  rdfCons09a
             
rdfCons09a :: RDFFormula
rdfCons09a = makeFormula scopeex "rdfCons09a" 
                "ex:s ex:p \"10\"^^xsd:integer ."
                
rdfStep09b :: RDFProofStep                
rdfStep09b = makeRDFProofStep (getRule "rs_rdf:lg")
                [rdfCons09a]  rdfCons09b
             
rdfCons09b :: RDFFormula
rdfCons09b = makeFormula scopeex "rdfCons09b" $
                "ex:s ex:p _:l ." `mappend`
                "_:l rdf:_allocatedTo \"10\"^^xsd:integer ."

rdfStep09c :: RDFProofStep
rdfStep09c = makeRDFProofStep (getRule "rs_rdfd:r1")
                [rdfAxiomIntDt,rdfCons09a,rdfCons09b]  rdfCons09c

rdfCons09c :: RDFFormula
rdfCons09c = makeFormula scopeex "rdfCons09c" 
                "_:l rdf:type xsd:integer ."
                
rdfStep09d :: RDFProofStep                
rdfStep09d = makeRDFProofStep (getRule "rs_rdf:sub")
                [rdfCons09a,rdfCons09b,rdfCons09c]  rdfGoal09
             
rdfGoal09 :: RDFFormula             
rdfGoal09  = makeFormula scopeex "rdfGoal09" $
                "ex:s ex:p  \"10\"^^xsd:integer ."           `mappend`
                "_:l rdf:_allocatedTo \"10\"^^xsd:integer ." `mappend`
                "_:l rdf:type xsd:integer ."

rdfProof09 :: RDFProof
rdfProof09 = makeRDFProof xsdintContext rdfBase09 rdfGoal09
                [ rdfStep09a, rdfStep09b, rdfStep09c, rdfStep09d ]

{- test data
p09t1 = fwdApply (getRule "rs_rdfd:r2")
    [(formExpr rdfAxiomIntDt),(formExpr rdfBase09)]
p09sh = putStrLn ("\n"++showProof "\n" rdfProof09++"\n")
-}


--  Proof of:
--    ex:s ex:p "abc" .
--    ex:s ex:p "def"^^xsd:string .
--  =>
--    ex:s ex:p "abc"^^xsd:string .
--    ex:s ex:p "def" .
--    xsd:string rdf:type rdfs:Datatype .

rdfAxiomStrDt :: RDFFormula
rdfAxiomStrDt = getContextAxiom
                    (makeNSScopedName (namespaceXsdType "string") "dt")
                    nullFormula
                    allRulesets

rdfAxiom10 :: Test
rdfAxiom10 = testEq "rdfAxiom10" "xsd_string:dt" $
                show (formName rdfAxiomStrDt)

rdfRule10 :: Test
rdfRule10 = testEq "rdfRule10" "xsd_string:ls" $
                show (ruleName (getRule "xsd_string:ls"))

rdfBase10 :: RDFFormula
rdfBase10  = makeFormula scopeex "rdfBase10" $
                "ex:s ex:p \"abc\" . " `mappend`
                "ex:s ex:p \"def\"^^xsd:string . "
                
rdfStep10a :: RDFProofStep                
rdfStep10a = makeRDFProofStep (getRule "xsd_string:ls")
                [rdfBase10]  rdfCons10a
             
rdfCons10a :: RDFFormula
rdfCons10a = makeFormula scopeex "rdfCons10a" 
                "ex:s ex:p \"abc\"^^xsd:string . "

rdfStep10b :: RDFProofStep
rdfStep10b = makeRDFProofStep (getRule "xsd_string:sl")
                [rdfBase10]  rdfCons10b

rdfCons10b :: RDFFormula
rdfCons10b = makeFormula scopeex "rdfCons10b" 
                "ex:s ex:p \"def\" . "

rdfStep10c :: RDFProofStep
rdfStep10c = makeRDFProofStep (getRule "rs_rdf:sub")
                [rdfCons10a,rdfCons10b,rdfAxiomStrDt] rdfGoal10

rdfGoal10 :: RDFFormula
rdfGoal10  = makeFormula scopeex "rdfGoal10" $
                "ex:s ex:p \"abc\"^^xsd:string . " `mappend`
                "ex:s ex:p \"def\" . "             `mappend`
                "xsd:string rdf:type rdfs:Datatype . "

rdfProof10 :: RDFProof
rdfProof10 = makeRDFProof xsdstrContext rdfBase10 rdfGoal10
                [ rdfStep10a, rdfStep10b, rdfStep10c ]

testRdf :: Test
testRdf = TestList
    [ testProofStep "rdfStep01a" True  [getRule "rs_rdf:r1"] [formExpr rdfBase01] rdfStep01a
    , testProof     "rdfProof01" True  rdfProof01
    --   Really should have support for scoped bnodes
    , testProof     "rdfProof02" True  rdfProof02
    , testProof     "rdfProof03" False rdfProof03
    , testProof     "rdfProof04" True  rdfProof04
    , testProof     "rdfProof05" True  rdfProof05
    , testProof     "rdfProof06" False rdfProof06
    , testProof     "rdfProof07" True  rdfProof07
    , testProof     "rdfProof08" True  rdfProof08
    , rdfAxiom09
    , testProof     "rdfProof09" True  rdfProof09
    , rdfAxiom10, rdfRule10
    , testProof     "rdfProof10" True  rdfProof10
    {-
    , TestCase $ putStrLn ("\n"++showProof "\n" rdfProof01)
    , TestCase $ putStrLn ("\n"++showProof "\n" rdfProof05)
    -}
    ]

------------------------------------------------------------
--  Full test suite, main program,
--  and useful expressions for interactive use
------------------------------------------------------------

allTests :: [TF.Test]
allTests =
  [ conv "rules" testRules
  , conv "rdf" testRdf
  ]

main :: IO ()
main = TF.defaultMain allTests

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011, 2012, 2013, 2014, 2021 Douglas Burke
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
