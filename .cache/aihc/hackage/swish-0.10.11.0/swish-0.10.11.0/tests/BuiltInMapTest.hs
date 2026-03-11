{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  BuiltInMapTest
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012, 2013 Douglas Burke
--  License     :  GPL V2
-- 
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  OverloadedStrings
-- 
--  This module contains test cases for accessing built-in variable
--  binding modifiers.
-- 
--------------------------------------------------------------------------------

module Main where

import qualified Test.Framework as TF

import qualified Data.Map as M

import Swish.Namespace (makeNSScopedName)
import Swish.Ruleset (getMaybeContextAxiom, getMaybeContextRule)

import Swish.RDF.BuiltIn
    ( findRDFOpenVarBindingModifier
    , findRDFDatatype
    , rdfRulesetMap
    , allRulesets
    )

import Swish.RDF.Datatype.XSD.Integer (typeNameXsdInteger, namespaceXsdInteger)

import Swish.RDF.Vocabulary
    ( swishName
    , scopeRDF
    , scopeRDFS
    , scopeRDFD
    , namespaceXsdType
    )

import Test.HUnit
    ( Test(TestCase,TestList)
    , assertEqual
    )

import TestHelpers (conv, testJust)

------------------------------------------------------------
--  Test finding built-in variable binding modifiers
------------------------------------------------------------

testVarMod01, testVarMod02, testVarMod03, testVarMod04, 
  testVarMod05, testVarMod06, testVarMod07 :: Test

testVarMod01 = testJust "testVarMod01" $
    findRDFOpenVarBindingModifier (swishName "rdfVarBindingUriRef")
testVarMod02 = testJust "testVarMod02" $
    findRDFOpenVarBindingModifier (swishName "rdfVarBindingDatatyped")
testVarMod03 = testJust "testVarMod03" $
    findRDFOpenVarBindingModifier (swishName "varFilterNE")
testVarMod04 = testJust "testVarMod04" $
    findRDFOpenVarBindingModifier (swishName "nullVarBindingModify")
testVarMod05 = testJust "testVarMod05" $
    findRDFOpenVarBindingModifier (makeNSScopedName namespaceXsdInteger "abs")
testVarMod06 = testJust "testVarMod06" $
    findRDFOpenVarBindingModifier (makeNSScopedName namespaceXsdInteger "divmod")
testVarMod07 = testJust "testVarMod07" $
    findRDFOpenVarBindingModifier (makeNSScopedName namespaceXsdInteger "ge")

testVarModSuite :: Test
testVarModSuite = TestList
    [ testVarMod01, testVarMod02, testVarMod03, testVarMod04
    , testVarMod05, testVarMod06, testVarMod07
      -- the following just exposes a few "edge" cases (a Show instance
      -- and using the namespace part of the swish namespace)
    , TestCase (assertEqual "show:rdfVarBindingUriRef" 
                (Just "swish:rdfVarBindingUriRef")
                $ fmap show (findRDFOpenVarBindingModifier (swishName "rdfVarBindingUriRef")))
    ]

------------------------------------------------------------
--  Test finding built-in datatypes
------------------------------------------------------------

testDatatype01 :: Test
testDatatype01 = testJust "testDatatype01" $ findRDFDatatype typeNameXsdInteger

testDatatypeSuite :: Test
testDatatypeSuite = TestList
    [ testDatatype01
    ]

------------------------------------------------------------
--  Test finding built-in rulesets
------------------------------------------------------------

testRuleset01 :: Test
testRuleset01 = testJust "testRuleset01" $
    M.lookup scopeRDF rdfRulesetMap

testRulesetSuite :: Test
testRulesetSuite = TestList
    [ testRuleset01
    ]

------------------------------------------------------------
--  Test finding arbitrary axioms and rules
------------------------------------------------------------

testFindAxiom01, testFindAxiom02, testFindAxiom03 :: Test

testFindAxiom01 = testJust "testFindAxiom01" $
    getMaybeContextAxiom (makeNSScopedName scopeRDF "a1") allRulesets
testFindAxiom02 = testJust "testFindAxiom02" $
    getMaybeContextAxiom (makeNSScopedName scopeRDFS "a01") allRulesets
testFindAxiom03 = testJust "testFindAxiom03" $
    getMaybeContextAxiom (makeNSScopedName (namespaceXsdType "integer") "dt")
        allRulesets

testFindAxiomSuite :: Test
testFindAxiomSuite = TestList
    [ testFindAxiom01, testFindAxiom02, testFindAxiom03
    ]

testFindRule01, testFindRule02, testFindRule03, testFindRule04 :: Test

testFindRule01 = testJust "testFindRule01" $
    getMaybeContextRule (makeNSScopedName scopeRDF "r1") allRulesets
testFindRule02 = testJust "testFindRule02" $
    getMaybeContextRule (makeNSScopedName scopeRDFS "r1") allRulesets
testFindRule03 = testJust "testFindRule03" $
    getMaybeContextRule (makeNSScopedName scopeRDFD "r1") allRulesets
testFindRule04 = testJust "testFindRule04" $
    getMaybeContextRule (makeNSScopedName (namespaceXsdType "integer") "Abs")
        allRulesets

testFindRuleSuite :: Test
testFindRuleSuite = TestList
    [ testFindRule01, testFindRule02, testFindRule03, testFindRule04
    ]

------------------------------------------------------------
--  All tests
------------------------------------------------------------

allTests :: [TF.Test]
allTests =
  [ conv "VarMod" testVarModSuite
  , conv "Datatype" testDatatypeSuite
  , conv "Ruleset" testRulesetSuite
  , conv "FindAxiom" testFindAxiomSuite
  , conv "FindRule" testFindRuleSuite
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
