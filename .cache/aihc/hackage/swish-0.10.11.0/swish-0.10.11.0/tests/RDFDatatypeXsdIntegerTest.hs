{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  RDFDatatypeXsdIntegerTest
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012, 2013, 2014 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  CPP, OverloadedStrings
--
--  This module contains test cases for variable binding values and
--  variable binding modifier values.
--
--------------------------------------------------------------------------------

module Main where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as B

import qualified Test.Framework as TF

import Swish.Datatype
    ( typeName, typeRules, typeMkRules
    , getTypeAxiom, getTypeRule
    , DatatypeVal(..)
    , getDTMod
    , DatatypeMap(..)
    , DatatypeMod(..)
    , nullDatatypeMod
    )
import Swish.Namespace (ScopedName, getNamespaceURI, makeScopedName, makeNSScopedName, namespaceToBuilder)
import Swish.QName (LName)
import Swish.Rule    (Formula(..), Rule(..), nullFormula, nullRule)
import Swish.Ruleset (Ruleset(..), getRulesetRule)
import Swish.VarBinding (makeVarBinding)

import Swish.RDF.ClassRestrictionRule (falseGraphStr)
import Swish.RDF.Datatype (RDFDatatypeMod, applyRDFDatatypeMod)

import Swish.RDF.Datatype.XSD.Integer
    ( rdfDatatypeXsdInteger
    , rdfDatatypeValXsdInteger
    , typeNameXsdInteger, namespaceXsdInteger
    , axiomsXsdInteger, rulesXsdInteger
    )

import Swish.RDF.VarBinding (RDFVarBinding)

import Swish.RDF.Ruleset (RDFRule, makeRDFGraphFromN3Builder)
import Swish.RDF.Graph (RDFLabel(..), RDFGraph)

import Swish.RDF.Vocabulary (namespaceDefault, namespaceRDF, namespaceRDFD, namespaceXSD)

import Data.List (intersperse)
import Data.Maybe (fromMaybe, fromJust)

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Data.Monoid (Monoid(..))
#endif

import Network.URI (URI, parseURI)

import Test.HUnit
    ( Test(TestCase,TestList)
      , assertFailure
    )
import TestHelpers (conv, testEq, testElem, testEqv, testEqv2)

------------------------------------------------------------
--  Misc values
------------------------------------------------------------

toURI :: String -> URI
toURI = fromJust . parseURI

xsdIntName :: LName -> ScopedName
xsdIntName = makeNSScopedName namespaceXsdInteger 

axiomXsdIntegerDT :: ScopedName
axiomXsdIntegerDT       = xsdIntName "dt"

ruleXsdIntegerAbs, ruleXsdIntegerNeg, ruleXsdIntegerSum,
  ruleXsdIntegerDiff, ruleXsdIntegerProd, ruleXsdIntegerDivMod,
  ruleXsdIntegerPower, ruleXsdIntegerEq, ruleXsdIntegerNe, 
  ruleXsdIntegerLt, ruleXsdIntegerLe, ruleXsdIntegerGt,
  ruleXsdIntegerGe :: ScopedName
ruleXsdIntegerAbs       = xsdIntName "Abs"
ruleXsdIntegerNeg       = xsdIntName "Neg"
ruleXsdIntegerSum       = xsdIntName "Sum"
ruleXsdIntegerDiff      = xsdIntName "Diff"
ruleXsdIntegerProd      = xsdIntName "Prod"
ruleXsdIntegerDivMod    = xsdIntName "DivMod"
ruleXsdIntegerPower     = xsdIntName "Power"
ruleXsdIntegerEq        = xsdIntName "Eq"
ruleXsdIntegerNe        = xsdIntName "Ne"
ruleXsdIntegerLt        = xsdIntName "Lt"
ruleXsdIntegerLe        = xsdIntName "Le"
ruleXsdIntegerGt        = xsdIntName "Gt"
ruleXsdIntegerGe        = xsdIntName "Ge"

------------------------------------------------------------
--  Basic rdfDatatypeXsdInteger tests
------------------------------------------------------------

getXsdIntegerAxiom :: ScopedName -> Formula RDFGraph
getXsdIntegerAxiom scopnam =
    fromMaybe nullFormula $ getTypeAxiom scopnam rdfDatatypeXsdInteger

getXsdIntegerRule :: ScopedName -> Rule RDFGraph
getXsdIntegerRule scopnam =
    fromMaybe nullRule $ getTypeRule scopnam rdfDatatypeXsdInteger

getXsdIntegerDTmod :: ScopedName -> DatatypeMod Integer RDFLabel RDFLabel
getXsdIntegerDTmod scopnam =
    fromMaybe nullDatatypeMod $ getDTMod scopnam rdfDatatypeValXsdInteger

testDatatypeSuite :: Test
testDatatypeSuite = 
  TestList
  [ testEq  "testDatatype01" typeNameXsdInteger $
    typeName rdfDatatypeXsdInteger
  , testEq  "testDatatype02" namespaceXsdInteger $
    rsNamespace xsdIntRules
  , testEqv "testDatatype03" axiomsXsdInteger $
    rsAxioms xsdIntRules
  , testEqv "testDatatype04" rulesXsdInteger $
    rsRules xsdIntRules
  , testEq "testDatatype05" axiomXsdIntegerDT $
    formName (getXsdIntegerAxiom axiomXsdIntegerDT)
  , testEq "testDatatype06" ruleXsdIntegerAbs $
    ruleName (getXsdIntegerRule ruleXsdIntegerAbs)
  ]
  
------------------------------------------------------------
--  Basic rdfDatatypeValXsdInteger tests
------------------------------------------------------------

testDatatypeValSuite :: Test
testDatatypeValSuite = 
  TestList
  [ testEq  "testDatatypeVal01" (Just 123) $
       mapL2V (tvalMap rdfDatatypeValXsdInteger) "123"
  , testEq  "testDatatypeVal02" (Just 0) $
       mapL2V (tvalMap rdfDatatypeValXsdInteger) "0"
  , testEq  "testDatatypeVal03" (Just 456) $
       mapL2V (tvalMap rdfDatatypeValXsdInteger) "+000456"
  , testEq  "testDatatypeVal04" (Just (-987)) $
       mapL2V (tvalMap rdfDatatypeValXsdInteger) "-0987"
  , testEq  "testDatatypeVal05" Nothing $
       mapL2V (tvalMap rdfDatatypeValXsdInteger) "11x2"
  , testEq  "testDatatypeVal06" Nothing $
       mapL2V (tvalMap rdfDatatypeValXsdInteger) " 321"
  , testEq  "testDatatypeVal07" Nothing $
       mapL2V (tvalMap rdfDatatypeValXsdInteger) "321 "

  , testEq  "testDatatypeVal11" (Just "123") $
       mapV2L (tvalMap rdfDatatypeValXsdInteger) 123
  , testEq  "testDatatypeVal12" (Just "-987") $
       mapV2L (tvalMap rdfDatatypeValXsdInteger) (-987)

  , testElem "testDatatypeVal21" dmodXsdIntegerAbs $
       map dmName (tvalMod rdfDatatypeValXsdInteger)
  , testEq "testDatatypeVal22" dmodXsdIntegerAbs $
       dmName (getXsdIntegerDTmod dmodXsdIntegerAbs)
  ]

------------------------------------------------------------
--  Variable binding modifier tests
------------------------------------------------------------

dmodXsdIntegerAbs, dmodXsdIntegerNeg, dmodXsdIntegerSum, 
  dmodXsdIntegerDiff, dmodXsdIntegerProd, dmodXsdIntegerDivMod, 
  dmodXsdIntegerPower, dmodXsdIntegerEq, dmodXsdIntegerNe, 
  dmodXsdIntegerLt, dmodXsdIntegerLe, dmodXsdIntegerGt, 
  dmodXsdIntegerGe :: ScopedName

dmodXsdIntegerAbs    = xsdIntName "abs"
dmodXsdIntegerNeg    = xsdIntName "neg"
dmodXsdIntegerSum    = xsdIntName "sum"
dmodXsdIntegerDiff   = xsdIntName "diff"
dmodXsdIntegerProd   = xsdIntName "prod"
dmodXsdIntegerDivMod = xsdIntName "divmod"
dmodXsdIntegerPower  = xsdIntName "power"
dmodXsdIntegerEq     = xsdIntName "eq"
dmodXsdIntegerNe     = xsdIntName "ne"
dmodXsdIntegerLt     = xsdIntName "lt"
dmodXsdIntegerLe     = xsdIntName "le"
dmodXsdIntegerGt     = xsdIntName "gt"
dmodXsdIntegerGe     = xsdIntName "ge"

testVmodN :: [RDFLabel]
    -> String -> Maybe (RDFDatatypeMod Integer)
    -> [RDFVarBinding] -> [RDFVarBinding]
    -> Test
testVmodN vars lab (Just dmod) ibinds obinds =
    testEqv lab obinds $
        applyRDFDatatypeMod rdfDatatypeValXsdInteger dmod vars ibinds
testVmodN _ lab Nothing _ _ = TestCase $
    assertFailure $ "testVmodN:"++lab++", null variable binding modifier"

testVmod2, testVmod3, testVmod4 :: 
  String -> Maybe (RDFDatatypeMod Integer)
  -> [RDFVarBinding] -> [RDFVarBinding]
  -> Test
testVmod2 = testVmodN [Var "a", Var "b"]
testVmod3 = testVmodN [Var "a", Var "b", Var "c"]
testVmod4 = testVmodN [Var "a", Var "b", Var "c", Var "d"]

--  make various kinds of RDF variable bindings

rdfVR :: (String, ScopedName) -> (RDFLabel, RDFLabel)
rdfVR (v,u) = (Var v,Res u)                     -- (Variable,Resource)

rdfVB :: (String, String) -> (RDFLabel, RDFLabel)
rdfVB (v,b) = (Var v,Blank b)                   -- (Variable,Blank)

rdfVL :: (String, String) -> (RDFLabel, RDFLabel)
rdfVL (v,l) = (Var v, Lit (T.pack l))             -- (Variable,Untyped literal)

rdfVI :: (String, String) -> (RDFLabel, RDFLabel)
rdfVI (v,l) = (Var v, TypedLit (T.pack l) typeNameXsdInteger)
                                                -- (Variable,Integer literal)

makeBVR :: [(String,ScopedName)] -> RDFVarBinding
makeBVR nls = makeVarBinding $ map rdfVR nls

makeBVB :: [(String,String)] -> RDFVarBinding
makeBVB nls = makeVarBinding $ map rdfVB nls

makeBVI :: [(String,String)] -> RDFVarBinding
makeBVI nls = makeVarBinding $ map rdfVI nls

makeBVL :: [(String,String)] -> RDFVarBinding
makeBVL nls = makeVarBinding $ map rdfVL nls

--  Test null modifier

testVarModify00 :: Test
testVarModify00 = testVmod2  "testVarModify00"
                    (Just nullDatatypeMod)
                    [makeBVI [("a","123")]]
                    [makeBVI [("a","123")]]

--  Tests for xsd_integer:abs

testVarModifyAbs01, testVarModifyAbs02, testVarModifyAbs03,
  testVarModifyAbs04, testVarModifyAbs05, testVarModifyAbs06,
  testVarModifyAbs07, testVarModifyAbs08, testVarModifyAbs09,
  testVarModifyAbs10 :: Test
testVarModifyAbs01 = testVmod2  "testVarModifyAbs01"
                    (getDTMod dmodXsdIntegerAbs rdfDatatypeValXsdInteger)
                    [makeBVI [("b","123")]]
                    [makeBVI [("a","123"),("b","123")]]

testVarModifyAbs02 = testVmod2  "testVarModifyAbs02"
                    (getDTMod dmodXsdIntegerAbs rdfDatatypeValXsdInteger)
                    [makeBVI [("b","-123")]]
                    [makeBVI [("a","123"),("b","-123")]]

testVarModifyAbs03 = testVmod2  "testVarModifyAbs03"
                    (getDTMod dmodXsdIntegerAbs rdfDatatypeValXsdInteger)
                    [makeBVI [("a","123"),("b","123")]]
                    [makeBVI [("a","123"),("b","123")]]

testVarModifyAbs04 = testVmod2  "testVarModifyAbs04"
                    (getDTMod dmodXsdIntegerAbs rdfDatatypeValXsdInteger)
                    [makeBVI [("a","123"),("b","-123")]]
                    [makeBVI [("a","123"),("b","-123")]]

testVarModifyAbs05 = testVmod2  "testVarModifyAbs05"
                    (getDTMod dmodXsdIntegerAbs rdfDatatypeValXsdInteger)
                    [makeBVI [("a","-123"),("b","123")]]
                    []

testVarModifyAbs06 = testVmod2  "testVarModifyAbs06"
                    (getDTMod dmodXsdIntegerAbs rdfDatatypeValXsdInteger)
                    [makeBVI [("a","123"),("b","456")]]
                    []

testVarModifyAbs07 = testVmod2  "testVarModifyAbs07"
                    (getDTMod dmodXsdIntegerAbs rdfDatatypeValXsdInteger)
                    [makeBVI [("c","123")]]
                    []

testVarModifyAbs08 = testVmod2  "testVarModifyAbs08"
                    (getDTMod dmodXsdIntegerAbs rdfDatatypeValXsdInteger)
                    [makeBVL [("b","123")]]
                    []

testVarModifyAbs09 = testVmod2  "testVarModifyAbs09"
                    (getDTMod dmodXsdIntegerAbs rdfDatatypeValXsdInteger)
                    [makeBVR [("b",makeScopedName Nothing (toURI "http://ex.org/") "123")]]
                    []

testVarModifyAbs10 = testVmod2  "testVarModifyAbs10"
                    (getDTMod dmodXsdIntegerAbs rdfDatatypeValXsdInteger)
                    [makeBVB [("b","123")]]
                    []

--  Tests for xsd_integer:neg

testVarModifyNeg01, testVarModifyNeg02, testVarModifyNeg03,
  testVarModifyNeg04, testVarModifyNeg05 :: Test

testVarModifyNeg01 = testVmod2  "testVarModifyNeg01"
                    (getDTMod dmodXsdIntegerNeg rdfDatatypeValXsdInteger)
                    [makeBVI [("a","123"),("b","-123")]]
                    [makeBVI [("a","123"),("b","-123")]]

testVarModifyNeg02 = testVmod2  "testVarModifyNeg02"
                    (getDTMod dmodXsdIntegerNeg rdfDatatypeValXsdInteger)
                    [makeBVI [("a","-123"),("b","123")]]
                    [makeBVI [("a","-123"),("b","123")]]

testVarModifyNeg03 = testVmod2  "testVarModifyNeg03"
                    (getDTMod dmodXsdIntegerNeg rdfDatatypeValXsdInteger)
                    [makeBVI [("a","123"),("b","123")]]
                    []

testVarModifyNeg04 = testVmod2  "testVarModifyNeg04"
                    (getDTMod dmodXsdIntegerNeg rdfDatatypeValXsdInteger)
                    [makeBVI [("b","123")]]
                    [makeBVI [("a","-123"),("b","123")]]

testVarModifyNeg05 = testVmod2  "testVarModifyNeg05"
                    (getDTMod dmodXsdIntegerNeg rdfDatatypeValXsdInteger)
                    [makeBVI [("a","-123")]]
                    [makeBVI [("a","-123"),("b","123")]]

--  Tests for xsd_integer:sum

testVarModifySum01, testVarModifySum02, testVarModifySum03,
  testVarModifySum04, testVarModifySum05 :: Test

testVarModifySum01 = testVmod3  "testVarModifySum01"
                    (getDTMod dmodXsdIntegerSum rdfDatatypeValXsdInteger)
                    [makeBVI [("a","33"),("b","22"),("c","11")]]
                    [makeBVI [("a","33"),("b","22"),("c","11")]]

testVarModifySum02 = testVmod3  "testVarModifySum02"
                    (getDTMod dmodXsdIntegerSum rdfDatatypeValXsdInteger)
                    [makeBVI [("b","22"),("c","11")]]
                    [makeBVI [("a","33"),("b","22"),("c","11")]]

testVarModifySum03 = testVmod3  "testVarModifySum03"
                    (getDTMod dmodXsdIntegerSum rdfDatatypeValXsdInteger)
                    [makeBVI [("a","33"),("c","11")]]
                    [makeBVI [("a","33"),("b","22"),("c","11")]]

testVarModifySum04 = testVmod3  "testVarModifySum04"
                    (getDTMod dmodXsdIntegerSum rdfDatatypeValXsdInteger)
                    [makeBVI [("a","33"),("b","22")]]
                    [makeBVI [("a","33"),("b","22"),("c","11")]]

testVarModifySum05 = testVmod3  "testVarModifySum05"
                    (getDTMod dmodXsdIntegerSum rdfDatatypeValXsdInteger)
                    [makeBVI [("a","44"),("b","22"),("c","11")]]
                    []

--  Tests for xsd_integer:diff

testVarModifyDiff01, testVarModifyDiff02, testVarModifyDiff03,
  testVarModifyDiff04, testVarModifyDiff05 :: Test

testVarModifyDiff01 = testVmod3  "testVarModifyDiff01"
                    (getDTMod dmodXsdIntegerDiff rdfDatatypeValXsdInteger)
                    [makeBVI [("a","11"),("b","33"),("c","22")]]
                    [makeBVI [("a","11"),("b","33"),("c","22")]]

testVarModifyDiff02 = testVmod3  "testVarModifyDiff02"
                    (getDTMod dmodXsdIntegerDiff rdfDatatypeValXsdInteger)
                    [makeBVI [("b","33"),("c","22")]]
                    [makeBVI [("a","11"),("b","33"),("c","22")]]

testVarModifyDiff03 = testVmod3  "testVarModifyDiff03"
                    (getDTMod dmodXsdIntegerDiff rdfDatatypeValXsdInteger)
                    [makeBVI [("a","11"),("c","22")]]
                    [makeBVI [("a","11"),("b","33"),("c","22")]]

testVarModifyDiff04 = testVmod3  "testVarModifyDiff04"
                    (getDTMod dmodXsdIntegerDiff rdfDatatypeValXsdInteger)
                    [makeBVI [("a","11"),("b","33")]]
                    [makeBVI [("a","11"),("b","33"),("c","22")]]

testVarModifyDiff05 = testVmod3  "testVarModifyDiff05"
                    (getDTMod dmodXsdIntegerDiff rdfDatatypeValXsdInteger)
                    [makeBVI [("a","11"),("b","44"),("c","22")]]
                    []

--  Tests for xsd_integer:prod
--
--  Note:   product can also be used to test if a value is
--          an exact multiple of some other.

testVarModifyProd01, testVarModifyProd02, testVarModifyProd03,
  testVarModifyProd04, testVarModifyProd05,
  testVarModifyProd06 :: Test

testVarModifyProd01 = testVmod3  "testVarModifyProd01"
                    (getDTMod dmodXsdIntegerProd rdfDatatypeValXsdInteger)
                    [makeBVI [("a","6"),("b","2"),("c","3")]]
                    [makeBVI [("a","6"),("b","2"),("c","3")]]

testVarModifyProd02 = testVmod3  "testVarModifyProd02"
                    (getDTMod dmodXsdIntegerProd rdfDatatypeValXsdInteger)
                    [makeBVI [("b","2"),("c","3")]]
                    [makeBVI [("a","6"),("b","2"),("c","3")]]

testVarModifyProd03 = testVmod3  "testVarModifyProd03"
                    (getDTMod dmodXsdIntegerProd rdfDatatypeValXsdInteger)
                    [makeBVI [("a","6"),("c","3")]]
                    [makeBVI [("a","6"),("b","2"),("c","3")]]

testVarModifyProd04 = testVmod3  "testVarModifyProd04"
                    (getDTMod dmodXsdIntegerProd rdfDatatypeValXsdInteger)
                    [makeBVI [("a","6"),("c","3")]]
                    [makeBVI [("a","6"),("b","2"),("c","3")]]

testVarModifyProd05 = testVmod3  "testVarModifyProd05"
                    (getDTMod dmodXsdIntegerProd rdfDatatypeValXsdInteger)
                    [makeBVI [("a","7"),("b","2"),("c","3")]]
                    []

testVarModifyProd06 = testVmod3  "testVarModifyProd06"
                    (getDTMod dmodXsdIntegerProd rdfDatatypeValXsdInteger)
                    [makeBVI [("a","7"),("b","2")]]
                    []

--  Tests for xsd_integer:divmod
--
--  Note:   truncates downwards, so remainder is same sign as divisor
--          cf. Haskell divMod function.

testVarModifyDivMod01, testVarModifyDivMod02, testVarModifyDivMod03,
  testVarModifyDivMod04, testVarModifyDivMod05,
  testVarModifyDivMod06, testVarModifyDivMod07 :: Test

testVarModifyDivMod01 = testVmod4  "testVarModifyDivMod01"
                    (getDTMod dmodXsdIntegerDivMod rdfDatatypeValXsdInteger)
                    [makeBVI [("a","2"),("b","1"),("c","7"),("d","3")]]
                    [makeBVI [("a","2"),("b","1"),("c","7"),("d","3")]]

testVarModifyDivMod02 = testVmod4  "testVarModifyDivMod02"
                    (getDTMod dmodXsdIntegerDivMod rdfDatatypeValXsdInteger)
                    [makeBVI [("c","7"),("d","3")]]
                    [makeBVI [("a","2"),("b","1"),("c","7"),("d","3")]]

testVarModifyDivMod03 = testVmod4  "testVarModifyDivMod03"
                    (getDTMod dmodXsdIntegerDivMod rdfDatatypeValXsdInteger)
                    [makeBVI [("c","-7"),("d","3")]]
                    [makeBVI [("a","-3"),("b","2"),("c","-7"),("d","3")]]

testVarModifyDivMod04 = testVmod4  "testVarModifyDivMod04"
                    (getDTMod dmodXsdIntegerDivMod rdfDatatypeValXsdInteger)
                    [makeBVI [("c","7"),("d","-3")]]
                    [makeBVI [("a","-3"),("b","-2"),("c","7"),("d","-3")]]

testVarModifyDivMod05 = testVmod4  "testVarModifyDivMod05"
                    (getDTMod dmodXsdIntegerDivMod rdfDatatypeValXsdInteger)
                    [makeBVI [("c","-7"),("d","-3")]]
                    [makeBVI [("a","2"),("b","-1"),("c","-7"),("d","-3")]]

testVarModifyDivMod06 = testVmod4  "testVarModifyDivMod06"
                    (getDTMod dmodXsdIntegerDivMod rdfDatatypeValXsdInteger)
                    [makeBVI [("a","2"),("b","5"),("c","7"),("d","3")]]
                    []

testVarModifyDivMod07 = testVmod4  "testVarModifyDivMod07"
                    (getDTMod dmodXsdIntegerDivMod rdfDatatypeValXsdInteger)
                    [makeBVI [("a","2"),("b","1"),("d","3")]]
                    []

--  Tests for xsd_integer:power

testVarModifyPower01, testVarModifyPower02, testVarModifyPower03,
  testVarModifyPower04, testVarModifyPower05,
  testVarModifyPower06, testVarModifyPower07,
  testVarModifyPower08 :: Test

testVarModifyPower01 = testVmod3  "testVarModifyPower01"
                    (getDTMod dmodXsdIntegerPower rdfDatatypeValXsdInteger)
                    [makeBVI [("a","8"),("b","2"),("c","3")]]
                    [makeBVI [("a","8"),("b","2"),("c","3")]]

testVarModifyPower02 = testVmod3  "testVarModifyPower02"
                    (getDTMod dmodXsdIntegerPower rdfDatatypeValXsdInteger)
                    [makeBVI [("b","2"),("c","3")]]
                    [makeBVI [("a","8"),("b","2"),("c","3")]]

testVarModifyPower03 = testVmod3  "testVarModifyPower03"
                    (getDTMod dmodXsdIntegerPower rdfDatatypeValXsdInteger)
                    [makeBVI [("a","8"),("c","3")]]
                    []

testVarModifyPower04 = testVmod3  "testVarModifyPower04"
                    (getDTMod dmodXsdIntegerPower rdfDatatypeValXsdInteger)
                    [makeBVI [("a","8"),("b","2")]]
                    []

testVarModifyPower05 = testVmod3  "testVarModifyPower05"
                    (getDTMod dmodXsdIntegerPower rdfDatatypeValXsdInteger)
                    [makeBVI [("a","8"),("b","3"),("c","2")]]
                    []

testVarModifyPower06 = testVmod3  "testVarModifyPower06"
                    (getDTMod dmodXsdIntegerPower rdfDatatypeValXsdInteger)
                    [makeBVI [("b","55"),("c","0")]]
                    [makeBVI [("a","1"),("b","55"),("c","0")]]

testVarModifyPower07 = testVmod3  "testVarModifyPower07"
                    (getDTMod dmodXsdIntegerPower rdfDatatypeValXsdInteger)
                    [makeBVI [("b","-2"),("c","3")]]
                    [makeBVI [("a","-8"),("b","-2"),("c","3")]]

testVarModifyPower08 = testVmod3  "testVarModifyPower08"
                    (getDTMod dmodXsdIntegerPower rdfDatatypeValXsdInteger)
                    [makeBVI [("b","55"),("c","-2")]]
                    []

--  Tests for xsd_integer:eq

testVarModifyEq01, testVarModifyEq02, testVarModifyEq03,
  testVarModifyEq04, testVarModifyEq05 :: Test

testVarModifyEq01 = testVmod2  "testVarModifyEq01"
                    (getDTMod dmodXsdIntegerEq rdfDatatypeValXsdInteger)
                    [makeBVI [("a","100"),("b","100")]]
                    [makeBVI [("a","100"),("b","100")]]

testVarModifyEq02 = testVmod2  "testVarModifyEq02"
                    (getDTMod dmodXsdIntegerEq rdfDatatypeValXsdInteger)
                    [makeBVI [("a","99"),("b","100")]]
                    []

testVarModifyEq03 = testVmod2  "testVarModifyEq03"
                    (getDTMod dmodXsdIntegerEq rdfDatatypeValXsdInteger)
                    [makeBVI [("a","-99"),("b","-100")]]
                    []

testVarModifyEq04 = testVmod2  "testVarModifyEq04"
                    (getDTMod dmodXsdIntegerEq rdfDatatypeValXsdInteger)
                    [makeBVI [("b","100")]]
                    []

testVarModifyEq05 = testVmod2  "testVarModifyEq05"
                    (getDTMod dmodXsdIntegerEq rdfDatatypeValXsdInteger)
                    [makeBVI [("a","100")]]
                    []

--  Tests for xsd_integer:ne

testVarModifyNe01, testVarModifyNe02, testVarModifyNe03,
  testVarModifyNe04, testVarModifyNe05 :: Test

testVarModifyNe01 = testVmod2  "testVarModifyNe01"
                    (getDTMod dmodXsdIntegerNe rdfDatatypeValXsdInteger)
                    [makeBVI [("a","100"),("b","100")]]
                    []

testVarModifyNe02 = testVmod2  "testVarModifyNe02"
                    (getDTMod dmodXsdIntegerNe rdfDatatypeValXsdInteger)
                    [makeBVI [("a","99"),("b","100")]]
                    [makeBVI [("a","99"),("b","100")]]

testVarModifyNe03 = testVmod2  "testVarModifyNe03"
                    (getDTMod dmodXsdIntegerNe rdfDatatypeValXsdInteger)
                    [makeBVI [("a","-99"),("b","-100")]]
                    [makeBVI [("a","-99"),("b","-100")]]

testVarModifyNe04 = testVmod2  "testVarModifyNe04"
                    (getDTMod dmodXsdIntegerNe rdfDatatypeValXsdInteger)
                    [makeBVI [("b","100")]]
                    []

testVarModifyNe05 = testVmod2  "testVarModifyNe05"
                    (getDTMod dmodXsdIntegerNe rdfDatatypeValXsdInteger)
                    [makeBVI [("a","100")]]
                    []

--  Tests for xsd_integer:lt

testVarModifyLt01, testVarModifyLt02, testVarModifyLt03,
  testVarModifyLt04, testVarModifyLt05 :: Test

testVarModifyLt01 = testVmod2  "testVarModifyLt01"
                    (getDTMod dmodXsdIntegerLt rdfDatatypeValXsdInteger)
                    [makeBVI [("a","100"),("b","100")]]
                    []

testVarModifyLt02 = testVmod2  "testVarModifyLt02"
                    (getDTMod dmodXsdIntegerLt rdfDatatypeValXsdInteger)
                    [makeBVI [("a","99"),("b","100")]]
                    [makeBVI [("a","99"),("b","100")]]

testVarModifyLt03 = testVmod2  "testVarModifyLt03"
                    (getDTMod dmodXsdIntegerLt rdfDatatypeValXsdInteger)
                    [makeBVI [("a","-99"),("b","-100")]]
                    []

testVarModifyLt04 = testVmod2  "testVarModifyLt04"
                    (getDTMod dmodXsdIntegerLt rdfDatatypeValXsdInteger)
                    [makeBVI [("b","100")]]
                    []

testVarModifyLt05 = testVmod2  "testVarModifyLt05"
                    (getDTMod dmodXsdIntegerLt rdfDatatypeValXsdInteger)
                    [makeBVI [("a","100")]]
                    []

--  Tests for xsd_integer:le

testVarModifyLe01, testVarModifyLe02, testVarModifyLe03,
  testVarModifyLe04, testVarModifyLe05 :: Test

testVarModifyLe01 = testVmod2  "testVarModifyLe01"
                    (getDTMod dmodXsdIntegerLe rdfDatatypeValXsdInteger)
                    [makeBVI [("a","100"),("b","100")]]
                    [makeBVI [("a","100"),("b","100")]]

testVarModifyLe02 = testVmod2  "testVarModifyLe02"
                    (getDTMod dmodXsdIntegerLe rdfDatatypeValXsdInteger)
                    [makeBVI [("a","99"),("b","100")]]
                    [makeBVI [("a","99"),("b","100")]]

testVarModifyLe03 = testVmod2  "testVarModifyLe03"
                    (getDTMod dmodXsdIntegerLe rdfDatatypeValXsdInteger)
                    [makeBVI [("a","-99"),("b","-100")]]
                    []

testVarModifyLe04 = testVmod2  "testVarModifyLe04"
                    (getDTMod dmodXsdIntegerLe rdfDatatypeValXsdInteger)
                    [makeBVI [("b","100")]]
                    []

testVarModifyLe05 = testVmod2  "testVarModifyLe05"
                    (getDTMod dmodXsdIntegerLe rdfDatatypeValXsdInteger)
                    [makeBVI [("a","100")]]
                    []

--  Tests for xsd_integer:gt

testVarModifyGt01, testVarModifyGt02, testVarModifyGt03,
  testVarModifyGt04, testVarModifyGt05 :: Test

testVarModifyGt01 = testVmod2  "testVarModifyGt01"
                    (getDTMod dmodXsdIntegerGt rdfDatatypeValXsdInteger)
                    [makeBVI [("a","100"),("b","100")]]
                    []

testVarModifyGt02 = testVmod2  "testVarModifyGt02"
                    (getDTMod dmodXsdIntegerGt rdfDatatypeValXsdInteger)
                    [makeBVI [("a","99"),("b","100")]]
                    []

testVarModifyGt03 = testVmod2  "testVarModifyGt03"
                    (getDTMod dmodXsdIntegerGt rdfDatatypeValXsdInteger)
                    [makeBVI [("a","-99"),("b","-100")]]
                    [makeBVI [("a","-99"),("b","-100")]]

testVarModifyGt04 = testVmod2  "testVarModifyGt04"
                    (getDTMod dmodXsdIntegerGt rdfDatatypeValXsdInteger)
                    [makeBVI [("b","100")]]
                    []

testVarModifyGt05 = testVmod2  "testVarModifyGt05"
                    (getDTMod dmodXsdIntegerGt rdfDatatypeValXsdInteger)
                    [makeBVI [("a","100")]]
                    []

--  Tests for xsd_integer:ge

testVarModifyGe01, testVarModifyGe02, testVarModifyGe03,
  testVarModifyGe04, testVarModifyGe05 :: Test

testVarModifyGe01 = testVmod2  "testVarModifyGe01"
                    (getDTMod dmodXsdIntegerGe rdfDatatypeValXsdInteger)
                    [makeBVI [("a","100"),("b","100")]]
                    [makeBVI [("a","100"),("b","100")]]

testVarModifyGe02 = testVmod2  "testVarModifyGe02"
                    (getDTMod dmodXsdIntegerGe rdfDatatypeValXsdInteger)
                    [makeBVI [("a","99"),("b","100")]]
                    []

testVarModifyGe03 = testVmod2  "testVarModifyGe03"
                    (getDTMod dmodXsdIntegerGe rdfDatatypeValXsdInteger)
                    [makeBVI [("a","-99"),("b","-100")]]
                    [makeBVI [("a","-99"),("b","-100")]]

testVarModifyGe04 = testVmod2  "testVarModifyGe04"
                    (getDTMod dmodXsdIntegerGe rdfDatatypeValXsdInteger)
                    [makeBVI [("b","100")]]
                    []

testVarModifyGe05 = testVmod2  "testVarModifyGe05"
                    (getDTMod dmodXsdIntegerGe rdfDatatypeValXsdInteger)
                    [makeBVI [("a","100")]]
                    []

--  Full suite for variable binding modifier tests

testVarModifySuite :: Test
testVarModifySuite = TestList
    [ testVarModify00
    , testVarModifyAbs01,    testVarModifyAbs02,    testVarModifyAbs03
    , testVarModifyAbs04,    testVarModifyAbs05,    testVarModifyAbs06
    , testVarModifyAbs07,    testVarModifyAbs08,    testVarModifyAbs09
    , testVarModifyAbs10
    , testVarModifyNeg01,    testVarModifyNeg02,    testVarModifyNeg03
    , testVarModifyNeg04,    testVarModifyNeg05
    , testVarModifySum01,    testVarModifySum02,    testVarModifySum03
    , testVarModifySum04,    testVarModifySum05
    , testVarModifyDiff01,   testVarModifyDiff02,   testVarModifyDiff03
    , testVarModifyDiff04,   testVarModifyDiff05
    , testVarModifyProd01,   testVarModifyProd02,   testVarModifyProd03
    , testVarModifyProd04,   testVarModifyProd05,   testVarModifyProd06
    , testVarModifyDivMod01, testVarModifyDivMod02, testVarModifyDivMod03
    , testVarModifyDivMod04, testVarModifyDivMod05, testVarModifyDivMod06
    , testVarModifyDivMod07
    , testVarModifyPower01,  testVarModifyPower02,  testVarModifyPower03
    , testVarModifyPower04,  testVarModifyPower05,  testVarModifyPower06
    , testVarModifyPower07,  testVarModifyPower08
    , testVarModifyEq01,     testVarModifyEq02,     testVarModifyEq03
    , testVarModifyEq04,     testVarModifyEq05
    , testVarModifyNe01,     testVarModifyNe02,     testVarModifyNe03
    , testVarModifyNe04,     testVarModifyNe05
    , testVarModifyLt01,     testVarModifyLt02,     testVarModifyLt03
    , testVarModifyLt04,     testVarModifyLt05
    , testVarModifyLe01,     testVarModifyLe02,     testVarModifyLe03
    , testVarModifyLe04,     testVarModifyLe05
    , testVarModifyGt01,     testVarModifyGt02,     testVarModifyGt03
    , testVarModifyGt04,     testVarModifyGt05
    , testVarModifyGe01,     testVarModifyGe02,     testVarModifyGe03
    , testVarModifyGe04,     testVarModifyGe05
    ]

------------------------------------------------------------
--  Test rules defined for datatype
------------------------------------------------------------

prefixes :: B.Builder
prefixes = mconcat $ map namespaceToBuilder
           [ namespaceRDF
           , namespaceRDFD
           , namespaceXSD
           , namespaceXsdInteger
           ]

mkGraph :: B.Builder -> RDFGraph
mkGraph gr = 
  let base = "@prefix : <" `mappend` (ns `mappend` "> . \n")
      ns = B.fromString $ show $ getNamespaceURI namespaceDefault
  in makeRDFGraphFromN3Builder (prefixes `mappend` (base `mappend` gr))

testRuleFwd :: String -> Maybe (Rule RDFGraph) -> B.Builder -> [B.Builder] -> Test
testRuleFwd lab (Just rule) antstr constrs =
    let
        antgr  = mkGraph antstr
        congrs = map mkGraph constrs
    in
        testEqv lab congrs $ fwdApply rule [antgr]
testRuleFwd lab Nothing _ _ = TestCase $
    assertFailure $ "testRuleFwd:"++lab++", null rule supplied"

testRuleBwd :: String -> Maybe (Rule RDFGraph) -> B.Builder -> [[B.Builder]] -> Test
testRuleBwd lab (Just rule) antstr prestrss =
    let
        antgr   = mkGraph antstr
        pregrss = map (map mkGraph) prestrss
    in
        testEqv2 lab pregrss $ bwdApply rule antgr
testRuleBwd lab Nothing _ _ = TestCase $
    assertFailure $ "testRuleBwd:"++lab++", null rule supplied"

{-
testRuleChk :: String -> Maybe (Rule RDFGraph) -> String -> String -> Test
testRuleChk lab (Just rule) antstr constr =
    let
        antgr = mkGraph antstr
        congr = mkGraph constr
    in
        test lab $ checkInference rule [antgr] congr
testRuleChk lab Nothing _ _ = TestCase $
    assertFailure $ "testRuleChk:"++lab++", null rule supplied"
-}

xsdIntRules :: Ruleset RDFGraph
xsdIntRules = typeRules rdfDatatypeXsdInteger

{-
axdt :: Maybe (Formula RDFGraph)
axdt        = getRulesetAxiom axiomXsdIntegerDT      xsdIntRules
-}

ruleabs, ruleneg, rulesum, rulediff, ruleprod,
  ruledivmod, rulepower, ruleeq, rulene, rulelt, rulele,
  rulegt, rulege :: Maybe (Rule RDFGraph)
ruleabs     = getRulesetRule  ruleXsdIntegerAbs      xsdIntRules
ruleneg     = getRulesetRule  ruleXsdIntegerNeg      xsdIntRules
rulesum     = getRulesetRule  ruleXsdIntegerSum      xsdIntRules
rulediff    = getRulesetRule  ruleXsdIntegerDiff     xsdIntRules
ruleprod    = getRulesetRule  ruleXsdIntegerProd     xsdIntRules
ruledivmod  = getRulesetRule  ruleXsdIntegerDivMod   xsdIntRules
rulepower   = getRulesetRule  ruleXsdIntegerPower    xsdIntRules
ruleeq      = getRulesetRule  ruleXsdIntegerEq       xsdIntRules
rulene      = getRulesetRule  ruleXsdIntegerNe       xsdIntRules
rulelt      = getRulesetRule  ruleXsdIntegerLt       xsdIntRules
rulele      = getRulesetRule  ruleXsdIntegerLe       xsdIntRules
rulegt      = getRulesetRule  ruleXsdIntegerGt       xsdIntRules
rulege      = getRulesetRule  ruleXsdIntegerGe       xsdIntRules

-- Test cases for the arithmetic functions

-- don't want to use text-format at present
qconv :: (Show a) => a -> B.Builder
qconv = B.fromString . show

-- abs

-- assume that vals is not empty
multiInp :: B.Builder -> [(Int, Int)] -> B.Builder
multiInp lbl vals = 
  let iConv (lval, val) = 
        mconcat
        [
          "  rdf:_", qconv lval, " \"", qconv val, "\"^^xsd:integer "
        ]
        
  in mconcat $
     [ "_:a a xsd_integer:"
     , lbl
     , " ;"
     ] ++ intersperse ";" (map iConv vals) ++ ["."]
  
singleInp :: B.Builder -> Int -> Int -> B.Builder
singleInp lbl lval val = multiInp lbl [(lval, val)]

abs01inp, abs02inp, abs03inp, abs04inp :: B.Builder
abs01inp = singleInp "Abs" 2 1
abs02inp = singleInp "Abs" 2 (-1)
abs03inp = singleInp "Abs" 1 1
abs04inp = singleInp "Abs" 1 (-1)
    
abs03bwd :: [[B.Builder]]
abs03bwd =
  [ [ "_:a a xsd_integer:Abs . "
    , "_:a rdf:_2 \"1\"^^xsd:integer . "
    ]
  , [ "_:a a xsd_integer:Abs . "
    , "_:a rdf:_2 \"-1\"^^xsd:integer . "
    ]
  ]

-- neg

neg01inp, neg02inp :: B.Builder
neg01inp = singleInp "Neg" 2 1
neg02inp = singleInp "Neg" 2 (-2)

-- sum

sum01inp :: B.Builder
sum01inp = multiInp "Sum" [(2, 31), (3, 20)]
  
sum01bwd :: [[B.Builder]]
sum01bwd =
    [ [ "_:a a xsd_integer:Sum . "
      , "_:a rdf:_1 \"51\"^^xsd:integer . "
      , "_:a rdf:_2 \"31\"^^xsd:integer . "
      ]
    , [ "_:a a xsd_integer:Sum . "
      , "_:a rdf:_1 \"51\"^^xsd:integer . "
      , "_:a rdf:_3 \"20\"^^xsd:integer . "
      ]
    ]

sum02inp :: B.Builder
sum02inp = multiInp "Sum" [(1, 52), (3, 21)]
  
sum02bwd :: [[B.Builder]]
sum02bwd =
    [ [ "_:a a xsd_integer:Sum . "
      , "_:a rdf:_1 \"52\"^^xsd:integer . "
      , "_:a rdf:_2 \"31\"^^xsd:integer . "
      ]
    , [ "_:a a xsd_integer:Sum . "
      , "_:a rdf:_2 \"31\"^^xsd:integer . "
      , "_:a rdf:_3 \"21\"^^xsd:integer . "
      ]
    ]

sum03inp :: B.Builder
sum03inp = multiInp "Sum" [(1, 53), (2, 32)]

sum03bwd :: [[B.Builder]]
sum03bwd =
    [ [ "_:a a xsd_integer:Sum . "
      , "_:a rdf:_1 \"53\"^^xsd:integer . "
      , "_:a rdf:_3 \"21\"^^xsd:integer . "
      ]
    , [ "_:a a xsd_integer:Sum . "
      , "_:a rdf:_2 \"32\"^^xsd:integer . "
      , "_:a rdf:_3 \"21\"^^xsd:integer . "
      ]
    ]

-- diff

diff01inp, diff02inp, diff03inp :: B.Builder
diff01inp = multiInp "Diff" [(2, 222), (3, 333)]
diff02inp = multiInp "Diff" [(1, -111), (3, 333)]
diff03inp = multiInp "Diff" [(1, -111), (2, 222)]
    
diff01bwd :: [[B.Builder]]
diff01bwd = 
    [ [ "_:a a xsd_integer:Diff . "
      , "_:a rdf:_1 \"-111\"^^xsd:integer . "
      , "_:a rdf:_2 \"222\"^^xsd:integer . "
      ]
    , [ "_:a a xsd_integer:Diff . "
      , "_:a rdf:_1 \"-111\"^^xsd:integer . "
      , "_:a rdf:_3 \"333\"^^xsd:integer . "
      ]
    ]

diff02bwd :: [[B.Builder]]
diff02bwd = 
    [ [ "_:a a xsd_integer:Diff . "
      , "_:a rdf:_1 \"-111\"^^xsd:integer . "
      , "_:a rdf:_2 \"222\"^^xsd:integer . "
      ]
    , [ "_:a a xsd_integer:Diff . "
      , "_:a rdf:_2 \"222\"^^xsd:integer . "
      , "_:a rdf:_3 \"333\"^^xsd:integer . "
      ]
    ]

diff03bwd :: [[B.Builder]]
diff03bwd = 
    [ [ "_:a a xsd_integer:Diff . "
      , "_:a rdf:_1 \"-111\"^^xsd:integer . "
      , "_:a rdf:_3 \"333\"^^xsd:integer . "
      ]
    , [ "_:a a xsd_integer:Diff . "
      , "_:a rdf:_2 \"222\"^^xsd:integer . "
      , "_:a rdf:_3 \"333\"^^xsd:integer . "
      ]
    ]

-- prod

prod01inp, prod02inp, prod03inp :: B.Builder
prod01inp = multiInp "Prod" [(2, 222), (3, 3)]
prod02inp = multiInp "Prod" [(1, 666), (3, 3)]
prod03inp = multiInp "Prod" [(1, 666), (2, 222)]

prod01bwd :: [[B.Builder]]
prod01bwd =
    [ [ "_:a a xsd_integer:Prod . "
      , "_:a rdf:_1 \"666\"^^xsd:integer . "
      , "_:a rdf:_2 \"222\"^^xsd:integer . "
      ]
    , [ "_:a a xsd_integer:Prod . "
      , "_:a rdf:_1 \"666\"^^xsd:integer . "
      , "_:a rdf:_3 \"3\"^^xsd:integer . "
      ]
    ]

prod02bwd :: [[B.Builder]]
prod02bwd =
    [ [ "_:a a xsd_integer:Prod . "
      , "_:a rdf:_1 \"666\"^^xsd:integer . "
      , "_:a rdf:_2 \"222\"^^xsd:integer . "
      ]
    , [ "_:a a xsd_integer:Prod . "
      , "_:a rdf:_2 \"222\"^^xsd:integer . "
      , "_:a rdf:_3 \"3\"^^xsd:integer . "
      ]
    ]

prod03bwd :: [[B.Builder]]
prod03bwd =
    [ [ "_:a a xsd_integer:Prod . "
      , "_:a rdf:_1 \"666\"^^xsd:integer . "
      , "_:a rdf:_3 \"3\"^^xsd:integer . "
      ]
    , [ "_:a a xsd_integer:Prod . "
      , "_:a rdf:_2 \"222\"^^xsd:integer . "
      , "_:a rdf:_3 \"3\"^^xsd:integer . "
      ]
    ]

-- divmod

divmod01inp, divmod02inp, divmod03inp :: B.Builder
divmod01inp = multiInp "DivMod" [(3, 33), (4, 5)]
divmod02inp = multiInp "DivMod" [(1, 6), (2, 3), (4, 5)]
divmod03inp = multiInp "DivMod" [(3, -33), (4, 5)]

-- power

power01inp, power02inp, power03inp :: B.Builder
power01inp = multiInp "Power" [(2, 2), (3, 5)]
power02inp = multiInp "Power" [(2, 111), (3, 0)]
power03inp = multiInp "Power" [(2, 22), (3, -33)]

-- eq

eq01inp, eq02inp, eq03inp :: B.Builder
eq01inp = multiInp "Eq" [(1, 11), (2, 11)]
eq02inp = multiInp "Eq" [(1, 21), (2, 22)]
eq03inp = multiInp "Eq" [(1, 31), (2, -32)]

-- ne

ne01inp, ne02inp, ne03inp :: B.Builder
ne01inp = multiInp "Ne" [(1, 11), (2, 11)]
ne02inp = multiInp "Ne" [(1, 21), (2, 22)]
ne03inp = multiInp "Ne" [(1, 31), (2, -32)]

-- lt

lt01inp, lt02inp, lt03inp :: B.Builder
lt01inp = multiInp "Lt" [(1, 11), (2, 11)]
lt02inp = multiInp "Lt" [(1, 21), (2, 22)]
lt03inp = multiInp "Lt" [(1, 31), (2, -32)]

-- le

le01inp, le02inp, le03inp :: B.Builder
le01inp = multiInp "Le" [(1, 11), (2, 11)]
le02inp = multiInp "Le" [(1, 21), (2, 22)]
le03inp = multiInp "Le" [(1, 31), (2, -32)]

-- gt

gt01inp, gt02inp, gt03inp :: B.Builder
gt01inp = multiInp "Gt" [(1, 11), (2, 11)]
gt02inp = multiInp "Gt" [(1, 21), (2, 22)]
gt03inp = multiInp "Gt" [(1, 31), (2, -32)]

-- ge

ge01inp, ge02inp, ge03inp :: B.Builder
ge01inp = multiInp "Ge" [(1, 11), (2, 11)]
ge02inp = multiInp "Ge" [(1, 21), (2, 22)]
ge03inp = multiInp "Ge" [(1, 31), (2, -32)]

-- Test cases from design notes

-- Make a vector of rules using the graph string below

pvRules :: [RDFRule]
-- pvRules = makeRDFDatatypeRestrictionRules rdfDatatypeValXsdInteger gr
pvRules = typeMkRules rdfDatatypeXsdInteger gr
    where
        gr = mkGraph $
             mconcat
             [ ":PassengerVehicle a rdfd:GeneralRestriction ; "
             , "  rdfd:onProperties (:totalCapacity :seatedCapacity :standingCapacity) ; "
             , "  rdfd:constraint xsd_integer:sum . "
             , ":PassengerVehicle1 a rdfd:GeneralRestriction ; "
             , "  rdfd:onProperties (:totalCapacity :seatedCapacity :standingCapacity) ; "
             , "  rdfd:constraint xsd_integer:sum ; "
             , "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
             ]
             
--  Now the test cases that use the rules created above.

pvRule0, pvRule1 :: Maybe (Rule RDFGraph)
pvRule0 = M.lookup 
            (makeNSScopedName namespaceDefault "PassengerVehicle")
            $ M.fromList $ map (\rl -> (ruleName rl, rl)) pvRules
pvRule1 = M.lookup
            (makeNSScopedName namespaceDefault "PassengerVehicle1")
            $ M.fromList $ map (\rl -> (ruleName rl, rl)) pvRules

pv01inp :: B.Builder
pv01inp =
  mconcat
  [ "_:a a :PassengerVehicle ; "
  , "  :seatedCapacity \"30\"^^xsd:integer ; "
  , "  :standingCapacity \"20\"^^xsd:integer . "
  ]

pv01fwd :: [B.Builder]
pv01fwd = [ "_:a :totalCapacity \"50\"^^xsd:integer . " ]

pv01bwd :: [[B.Builder]]
pv01bwd =
    [ [ "_:a a :PassengerVehicle . "
      , "_:a :totalCapacity \"50\"^^xsd:integer . "
      , "_:a :seatedCapacity \"30\"^^xsd:integer . "
      ]
    , [ "_:a a :PassengerVehicle . "
      , "_:a :totalCapacity \"50\"^^xsd:integer . "
      , "_:a :standingCapacity \"20\"^^xsd:integer . "
      ]
    ]

pv02inp :: B.Builder
pv02inp =
  mconcat
  [ "_:a a :PassengerVehicle ; "
  , "  :seatedCapacity \"30\"^^xsd:integer ; "
  , "  :totalCapacity \"51\"^^xsd:integer . "
  , "_:b a :PassengerVehicle ; "
  , "  :standingCapacity \"20\"^^xsd:integer ; "
  , "  :totalCapacity \"52\"^^xsd:integer . "
  ]

pv02fwd :: [B.Builder]
pv02fwd =
    [ "_:a :standingCapacity \"21\"^^xsd:integer . "
    , "_:b :seatedCapacity \"32\"^^xsd:integer . "
    ]

pv02bwd :: [[B.Builder]]
pv02bwd =
    [ [ "_:a a :PassengerVehicle . "
      , "_:a :standingCapacity \"21\"^^xsd:integer . "
      , "_:a :totalCapacity \"51\"^^xsd:integer . "
      , "_:b a :PassengerVehicle . "
      , "_:b :seatedCapacity \"32\"^^xsd:integer . "
      , "_:b :totalCapacity \"52\"^^xsd:integer . "
      ]
    , [ "_:a a :PassengerVehicle . "
      , "_:a :seatedCapacity \"30\"^^xsd:integer . "
      , "_:a :standingCapacity \"21\"^^xsd:integer . "
      , "_:b a :PassengerVehicle . "
      , "_:b :seatedCapacity \"32\"^^xsd:integer . "
      , "_:b :totalCapacity \"52\"^^xsd:integer . "
      ]
    , [ "_:a a :PassengerVehicle . "
      , "_:a :standingCapacity \"21\"^^xsd:integer . "
      , "_:a :totalCapacity \"51\"^^xsd:integer . "
      , "_:b a :PassengerVehicle . "
      , "_:b :seatedCapacity \"32\"^^xsd:integer . "
      , "_:b :standingCapacity \"20\"^^xsd:integer . "
      ]
    , [ "_:a a :PassengerVehicle . "
      , "_:a :seatedCapacity \"30\"^^xsd:integer . "
      , "_:a :standingCapacity \"21\"^^xsd:integer . "
      , "_:b a :PassengerVehicle . "
      , "_:b :seatedCapacity \"32\"^^xsd:integer . "
      , "_:b :standingCapacity \"20\"^^xsd:integer . "
      ]
    ]

pv03inp :: B.Builder
pv03inp =
  mconcat
  [ "_:a a :PassengerVehicle ; "
  , "  :seatedCapacity \"30\"^^xsd:integer ; "
  , "  :standingCapacity \"23\"^^xsd:integer ; "
  , "  :totalCapacity \"53\"^^xsd:integer . "
  ]

pv03fwd :: [B.Builder]
pv03fwd = []

pv04inp :: B.Builder
pv04inp =
  mconcat
  [ "_:a a :PassengerVehicle ; "
  , "  :seatedCapacity \"30\"^^xsd:integer ; "
  , "  :standingCapacity \"20\"^^xsd:integer ; "
  , "  :totalCapacity \"54\"^^xsd:integer . "
  ]
  
pv04fwd :: [B.Builder]
pv04fwd =
  [ mconcat
    [ "_:a :standingCapacity \"24\"^^xsd:integer . "
    , "_:a :seatedCapacity \"34\"^^xsd:integer . "
    , "_:a :totalCapacity \"50\"^^xsd:integer . "
    ]
  ]

pv05inp :: B.Builder
pv05inp =
   mconcat     
   [ "_:a a :PassengerVehicle1 ; "
   , "  :seatedCapacity \"30\"^^xsd:integer ; "
   , "  :standingCapacity \"25\"^^xsd:integer ; "
   , "  :totalCapacity \"55\"^^xsd:integer . "
   ]

pv05fwd :: [B.Builder]
pv05fwd = []

pv06inp :: B.Builder
pv06inp =
  mconcat
  [ "_:a a :PassengerVehicle1 ; "
  , "  :seatedCapacity \"30\"^^xsd:integer ; "
  , "  :standingCapacity \"20\"^^xsd:integer ; "
  , "  :totalCapacity \"56\"^^xsd:integer . "
  ]
  
pv06fwd :: [B.Builder]
pv06fwd =
    [ falseGraphStr
    ]

pv06bwd :: [[B.Builder]]
pv06bwd =
    [ [ falseGraphStr
      ]
    ]

pv07inp :: B.Builder
pv07inp =
  "_:a a :PassengerVehicle ; " `mappend`
  "  :totalCapacity \"57\"^^xsd:integer . "

pv07fwd :: [B.Builder]
pv07fwd = []

-- how come this isn't [[String]] ?
pv07bwd :: [B.Builder]
pv07bwd = []

--  Full suite for datatype rule tests

testDatatypeRuleSuite :: Test
testDatatypeRuleSuite = TestList
  [ testRuleFwd "testRuleFwdAbs01" ruleabs abs01inp [ "_:a rdf:_1 \"1\"^^xsd:integer . " ]
  , testRuleFwd "testRuleFwdAbs02" ruleabs abs02inp [ "_:a rdf:_1 \"1\"^^xsd:integer . " ]
  , testRuleFwd "testRuleFwdAbs03" ruleabs abs03inp []
  , testRuleFwd "testRuleFwdAbs04" ruleabs abs04inp [falseGraphStr]
  , testRuleFwd "testRuleFwdNeg01" ruleneg neg01inp [ "_:a rdf:_1 \"-1\"^^xsd:integer . " ]
  , testRuleFwd "testRuleFwdNeg02" ruleneg neg02inp [ "_:a rdf:_1 \"2\"^^xsd:integer . " ]
  , testRuleFwd "testRuleFwdSum01" rulesum sum01inp [ "_:a rdf:_1 \"51\"^^xsd:integer . " ]
  , testRuleFwd "testRuleFwdSum02" rulesum sum02inp [ "_:a rdf:_2 \"31\"^^xsd:integer . " ]
  , testRuleFwd "testRuleFwdSum03" rulesum sum03inp [ "_:a rdf:_3 \"21\"^^xsd:integer . " ]
  , testRuleFwd "testRuleFwdDiff01" rulediff diff01inp [ "_:a rdf:_1 \"-111\"^^xsd:integer . " ]
  , testRuleFwd "testRuleFwdDiff02" rulediff diff02inp [ "_:a rdf:_2 \"222\"^^xsd:integer . " ]
  , testRuleFwd "testRuleFwdDiff03" rulediff diff03inp [ "_:a rdf:_3 \"333\"^^xsd:integer . " ]
  , testRuleFwd "testRuleFwdProd01" ruleprod prod01inp [ "_:a rdf:_1 \"666\"^^xsd:integer . " ]
  , testRuleFwd "testRuleFwdProd02" ruleprod prod02inp [ "_:a rdf:_2 \"222\"^^xsd:integer . " ]
  , testRuleFwd "testRuleFwdProd03" ruleprod prod03inp [ "_:a rdf:_3 \"3\"^^xsd:integer . " ]
  , testRuleFwd "testRuleFwdDivMod01" ruledivmod divmod01inp
    [ "_:a rdf:_1 \"6\"^^xsd:integer . " `mappend` "_:a rdf:_2 \"3\"^^xsd:integer . " ]
  , testRuleFwd "testRuleFwdDivMod02" ruledivmod divmod02inp []
  , testRuleFwd "testRuleFwdDivMod03" ruledivmod divmod03inp 
    [ "_:a rdf:_1 \"-7\"^^xsd:integer . " `mappend` "_:a rdf:_2 \"2\"^^xsd:integer . " ]
  , testRuleFwd "testRuleFwdPower01" rulepower power01inp [ "_:a rdf:_1 \"32\"^^xsd:integer . " ]
  , testRuleFwd "testRuleFwdPower02" rulepower power02inp [ "_:a rdf:_1 \"1\"^^xsd:integer . " ]
  , testRuleFwd "testRuleFwdPower03" rulepower power03inp [falseGraphStr]
  , testRuleFwd "testRuleFwdEq01" ruleeq eq01inp []
  , testRuleFwd "testRuleFwdEq02" ruleeq eq02inp [falseGraphStr]
  , testRuleFwd "testRuleFwdEq03" ruleeq eq03inp [falseGraphStr]
  , testRuleFwd "testRuleFwdNe01" rulene ne01inp [falseGraphStr]
  , testRuleFwd "testRuleFwdNe02" rulene ne02inp []
  , testRuleFwd "testRuleFwdNe03" rulene ne03inp []
  , testRuleFwd "testRuleFwdLt01" rulelt lt01inp [falseGraphStr]
  , testRuleFwd "testRuleFwdLt02" rulelt lt02inp []
  , testRuleFwd "testRuleFwdLt03" rulelt lt03inp [falseGraphStr]
  , testRuleFwd "testRuleFwdLe01" rulele le01inp []
  , testRuleFwd "testRuleFwdLe02" rulele le02inp []
  , testRuleFwd "testRuleFwdLe03" rulele le03inp [falseGraphStr]
  , testRuleFwd "testRuleFwdGt01" rulegt gt01inp [falseGraphStr]
  , testRuleFwd "testRuleFwdGt02" rulegt gt02inp [falseGraphStr]
  , testRuleFwd "testRuleFwdGt03" rulegt gt03inp []
  , testRuleFwd "testRuleFwdGe01" rulege ge01inp []
  , testRuleFwd "testRuleFwdGe02" rulege ge02inp [falseGraphStr]
  , testRuleFwd "testRuleFwdGe03" rulege ge03inp []
                                      
    -- backard chaining tests
  , testRuleBwd "testRuleBwdAbs01" ruleabs abs01inp []
  , testRuleBwd "testRuleBwdAbs02" ruleabs abs02inp []
  , testRuleBwd "testRuleBwdAbs03" ruleabs abs03inp abs03bwd
  , testRuleBwd "testRuleBwdAbs04" ruleabs abs04inp [[falseGraphStr]]
  , testRuleBwd "testRuleBwdNeg01" ruleneg neg01inp
    [[ "_:a a xsd_integer:Neg . ", "_:a rdf:_1 \"-1\"^^xsd:integer . "]]
  , testRuleBwd "testRuleBwdNeg02" ruleneg neg02inp 
    [[ "_:a a xsd_integer:Neg . ", "_:a rdf:_1 \"2\"^^xsd:integer . "]]
  , testRuleBwd "testRuleBwdSum01" rulesum sum01inp sum01bwd
  , testRuleBwd "testRuleBwdSum02" rulesum sum02inp sum02bwd
  , testRuleBwd "testRuleBwdSum03" rulesum sum03inp sum03bwd
  , testRuleBwd "testRuleBwdDiff01" rulediff diff01inp diff01bwd
  , testRuleBwd "testRuleBwdDiff02" rulediff diff02inp diff02bwd
  , testRuleBwd "testRuleBwdDiff03" rulediff diff03inp diff03bwd
  , testRuleBwd "testRuleBwdProd01" ruleprod prod01inp prod01bwd
  , testRuleBwd "testRuleBwdProd02" ruleprod prod02inp prod02bwd
  , testRuleBwd "testRuleBwdProd03" ruleprod prod03inp prod03bwd
  , testRuleBwd "testRuleBwdDivMod01" ruledivmod divmod01inp []
  , testRuleBwd "testRuleBwdDivMod02" ruledivmod divmod02inp []
  , testRuleBwd "testRuleBwdDivMod03" ruledivmod divmod03inp []
  , testRuleBwd "testRuleBwdPower01" rulepower power01inp []
  , testRuleBwd "testRuleBwdPower02" rulepower power02inp []
  , testRuleBwd "testRuleBwdPower03" rulepower power03inp [[falseGraphStr]]
  , testRuleBwd "testRuleBwdEq01" ruleeq eq01inp []
  , testRuleBwd "testRuleBwdEq02" ruleeq eq02inp [[falseGraphStr]]
  , testRuleBwd "testRuleBwdEq03" ruleeq eq03inp [[falseGraphStr]]
  , testRuleBwd "testRuleBwdNe01" rulene ne01inp [[falseGraphStr]]
  , testRuleBwd "testRuleBwdNe02" rulene ne02inp []
  , testRuleBwd "testRuleBwdNe03" rulene ne03inp []
  , testRuleBwd "testRuleBwdLt01" rulelt lt01inp [[falseGraphStr]]
  , testRuleBwd "testRuleBwdLt02" rulelt lt02inp []
  , testRuleBwd "testRuleBwdLt03" rulelt lt03inp [[falseGraphStr]]
  , testRuleBwd "testRuleBwdLe01" rulele le01inp []
  , testRuleBwd "testRuleBwdLe02" rulele le02inp []
  , testRuleBwd "testRuleBwdLe03" rulele le03inp [[falseGraphStr]]
  , testRuleBwd "testRuleBwdGt01" rulegt gt01inp [[falseGraphStr]]
  , testRuleBwd "testRuleBwdGt02" rulegt gt02inp [[falseGraphStr]]
  , testRuleBwd "testRuleBwdGt03" rulegt gt03inp []
  , testRuleBwd "testRuleBwdGe01" rulege ge01inp []
  , testRuleBwd "testRuleBwdGe02" rulege ge02inp [[falseGraphStr]]
  , testRuleBwd "testRuleBwdGe03" rulege ge03inp []

    -- test cases from design notes
  , testRuleFwd "testRuleFwdPv01" pvRule0 pv01inp pv01fwd
  , testRuleFwd "testRuleFwdPv02" pvRule0 pv02inp pv02fwd
  , testRuleFwd "testRuleFwdPv03" pvRule0 pv03inp pv03fwd
  , testRuleFwd "testRuleFwdPv04" pvRule0 pv04inp pv04fwd
  , testRuleFwd "testRuleFwdPv05" pvRule1 pv05inp pv05fwd
  , testRuleFwd "testRuleFwdPv06" pvRule1 pv06inp pv06fwd
  , testRuleFwd "testRuleFwdPv07" pvRule0 pv07inp pv07fwd

  , testRuleBwd "testRuleBwdPv01" pvRule0 pv01inp pv01bwd
  , testRuleBwd "testRuleBwdPv02" pvRule0 pv02inp pv02bwd
  , testRuleBwd "testRuleBwdPv06" pvRule1 pv06inp pv06bwd
  , testRuleFwd "testRuleBwdPv07" pvRule0 pv07inp pv07bwd
    
  ]

------------------------------------------------------------
--  All tests
------------------------------------------------------------

allTests :: [TF.Test]
allTests = 
  [ conv "Datatype" testDatatypeSuite
  , conv "DatatypeVal" testDatatypeValSuite
  , conv "VarModify" testVarModifySuite
  , conv "DatatypeRule" testDatatypeRuleSuite
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
