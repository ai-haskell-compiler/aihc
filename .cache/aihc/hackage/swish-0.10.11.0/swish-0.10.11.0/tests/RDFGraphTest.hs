{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  RDFGraphTest
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012, 2013, 2014, 2017, 2021, 2026 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  CPP, OverloadedStrings
--
--  This module contains test cases for module RDFGraph.
--
--------------------------------------------------------------------------------

module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

-- import qualified Data.Traversable as Traversable
import qualified Data.Foldable as Foldable

import qualified Test.Framework as TF

import Swish.Namespace (Namespace, makeNamespace, getNamespaceURI, getNamespaceTuple, ScopedName, makeNSScopedName, nullScopedName)
import Swish.QName (QName, qnameFromURI)

import Swish.GraphClass (Label(..), arc {- , arcToTriple -} )

import Swish.RDF.Graph
  ( RDFTriple, toRDFTriple, fromRDFTriple
  , RDFGraph, NamespaceMap, Formula
  , RDFLabel(..), ToRDFLabel(..), FromRDFLabel(..)
  , NSGraph(..)
  , isLiteral, isUntypedLiteral, isTypedLiteral, isXMLLiteral
  , isDatatyped, isMemberProp
  , isUri, isBlank, isQueryVar, makeBlank
  , getScopedName
  , LookupFormula(..), FormulaMap, emptyFormulaMap
  , getArcs, addArc
  , remapLabels, remapLabelList
  , setFormulae, getFormulae, setFormula, getFormula
  , fmapNSGraph
  , newNode, newNodes )

import Swish.RDF.Vocabulary
  ( namespaceRDF
  , LanguageTag
  , toLangTag 
  , rdfXMLLiteral
  , xsdBoolean
  , xsdInteger
  , xsdFloat
  , xsdDouble
  , xsdDateTime
  , xsdDate
    )

import Control.Arrow ((&&&), first)

import Data.List (elemIndex, intercalate)
import Data.Maybe (fromJust, fromMaybe)
import Data.Ord (comparing)

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Data.Monoid (Monoid(..))
#endif

import Network.URI (URI, parseURI)

#if MIN_VERSION_time(1,15,0)
import Data.Time (UTCTime(..), Day, fromGregorian, defaultTimeLocale)
import Data.Time.Format (parseTimeM)
#elif MIN_VERSION_time(1,9,0)
import Data.Time (UTCTime(..), Day, fromGregorian, defaultTimeLocale)
import Data.Time.Format.Internal (buildTime)
#elif MIN_VERSION_time(1,5,0)
import Data.Time (UTCTime(..), Day, fromGregorian, buildTime, defaultTimeLocale)
#else
import Data.Time (UTCTime(..), Day, fromGregorian, buildTime)
import System.Locale (defaultTimeLocale)
#endif

import Test.HUnit
    ( Test(TestCase,TestList,TestLabel)
    , Assertion
    , assertBool, assertEqual )

import TestHelpers ( conv, testEq
                   , testEqv
                   , testCompare
                   , testCompareEq)

------------------------------------------------------------
--  Test language tag comparisons
------------------------------------------------------------

type Lang = Maybe LanguageTag

toLT :: T.Text -> LanguageTag
toLT = fromJust . toLangTag

lt0, lt1, lt2, lt3, lt4, lt5, lt6,
  lt7, lt8 :: Lang
lt0 = Nothing
lt1 = Just (toLT "en")
lt2 = Just (toLT "EN")
lt3 = Just (toLT "fr")
lt4 = Just (toLT "FR")
lt5 = Just (toLT "en-us")
lt6 = Just (toLT "en-US")
lt7 = Just (toLT "EN-us")
lt8 = Just (toLT "EN-US")

langlist :: [(String, Lang)]
langlist =
  [ ("lt0",lt0),
    ("lt1",lt1), ("lt2",lt2), ("lt3",lt3), ("lt4",lt4),
    ("lt5",lt5), ("lt6",lt6), ("lt7",lt7), ("lt8",lt8) ]

langeqlist :: [(String, String)]
langeqlist =
  [
    ("lt1","lt2"),
    ("lt3","lt4"),
    ("lt5","lt6"),
    ("lt5","lt7"),
    ("lt5","lt8"),
    ("lt6","lt7"),
    ("lt6","lt8"),
    ("lt7","lt8")
  ]

testLangEq :: String -> Bool -> Lang -> Lang -> Test
testLangEq = testCompareEq "testLangEq:"

hyphen :: String -> String -> String
hyphen a b = a ++ "-" ++ b

testLangEqSuite :: Test
testLangEqSuite = TestList
  [ testLangEq (ll1 `hyphen` ll2) (tEq  ll1 ll2) t1 t2
      | (ll1,t1) <- langlist , (ll2,t2) <- langlist ]
    where
    tEq  ll1 ll2 = (ll1 == ll2)        ||
         (ll1,ll2) `elem` langeqlist ||
         (ll2,ll1) `elem` langeqlist

------------------------------------------------------------
--  Define some common values
------------------------------------------------------------

base1Str :: String
base1Str = "http://id.ninebynine.org/wip/2003/test/graph1/node#"

toURI :: String -> URI
toURI s = fromMaybe (error $ "Error: unable to parse URI " ++ s) (parseURI s)

toNS :: T.Text -> String -> Namespace
toNS p = makeNamespace (Just p) . toURI

-- TODO: basee and baseu had prefixes of "" and "?" before the conversion
--       to using Maybe String. Now both are Nothing; is this semantically
--       correct? Probably.

basee, baseu, base1, base2, base3, base4 :: Namespace
basee = makeNamespace Nothing $ toURI "http://example.com/a#"
baseu = makeNamespace Nothing $ toURI "http://example.com/"
base1 = toNS "base1" base1Str
base2 = toNS "base2" "http://id.ninebynine.org/wip/2003/test/graph2/node/"
base3 = toNS "base3" "http://id.ninebynine.org/wip/2003/test/graph3/node"
base4 = toNS "base4" "http://id.ninebynine.org/wip/2003/test/graph3/nodebase"

qn1s1 :: QName
qn1s1 = fromJust $ qnameFromURI $ toURI $ base1Str ++ "s1"

qu1s1 :: URI
qu1s1 = toURI $ base1Str ++ "s1"

qbes1, qbus1, qb1s1, qb2s2, qb3s3, qb3, qb3bm, qb4m :: ScopedName
qbes1 = makeNSScopedName basee "s1"
qbus1 = makeNSScopedName baseu "s1"
qb1s1 = makeNSScopedName base1 "s1"
qb2s2 = makeNSScopedName base2 "s2"
qb3s3 = makeNSScopedName base3 "s3"
qb3   = makeNSScopedName base3 ""
qb3bm = makeNSScopedName base3 "basemore"
qb4m  = makeNSScopedName base4 "more"

es1, us1, s1, s2, s3, s4, s5, s6, s7, s8 :: RDFLabel
es1 = Res qbes1
us1 = Res qbus1
s1 = toRDFLabel qb1s1 
s2 = toRDFLabel qb2s2 
s3 = toRDFLabel qb3s3 
s4 = Res qb3   
s5 = Blank "s5"
s6 = Res qb3bm 
s7 = Res qb4m  
s8 = Blank "s8"

st1, st2, st3 :: RDFLabel
st1 = toRDFLabel $ makeNSScopedName base1 "st1"
st2 = toRDFLabel $ makeNSScopedName base2 "st2" 
st3 = toRDFLabel $ makeNSScopedName base3 "st3"

bb, bb0, b1, b2, b3, b4, b5, b6, b7,
  b8, b9, b10 :: RDFLabel
bb  = Blank "bb" 
bb0 = Blank "bb0"
b1  = Blank "b1" 
b2  = Blank "b2" 
b3  = Blank "b3" 
b4  = Blank "b4" 
b5  = Blank "b5" 
b6  = Blank "b6" 
b7  = Blank "b7" 
b8  = Blank "b8" 
b9  = Blank "b9" 
b10 = Blank "b10"

c1, c2, c3, c4 :: RDFLabel
c1 = Blank "c1"
c2 = Blank "c2"
c3 = Blank "c3"
c4 = Blank "c4"

ba1, ba2, ba3, ba4 :: RDFLabel
ba1 = Blank "_1"
ba2 = Blank "_2"
ba3 = Blank "_3"
ba4 = Blank "_4"

bn3, bn4, bn5, bn6 :: RDFLabel
bn3 = Blank "3"
bn4 = Blank "4"
bn5 = Blank "5"
bn6 = Blank "6"

p1, p2, p3, p4 :: RDFLabel
p1 = Res $ makeNSScopedName base1 "p1"
p2 = Res $ makeNSScopedName base2 "p2"
p3 = Res $ makeNSScopedName base3 "p3"
p4 = Res $ makeNSScopedName base3 "p4"

o1, o2, o3, o4, o5, o6 :: RDFLabel
o1 = Res $ makeNSScopedName base1 "o1"
o2 = Res $ makeNSScopedName base2 "o2"
o3 = Res $ makeNSScopedName base3 "o3"
o4 = toRDFLabel qb3   
o5 = Blank "o5"
o6 = Blank "s5"

qb1t1, qb1t2 :: ScopedName
qb1t1 = makeNSScopedName base1 "type1"
qb1t2 = makeNSScopedName base1 "type2"

l1, l2, l2gb, l3, l4, l5, l6, l7, l8,
  l9, l10, l11, l12 :: RDFLabel
l1   = "l1" -- use IsString instance
l2   = LangLit "l2"  (toLT "en")
l2gb = LangLit "l2"  (toLT "en-gb")
l3   = LangLit "l2"  (toLT "fr")
l4   = TypedLit "l4"  qb1t1    
l5   = TypedLit "l4"  qb1t1           
l6   = TypedLit "l4"  qb1t1           
l7   = TypedLit "l4"  qb1t2           
l8   = TypedLit "l4"  qb1t2           
l9   = TypedLit "l4"  qb1t2           
l10  = TypedLit "l10" rdfXMLLiteral
-- l11  = TypedLit "l11" (Just rdfXMLLiteral)  
-- l12  = TypedLit "l12" (Just rdfXMLLiteral)  
l11  = TypedLit "l10"  rdfXMLLiteral   -- are these meant to both be l10?
l12  = TypedLit "l10"  rdfXMLLiteral   -- if you change them some tests fail

nanF, infF, ninfF :: RDFLabel
nanF  = TypedLit "NaN"  xsdFloat
infF  = TypedLit "INF"  xsdFloat
ninfF = TypedLit "-INF" xsdFloat

nanD, infD, ninfD :: RDFLabel
nanD  = TypedLit "NaN"  xsdDouble
infD  = TypedLit "INF"  xsdDouble
ninfD = TypedLit "-INF" xsdDouble

v1, v2, v3, v4, vb3, vb4 :: RDFLabel
v1  = Var "v1"  
v2  = Var "v2"  
v3  = Var "v3"  
v4  = Var "v4"  
vb3 = Blank "v3"
vb4 = Blank "v4"

-- Test cases for isMemberProp

cm1, cm2, nm1, nm2 :: RDFLabel
cm1  = Res $ makeNSScopedName namespaceRDF "_1"
cm2  = Res $ makeNSScopedName namespaceRDF "_234567"
nm1  = Res $ makeNSScopedName namespaceRDF "987"
nm2  = Res $ makeNSScopedName namespaceRDF "_987a65"

------------------------------------------------------------
--  RDFLabel construction and equality tests
------------------------------------------------------------

testLabelEq :: String -> Bool -> RDFLabel -> RDFLabel -> Test
testLabelEq = testCompareEq "testLabelEq:"

nodelist :: [(String, RDFLabel)]
nodelist =
  [ ("es1",es1), ("us1",us1)
  , ("s1",s1), ("s2",s2), ("s3",s3), ("s4",s4), ("s5",s5)
  , ("s6",s6), ("s7",s7), ("s8",s8)
  , ("b1",b1), ("b2",b2), ("b3",b3), ("b4",b4)
  , ("p1",p1), ("p2",p2), ("p3",p3), ("p4",p4)
  , ("o1",o1), ("o2",o2), ("o3",o3), ("o4",o4), ("o5",o5)
  , ("l1",l1), ("l2",l2), ("l2gb",l2gb), ("l3",l3)
  , ("l4",l4), ("l5",l5), ("l6",l6)
  , ("l7",l7), ("l8",l8), ("l9",l9)
  , ("l10",l10), ("l11",l11), ("l12",l12)
  , ("v1",v1), ("v2",v2)
  ]

nodeeqlist :: [(String, String)]
nodeeqlist =
  [ ("s4","o4")
  , ("s5","o6")
  , ("s6","s7")
  , ("l4","l5")
  , ("l4","l6")
  , ("l5","l6")
  , ("l7","l8")
  , ("l7","l9")
  , ("l8","l9")
  , ("l10","l11")
  , ("l10","l12")
  , ("l11","l12")
  ]

testNodeEqSuite :: Test
testNodeEqSuite = TestList
  [ testLabelEq (ll1 `hyphen` ll2) (tEq  ll1 ll2) n1 n2
      | (ll1,n1) <- nodelist , (ll2,n2) <- nodelist ]
    where
    tEq  ll1 ll2 = (ll1 == ll2)        ||
         (ll1,ll2) `elem` nodeeqlist ||
         (ll2,ll1) `elem` nodeeqlist

-- test ToRDFLabel/FromRDFlabel/IsString instances
--

toLbl :: T.Text -> Maybe ScopedName -> RDFLabel
toLbl sVal (Just dtype) = TypedLit sVal dtype
toLbl sVal _            = Lit sVal
    
testToConv :: 
  (ToRDFLabel a, Eq a, Show a) 
  => String -> T.Text -> Maybe ScopedName -> a -> Test
testToConv lbl sVal dtype hVal = 
    testEq (":tconv:" ++ lbl) (toLbl sVal dtype) $ toRDFLabel hVal
  
testFrConv :: 
  (FromRDFLabel a, Eq a, Show a) 
  => String -> T.Text -> Maybe ScopedName -> a -> Test
testFrConv lbl sVal dtype hVal = 
    testEq (":fconv:" ++ lbl) (Just hVal) $ fromRDFLabel (toLbl sVal dtype)
  
testConv :: 
  (ToRDFLabel a, FromRDFLabel a, Eq a, Show a) 
  => String -> T.Text -> Maybe ScopedName -> a -> Test    
testConv lbl sVal dtype hVal = 
  TestList [ 
    testToConv lbl sVal dtype hVal,
    testFrConv lbl sVal dtype hVal         
    ]
  
-- get an integer that is larger than can be stored in an Int
-- (could just pick a value bigger than maxBound seen on a 64-bit build
--  but try to be fancy)
--
bigInt :: T.Text
bigInt = let bi :: Integer
             bi = fromIntegral (maxBound :: Int) + 11
         in T.pack $ show bi

-- some conversions (e.g. toRDFTriple) are covered by  
-- other tests
--
testConversionSuite :: Test
testConversionSuite =
  TestList
  [
    -- failure case
    testEq "fconv:fail chr1"    (Nothing :: Maybe Char)   (fromRDFLabel l1)
  , testEq "fconv:fail chr2"    (Nothing :: Maybe Char)   (fromRDFLabel s1)
  , testEq "fconv:fail str1"    (Nothing :: Maybe String) (fromRDFLabel (TypedLit "1.23" xsdFloat))
  , testEq "fconv:fail bool1"   (Nothing :: Maybe Bool)  (fromRDFLabel l1)
  , testEq "fconv:fail bool2"   (Nothing :: Maybe Bool)  (fromRDFLabel (TypedLit "True" xsdBoolean)) -- should we just let this be valid?
  , testEq "fconv:fail bool3"   (Nothing :: Maybe Bool)  (fromRDFLabel (TypedLit "true" xsdFloat))
  , testEq "fconv:fail int1"    (Nothing :: Maybe Int)  (fromRDFLabel l1)
  , testEq "fconv:fail int2"    (Nothing :: Maybe Int)  (fromRDFLabel (TypedLit bigInt xsdInteger)) 
  , testEq "fconv:fail float1"  (Nothing :: Maybe Float)  (fromRDFLabel l1)
  , testEq "fconv:fail float2"  (Nothing :: Maybe Float)  (fromRDFLabel (TypedLit "1.234e101" xsdFloat)) -- invalid input 
  , testEq "fconv:fail float3"  (Nothing :: Maybe Float)  (fromRDFLabel (TypedLit "-1.234e101" xsdFloat)) -- invalid input 
  , testEq "fconv:fail float4"  (Nothing :: Maybe Float)  (fromRDFLabel (TypedLit "NaNs" xsdFloat)) -- invalid input 
  , testEq "fconv:fail dbl1"    (Nothing :: Maybe Double)  (fromRDFLabel (TypedLit "1.23" xsdFloat)) -- invalid input 
  , testEq "fconv:fail sn1"     (Nothing :: Maybe ScopedName) (fromRDFLabel l1)
  , testEq "fconv:fail qn1"     (Nothing :: Maybe QName)      (fromRDFLabel l1)
  , testEq "fconv:fail qu1"     (Nothing :: Maybe URI)        (fromRDFLabel l1)
  , testEq "fconv:fail triple"  (Nothing :: Maybe (ScopedName, ScopedName, Int)) (fromRDFTriple t01)
                                    
    -- basic string tests
  , testEq     "tconv:emptystring1"  (Lit "")    ""       -- want to try out IsString so do not use testToConv
  , testConv   "emptystring2"        ""                  Nothing    (""::String)
  , testConv   "char"                "x"                 Nothing    'x'
  , testToConv "l1-1"                "l1"                Nothing    l1
  , testConv   "l1-2"                "l1"                Nothing    ("l1"::String)
    
    -- boolean
  , testFrConv  "True1"    "1"       (Just xsdBoolean)   True
  , testFrConv  "False0"   "0"       (Just xsdBoolean)   False
  , testConv    "True"     "true"    (Just xsdBoolean)   True
  , testConv    "False"    "false"   (Just xsdBoolean)   False
    
    {-
For example, -1E4, 1267.43233E12, 12.78e-2, 12 , -0, 0 and INF are all legal literals for float.
For example, -1E4, 1267.43233E12, 12.78e-2, 12 , -0, 0 and INF are all legal literals for double.

Valid values for xsd:integer include -123456789012345678901234567890, 2147483647, 0, or -0000000000000000000005.


     -}
    
    -- numeric types
  , testConv   "int 0"    "0"       (Just xsdInteger) (0::Int)
  , testConv   "int -10"  "-10"     (Just xsdInteger) ((-10)::Int)
  , testConv   "int 10"   "10"      (Just xsdInteger) (10::Int)
  , testConv   "integer 0"    "0"       (Just xsdInteger) (0::Integer)
  , testConv   "integer -10"  "-10"     (Just xsdInteger) ((-10)::Integer)
  , testConv   "integer 10"   "10"      (Just xsdInteger) (10::Integer)
  , testFrConv "integer -0..05" "-0000000000000000000005" (Just xsdInteger)  ((-5)::Integer)
  , testConv   "integer big"  "123456789012345678901234567890"      (Just xsdInteger) (123456789012345678901234567890::Integer)
  , testConv   "integer -big" "-123456789012345678901234567890"     (Just xsdInteger) ((-123456789012345678901234567890)::Integer)
  
  , testToConv "float NaN"    "NaN"          (Just xsdFloat)   ((0.0::Float)/0.0)
  , testToConv "float INF"    "INF"          (Just xsdFloat)   ((1.0::Float)/0.0)
  , testToConv "float -INF"   "-INF"         (Just xsdFloat)   (((-1.0)::Float)/0.0)
  , testEq     ":fconv:float NaN"  (Just True)  (fmap isNaN (fromRDFLabel nanF :: Maybe Float))
  , testEq     ":fconv:float INF"  (Just True)  (fmap isInfinite (fromRDFLabel infF :: Maybe Float))
  , testEq     ":fconv:float -INF" (Just True)  (fmap isInfinite (fromRDFLabel ninfF :: Maybe Float))
  
  , testToConv "double NaN"    "NaN"          (Just xsdDouble)   ((0.0::Double)/0.0)
  , testToConv "double INF"    "INF"          (Just xsdDouble)   ((1.0::Double)/0.0)
  , testToConv "double -INF"   "-INF"         (Just xsdDouble)   (((-1.0)::Double)/0.0)
  , testEq     ":fconv:double NaN"  (Just True)  (fmap isNaN (fromRDFLabel nanD :: Maybe Double))
  , testEq     ":fconv:double INF"  (Just True)  (fmap isInfinite (fromRDFLabel infD :: Maybe Double))
  , testEq     ":fconv:double -INF" (Just True)  (fmap isInfinite (fromRDFLabel ninfD :: Maybe Double))
  
  , testFrConv "float 0.0"      "0.0"       (Just xsdFloat)   (0::Float)
  , testToConv "float 0.0"      "0.0E0"     (Just xsdFloat)   (0::Float)
  , testFrConv "float 0."       "0."        (Just xsdFloat)   (0::Float)
  , testFrConv "float -0"       "-0"        (Just xsdFloat)   (0::Float)
  , testFrConv "float 0.2"      "0.2"       (Just xsdFloat) (0.2::Float)
  , testToConv "float 0.2"      "2.0E-1"    (Just xsdFloat) (0.2::Float)
  , testFrConv "float -0.2"     "-0.2"      (Just xsdFloat) ((-0.2)::Float)
  , testToConv "float -0.2"     "-2.0E-1"   (Just xsdFloat) ((-0.2)::Float)
  , testConv   "float 2.01e-4"  "2.01E-4"   (Just xsdFloat) (0.000201::Float)
  , testConv   "float -2.01e-4" "-2.01E-4"  (Just xsdFloat) ((-0.000201)::Float)
  , testConv   "float 2.01e38"  "2.01E38"   (Just xsdFloat) (2.01e38::Float)
  , testConv   "float -2.01e38" "-2.01E38"  (Just xsdFloat) ((-2.01e38)::Float)
    
  , testFrConv "double 0"        "0.0"     (Just xsdDouble) (0::Double)
  , testToConv "double 0"        "0.0E0"   (Just xsdDouble) (0::Double)
  , testFrConv "double 0."       "0."      (Just xsdDouble)   (0::Double)
  , testFrConv "double -0"       "-0"      (Just xsdDouble)   (0::Double)
  , testFrConv "double 0.2"      "0.2"     (Just xsdDouble) (0.2::Double)
  , testToConv "double 0.2"      "2.0E-1"  (Just xsdDouble) (0.2::Double)
  , testFrConv "double -0.2"     "-0.2"    (Just xsdDouble) ((-0.2)::Double)
  , testToConv "double -0.2"     "-2.0E-1" (Just xsdDouble) ((-0.2)::Double)
  , testFrConv "double 2.01e-4"  "2.01e-4"  (Just xsdDouble) (0.000201::Double)
  , testToConv "double 2.01e-4"  "2.01E-4"  (Just xsdDouble) (0.000201::Double)
  , testFrConv "double -2.01e-4" "-2.01e-4" (Just xsdDouble) ((-0.000201)::Double)
  , testToConv "double -2.01e-4" "-2.01E-4" (Just xsdDouble) ((-0.000201)::Double)
  , testConv   "double 2.01e38"  "2.01E38"  (Just xsdDouble) (2.01e38::Double)
  , testConv   "double -2.01e38" "-2.01E38" (Just xsdDouble) ((-2.01e38)::Double)
  , testConv   "double 2.01e108"  "2.01E108"  (Just xsdDouble) (2.01e108::Double)
  , testConv   "double -2.01e108" "-2.01E108" (Just xsdDouble) ((-2.01e108)::Double)
    
    -- URI related types
  , testEq "tconv:sname s1"    s1             (toRDFLabel qb1s1)
  , testEq "fconv:sname s1"    (Just qb1s1)   (fromRDFLabel s1)
    
  , testEq "tconv:qname s1"    s1             (toRDFLabel qn1s1)
  , testEq "fconv:qname s1"    (Just qn1s1)   (fromRDFLabel s1)
    
  , testEq "tconv:URI s1"      s1             (toRDFLabel qu1s1)
  , testEq "fconv:URI s1"      (Just qu1s1)   (fromRDFLabel s1)
    
    -- time values
  , testConv   "time1"   "1970-01-01T00:00:00Z"            (Just xsdDateTime)  utc1
  , testToConv "time2"   "2011-02-28T20:04:02.304Z"        (Just xsdDateTime)  utc2
  , testFrConv "time2a"  "2011-02-28T20:04:02.304Z"        (Just xsdDateTime)  utc2
  , testFrConv "time2b"  "2011-02-28T17:04:02.304-03:00"   (Just xsdDateTime)  utc2
  , testFrConv "time2c"  "2011-03-01T00:04:02.304+04:00"   (Just xsdDateTime)  utc2
  , testFrConv "time2d"  "2011-02-28T20:04:02.304"         (Just xsdDateTime)  utc2
  , testConv   "time2Z"  "2011-02-28T20:04:02.304Z"        (Just xsdDateTime)  utc2
                              
  , testConv   "day1a"   "1970-01-01Z"                     (Just xsdDate)      day1
  , testFrConv "day1b"   "1970-01-01"                      (Just xsdDate)      day1
  , testFrConv "day1c"   "1970-01-01-03:00"                (Just xsdDate)      day1
  , testFrConv "day1d"   "1970-01-01+04:00"                (Just xsdDate)      day1
    
    -- basic fromRDFTriple test
    
  , testEq   "fconv:triple" (Just (qb1s1, p1, "l1" :: String))     (fromRDFTriple t04)
    
    -- TODO
    
  ]
  
utc1, utc2 :: UTCTime
#if MIN_VERSION_time(1,15,0)
utc1 = fromJust (parseTimeM False defaultTimeLocale "" "")
#elif MIN_VERSION_time(1,6,0)
utc1 = fromJust (buildTime defaultTimeLocale [])
#else
utc1 = buildTime defaultTimeLocale []
#endif
utc2 =
  let dNum = fromGregorian 2011 2 28
      tDiff = (23.0 - 3.0) * 3600.0 + 4.0 * 60.0 + 2.304 
  in UTCTime dNum tDiff
     
day1 :: Day
day1 = fromGregorian 1970 1 1

------------------------------------------------------------
--  RDFLabel classification tests
------------------------------------------------------------

testClass :: String -> (RDFLabel -> Bool) -> RDFLabel -> Bool -> Test
testClass lab clsf nod eq = testCompare "testClass:" lab eq (clsf nod)

altIsXmlLit :: RDFLabel -> Bool
altIsXmlLit = isDatatyped rdfXMLLiteral

testNodeClassSuite :: Test
testNodeClassSuite = TestList
  [ testClass "testClass01" isUri            s1  True
  , testClass "testClass02" isUri            s5  False
  , testClass "testClass03" isUri            ba1 False
  , testClass "testClass04" isUri            l1  False
  , testClass "testClass05" isUri            l10 False
  , testClass "testClass06" isUri            cm1 True
  , testClass "testClass07" isUri            nm1 True
  , testClass "testClass08" isUri            v1  False

  , testClass "testClass10" isLiteral        s1  False
  , testClass "testClass11" isLiteral        s5  False
  , testClass "testClass12" isLiteral        ba1 False
  , testClass "testClass13" isLiteral        l1  True
  , testClass "testClass14" isLiteral        l4  True
  , testClass "testClass15" isLiteral        l5  True
  , testClass "testClass16" isLiteral        l10 True
  , testClass "testClass17" isLiteral        l11 True
  , testClass "testClass18" isLiteral        cm1 False
  , testClass "testClass19" isLiteral        v1  False

  , testClass "testClass20" isTypedLiteral   s1  False
  , testClass "testClass21" isTypedLiteral   s5  False
  , testClass "testClass22" isTypedLiteral   ba1 False
  , testClass "testClass23" isTypedLiteral   l1  False
  , testClass "testClass24" isTypedLiteral   l2  False
  , testClass "testClass25" isTypedLiteral   l4  True
  , testClass "testClass26" isTypedLiteral   l5  True
  , testClass "testClass27" isTypedLiteral   l10 True
  , testClass "testClass28" isTypedLiteral   l11 True
  , testClass "testClass29" isTypedLiteral   v1  False

  , testClass "testClass30" isUntypedLiteral s1  False
  , testClass "testClass31" isUntypedLiteral s5  False
  , testClass "testClass32" isUntypedLiteral ba1 False
  , testClass "testClass33" isUntypedLiteral l1  True
  , testClass "testClass34" isUntypedLiteral l2  True
  , testClass "testClass35" isUntypedLiteral l4  False
  , testClass "testClass36" isUntypedLiteral l5  False
  , testClass "testClass37" isUntypedLiteral l10 False
  , testClass "testClass38" isUntypedLiteral l11 False
  , testClass "testClass39" isUntypedLiteral v1  False

  , testClass "testClass40" isXMLLiteral     s1  False
  , testClass "testClass41" isXMLLiteral     s5  False
  , testClass "testClass42" isXMLLiteral     ba1 False
  , testClass "testClass43" isXMLLiteral     l1  False
  , testClass "testClass44" isXMLLiteral     l2  False
  , testClass "testClass45" isXMLLiteral     l4  False
  , testClass "testClass46" isXMLLiteral     l5  False
  , testClass "testClass47" isXMLLiteral     l10 True
  , testClass "testClass48" isXMLLiteral     l11 True
  , testClass "testClass49" isXMLLiteral     v1  False

  , testClass "testClass50" altIsXmlLit      s1  False
  , testClass "testClass51" altIsXmlLit      s5  False
  , testClass "testClass52" altIsXmlLit      ba1 False
  , testClass "testClass53" altIsXmlLit      l1  False
  , testClass "testClass54" altIsXmlLit      l2  False
  , testClass "testClass55" altIsXmlLit      l4  False
  , testClass "testClass56" altIsXmlLit      l5  False
  , testClass "testClass57" altIsXmlLit      l10 True
  , testClass "testClass58" altIsXmlLit      l11 True

  , testClass "testClass60" isMemberProp     s1  False
  , testClass "testClass61" isMemberProp     s5  False
  , testClass "testClass62" isMemberProp     ba1 False
  , testClass "testClass63" isMemberProp     l1  False
  , testClass "testClass64" isMemberProp     l10 False
  , testClass "testClass65" isMemberProp     cm1 True
  , testClass "testClass66" isMemberProp     cm2 True
  , testClass "testClass67" isMemberProp     nm1 False
  , testClass "testClass68" isMemberProp     nm2 False

  , testClass "testClass70" isBlank          s7  False
  , testClass "testClass71" isBlank          s5  True
  , testClass "testClass72" isBlank          ba1 True
  , testClass "testClass73" isBlank          l1  False
  , testClass "testClass74" isBlank          l4  False
  , testClass "testClass75" isBlank          l5  False
  , testClass "testClass76" isBlank          l10 False
  , testClass "testClass77" isBlank          l11 False
  , testClass "testClass78" isBlank          cm1 False
  , testClass "testClass79" isBlank          v1  False

  , testClass "testClass80" isQueryVar       s8  False
  , testClass "testClass81" isQueryVar       s5  False
  , testClass "testClass82" isQueryVar       ba1 False
  , testClass "testClass83" isQueryVar       l1  False
  , testClass "testClass84" isQueryVar       l4  False
  , testClass "testClass85" isQueryVar       l5  False
  , testClass "testClass86" isQueryVar       l10 False
  , testClass "testClass87" isQueryVar       l11 False
  , testClass "testClass88" isQueryVar       cm1 False
  , testClass "testClass89" isQueryVar       v1  True
    
  ]

------------------------------------------------------------
--  RDFLabel local part separation and recombination tests
------------------------------------------------------------

testLocalEq :: String -> String -> String -> Test
testLocalEq = testCompare "testLocalEq:"

testLocalLabEq :: String -> RDFLabel -> RDFLabel -> Test
testLocalLabEq = testCompare "testLocalEq:"

testNodeLocalSuite :: Test
testNodeLocalSuite = TestList
  [ testLocalEq    "01" "b1"  (getLocal b1)
  , testLocalEq    "02" "b2"  (getLocal b2)
  , testLocalEq    "03" "?v1" (getLocal v1)
  , testLocalEq    "04" "?v2" (getLocal v2)
  , testLocalLabEq "05" b1    (makeLabel "b1")
  , testLocalLabEq "06" b2    (makeLabel "b2")
  , testLocalLabEq "07" v1    (makeLabel "?v1")
  , testLocalLabEq "08" v2    (makeLabel "?v2")
  ] 
  
------------------------------------------------------------
--  Node generation tests
------------------------------------------------------------

take3 :: [a] -> (a, a, a)
take3 (a : b : c : _) = (a, b, c)
take3 _ = error "Expected at least 3 elements"


testNodeEq :: String -> RDFLabel -> RDFLabel -> Test
testNodeEq = testCompare "testNodeEq:"

tnn01, tnn02 :: RDFLabel
tnn01 = newNode  v1 [b1,b3,v1,v2]
tnn02 = newNode  b1 [b1,b3,v1,v2]

tnn03, tnn04, tnn05 :: RDFLabel
(tnn03, tnn04, tnn05) = take3 $ newNodes b1 [b1, b3, v1, v2]

tnn06, tnn07, tnn08 :: RDFLabel
(tnn06, tnn07, tnn08) = take3 $ newNodes s1 [b1,b3,v1,v2,tnns3]

tnn09 :: RDFLabel
tnn09 = newNodes l1 [b1,b3,v1,v2,tnns3] !! 2

tnns1, tnns2, tnns3, tnns4, tnnl1 :: RDFLabel
tnns1 = Blank "Res_s1"
tnns2 = Blank "Res_s2"
tnns3 = Blank "Res_s3"
tnns4 = Blank "Res_s4"
tnnl1 = Blank "Lit_2"

testNewNodeSuite :: Test
testNewNodeSuite = TestList
  [ testNodeEq "testNewNode01" v3    tnn01
  , testNodeEq "testNewNode02" b2    tnn02
  , testNodeEq "testNewNode03" b2    tnn03
  , testNodeEq "testNewNode04" b4    tnn04
  , testNodeEq "testNewNode05" b5    tnn05
  , testNodeEq "testNewNode06" tnns1 tnn06
  , testNodeEq "testNewNode07" tnns2 tnn07
  , testNodeEq "testNewNode08" tnns4 tnn08
  , testNodeEq "testNewNode09" tnnl1 tnn09
  ]

------------------------------------------------------------
--  RDFLabel ordering tests
------------------------------------------------------------

testLabelOrd :: String -> Ordering -> RDFLabel -> RDFLabel -> Test
testLabelOrd lab order n1 n2 =
    TestCase ( assertEqual
               ("testLabelOrd:"++lab++"["++show n1++","++show n2++"]")
               order (compare n1 n2) )

nodeorder :: [String]
nodeorder =
  [ 
  -- URIs
  "es1", "us1"
  , "o1", "p1", "s1"
  , "o2", "p2", "s2"
  , "s4", "o4", "s6", "s7"
  , "o3", "p3", "p4", "s3"
    -- literals
  , "l1"
    -- language literals
  , "l2", "l2gb", "l3"
    -- typed literals (we have repeats)
  , "l10", "l11", "l12"
  , "l4", "l5", "l6"
  , "l7", "l8", "l9"
  -- blank nodes
  , "b1", "b2", "b3", "b4"
  , "o5", "s5", "s8"
  -- variables
  , "v1", "v2"
  ]

testNodeOrdSuite :: Test
testNodeOrdSuite = TestList
  [ testLabelOrd (ll1 `hyphen` ll2) (tOrd ll1 ll2) n1 n2
      | (ll1,n1) <- nodelist , (ll2,n2) <- nodelist ]
    where
    tOrd ll1 ll2
      | tEq ll1 ll2  = EQ
      | otherwise    = comparing fromJust 
                       (elemIndex ll1 nodeorder)
                       (elemIndex ll2 nodeorder)
    tEq  ll1 ll2 = (ll1 == ll2)        ||
           (ll1,ll2) `elem` nodeeqlist ||
           (ll2,ll1) `elem` nodeeqlist

------------------------------------------------------------
--  Other RDFLabel tests
------------------------------------------------------------

testLabelOtherSuite :: Test
testLabelOtherSuite = TestList
    [ testEq "testLabelName01" (getScopedName s1) qb1s1
    , testEq "testLabelName02" (getScopedName b1) nullScopedName
    , testEq "testLabelName03" (getScopedName l1) nullScopedName
    , testEq "testLabelName04" (getScopedName v1) nullScopedName
    ]

------------------------------------------------------------
--  Statement construction and equality tests
------------------------------------------------------------

testStmtEq :: String -> Bool -> RDFTriple -> RDFTriple -> Test
testStmtEq = testCompareEq "testStmtEq:"

slist, plist, olist :: [(String, RDFLabel)]
slist =
  [
    ("s1",s1), ("s4",s4), ("s5",s5), ("s6",s6), ("s7",s7)
  ]

plist =
  [
    ("p1",p1)
  ]

olist =
  [ ("o1",o1), ("o4",o4), ("o5",o5),
    ("l1",l1), ("l4",l4), ("l7",l7), ("l8",l8), ("l10",l10)
  ]

tlist :: [(String, RDFTriple)]
tlist =
  [ (lab s p o,trp s p o) | s <- slist, p <- plist, o <- olist ]
    where
    lab (s,_) (p,_) (o,_) = intercalate ("."::String) [s, p, o]
    trp (_,s) (_,p) (_,o) = arc s p o

stmteqlist :: [(String, String)]
stmteqlist =
  [
    ("s6.p1.l1", "s7.p1.l1"),
    ("s6.p1.l4", "s7.p1.l4"),
    ("s6.p1.l7", "s7.p1.l7"),
    ("s6.p1.l7", "s7.p1.l8"),
    ("s6.p1.l8", "s7.p1.l7"),
    ("s6.p1.l8", "s7.p1.l8"),
    ("s6.p1.l10","s7.p1.l10"),
    ("s6.p1.o1", "s7.p1.o1"),
    ("s6.p1.o4", "s7.p1.o4"),
    ("s6.p1.o5", "s7.p1.o5"),
    ("s1.p1.l7", "s1.p1.l8"),
    ("s4.p1.l7", "s4.p1.l8"),
    ("s5.p1.l7", "s5.p1.l8"),
    ("s6.p1.l7", "s6.p1.l8"),
    ("s7.p1.l7", "s7.p1.l8")
  ]

testStmtEqSuite :: Test
testStmtEqSuite = TestList
  [ testStmtEq (ll1 `hyphen` ll2) (tEq  ll1 ll2) t1 t2
      | (ll1,t1) <- tlist , (ll2,t2) <- tlist ]
    where
    tEq  ll1 ll2 = (ll1 == ll2)        ||
            (ll1,ll2) `elem` stmteqlist ||
            (ll2,ll1) `elem` stmteqlist

------------------------------------------------------------
--  Graph construction and equality tests
------------------------------------------------------------

testGraphEq :: String -> Bool -> RDFGraph -> RDFGraph -> Test
testGraphEq lab eq gg1 gg2 =
    --  Set test False to get extra trace info about graph differences
    --  Some tests will fail with this setting, so revert to True to
    --  get test result.
    if True then
        testCompareEq "testGraphEq:" lab eq gg1 gg2
    else
        TestList
            [ TestCase ( assertEqual ("testGraphEq:"++lab) eq (gg1==gg2) )
            , TestCase ( assertEqual ("testGraphEq:"++lab) gg1 gg2 )
            ]

testGraphEqM :: String -> Bool -> Maybe RDFGraph -> Maybe RDFGraph -> Test
testGraphEqM = testCompareEq "testGraphEq:"

t01, t02, t03, t04, t05, t06 :: RDFTriple
t01 = arc s1 p1 o1
t02 = arc s2 p1 o2
t03 = arc s3 p1 o3
t04 = arc s1 p1 l1
t05 = arc s2 p1 l4
t06 = arc s3 p1 l10

t10, t11, t12 :: RDFTriple
t10 = arc s1 p1 b1
t11 = arc b1 p2 b2
t12 = arc b2 p3 o1

t20, t21, t22 :: RDFTriple
t20 = arc s1 p1 b3
t21 = arc b3 p2 b4
t22 = arc b4 p3 o1

tt01, tt02, tt03, tt04, tt05, tt06 :: RDFTriple
tt01 = arc st1 p1 o1
tt02 = arc st2 p1 o2
tt03 = arc st3 p1 o3
tt04 = toRDFTriple st1 p1 ("l1" :: RDFLabel)
-- tt04 = arc st1 p1 l1
tt05 = arc st2 p1 l4
tt06 = arc st3 p1 l10

makeNewPrefixNamespace :: (T.Text,Namespace) -> Namespace
makeNewPrefixNamespace (pre,ns) = makeNamespace (Just pre) (getNamespaceURI ns)

nslist :: NamespaceMap
nslist = M.fromList $ map getNamespaceTuple [base1,base2,base3,base4]

nslistalt :: NamespaceMap
nslistalt = M.fromList $ map (first (fmap (T.append "alt")) . getNamespaceTuple) [base1,base2,base3]

toGraph :: [RDFTriple] -> RDFGraph
toGraph stmts = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        , statements = S.fromList stmts
        }

g1, gt1 :: RDFGraph
g1  = toGraph [t01]
gt1 = toGraph [tt01]

-- Check for nonsensitivety of graph equility to namespace differences:

g1alt :: RDFGraph
g1alt = NSGraph
        { namespaces = nslistalt
        , formulae   = emptyFormulaMap
        , statements = S.singleton t01
        }

--  Construct version of g1 using just URIs

uris1, urip1, urio1 :: ScopedName
uris1 = "http://id.ninebynine.org/wip/2003/test/graph1/node#s1"
urip1 = "http://id.ninebynine.org/wip/2003/test/graph1/node#p1"
urio1 = "http://id.ninebynine.org/wip/2003/test/graph1/node#o1"

tu01 :: RDFTriple
tu01  = toRDFTriple uris1 urip1 urio1

g2arcs :: [String]
g2arcs = 
    [ "(base1:s1,base1:p1,base1:o1)"
    , "(base1:s1,base1:p1,\"l1\")"
    , "(base2:s2,base1:p1,base2:o2)"
    , "(base2:s2,base1:p1,\"l4\"^^base1:type1)" 
    , "(base3:s3,base1:p1,base3:o3)"
    , "(base3:s3,base1:p1,\"l10\"^^rdf:XMLLiteral)"
    ]
  
g2str :: String -> String
g2str sp = 
  let spaces = "    "
  in intercalate ('\n':sp) $ 
     ["Graph, formulae: ",
      "arcs: "]
     ++ map (spaces++) g2arcs
     
g2Labels :: [RDFTriple]
g2Labels = [t01,t02,t03,t04,t05,t06]

g1uri, g2, gt2, g3, gt3, g4, g5, g6, g7, g8, g9, g10 :: RDFGraph
g1uri = toGraph [tu01]
g2    = toGraph g2Labels
gt2   = toGraph [tt01,tt02,tt03,tt04,tt05,tt06]
g3    = toGraph [t06,t05,t04,t03,t02,t01]
gt3   = toGraph [tt06,tt05,tt04,tt03,tt02,tt01]
g4    = toGraph [t01,t02,t03,t04,t05,t06,t10,t11,t12]
g5    = toGraph [t01,t02,t03,t04,t05,t06,t20,t21,t22]
g6    = toGraph [t01,t02,t03,t04,t05,t06,t10,t11,t12,t20,t21,t22]
g7    = toGraph [t01,t02]
g8    = toGraph [t02,t01]
g9    = toGraph [t03,t02,t01]
g10   = toGraph [t02,t02,t01]

fg1g2 :: RDFGraph
fg1g2 = g1 `mappend` g2

g9a, g10a :: RDFGraph
g9a  = addArc t03 g8
g10a = addArc t02 g8

glist :: [(String, RDFGraph)]
glist =
  [ ("g1",g1), ("g1alt",g1alt), ("g1uri",g1uri)
  , ("g2",g2), ("g3",g3), ("g4",g4), ("g5",g5), ("g6",g6)
  , ("g7",g7), ("g8",g8), ("g9",g9), ("g10",g10)
  , ("g9a",g9a), ("g10a",g10a)
  ]

grapheqlist :: [(String, String)]
grapheqlist =
  [ ("g1","g1alt")
  , ("g1","g1uri")
  , ("g1alt","g1uri")
  , ("g2","g3")
  , ("g4","g5")
  , ("g7","g8")
  , ("g7","g10")
  , ("g7","g10a")
  , ("g8","g10")
  , ("g8","g10a")
  , ("g9","g9a")
  , ("g10","g10a")
  ]

testGraphEqSuite :: Test
testGraphEqSuite = TestList
  [ testGraphEq (ll1 `hyphen` ll2) (tEq ll1 ll2) gg1 gg2
      | (ll1,gg1) <- glist , (ll2,gg2) <- glist ]
    where
    tEq  ll1 ll2 = (ll1 == ll2)        ||
            (ll1,ll2) `elem` grapheqlist ||
            (ll2,ll1) `elem` grapheqlist

-- Selected tests for debugging

testGraphEqSelSuite :: Test
testGraphEqSelSuite = TestList
  [ testGraphEq "g1-g2" False g1 g2
  , testGraphEq "g2-g1" False g2 g1
  , testGraphEq "g2-g2" True  g2 g2
  , testGraphEq "g2-g3" True  g2 g3
  , testGraphEq "g1-g4" False g1 g4
  , testGraphEq "g2-g4" False g2 g4
  , testGraphEq "g7-g7" True  g7 g7
  , testGraphEq "g7-g8" True  g7 g8
  , testGraphEq "g8-g7" True  g8 g7
  , testGraphEq "g9-g9a"   True g9 g9a
  , testGraphEq "g10-g10a" True g10 g10a
  ]

{-
showLabel :: RDFLabel -> String
showLabel = (" " ++) . show
-}

-- QUS: are these tests useful, since all they test os the Monoid instance
-- of NSGraph (but maybe we do not explicitly test this elsewhere?)
--
-- The tests also used to check the Foldable instance of NSGraph but these
-- have been removed with the move to Set, rather than list, storage.
testGraphFoldSuite :: Test
testGraphFoldSuite = TestList
  [ 
    testEq "fold0"    (mempty :: RDFGraph) (Foldable.fold [])
  , testEq "foldE"    (mempty :: RDFGraph) (Foldable.fold [mempty])
  , testEq "foldEE"   (mempty :: RDFGraph) (Foldable.fold [mempty,mempty])
  , testEq "foldg1"   g1                   (Foldable.fold [g1])
  , testEq "foldg1E"  g1                   (Foldable.fold [g1,mempty])
  , testEq "foldEg1"  g1                   (Foldable.fold [mempty,g1])
  , testEq "foldg1g2" fg1g2                (Foldable.fold [g1,g2])
  , testEq "foldg2g1" fg1g2                (Foldable.fold [g2,g1])
{- TODO: reinstate?
  , testEq "foldMap0" ""                   (Foldable.foldMap showLabel (mempty::RDFGraph))
  , testEq "foldMapg1"                    
    (concatMap showLabel [s1,p1,o1])
    (Foldable.foldMap showLabel g1)
  , testEq "foldMapg1f2"                    
    (concatMap showLabel $ s2 : concatMap arcToTriple g2Labels ++ [s1,p1,o1])
    (Foldable.foldMap showLabel g1f2)
-}
  ]
  
------------------------------------------------------------
--  Test updating formulae
------------------------------------------------------------

testFormulaLookup ::
    String -> FormulaMap RDFLabel -> RDFLabel -> Maybe RDFGraph -> Test
testFormulaLookup lab fs fl gr =
  testCompare "testFormulaLookup:" lab gr $ M.lookup fl fs
  -- testCompare "testFormulaLookup:" lab gr $ fmap formGraph $ M.lookup fl fs

testMaybeEq :: (Eq a, Show a) => String -> Maybe a -> Maybe a -> Test
testMaybeEq = testCompare "testMaybeEq:"

g1f1, g1f2, g1f3, g1f4, g1f5, g1f6, g1f7 :: RDFGraph
g1f1 = g1
g1f2 = setFormulae fm2 g1f1
g1f3 = setFormulae fm3 g1f1
g1f4 = setFormulae fm4 g1f1
g1f5 = setFormulae fm5 g1f1
g1f6 = setFormulae fm6 g1f1
g1f7 = setFormulae fm7 g1f1


g1f1str, g1f2str :: String

g1f1str = 
  "Graph, formulae: \n" ++
  "arcs: \n" ++
  "    (base1:s1,base1:p1,base1:o1)"

g1f2str =
  "Graph, formulae: \n    " ++
  lf22str ++ "\n" ++
  "arcs: \n" ++
  "    (base1:s1,base1:p1,base1:o1)"

lf11, lf22, lf23, lf24, lf25, lf27, lf33, lf36 :: Formula RDFLabel
lf11 = Formula s1 g1
lf22 = Formula s2 g2
lf23 = Formula s2 g3
lf24 = Formula s2 g4
lf25 = Formula s2 g5
lf27 = Formula s2 g7
lf33 = Formula s3 g3
lf36 = Formula s3 g6

lf22str :: String
lf22str =
  "base2:s2 :- { \n" ++ 
  "        (base1:s1,base1:p1,base1:o1)\n" ++ 
  "        (base1:s1,base1:p1,\"l1\")\n" ++ 
  "        (base2:s2,base1:p1,base2:o2)\n" ++ 
  "        (base2:s2,base1:p1,\"l4\"^^base1:type1)\n" ++ 
  "        (base3:s3,base1:p1,base3:o3)\n" ++
  "        (base3:s3,base1:p1,\"l10\"^^rdf:XMLLiteral) }"

toFM :: [Formula RDFLabel] -> FormulaMap RDFLabel
toFM = M.fromList . map (formLabel &&& formGraph)

fm2, fm3, fm4, fm5, fm6, fm7 :: FormulaMap RDFLabel
fm2  = toFM [lf22]
fm3  = toFM [lf11, lf22, lf33]
fm4  = toFM [lf11, lf23, lf33]
fm5  = toFM [lf11, lf24, lf36]
fm6  = toFM [lf11, lf25, lf36]
fm7  = toFM [lf11, lf27, lf36]

f1, f2, f3, f4, f5, f6, f7 :: FormulaMap RDFLabel
f1   = getFormulae g1f1
f2   = getFormulae g1f2
f3   = getFormulae g1f3
f4   = getFormulae g1f4
f5   = getFormulae g1f5
f6   = getFormulae g1f6
f7   = getFormulae g1f7

--  Same pattern as 1-3, but using base graph with more nodes used:
--  The graph comparison results are expected to be different,
--  because of formulae associated with nodes actually used in the
--  graph

g2f1, g2f2, g2f3 :: RDFGraph
g2f1 = g2
g2f2 = setFormulae fm2 g2f1
g2f3 = setFormulae fm3 g2f1

f8, f9, f10 :: FormulaMap RDFLabel
f8   = getFormulae g2f1
f9   = getFormulae g2f2
f10  = getFormulae g2f3

--  Comparison of graphs containing formulae.
--  The intent is that graphs are matched if there is a bijection,
--  where the matched nodes are associated with matching formulae.
--  Definitions of formulae not used in the graphs don't affect the
--  match result.

--  Test methods to set/access an individual formula in a graph

g1f21, g1f22 :: RDFGraph
g1f21 = setFormula  (Formula s1 g7) g1f2
g1f22 = setFormula  (Formula s1 g1) g1f21

f21, f22 :: FormulaMap RDFLabel
f21  = getFormulae g1f21
f22  = getFormulae g1f22

f23a, f23b, f23c :: Maybe RDFGraph
f23a = getFormula g1f22 s1
f23b = getFormula g1f22 s2
f23c = getFormula g1f22 s3

testGraphFormulaSuite :: Test
testGraphFormulaSuite = TestLabel "TestFormulae" $ TestList
  [ testFormulaLookup "01a" f1 s1 Nothing
  , testFormulaLookup "01b" f1 s2 Nothing
  , testFormulaLookup "01c" f1 s3 Nothing
  , testFormulaLookup "02a" f2 s1 Nothing
  , testFormulaLookup "02b" f2 s2 (Just g2)
  , testFormulaLookup "02c" f2 s3 Nothing
  , testFormulaLookup "03a" f3 s1 (Just g1)
  , testFormulaLookup "03b" f3 s2 (Just g2)
  , testFormulaLookup "03c" f3 s3 (Just g3)
  , testFormulaLookup "04a" f4 s1 (Just g1)
  , testFormulaLookup "04b" f4 s2 (Just g3)
  , testFormulaLookup "04c" f4 s3 (Just g3)
  , testFormulaLookup "05a" f5 s1 (Just g1)
  , testFormulaLookup "05b" f5 s2 (Just g4)
  , testFormulaLookup "05c" f5 s3 (Just g6)
  , testFormulaLookup "06a" f6 s1 (Just g1)
  , testFormulaLookup "06b" f6 s2 (Just g5)
  , testFormulaLookup "06c" f6 s3 (Just g6)
  , testFormulaLookup "07a" f7 s1 (Just g1)
  , testFormulaLookup "07b" f7 s2 (Just g7)
  , testFormulaLookup "07c" f7 s3 (Just g6)
  , testFormulaLookup "08a" f8 s1 Nothing
  , testFormulaLookup "08b" f8 s2 Nothing
  , testFormulaLookup "08c" f8 s3 Nothing
  , testFormulaLookup "09a" f9 s1 Nothing
  , testFormulaLookup "09b" f9 s2 (Just g2)
  , testFormulaLookup "09c" f9 s3 Nothing
  , testFormulaLookup "10a" f10 s1 (Just g1)
  , testFormulaLookup "10b" f10 s2 (Just g2)
  , testFormulaLookup "10c" f10 s3 (Just g3)
    
    -- a few tests added in to improve test coverage
  , testEq "lf11" (Formula s1 g1) lf11  
  , testEq "g2:show"    (g2str "") (show g2) 
  , testEq "g1f1:show"  g1f1str (show g1f1) 
  , testEq "g1f2:show"  g1f2str (show g1f2) 
  , testEq "lf22:show" lf22str (show lf22) 
  , testEq "[]:showList"    "[no graphs]" (show ([] :: [RDFGraph])) 
  , testEq "g2:showList1"    ("[" ++ g2str " " ++ "]") (show [g2]) 
  , testEq "g2:showList2"    ("[" ++ g2str " " ++ ",\n " ++ g2str " " ++ "]") (show [g2,g2]) 
    
    -- back to the main schedule
  , testGraphEq "g1f1-g1f1" True  g1f1 g1f1
  , testGraphEq "g1f1-g1f2" True  g1f1 g1f2
  , testGraphEq "g1f1-g1f3" False g1f1 g1f3
  , testGraphEq "g1f2-g1f1" True  g1f2 g1f1
  , testGraphEq "g1f2-g1f2" True  g1f2 g1f2
  , testGraphEq "g1f2-g1f3" False g1f2 g1f3
  , testGraphEq "g1f3-g1f1" False g1f3 g1f1
  , testGraphEq "g1f3-g1f2" False g1f3 g1f2
  , testGraphEq "g1f3-g1f3" True  g1f3 g1f3
  , testGraphEq "g1f4-g1f3" True  g1f4 g1f3
  , testGraphEq "g1f4-g1f4" True  g1f4 g1f4
  , testGraphEq "g1f4-g1f5" True  g1f4 g1f5
  , testGraphEq "g1f5-g1f5" True  g1f5 g1f5
  , testGraphEq "g1f5-g1f6" True  g1f5 g1f6
  , testGraphEq "g1f5-g1f7" True  g1f5 g1f7
  , testGraphEq "g1f6-g1f5" True  g1f6 g1f5
  , testGraphEq "g1f6-g1f6" True  g1f6 g1f6
  , testGraphEq "g1f6-g1f7" True  g1f6 g1f7
  , testGraphEq "g1f7-g1f5" True  g1f7 g1f5
  , testGraphEq "g1f7-g1f6" True  g1f7 g1f6
  , testGraphEq "g1f7-g1f7" True  g1f7 g1f7
  , testGraphEq "g2f1-g2f1" True  g2f1 g2f1
  , testGraphEq "g2f1-g2f2" False g2f1 g2f2
  , testGraphEq "g2f1-g2f3" False g2f1 g2f3
  , testGraphEq "g2f2-g2f1" False g2f2 g2f1
  , testGraphEq "g2f2-g2f2" True  g2f2 g2f2
  , testGraphEq "g2f2-g2f3" False g2f2 g2f3
  , testGraphEq "g2f3-g2f1" False g2f3 g2f1
  , testGraphEq "g2f3-g2f2" False g2f3 g2f2
  , testGraphEq "g2f3-g2f3" True  g2f3 g2f3
  , testFormulaLookup "21a" f21 s1 (Just g7)
  , testFormulaLookup "21b" f21 s2 (Just g2)
  , testFormulaLookup "21c" f21 s3 Nothing
  , testFormulaLookup "22a" f22 s1 (Just g1)
  , testFormulaLookup "22b" f22 s2 (Just g2)
  , testFormulaLookup "22c" f22 s3 Nothing
  , testMaybeEq "23a" f23a (Just g1)
  , testMaybeEq "23b" f23b (Just g2)
  , testMaybeEq "23c" f23c Nothing
  ]

------------------------------------------------------------
--  Test fmap translations of graphs, including formulae
------------------------------------------------------------

translate :: RDFLabel -> RDFLabel
translate lab
    | lab == s1 = st1
    | lab == s2 = st2
    | lab == s3 = st3
    | otherwise = lab

translateM :: RDFLabel -> Maybe RDFLabel
translateM lab
    | lab == s1   = Just st1
    | lab == s2   = Just st2
    | lab == s3   = Just st3
    | isBlank lab = Nothing
    | otherwise   = Just lab

gt1f1a, gt1f1b, gt1f2a, gt1f2b, gt1f3a, gt1f3b,
  gt2f1a, gt2f1b, gt2f2a, gt2f2b,
  gt2f3a, gt2f3b :: RDFGraph
gt1f1a = gt1
gt1f1b = fmapNSGraph translate g1f1
gt1f2a = setFormulae ftm2 gt1
gt1f2b = fmapNSGraph translate g1f2
gt1f3a = setFormulae ftm3 gt1
gt1f3b = fmapNSGraph translate g1f3
gt2f1a = gt2
gt2f1b = fmapNSGraph translate g2f1
gt2f2a = setFormulae ftm2 gt2
gt2f2b = fmapNSGraph translate g2f2
gt2f3a = setFormulae ftm3 gt2
gt2f3b = fmapNSGraph translate g2f3

ft1, ft2, ft3, ft4, ft5, ft6 :: FormulaMap RDFLabel
ft1 = getFormulae gt1f1b
ft2 = getFormulae gt1f2b
ft3 = getFormulae gt1f3b
ft4 = getFormulae gt2f1b
ft5 = getFormulae gt2f2b
ft6 = getFormulae gt2f3b

ftm2, ftm3 :: FormulaMap RDFLabel
ftm2   = toFM [Formula st2 gt2]
ftm3   = toFM [Formula st1 gt1,Formula st2 gt2,Formula st3 gt3]

-- Monadic translate tests, using Maybe Monad

{- TODO: reinstate?

gt1f1aM, gt1f1bM, gt1f2aM, gt1f2bM, gt1f5M :: Maybe RDFGraph
gt1f1aM = Just gt1
gt1f1bM = Traversable.mapM translateM g1f1
gt1f2aM = Just gt1f2a
gt1f2bM = Traversable.mapM translateM g1f2
gt1f5M = Traversable.mapM translateM g1f5

ft1M, ft2M :: FormulaMap RDFLabel
ft1M = getFormulae $ fromMaybe (error "Unexpected: gt1f1bM") gt1f1bM
ft2M = getFormulae $ fromMaybe (error "Unexpected: gt1f2bM") gt1f2bM
--}

testGraphTranslateSuite :: Test
testGraphTranslateSuite = TestLabel "TestTranslate" $ TestList
  [ testGraphEq "gt1f1a-gt1f1b" True gt1f1a gt1f1b
  , testFormulaLookup "GraphTranslate01b" ft1 st1 Nothing
  , testFormulaLookup "GraphTranslate01c" ft1 st2 Nothing
  , testFormulaLookup "GraphTranslate01d" ft1 st3 Nothing
  , testEq "gt1f1a-gt1f1b" gt1f1a gt1f1b
  , testGraphEq "gt1f2a-gt1f2b" True gt1f2a gt1f2b
  , testFormulaLookup "GraphTranslate02b" ft2 st1 Nothing
  , testFormulaLookup "GraphTranslate02c" ft2 st2 (Just gt2)
  , testFormulaLookup "GraphTranslate02d" ft2 st3 Nothing
  , testGraphEq "gt1f3a-gt1f3b" True gt1f3a gt1f3b
  , testFormulaLookup "GraphTranslate03b" ft3 st1 (Just gt1)
  , testFormulaLookup "GraphTranslate03c" ft3 st2 (Just gt2)
  , testFormulaLookup "GraphTranslate03d" ft3 st3 (Just gt3)
  , testGraphEq "gt2f1a-gt2f1b" True gt2f1a gt2f1b
  , testFormulaLookup "GraphTranslate04b" ft4 st1 Nothing
  , testFormulaLookup "GraphTranslate04c" ft4 st2 Nothing
  , testFormulaLookup "GraphTranslate04d" ft4 st3 Nothing
  , testGraphEq "gt2f2a-gt2f2b" True gt2f2a gt2f2b
  , testFormulaLookup "GraphTranslate05b" ft5 st1 Nothing
  , testFormulaLookup "GraphTranslate05c" ft5 st2 (Just gt2)
  , testFormulaLookup "GraphTranslate05d" ft5 st3 Nothing
  , testGraphEq "gt2f3a-gt2f3b" True gt2f3a gt2f3b
  , testFormulaLookup "GraphTranslate06b" ft6 st1 (Just gt1)
  , testFormulaLookup "GraphTranslate06c" ft6 st2 (Just gt2)
  , testFormulaLookup "GraphTranslate06d" ft6 st3 (Just gt3)
--  , testGraphEqM "gt1f1aM-gt1f1bM" True gt1f1aM gt1f1bM      TODO: reinstate?
--  , testFormulaLookup "GraphTranslate07b" ft1M st1 Nothing
--  , testFormulaLookup "GraphTranslate07c" ft1M st2 Nothing
--  , testFormulaLookup "GraphTranslate07d" ft1M st3 Nothing
--  , testEq "gt1f1aM-gt1f1bM" gt1f1aM gt1f1bM
--  , testGraphEqM "gt1f2aM-gt1f2bM" True gt1f2aM gt1f2bM
--  , testFormulaLookup "GraphTranslate08b" ft2M st1 Nothing
--  , testFormulaLookup "GraphTranslate08c" ft2M st2 (Just gt2)
--  , testFormulaLookup "GraphTranslate08d" ft2M st3 Nothing
--  , testEq "gt1f2aM-gt1f2bM" gt1f2aM gt1f1bM
--  , testEq "GraphTranslate09a" Nothing gt1f5M
  ]

------------------------------------------------------------
--  Test merge with conflicting bnodes, including formulae
------------------------------------------------------------

testMerge :: String -> RDFGraph -> RDFGraph -> RDFGraph -> Test
testMerge lab a1 a2 gr =
    TestCase ( assertGrEquiv ("testMerge:"++lab) gr (a1 `mappend` a2) )
            
assertGrHelper :: String -> RDFGraph -> RDFGraph -> Bool -> Assertion
assertGrHelper lbl gg1 gg2 = assertBool $ 
    lbl++"\nExpected: "++ show gg1 ++"\nObtained: "++ show gg2
  
assertGrEquiv :: String -> RDFGraph -> RDFGraph -> Assertion
assertGrEquiv lbl gg1 gg2 = 
  assertGrHelper lbl gg1 gg2 $ getArcs gg1 == getArcs gg2

assertGrEq :: String -> RDFGraph -> RDFGraph -> Assertion
assertGrEq lbl gg1 gg2 = 
  assertGrHelper lbl gg1 gg2 $ gg1 == gg2

tm01, tm02, tm03, tm04, tm05, tm06, tm07, tm08, tm09,
  tm10, tm11, tm12, tm13, tm14 :: RDFTriple
tm01 = arc s1  p1 b1
tm02 = arc b1  p1 o2
tm03 = arc b1  p1 o3
tm04 = arc b2  p2 b3
tm05 = arc b3  p2 b4
tm06 = arc bb  p2 b5
tm07 = arc s2  p3 v1
tm08 = arc s3  p3 v2
tm09 = arc s4  p1 c1
tm10 = arc c2  p1 o4
tm11 = arc s4  p2 ba1
tm12 = arc ba2 p2 o4
tm13 = arc s4  p2 bn3
tm14 = arc bn4 p2 o4

tm21, tm22, tm23, tm24, tm25, tm26, tm27, tm28, tm29,
  tm30, tm31, tm32, tm33, tm34 :: RDFTriple
tm21 = arc s1  p1 b6
tm22 = arc b6  p1 o2
tm23 = arc b6  p1 o3
tm24 = arc b7  p2 b8
tm25 = arc b8  p2 b9
tm26 = arc bb0 p2 b10
tm27 = arc s2  p3 v3
tm28 = arc s3  p3 v4
tm29 = arc s4  p1 c3
tm30 = arc c4  p1 o4
tm31 = arc s4  p2 ba3
tm32 = arc ba4 p2 o4
tm33 = arc s4  p2 bn5
tm34 = arc bn6 p2 o4

tm41, tm42, tm43, tm44 :: RDFTriple
tm41  = arc s1  p1 b2
tm42  = arc b2  p1 o2
tm43  = arc b2  p1 o3
tm44  = arc b4  p2 b5

tm41a, tm44a :: RDFTriple
tm41a = arc s1  p1 b4
tm44a = arc b5  p2 b6

tm67, tm68, tm69, tm70, tm71, tm72,
  tm73, tm74 :: RDFTriple
tm67 = arc s2  p3 v3
tm68 = arc s3  p3 v4
tm69 = arc s4  p1 c3
tm70 = arc c4  p1 o4
tm71 = arc s4  p2 ba3
tm72 = arc ba4 p2 o4
tm73 = arc s4  p2 bn5
tm74 = arc bn6 p2 o4

gm0, gms, gms2, gm1, gm11, gm2, gm2f, gm22, gm3, gm3f, gm33,
  gm4, gm44 :: RDFGraph
gm0  = mempty
gms  = toGraph [arc s1 p1 o1, arc o1 p2 s3, arc s2 p3 o4]
gms2 = toGraph [arc us1 p1 o1, arc p1 p2 es1]
gm1  = toGraph [tm01,tm02,tm03,tm04,tm05,tm06,tm07,tm08
               ,tm09,tm10,tm11,tm12,tm13,tm14
               ]
gm11 = toGraph [tm01,tm02,tm03,tm04,tm05,tm06,tm07,tm08
               ,tm09,tm10,tm11,tm12,tm13,tm14
               ,tm21,tm22,tm23,tm24,tm25,tm26,tm27,tm28
               ,tm29,tm30,tm31,tm32,tm33,tm34
               ]
gm2  = toGraph [tm01]
gm2f = toGraph [tm41]
gm22 = toGraph [tm01,tm41]
gm3  = toGraph [tm04]
gm3f = toGraph [tm44]
gm33 = toGraph [tm04,tm44]
gm4  = toGraph [tm01,tm04]
gm44 = toGraph [tm01,tm04,tm41a,tm44a]

gm5, gm55 :: RDFGraph
gm5  = NSGraph
        { namespaces = nslist
        , formulae   = toFM [Formula b1 gm2]
        , statements = S.fromList [tm01,tm02,tm03]
        }

gm55 = NSGraph
        { namespaces = nslist
        , formulae   = toFM [Formula b1 gm2,Formula b2 gm2f]
        , statements = S.fromList [tm01,tm02,tm03,tm41,tm42,tm43]
        }

gm5s :: RDFGraph
gm5s  = NSGraph
        { namespaces = nslist
        , formulae   = toFM [Formula b1 gm2]
        , statements = S.fromList [tm01,tm02,tm03,
                        arc s1 p1 o1, arc o1 p2 s3, arc s2 p3 o4]
        }

gm6, gm66 :: RDFGraph
gm6 = NSGraph
        { namespaces = nslist
        , formulae   = toFM [Formula ba1 gm2,Formula bn3 gm3]
        , statements = S.fromList [tm07,tm08,tm09,tm10,tm11,tm12,tm13,tm14]
        }

gm66 = NSGraph
        { namespaces = nslist
        , formulae   = toFM
                       [Formula ba1 gm2,Formula bn3 gm3
                       ,Formula ba3 gm2f,Formula bn5 gm3f
                       ]
        , statements = S.fromList [tm07,tm08,tm09,tm10,tm11,tm12,tm13,tm14
                       ,tm67,tm68,tm69,tm70,tm71,tm72,tm73,tm74
                       ]
        }

gm456 :: RDFGraph
gm456 = NSGraph 
  { namespaces = nslist
  -- , formulae   = toFM [Formula b1 gm2, Formula ba1 gm2, Formula bn3 gm3]
  , formulae   = M.empty
  , statements = S.fromList [tm01, tm04,
                  tm07, tm08, tm09, tm10, tm11, tm12, tm13, tm14
                  , arc s1 p1 b4
                  , arc b4 p1 o2
                  , arc b4 p1 o3
                 ]
  }

gm564 :: RDFGraph
gm564 = NSGraph 
  { namespaces = nslist
  -- , formulae   = toFM [Formula b1 gm2, Formula ba1 gm2, Formula bn3 gm3]
  , formulae   = M.empty
  , statements = S.fromList [tm01, tm02, tm03
                  , arc b5 p2 b6
                  , tm07, tm08, tm09, tm10, tm11, tm12, tm13, tm14
                  , arc s1 p1 b4
                 ]
  }

gm645 :: RDFGraph
gm645 = NSGraph 
  { namespaces = nslist
  -- , formulae   = toFM [Formula b1 gm2, Formula ba1 gm2, Formula bn3 gm3]
  , formulae   = M.empty
  , statements = S.fromList [tm07, tm08, tm09, tm10, tm11, tm12, tm13, tm14
                  , arc s1 p1 b4
                  , arc b4 p1 o2
                  , arc b4 p1 o3
                  , arc s1 p1 b5
                  , arc b6 p2 b7
                 ]
  }

tm81, tm82, tm811, tm821, tm812, tm822 :: RDFTriple
tm81  = arc b1 p1 v1
tm82  = arc b2 p2 v2
tm811 = arc b1 p1 v3
tm821 = arc b2 p2 v4
tm812 = arc b1 p1 vb3
tm822 = arc b2 p2 vb4

gm82b1, gm82b2 :: [(RDFLabel,RDFLabel)]
gm82b1 = remapLabelList [v1,v2] [v1,v2,b1,b2]
gm82b2 = [(v1,v3),(v2,v4)]

gm81, gm82, gm82a, gm83, gm83a :: RDFGraph
gm81   = toGraph [tm81,tm82]
gm82   = toGraph [tm811,tm821]
gm82a  = remapLabels [v1,v2] [v1,v2,b1,b2] id gm81
gm83   = toGraph [tm811,tm821]
gm83a  = remapLabels [v1,v2] [v1,v2,b1,b2] makeBlank gm81

gm84, gm85, gm85a, gm86, gm86a :: RDFGraph
gm84  = NSGraph
        { namespaces = nslist
        , formulae   = toFM [Formula b1 gm81,Formula v2 gm81]
        , statements = S.fromList [tm81,tm82]
        }

gm85 = NSGraph
        { namespaces = nslist
        , formulae   = toFM [Formula b1 gm82,Formula v4 gm82]
        , statements = S.fromList [tm811,tm821]
        }
gm85a = remapLabels [v1,v2] [v1,v2,b1,b2] id gm84

gm86 = NSGraph
        { namespaces = nslist
        , formulae   = toFM [Formula b1 gm82,Formula vb4 gm82]
        , statements = S.fromList [tm812,tm822]
        }
gm86a = remapLabels [v1,v2] [v1,v2,b1,b2] makeBlank gm84

testMergeSuite :: Test
testMergeSuite = TestList
  [ testMerge "00" gm0 gm0 gm0
  , testMerge "0s" gms gms gms
  , testMerge "0s2" gms2 gms2 gms2
  , testMerge "01" gm1 gm1 gm11
  , testMerge "02" gm2 gm2 gm22
  , testMerge "03" gm3 gm3 gm33
  , testMerge "04" gm4 gm4 gm44
  , testMerge "05" gm5 gm5 gm55
  , testMerge "06" gm6 gm6 gm66
  , testMerge "0+s" gm0 gms gms
  , testMerge "s+0" gms gm0 gms
  , testMerge "0+5" gm0 gm5 gm5
  , testMerge "5+0" gm5 gm0 gm5
  , testMerge "5+s" gm5 gms gm5s
  , testMerge "s+5" gms gm5 gm5s
  , TestCase (assertGrEquiv "mconcat:456" gm456 (mconcat [gm4,gm5,gm6]))
  , TestCase (assertGrEquiv "mconcat:564" gm564 (mconcat [gm5,gm6,gm4]))
  , TestCase (assertGrEquiv "mconcat:645" gm645 (mconcat [gm6,gm4,gm5]))
  , TestCase (assertGrEq "mappend"  
              (mappend gm4 (mappend gm5 gm6))
              (mappend (mappend gm4 gm5) gm6))
  , testGraphEq "Remap07" True gm82 gm82a
  , testEqv "testRemapList07" gm82b2 gm82b1
  , testGraphEq "Remap08" True gm83 gm83a
  , testGraphEq "Remap09" True gm85 gm85a
  , testGraphEq "Remap10" True gm86 gm86a
  ]

------------------------------------------------------------
--  All tests
------------------------------------------------------------

allTests :: [TF.Test]
allTests = 
  [ conv "LangEq" testLangEqSuite
  , conv "Conversion" testConversionSuite
  , conv "NodeEq" testNodeEqSuite
  , conv "NodeClass" testNodeClassSuite
  , conv "NodeLocal" testNodeLocalSuite
  , conv "NewNode" testNewNodeSuite
  , conv "NodeOrd" testNodeOrdSuite
  , conv "LabelOther" testLabelOtherSuite
  , conv "StmtEq" testStmtEqSuite
  , conv "GraphEq" testGraphEqSuite
  , conv "GraphEqSel" testGraphEqSelSuite
  , conv "GraphFold" testGraphFoldSuite
  , conv "GraphFormula" testGraphFormulaSuite
  , conv "GraphTranslate" testGraphTranslateSuite
  , conv "Merge" testMergeSuite
  ]

main :: IO ()
main = TF.defaultMain allTests

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011, 2012, 2013, 2014, 2017, 2021 Douglas Burke  
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
