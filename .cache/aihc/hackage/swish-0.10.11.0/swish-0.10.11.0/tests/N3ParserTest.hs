{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  N3ParserTest
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012, 2013, 2014 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  CPP, OverloadedStrings
--
--  This Module contains test cases for module "N3Parser".
--
--------------------------------------------------------------------------------

module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B

import qualified Test.Framework as TF

import Swish.GraphClass (Arc, arc) 
import Swish.Namespace (
  Namespace, makeNamespace, getNamespaceURI, getNamespaceTuple
  , ScopedName
  , makeScopedName
  , makeNSScopedName
  , nullScopedName
  , namespaceToBuilder
  )
import Swish.QName (QName, qnameFromURI)

import Swish.RDF.Parser.N3
    ( parseN3
    , parseTextFromText, parseAltFromText
    , parseNameFromText -- , parsePrefixFromText
    , parseAbsURIrefFromText, parseLexURIrefFromText
    , parseURIref2FromText
    )

import Swish.RDF.Graph
    ( RDFGraph, RDFLabel(..), NSGraph(..), NamespaceMap
    , emptyRDFGraph, toRDFGraph
    , resRdfType, resRdfFirst, resRdfRest, resRdfNil
    , resOwlSameAs, resLogImplies
    )

import Swish.RDF.Vocabulary
    ( namespaceRDF
    , toLangTag
    , rdfXMLLiteral
    , xsdBoolean 
    , xsdInteger
    , xsdDecimal 
    , xsdDouble 
    )

import Test.HUnit (Test(TestCase,TestList), assertEqual)

import Network.URI (URI, nullURI, parseURIReference)

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Data.Monoid (Monoid(..))
#endif

import Data.Maybe (fromJust, fromMaybe)
import Data.List (intercalate)

import TestHelpers (conv, testCompareEq)

-- Specialized equality comparisons

testLabelEq :: String -> Bool -> RDFLabel -> RDFLabel -> Test
testLabelEq = testCompareEq "testLabelEq:"

testGraphEq :: String -> Bool -> RDFGraph -> RDFGraph -> Test
testGraphEq = testCompareEq "testGraphEq:"

toURI :: String -> URI
toURI s = fromMaybe (error ("Internal error: invalid uri=" ++ s)) (parseURIReference s)

------------------------------------------------------------
--  Generic item parsing test wrapper
------------------------------------------------------------

type ParseFromText a = L.Text -> Either String a

parseItemTest :: (Eq a, Show a) => ParseFromText a -> a
                 -> String -> L.Text -> a -> String -> Test
parseItemTest ifroms def lab inp val err =
    TestList
      [ TestCase ( assertEqual ("parseItemError:"++lab) fixerr pe )
      , TestCase ( assertEqual ("parseItemValue:"++lab) val pv )
      ]
    where
        (pe,pv) = case ifroms inp of
            Left  e -> (e,def)
            Right v -> (noError,v)
        fixerr = if err /= noError then pe else noError

noError, errorText :: String
noError   = ""
errorText = "*"

------------------------------------------------------------
--  Common test wrappers
------------------------------------------------------------

parseTestBase :: String -> Maybe QName -> String -> B.Builder -> RDFGraph -> String -> Test
parseTestBase lbl1 mbase lbl2 inp gr er =
    TestList
      [ TestCase ( assertEqual ("parseTestError:"++lbl1++lbl2) er pe )
      , TestCase ( assertEqual ("parseTestGraph:"++lbl1++lbl2) gr pg )
      ]
    where
        (pe,pg) = case parseN3 (B.toLazyText inp) mbase of
            Right g -> ("", g)
            Left  s -> (s, mempty)

parseTest :: String -> B.Builder -> RDFGraph -> String -> Test
parseTest = parseTestBase "<nobase>" Nothing

parseTestB :: QName -> String -> B.Builder -> RDFGraph -> String -> Test
parseTestB base = parseTestBase "<base>" (Just base)

------------------------------------------------------------
--  Test simple character parsing
------------------------------------------------------------

parseCharTest :: String -> String
                 -> L.Text -> String -> String -> Test
parseCharTest c = parseItemTest (parseTextFromText c) ""

parseAltTest :: String -> String
                -> String -> L.Text -> String -> String -> Test
parseAltTest cc1 cc2 = parseItemTest (parseAltFromText cc1 cc2) ""

charTestSuite :: Test
charTestSuite = TestList
  [ parseCharTest ":" "parseCharTest01" ":" ":" noError
  , parseCharTest "<>" "parseCharTest02" "<>" "<>" noError
  , parseAltTest ":" "<>" "parseCharTest03" ":" ":" noError
  , parseAltTest ":" "<>" "parseCharTest04" "<>" "<>" noError
  , parseAltTest ":" "<>" "parseCharTest04" "<=" "" errorText
  ]

------------------------------------------------------------
--  Test simple name parsing
------------------------------------------------------------

parseNameTest :: String -> L.Text -> String -> String -> Test
parseNameTest = parseItemTest parseNameFromText ""

nameTestSuite :: Test
nameTestSuite = TestList
  [ parseNameTest "parseNameTest01" "name" "name" ""
  , parseNameTest "parseNameTest02" "rdf" "rdf" ""
  ]

{-

Not convinced it's worth testing this piece separately, so removing for now.

------------------------------------------------------------
--  Test simple prefix parsing
------------------------------------------------------------

parsePrefixTest :: String -> L.Text -> Namespace -> String -> Test
parsePrefixTest = parseItemTest parsePrefixFromText (Namespace Nothing nullURI)

prefixTestSuite :: Test
prefixTestSuite = TestList
  [ parsePrefixTest "parsePrefixTest01" "pref" (Namespace (Just "pref") (toURI "pref:")) ""
  , parsePrefixTest "parsePrefixTest02" "rdf" namespaceRDF ""
  ]

-}

------------------------------------------------------------
--  Test absolute URIref parsing
------------------------------------------------------------

parseAbsUriRefTest :: String -> L.Text -> URI -> String -> Test
parseAbsUriRefTest = parseItemTest parseAbsURIrefFromText nullURI

parseLexUriRefTest :: String -> L.Text -> URI -> String -> Test
parseLexUriRefTest = parseItemTest parseLexURIrefFromText nullURI

absUriRefInp01, absUriRefInp01s, absUriRefInp02, absUriRefInp02s :: L.Text

absUriRefInp01  = "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
absUriRefInp01s = "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> "
absUriRefInp02  = "<http://id.ninebynine.org/wip/2003/test/graph1/node#s1>"
absUriRefInp02s = "<http://id.ninebynine.org/wip/2003/test/graph1/node#s1> "

absUriRef01, absUriRef02 :: URI

absUriRef01     = toURI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
absUriRef02     = toURI "http://id.ninebynine.org/wip/2003/test/graph1/node#s1"

absUriRefTestSuite :: Test
absUriRefTestSuite = TestList
  [ parseAbsUriRefTest "parseAbsUriRefTest01" absUriRefInp01 absUriRef01 ""
  , parseAbsUriRefTest "parseAbsUriRefTest02" absUriRefInp02 absUriRef02 ""
  , parseLexUriRefTest "parseAbsUriRefTest03" absUriRefInp01s absUriRef01 ""
  , parseLexUriRefTest "parseAbsUriRefTest04" absUriRefInp02s absUriRef02 ""
  ]

------------------------------------------------------------
--  Test simple URIref parsing
------------------------------------------------------------

parseUriRef2Test :: String -> L.Text -> ScopedName -> String -> Test
parseUriRef2Test = parseItemTest parseURIref2FromText nullScopedName

sname01 :: ScopedName
sname01  = makeNSScopedName namespaceRDF "type"

uriRef02 :: L.Text
uriRef02 = "<http://id.ninebynine.org/wip/2003/test/graph1/node#s1> "

sname02 :: ScopedName
sname02  =
    makeScopedName Nothing (toURI "http://id.ninebynine.org/wip/2003/test/graph1/node#") "s1"

uriRef2TestSuite :: Test
uriRef2TestSuite = TestList
  [ parseUriRef2Test "parseUriRef2Test01" "rdf:type" sname01 ""
  , parseUriRef2Test "parseUriRef2Test02" uriRef02 sname02 ""
  ]

------------------------------------------------------------
--  Define some common values
------------------------------------------------------------

baseFile :: String
baseFile = "file:///dev/null"

dqn :: QName
dqn = (fromJust . qnameFromURI . toURI) baseFile

toNS :: T.Text -> String -> Namespace
toNS p = makeNamespace (Just p) . toURI

dbase, base1, base2, base3, base4, basea :: Namespace
dbase = makeNamespace Nothing $ toURI "#"
base1 = toNS "base1" "http://id.ninebynine.org/wip/2003/test/graph1/node/"
base2 = toNS "base2" "http://id.ninebynine.org/wip/2003/test/graph2/node#"
base3 = toNS "base3" "http://id.ninebynine.org/wip/2003/test/graph3/node"
base4 = toNS "base4" "http://id.ninebynine.org/wip/2003/test/graph3/nodebase"
basea = toNS "a" "http://example.org/basea#"

xsdNS :: Namespace
xsdNS = toNS "xsd" "http://www.w3.org/2001/XMLSchema#"

u1 :: RDFLabel
u1 = Res $ makeNSScopedName base1 ""

ds1, ds2, ds3 :: RDFLabel
ds1 = Res $ makeNSScopedName dbase "s1"
ds2 = Res $ makeNSScopedName dbase "s2"
ds3 = Res $ makeNSScopedName dbase "s3"

dp1, dp2, dp3 :: RDFLabel
dp1 = Res $ makeNSScopedName dbase "p1"
dp2 = Res $ makeNSScopedName dbase "p2"
dp3 = Res $ makeNSScopedName dbase "p3"

do1, do2, do3 :: RDFLabel
do1 = Res $ makeNSScopedName dbase "o1"
do2 = Res $ makeNSScopedName dbase "o2"
do3 = Res $ makeNSScopedName dbase "o3"

s1, s2, s3, sa :: RDFLabel
s1 = Res $ makeNSScopedName base1 "s1"
s2 = Res $ makeNSScopedName base2 "s2"
s3 = Res $ makeNSScopedName base3 "s3"
sa = Res $ makeNSScopedName basea "a"

b1, b2, b3, b4, b5, b6, b7, b8 :: RDFLabel
b1 = Blank "b1"
b2 = Blank "b2"
b3 = Blank "b3"
b4 = Blank "b4"
b5 = Blank "b5"
b6 = Blank "b6"
b7 = Blank "b7"
b8 = Blank "b8"

c1, c2, c3, c4, c5, c6 :: RDFLabel
c1 = Blank "c1"
c2 = Blank "c2"
c3 = Blank "c3"
c4 = Blank "c4"
c5 = Blank "c5"
c6 = Blank "c6"

p1, p2, p3, pa :: RDFLabel
p1 = Res $ makeNSScopedName base1 "p1" 
p2 = Res $ makeNSScopedName base2 "p2" 
p3 = Res $ makeNSScopedName base3 "p3" 
pa = Res $ makeNSScopedName basea "b" 

o1, o2, o3, oa :: RDFLabel
o1 = Res $ makeNSScopedName base1 "o1"
o2 = Res $ makeNSScopedName base2 "o2"
o3 = Res $ makeNSScopedName base3 "o3"
oa = Res $ makeNSScopedName basea "c"

l1, l2, l3 :: RDFLabel
l1 = Lit "l1"
l2 = Lit "l2-'\"line1\"'\n\nl2-'\"\"line2\"\"'"
l3 = Lit "l3--\r\"'\\--\x0020\&--\x00A0\&--"

lfr, lxml, lfrxml :: RDFLabel
lfr    = LangLit "chat"           $ fromJust $ toLangTag "fr"
lxml   = TypedLit "<br/>"         rdfXMLLiteral
lfrxml = TypedLit "<em>chat</em>" rdfXMLLiteral

bTrue, bFalse :: RDFLabel
bTrue  = TypedLit "true"  xsdBoolean
bFalse = TypedLit "false" xsdBoolean

f1, f2 :: RDFLabel
f1 = Res $ makeNSScopedName base1 "f1"
f2 = Res $ makeNSScopedName base2 "f2" 

v1, v2, v3, v4 :: RDFLabel
v1 = Var "var1"
v2 = Var "var2"
v3 = Var "var3"
v4 = Var "var4"

------------------------------------------------------------
--  Construct graphs for testing
------------------------------------------------------------

t01 , t01b, t02, t03, t04, t05, t06, t07 :: Arc RDFLabel
t01  = arc s1 p1 o1
t01b = arc b1 b2 b3
t02  = arc s2 p1 o2
t03  = arc s3 p1 o3
t04  = arc s1 p1 l1
t05  = arc s2 p1 b1
t06  = arc s3 p1 l2
t07  = arc s3 p2 l3

makeNewPrefixNamespace :: (T.Text,Namespace) -> Namespace
makeNewPrefixNamespace (pre,ns) = makeNamespace (Just pre) (getNamespaceURI ns)

dg1, dg2, dg3 :: RDFGraph
dg1 = toRDFGraph $ S.singleton $ arc ds1 dp1 do1
dg2 = toRDFGraph $ S.fromList
      [ arc xa1 xb1 xc1
      , arc xa2 xb2 xc2
      , arc xa3 xb3 xc3
      , arc xa4 xb4 xc4
      , arc xa5 xb5 xc5
      ]
  where
    -- the document base is set to file:///dev/null to begin with
    xa1 = Res "file:///dev/a1"
    xb1 = Res "file:///dev/b1"
    xc1 = Res "file:///dev/c1"
    xa2 = Res "http://example.org/ns/a2"
    xb2 = Res "http://example.org/ns/b2"
    xc2 = Res "http://example.org/ns/c2"
    xa3 = Res "http://example.org/ns/foo/a3"
    xb3 = Res "http://example.org/ns/foo/b3"
    xc3 = Res "http://example.org/ns/foo/c3"
    
    ns4 = makeNamespace Nothing $ toURI "http://example.org/ns/foo/bar#"
    ns5 = makeNamespace Nothing $ toURI "http://example.org/ns2#"
    mUN a b = Res (makeNSScopedName a b)
    xa4 = mUN ns4 "a4"
    xb4 = mUN ns4 "b4"
    xc4 = mUN ns4 "c4"
    xa5 = mUN ns5 "a5"
    xb5 = mUN ns5 "b5"
    xc5 = mUN ns5 "c5"

dg3 = -- TODO: add in prefixes ?
  toRDFGraph $ S.singleton $ arc (Res "file:///home/swish/photos/myphoto") (Res "http://example.com/ns#photoOf") (Res "http://example.com/ns#me")
  
nslist, xnslist :: NamespaceMap
nslist = M.fromList $ map getNamespaceTuple [base1, base2, base3, base4]
xnslist = (\(a,b) -> M.insert a b nslist) $ getNamespaceTuple xsdNS

toGraph :: [Arc RDFLabel] -> RDFGraph
toGraph stmts = mempty { namespaces = nslist
                        , statements = S.fromList stmts
                        }

g1 :: RDFGraph
g1 = toGraph [t01]

g1a :: RDFGraph
g1a = toGraph [arc sa pa oa] 

g1_31 :: RDFGraph
g1_31 = toGraph [arc u1 u1 u1]

g1b :: RDFGraph
g1b = toGraph [t01b]

g1b_1 :: RDFGraph
g1b_1 = toGraph [arc b1 p1 o1]

g2 :: RDFGraph
g2 = toGraph [t01,t02,t03]

g3 :: RDFGraph
g3 = toGraph [t01,t04]

g4 :: RDFGraph
g4 = toGraph [t01,t05]

g4_1 :: RDFGraph
g4_1 = toGraph [arc b1 p1 o1, arc b2 p2 o2]

g4_2 :: RDFGraph
g4_2 = toGraph [arc b1 resRdfType o1, arc b2 resRdfType o2]

g5 :: RDFGraph
g5 = toGraph [t01,t02,t03,t04,t05]

g6 :: RDFGraph
g6 = toGraph [t01,t06]

g7 :: RDFGraph
g7 = toGraph [t01,t07]

t801, t802, t807, t808, t811, t812 :: Arc RDFLabel
t801 = arc s1 resRdfType       o1
t802 = arc s2 resOwlSameAs     o2
t807 = arc o1 p1 s1
t808 = arc s2 p1 o2
t811 = arc s1 resLogImplies o1
t812 = arc o2 resLogImplies s2

g8 :: RDFGraph
g8 = toGraph [t801,t802,t807,t808,t811,t812]

g81 :: RDFGraph
g81 = toGraph [t801,t802]

g83 :: RDFGraph
g83 = toGraph [t807,t808,t811,t812]

t911, t912, t913, t914, t921, t922, t923, t924,
  t925, t926, t927, t928 :: Arc RDFLabel
t911 = arc s1 p1 o1
t912 = arc s1 p1 o2
t913 = arc s1 p2 o2
t914 = arc s1 p2 o3
t921 = arc s2 p1 o1
t922 = arc s2 p1 o2
t923 = arc s2 p1 o3
t924 = arc s2 p1 l1
t925 = arc s2 p2 o1
t926 = arc s2 p2 o2
t927 = arc s2 p2 o3
t928 = arc s2 p2 l1

g9 :: RDFGraph
g9 = toGraph [t911,t912,t913,t914,
              t921,t922,t923,t924,
              t925,t926,t927,t928]

t1011, t1012, t1013, t1014, t1021, t1022, t1023, t1024,
  t1025, t1026, t1027, t1028 :: Arc RDFLabel
t1011 = arc s1 p1 o1
t1012 = arc o2 p1 s1
t1013 = arc s1 p2 o2
t1014 = arc o3 p2 s1
t1021 = arc s2 p1 o1
t1022 = arc s2 p1 o2
t1023 = arc s2 p1 o3
t1024 = arc s2 p1 l1
t1025 = arc o1 p2 s2
t1026 = arc o2 p2 s2
t1027 = arc o3 p2 s2
t1028 = arc l1 p2 s2

g10 :: RDFGraph
g10 = toGraph [t1011,t1012,t1013,t1014,
               t1021,t1022,t1023,t1024,
               t1025,t1026,t1027,t1028]

t1111, t1112, t1113 :: Arc RDFLabel
t1111 = arc s1 p1 v1
t1112 = arc v2 p1 o1
t1113 = arc v3 p1 v4

g11 :: RDFGraph
g11 = toGraph [t1111,t1112,t1113]

t1211, t1221, t1222, t1223, t1224 :: Arc RDFLabel
t1211 = arc b1 p1 o1
t1221 = arc b2 resRdfFirst v1
t1222 = arc b2 resRdfRest  b3
t1223 = arc b3 resRdfFirst v2
t1224 = arc b3 resRdfRest  resRdfNil

g12 :: RDFGraph
g12 = toGraph [t1211,t1221,t1222,t1223,t1224]

t1711, t1722, t1733 :: Arc RDFLabel
t1711 = arc s1 p1 lfr
t1722 = arc s2 p2 lxml
t1733 = arc s3 p3 lfrxml

g17 :: RDFGraph
g17 = toGraph [t1711,t1722,t1733]

tx101, tx102, tx111, tx112, tx113, tx114, tx121, tx122, tx123,
  tx124, tx125, tx126, tx127, tx128 :: Arc RDFLabel
tx101 = arc b1 resOwlSameAs s1
tx102 = arc s2 resOwlSameAs b2
tx111 = arc b1 p1 o1
tx112 = arc b1 p1 o2
tx113 = arc b1 p2 o2
tx114 = arc b1 p2 o3
tx121 = arc b2 p1 o1
tx122 = arc b2 p1 o2
tx123 = arc b2 p1 o3
tx124 = arc b2 p1 l1
tx125 = arc b2 p2 o1
tx126 = arc b2 p2 o2
tx127 = arc b2 p2 o3
tx128 = arc b2 p2 l1

x1 :: RDFGraph
x1 = toGraph [tx101,tx102,
              tx111,tx112,tx113,tx114,
              tx121,tx122,tx123,tx124,
              tx125,tx126,tx127,tx128]

tx201, tx202, tx211, tx212, tx213, tx214, tx221, tx222, tx223,
  tx224, tx225, tx226, tx227, tx228 :: Arc RDFLabel
tx201 = arc b1 resOwlSameAs s1
tx202 = arc s2 resOwlSameAs b2
tx211 = arc b1 p1 o1
tx212 = arc o2 p1 b1
tx213 = arc b1 p2 o2
tx214 = arc o3 p2 b1
tx221 = arc b2 p1 o1
tx222 = arc b2 p1 o2
tx223 = arc b2 p1 o3
tx224 = arc b2 p1 l1
tx225 = arc o1 p2 b2
tx226 = arc o2 p2 b2
tx227 = arc o3 p2 b2
tx228 = arc l1 p2 b2

x2 :: RDFGraph
x2 = toGraph [tx201,tx202,
              tx211,tx212,tx213,tx214,
              tx221,tx222,tx223,tx224,
              tx225,tx226,tx227,tx228]

tx311, tx312, tx313, tx314, tx321, tx322, tx323,
  tx324, tx325, tx326, tx327, tx328 :: Arc RDFLabel
tx311 = arc s1 p1 o1
tx312 = arc o2 p1 s1
tx313 = arc s1 p2 o2
tx314 = arc o3 p2 s1
tx321 = arc s2 p1 o1
tx322 = arc s2 p1 o2
tx323 = arc s2 p1 o3
tx324 = arc s2 p1 l1
tx325 = arc o1 p2 s2
tx326 = arc o2 p2 s2
tx327 = arc o3 p2 s2
tx328 = arc l1 p2 s2

x3 :: RDFGraph
x3 = toGraph [tx311,tx312,tx313,tx314,
              tx321,tx322,tx323,tx324,
              tx325,tx326,tx327,tx328]

tx401, tx402, tx403, tx404, tx405, tx406, tx407,
  tx408, tx409 :: Arc RDFLabel
tx401 = arc s1 resOwlSameAs b1
tx402 = arc b1 resRdfFirst  o1
tx403 = arc b1 resRdfRest   b2
tx404 = arc b2 resRdfFirst  o2
tx405 = arc b2 resRdfRest   b3
tx406 = arc b3 resRdfFirst  o3
tx407 = arc b3 resRdfRest   b4
tx408 = arc b4 resRdfFirst  l1
tx409 = arc b4 resRdfRest   resRdfNil

x4 :: RDFGraph
x4 = toGraph [tx401,tx402,tx403,tx404,
              tx405,tx406,tx407,tx408,
              tx409]

tx501, tx502, tx503, tx504, tx505, tx506, tx507,
  tx508, tx509 :: Arc RDFLabel
tx501 = arc b1 resOwlSameAs s1
tx502 = arc b1 resRdfFirst  o1
tx503 = arc b1 resRdfRest   b2
tx504 = arc b2 resRdfFirst  o2
tx505 = arc b2 resRdfRest   b3
tx506 = arc b3 resRdfFirst  o3
tx507 = arc b3 resRdfRest   b4
tx508 = arc b4 resRdfFirst  l1
tx509 = arc b4 resRdfRest   resRdfNil

x5 :: RDFGraph
x5 = toGraph [tx501,tx502,tx503,tx504,
              tx505,tx506,tx507,tx508,
              tx509]

tx601, tx602, tx603, tx604, tx605, tx606, tx607,
  tx608 :: Arc RDFLabel
tx601 = arc s1 resRdfFirst o1
tx602 = arc s1 resRdfRest  b2
tx603 = arc b2 resRdfFirst o2
tx604 = arc b2 resRdfRest  b3
tx605 = arc b3 resRdfFirst o3
tx606 = arc b3 resRdfRest  b4
tx607 = arc b4 resRdfFirst l1
tx608 = arc b4 resRdfRest  resRdfNil

x6 :: RDFGraph
x6 = toGraph [tx601,tx602,tx603,tx604,
              tx605,tx606,tx607,tx608]

tx701 :: Arc RDFLabel
tx701 = arc b1 p2 f2

x7 :: RDFGraph
x7    = NSGraph
        { namespaces = nslist
        , formulae   = M.singleton b1 g2 -- $ Formula b1 g2
        , statements = S.singleton tx701
        }

tx801 :: Arc RDFLabel
tx801 = arc f1 p2 f2

x8 :: RDFGraph
x8    = NSGraph
        { namespaces = nslist
        , formulae   = M.singleton f1 g2 -- $ Formula f1 g2
        , statements = S.singleton tx801
        }

x9 :: RDFGraph
x9    = NSGraph
        { namespaces = nslist
        , formulae   = M.singleton f1 g1 -- $ Formula f1 g1
        , statements = S.singleton tx801
        }

--  Test allocation of bnodes carries over a nested formula

tx1201, tx1202, tx1203, tx1204, tx1205, 
  tx1211, tx1212 :: Arc RDFLabel
tx1201 = arc s1 p1 b1
tx1202 = arc b1 p1 o1
tx1203 = arc b2 p2 f2
tx1204 = arc s3 p3 b3
tx1205 = arc b3 p3 o3
tx1211 = arc s2 p2 b4
tx1212 = arc b4 p2 o2

x12fg :: RDFGraph
x12fg  = mempty { statements = S.fromList [tx1211,tx1212] }
{-
x12fg  = NSGraph
        { namespaces = emptyNamespaceMap
        , formulae   = emptyFormulaMap
        , statements = S.fromList [tx1211,tx1212]
        }
-}
        
x12 :: RDFGraph
x12    = NSGraph
        { namespaces = nslist
        , formulae   = M.singleton b2 x12fg -- $ Formula b2 x12fg
        , statements = S.fromList [tx1201,tx1202,tx1203,tx1204,tx1205]
        }

--  List of simple anon nodes

tx1301, tx1302, tx1303, tx1304, tx1305, tx1306, tx1307,
  tx1308, tx1309 :: Arc RDFLabel
tx1301 = arc s1 resRdfFirst b1
tx1302 = arc s1 resRdfRest  c1
tx1303 = arc c1 resRdfFirst b2
tx1304 = arc c1 resRdfRest  c2
tx1305 = arc c2 resRdfFirst b3
tx1306 = arc c2 resRdfRest  resRdfNil
tx1307 = arc b1 p1 o1
tx1308 = arc b2 p1 o2
tx1309 = arc b3 p1 o3

x13 :: RDFGraph
x13 = toGraph [tx1301,tx1302,tx1303,tx1304,tx1305,tx1306,
               tx1307,tx1308,tx1309]

--  List of more complex anon nodes

tx1401, tx1402, tx1403, tx1404, tx1405, tx1406, tx1407,
  tx1408, tx1409, tx1410, tx1411, tx1412 :: Arc RDFLabel
tx1401 = arc s1 resRdfFirst b1
tx1402 = arc s1 resRdfRest  c1
tx1403 = arc c1 resRdfFirst b2
tx1404 = arc c1 resRdfRest  c2
tx1405 = arc c2 resRdfFirst b3
tx1406 = arc c2 resRdfRest  resRdfNil
tx1407 = arc b1 p1 o1
tx1408 = arc b1 p2 o1
tx1409 = arc b2 p1 o2
tx1410 = arc b2 p2 o2
tx1411 = arc b3 p1 o3
tx1412 = arc b3 p2 o3

x14 :: RDFGraph
x14 = toGraph [tx1401,tx1402,tx1403,tx1404,tx1405,tx1406,
               tx1407,tx1408,tx1409,tx1410,tx1411,tx1412]

--  List with nested list

tx1501, tx1502, tx1503, tx1504, tx1505, tx1506, tx1507,
  tx1508, tx1509 :: Arc RDFLabel
tx1501 = arc s1 resRdfFirst b1
tx1502 = arc s1 resRdfRest  c1
tx1503 = arc c1 resRdfFirst b2
tx1504 = arc c1 resRdfRest  c2
tx1505 = arc c2 resRdfFirst b3
tx1506 = arc c2 resRdfRest  resRdfNil
tx1507 = arc b1 p1 o1
tx1508 = arc b2 p2 c3
tx1509 = arc b3 p1 o3

tx1521, tx1522, tx1523, tx1524, tx1525, tx1526, tx1527,
  tx1528, tx1529 :: Arc RDFLabel
tx1521 = arc c3 resRdfFirst b4
tx1522 = arc c3 resRdfRest  c4
tx1523 = arc c4 resRdfFirst b5
tx1524 = arc c4 resRdfRest  c5
tx1525 = arc c5 resRdfFirst b6
tx1526 = arc c5 resRdfRest  resRdfNil
tx1527 = arc b4 p1 o1
tx1528 = arc b5 p1 o2
tx1529 = arc b6 p1 o3

x15 :: RDFGraph
x15 = toGraph [tx1501,tx1502,tx1503,tx1504,tx1505,tx1506,
               tx1507,tx1508,tx1509,
               tx1521,tx1522,tx1523,tx1524,tx1525,tx1526,
               tx1527,tx1528,tx1529]

--  More complex list with nested list

tx1601, tx1602, tx1603, tx1604, tx1605, tx1606, tx1607,
  tx1608, tx1609, tx1610, tx1611 :: Arc RDFLabel
tx1601 = arc s1 resRdfFirst b1
tx1602 = arc s1 resRdfRest  c1
tx1603 = arc c1 resRdfFirst b2
tx1604 = arc c1 resRdfRest  c2
tx1605 = arc c2 resRdfFirst b3
tx1606 = arc c2 resRdfRest  resRdfNil
tx1607 = arc b1 p1 o1
tx1608 = arc b1 p2 o1
tx1609 = arc b2 p2 c3
tx1610 = arc b3 p1 o3
tx1611 = arc b3 p2 o3

tx1621, tx1622, tx1623, tx1624, tx1625, tx1626, tx1627,
  tx1628, tx1629, tx1630, tx1631, tx1632 :: Arc RDFLabel
tx1621 = arc c3 resRdfFirst b4
tx1622 = arc c3 resRdfRest  c4
tx1623 = arc c4 resRdfFirst b5
tx1624 = arc c4 resRdfRest  c5
tx1625 = arc c5 resRdfFirst b6
tx1626 = arc c5 resRdfRest  resRdfNil
tx1627 = arc b4 p1 o1
tx1628 = arc b4 p2 o1
tx1629 = arc b5 p1 o2
tx1630 = arc b5 p2 o2
tx1631 = arc b6 p1 o3
tx1632 = arc b6 p2 o3

x16 :: RDFGraph
x16 = toGraph [tx1601,tx1602,tx1603,tx1604,tx1605,tx1606,
               tx1607,tx1608,tx1609,tx1610,tx1611,
               tx1621,tx1622,tx1623,tx1624,tx1625,tx1626,
               tx1627,tx1628,tx1629,tx1630,tx1631,tx1632]

kg1 :: RDFGraph
kg1 = toRDFGraph $ S.singleton $ arc b a c
  where
    -- the document base is set to file:///dev/null to begin with
    mUN = Res . makeNSScopedName dbase
    a = mUN "a"
    b = mUN "b"
    c = mUN "c"

------------------------------------------------------------
--  Simple parser tests
------------------------------------------------------------

-- check default base
simpleN3Graph_dg_01 :: B.Builder
simpleN3Graph_dg_01 = ":s1 :p1 :o1 ."

-- from the turtle documentation
simpleN3Graph_dg_02 :: B.Builder
simpleN3Graph_dg_02 =
  mconcat
  [ "# this is a complete turtle document\n"
  , "# In-scope base URI is the document URI at this point\n"
  , "<a1> <b1> <c1> .\n"
  , "@base <http://example.org/ns/> .\n"
  , "# In-scope base URI is http://example.org/ns/ at this point\n"
  , "<a2> <http://example.org/ns/b2> <c2> .\n"
  , "@base <foo/> .\n"
  , "# In-scope base URI is http://example.org/ns/foo/ at this point\n"
  , "<a3> <b3> <c3> .\n"
  , "@prefix : <bar#> .\n"
  , ":a4 :b4 :c4 .\n"
  , "@prefix : <http://example.org/ns2#> .\n"
  , ":a5 :b5 :c5 .\n"
  ]
  
-- try out file prefixes
simpleN3Graph_dg_03 :: B.Builder  
simpleN3Graph_dg_03 =  
  mconcat
  [ "@prefix : <file:///home/swish/photos/>.\n"
  , "@prefix me: <http://example.com/ns#>.\n"
  , ":myphoto me:photoOf me:me."
  ]
  
commonPrefixes :: B.Builder
commonPrefixes =
  mconcat $ map namespaceToBuilder [base1, base2, base3]

rdfPrefix :: B.Builder
rdfPrefix = namespaceToBuilder namespaceRDF

--  Single statement using <uri> form
simpleN3Graph_g1_01 :: B.Builder
simpleN3Graph_g1_01 =
  "<http://id.ninebynine.org/wip/2003/test/graph1/node/s1>  <http://id.ninebynine.org/wip/2003/test/graph1/node/p1>  <http://id.ninebynine.org/wip/2003/test/graph1/node/o1> . "

--  Single statement using prefix:name form
simpleN3Graph_g1_02 :: B.Builder
simpleN3Graph_g1_02 =
  namespaceToBuilder base1 `mappend`
  " base1:s1 base1:p1 base1:o1 . "

--  Single statement using prefix:name form
--  (this was added to check that the parser did not
--   think we meant 'a:a a :b .' here)
--
simpleN3Graph_g1_02a :: B.Builder
simpleN3Graph_g1_02a =
  namespaceToBuilder basea `mappend`
  "a:a a:b a:c ."

nToB :: Namespace -> B.Builder
nToB = B.fromString . show . getNamespaceURI

--  Single statement using :name form
simpleN3Graph_g1_03 :: B.Builder
simpleN3Graph_g1_03 =
  mconcat
  [ "@prefix : <", nToB base1,  "> .\n"
  , " :s1 :p1 :o1 . "
  ]
  
--  Check we can handle ':' and 'prefix:' forms.
--
simpleN3Graph_g1_03_1 :: B.Builder
simpleN3Graph_g1_03_1 =
  mconcat
  [ "@prefix : <", nToB base1,  "> .\n"
  , " : : :."
  ]

simpleN3Graph_g1_03_2 :: B.Builder
simpleN3Graph_g1_03_2 =
  mconcat
  [ "@prefix b: <", nToB base1, "> .\n"
  , "b: b: b:. "
  ]

--  Single statement using relative URI form
simpleN3Graph_g1_04 :: B.Builder
simpleN3Graph_g1_04 =
  mconcat
  [ "@base <", nToB base1, "> .\n"
  , " <s1> <p1> <o1> . "
  ]

--  Single statement using blank nodes
simpleN3Graph_g1_05 :: B.Builder
simpleN3Graph_g1_05 =
  mconcat
  [ "@base <", nToB base1, "> .\n"
  , " _:b1 _:b2 _:b3 . "
  ]

simpleN3Graph_g1_05_1 :: B.Builder
simpleN3Graph_g1_05_1 =
  commonPrefixes `mappend`
  " _:b1 base1:p1 base1:o1 . "

--  Single statement with junk following
simpleN3Graph_g1_06 :: B.Builder
simpleN3Graph_g1_06 =
  mconcat
  [ namespaceToBuilder base1
  , " base1:s1 base1:p1 base1:o1 . " 
  , " **** "
  ]

--  Multiple statements
simpleN3Graph_g2 :: B.Builder
simpleN3Graph_g2 =
  mconcat
  [ commonPrefixes
  , " base1:s1 base1:p1 base1:o1 . \n"
  , " base2:s2 base1:p1 base2:o2 . \n"
  , " base3:s3 base1:p1 base3:o3 . \n"
  ]

--  Graph with literal
simpleN3Graph_g3 :: B.Builder
simpleN3Graph_g3 =
  mconcat
  [ commonPrefixes
  , " base1:s1 base1:p1 base1:o1 . \n"
  , " base1:s1 base1:p1 \"l1\" . \n"
  ]
  
--  Graph with nodeid
simpleN3Graph_g4 :: B.Builder
simpleN3Graph_g4 =
  mconcat
  [ commonPrefixes
  , " base1:s1 base1:p1 base1:o1 . \n"
  , " base2:s2 base1:p1 _:b1 . \n"
  ]
  
simpleN3Graph_g4_1 :: B.Builder
simpleN3Graph_g4_1 =
  commonPrefixes `mappend`
  " _:b1 base1:p1 base1:o1._:b2 base2:p2 base2:o2."

simpleN3Graph_g4_2 :: B.Builder
simpleN3Graph_g4_2 =
  commonPrefixes `mappend`
  " _:foo1 a base1:o1. _:bar2 a base2:o2."

-- same graph as g4_2
simpleN3Graph_g4_3 :: B.Builder
simpleN3Graph_g4_3 =
  commonPrefixes `mappend`
  " [] a base1:o1.[a base2:o2]."

--  Graph with literal and nodeid
simpleN3Graph_g5 :: B.Builder
simpleN3Graph_g5 =
  mconcat
  [ commonPrefixes
  , " base1:s1 base1:p1 base1:o1 . \n"
  , " base2:s2 base1:p1 base2:o2 . \n"
  , " base3:s3 base1:p1 base3:o3 . \n"
  , " base1:s1 base1:p1 \"l1\" . \n"  
  , " base2:s2 base1:p1 _:b1 . \n"
  ]

--  Triple-quoted literal
simpleN3Graph_g6 :: B.Builder 
simpleN3Graph_g6 =
  mconcat
  [ commonPrefixes
  , " base1:s1 base1:p1 base1:o1 . \n"
  , " base3:s3 base1:p1 \"\"\"l2-'\"line1\"'\n\nl2-'\"\"line2\"\"'\"\"\" . \n"
  ]

--  String escapes
simpleN3Graph_g7 :: B.Builder
simpleN3Graph_g7 =
  mconcat
  [ commonPrefixes
  , " base1:s1 base1:p1 base1:o1 . \n"
  , " base3:s3 base2:p2 "
  , " \"l3--\\r\\\"\\'\\\\--\\u0020--\\U000000A0--\" "
  , " . \n"
  ]

--  Different verb forms
simpleN3Graph_g8 :: B.Builder
simpleN3Graph_g8 =
  mconcat
  [ commonPrefixes
  , " base1:s1 a base1:o1 . \n"
  , " base2:s2 = base2:o2 . \n"
  , " base1:s1 @is  base1:p1 @of base1:o1 . \n"
  , " base2:s2 @has base1:p1 base2:o2 . \n"
  , " base1:s1 => base1:o1 . \n"
  , " base2:s2 <= base2:o2 . \n"
  ]
  
simpleN3Graph_g8b :: B.Builder
simpleN3Graph_g8b =
  mconcat
  [ commonPrefixes
  , " base1:s1 a base1:o1 . \n"
  , " base2:s2 = base2:o2 . \n"
  , " base1:s1 is  base1:p1 of base1:o1 . \n"
  , " base2:s2 @has base1:p1 base2:o2 . \n"
  , " base1:s1 => base1:o1 . \n"
  , " base2:s2 <= base2:o2 . \n"
  ]
  
simpleN3Graph_g81 :: B.Builder
simpleN3Graph_g81 =
  mconcat
  [ commonPrefixes
  , " base1:s1 a base1:o1 . \n"
  , " base2:s2 = base2:o2 . \n"
  ]

simpleN3Graph_g83 :: B.Builder
simpleN3Graph_g83 =
  mconcat
  [ commonPrefixes
  , " base1:s1 @is  base1:p1 @of base1:o1 . \n"
  , " base2:s2 @has base1:p1 base2:o2 . \n"
  , " base1:s1 => base1:o1 . \n"
  , " base2:s2 <= base2:o2 . \n"
  ]

simpleN3Graph_g83b :: B.Builder
simpleN3Graph_g83b =
  mconcat
  [ commonPrefixes
  , " base1:s1 is  base1:p1 of base1:o1 . \n"
  , " base2:s2 @has base1:p1 base2:o2 . \n"
  , " base1:s1 => base1:o1 . \n"
  , " base2:s2 <= base2:o2 . \n"
  ]
  
--  Semicolons and commas
simpleN3Graph_g9 :: B.Builder
simpleN3Graph_g9 =
  mconcat
  [ commonPrefixes
  , " base1:s1 base1:p1 base1:o1 ; \n"
  , "          base1:p1 base2:o2 ; \n"
  , "          base2:p2 base2:o2 ; \n"
  , "          base2:p2 base3:o3 . \n"
  , " base2:s2 base1:p1 base1:o1 , \n"
  , "                   base2:o2 , \n"
  , "                   base3:o3 , \n"
  , "                   \"l1\"   ; \n"
  , "          base2:p2 base1:o1 , \n"
  , "                   base2:o2 , \n"
  , "                   base3:o3 , \n"
  , "                   \"l1\"   . \n"
  ]

-- ensure you can end a property list with a semicolon
simpleN3Graph_g9b :: B.Builder
simpleN3Graph_g9b =
  mconcat
  [ commonPrefixes
  , " base1:s1 base1:p1 base1:o1 ; \n"
  , "          base1:p1 base2:o2 ; \n"
  , "          base2:p2 base2:o2 ; \n"
  , "          base2:p2 base3:o3;. \n"
  , " base2:s2 base1:p1 base1:o1 , \n"
  , "                   base2:o2 , \n"
  , "                   base3:o3 , \n"
  , "                   \"l1\"   ; \n"
  , "          base2:p2 base1:o1 , \n"
  , "                   base2:o2 , \n"
  , "                   base3:o3 , \n"
  , "                   \"l1\"   ;. \n"
  ]
  
--  'is ... of' and semicolons and commas
simpleN3Graph_g10 :: B.Builder
simpleN3Graph_g10 =
  mconcat
  [ commonPrefixes 
  , " base1:s1 @has base1:p1 base1:o1 ; \n"
  , "          @is  base1:p1 @of base2:o2 ; \n"
  , "          @has base2:p2 base2:o2 ; \n"
  , "          @is  base2:p2 @of base3:o3 . \n"
  , " base2:s2 @has base1:p1 base1:o1 , \n"
  , "                        base2:o2 , \n"
  , "                        base3:o3 , \n"
  , "                        \"l1\"   ; \n"
  , "          @is  base2:p2 @of base1:o1 , \n"
  , "                          base2:o2 , \n"
  , "                          base3:o3 , \n"
  , "                          \"l1\"   . \n"
  ]
  
--  Simple statements using ?var form
simpleN3Graph_g11 :: B.Builder
simpleN3Graph_g11 =
  mconcat
  [ namespaceToBuilder base1
  , " base1:s1 base1:p1 ?var1 . \n"         
  , " ?var2 base1:p1 base1:o1 . \n"         
  , " ?var3 base1:p1 ?var4 .    \n"
  ]
  
--  Bare anonymous nodes
simpleN3Graph_g12 :: B.Builder
simpleN3Graph_g12 =
  mconcat
  [ namespaceToBuilder base1
  , " [ base1:p1 base1:o1 ] .  \n"         
  , " ( ?var1 ?var2 ) .    \n"
  ]
  
--  Literals with dataype and language
simpleN3Graph_g17 :: B.Builder
simpleN3Graph_g17 =
  mconcat
  [ commonPrefixes
  , rdfPrefix
  , " base1:s1 base1:p1 \"chat\"@fr . \n "                         
  , " base2:s2 base2:p2 \"<br/>\"^^rdf:XMLLiteral . \n "           
  , " base3:s3 base3:p3 \"<em>chat</em>\"^^rdf:XMLLiteral . \n "
  ]
  
emsg16 :: String
{- parsec error
emsg16 = intercalate "\n" [
  "",
  "@prefix base1 : <http://id.ninebynine.org/wip/2003/test/graph1/node/> . base1:s1 base1:p1 base1:o1 .  **** ",
  "                                                                                                      ^",
  "(line 1, column 103 indicated by the '^' sign above):",
  "",
  "unexpected \"*\"",
  "expecting declaration, \"@\", pathitem or end of input"
  ]
-}
emsg16 = "Expected end of input (EOF)\nRemaining input:\n**** "

simpleTestSuite :: Test
simpleTestSuite = TestList
  [ parseTestB dqn "simpleTestd01" simpleN3Graph_dg_01 dg1  noError
  , parseTestB dqn "simpleTestd02" simpleN3Graph_dg_02 dg2  noError
  , parseTestB dqn "simpleTestd03" simpleN3Graph_dg_03 dg3  noError
  , parseTest "simpleTest011" simpleN3Graph_g1_01 g1  noError
  , parseTest "simpleTest012" simpleN3Graph_g1_02 g1  noError
  , parseTest "simpleTest012a" simpleN3Graph_g1_02a g1a  noError
  , parseTest "simpleTest013" simpleN3Graph_g1_03 g1  noError
  , parseTest "simpleTest013_1" simpleN3Graph_g1_03_1 g1_31  noError
  , parseTest "simpleTest013_2" simpleN3Graph_g1_03_2 g1_31  noError
  , parseTest "simpleTest014" simpleN3Graph_g1_04 g1  noError
  , parseTest "simpleTest015" simpleN3Graph_g1_05 g1b noError
  , parseTest "simpleTest015_1" simpleN3Graph_g1_05_1 g1b_1 noError
  , parseTest "simpleTest016" simpleN3Graph_g1_06 emptyRDFGraph emsg16
  , parseTest "simpleTest03"  simpleN3Graph_g2    g2  noError
  , parseTest "simpleTest04"  simpleN3Graph_g3    g3  noError
  , parseTest "simpleTest05"  simpleN3Graph_g4    g4  noError
  , parseTest "simpleTest05_1"  simpleN3Graph_g4_1    g4_1  noError
  , parseTest "simpleTest05_2"  simpleN3Graph_g4_2    g4_2  noError
  , parseTest "simpleTest05_3"  simpleN3Graph_g4_3    g4_2  noError
  , parseTest "simpleTest06"  simpleN3Graph_g5    g5  noError
  , parseTest "simpleTest07"  simpleN3Graph_g6    g6  noError
  , parseTest "simpleTest08"  simpleN3Graph_g7    g7  noError
  , parseTest "simpleTest09"  simpleN3Graph_g8    g8  noError
  , parseTest "simpleTest09b" simpleN3Graph_g8b   g8  noError
  , parseTest "simpleTest10"  simpleN3Graph_g81   g81 noError
  , parseTest "simpleTest12"  simpleN3Graph_g83   g83 noError
  , parseTest "simpleTest12b" simpleN3Graph_g83b  g83 noError
  , parseTest "simpleTest13"  simpleN3Graph_g9    g9  noError
  , parseTest "simpleTest13b" simpleN3Graph_g9b   g9  noError
  , parseTest "simpleTest14"  simpleN3Graph_g10   g10 noError
  , parseTest "simpleTest15"  simpleN3Graph_g11   g11 noError
  , parseTest "simpleTest16"  simpleN3Graph_g12   g12 noError
  , parseTest "simpleTest17"  simpleN3Graph_g17   g17 noError
  ]

------------------------------------------------------------
--  Literal parser tests
------------------------------------------------------------
--
--  Expand upon the literal testing done above
--

litN3Graph_g1 :: B.Builder
litN3Graph_g1 =
  mconcat
  [ commonPrefixes
  , " base1:s1 base1:p1 \"true\"^^<http://www.w3.org/2001/XMLSchema#boolean>.\n"
  , " base2:s2 base2:p2 \"false\"^^<http://www.w3.org/2001/XMLSchema#boolean>.\n"
  , " base3:s3 base3:p3 \"true\"^^<http://www.w3.org/2001/XMLSchema#boolean>.\n"
  ]
    
litN3Graph_g2 :: B.Builder
litN3Graph_g2 =
  mconcat
  [ commonPrefixes
  , namespaceToBuilder xsdNS
  , " base1:s1 base1:p1 \"true\"^^xsd:boolean.\n"
  , " base2:s2 base2:p2 \"false\"^^xsd:boolean.\n"
  , " base3:s3 base3:p3 \"true\"^^xsd:boolean.\n"
  ]
  
litN3Graph_g3 :: B.Builder
litN3Graph_g3 =
  mconcat
  [ commonPrefixes
  , " base1:s1 base1:p1 @true.\n"
  , " base2:s2 base2:p2 @false.\n"
  , " base3:s3 base3:p3 true.\n"
  ]
  
litN3Graph_g4 :: B.Builder
litN3Graph_g4 =
  commonPrefixes `mappend`
  " base1:s1 base1:p1 ( true 1 2.0 -2.21 -2.3e-4 ).\n"

lit_g1 :: RDFGraph
lit_g1 = toGraph [ arc s1 p1 bTrue
                 , arc s2 p2 bFalse
                 , arc s3 p3 bTrue
                 ]

-- at the moment we could use lit_g1 rather than lit_g2, since
-- the namespace map isn't used in the comparison.
--
lit_g2 :: RDFGraph
lit_g2 = lit_g1 { namespaces = xnslist }

bOne, b20, b221, b23e4 :: RDFLabel
bOne  = TypedLit "1"       xsdInteger
b20   = TypedLit "2.0"     xsdDecimal
b221  = TypedLit "-2.21"   xsdDecimal
b23e4 = TypedLit "-2.3E-4" xsdDouble

lit_g4 :: RDFGraph
lit_g4 = mempty {
  namespaces = xnslist
  , statements = S.fromList [
    arc s1 p1 b1
    , arc b1 resRdfFirst bTrue
    , arc b1 resRdfRest  b2
    , arc b2 resRdfFirst bOne
    , arc b2 resRdfRest  b3
    , arc b3 resRdfFirst b20
    , arc b3 resRdfRest  b4
    , arc b4 resRdfFirst b221
    , arc b4 resRdfRest  b5
    , arc b5 resRdfFirst b23e4
    , arc b5 resRdfRest  resRdfNil
    ]
  }

litTestSuite :: Test
litTestSuite = TestList
  [ parseTest "litTest01" litN3Graph_g1 lit_g1  noError
  , parseTest "litTest02" litN3Graph_g2 lit_g2  noError
  , parseTest "litTest03" litN3Graph_g3 lit_g2  noError
  , parseTest "litTest04" litN3Graph_g4 lit_g4  noError
  ]

------------------------------------------------------------
--  Exotic parser tests
------------------------------------------------------------
--
--  These tests cover various forms of anonymous nodes
--  [...], lists and formula. together with uses of ':-'
--

--  Simple anon nodes, with semicolons and commas
exoticN3Graph_x1 :: B.Builder
exoticN3Graph_x1 =
  mconcat
  [ commonPrefixes
  , " [ base1:p1 base1:o1 ; \n"
  , "   base1:p1 base2:o2 ; \n"
  , "   base2:p2 base2:o2 ; \n"
  , "   base2:p2 base3:o3 ] = base1:s1 . \n"
  , " base2:s2 = \n"
  , " [ base1:p1 base1:o1 , \n"
  , "   base2:o2 , \n"
  , "   base3:o3 , \n"
  , "   \"l1\"   ; \n"
  , "   base2:p2 base1:o1 , \n"
  , "            base2:o2 , \n"
  , "            base3:o3 , \n"
  , "            \"l1\"   ] . \n"
  ]
  
-- check semi-colons at end of property list
exoticN3Graph_x1b :: B.Builder
exoticN3Graph_x1b =
  mconcat
  [ commonPrefixes
  , " [ base1:p1 base1:o1 ; \n"
  , "   base1:p1 base2:o2 ; \n"
  , "   base2:p2 base2:o2 ; \n"
  , "   base2:p2 base3:o3; ] = base1:s1 . \n"
  , " base2:s2 = \n"
  , " [ base1:p1 base1:o1 , \n"
  , "   base2:o2 , \n"
  , "   base3:o3 , \n"
  , "   \"l1\"   ; \n"
  , "   base2:p2 base1:o1 , \n"
  , "            base2:o2 , \n"
  , "            base3:o3 , \n"
  , "            \"l1\" ;  ] ;. \n"
  ]
  
--  Simple anon nodes, with 'is ... of' and semicolons and commas
exoticN3Graph_x2 :: B.Builder
exoticN3Graph_x2 =
  mconcat
  [ commonPrefixes
  , " [ @has base1:p1     base1:o1 ; \n"
  , "   @is  base1:p1 @of base2:o2 ; \n"
  , "   @has base2:p2     base2:o2 ; \n"
  , "   @is  base2:p2 @of base3:o3 ] = base1:s1 . \n"
  , " base2:s2 = \n"
  , " [ @has base1:p1 base1:o1 , \n"
  , "                 base2:o2 , \n"
  , "                 base3:o3 , \n"
  , "                 \"l1\"   ; \n"
  , "   @is  base2:p2 @of base1:o1 , \n"
  , "                     base2:o2 , \n"
  , "                     base3:o3 , \n"
  , "                     \"l1\"   ] . \n"
  ]

--  List nodes

exoticN3Graph_x4 :: B.Builder
exoticN3Graph_x4 =
    commonPrefixes `mappend`
    " base1:s1 = (base1:o1 base2:o2 base3:o3 \"l1\") .\n"

exoticN3Graph_x5 :: B.Builder
exoticN3Graph_x5 =
    commonPrefixes `mappend`
    " (base1:o1 base2:o2 base3:o3 \"l1\") = base1:s1 .\n"

--  Formula nodes, with and without :-

exoticN3Graph_x7 :: B.Builder
exoticN3Graph_x7 =
  mconcat
  [ commonPrefixes 
  , " { base1:s1 base1:p1 base1:o1 .   \n"
  , "   base2:s2 base1:p1 base2:o2 .   \n"
  , "   base3:s3 base1:p1 base3:o3 . } \n"
  , " base2:p2 base2:f2 . "
  ]
  
--  Test allocation of bnodes carries over a nested formula
exoticN3Graph_x12 :: B.Builder
exoticN3Graph_x12 =
  mconcat
  [ commonPrefixes 
  , " base1:s1 base1:p1 [ base1:p1 base1:o1 ] .     \n"
  , " { base2:s2 base2:p2 [ base2:p2 base2:o2 ] . } \n"
  , "            base2:p2 base2:f2 .                \n"
  , " base3:s3 base3:p3 [ base3:p3 base3:o3 ] ."
  ]

exoticTestSuite :: Test
exoticTestSuite = 
  TestList
  [ parseTest "exoticTest01"  exoticN3Graph_x1   x1  noError
  , parseTest "exoticTest01b" exoticN3Graph_x1b  x1  noError
  , parseTest "exoticTest02"  exoticN3Graph_x2   x2  noError
  , parseTest "exoticTest04" exoticN3Graph_x4  x4  noError
  , parseTest "exoticTest05" exoticN3Graph_x5  x5  noError
  , parseTest "exoticTest07" exoticN3Graph_x7  x7  noError
  , parseTest "exoticTest12" exoticN3Graph_x12 x12 noError
  , testGraphEq "exoticTest20" False x7 x8
  , testGraphEq "exoticTest21" False x8 x9
  ]

keywordN3Graph_01 :: B.Builder
keywordN3Graph_01 = 
  "@keywords .\n" `mappend`
  "b a c . "

-- a modification of simpleN3Graph_g8
keywordN3Graph_02 :: B.Builder
keywordN3Graph_02 = 
  mconcat
  [ commonPrefixes 
  , "@prefix : <http://id.ninebynine.org/wip/2003/test/graph1/node/>."  
  , "@keywords is,has.\n"
  , " s1 @a o1 . \n"
  , " base2:s2 = base2:o2 . \n"
  , " s1 is base1:p1 @of o1 . \n"
  , " base2:s2 has p1 base2:o2 . \n"
  , " s1 => o1 . \n"
  , " base2:s2 <= <http://id.ninebynine.org/wip/2003/test/graph2/node#o2> . \n" -- object is base2:o2
  ]
  
-- a modification of simpleN3Graph_g83
keywordN3Graph_03 :: B.Builder
keywordN3Graph_03 = 
  mconcat
  [ commonPrefixes
  , "@keywords of.\n"
  , " base1:s1 @is  base1:p1 of base1:o1 . \n"
  , " base2:s2 @has base1:p1 base2:o2 . \n"
  , " base1:s1 => base1:o1 . \n"
  , " base2:s2 <= base2:o2 . \n"
  ]

keywordTestSuite :: Test
keywordTestSuite = TestList
  [ parseTestB dqn "keywordTest01" keywordN3Graph_01  kg1  noError
  , parseTest "keywordTest02"      keywordN3Graph_02  g8  noError
  , parseTest "keywordTest03"      keywordN3Graph_03  g83 noError
  ]
    
------------------------------------------------------------
--  Test parser failure
------------------------------------------------------------
--
--  Very limited at the moment.
--

failTest :: String -> B.Builder -> String -> Test
failTest lbl gr = parseTest lbl gr emptyRDFGraph 

failN3Graph_g1 :: B.Builder
failN3Graph_g1 =
    commonPrefixes `mappend`
    " base1:s1 base2:p2 unknown3:o3 . "

fail1 :: String
{- parsec error
fail1 = intercalate "\n" [
         "",
         "@prefix base3 : <http://id.ninebynine.org/wip/2003/test/graph3/node> . ",
         " base1:s1 base2:p2 unknown3:o3 . ",
         "                            ^",
         "(line 4, column 29 indicated by the '^' sign above):",
         "",
         "unexpected Prefix 'unknown3:' not bound."
        ]
-}
fail1 = intercalate "\n" 
        ["When looking for a non-empty sequence with separators:"
        , "\tPrefix 'unknown3:' not bound."
        , "Remaining input:"
        , "o3 . "]

failTestSuite :: Test
failTestSuite = TestList
  [ failTest "failTest01" failN3Graph_g1 fail1
  ]

------------------------------------------------------------
--  All tests
------------------------------------------------------------

allTests :: [TF.Test]
allTests = 
  [ conv "char" charTestSuite
  , conv "name" nameTestSuite
  -- , prefixTestSuite
  , conv "absUriRef" absUriRefTestSuite
  , conv "uriRef2" uriRef2TestSuite
  , conv "simple" simpleTestSuite
  , conv "lit" litTestSuite
  , conv "exotic" exoticTestSuite
  , conv "keyword" keywordTestSuite
  , conv "fail" failTestSuite
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
