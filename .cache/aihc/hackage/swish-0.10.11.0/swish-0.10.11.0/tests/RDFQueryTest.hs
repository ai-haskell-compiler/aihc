{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  RDFQueryTest
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012, 2013, 2014, 2018, 2021 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  CPP, OverloadedStrings
--
--  This module defines test cases for querying an RDF graph to obtain
--  a set of variable substitutions, and to apply a set of variable
--  substitutions to a query pattern to obtain a new graph.
--
--  It also tests some primitive graph access functions.
--
--------------------------------------------------------------------------------

module Main where

import qualified Data.Set as S
import qualified Data.Text.Lazy.Builder as B

import qualified Test.Framework as TF

import Swish.Namespace (getNamespaceURI, ScopedName, makeScopedName)
import Swish.VarBinding
    ( VarBinding(..)
    , makeVarBinding
    , joinVarBindings
    , VarBindingModify(..)
    , makeVarFilterModify
    , varBindingId
    , varFilterNE
    )
import Swish.RDF.Query
    ( rdfQueryFind, rdfQueryFilter
    , rdfQueryBack, rdfQueryBackFilter, rdfQueryBackModify
    , rdfQueryInstance
    , rdfQuerySubs, rdfQueryBackSubs
    , rdfQuerySubsAll
    , rdfQuerySubsBlank, rdfQueryBackSubsBlank
    )

import Swish.RDF.VarBinding
    ( RDFVarBinding
    , RDFVarBindingFilter
    , rdfVarBindingUriRef, rdfVarBindingBlank
    , rdfVarBindingLiteral
    , rdfVarBindingUntypedLiteral, rdfVarBindingTypedLiteral
    , rdfVarBindingXMLLiteral, rdfVarBindingDatatyped
    , rdfVarBindingMemberProp
    )

import Swish.RDF.Graph (RDFGraph, RDFLabel(..), merge)

import Swish.RDF.Vocabulary (namespaceRDF, toLangTag, swishName, rdfType, rdfXMLLiteral)
import Swish.RDF.Parser.N3 (parseN3)

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Data.Monoid (Monoid(..))
#endif

import Data.Maybe (fromJust)

import Network.URI (URI, parseURI)

import Test.HUnit ( Test(TestList) )
import TestHelpers ( conv, test
                   , testEq
                   , testElem
                   , testEqv
                   )

------------------------------------------------------------
--  misc helpers
------------------------------------------------------------

testGr :: String -> B.Builder -> [RDFGraph] -> Test
testGr lab e = testElem lab (graphFromBuilder mempty e)

graphFromBuilder :: B.Builder -> B.Builder -> RDFGraph
graphFromBuilder prefix body = 
  let txt = B.toLazyText $ prefix `mappend` body
  in case parseN3 txt Nothing of
    Right gr -> gr
    Left msg -> error msg
    
------------------------------------------------------------
--  test1:  simple query qith URI, literal and blank nodes.
------------------------------------------------------------

prefix1 :: B.Builder
prefix1 = "@prefix ex: <http://example.org/> . \n"

gr1 :: B.Builder -> RDFGraph
gr1 = graphFromBuilder prefix1

graph1 :: RDFGraph
graph1 = gr1 $
         mconcat
         [ "ex:s1  ex:p  ex:o1 . \n"
         , "ex:s2  ex:p  \"lit1\" . \n"
         , "[ ex:p ex:o3 ] . \n" ]

query11 :: RDFGraph
query11 = gr1 "?s  ex:p  ?o . \n"

result11 :: RDFGraph
result11 = gr1 "?s  ex:r  ?o . \n"

result11a, result11b, result11c :: B.Builder

result11a = prefix1 `mappend` "ex:s1  ex:r  ex:o1 . \n"
result11b = prefix1 `mappend` "ex:s2  ex:r  \"lit1\" . \n"
result11c = prefix1 `mappend` "[ ex:r ex:o3 ] . \n"

-- Avoid incomplete-coverage warnings from ghc 9.2
get1 :: [x] -> x
get1 [x] = x
get1 _ = error "Invalid response - not a singleton"

get2 :: [x] -> (x, x)
get2 [x, y] = (x, y)
get2 _ = error "Invalid response - not a pair"


var11 :: [RDFVarBinding]
var11 = rdfQueryFind query11 graph1

res11 :: [RDFGraph]
res11 = rdfQuerySubs var11 result11

test1 :: Test
test1 = 
  TestList
  [ test "testQuery11" (not $ null var11)
  , testEq "testResult11" 3 (length res11)
  , testGr "testResult11a" result11a res11
  , testGr "testResult11b" result11b res11
  , testGr "testResult11c" result11c res11
  ]
  
------------------------------------------------------------
--  test2:  a range of more complex queries based on a
--  single relationship graph.
------------------------------------------------------------

prefix2 :: B.Builder
prefix2 =
  "@prefix pers: <urn:pers:> . \n" `mappend`
  "@prefix rel:  <urn:rel:> . \n"

gr2 :: B.Builder -> RDFGraph
gr2 = graphFromBuilder prefix2

graph2 :: RDFGraph
graph2 = gr2 $
         mconcat
         [ "pers:St1 rel:wife     pers:Do1 ; \n"
         , "         rel:daughter pers:Ma2 ; \n"
         , "         rel:daughter pers:An2 . \n"
         , "pers:Pa2 rel:wife     pers:Ma2 ; \n"
         , "         rel:son      pers:Gr3 ; \n"
         , "         rel:son      pers:La3 ; \n"
         , "         rel:son      pers:Si3 ; \n"
         , "         rel:son      pers:Al3 . \n"
         , "pers:Br2 rel:wife     pers:Ri2 ; \n"
         , "         rel:daughter pers:Ma3 ; \n"
         , "         rel:son      pers:Wi3 . \n"
         , "pers:Gr3 rel:wife     pers:Ma3 ; \n"
         , "         rel:son      pers:Ro4 ; \n"
         , "         rel:daughter pers:Rh4 . \n"
         , "pers:Si3 rel:wife     pers:Jo3 ; \n"
         , "         rel:son      pers:Ol4 ; \n"
         , "         rel:son      pers:Lo4 . \n"
         , "pers:Al3 rel:wife     pers:Su3 ; \n"
         , "         rel:son      pers:Ha4 ; \n"
         , "         rel:son      pers:El4 . \n"
         ]

query21 :: RDFGraph
query21 = gr2 "?a rel:wife ?b . \n"

result21 :: RDFGraph
result21 = gr2 "?b rel:husband ?a . \n"

result21a, result21b, result21c, result21d,
  result21e, result21f :: B.Builder

result21a = prefix2 `mappend` "pers:Do1 rel:husband pers:St1 . \n"
result21b = prefix2 `mappend` "pers:Ma2 rel:husband pers:Pa2 . \n"
result21c = prefix2 `mappend` "pers:Ri2 rel:husband pers:Br2 . \n"
result21d = prefix2 `mappend` "pers:Ma3 rel:husband pers:Gr3 . \n"
result21e = prefix2 `mappend` "pers:Jo3 rel:husband pers:Si3 . \n"
result21f = prefix2 `mappend` "pers:Su3 rel:husband pers:Al3 . \n"

var21 :: [RDFVarBinding]
var21 = rdfQueryFind query21 graph2

res21 :: [RDFGraph]
res21 = rdfQuerySubs var21 result21

query22 :: RDFGraph
query22 = gr2 $
          "?a rel:son ?b . \n" `mappend`
          "?b rel:son ?c . \n"

result22 :: RDFGraph
result22 = gr2 "?a rel:grandparent ?c . \n"

result22a, result22b, result22c, result22d, result22e :: B.Builder

result22a = prefix2 `mappend`
    "pers:Pa2 rel:grandparent pers:Ro4 . \n"

result22b = prefix2 `mappend`
    "pers:Pa2 rel:grandparent pers:Ol4 . \n"

result22c = prefix2 `mappend`
    "pers:Pa2 rel:grandparent pers:Lo4 . \n"

result22d = prefix2 `mappend`
    "pers:Pa2 rel:grandparent pers:Ha4 . \n"

result22e = prefix2 `mappend`
    "pers:Pa2 rel:grandparent pers:El4 . \n"

var22 :: [RDFVarBinding]
var22 = rdfQueryFind query22 graph2

res22 :: [RDFGraph]
res22 = rdfQuerySubs var22 result22

query23 :: RDFGraph
query23 = gr2 $
    "?a rel:son ?b . \n" `mappend`
    "?a rel:son ?c . \n"

result23 :: RDFGraph
result23 = gr2 "?b rel:brother ?c . \n"

result23a, result23b, result23c, result23d,
  result23e, result23f, result23g, result23h,
  result23i, result23j, result23k, result23l,
  result23m, result23n, result23o, result23p,
  result23q, result23r, result23s, result23t,
  result23u, result23v, result23w, result23x,
  result23y, result23z :: B.Builder

result23a = prefix2 `mappend`
    "pers:Gr3 rel:brother pers:Gr3 . \n"

result23b = prefix2 `mappend`
    "pers:Gr3 rel:brother pers:La3 . \n"

result23c = prefix2 `mappend`
    "pers:Gr3 rel:brother pers:Si3 . \n"

result23d = prefix2 `mappend`
    "pers:Gr3 rel:brother pers:Al3 . \n"

result23e = prefix2 `mappend`
    "pers:La3 rel:brother pers:Gr3 . \n"

result23f = prefix2 `mappend`
    "pers:La3 rel:brother pers:La3 . \n"

result23g = prefix2 `mappend`
    "pers:La3 rel:brother pers:Si3 . \n"

result23h = prefix2 `mappend`
    "pers:La3 rel:brother pers:Al3 . \n"

result23i = prefix2 `mappend`
    "pers:Si3 rel:brother pers:Gr3 . \n"

result23j = prefix2 `mappend`
    "pers:Si3 rel:brother pers:La3 . \n"

result23k = prefix2 `mappend`
    "pers:Si3 rel:brother pers:Si3 . \n"

result23l = prefix2 `mappend`
    "pers:Si3 rel:brother pers:Al3 . \n"

result23m = prefix2 `mappend`
    "pers:Al3 rel:brother pers:Gr3 . \n"

result23n = prefix2 `mappend`
    "pers:Al3 rel:brother pers:La3 . \n"

result23o = prefix2 `mappend`
    "pers:Al3 rel:brother pers:Si3 . \n"

result23p = prefix2 `mappend`
    "pers:Al3 rel:brother pers:Al3 . \n"

result23q = prefix2 `mappend`
    "pers:Wi3 rel:brother pers:Wi3 . \n"

result23r = prefix2 `mappend`
    "pers:Ro4 rel:brother pers:Ro4 . \n"

result23s = prefix2 `mappend`
    "pers:Ol4 rel:brother pers:Lo4 . \n"

result23t = prefix2 `mappend`
    "pers:Ol4 rel:brother pers:Ol4 . \n"

result23u = prefix2 `mappend`
    "pers:Lo4 rel:brother pers:Lo4 . \n"

result23v = prefix2 `mappend`
    "pers:Lo4 rel:brother pers:Ol4 . \n"

result23w = prefix2 `mappend`
    "pers:Ha4 rel:brother pers:El4 . \n"

result23x = prefix2 `mappend`
    "pers:Ha4 rel:brother pers:Ha4 . \n"

result23y = prefix2 `mappend`
    "pers:El4 rel:brother pers:El4 . \n"

result23z = prefix2 `mappend`
    "pers:El4 rel:brother pers:Ha4 . \n"

var23 :: [RDFVarBinding]
var23 = rdfQueryFind query23 graph2

res23 :: [RDFGraph]
res23 = rdfQuerySubs var23 result23

-- apply filtering to result:

filter23 :: RDFVarBindingFilter
filter23 = varFilterNE (Var "b") (Var "c")

var23F :: [RDFVarBinding]
var23F   = rdfQueryFilter filter23 var23

res23F :: [RDFGraph]
res23F   = rdfQuerySubs var23F result23

query24 :: RDFGraph
query24 = gr2 $
          "?a rel:daughter ?b . \n" `mappend`
          "?a rel:daughter ?c . \n"

result24 :: RDFGraph
result24 = gr2 "?b rel:sister ?c . \n"

result24a, result24b, result24c, result24d,
  result24e, result24f :: B.Builder
 
result24a = prefix2 `mappend`
            "pers:Ma2 rel:sister pers:Ma2 . \n"

result24b = prefix2 `mappend`
            "pers:Ma2 rel:sister pers:An2 . \n"

result24c = prefix2 `mappend`
            "pers:An2 rel:sister pers:Ma2 . \n"

result24d = prefix2 `mappend`
            "pers:An2 rel:sister pers:An2 . \n"

result24e = prefix2 `mappend`
            "pers:Ma3 rel:sister pers:Ma3 . \n"

result24f = prefix2 `mappend`
            "pers:Rh4 rel:sister pers:Rh4 . \n"

var24 :: [RDFVarBinding]
var24 = rdfQueryFind query24 graph2

res24 :: [RDFGraph]
res24 = rdfQuerySubs var24 result24

query25 :: RDFGraph
query25 = gr2 $
    "?a rel:son      ?b . \n" `mappend`
    "?a rel:daughter ?c . \n"

result25 :: RDFGraph
result25 = gr2 $
    "?b rel:sister  ?c . \n" `mappend`
    "?c rel:brother ?b . \n"

result25a, result25b :: B.Builder
result25a = 
  mconcat 
  [ prefix2
  , "pers:Wi3 rel:sister  pers:Ma3 . \n"
  , "pers:Ma3 rel:brother pers:Wi3 . \n"
  ]
  
result25b = 
  mconcat
  [ prefix2 
  , "pers:Ro4 rel:sister  pers:Rh4 . \n"
  , "pers:Rh4 rel:brother pers:Ro4 . \n"
  ]
  
var25 :: [RDFVarBinding]
var25 = rdfQueryFind query25 graph2

res25 :: [RDFGraph]
res25 = rdfQuerySubs var25 result25

test2 :: Test
test2 =
  TestList
  [ test "testQuery21" (not $ null var21)
  , testEq "testResult21" 6 (length res21)
  , testGr "testResult21a" result21a res21
  , testGr "testResult21b" result21b res21
  , testGr "testResult21c" result21c res21
  , testGr "testResult21d" result21d res21
  , testGr "testResult21e" result21e res21
  , testGr "testResult21f" result21f res21
  , test "testQuery22" (not $ null var22)
  , testEq "testResult22" 5 (length res22)
  , testGr "testResult22a" result22a res22
  , testGr "testResult22b" result22b res22
  , testGr "testResult22c" result22c res22
  , testGr "testResult22d" result22d res22
  , testGr "testResult22e" result22e res22
  , test "testQuery23" (not $ null var23)
  , testEq "testResult23" 26 (length res23)
  , testGr "testResult23a" result23a res23
  , testGr "testResult23b" result23b res23
  , testGr "testResult23c" result23c res23
  , testGr "testResult23d" result23d res23
  , testGr "testResult23e" result23e res23
  , testGr "testResult23f" result23f res23
  , testGr "testResult23g" result23g res23
  , testGr "testResult23h" result23h res23
  , testGr "testResult23i" result23i res23
  , testGr "testResult23j" result23j res23
  , testGr "testResult23k" result23k res23
  , testGr "testResult23l" result23l res23
  , testGr "testResult23m" result23m res23
  , testGr "testResult23n" result23n res23
  , testGr "testResult23o" result23o res23
  , testGr "testResult23p" result23p res23
  , testGr "testResult23q" result23q res23
  , testGr "testResult23r" result23r res23
  , testGr "testResult23s" result23s res23
  , testGr "testResult23t" result23t res23
  , testGr "testResult23u" result23u res23
  , testGr "testResult23v" result23v res23
  , testGr "testResult23w" result23w res23
  , testGr "testResult23x" result23x res23
  , testGr "testResult23y" result23y res23
  , testGr "testResult23z" result23z res23
  , testEq "testResult23" 16 (length res23F)
  , testGr "testResult23b" result23b res23F
  , testGr "testResult23c" result23c res23F
  , testGr "testResult23d" result23d res23F
  , testGr "testResult23e" result23e res23F
  , testGr "testResult23g" result23g res23F
  , testGr "testResult23h" result23h res23F
  , testGr "testResult23i" result23i res23F
  , testGr "testResult23j" result23j res23F
  , testGr "testResult23l" result23l res23F
  , testGr "testResult23m" result23m res23F
  , testGr "testResult23n" result23n res23F
  , testGr "testResult23o" result23o res23F
  , testGr "testResult23s" result23s res23F
  , testGr "testResult23v" result23v res23F
  , testGr "testResult23w" result23w res23F
  , testGr "testResult23z" result23z res23F
  , test "testQuery24" (not $ null var24)
  , testEq "testResult24" 6 (length res24)
  , testGr "testResult24a" result24a res24
  , testGr "testResult24b" result24b res24
  , testGr "testResult24c" result24c res24
  , testGr "testResult24d" result24d res24
  , testGr "testResult24e" result24e res24
  , testGr "testResult24f" result24f res24
    
  , test "testQuery25" (not $ null var25)
  , testEq "testResult25" 2 (length res25)
  , testGr "testResult25a" result25a res25
  , testGr "testResult25b" result25b res25
  ]

------------------------------------------------------------
--  test handling of unsubstituted variables, and
--  rdfQuerySubsAll, rdfQuerySubsBlank
------------------------------------------------------------

graph3 :: RDFGraph
graph3 = gr2 $
    "pers:Pa2 rel:grandparent pers:Ro4 . \n" `mappend`
    "pers:Pa2 rel:grandparent pers:Ol4 . \n"

query31 :: RDFGraph
query31 = gr2 "?a rel:grandparent ?c . \n"

result31 :: RDFGraph
result31 = gr2 $
    "?a rel:son ?b . \n" `mappend`
    "?b rel:son ?c . \n"

result31a, result31b :: B.Builder
result31a = 
  mconcat
  [ prefix2 
  , "pers:Pa2 rel:son ?b . \n" 
  , "?b rel:son pers:Ro4 . \n"
  ]
  
result31b = 
  mconcat
  [ prefix2
  , "pers:Pa2 rel:son ?b . \n"
  , "?b rel:son pers:Ol4 . \n"
  ]

var31 :: [RDFVarBinding]
var31 = rdfQueryFind query31 graph3

res31pairs :: [(RDFGraph, [RDFLabel])]
res31pairs = rdfQuerySubsAll var31 result31

res31 :: [RDFGraph]
res31v :: [[RDFLabel]]
(res31,res31v) = unzip res31pairs

query32 :: RDFGraph
query32 = gr2 "?a rel:grandparent ?c . \n"

result32 :: RDFGraph
result32 = gr2 $
           mconcat
           [ "?a rel:wife _:b  . \n"
           , "?d rel:any  _:b0 . \n"
           , "?a rel:son ?b . \n"   
           , "?b rel:son ?c . \n"
           ]
           
result32a, result32b :: B.Builder
result32a = 
  mconcat
  [ prefix2 
  , "pers:Pa2 rel:wife _:b      . \n"
  , "_:d0     rel:any  _:b0     . \n"
  , "pers:Pa2 rel:son  _:b1     . \n"
  , "_:b1     rel:son  pers:Ro4 . \n"
  ]

result32b = 
  mconcat
  [ prefix2
  , "pers:Pa2 rel:wife _:b      . \n"
  , "_:d0     rel:any  _:b0     . \n"
  , "pers:Pa2 rel:son  _:b1     . \n"
  , "_:b1     rel:son  pers:Ol4 . \n"
  ]

res32, res33 :: [RDFGraph]
res32 = rdfQuerySubsBlank var31 result32
res33 = rdfQuerySubs var31 result32

test3 :: Test
test3 = 
  TestList
  [ test "testQuery31" (not $ null var31)
  , testEq "testUnsubs31" 2 (length res31v)
  , testEq "testUnsubs31a" [Var "b"] (head res31v)
  , testEq "testUnsubs31a" [Var "b"] (head . tail $ res31v)
  , testEq "testResult31" 2 (length res31)
  , testGr "testResult31a" result31a res31
  , testGr "testResult31b" result31b res31
  , testEq "testResult32" 2 (length res32)
  , testGr "testResult32a" result32a res32
  , testGr "testResult32b" result32b res32
  , testEq "testResult33" 0 (length res33)
  ]

{-
--  Debug sequence for rdfQuerySubsBlank
--  (using internals of rdfQuerySubsBlank implementation)
--  res32 = rdfQuerySubsBlank (fromJust var31) result32
d1 = result32
d2 = rdfQuerySubs2 (head $ var31) d1
d3 = allLabels isBlank (fst d2)
d4 = remapLabels (snd d2) d3 makeBlank (fst d2)
-}

------------------------------------------------------------
--  test4:  test of backward-chaining query
------------------------------------------------------------

prefix4 :: B.Builder
prefix4 =
  "@prefix pers: <urn:pers:> . \n" `mappend`
  "@prefix rel:  <urn:rel:> . \n"

-- should use gr4l rather than gr
gr4 :: B.Builder -> RDFGraph
gr4 = graphFromBuilder prefix4

gr4l :: [B.Builder] -> RDFGraph
gr4l = graphFromBuilder prefix4 . mconcat

b4 :: [B.Builder] -> B.Builder
b4 = mconcat . (prefix4 :)

graph41 :: RDFGraph
graph41 = gr4 "pers:St1 rel:wife     pers:Do1 . \n"

query41 :: RDFGraph
query41 = gr4 "?a rel:wife ?b . \n"

result41 :: RDFGraph
result41 = gr4 "?b rel:husband ?a . \n"

result41a :: B.Builder
result41a = prefix4 `mappend`
            "pers:Do1 rel:husband pers:St1 . \n"

var41 :: [[RDFVarBinding]]
var41 = rdfQueryBack query41 graph41

res41 :: [[(RDFGraph, [RDFLabel])]]
res41 = rdfQueryBackSubs var41 result41

graph42 :: RDFGraph
graph42 = gr4 "pers:Pa2 rel:grandparent pers:Ro4 . \n"

query42 :: RDFGraph
query42 = gr4 "?a rel:grandparent ?c . \n"

result42 :: RDFGraph
result42 = gr4 $
    "?a rel:son ?b . \n" `mappend`
    "?b rel:son ?c . \n"

result42a :: B.Builder
result42a = 
  mconcat
  [ prefix4
  , "pers:Pa2 rel:son ?b       . \n"
  , "?b       rel:son pers:Ro4 . \n"
  ]
  
var42 :: [[RDFVarBinding]]
var42 = rdfQueryBack query42 graph42

res42 :: [[(RDFGraph, [RDFLabel])]]
res42 = rdfQueryBackSubs var42 result42

graph43 :: RDFGraph
graph43 = gr4 "pers:Gr3 rel:brother pers:La3 . \n"

query43 :: RDFGraph
query43 = gr4 "?b rel:brother ?c . \n"

result43 :: RDFGraph
result43 = gr4 $
    "?a rel:son ?b . \n" `mappend`
    "?a rel:son ?c . \n"

result43a :: B.Builder
result43a = 
  mconcat
  [ prefix4
  , "?a rel:son pers:Gr3 . \n"
  , "?a rel:son pers:La3 . \n"
  ]
  
var43 :: [[RDFVarBinding]]
var43 = rdfQueryBack query43 graph43

res43 :: [[(RDFGraph, [RDFLabel])]]
res43 = rdfQueryBackSubs var43 result43

graph44 :: RDFGraph
graph44 = gr4 "pers:Pa2 rel:grandson pers:Ro4 . \n"

query44 :: RDFGraph
query44 = gr4 $
    "?a rel:grandson ?b . \n" `mappend`
    "?c rel:grandson ?d . \n"

result44 :: RDFGraph
result44 = gr4 $
           mconcat
           [ "?a rel:son      ?m . \n"
           , "?m rel:son      ?b . \n"
           , "?c rel:daughter ?n . \n"
           , "?n rel:son      ?d . \n"
           ]
           
result44a, result44b :: B.Builder
result44a = 
  mconcat
  [ prefix4
  , "?a rel:son      ?m . \n"
  , "?m rel:son      ?b . \n"
  , "pers:Pa2 rel:daughter ?n .       \n"
  , "?n       rel:son      pers:Ro4 . \n"
  ]
  
result44b = 
  mconcat
  [ prefix4
  , "pers:Pa2 rel:son ?m       . \n"
  , "?m       rel:son pers:Ro4 . \n"
  , "?c rel:daughter ?n . \n"
  , "?n rel:son      ?d . \n"
  ]

unbound44a, unbound44b :: [RDFLabel]
unbound44a = [Var "a", Var "b", Var "m", Var "n"]
unbound44b = [Var "c", Var "d", Var "m", Var "n"]

var44 :: [[RDFVarBinding]]
var44 = rdfQueryBack query44 graph44

res44 :: [[(RDFGraph, [RDFLabel])]]
res44 = rdfQueryBackSubs var44 result44

res44_1, res44_2 :: [(RDFGraph, [RDFLabel])]
(res44_1, res44_2) = get2 res44


--  test45:  multiple substitutions used together
--
--  (?a daughter ?b, ?a son ?c) => ?b brother ?c
--
--  (b1 brother c1, b2 brother c2) if
--      (?a daughter b1, ?a son c1) && (?a daughter b2, ?a son c2)

graph45 :: RDFGraph
graph45 = gr4 $
    "pers:Rh4 rel:brother pers:Ro4 . \n" `mappend`
    "pers:Ma3 rel:brother pers:Wi3 . \n"

query45 :: RDFGraph
query45 = gr4 "?b rel:brother ?c . \n"

result45 :: RDFGraph
result45 = gr4 $
    "?a rel:daughter ?b . \n" `mappend`
    "?a rel:son      ?c . \n"

result45a1, result45a2 :: B.Builder
result45a1 = 
  mconcat
  [ prefix4
  , "?a rel:daughter pers:Rh4 . \n"
  , "?a rel:son      pers:Ro4 . \n"
  ]

result45a2 = 
  mconcat
  [ prefix4
  , "?a rel:daughter pers:Ma3 . \n"
  , "?a rel:son      pers:Wi3 . \n"
  ]
  
unbound45a1, unbound45a2 :: [RDFLabel]
unbound45a1 = [Var "a"]
unbound45a2 = [Var "a"]

var45 :: [[RDFVarBinding]]
var45 = rdfQueryBack query45 graph45

res45 :: [[(RDFGraph, [RDFLabel])]]
res45 = rdfQueryBackSubs var45 result45

res45_1 :: [(RDFGraph, [RDFLabel])]
res45_1 = get1 $ res45

res45_11, res45_12 :: (RDFGraph, [RDFLabel])
(res45_11, res45_12) = get2 $ res45_1

--  test46:  multiple ways to get solution
--
--  (?c son ?a, ?c stepSon b) => (?a stepBrother ?b, ?b stepBrother ?a)
--
--  a stepBrother b if
--      (_:c1 son a, _:c1 stepSon b) || (_:c2 stepSon a, _:c2 son b)

graph46 :: RDFGraph
graph46 = gr4 "pers:Gr3 rel:stepbrother pers:St3 . \n"

query46 :: RDFGraph
query46 = gr4 $
    "?b rel:stepbrother ?c . \n" `mappend`
    "?c rel:stepbrother ?b . \n"

result46 :: RDFGraph
result46 = gr4 $
    "?a rel:son     ?b . \n" `mappend`
    "?a rel:stepson ?c . \n"

result46a, result46b :: B.Builder
result46a = 
  mconcat
  [ prefix4
  , "?a rel:son     pers:Gr3 . \n"
  , "?a rel:stepson pers:St3 . \n"
  ]
  
result46b = 
  mconcat
  [ prefix4
  , "?a rel:son     pers:St3 . \n"
  , "?a rel:stepson pers:Gr3 . \n"
  ]
  
unbound46a, unbound46b :: [RDFLabel]
unbound46a = [Var "a"]
unbound46b = [Var "a"]

var46 :: [[RDFVarBinding]]
var46 = rdfQueryBack query46 graph46

res46 :: [[(RDFGraph, [RDFLabel])]]
res46 = rdfQueryBackSubs var46 result46

res46_1, res46_2 :: [(RDFGraph, [RDFLabel])]
(res46_1, res46_2) = get2 res46

res46_11, res46_21 :: (RDFGraph, [RDFLabel])
res46_11 = get1 res46_1
res46_21 = get1 res46_2

--  test47:  multiple ways to multiple solutions
--
--  (?c son ?a, ?c stepSon b) => (?a stepBrother ?b, ?b stepBrother ?a)
--
--  (a stepBrother b, c stepBrother d) if
--      ((_:e son a, _:e stepSon b) && (_:f son a, _:f stepSon b)) ||
--      ((_:e son a, _:e stepSon b) && (_:f stepSon a, _:f son b)) ||
--      ((_:e stepSon a, _:e son b) && (_:f son a, _:f stepSon b)) ||
--      ((_:e stepSon a, _:e son b) && (_:f stepSon a, _:f son b))

graph47 :: RDFGraph
graph47 = gr4 $
    "pers:Gr3 rel:stepbrother pers:St3 . \n" `mappend`
    "pers:St3 rel:stepbrother pers:Gr3 . \n"

query47 :: RDFGraph
query47 = gr4 $
    "?b rel:stepbrother ?c . \n" `mappend`
    "?c rel:stepbrother ?b . \n"

result47 :: RDFGraph
result47 = gr4 $
    "?a rel:son     ?b . \n" `mappend`
    "?a rel:stepson ?c . \n"

result47a1, result47a2,
  result47b1, result47b2,
  result47c1, result47c2,
  result47d1, result47d2 :: B.Builder

result47a1 = 
  b4 [ "?a rel:son     pers:St3 . \n"
     , "?a rel:stepson pers:Gr3 . \n"]

result47a2 = 
  b4 [ "?a rel:son     pers:Gr3 . \n"
     , "?a rel:stepson pers:St3 . \n"]

result47b1 = 
  b4 [ "?a rel:stepson pers:St3 . \n"
     , "?a rel:son     pers:Gr3 . \n"]

result47b2 = 
  b4 [ "?a rel:stepson pers:St3 . \n"
     , "?a rel:son     pers:Gr3 . \n"]

result47c1 = 
  b4 [ "?a rel:son     pers:St3 . \n"
     , "?a rel:stepson pers:Gr3 . \n"]

result47c2 = 
  b4 [ "?a rel:son     pers:St3 . \n"
     , "?a rel:stepson pers:Gr3 . \n"]

result47d1 =
  b4 [ "?a rel:stepson pers:St3 . \n"
     , "?a rel:son     pers:Gr3 . \n"]

result47d2 = 
  b4 [ "?a rel:son     pers:St3 . \n"
     , "?a rel:stepson pers:Gr3 . \n"]

unbound47a1, unbound47a2,
  unbound47b1, unbound47b2,
  unbound47c1, unbound47c2,
  unbound47d1, unbound47d2 :: [RDFLabel]

unbound47a1 = [Var "a"]
unbound47a2 = [Var "a"]
unbound47b1 = [Var "a"]
unbound47b2 = [Var "a"]
unbound47c1 = [Var "a"]
unbound47c2 = [Var "a"]
unbound47d1 = [Var "a"]
unbound47d2 = [Var "a"]

var47 :: [[RDFVarBinding]]
var47 = rdfQueryBack query47 graph47

res47 :: [[(RDFGraph, [RDFLabel])]]
res47 = rdfQueryBackSubs var47 result47

res47_1, res47_2, res47_3, res47_4 :: [(RDFGraph, [RDFLabel])]
(res47_1, res47_2, res47_3, res47_4) = case res47 of
  [a, b, c, d] -> (a, b, c, d)
  _ -> error "Expected 4 answers"
  

res47_11, res47_12,
  res47_21, res47_22, 
  res47_31, res47_32, 
  res47_41, res47_42 :: (RDFGraph, [RDFLabel])
(res47_11, res47_12) = get2 res47_1
(res47_21, res47_22) = get2 res47_2
(res47_31, res47_32) = get2 res47_3
(res47_41, res47_42) = get2 res47_4

--  test48:  redundant multiple ways to get solution
--
--  (?a son ?b, ?a son ?c) => (?b brother ?c, ?c brother ?b)
--
--  (a brother b) if
--      (_:c1 son a, _:c1 son b) || (_:c2 son b, _:c2 son a)

graph48 :: RDFGraph
graph48    = gr4 "pers:Gr3 rel:brother pers:La3 . \n"

query48 :: RDFGraph
query48    = gr4 $
    "?b rel:brother ?c . \n" `mappend`
    "?c rel:brother ?b . \n"

result48 :: RDFGraph
result48    = gr4 $
    "?a rel:son ?b . \n" `mappend`
    "?a rel:son ?c . \n"

result48a, result48b :: B.Builder
result48a = 
  b4 [ "?a rel:son pers:La3 . \n"
     , "?a rel:son pers:Gr3 . \n"]
result48b =
  b4 [ "?a rel:son pers:Gr3 . \n"
     , "?a rel:son pers:La3 . \n"]
    
unbound48a, unbound48b :: [RDFLabel]
unbound48a = [Var "a"]
unbound48b = [Var "a"]

var48 :: [[RDFVarBinding]]
var48 = rdfQueryBack query48 graph48

res48 :: [[(RDFGraph, [RDFLabel])]]
res48 = rdfQueryBackSubs var48 result48

res48_1, res48_2 :: [(RDFGraph, [RDFLabel])]
(res48_1, res48_2) = get2 res48

res48_11, res48_21 :: (RDFGraph, [RDFLabel])
res48_11 = get1 res48_1
res48_21 = get1 res48_2

-- test49: goal not satisfiable by rule
--
--  (?a foo ?b, ?b foo ?a) => (?a bar ?a)
--
--  (a bar b) cannot be deduced directly

graph49 :: RDFGraph
graph49 = gr4l ["pers:Gr3 rel:foo pers:La3 . \n"]

query49 :: RDFGraph
query49 = gr4l ["?a rel:bar ?a . \n"]

result49 :: RDFGraph
result49 = gr4l 
           [ "?a rel:foo ?b . \n"
           , "?b rel:foo ?a . \n"]

var49 :: [[RDFVarBinding]]
var49 = rdfQueryBack query49 graph49

res49 :: [[(RDFGraph, [RDFLabel])]]
res49 = rdfQueryBackSubs var49 result49

--  test50:  back-chaining with filter
--
--  (?a son ?b, ?a son ?c) => (?b brother ?c, ?c brother ?b)
--
--  (a brother b) if
--      (_:c1 son a, _:c1 son b) || (_:c2 son b, _:c2 son a)

graph50 :: RDFGraph
graph50 = gr4l ["pers:Gr3 rel:brother pers:Gr3 . \n"]

query50 :: RDFGraph
query50 = gr4l
          [ "?b rel:brother ?c . \n"
          , "?c rel:brother ?b . \n"]

result50 :: RDFGraph
result50 = gr4l 
           [ "?a rel:son ?b . \n"
           , "?a rel:son ?c . \n"]

result50a, result50b :: B.Builder
result50a = 
  b4 [ "?a rel:son pers:Gr3 . \n"
     , "?a rel:son pers:Gr3 . \n"]

result50b = 
  b4 [ "?a rel:son pers:Gr3 . \n"
     , "?a rel:son pers:Gr3 . \n"]

unbound50a, unbound50b :: [RDFLabel]
unbound50a = [Var "a"]
unbound50b = [Var "a"]

var50 :: [[RDFVarBinding]]
var50 = rdfQueryBack query50 graph50

res50 :: [[(RDFGraph, [RDFLabel])]]
res50 = rdfQueryBackSubs var50 result50

res50_1, res50_2 :: [(RDFGraph, [RDFLabel])]
(res50_1, res50_2) = get2 res50

res50_11, res50_21 :: (RDFGraph, [RDFLabel])
res50_11 = get1 res50_1
res50_21 = get1 res50_2

filter50 :: RDFVarBindingFilter
filter50 = varFilterNE (Var "b") (Var "c")

var50F :: [[RDFVarBinding]]
var50F = rdfQueryBackFilter filter50 var50

res50F :: [[(RDFGraph, [RDFLabel])]]
res50F = rdfQueryBackSubs var50F result50

--  Backward substitution query test suite

test4 :: Test
test4 = 
  TestList
  [ test "testQuery41" (not $ null var41)
  , testEq "testQuery41a" 1 (length var41)
  , testEq "testResult41" 1 (length res41)
  , testGr "testResult41a" result41a (map fst (head res41))
  , testEqv "testUnbound41a" [] (snd $ head $ head res41)
  , test "testQuery42" (not $ null var42)
  , testEq "testQuery42a" 1 (length var42)
  , testEq "testResult42" 1 (length res42)
  , testGr "testResult42a" result42a (map fst (head res42))
  , testEqv "testUnbound42a" [Var "b"] (snd $ head $ head res42)
  , test "testQuery43" (not $ null var43)
  , testEq "testQuery43a" 1 (length var43)
  , testEq "testResult43" 1 (length res43)
  , testGr "testResult43a" result43a (map fst (head res43))
  , testEqv "testUnbound43a" [Var "a"] (snd $ head $ head res43)
  , test "testQuery44" (not $ null var44)
  , testEq "testQuery44a"   2 (length var44)
  , testEq "testResult44"   2 (length res44)
  , testGr "testResult44a"  result44a  (map fst res44_2)
  , testEqv "testUnbound44a" unbound44a (snd $ head res44_2)
  , testGr "testResult44b"  result44b  (map fst res44_1)
  , testEqv "testUnbound44b" unbound44b (snd $ head res44_1)
  , test "testQuery45" (not $ null var45)
  , testEq "testQuery45a"   1 (length var45)
  , testEq "testResult45"   1 (length res45)
  , testEq "testResult45_1" 2 (length res45_1)
  , testGr "testResult45a1"  result45a1  [fst res45_11]
  , testEqv "testUnbound45a1" unbound45a1 (snd res45_11)
  , testGr "testResult45a2"  result45a2  [fst res45_12]
  , testEqv "testUnbound45a2" unbound45a2 (snd res45_12)
  , test "testQuery46" (not $ null var46)
  , testEq "testQuery46a"   2 (length var46)
  , testEq "testResult46"   2 (length res46)
  , testEq "testResult46_1" 1 (length res46_1)
  , testEq "testResult46_2" 1 (length res46_2)
  , testGr "testResult46a"  result46a  [fst res46_11]
  , testEqv "testUnbound46a" unbound46a (snd res46_11)
  , testGr "testResult46b"  result46b  [fst res46_21]
  , testEqv "testUnbound46b" unbound46b (snd res46_21)
  , test "testQuery47" (not $ null var47)
  , testEq "testQuery47a"   4 (length var47)
  , testEq "testResult47"   4 (length res47)
  , testEq "testResult47_1" 2 (length res47_1)
  , testEq "testResult47_2" 2 (length res47_2)
  , testEq "testResult47_3" 2 (length res47_3)
  , testEq "testResult47_4" 2 (length res47_4)
  , testGr "testResult47a1"  result47a1  [fst res47_11]
  , testEqv "testUnbound47a1" unbound47a1 (snd res47_11)
  , testGr "testResult47a2"  result47a2  [fst res47_12]
  , testEqv "testUnbound47a2" unbound47a2 (snd res47_12)
  , testGr "testResult47b1"  result47b1  [fst res47_21]
  , testEqv "testUnbound47b1" unbound47b1 (snd res47_21)
  , testGr "testResult47b2"  result47b2  [fst res47_22]
  , testEqv "testUnbound47b2" unbound47b2 (snd res47_22)
  , testGr "testResult47c1"  result47c1  [fst res47_31]
  , testEqv "testUnbound47c1" unbound47c1 (snd res47_31)
  , testGr "testResult47c2"  result47c2  [fst res47_32]
  , testEqv "testUnbound47c2" unbound47c2 (snd res47_32)
  , testGr "testResult47d1"  result47d1  [fst res47_41]
  , testEqv "testUnbound47d1" unbound47d1 (snd res47_41)
  , testGr "testResult47d2"  result47d2  [fst res47_42]
  , testEqv "testUnbound47d2" unbound47d2 (snd res47_42)
  , test "testQuery48" (not $ null var48)
  , testEq "testQuery48a"   2 (length var48)
  , testEq "testResult48"   2 (length res48)
  , testEq "testResult48_1" 1 (length res48_1)
  , testEq "testResult48_2" 1 (length res48_2)
  , testGr "testResult48a"  result48a  [fst res48_11]
  , testEqv "testUnbound48a" unbound48a (snd res48_11)
  , testGr "testResult48b"  result48b  [fst res48_21]
  , testEqv "testUnbound48b" unbound48b (snd res48_21)
  , test "testQuery49" (null var49)
  , testEq "testQuery49a"   0 (length var49)
  , testEq "testResult49"   0 (length res49)
  , test "testQuery50" (not $ null var50)
  , testEq "testQuery50a"   2 (length var50)
  , testEq "testResult50"   2 (length res50)
  , testEq "testResult50_1" 1 (length res50_1)
  , testEq "testResult50_2" 1 (length res50_2)
  , testGr "testResult50a"  result50a  [fst res50_11]
  , testEqv "testUnbound50a" unbound50a (snd res50_11)
  , testGr "testResult50b"  result50b  [fst res50_21]
  , testEqv "testUnbound50b" unbound50b (snd res50_21)
  , testEq "testResult50F" 0 (length res50F)
  ]

------------------------------------------------------------
--  Instance query test suite
------------------------------------------------------------
--
--  The test plan is this:
--  (1) perform a backward chaining query against some desired result.
--      ?f father ?a, ?f father ?b, ?a /= ?b => ?a brother ?b
--      against
--      Gr3 brother La3, Gr3 brother Si3
--      should yield:
--      _:a father Gr3
--      _:a father La3
--      _:b father Gr3
--      _:b father Si3
--  (2) Perform instance query of result against 'graph2' (see above)
--      should yield:
--      _:a = Pa2
--      _:b = Pa2
--  (3) Substitute this into query, should yield:
--      Pa2 father Gr3
--      Pa2 father La3
--      Pa2 father Gr3
--      Pa2 father Si3
--  (4) Use this result in an instance query against 'graph2':  it should
--      match without any variable substitutions, indicating that it is
--      a subgraph

graph61 :: RDFGraph
graph61 = gr4l 
          [ "pers:Gr3 rel:brother pers:La3 . \n"
          , "pers:Gr3 rel:brother pers:Si3 . \n"]

query61 :: RDFGraph
query61 = gr4l ["?b rel:brother ?c . \n"]

result61 :: RDFGraph
result61 = gr4l 
           [ "?a rel:son ?b . \n"
           , "?a rel:son ?c . \n"]

result61a, result63a :: B.Builder
result61a = 
  b4 [ "_:a1 rel:son pers:Gr3 . \n"
     , "_:a1 rel:son pers:La3 . \n"
     , "_:a2 rel:son pers:Gr3 . \n"
     , "_:a2 rel:son pers:Si3 . \n"]

result63a = 
  b4 [ "pers:Pa2 rel:son pers:Gr3 . \n"
     , "pers:Pa2 rel:son pers:La3 . \n"
     , "pers:Pa2 rel:son pers:Gr3 . \n"
     , "pers:Pa2 rel:son pers:Si3 . \n"]

--  1. Backchain query with blank substutions

var61 :: [[RDFVarBinding]]
var61          = rdfQueryBack query61 graph61

res61 :: [[RDFGraph]]
res61          = rdfQueryBackSubsBlank var61 result61

res61a1, res61a2, res61a :: RDFGraph
(res61a1, res61a2) = get2 $ get1 $ res61
res61a = merge res61a1 res61a2

--  2. Instance query against 'graph2'

var62 :: [RDFVarBinding]
var62 = rdfQueryInstance res61a graph2

--  3. Substitute into instance query graph

res63 :: [RDFGraph]
res63 = rdfQuerySubs var62 res61a

res63a :: RDFGraph
res63a = get1 res63

--  4. Repeat instance query against 'graph2'
--     Query bindings should be null.

var64 :: [RDFVarBinding]
var64 = rdfQueryInstance res63a graph2

var64a :: RDFVarBinding
var64a = get1 var64

test6 :: Test
test6 = 
  TestList
  [ test   "testQuery61" (not $ null var61)
  , testEq "testQuery61a" 1 (length var61)
  , testEq "testResult61" 1 (length res61)
  , testGr "testResult61a" result61a [res61a]
  , test   "testQuery62" (not $ null var62)
  , testEq "testQuery62a" 1 (length var62)
  , test   "testQuery63" (not $ null res63)
  , testEq "testQuery63a" 1 (length res63)
  , testGr "testResult63a" result63a [res63a]
  , test   "testQuery64" (not $ null var64)
  , testEq "testQuery64a" 1 (length var64)
  , test   "testQuery64b" (S.null $ vbEnum var64a)
  ]    

------------------------------------------------------------
--  Specific test cases
------------------------------------------------------------

--  Back-chaining query binding modifier

--  Set up call of rdfQueryBackModify
--  (1) simple filter
--  (2) allocate new binding
{-
rdfQueryBackModify ::
    RDFVarBindingModify -> [[RDFVarBinding]] -> [[RDFVarBinding]]
rdfQueryBackModify qbm qbss = concatMap (rdfQueryBackModify1 qbm) qbss
-}

toURI :: String -> URI
toURI = fromJust . parseURI

baseex :: URI
baseex = toURI "http://example.org/"

baserdf :: URI
baserdf  = getNamespaceURI namespaceRDF

q_dattyp :: ScopedName
q_dattyp = makeScopedName Nothing baseex "datatype"

v_a, v_b, v_c, v_x, v_y, v_z :: RDFLabel
v_a   = Var "a"
v_b   = Var "b"
v_c   = Var "c"
v_x   = Var "x"
v_y   = Var "y"
v_z   = Var "z"

u_s, u_o, u_p, u_p1, u_p2a, u_p2b, u_m1, u_m2,
  u_rt, u_xt, u_dt :: RDFLabel
u_s   = Res $ makeScopedName Nothing baseex "s"
u_o   = Res $ makeScopedName Nothing baseex "o"
u_p   = Res $ makeScopedName Nothing baseex "p"
u_p1  = Res $ makeScopedName Nothing baseex "p1"
u_p2a = Res $ makeScopedName Nothing baseex "p2a"
u_p2b = Res $ makeScopedName Nothing baseex "p2b"
u_m1  = Res $ makeScopedName Nothing baserdf "_1"
u_m2  = Res $ makeScopedName Nothing baserdf "_2"
u_rt  = Res rdfType
u_xt  = Res rdfXMLLiteral
u_dt  = Res q_dattyp

l_1, l_2, l_3, l_4, l_5 :: RDFLabel
l_1   = Lit "l1"
l_2   = LangLit "l2" $ fromJust $ toLangTag "fr"
l_3   = TypedLit "l3" q_dattyp
l_4   = TypedLit "l4" q_dattyp
l_5   = TypedLit "l5" rdfXMLLiteral

b_1, b_2, b_3, b_l1, b_l2 :: RDFLabel
b_1   = Blank "1"
b_2   = Blank "2"
b_3   = Blank "3"
b_l1  = Blank "l1"
b_l2  = Blank "l2"

vbss01a, vbss01b, vbss01c, vbss01d, vbss01e, vbss01f,
  vbss01g, vbss01h, vbss01i :: [RDFVarBinding]
vbss01a =               -- ?a is uri, ?b is uri
    [ makeVarBinding [ (v_a,u_s), (v_b,u_p),  (v_c,u_o) ]
    , makeVarBinding [ (v_a,u_s), (v_b,u_p),  (v_c,b_1) ]
    , makeVarBinding [ (v_a,u_s), (v_b,u_p),  (v_c,l_1) ]
    ]

vbss01b =               -- ?c is blank
    [ makeVarBinding [ (v_a,u_s), (v_b,u_p),  (v_c,b_1) ]
    ]

vbss01c =               -- ?c is literal
    [ makeVarBinding [ (v_a,u_s), (v_b,u_p),  (v_c,l_1) ]
    , makeVarBinding [ (v_a,u_s), (v_b,u_p),  (v_c,l_2) ]
    , makeVarBinding [ (v_a,u_s), (v_b,u_p),  (v_c,l_3) ]
    ]

vbss01d =               -- ?c is untyped literal
    [ makeVarBinding [ (v_a,u_s), (v_b,u_p),  (v_c,l_1) ]
    , makeVarBinding [ (v_a,u_s), (v_b,u_p),  (v_c,l_2) ]
    ]

vbss01e =               -- ?c is typed literal
    [ makeVarBinding [ (v_a,u_s), (v_b,u_p),  (v_c,l_3) ]
    , makeVarBinding [ (v_a,u_s), (v_b,u_p),  (v_c,l_4) ]
    , makeVarBinding [ (v_a,b_3), (v_b,u_p),  (v_c,l_5) ]
    ]

vbss01f =               -- ?c is XML literal
    [ makeVarBinding [ (v_a,b_1), (v_b,u_p),  (v_c,l_5) ]
    ]

vbss01g =               -- ?b is member property
    [ makeVarBinding [ (v_a,b_1), (v_b,u_m1), (v_c,u_o) ]
    , makeVarBinding [ (v_a,u_s), (v_b,u_m2), (v_c,b_1) ]
    ]

vbss01h =               -- ?c is datatyped with ?x
    [ makeVarBinding [ (v_a,b_1), (v_b,u_p),  (v_c,l_3), (v_x,u_dt) ]
    , makeVarBinding [ (v_a,b_2), (v_b,u_p),  (v_c,l_4), (v_x,u_dt) ]
    , makeVarBinding [ (v_a,u_s), (v_b,u_p),  (v_c,l_5), (v_x,u_xt) ]
    ]

vbss01i =               -- ?c is not datatyped with ?x
    [ makeVarBinding [ (v_a,b_1), (v_b,u_p),  (v_c,l_3), (v_x,u_dt) ]
    , makeVarBinding [ (v_a,b_2), (v_b,u_p),  (v_c,l_4), (v_x,u_xt) ]
    , makeVarBinding [ (v_a,b_3), (v_b,u_p),  (v_c,l_5), (v_x,u_xt) ]
    ]

vbss01 :: [[RDFVarBinding]]
vbss01  = [ vbss01a     -- ?a is uri, ?b is uri
          , vbss01b     -- ?c is blank
          , vbss01c     -- ?c is literal
          , vbss01d     -- ?c is untyped literal
          , vbss01e     -- ?c is typed literal
          , vbss01f     -- ?c is XML literal
          , vbss01g     -- ?b is member property
          , vbss01h     -- ?c is datatyped with ?x
          , vbss01i     -- ?c is not datatyped with ?x
          ]

vbss02a, vbss02b, vbss02c, vbss02d :: [RDFVarBinding]

vbss02a = [ makeVarBinding [ (v_x,u_s), (v_a,u_p1),  (v_b,b_l1) ]
          , makeVarBinding [ (v_x,u_s), (v_a,u_p2a), (v_b,b_l2) ]
          , makeVarBinding [ (v_x,u_s), (v_a,u_p2b), (v_b,b_l2) ]
          , makeVarBinding [ (v_b,b_l1) ]
          , makeVarBinding [ (v_b,b_l2) ]
          ]

vbss02b = [ makeVarBinding [ (v_x,u_s),  (v_a,u_p1),  (v_b,b_l1) ]
          , makeVarBinding [ (v_x,u_s),  (v_a,u_p2a), (v_b,b_l2) ]
          , makeVarBinding [ (v_x,u_s),  (v_a,u_p2b), (v_b,b_l2) ]
          , makeVarBinding [ (v_x,b_l1), (v_a,u_rt),  (v_b,u_xt) ]
          , makeVarBinding [ (v_b,b_l2) ]
          ]

vbss02c = [ makeVarBinding [ (v_x,u_s),  (v_a,u_p1),  (v_b,b_l1) ]
          , makeVarBinding [ (v_x,u_s),  (v_a,u_p2a), (v_b,b_l2) ]
          , makeVarBinding [ (v_x,u_s),  (v_a,u_p2b), (v_b,b_l2) ]
          , makeVarBinding [ (v_b,b_l1) ]
          , makeVarBinding [ (v_x,b_l2), (v_a,u_rt),  (v_b,u_xt) ]
          ]

vbss02d = [ makeVarBinding [ (v_x,u_s),  (v_a,u_p1),  (v_b,b_l1) ]
          , makeVarBinding [ (v_x,u_s),  (v_a,u_p2a), (v_b,b_l2) ]
          , makeVarBinding [ (v_x,u_s),  (v_a,u_p2b), (v_b,b_l2) ]
          , makeVarBinding [ (v_x,b_l1), (v_a,u_rt),  (v_b,u_xt) ]
          , makeVarBinding [ (v_x,b_l2), (v_a,u_rt),  (v_b,u_xt) ]
          ]

vbss02 :: [[RDFVarBinding]]
vbss02  = [ vbss02a
          , vbss02b
          , vbss02c
          , vbss02d
          ]

--  Variable binding modifier that adds new bindings, if certain
--  others are present.

vbm22 :: VarBindingModify RDFLabel RDFLabel
vbm22 = VarBindingModify
        { vbmName  = swishName "vbm22"
        , vbmApply = concatMap apply1
        , vbmVocab = [v_a,v_b,v_x,v_y]
        , vbmUsage = [[v_y]]
        }
    where
        apply1 :: RDFVarBinding -> [RDFVarBinding]
        apply1 vb = apply2 vb (vbMap vb v_a) (vbMap vb v_b) (vbMap vb v_x)
        apply2 vb (Just a) (Just b) (Just _) =
            [ joinVarBindings nva vb, joinVarBindings nvb vb ]
            where
                nva = makeVarBinding [(v_y,a)]
                nvb = makeVarBinding [(v_y,b)]
        apply2 _ _ _ _ = []

vbss02dy :: [[RDFVarBinding]]
vbss02dy = sequence
    [ [ makeVarBinding [ (v_x,u_s),  (v_a,u_p1),  (v_b,b_l1), (v_y,u_p1)  ]
      , makeVarBinding [ (v_x,u_s),  (v_a,u_p1),  (v_b,b_l1), (v_y,b_l1)  ]
      ]
    , [ makeVarBinding [ (v_x,u_s),  (v_a,u_p2a), (v_b,b_l2), (v_y,u_p2a) ]
      , makeVarBinding [ (v_x,u_s),  (v_a,u_p2a), (v_b,b_l2), (v_y,b_l2)  ]
      ]
    , [ makeVarBinding [ (v_x,u_s),  (v_a,u_p2b), (v_b,b_l2), (v_y,u_p2b) ]
      , makeVarBinding [ (v_x,u_s),  (v_a,u_p2b), (v_b,b_l2), (v_y,b_l2)  ]
      ]
    , [ makeVarBinding [ (v_x,b_l1), (v_a,u_rt),  (v_b,u_xt), (v_y,u_rt)  ]
      , makeVarBinding [ (v_x,b_l1), (v_a,u_rt),  (v_b,u_xt), (v_y,u_xt)  ]
      ]
    , [ makeVarBinding [ (v_x,b_l2), (v_a,u_rt),  (v_b,u_xt), (v_y,u_rt)  ]
      , makeVarBinding [ (v_x,b_l2), (v_a,u_rt),  (v_b,u_xt), (v_y,u_xt)  ]
      ]
    ]

--  simplified version of above for debugging --

vbss03a :: [RDFVarBinding]
vbss03a = [ makeVarBinding [ (v_x,u_s), (v_a,u_p1),  (v_b,b_l1) ]
          , makeVarBinding [ (v_b,b_l1) ]
          ]

vbss03b :: [RDFVarBinding]
vbss03b = [ makeVarBinding [ (v_x,u_s),  (v_a,u_p1),  (v_b,b_l1) ]
          , makeVarBinding [ (v_x,u_s),  (v_a,u_p2a), (v_b,b_l2) ]
          ]

vbss03 :: [[RDFVarBinding]]
vbss03  = [ vbss03a
          , vbss03b
          ]

vbss03by :: [[RDFVarBinding]]
vbss03by = sequence
    [ [ makeVarBinding [ (v_x,u_s),  (v_a,u_p1),  (v_b,b_l1), (v_y,u_p1)  ]
      , makeVarBinding [ (v_x,u_s),  (v_a,u_p1),  (v_b,b_l1), (v_y,b_l1)  ]
      ]
    , [ makeVarBinding [ (v_x,u_s),  (v_a,u_p2a), (v_b,b_l2), (v_y,u_p2a) ]
      , makeVarBinding [ (v_x,u_s),  (v_a,u_p2a), (v_b,b_l2), (v_y,b_l2)  ]
      ]
    ]

test7 :: Test
test7 = TestList
    [ testEq "testBackMod01" vbss01 $
                rdfQueryBackModify varBindingId vbss01
    , testEq "testBackMod02" [vbss01a,vbss01b,vbss01c,vbss01d] $
                rdfQueryBackModify
                    (makeVarFilterModify $ rdfVarBindingUriRef v_a)
                    vbss01
    , testEq "testBackMod03" [vbss01f,vbss01i] $
                rdfQueryBackModify
                    (makeVarFilterModify $ rdfVarBindingBlank v_a)
                    vbss01
    , testEq "testBackMod04" vbss01 $
                rdfQueryBackModify
                    (makeVarFilterModify $ rdfVarBindingUriRef v_b)
                    vbss01
    , testEq "testBackMod05"
                [vbss01c,vbss01d,vbss01e,vbss01f,vbss01h,vbss01i] $
                rdfQueryBackModify
                    (makeVarFilterModify $ rdfVarBindingLiteral v_c)
                    vbss01
    , testEq "testBackMod06" [vbss01d] $
                rdfQueryBackModify
                    (makeVarFilterModify $ rdfVarBindingUntypedLiteral v_c)
                    vbss01
    , testEq "testBackMod07" [vbss01e,vbss01f,vbss01h,vbss01i] $
                rdfQueryBackModify
                    (makeVarFilterModify $ rdfVarBindingTypedLiteral v_c)
                    vbss01
    , testEq "testBackMod08" [vbss01f] $
                rdfQueryBackModify
                    (makeVarFilterModify $ rdfVarBindingXMLLiteral v_c)
                    vbss01
    , testEq "testBackMod09" [vbss01g] $
                rdfQueryBackModify
                    (makeVarFilterModify $ rdfVarBindingMemberProp v_b)
                    vbss01
    , testEq "testBackMod10" [vbss01h] $
                rdfQueryBackModify
                    (makeVarFilterModify $ rdfVarBindingDatatyped v_x v_c)
                    vbss01
    , testEq "testBackMod20" vbss02 $
                rdfQueryBackModify varBindingId vbss02
    , testEq "testBackMod21" [vbss02d] $
                rdfQueryBackModify
                    (makeVarFilterModify $ rdfVarBindingUriRef v_a)
                    vbss02
    , testEq "testBackMod22" vbss02dy $
                rdfQueryBackModify vbm22 vbss02
    , testEq "testBackMod30" vbss03by $
                rdfQueryBackModify vbm22 vbss03
    ]

------------------------------------------------------------
--  Test simple value and list queries
------------------------------------------------------------

{-
TODO: for now remove this from the test since it uses :-

namespacetest, namespacelist :: Namespace
namespacetest    =
    Namespace   "test"   "urn:test:"
namespacelist    =
    Namespace   "list"   "urn:list:"

qntest, qnlist :: String -> ScopedName
qntest loc = ScopedName namespacetest loc
qnlist loc = ScopedName namespacelist loc

prefixlist :: String
prefixlist =
    "@prefix rdf  : <" ++ nsURI namespaceRDF ++ "> . \n"  ++
    "@prefix xsd  : <" ++ nsURI namespaceXSD ++ "> . \n"  ++
    "@prefix test : <" ++ nsURI namespacetest ++ "> . \n" ++
    "@prefix list : <" ++ nsURI namespacelist ++ "> . \n" ++
    " \n"

graphlist    = graphFromBuilder graphliststr
graphliststr = prefixlist ++
    "test:a rdf:type test:C1 ; "                   ++
    "  test:p test:item1 ; "                       ++
    "  test:p test:item2 . "                       ++
    "test:b rdf:type test:C1 ; "                   ++
    "  test:p \"1\"^^xsd:integer ; "               ++
    "  test:p \"2\"^^xsd:integer ; "               ++
    "  test:p \"3\"^^xsd:integer . "               ++
    "test:c rdf:type test:C1 ; "                   ++
    "  test:q \"1\"^^xsd:integer ; "               ++
    "  test:q \"2\"^^xsd:boolean ; "               ++
    "  test:q \"3\" . "                            ++
    "list:three :- (list:_1 list:_2 list:_3) . \n" ++
    "list:empty :- () . \n"

testC1  = Res (qntest "C1")
testabc = [ Res (qntest "a"),Res (qntest "b"),Res (qntest "c") ]
testp   = Res (qntest "p")
testq   = Res (qntest "q")
testi12 = [ Res (qntest "item1"),Res (qntest "item2") ]
test123 = [ Lit "1" (Just xsd_integer)
          , Lit "2" (Just xsd_integer)
          , Lit "3" (Just xsd_integer)
          ]
test1fp = [ Lit "1" (Just xsd_integer)
          , Lit "2" (Just xsd_boolean)
          , Lit "3" Nothing
          ]

list01 = [Res (qnlist "_1"),Res (qnlist "_2"),Res (qnlist "_3")]
list02 = []

testVal01  = testEqv "testVal01" testabc $
                rdfFindValSubj res_rdfType testC1 graphlist
testVal02  = testEqv "testVal02" testi12 $
                rdfFindPredVal (testabc!!0) testp graphlist
testVal03  = testEqv "testVal03" test123 $
                rdfFindPredVal (testabc!!1) testp graphlist
testVal04  = testEqv "testVal04" test1fp $
                rdfFindPredVal (testabc!!2) testq graphlist
testVal05  = testEqv "testVal05" [] $
                rdfFindPredVal (testabc!!2) testp graphlist
testVal06  = testEqv "testVal06" [] $
                rdfFindPredInt (testabc!!0) testp graphlist
testVal07  = testEqv "testVal07" [1,2,3] $
                rdfFindPredInt (testabc!!1) testp graphlist
testVal08  = testEqv "testVal08" [1] $
                rdfFindPredInt (testabc!!2) testq graphlist

testlist01 = testEq "testlist01" list01 $
    rdfFindList graphlist (Res $ qnlist "three")
testlist02 = testEq "testlist02" list02 $
    rdfFindList graphlist (Res $ qnlist "empty")

test8 = TestList
    [ testVal01, testVal02, testVal03, testVal04
    , testVal05, testVal06, testVal07, testVal08
    , testlist01, testlist02
    ]

-}

{-----
queryList :: RDFGraph -> RDFLabel -> [RDFLabel]
-- queryList gr res_rdf_nil = []
-- queryList gr hd          = findhead g:rdfQueryList gr (findrest g)
queryList gr hd
    | hd == res_rdf_nil = []
    | otherwise         = (findhead g):(queryList gr (findrest g))
    where
        g = subgr gr hd

findhead g = headOrNil [ ob | Arc _ sb ob <- g, sb == res_rdf_first ]
findrest g = headOrNil [ ob | Arc _ sb ob <- g, sb == res_rdf_rest  ]
subgr g h  = filter ((==) h . arcSubj) $ getArcs g
headOrNil  = foldr const res_rdf_nil

th1  = (Res $ qnlist "empty")
th3  = (Res $ qnlist "three")
th3a = subgr graphlist th3
th3b = findhead th3a
th3c = findrest th3a
tl3c = queryList graphlist th3c
th3d = subgr graphlist th3c
th3e = findhead th3d
th3f = findrest th3d

tl3  = queryList graphlist th3
-----}

------------------------------------------------------------
--  Full test suite, main program,
--  and useful expressions for interactive use
------------------------------------------------------------

allTests :: [TF.Test]
allTests = 
  [ conv "1" test1
  , conv "2" test2
  , conv "3" test3
  , conv "4" test4
  , conv "6" test6
  , conv "7" test7
--  , test8
  ]

main :: IO ()
main = TF.defaultMain allTests

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011, 2012, 2013, 2014, 2018, 2021 Douglas Burke  
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
