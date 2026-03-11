{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  RDFProofTest
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2013, 2014 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  CPP, OverloadedStrings
--
--  This module tests the RDFproof module, which instantiates the proof
--  rule class over RDF graphs.
--
--------------------------------------------------------------------------------

module Main where

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as B

import qualified Test.Framework as TF

import Swish.Namespace (Namespace, makeNamespace, ScopedName, makeNSScopedName)
import Swish.Rule (Rule(..))
import Swish.VarBinding (VarBinding(..), VarBindingModify(..))
import Swish.VarBinding (makeVarFilterModify, varBindingId, varFilterNE)

import Swish.RDF.Proof
    ( makeRdfInstanceEntailmentRule
    , makeRdfSubgraphEntailmentRule
    , makeRdfSimpleEntailmentRule
    )
import Swish.RDF.Ruleset
    ( RDFRule
    , makeRDFGraphFromN3Builder
    , makeN3ClosureAllocatorRule
    , makeN3ClosureRule
    , makeN3ClosureSimpleRule
    , makeNodeAllocTo
    )

import Swish.RDF.Query (rdfQueryFind, rdfQuerySubs)
import Swish.RDF.VarBinding (RDFVarBinding, RDFVarBindingModify)

import Swish.RDF.Graph
    ( Label(..), RDFLabel(..), RDFGraph
    , addGraphs, allLabels, allNodes )

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Data.Monoid (Monoid(..))
#endif

import Data.Maybe (fromJust)

import Network.URI (URI, parseURI)

import Test.HUnit ( Test(TestList) )

import TestHelpers ( conv, test
                   , testEq, testElem
                   , testNo
                   )

--  misc helpers

testIn :: (Eq a, Show a) => String -> a -> [a] -> Test
testIn = testElem -- lab eg a = TestCase $ assertBool lab (eg `elem` a)

mkGr :: B.Builder -> [B.Builder] -> RDFGraph
mkGr pr bdy = makeRDFGraphFromN3Builder $ mconcat (pr : bdy)

mkGr1, mkGr2 :: [B.Builder] -> RDFGraph
mkGr1 = mkGr prefix1
mkGr2 = mkGr prefix2

toURI :: String -> URI
toURI = fromJust . parseURI

toNS :: Maybe T.Text -> String -> Namespace
toNS p = makeNamespace p . toURI

--  test1:  simple query with URI, literal and blank nodes.

scope1 :: Namespace
scope1 = toNS (Just "scope1")
    "http://id.ninebynine.org/wip/2003/rdfprooftest/scope1"

prefix1 :: B.Builder
prefix1 = "@prefix ex: <http://example.org/> . \n"

graph1 :: RDFGraph
graph1 = mkGr1 
         ["ex:s1  ex:p  ex:o1 . \n"
         , "ex:s2  ex:p  \"lit1\" . \n"
         , "[ ex:p ex:o3 ] . \n"
         ]

query11 :: RDFGraph
query11 = makeRDFGraphFromN3Builder query11str

query11str :: B.Builder
query11str = prefix1 `mappend` "?s  ex:p  ?o . \n"

result11 :: RDFGraph
result11 = makeRDFGraphFromN3Builder result11str

result11str :: B.Builder
result11str = prefix1 `mappend` "?s  ex:r  ?o . \n"

result11a :: RDFGraph
result11a = mkGr1 
            [ "ex:s1  ex:r  ex:o1    . \n"
            , "ex:s2  ex:r  \"lit1\" . \n" 
            , "[ ex:r ex:o3 ]        . \n"
            ]

result11b :: RDFGraph
result11b = mkGr1 ["ex:s1  ex:r  ex:o1    . \n"]

result11c :: RDFGraph
result11c = mkGr1 ["ex:s2  ex:r  \"lit1\" . \n"]

backsub11a :: RDFGraph
backsub11a = mkGr1   
             [ "ex:s1  ex:p  ex:o1    . \n"
             , "ex:s2  ex:p  \"lit1\" . \n"
             ]

backsub11b :: RDFGraph
backsub11b = mkGr1 ["ex:s2  ex:p  \"lit1\" . \n"]

rul11 :: RDFRule
rul11 = makeN3ClosureSimpleRule scope1 "rul11" query11str result11str

fwd11 :: [RDFGraph]
fwd11 = fwdApply rul11 [graph1]

bwd11 :: [[RDFGraph]]
bwd11 = bwdApply rul11 (addGraphs result11b result11c)

test1 :: Test
test1 = 
  TestList
  [ testEq "testFwd11" 1 (length fwd11)
  , testIn "testFwd11a" result11a fwd11
  , testEq "testBwd11"  1 (length (head bwd11))
  , testIn "testBwd11a" backsub11a (head bwd11)
  ]

--  test2:  a range of more complex queries based on a
--  single relationship graph.

scope2 :: Namespace
scope2 = toNS (Just "scope2")
    "http://id.ninebynine.org/wip/2003/rdfprooftest/scope2"

prefix2 :: B.Builder
prefix2 =
    "@prefix pers: <urn:pers:> . \n"
    `mappend`
    "@prefix rel:  <urn:rel:> . \n"

graph2 :: RDFGraph
graph2 = mkGr2
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
query21 = makeRDFGraphFromN3Builder query21str

query21str :: B.Builder
query21str = prefix2 `mappend` "?a rel:wife ?b . \n"

result21 :: RDFGraph
result21 = makeRDFGraphFromN3Builder result21str

result21str :: B.Builder
result21str = prefix2 `mappend` "?b rel:husband ?a . \n"

result21a :: RDFGraph
result21a = mkGr2
            [ "pers:Do1 rel:husband pers:St1 . \n"
            , "pers:Ma2 rel:husband pers:Pa2 . \n"
            , "pers:Ri2 rel:husband pers:Br2 . \n"
            , "pers:Ma3 rel:husband pers:Gr3 . \n"
            , "pers:Jo3 rel:husband pers:Si3 . \n"
            , "pers:Su3 rel:husband pers:Al3 . \n"
            ]
            
result21b :: RDFGraph
result21b = mkGr2   
            [ "pers:Do1 rel:husband pers:St1 . \n"
            , "pers:Ma2 rel:husband pers:Pa2 . \n"
            ]

bwd21a :: RDFGraph
bwd21a = mkGr2   
         [ "pers:St1 rel:wife     pers:Do1 . \n"
         , "pers:Pa2 rel:wife     pers:Ma2 . \n"
         ]
  
rul21 :: RDFRule
rul21 = makeN3ClosureSimpleRule scope2 "rul21" query21str result21str

fwd21 :: [RDFGraph]
fwd21 = fwdApply rul21 [graph2]

bwd21 :: [[RDFGraph]]
bwd21 = bwdApply rul21 result21b

query22 :: RDFGraph
query22 = makeRDFGraphFromN3Builder query22str

query22str :: B.Builder
query22str = 
  mconcat
  [ prefix2
  , "?a rel:son ?b . \n"
  , "?b rel:son ?c . \n"
  ]
  
result22 :: RDFGraph
result22 = makeRDFGraphFromN3Builder result22str

result22str :: B.Builder
result22str = prefix2 `mappend` "?a rel:grandparent ?c . \n"

result22a :: RDFGraph
result22a = mkGr2   
            [ "pers:Pa2 rel:grandparent pers:Ro4 . \n"
            , "pers:Pa2 rel:grandparent pers:Ol4 . \n"
            , "pers:Pa2 rel:grandparent pers:Lo4 . \n"
            , "pers:Pa2 rel:grandparent pers:Ha4 . \n"
            , "pers:Pa2 rel:grandparent pers:El4 . \n"
            ]
            
result22b :: RDFGraph
result22b = mkGr2
            [ "pers:Pa2 rel:grandparent pers:Ro4 . \n"
            , "pers:Pa2 rel:grandparent pers:Ol4 . \n"
            ]
            
bwd22a :: RDFGraph
bwd22a = mkGr2
         [ "pers:Pa2 rel:son      _:p1 . \n"
         , "_:p1 rel:son      pers:Ro4 . \n"
         , "pers:Pa2 rel:son      _:p2 . \n"
         , "_:p2 rel:son      pers:Ol4 . \n"
         ]
         
rul22 :: RDFRule
rul22 = makeN3ClosureSimpleRule scope2 "rul22" query22str result22str

fwd22 :: [RDFGraph]
fwd22 = fwdApply rul22 [graph2]

bwd22 :: [[RDFGraph]]
bwd22 = bwdApply rul22 result22b

query23 :: RDFGraph
query23 = makeRDFGraphFromN3Builder query23str

query23str :: B.Builder
query23str = 
  mconcat 
  [ prefix2
  , "?a rel:son ?b . \n"
  , "?a rel:son ?c . \n"
  ]
  
result23 :: RDFGraph
result23 = makeRDFGraphFromN3Builder result23str

result23str :: B.Builder
result23str = prefix2 `mappend` "?b rel:brother ?c . \n"

result23a :: RDFGraph
result23a = mkGr2
            [ "pers:Gr3 rel:brother pers:Gr3 . \n"
            , "pers:Gr3 rel:brother pers:La3 . \n"
            , "pers:Gr3 rel:brother pers:Si3 . \n"
            , "pers:Gr3 rel:brother pers:Al3 . \n"
            , "pers:La3 rel:brother pers:Gr3 . \n"
            , "pers:La3 rel:brother pers:La3 . \n"
            , "pers:La3 rel:brother pers:Si3 . \n"
            , "pers:La3 rel:brother pers:Al3 . \n"
            , "pers:Si3 rel:brother pers:Gr3 . \n"
            , "pers:Si3 rel:brother pers:La3 . \n"
            , "pers:Si3 rel:brother pers:Si3 . \n"
            , "pers:Si3 rel:brother pers:Al3 . \n"
            , "pers:Al3 rel:brother pers:Gr3 . \n"
            , "pers:Al3 rel:brother pers:La3 . \n"
            , "pers:Al3 rel:brother pers:Si3 . \n"
            , "pers:Al3 rel:brother pers:Al3 . \n"
            , "pers:Wi3 rel:brother pers:Wi3 . \n"
            , "pers:Ro4 rel:brother pers:Ro4 . \n"
            , "pers:Ol4 rel:brother pers:Lo4 . \n"
            , "pers:Ol4 rel:brother pers:Ol4 . \n"
            , "pers:Lo4 rel:brother pers:Lo4 . \n"
            , "pers:Lo4 rel:brother pers:Ol4 . \n"
            , "pers:Ha4 rel:brother pers:El4 . \n"
            , "pers:Ha4 rel:brother pers:Ha4 . \n"
            , "pers:El4 rel:brother pers:El4 . \n"
            , "pers:El4 rel:brother pers:Ha4 . \n"
            ]
            
result23b :: RDFGraph
result23b = mkGr2
            [ "pers:Gr3 rel:brother pers:Gr3 . \n"
            , "pers:Gr3 rel:brother pers:La3 . \n"
            ]

bwd23a :: RDFGraph
bwd23a = mkGr2 
         [ "_:a1 rel:son pers:Gr3 . \n"
         , "_:a1 rel:son pers:Gr3 . \n"
         , "_:a2 rel:son pers:Gr3 . \n"
         , "_:a2 rel:son pers:La3 . \n"
         ]
         
rul23 :: RDFRule
rul23 = makeN3ClosureSimpleRule scope2 "rul23" query23str result23str

fwd23 :: [RDFGraph]
fwd23 = fwdApply rul23 [graph2]

bwd23 :: [[RDFGraph]]
bwd23 = bwdApply rul23 result23b

--  Test case to return multiple alternative bindings
--
--  (?c on ?a, ?c stepSon b) => (?a stepBrother ?b, ?b stepBrother ?a)
--
--  a stepBrother b if
--      (_:c1 son a, _:c1 stepSon b) || (_:c2 stepSon a, _:c2 son b)

graph24 :: RDFGraph
graph24 = mkGr2
  [ "pers:Ma2 rel:son     pers:Gr3 . \n"
  , "pers:Ma2 rel:stepson pers:St3 . \n"
  ]
  
query24 :: RDFGraph
query24 = makeRDFGraphFromN3Builder query24str

query24str :: B.Builder
query24str = 
  mconcat
  [ prefix2
  , "?c rel:son ?a     . \n"
  , "?c rel:stepson ?b . \n"
  ]
  
result24 :: RDFGraph
result24 = makeRDFGraphFromN3Builder result24str

result24str :: B.Builder
result24str = 
  mconcat
  [ prefix2 
  , "?a rel:stepbrother ?b . \n"
  , "?b rel:stepbrother ?a . \n"
  ]
  
result24a :: RDFGraph
result24a = mkGr2
            [ "pers:Gr3 rel:stepbrother pers:St3 . \n"
            , "pers:St3 rel:stepbrother pers:Gr3 . \n"
            ]
            
bwd24a1 :: RDFGraph
bwd24a1 = mkGr2   
          ["_:c1 rel:son     pers:Gr3 . \n"
          , "_:c1 rel:stepson pers:St3 . \n"
          , "_:c2 rel:stepson pers:Gr3 . \n"
          , "_:c2 rel:son     pers:St3 . \n"
          ]
          
bwd24a2 :: RDFGraph
bwd24a2 = mkGr2
          [ "_:c1 rel:son     pers:Gr3 . \n"
          , "_:c1 rel:stepson pers:St3 . \n"
          ]
          
bwd24a3 :: RDFGraph
bwd24a3 = mkGr2   
          [ "_:c2 rel:stepson pers:Gr3 . \n"
          , "_:c2 rel:son     pers:St3 . \n"
          ]
  
bwd24a4 :: RDFGraph
bwd24a4 = mkGr2   
  [ "_:c1 rel:son     pers:Gr3 . \n" 
  , "_:c1 rel:stepson pers:St3 . \n"
  , "_:c2 rel:stepson pers:Gr3 . \n"
  , "_:c2 rel:son     pers:St3 . \n"
  ]
  
rul24 :: RDFRule
rul24 = makeN3ClosureSimpleRule scope2 "rul24" query24str result24str

fwd24 :: [RDFGraph]
fwd24 = fwdApply rul24 [graph24]

bwd24 :: [[RDFGraph]]
bwd24 = bwdApply rul24 result24a

--  bwd chain from partial conclusion
--  Also, fail because conclusion is more than the rule
--  can derive from any input.

query25 :: RDFGraph
query25 = makeRDFGraphFromN3Builder query25str

query25str :: B.Builder
query25str = 
  mconcat
  [ prefix2
  , "?a rel:son      ?b . \n"
  , "?a rel:daughter ?c . \n"
  ]

result25 :: RDFGraph
result25 = makeRDFGraphFromN3Builder result25str

result25str :: B.Builder
result25str = 
  mconcat
  [ prefix2
  , "?b rel:sister  ?c . \n"
  , "?c rel:brother ?b . \n"
  ]
  
result25a :: RDFGraph
result25a = mkGr2
            [ "pers:Wi3 rel:sister  pers:Ma3 . \n"
            , "pers:Ma3 rel:brother pers:Wi3 . \n"
            , "pers:Ro4 rel:sister  pers:Rh4 . \n"
            , "pers:Rh4 rel:brother pers:Ro4 . \n"
            ]

{-
result25b    = makeRDFGraphFromN3Builder result25bstr
result25bstr = prefix2 ++
    "pers:Ro4 rel:sister  pers:Rh4 . \n" ++
    "pers:Rh4 rel:brother pers:Ro4 . \n"
-}

result25c :: RDFGraph
result25c = mkGr2
            [ "pers:Wi3 rel:sister  pers:Ma3 . \n"
            , "pers:Ma3 rel:brother pers:Wi3 . \n"
            , "pers:Ro4 rel:sister  pers:Rh4 . \n"
            , "pers:Rh4 rel:brother pers:Ro4 . \n"
            , "pers:xx3 rel:mother  pers:yy3 . \n"
            , "pers:yy3 rel:brother pers:xx3 . \n"
            ]
            
result25d :: RDFGraph
result25d = mkGr2   
            [ "pers:Wi3 rel:sister  pers:Ma3 . \n"
            , "pers:Ma3 rel:brother pers:Wi3 . \n"
            , "pers:Ro4 rel:sister  pers:Rh4 . \n"
            , "pers:Rh4 rel:brother pers:Ro4 . \n"
            , "pers:xx3 rel:father  pers:yy3 . \n"
            ]
            
conc25 :: RDFGraph
conc25 = mkGr2
         [ "pers:Wi3 rel:sister  pers:Ma3 . \n"
         , "pers:Rh4 rel:brother pers:Ro4 . \n"
         ]
         
bwd25a :: RDFGraph
bwd25a = mkGr2
         [ "_:a1 rel:son      pers:Wi3 . \n"
         , "_:a1 rel:daughter pers:Ma3 . \n"
         , "_:a2 rel:son      pers:Ro4 . \n"
         , "_:a2 rel:daughter pers:Rh4 . \n"
         ]
         
rul25 :: RDFRule
rul25 = makeN3ClosureSimpleRule scope2 "rul25" query25str result25str

fwd25 :: [RDFGraph]
fwd25 = fwdApply rul25 [graph2]

bwd25, bwd25c, bwd25d :: [[RDFGraph]]
bwd25 = bwdApply rul25 conc25
bwd25c = bwdApply rul25 result25c
bwd25d = bwdApply rul25 result25d

test2 :: Test
test2 = 
  TestList
  [ testEq "testResult21" 1 (length fwd21)
  , testIn "testResult21a" result21a fwd21
  , testEq "testBwd21"  1 (length $ head bwd21)
  , testIn "testBwd21a" bwd21a (head bwd21)
  , testEq "testResult22" 1 (length fwd22)
  , testIn "testResult22a" result22a fwd22
  , testEq "testBwd22"  1 (length $ head bwd22)
  , testIn "testBwd22a" bwd22a (head bwd22)
  , testEq "testResult23" 1 (length fwd23)
  , testIn "testResult23a" result23a fwd23
  , testEq "testBwd23"  1 (length $ head bwd23)
  , testIn "testBwd23a" bwd23a (head bwd23)
  , testEq "testResult24" 1 (length fwd24)
  , testIn "testResult24a" result24a fwd24
  , testEq "testBwd24"  4 (length bwd24)
  , testIn "testBwd24a1" bwd24a1 (head bwd24)
  , testIn "testBwd24a2" bwd24a2 (bwd24!!1)
  , testIn "testBwd24a3" bwd24a3 (bwd24!!2)
  , testIn "testBwd24a4" bwd24a4 (bwd24!!3)
  , testEq "testResult25" 1 (length fwd25)
  , testIn "testResult25a" result25a fwd25
  , testEq "testBwd25"  1 (length $ head bwd25)
  , testIn "testBwd25a" bwd25a (head bwd25)
    -- testBwd25a1 = testEq "testBwd25a" bwd25a (head $ head bwd25)
  , testNo "testBwd25c" bwd25c
  , testNo "testBwd25d" bwd25d
  ]

--  test3:  check variable binding filters

scope3 :: Namespace
scope3 = toNS (Just "scope3")
    "http://id.ninebynine.org/wip/2003/rdfprooftest/scope3"

query31 :: RDFGraph
query31 = makeRDFGraphFromN3Builder query31str

query31str :: B.Builder
query31str = 
  mconcat
  [ prefix2
  , "?a rel:son ?b . \n"
  , "?a rel:son ?c . \n"
  ]
  
modify31 :: RDFVarBindingModify
modify31 = makeVarFilterModify $ varFilterNE (Var "b") (Var "c")

result31 :: RDFGraph
result31 = makeRDFGraphFromN3Builder result31str

result31str :: B.Builder
result31str = prefix2 `mappend` "?b rel:brother ?c . \n"

result31a :: RDFGraph
result31a = mkGr2
            [ "pers:Gr3 rel:brother pers:La3 . \n"
            , "pers:Gr3 rel:brother pers:Si3 . \n"
            , "pers:Gr3 rel:brother pers:Al3 . \n"
            , "pers:La3 rel:brother pers:Gr3 . \n"
            , "pers:La3 rel:brother pers:Si3 . \n"
            , "pers:La3 rel:brother pers:Al3 . \n"
            , "pers:Si3 rel:brother pers:Gr3 . \n"
            , "pers:Si3 rel:brother pers:La3 . \n"
            , "pers:Si3 rel:brother pers:Al3 . \n"
            , "pers:Al3 rel:brother pers:Gr3 . \n"
            , "pers:Al3 rel:brother pers:La3 . \n"
            , "pers:Al3 rel:brother pers:Si3 . \n"
            , "pers:Ol4 rel:brother pers:Lo4 . \n"
            , "pers:Lo4 rel:brother pers:Ol4 . \n"
            , "pers:Ha4 rel:brother pers:El4 . \n"
            , "pers:El4 rel:brother pers:Ha4 . \n"
            ]
            
result31b :: RDFGraph
result31b = mkGr2 ["pers:Gr3 rel:brother pers:Gr3 . \n"]

result31c :: RDFGraph
result31c = mkGr2 ["pers:Gr3 rel:brother pers:La3 . \n"]

bwd31c :: RDFGraph
bwd31c = mkGr2 
         [ "_:a rel:son pers:Gr3 . \n"
         , "_:a rel:son pers:La3 . \n"
         ]
         
rul31 :: RDFRule
rul31 = makeN3ClosureRule scope3 "rul31" query31str result31str modify31

fwd31 :: [RDFGraph]
fwd31 = fwdApply rul31 [graph2]

calcbwd31b, calcbwd31c :: [[RDFGraph]]
calcbwd31b = bwdApply rul31 result31b
calcbwd31c = bwdApply rul31 result31c

test3 :: Test
test3 = 
  TestList
  [ testEq "testResult31" 1 (length fwd31)
  , testIn "testResult31a" result31a fwd31
  , testEq "testBwd31"  0 (length calcbwd31b)
  , testEq "testBwd31"  1 (length $ head calcbwd31c)
  , testIn "testBwd31c" bwd31c (head calcbwd31c)
  ]

--  Instance entailment tests

scope4 :: Namespace
scope4 = toNS (Just "scope4")
    "http://id.ninebynine.org/wip/2003/rdfprooftest/scope4"

graph4 :: RDFGraph
graph4 = mkGr2
         [ "pers:Gr3 rel:son      pers:Ro4 ; \n"
         , "         rel:daughter pers:Rh4 . \n"
         ]
         
vocab4 :: S.Set RDFLabel
vocab4 = allNodes (not . labelIsVar) graph4

name4 :: ScopedName
name4 = makeNSScopedName scope4 "instance4"

rule4 :: RDFRule
rule4 = makeRdfInstanceEntailmentRule name4 (S.toList vocab4)

fwd42a :: RDFGraph
fwd42a = mkGr2    
         [ "pers:Gr3 rel:son      _:Ro4 ;    \n"
         , "         rel:daughter pers:Rh4 . \n"
         ]
         
fwd42b :: RDFGraph
fwd42b = mkGr2 
         [ "pers:Gr3 rel:son      pers:Ro4 ; \n"
         , "         rel:daughter _:Rh4 .    \n"
         ]
           
fwd42c :: RDFGraph
fwd42c = mkGr2
         [ "pers:Gr3 rel:son      _:Ro4 ;    \n"
         , "         rel:daughter _:Rh4 .    \n"
         ]
         
fwd42d :: RDFGraph
fwd42d = mkGr2
         [ "_:Gr3    rel:son      _:Ro4 ;    \n"
         , "         rel:daughter pers:Rh4 . \n"
         ]

fwd42e :: RDFGraph
fwd42e = mkGr2 
         [ "_:Gr3    rel:son      _:Ro4 ;    \n"
         , "         rel:daughter pers:Rh4 . \n"
         ]

fwd42f :: RDFGraph
fwd42f = mkGr2 
         [ "_:Gr3    rel:son      pers:Ro4 ; \n"
         , "         rel:daughter _:Rh4 .    \n"
         ]
         
fwd42g :: RDFGraph
fwd42g = mkGr2
         [ "_:Gr3    rel:son      _:Ro4 ;    \n"
         , "         rel:daughter _:Rh4 .    \n"
         ]

--  Non-entailments

fwd42w :: RDFGraph
fwd42w = mkGr2 ["pers:Gr3 rel:daughter pers:Ro4 . \n"]

fwd42x :: RDFGraph
fwd42x = mkGr2 ["pers:Gr3 rel:daughter pers:Ro4 . \n"]

fwd42y :: RDFGraph
fwd42y = mkGr2 
         [ "_:Gr3    rel:son      pers:Ro4 ; \n"
         , "         rel:daughter pers:Ro4 . \n"
         ]
         
fwd42z :: RDFGraph
fwd42z = mkGr2 
         [ "_:Gr3    rel:son      _:Ro4 ; \n"
         , "         rel:son      _:Rh4 . \n"
         ]
         
bwd43 :: RDFGraph
bwd43 = mkGr2
        [ "_:a1 rel:son      pers:Ro4 . \n"
        , "_:a2 rel:daughter pers:Rh4 . \n"
        ]

bwd43a :: RDFGraph
bwd43a = mkGr2 
         [ "pers:Gr3 rel:son      pers:Ro4 . \n"
         , "pers:Gr3 rel:daughter pers:Rh4 . \n"
         ]

bwd43b :: RDFGraph
bwd43b = mkGr2
         [ "pers:Gr3 rel:son      pers:Ro4 . \n"
         , "pers:Ro4 rel:daughter pers:Rh4 . \n"
         ]
         
bwd43c :: RDFGraph
bwd43c = mkGr2
         [ "pers:Gr3 rel:son      pers:Ro4 . \n"
         , "pers:Rh4 rel:daughter pers:Rh4 . \n"
         ]
         
bwd43d :: RDFGraph
bwd43d = mkGr2
         [ "pers:Ro4 rel:son      pers:Ro4 . \n"
         , "pers:Gr3 rel:daughter pers:Rh4 . \n"
         ]
         
bwd43e :: RDFGraph
bwd43e = mkGr2 
         [ "pers:Ro4 rel:son      pers:Ro4 . \n"
         , "pers:Ro4 rel:daughter pers:Rh4 . \n"
         ]
         
bwd43f :: RDFGraph
bwd43f = mkGr2
         [ "pers:Ro4 rel:son      pers:Ro4 . \n"
         , "pers:Rh4 rel:daughter pers:Rh4 . \n"
         ]
         
bwd43g :: RDFGraph
bwd43g = mkGr2
         [ "pers:Rh4 rel:son      pers:Ro4 . \n"
         , "pers:Gr3 rel:daughter pers:Rh4 . \n"
         ]
         
bwd43h :: RDFGraph
bwd43h = mkGr2
         [ "pers:Rh4 rel:son      pers:Ro4 . \n"
         , "pers:Ro4 rel:daughter pers:Rh4 . \n"
         ]
         
bwd43i :: RDFGraph
bwd43i = mkGr2
         [ "pers:Rh4 rel:son      pers:Ro4 . \n"
         , "pers:Rh4 rel:daughter pers:Rh4 . \n"
         ]
         
--  Forward chaining

fwdApply42 :: [RDFGraph]
fwdApply42 = fwdApply rule4 [graph4]

--  Backward chaining

bwdApply43 :: [[RDFGraph]]
bwdApply43 = bwdApply rule4 bwd43

test4 :: Test
test4 = 
  TestList
  [ 
    --  Check basics
    testEq "testRuleName41" name4 (ruleName rule4)
  , testEq "testVocab41"    3     (S.size vocab4)
  , testEq "testFwdLength42" 7 (length fwdApply42)
  , testIn "testFwdApply42a"  fwd42a fwdApply42
  , testIn "testFwdApply42b"  fwd42b fwdApply42
  , testIn "testFwdApply42c"  fwd42c fwdApply42
  , testIn "testFwdApply42d"  fwd42d fwdApply42
  , testIn "testFwdApply42e"  fwd42e fwdApply42
  , testIn "testFwdApply42f"  fwd42f fwdApply42
  , testIn "testFwdApply42g"  fwd42g fwdApply42
  , testEq "testBwdLength43" 9 (length bwdApply43)
  , testIn "testBwdApply43a"  [bwd43a] bwdApply43
  , testIn "testBwdApply43b"  [bwd43b] bwdApply43
  , testIn "testBwdApply43c"  [bwd43c] bwdApply43
  , testIn "testBwdApply43d"  [bwd43d] bwdApply43
  , testIn "testBwdApply43e"  [bwd43e] bwdApply43
  , testIn "testBwdApply43f"  [bwd43f] bwdApply43
  , testIn "testBwdApply43g"  [bwd43g] bwdApply43
  , testIn "testBwdApply43h"  [bwd43h] bwdApply43
  , testIn "testBwdApply43i"  [bwd43i] bwdApply43
      
    --  Entailment checks
  , testEq "testEntail44a" True  (checkInference rule4 [graph4] fwd42a)
  , testEq "testEntail44b" True  (checkInference rule4 [graph4] fwd42b)
  , testEq "testEntail44g" True  (checkInference rule4 [graph4] fwd42g)
  , testEq "testEntail44w" False (checkInference rule4 [graph4] fwd42w)
  , testEq "testEntail44x" False (checkInference rule4 [graph4] fwd42x)
  , testEq "testEntail44y" False (checkInference rule4 [graph4] fwd42y)
  , testEq "testEntail44z" False (checkInference rule4 [graph4] fwd42z)
  ]

--  Subgraph entailment tests

scope5 :: Namespace
scope5 = toNS (Just "scope5")
    "http://id.ninebynine.org/wip/2003/rdfprooftest/scope5"

graph5 :: RDFGraph
graph5 = mkGr2
         [ "pers:Gr3 rel:son      pers:Ro4 ; \n"
         , "         rel:daughter pers:Rh4 . \n"
         , "pers:Si3 rel:son      pers:Ol4 . \n"
         ]

name5 :: ScopedName
name5 = makeNSScopedName scope5 "subgraph5"

rule5 :: RDFRule
rule5 = makeRdfSubgraphEntailmentRule name5

--  Forward chaining excludes null agraph and copy of antecedent

fwd52a :: RDFGraph
fwd52a = mkGr2 ["pers:Gr3 rel:son      pers:Ro4 . \n"]

fwd52b :: RDFGraph
fwd52b = mkGr2 ["pers:Gr3 rel:daughter pers:Rh4 . \n"]

fwd52c :: RDFGraph
fwd52c = mkGr2 ["pers:Si3 rel:son      pers:Ol4 . \n"]

fwd52d :: RDFGraph
fwd52d = mkGr2
         [ "pers:Gr3 rel:son      pers:Ro4 . \n"
         , "pers:Gr3 rel:daughter pers:Rh4 . \n"
         ]

fwd52e :: RDFGraph
fwd52e = mkGr2
         [ "pers:Gr3 rel:son      pers:Ro4 . \n"
         , "pers:Si3 rel:son      pers:Ol4 . \n"
         ]
         
fwd52f :: RDFGraph
fwd52f = mkGr2
         [ "pers:Gr3 rel:daughter pers:Rh4 . \n"
         , "pers:Si3 rel:son      pers:Ol4 . \n"
         ]

--  Forward chaining

fwdApply52 :: [RDFGraph]
fwdApply52 = fwdApply rule5 [graph5]

test5 :: Test
test5 = 
  TestList
  [ testEq "testRuleName51" name5 (ruleName rule5)
  , testEq "testFwdLength52" 6 (length fwdApply52)
  , testIn "testFwdApply52a"  fwd52a fwdApply52
  , testIn "testFwdApply52b"  fwd52b fwdApply52
  , testIn "testFwdApply52c"  fwd52c fwdApply52
  , testIn "testFwdApply52d"  fwd52d fwdApply52
  , testIn "testFwdApply52e"  fwd52e fwdApply52
  , testIn "testFwdApply52f"  fwd52f fwdApply52
  ]

--  Simple entailment test
--  Simple entailment provides entailment check only, no forward or
--  backward chaining.  For that use instance- and subgraph- rules.

scope6 :: Namespace
scope6 = toNS (Just "scope6")
    "http://id.ninebynine.org/wip/2003/rdfprooftest/scope6"

graph6 :: RDFGraph
graph6 = mkGr2
         [ "pers:Gr3 rel:son      pers:Ro4 ; \n"
         , "         rel:daughter pers:Rh4 . \n"
         , "pers:Si3 rel:son      pers:Ol4 ; \n"
         , "         rel:son      pers:Lo4 . \n"
         ]

name6 :: ScopedName
name6 = makeNSScopedName scope5 "subgraph6"

rule6 :: RDFRule
rule6 = makeRdfSimpleEntailmentRule name6

simple6a :: RDFGraph
simple6a = mkGr2 
           [ "_:Gr3 rel:son      pers:Ro4 ; \n"
           , "      rel:daughter pers:Rh4 . \n"
           ]

simple6b :: RDFGraph
simple6b = mkGr2
           [ "_:Si3 rel:son      pers:Ol4 ; \n"
           , "      rel:son      pers:Lo4 . \n"
           ]
  
simple6c :: RDFGraph
simple6c = mkGr2
           [ "_:Si3 rel:son      _:Ol4 ; \n"
           , "      rel:son      _:Lo4 . \n"
           ]
  
simple6d :: RDFGraph
simple6d = mkGr2
           [ "_:Si3 rel:son      _:Ol4 ; \n"
           , "      rel:daughter _:Lo4 . \n"
           ]
           
simple6e :: RDFGraph
simple6e = mkGr2
           [ "_:Si3 rel:daughter _:Ol4 ; \n"
           , "      rel:mother   _:Lo4 . \n"
           ]

test6 :: Test
test6 = 
  TestList
  [ testEq "testRuleName61" name6 (ruleName rule6)
  , test "testSimple62" (checkInference rule6 [graph6] simple6a)
  , test "testSimple63" (checkInference rule6 [graph6] simple6b)
  , test "testSimple64" (checkInference rule6 [graph6] simple6c)
  , test "testSimple65" (checkInference rule6 [graph6] simple6d)
  , test "testSimple66" (not $ checkInference rule6 [graph6] simple6e)
  , test "testFwd64"    (null $ fwdApply rule6 [graph6])
  , test "testBwd65"    (null $ bwdApply rule6 graph6)
  ]

--  Test forward chaining node allocation logic
--
--  ?a uncle ?c => ?a father ?b, ?b brother ?c,   ?b allocTo ?a
--
--    Ro4 uncle La3, Ro4 uncle Si3, Rh4 uncle La3, Rh4 uncle Si3
--  =>
--    Ro4 father _:f1, _:f1 brother La3,
--    Ro4 father _:f1, _:f1 brother Si3,
--    Rh4 father _:f2, _:f2 brother La3,
--    Rh4 father _:f2, _:f2 brother Si3

scope7 :: Namespace
scope7 = toNS (Just "scope7")
    "http://id.ninebynine.org/wip/2003/rdfprooftest/scope7"

graph7 :: RDFGraph
graph7 = mkGr2
         [ "pers:Ro4 rel:uncle pers:La3 ; \n"
         , "         rel:uncle pers:Si3 . \n"
         , "pers:Rh4 rel:uncle pers:La3 ; \n"
         , "         rel:uncle pers:Si3 . \n"
         ]

query71str, result71str :: B.Builder
query71str = prefix2 `mappend` "?a rel:uncle ?c . \n"
result71str =
  mconcat 
  [ prefix2
  , "?a rel:father  ?b . \n"
  , "?b rel:brother ?c . \n"
  ]
           
query71, result71 :: RDFGraph
query71 = makeRDFGraphFromN3Builder query71str
result71 = makeRDFGraphFromN3Builder result71str

result71a :: RDFGraph
result71a = mkGr2 
            [ "pers:Ro4 rel:father  _:f1     . \n"
            , "_:f1     rel:brother pers:La3 . \n"
            , "pers:Ro4 rel:father  _:f1     . \n"
            , "_:f1     rel:brother pers:Si3 . \n"
            , "pers:Rh4 rel:father  _:f2     . \n"
            , "_:f2     rel:brother pers:La3 . \n"
            , "pers:Rh4 rel:father  _:f2     . \n"
            , "_:f2     rel:brother pers:Si3 . \n"
            ]
  
rul71 :: RDFRule
rul71 = makeN3ClosureAllocatorRule scope7 "rul71"
    query71str result71str varBindingId mod71

mod71 :: [RDFLabel] -> RDFVarBindingModify
mod71 = makeNodeAllocTo (Var "b") (Var "a")

var71 :: [RDFVarBinding]
var71 = rdfQueryFind query71 graph7

var71a :: [VarBinding RDFLabel RDFLabel]
var71a = vbmApply (mod71 (S.toList (allLabels labelIsVar graph7))) var71

var71_1 :: VarBinding RDFLabel RDFLabel
var71_1 = head var71a

map71a, map71b, map71c :: Maybe RDFLabel
map71a = Just (Var "#a")
map71b = Just (Var "#b")
map71c = Just (Var "#c")

sub71a :: [RDFGraph]
sub71a = rdfQuerySubs var71a result71

fwd71 :: [RDFGraph]
fwd71 = fwdApply rul71 [graph7]

test7 :: Test
test7 = 
  TestList
  [ testEq "testVar71" 4 (length var71)
  , testEq "testVar71a" 4 (length var71a)
-- testVar71_1a = testEq "testVar71_1a" map71a ( vbMap var71_1 (Var "a"))
-- testVar71_1b = testEq "testVar71_1b" map71b ( vbMap var71_1 (Var "b"))
-- testVar71_1c = testEq "testVar71_1c" map71c ( vbMap var71_1 (Var "c"))
  , testEq "testVar71a" 4 (length sub71a)
  , testEq "testResult71" 1 (length fwd71)
  , testIn "testResult71a" result71a fwd71
  ]    

--  Full test suite, main program, and useful expressions for interactive use

allTests :: [TF.Test]
allTests =
  [ conv "1" test1
  , conv "2" test2
  , conv "3" test3
  , conv "4" test4
  , conv "5" test5
  , conv "6" test6
  , conv "7" test7
  ]

main :: IO ()
main = TF.defaultMain allTests

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--     2011, 2013 Douglas Burke  
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
