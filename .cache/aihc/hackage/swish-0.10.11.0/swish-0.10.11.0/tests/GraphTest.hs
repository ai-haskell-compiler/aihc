--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  GraphTest
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012, 2013, 2014 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  H98
--
--  This module defines test cases for module Graph.
--
--------------------------------------------------------------------------------

module Main where

import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Test.Framework as TF
import qualified Test.Framework.Providers.HUnit as TF

import Swish.GraphClass (Arc(..), ArcSet, LDGraph(..), Label(..))
import Swish.GraphClass (arc, arcFromTriple, arcToTriple)
import Swish.GraphMem
import Swish.GraphMatch
      ( LabelMap, GenLabelMap(..), 
        EquivalenceClass,
        ScopedLabel(..), makeScopedLabel, makeScopedArc,
        LabelIndex, nullLabelVal, emptyMap,
        mapLabelIndex, {-mapLabelList,-} setLabelHash, newLabelMap,
        graphLabels, assignLabelMap, newGenerationMap
        -- graphMatch1  only used with pairSort
        , equivalenceClasses
      )

-- import Swish.Utils.ListHelpers (subset)

import Data.Function (on)
import Data.Hashable (hashWithSalt)
import Data.List (sort, sortBy, elemIndex)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Word (Word32)

import Test.HUnit
      ( Test(TestCase,TestList,TestLabel)
        , (@=?)
        , assertEqual, assertBool )

import TestHelpers (conv, testEq, testNe)

default ( Int )

------------------------------------------------------------
-- Define some common values
------------------------------------------------------------

type Statement = Arc LabelMem

base1, base2, base3, base4 :: String

base1 = "http://id.ninebynine.org/wip/2003/test/graph1/node#"
base2 = "http://id.ninebynine.org/wip/2003/test/graph2/node/"
base3 = "http://id.ninebynine.org/wip/2003/test/graph3/node"
base4 = "http://id.ninebynine.org/wip/2003/test/graph3/nodebase"

------------------------------------------------------------
--  Set, get graph arcs as sets of triples
------------------------------------------------------------

setArcsT :: (Ord lb, LDGraph lg lb) =>
            S.Set (lb, lb, lb) -> lg lb -> lg lb
setArcsT a g = setArcs g $ S.map arcFromTriple a

getArcsT :: (Ord lb, LDGraph lg lb) =>
            lg lb -> S.Set (lb, lb, lb)
getArcsT g = S.map arcToTriple $ getArcs g

------------------------------------------------------------
--  Label map and entry creation helpers
------------------------------------------------------------

tstLabelMap :: (Label lb) => Word32 -> [(lb,LabelIndex)] -> LabelMap lb
tstLabelMap gen = LabelMap gen . M.fromList

------------------------------------------------------------
--  Graph helper function tests
------------------------------------------------------------

{-

-- select; no longer exported so need to check that
-- other tests still test this routine

testSelect :: String -> String -> String -> Test
testSelect lab = testEq ("Select"++lab )

isOne :: Int -> Bool
isOne = (1 ==)

testSelect01, testSelect02, testSelect03, testSelect04 :: Test
testSelect01 = testSelect "01"
                (select isOne [0,1,2,0,1,2] "abcabc") "bb"
testSelect02 = testSelect "02"
                (select isOne [1,1,1,1,1,1] "abcabc") "abcabc"
testSelect03 = testSelect "03"
                (select isOne [0,0,0,0,0,0] "abcabc")
                []
testSelect04 = testSelect "04"
                (select isOne []            []                       )
                []

testSelectSuite :: Test
testSelectSuite = TestList
    [
    testSelect01, testSelect02, testSelect03, testSelect04
    ]

-}

-- subset: hopefully can remove soon

{-

testSubset :: String -> Bool -> [Int] -> [Int] -> Test
testSubset lab res l1s l2s = testEq ("Mapset"++lab ) res (l1s `subset` l2s)

testSubsetSuite :: Test
testSubsetSuite = TestList
    [ testSubset "01" True  [1,2,3]       [0,1,2,3,4,5]
    , testSubset "02" True  [5,3,1]       [0,1,2,3,4,5]
    , testSubset "03" True  [5,4,3,2,1,0] [0,1,2,3,4,5]
    , testSubset "04" True  []            []
    , testSubset "05" False [0,1,2,3,4,5] [1,2,3]
    , testSubset "06" False [0,1,2,3,4,5] [5,3,1]
    , testSubset "07" True  []            [1,2,3]
    , testSubset "08" False [1,2,3]       []
    ]

-}

------------------------------------------------------------
--  Simple graph label tests
------------------------------------------------------------

testLabSuite :: Test
testLabSuite = TestList
    [ testEq "Lab01" False (labelIsVar lab1f)
    , testEq "Lab02" True  (labelIsVar lab1v)
    , testEq "Lab03" False (labelIsVar lab2f)
    , testEq "Lab04" True  (labelIsVar lab2v)

    {- This just tests the hash routine, which doesn't
       really tell us that much, so replace by checks that the
       hashes aren't the same
    , testEq "Lab05" 39495998 (labelHash 1 lab1f)
    , testEq "Lab06" 45349309 (labelHash 1 lab1v)
    , testEq "Lab07" 39495997 (labelHash 1 lab2f)
    , testEq "Lab08" 45349310 (labelHash 1 lab2v)
    -}

    , testNe "Hash Lab05/6" (labelHash 1 lab1f) (labelHash 1 lab1v)
    , testNe "Hash Lab05/7" (labelHash 1 lab1f) (labelHash 1 lab2f)
    , testNe "Hash Lab06/8" (labelHash 1 lab1v) (labelHash 1 lab2v)
    , testNe "Hash Lab07/8" (labelHash 1 lab2f) (labelHash 1 lab2v)
    , testNe "Hash Lab05/8" (labelHash 1 lab1f) (labelHash 1 lab2v)
    , testNe "Hash Lab06/7" (labelHash 1 lab1v) (labelHash 1 lab2f)

    , testEq "Lab09" "!lab1" (show lab1f)
    , testEq "Lab10" "?lab1" (show lab1v)
    , testEq "Lab11" "!lab2" (show lab2f)
    , testEq "Lab12" "?lab2" (show lab2v)

    , testEq "Lab13" "lab1" (getLocal lab1v)
    , testEq "Lab14" "lab2" (getLocal lab2v)
    , testEq "Lab15" lab1v  (makeLabel "lab1")
    , testEq "Lab16" lab2v  (makeLabel "lab2")
    ]

------------------------------------------------------------
--  Simple graph tests
------------------------------------------------------------

lab1f, lab1v, lab2f, lab2v :: LabelMem
lab1f = LF "lab1"
lab1v = LV "lab1"
lab2f = LF "lab2"
lab2v = LV "lab2"

ga1 :: S.Set (LabelMem, LabelMem, LabelMem)
ga1 = S.fromList
    [
    (lab1f,lab1f,lab1f),
    (lab1v,lab1v,lab1v),
    (lab2f,lab2f,lab2f),
    (lab2v,lab2v,lab2v),
    (lab1f,lab1f,lab1v),
    (lab1f,lab1f,lab2f),
    (lab1f,lab1f,lab2v),
    (lab1v,lab1v,lab1f),
    (lab1v,lab1v,lab2f),
    (lab1v,lab1v,lab2v),
    (lab1f,lab1v,lab2f),
    (lab1f,lab1v,lab2v),
    (lab1v,lab2f,lab2v)
    ]

gs4 :: Statement -> Bool
gs4 (Arc _ _ (LV "lab2")) = True
gs4 _                     = False

ga4 :: S.Set (LabelMem, LabelMem, LabelMem)
ga4 = S.fromList
    [
    (lab2v,lab2v,lab2v),
    (lab1f,lab1f,lab2v),
    (lab1v,lab1v,lab2v),
    (lab1f,lab1v,lab2v),
    (lab1v,lab2f,lab2v)
    ]

ga2 :: S.Set (LabelMem, LabelMem, LabelMem)
ga2 = S.fromList
    [
    (lab1f,lab1f,lab1f),
    (lab1v,lab1v,lab1v),
    (lab2f,lab2f,lab2f),
    (lab2v,lab2v,lab2v)
    ]

ga3 :: S.Set (LabelMem, LabelMem, LabelMem)
ga3 = S.fromList
    [
    (lab1f,lab1f,lab1v),
    (lab1f,lab1f,lab2f),
    (lab1f,lab1f,lab2v),
    (lab1v,lab1v,lab1f),
    (lab1v,lab1v,lab2f),
    (lab1v,lab1v,lab2v),
    (lab1f,lab1v,lab2f),
    (lab1f,lab1v,lab2v),
    (lab1v,lab2f,lab2v)
    ]

gl4 :: S.Set LabelMem
gl4 = S.fromList [lab1f,lab1v,lab2f,lab2v]

gr1a, gr2a, gr3a, gr4a, gr4b, gr4c, gr4d, gr4e,
  gr4g :: GraphMem LabelMem
gr1a = setArcsT ga1 (GraphMem S.empty)
gr2a = setArcsT ga2 emptyGraph
gr3a = setArcsT ga3 emptyGraph
gr4a = addGraphs gr2a gr3a
gr4b = addGraphs gr3a gr2a
gr4c = delete gr2a gr4a
gr4d = delete gr3a gr4a
gr4e = extract gs4 gr4a
gr4g = addGraphs gr2a gr4a

gl4f :: S.Set LabelMem
gl4f = labels gr4a

{-
gr4ee :: [Bool]
gr4ee = map gs4 $ S.toList (getArcs gr4a)
-}

testGraphSuite :: Test
testGraphSuite = TestList
    [ testEq "Graph01" ga1 (getArcsT gr1a)
    , testEq "Graph01" ga2 (getArcsT gr2a)
    , testEq "Graph03" ga3 (getArcsT gr3a)
    , testEq "Graph04" ga1 (getArcsT gr4a)
    , testEq "Graph05" ga1 (getArcsT gr4b)
    , testEq "Graph06" ga3 (getArcsT gr4c)
    , testEq "Graph07" ga2 (getArcsT gr4d)
    , testEq "Graph08" ga4 (getArcsT gr4e)
    , testEq "Graph09" gl4 gl4f
    , testEq "Graph10" ga1 (getArcsT gr4g)
    ]

------------------------------------------------------------
--
------------------------------------------------------------

------------------------------------------------------------
-- Define some common values
------------------------------------------------------------

s1, s2, s3, s4, s5, s6, s7, s8 :: LabelMem
s1 = LF "s1"
s2 = LF "s2"
s3 = LF "s3"
s4 = LF ""
s5 = LV "s5"
s6 = LF "basemore"
s7 = LF ("base"++"more")
s8 = LV "s8"

b1, b2, b3, b4 :: LabelMem
b1 = LV "b1"
b2 = LV "b2"
b3 = LV "b3"
b4 = LV "b4"

p1, p2, p3, p4 :: LabelMem
p1 = LF "p1"
p2 = LF "p2"
p3 = LF "p3"
p4 = LF "p4"

o1, o2, o3, o4, o5, o6 :: LabelMem
o1 = LF "o1"
o2 = LF "o2"
o3 = LF "o3"
o4 = LF ""
o5 = LV "o5"
o6 = LV "s5"

l1, l2, l3, l4, l5, l6, l7, l8, l9,
  l10, l11, l12 :: LabelMem
l1  = LF "l1"
l2  = LF "l2-en"
l3  = LF "l2-fr"
l4  = LF "l4-type1"
l5  = LF "l4-type1"
l6  = LF "l4-type1"
l7  = LF "l4-type2"
l8  = LF "l4-type2"
l9  = LF "l4-type2"
l10 = LF "l10-xml"
l11 = LF "l10-xml-en"
l12 = LF "l10-xml-fr"

v1, v2, v3, v4 :: LabelMem
v1  = LV "v1"
v2  = LV "v2"
v3  = LV "v3"
v4  = LV "v4"

------------------------------------------------------------
--  Label construction and equality tests
------------------------------------------------------------

testLabelEq :: String -> Bool -> LabelMem -> LabelMem -> Test
testLabelEq lab eq n1 n2 =
    TestCase ( assertEqual ("testLabelEq:"++lab) eq (n1==n2) )

nodelist :: [(String, LabelMem)]
nodelist =
  [ ("s1",s1), ("s2",s2), ("s3",s3), ("s4",s4), ("s5",s5),
    ("s6",s6), ("s7",s7), ("s8",s8),
    ("o5",o5),
    ("p1",p1), ("p2",p2), ("p3",p3), ("p4",p4),
    ("o1",o1), ("o2",o2), ("o3",o3), ("o4",o4),
    ("l1",l1), ("l2",l2), ("l3",l3), ("l4",l4), ("l5",l5),
    ("l6",l6), ("l7",l7), ("l8",l8), ("l9",l9),
    ("l10",l10), ("l11",l11), ("l12",l12),
    ("v1",v1), ("v2",v2)
  ]

nodeeqlist :: [(String, String)]
nodeeqlist =
  [
    ("s4","o4"),
    ("s5","o6"),
    ("s6","s7"),
    ("l4","l5"),
    ("l4","l6"),
    ("l5","l6"),
    ("l7","l8"),
    ("l7","l9"),
    ("l8","l9")
  ]

testLabelEqSuite :: Test
testLabelEqSuite = TestList
  [ testLabelEq (testLab a b) (nodeTest a b) n1 n2
      | (a,n1) <- nodelist , (b,n2) <- nodelist ]
    where
    testLab a b = a ++ "-" ++ b
    nodeTest  a b = (a == b)        ||
            (a,b) `elem` nodeeqlist ||
            (b,a) `elem` nodeeqlist


------------------------------------------------------------
--  Label ordering tests
------------------------------------------------------------

testLabelOrd :: String -> Ordering -> LabelMem -> LabelMem -> Test
testLabelOrd lab order n1 n2 =
    TestCase ( assertEqual ("testLabelOrd:"++lab) order (compare n1 n2) )

nodeorder :: [String]
nodeorder =
  [
    "o4",
    "s4", "s6", "s7",
    "l1", "l10", "l11", "l12", "l2", "l3", "l4", "l5", "l6", "l7", "l8", "l9",
    "o1", "o2", "o3",
    "p1", "p2", "p3", "p4",
    "s1", "s2", "s3",
    "b1", "b2", "b3", "b4",
    "o5",
    "s5", "s8",
    "v1", "v2"
  ]

testLabelOrdSuite :: Test
testLabelOrdSuite = TestList
  [ testLabelOrd (testLab a b) (testOrd a b) n1 n2
      | (a,n1) <- nodelist , (b,n2) <- nodelist ]
    where
    testLab a b = a ++ "-" ++ b
    testOrd a b
      | nodeTest a b = EQ
      | otherwise  = comparing fromJust
                     (elemIndex a nodeorder)
                     (elemIndex b nodeorder)
    nodeTest a b = (a == b)        ||
            (a,b) `elem` nodeeqlist ||
            (b,a) `elem` nodeeqlist


------------------------------------------------------------
-- Statement construction and equality tests
------------------------------------------------------------

testStmtEq :: String -> Bool -> Statement -> Statement -> Test
testStmtEq lab eq t1 t2 =
    TestCase ( assertEqual ("testStmtEq:"++lab) eq (t1==t2) )

-- String argument is no longer needed, due to refactoring of
-- tlist, but left in
--
slist :: [(String, LabelMem)]
slist =
  [
    ("s1",s1), ("s4",s4), ("s5",s5), ("s6",s6), ("s7",s7)
  ]

plist :: [(String, LabelMem)]
plist =
  [
    ("p1",p1)
  ]

olist :: [(String, LabelMem)]
olist =
  [ ("o1",o1), ("o4",o4), ("o5",o5),
    ("l1",l1), ("l4",l4), ("l7",l7), ("l8",l8), ("l10",l10)
  ]

tlist :: [(String, Statement)]
tlist =
  [ t s p o | (_,s) <- slist, (_,p) <- plist, (_,o) <- olist ]
    where
      t s p o = let a = Arc s p o in (show a, a)

stmteqlist :: [(String, String)]
stmteqlist =
  [
    ("(s6,p1,l1)", "(s7,p1,l1)"),
    ("(s6,p1,l4)", "(s7,p1,l4)"),
    ("(s6,p1,l7)", "(s7,p1,l7)"),
    ("(s6,p1,l7)", "(s7,p1,l8)"),
    ("(s6,p1,l8)", "(s7,p1,l7)"),
    ("(s6,p1,l8)", "(s7,p1,l8)"),
    ("(s6,p1,l10)","(s7,p1,l10)"),
    ("(s6,p1,o1)", "(s7,p1,o1)"),
    ("(s6,p1,o4)", "(s7,p1,o4)"),
    ("(s6,p1,o5)", "(s7,p1,o5)"),
    ("(s1,p1,l7)", "(s1,p1,l8)"),
    ("(s4,p1,l7)", "(s4,p1,l8)"),
    ("(s5,p1,l7)", "(s5,p1,l8)"),
    ("(s6,p1,l7)", "(s6,p1,l8)"),
    ("(s7,p1,l7)", "(s7,p1,l8)")
  ]

testStmtEqSuite :: Test
testStmtEqSuite = TestList
  [ testStmtEq (testLab a b) (nodeTest  a b) t1 t2
      | (a,t1) <- tlist , (b,t2) <- tlist ]
    where
    testLab a b = a ++ "-" ++ b
    nodeTest  a b = (a == b)        ||
            (a,b) `elem` stmteqlist ||
            (b,a) `elem` stmteqlist

------------------------------------------------------------
--  Graph element handling support routines
------------------------------------------------------------

lmap :: LabelMap LabelMem
lmap = tstLabelMap 5 [(s1,(1,1)),(s2,(2,2)),(s3,(3,3)),(s4,(4,4)),
                      (o1,(1,1)),(o2,(2,2)),(o3,(3,3))]
       
-- setLabelHash :: (Label lb) => LabelMap lb -> (lb,Int) -> LabelMap lb
lmap1, lmap2a, lmap2b, lmap3 :: LabelMap LabelMem
lmap1  = setLabelHash lmap (s2,22)
lmap2a = setLabelHash lmap1  (o1,66)
lmap2b = setLabelHash lmap2a (o5,67)

-- newLabelMap :: (Label lb) => LabelMap lb -> [(lb,Int)] -> LabelMap lb
lmap3 = newLabelMap lmap [(s1,61),(s3,63),(o2,66)]

llst :: [String]       
llst = ["s1","s2","s3","s4","o1","o2","o3"]

-- showLabelMap :: (Label lb) => LabelMap lb -> String

testShowLabelMap :: Test
testShowLabelMap = testEq "showLabelMap" showMap (show lmap)
    where
        showMap = "LabelMap gen=5, map=\n"++
                  "    (!,(4,4))\n"++
                  "    (!o1,(1,1))\n"++
                  "    (!o2,(2,2))\n"++
                  "    (!o3,(3,3))\n"++
                  "    (!s1,(1,1))\n"++
                  "    (!s2,(2,2))\n"++
                  "    (!s3,(3,3))"

testMapLabelHash00 :: Test
testMapLabelHash00 = testEq "mapLabelHash00" showMap (show lmap1)
    where
        showMap = "LabelMap gen=5, map=\n"++
                  "    (!,(4,4))\n"++
                  "    (!o1,(1,1))\n"++
                  "    (!o2,(2,2))\n"++
                  "    (!o3,(3,3))\n"++
                  "    (!s1,(1,1))\n"++
                  "    (!s2,(5,22))\n"++
                  "    (!s3,(3,3))"

-- mapLabelIndex :: (Label lb) => LabelMap lb -> lb -> LabelIndex

testLabelMapSuite :: Test
testLabelMapSuite = TestList
  [ testShowLabelMap
  , testMapLabelHash00

  , testEq "testMapLabelIndex01" (1,1) (mapLabelIndex lmap s1 )
  , testEq "testMapLabelIndex02" (2,2) (mapLabelIndex lmap s2 )
  , testEq "testMapLabelIndex03" (3,3) (mapLabelIndex lmap s3 )
  , testEq "testMapLabelIndex04" (4,4) (mapLabelIndex lmap s4 )
  , testEq "testMapLabelIndex05" (1,1) (mapLabelIndex lmap o1 )
  , testEq "testMapLabelIndex06" (4,4) (mapLabelIndex lmap o4 )
  , testEq "testMapLabelIndex07" nullLabelVal (mapLabelIndex lmap o5 )
  , testEq "testMapLabelIndex08" nullLabelVal (mapLabelIndex lmap o6 )

  , testEq "MapLabelHash01" (1,1)  (mapLabelIndex lmap1 s1 )
  , testEq "MapLabelHash02" (5,22) (mapLabelIndex lmap1 s2 )
  , testEq "MapLabelHash03" (3,3)  (mapLabelIndex lmap1 s3 )
  , testEq "MapLabelHash04" (4,4)  (mapLabelIndex lmap1 s4 )
  , testEq "MapLabelHash05" (1,1)  (mapLabelIndex lmap1 o1 )
  , testEq "MapLabelHash06" (4,4)  (mapLabelIndex lmap1 o4 )
  , testEq "MapLabelHash07" nullLabelVal (mapLabelIndex lmap1 o5 )
  , testEq "MapLabelHash08" nullLabelVal (mapLabelIndex lmap1 o6 )

  , testEq "MapLabelHash11" (1,1)  (mapLabelIndex lmap2b s1 )
  , testEq "MapLabelHash12" (5,22) (mapLabelIndex lmap2b s2 )
  , testEq "MapLabelHash13" (3,3)  (mapLabelIndex lmap2b s3 )
  , testEq "MapLabelHash14" (4,4)  (mapLabelIndex lmap2b s4 )
  , testEq "MapLabelHash15" (5,66) (mapLabelIndex lmap2b o1 )
  , testEq "MapLabelHash16" (2,2)  (mapLabelIndex lmap2b o2 )
  , testEq "MapLabelHash17" (4,4)  (mapLabelIndex lmap2b o4 )
  , testEq "MapLabelHash18" nullLabelVal (mapLabelIndex lmap1 o5 )
    
  , testEq "LabelMap01" (6,61) (mapLabelIndex lmap3 s1 )
  , testEq "LabelMap02" (2,2)  (mapLabelIndex lmap3 s2 )
  , testEq "LabelMap03" (6,63) (mapLabelIndex lmap3 s3 )
  , testEq "LabelMap04" (4,4)  (mapLabelIndex lmap3 s4 )
  , testEq "LabelMap05" (1,1)  (mapLabelIndex lmap3 o1 )
  , testEq "LabelMap06" (6,66) (mapLabelIndex lmap3 o2 )
    
  ]

------------------------------------------------------------
--  Graph matching support
------------------------------------------------------------

t01, t02, t03, t04, t05, t06 :: Statement
t01 = arc s1 p1 o1
t02 = arc s2 p1 o2
t03 = arc s3 p1 o3
t04 = arc s1 p1 l1
t05 = arc s2 p1 l4
t06 = arc s3 p1 l10

tOrder16 :: [Statement]
tOrder16 = [t04, t01, t05, t02, t06, t03]
  
t10, t11, t12 :: Statement
t10 = arc s1 p1 b1
t11 = arc b1 p2 b2
t12 = arc b2 p3 o1

t20, t21, t22 :: Statement
t20 = arc s1 p1 b3
t21 = arc b3 p2 b4
t22 = arc b4 p3 o1

as1, as2, as4, as5, as6 :: S.Set Statement
as1 = S.singleton t01
as2 = S.fromList [t01,t02,t03,t04,t05,t06]
as4 = S.fromList [t01,t02,t03,t04,t05,t06,t10,t11,t12]
as5 = S.fromList [t01,t02,t03,t04,t05,t06,t20,t21,t22]
as6 = S.fromList [t01,t02,t03,t04,t05,t06,t10,t11,t12,t20,t21,t22]

-- graphLabels :: (Label lb) => ArcSet lb -> S.Set lb

-- not clear both the 'raw' and 'string' versions are still needed.

ls4, glas4 :: S.Set LabelMem
ls4   = S.fromList [s1,s2,s3,p1,p2,p3,o1,o2,o3,l1,l4,l10,b1,b2]
glas4 = graphLabels as4

testGraphLabels04, testGraphLabels14 :: Test
testGraphLabels04 = testEq "GraphLabels04" ls4 glas4
testGraphLabels14 = testEq "GraphLabels14" str (show glas4)
    where
      -- str = "[!s1,!p1,!o1,!s2,!o2,!s3,!o3,!l1,!l4-type1,!l10-xml,?b1,!p2,?b2,!p3]"
      str = "fromList [!l1,!l10-xml,!l4-type1,!o1,!o2,!o3,!p1,!p2,!p3,!s1,!s2,!s3,?b1,?b2]"
      -- str = show ls4

ls5, glas5 :: S.Set LabelMem
ls5   = S.fromList [s1,s2,s3,p1,p2,p3,o1,o2,o3,l1,l4,l10,b3,b4]
glas5 = graphLabels as5

testGraphLabels05, testGraphLabels15 :: Test
testGraphLabels05 = testEq "GraphLabels05" ls5 glas5
testGraphLabels15 = testEq "GraphLabels15" str (show glas5)
    where
      -- str = "[!s1,!p1,!o1,!s2,!o2,!s3,!o3,!l1,!l4-type1,!l10-xml,?b3,!p2,?b4,!p3]"
      str = "fromList [!l1,!l10-xml,!l4-type1,!o1,!o2,!o3,!p1,!p2,!p3,!s1,!s2,!s3,?b3,?b4]"
      -- str = show ls5

ls6, glas6 :: S.Set LabelMem
ls6   = S.fromList [s1,s2,s3,p1,p2,p3,o1,o2,o3,l1,l4,l10,b1,b2,b3,b4]
glas6 = graphLabels as6

testGraphLabels06, testGraphLabels16 :: Test
testGraphLabels06 = testEq "GraphLabels05" ls6 glas6
testGraphLabels16 = testEq "GraphLabels16" str (show glas6)
    where
      -- str = "[!s1,!p1,!o1,!s2,!o2,!s3,!o3"++
      --       ",!l1,!l4-type1,!l10-xml,?b1,!p2,?b2,!p3,?b3,?b4]"
      str = "fromList [!l1,!l10-xml,!l4-type1,!o1,!o2,!o3,!p1,!p2,!p3,!s1,!s2,!s3,?b1,?b2,?b3,?b4]"
      -- str = show ls6

-- assignLabels :: (Label lb) => [lb] -> LabelMap lb -> LabelMap lb

bhash :: Word32
-- bhash = 23 -- before trying to support Hashable 1.2.0
bhash = fromIntegral (23 `hashWithSalt` (0::Word32))

-- since the hashing is now done by hashable, is it worth checking
-- the hash values directly? It would perhaps be better to just
-- quickcheck that values get sorted.

-- copy of internal code in GraphMatch
toHash :: (Label lb) => Word32 -> lb -> Word32
toHash s lbl = 
    fromIntegral $
      if labelIsVar lbl 
        then 23 `hashWithSalt` s
        else labelHash (fromIntegral s) lbl

l1hash, l4hash, l10hash :: Word32
l1hash = toHash 0 l1
l4hash = toHash 0 l4
l10hash = toHash 0 l10  

l1hash2, l4hash2, l10hash2 :: Word32
l1hash2 = l1hash 
l4hash2 = l4hash
l10hash2 = l10hash

o1hash, o2hash, o3hash :: Word32
o1hash = toHash 0 o1
o2hash = toHash 0 o2
o3hash = toHash 0 o3

p1hash, p2hash, p3hash :: Word32
p1hash = toHash 0 p1
p2hash = toHash 0 p2
p3hash = toHash 0 p3

s1hash, s2hash, s3hash :: Word32
s1hash = toHash 0 s1
s2hash = toHash 0 s2
s3hash = toHash 0 s3

lmap5 :: LabelMap LabelMem
lmap5 = tstLabelMap 2 
        [
          (b4,(1,bhash)),
          (b3,(1,bhash)),
          (l10,(1,l10hash)),
          (l4,(1,l4hash)),
          (l1,(1,l1hash)),
          (o3,(1,o3hash)),
          (o2,(1,o2hash)),
          (o1,(1,o1hash)),
          (p3,(1,p3hash)),
          (p2,(1,p2hash)),
          (p1,(1,p1hash)),
          (s3,(1,s3hash)),
          (s2,(1,s2hash)),
          (s1,(1,s1hash))
        ]

testAssignLabelMap05 :: Test        
testAssignLabelMap05 = testEq "AssignLabels05" lmap5
                        (newGenerationMap $ assignLabelMap ls5 emptyMap)

lmap6 :: LabelMap LabelMem
lmap6 = tstLabelMap 2
        [
          (b2,(2,bhash)),
          (b1,(2,bhash)),
          (b4,(1,bhash)),
          (b3,(1,bhash)),
          (l10,(1,l10hash)),
          (l4,(1,l4hash)),
          (l1,(1,l1hash)),
          (o3,(1,o3hash)),
          (o2,(1,o2hash)),
          (o1,(1,o1hash)),
          (p3,(1,p3hash)),
          (p2,(1,p2hash)),
          (p1,(1,p1hash)),
          (s3,(1,s3hash)),
          (s2,(1,s2hash)),
          (s1,(1,s1hash))
        ]
        
testAssignLabelMap06 :: Test
testAssignLabelMap06 = testEq "AssignLabels06" lmap6 (assignLabelMap ls6 lmap5)

lmapc :: LabelMap LabelMem
lmapc = tstLabelMap 1 [(s1,(1,11)),(s2,(1,12)),(s3,(1,13)),
                       (p1,(1,21)),(p2,(1,22)),(p3,(1,13)),
                       (o1,(1,31)),(o2,(1,32)),(o3,(1,13)),
                       (l1,(1,41)),(l4,(1,42)),(l10,(1,43)),
                       (b1,(1,51)),(b2,(1,51)),(b3,(1,51)),(b4,(1,51))]

-- [[[TODO: test hash value collision on non-variable label]]]

testGraphMatchSupportSuite :: Test
testGraphMatchSupportSuite = TestList
  [ testGraphLabels04
  , testGraphLabels14
  , testGraphLabels05
  , testGraphLabels15
  , testGraphLabels06
  , testGraphLabels16
  , testAssignLabelMap05
  , testAssignLabelMap06
    -- implicitly tested elsewhere but included here for completeness
  , testEq "O16-identity" tOrder16 $ sort tOrder16
  , testEq "O16-compare"  tOrder16 $ sort [t01,t02,t03,t04,t05,t06]
  ]

------------------------------------------------------------
--  Test steps in graph equality test
------------------------------------------------------------

matchable :: a -> b -> Bool
matchable _ _ = True

s1_1, s2_1, s3_1 :: ScopedLabel LabelMem
s1_1 = makeScopedLabel 1 s1
s2_1 = makeScopedLabel 1 s2
s3_1 = makeScopedLabel 1 s3

p1_1, p2_1, p3_1 :: ScopedLabel LabelMem
p1_1 = makeScopedLabel 1 p1
p2_1 = makeScopedLabel 1 p2
p3_1 = makeScopedLabel 1 p3

o1_1, o2_1, o3_1 :: ScopedLabel LabelMem
o1_1 = makeScopedLabel 1 o1
o2_1 = makeScopedLabel 1 o2
o3_1 = makeScopedLabel 1 o3

l1_1, l4_1, l10_1 :: ScopedLabel LabelMem
l1_1 = makeScopedLabel 1 l1
l4_1 = makeScopedLabel 1 l4
l10_1 = makeScopedLabel 1 l10

b1_1, b2_1, b3_1, b4_1 :: ScopedLabel LabelMem
b1_1 = makeScopedLabel 1 b1
b2_1 = makeScopedLabel 1 b2
b3_1 = makeScopedLabel 1 b3
b4_1 = makeScopedLabel 1 b4

s1_2, s2_2, s3_2 :: ScopedLabel LabelMem
s1_2 = makeScopedLabel 2 s1
s2_2 = makeScopedLabel 2 s2
s3_2 = makeScopedLabel 2 s3

p1_2, p2_2, p3_2 :: ScopedLabel LabelMem
p1_2 = makeScopedLabel 2 p1
p2_2 = makeScopedLabel 2 p2
p3_2 = makeScopedLabel 2 p3

o1_2, o2_2, o3_2 :: ScopedLabel LabelMem
o1_2 = makeScopedLabel 2 o1
o2_2 = makeScopedLabel 2 o2
o3_2 = makeScopedLabel 2 o3

l1_2, l4_2, l10_2 :: ScopedLabel LabelMem
l1_2 = makeScopedLabel 2 l1
l4_2 = makeScopedLabel 2 l4
l10_2 = makeScopedLabel 2 l10

b1_2, b2_2, b3_2, b4_2 :: ScopedLabel LabelMem
b1_2 = makeScopedLabel 2 b1
b2_2 = makeScopedLabel 2 b2
b3_2 = makeScopedLabel 2 b3
b4_2 = makeScopedLabel 2 b4

t01_1 :: Arc (ScopedLabel LabelMem)
t01_1 = makeScopedArc 1 t01

t01_2, t02_2, t03_2, t04_2, t05_2, t06_2 :: Arc (ScopedLabel LabelMem)
t01_2 = makeScopedArc 2 t01
t02_2 = makeScopedArc 2 t02
t03_2 = makeScopedArc 2 t03
t04_2 = makeScopedArc 2 t04
t05_2 = makeScopedArc 2 t05
t06_2 = makeScopedArc 2 t06

t10_1, t11_1, t12_1, t20_1, t21_1, t22_1 :: Arc (ScopedLabel LabelMem)
t10_1 = makeScopedArc 1 t10
t11_1 = makeScopedArc 1 t11
t12_1 = makeScopedArc 1 t12
t20_1 = makeScopedArc 1 t20
t21_1 = makeScopedArc 1 t21
t22_1 = makeScopedArc 1 t22

t10_2, t11_2, t12_2, t20_2, t21_2, t22_2 :: Arc (ScopedLabel LabelMem)
t10_2 = makeScopedArc 2 t10
t11_2 = makeScopedArc 2 t11
t12_2 = makeScopedArc 2 t12
t20_2 = makeScopedArc 2 t20
t21_2 = makeScopedArc 2 t21
t22_2 = makeScopedArc 2 t22

-- Compare graph as6 with self, in steps

as61, as62 :: ArcSet (ScopedLabel LabelMem)
as61 = S.map (makeScopedArc 1) as6
as62 = S.map (makeScopedArc 2) as6

eq1lmap :: LabelMap (ScopedLabel LabelMem)
eq1lmap     = newGenerationMap $
              assignLabelMap (graphLabels as62) $
              assignLabelMap (graphLabels as61) emptyMap

eq1ltst :: LabelMap (ScopedLabel LabelMem)
eq1ltst = tstLabelMap 2 
          [
            (b4_2,(1,bhash)),
            (b3_2,(1,bhash)),
            (p3_2,(1,p3hash)),
            (b2_2,(1,bhash)),
            (p2_2,(1,p2hash)),
            (b1_2,(1,bhash)),
            (l10_2,(1,l10hash)),
            (l4_2,(1,l4hash)),
            (l1_2,(1,l1hash)),
            (o3_2,(1,o3hash)),
            (s3_2,(1,s3hash)),
            (o2_2,(1,o2hash)),
            (s2_2,(1,s2hash)),
            (o1_2,(1,o1hash)),
            (p1_2,(1,p1hash)),
            (s1_2,(1,s1hash)),
            (b4_1,(1,bhash)),
            (b3_1,(1,bhash)),
            (p3_1,(1,p3hash)),
            (b2_1,(1,bhash)),
            (p2_1,(1,p2hash)),
            (b1_1,(1,bhash)),
            (l10_1,(1,l10hash)),
            (l4_1,(1,l4hash)),
            (l1_1,(1,l1hash)),
            (o3_1,(1,o3hash)),
            (s3_1,(1,s3hash)),
            (o2_1,(1,o2hash)),
            (s2_1,(1,s2hash)),
            (o1_1,(1,o1hash)),
            (p1_1,(1,p1hash)),
            (s1_1,(1,s1hash))
            ]
          
testEqAssignMap01 :: Test              
testEqAssignMap01 = testEq "EqAssignMap01" eq1ltst eq1lmap

eq1hs1, eq1hs2 :: [Arc (ScopedLabel LabelMem)]
eq1hs1      = [t10_1,t11_1,t12_1,t20_1,t21_1,t22_1]
eq1hs2      = [t10_2,t11_2,t12_2,t20_2,t21_2,t22_2]

eq1lmap' :: LabelMap (ScopedLabel LabelMem)
eq1lmap'    = tstLabelMap 2 [(s1_1,(1,142577)),(s2_1,(1,142578)),(s3_1,(1,142579)),
                             (s1_2,(1,142577)),(s2_2,(1,142578)),(s3_2,(1,142579)),
                             (p1_1,(1,142385)),(p2_1,(1,142386)),(p3_1,(1,142387)),
                             (p1_2,(1,142385)),(p2_2,(1,142386)),(p3_2,(1,142387)),
                             (o1_1,(1,142321)),(o2_1,(1,142322)),(o3_1,(1,142323)),
                             (o1_2,(1,142321)),(o2_2,(1,142322)),(o3_2,(1,142323)),
                             (l1_1,(1,142129)),(l4_1,(1,1709580)),(l10_1,(1,3766582)),
                             (l1_2,(1,142129)),(l4_2,(1,1709580)),(l10_2,(1,3766582)),
                             (b1_1,(2,3880463)),(b2_1,(2,3400925)),
                                                (b3_1,(2,3880463)),
                                                (b4_1,(2,3400925)),
                             (b1_2,(2,3880463)),(b2_2,(2,3400925)),
                                                (b3_2,(2,3880463)),
                                                (b4_2,(2,3400925))]

eq1lmap'' :: LabelMap (ScopedLabel LabelMem)
eq1lmap''   = newLabelMap eq1lmap'
                [
                (b1_1,2576315),(b2_1,3400925),(b3_1,1571691),
                (b1_2,2576315),(b2_2,3400925),(b3_2,1571691)
                ]

eq1ltst'' :: LabelMap (ScopedLabel LabelMem)
eq1ltst''   = tstLabelMap 3 [
                            (s1_1,(1,142577)),(s2_1,(1,142578)),(s3_1,(1,142579)),
                            (p1_1,(1,142385)),(p2_1,(1,142386)),(p3_1,(1,142387)),
                            (o1_1,(1,142321)),(o2_1,(1,142322)),(o3_1,(1,142323)),
                            (l1_1,(1,142129)),(l4_1,(1,1709580)),(l10_1,(1,3766582)),
                            (b1_1,(3,2576315)),
                            (b2_1,(3,3400925)),
                            (b3_1,(3,1571691)),
                            (b4_1,(2,3400925)),
                            (s1_2,(1,142577)),(s2_2,(1,142578)),(s3_2,(1,142579)),
                            (p1_2,(1,142385)),(p2_2,(1,142386)),(p3_2,(1,142387)),
                            (o1_2,(1,142321)),(o2_2,(1,142322)),(o3_2,(1,142323)),
                            (l1_2,(1,142129)),(l4_2,(1,1709580)),(l10_2,(1,3766582)),
                            (b1_2,(3,2576315)),
                            (b2_2,(3,3400925)),
                            (b3_2,(3,1571691)),
                            (b4_2,(2,3400925))
                            ]

testEqNewLabelMap07 :: Test
testEqNewLabelMap07 = testEq "EqNewLabelMap07" eq1ltst'' eq1lmap''

-- Repeat same tests for as4...

as41, as42 :: ArcSet (ScopedLabel LabelMem)
as41 = S.map (makeScopedArc 1) as4
as42 = S.map (makeScopedArc 2) as4

eq2lmap :: LabelMap (ScopedLabel LabelMem)
eq2lmap     = newGenerationMap $
              assignLabelMap (graphLabels as42) $
              assignLabelMap (graphLabels as41) emptyMap
              
eq2ltst :: LabelMap (ScopedLabel LabelMem)
eq2ltst = tstLabelMap 2
          [
            (p3_2,(1,p3hash)),
            (b2_2,(1,bhash)),
            (b1_2,(1,bhash)),
            (p2_2,(1,p2hash)),
            (l10_2,(1,l10hash)),
            (l4_2,(1,l4hash)),
            (l1_2,(1,l1hash)),
            (o3_2,(1,o3hash)),
            (s3_2,(1,s3hash)),
            (o2_2,(1,o2hash)),
            (s2_2,(1,s2hash)),
            (o1_2,(1,o1hash)),
            (p1_2,(1,p1hash)),
            (s1_2,(1,s1hash)),
            (p3_1,(1,p3hash)),
            (b2_1,(1,bhash)),
            (p2_1,(1,p2hash)),
            (b1_1,(1,bhash)),
            (l10_1,(1,l10hash)),
            (l4_1,(1,l4hash)),
            (l1_1,(1,l1hash)),
            (o3_1,(1,o3hash)),
            (s3_1,(1,s3hash)),
            (o2_1,(1,o2hash)),
            (s2_1,(1,s2hash)),
            (o1_1,(1,o1hash)),
            (p1_1,(1,p1hash)),
            (s1_1,(1,s1hash))
          ]

testEqAssignMap21 :: Test
testEqAssignMap21 = testEq "EqAssignMap21" eq2ltst eq2lmap

eq2hs1, eq2hs2 :: [Arc (ScopedLabel LabelMem)]
eq2hs1      = [t10_1,t11_1,t12_1]
eq2hs2      = [t10_2,t11_2,t12_2]

eq2lmap' :: LabelMap (ScopedLabel LabelMem)
eq2lmap'    = tstLabelMap 2 [
                             (s1_1,(1,142577)),(s2_1,(1,142578)),(s3_1,(1,142579)),
                             (p1_1,(1,142385)),(p2_1,(1,142386)),(p3_1,(1,142387)),
                             (o1_1,(1,142321)),(o2_1,(1,142322)),(o3_1,(1,142323)),
                             (l1_1,(1,142129)),(l4_1,(1,1709580)),(l10_1,(1,3766582)),
                             (b1_1,(2,3880463)),(b2_1,(2,3400925)),
                             (s1_2,(1,142577)),(s2_2,(1,142578)),(s3_2,(1,142579)),
                             (p1_2,(1,142385)),(p2_2,(1,142386)),(p3_2,(1,142387)),
                             (o1_2,(1,142321)),(o2_2,(1,142322)),(o3_2,(1,142323)),
                             (l1_2,(1,142129)),(l4_2,(1,1709580)),(l10_2,(1,3766582)),
                             (b1_2,(2,3880463)),(b2_2,(2,3400925))
                            ]

eq2lmap'' :: LabelMap (ScopedLabel LabelMem)
eq2lmap''   = newLabelMap eq2lmap'
                [
                (b2_1,3400925),
                (b2_2,3400925)
                ]
                
eq2ltst'' :: LabelMap (ScopedLabel LabelMem)
eq2ltst''   = tstLabelMap 3 [
                            (s1_1,(1,142577)),(s2_1,(1,142578)),(s3_1,(1,142579)),
                            (p1_1,(1,142385)),(p2_1,(1,142386)),(p3_1,(1,142387)),
                            (o1_1,(1,142321)),(o2_1,(1,142322)),(o3_1,(1,142323)),
                            (l1_1,(1,142129)),(l4_1,(1,1709580)),(l10_1,(1,3766582)),
                            (b1_1,(2,3880463)),
                            (b2_1,(3,3400925)),
                            (s1_2,(1,142577)),(s2_2,(1,142578)),(s3_2,(1,142579)),
                            (p1_2,(1,142385)),(p2_2,(1,142386)),(p3_2,(1,142387)),
                            (o1_2,(1,142321)),(o2_2,(1,142322)),(o3_2,(1,142323)),
                            (l1_2,(1,142129)),(l4_2,(1,1709580)),(l10_2,(1,3766582)),
                            (b1_2,(2,3880463)),
                            (b2_2,(3,3400925))
                            ]

testEqNewLabelMap27 :: Test
testEqNewLabelMap27 = testEq "EqNewLabelMap27" eq2ltst'' eq2lmap''

-- Compare as1 with as2, in steps

as11, as22 :: ArcSet (ScopedLabel LabelMem)
as11 = S.map (makeScopedArc 1) as1
as22 = S.map (makeScopedArc 2) as2

eq3hs1, eq3hs2 :: ArcSet (ScopedLabel LabelMem)
eq3hs1   = S.singleton t01_1
eq3hs2   = S.fromList [t01_2,t02_2,t03_2,t04_2,t05_2,t06_2]

testEqGraphMap31_1, testEqGraphMap31_2 :: Test
testEqGraphMap31_1 = testEq "testEqGraphMap31_1" eq3hs1 as11
testEqGraphMap31_2 = testEq "testEqGraphMap31_2" eq3hs2 as22

eq3lmap :: LabelMap (ScopedLabel LabelMem)
eq3lmap     = newGenerationMap $
              assignLabelMap (graphLabels as11) $
              assignLabelMap (graphLabels as22) emptyMap
              
eq3ltst :: LabelMap (ScopedLabel LabelMem)
eq3ltst = tstLabelMap 2
    [ 
      (o1_1,(1,o1hash))
    , (p1_1,(1,p1hash))
    , (s1_1,(1,s1hash))
    , (l10_2,(1,l10hash))
    , (l4_2,(1,l4hash))
    , (l1_2,(1,l1hash))
    , (o3_2,(1,o3hash))
    , (s3_2,(1,s3hash))
    , (o2_2,(1,o2hash))
    , (s2_2,(1,s2hash))
    , (o1_2,(1,o1hash))
    , (p1_2,(1,p1hash))
    , (s1_2,(1,s1hash)) 
    ]

testEqAssignMap32 :: Test    
testEqAssignMap32 = testEq "EqAssignMap32" eq3ltst eq3lmap

type EquivClass = EquivalenceClass (ScopedLabel LabelMem)
type EquivArgs  = ((Word32, Word32), [ScopedLabel LabelMem])

ec31 :: [EquivClass]
ec31 = equivalenceClasses eq3lmap (graphLabels as11)

ec31test :: [EquivArgs]
ec31test =
    sortBy (compare `on` (snd.fst))
    [ ((1,o1hash),[o1_1])
    , ((1,p1hash),[p1_1])
    , ((1,s1hash),[s1_1])
    ]

ec32 :: [EquivClass]
ec32 = equivalenceClasses eq3lmap (graphLabels as22)

ec32test :: [EquivArgs]
ec32test =
    sortBy (compare `on` (snd.fst))
    [ ((1,l1hash),[l1_2])
    , ((1,o2hash),[o2_2])
    , ((1,o3hash),[o3_2])
    , ((1,o1hash),[o1_2])
    , ((1,p1hash),[p1_2])
    , ((1,s2hash),[s2_2])
    , ((1,s3hash),[s3_2])
    , ((1,s1hash),[s1_2])
    , ((1,l10hash),[l10_2])
    , ((1,l4hash),[l4_2])
    ]
  
testEquivClass33_1, testEquivClass33_2 :: Test
testEquivClass33_1 = testEq "EquivClass33_1" ec31test ec31
testEquivClass33_2 = testEq "EquivClass33_2" ec32test ec32

{- as pairSOrt is no-longer exported need to check this code gets tested

-- This value is nonsense for this test,
-- but a parameter is needed for graphMatch1 (below)

ec3pairs :: [(EquivClass, EquivClass)]
ec3pairs = zip (pairSort ec31) (pairSort ec32)

-}

{-  This is a pointless test in this case

ec3test :: [(EquivClass, EquivClass)]
ec3test  =
    [ ( ((1,142321),[o1_1]), ((1,142321),[o1_2]) )
    , ( ((1,142385),[p1_1]), ((1,142385),[p1_2]) )
    , ( ((1,142577),[s1_1]), ((1,142577),[s1_2]) )
    ]

testEquivClass33_3 = testEq "EquivClass33_3" ec3test ec3pairs
-}

{- pairSort is no longer exported

eq3lmap1 :: (Bool, LabelMap (ScopedLabel LabelMem))
eq3lmap1 = graphMatch1 False matchable eq3hs1 eq3hs2 eq3lmap ec3pairs

-}

eq3ltst1 :: LabelMap (ScopedLabel LabelMem)
eq3ltst1 = tstLabelMap 2
    [ (o1_1,(1,142321))
    , (p1_1,(1,142385))
    , (s1_1,(1,142577))
    , (l10_2,(1,3766582))
    , (l4_2,(1,1709580))
    , (l1_2,(1,142129))
    , (o3_2,(1,142323))
    , (s3_2,(1,142579))
    , (o2_2,(1,142322))
    , (s2_2,(1,142578))
    , (o1_2,(1,142321))
    , (p1_2,(1,142385))
    , (s1_2,(1,142577))
    ]
-- testEqAssignMap34 = testEq "EqAssignMap34" (Just eq3ltst1) eq3lmap1
-- testEqAssignMap34 = testEq "EqAssignMap34" Nothing eq3lmap1

{- pairSort is not exported
testEqAssignMap34 :: Test
testEqAssignMap34 = testEq "EqAssignMap34" False (fst eq3lmap1)
-}

{-
eq3rc1      = reclassify eq3hs1 eq3lmap
eq3rctst1   = []
testEqReclassify35_1 = testEqv "EqReclassify35_1" (makeEntries eq3rctst1) eq3rc1
eq3rc2      = reclassify eq3hs2 eq3lmap
eq3rctst2   = []
testEqReclassify35_2 = testEqv "EqReclassify35_2" (makeEntries eq3rctst2) eq3rc2
-}


-- Test suite

testGraphMatchStepSuite :: Test
testGraphMatchStepSuite = TestList
  [ testEqAssignMap01
  -- , testEqReclassify03_1, testEqReclassify03_2
  , testEqNewLabelMap07
  -- , testEqGraphMatch08
  , testEqAssignMap21
  -- , testEqReclassify23_1, testEqReclassify23_2
  , testEqNewLabelMap27
  -- , testEqGraphMatch28
  , testEqGraphMap31_1, testEqGraphMap31_2
  , testEqAssignMap32
  , testEquivClass33_1, testEquivClass33_2 -- , testEquivClass33_3
  -- , testEqAssignMap34    pairSort is not exported
  -- , testEqReclassify35_1, testEqReclassify35_2
  ]

------------------------------------------------------------
--  Graph equality tests
------------------------------------------------------------

testGraphEq :: ( Label lb ) => String -> Bool -> GraphMem lb -> GraphMem lb -> Test
testGraphEq lab eq gg1 gg2 =
    TestCase ( assertEqual ("testGraphEq:"++lab) eq (gg1==gg2) )

toG :: [Statement] -> GraphMem LabelMem
toG stmts = GraphMem { arcs = S.fromList stmts }

g1, g2, g3, g4, g5, g6, g7, g8 :: GraphMem LabelMem
g1 = toG [t01]
g2 = toG [t01,t02,t03,t04,t05,t06]
g3 = toG [t06,t05,t04,t03,t02,t01]
g4 = toG [t01,t02,t03,t04,t05,t06,t10,t11,t12]
g5 = toG [t01,t02,t03,t04,t05,t06,t20,t21,t22]
g6 = toG [t01,t02,t03,t04,t05,t06,t10,t11,t12,t20,t21,t22]
g7 = toG [t01,t02]
g8 = toG [t02,t01]

glist :: [(String, GraphMem LabelMem)]
glist =
  [ ("g1",g1), ("g2",g2), ("g3",g3), ("g4",g4), ("g5",g5), ("g6",g6) ]

grapheqlist :: [(String, String)]
grapheqlist =
  [ ("g2","g3")
  , ("g4","g5")
  ]

testGraphEqSuitePart :: Test
testGraphEqSuitePart = TestLabel "testGraphEqSuitePart" $ TestList
  [ testGraphEq "g1-g2" False g1 g2
  , testGraphEq "g2-g1" False g2 g1
  , testGraphEq "g2-g2" True  g2 g2
  , testGraphEq "g2-g3" True  g2 g3
  , testGraphEq "g1-g4" False g1 g4
  , testGraphEq "g2-g4" False g2 g4
  , testGraphEq "g3-g4" False g3 g4
  , testGraphEq "g4-g3" False g4 g3
  , testGraphEq "g4-g4" True  g4 g4
  , testGraphEq "g4-g5" True  g4 g5
  , testGraphEq "g4-g6" False g4 g6
  , testGraphEq "g6-g6" True  g6 g6
  , testGraphEq "g7-g7" True  g7 g7
  , testGraphEq "g7-g8" True  g7 g8
  , testGraphEq "g8-g7" True  g8 g7
  ]

testGraphEqSuite :: Test
testGraphEqSuite = TestLabel "testGraphEqSuite" $ TestList
  [ testGraphEq (testLab ll1 ll2) (nodeTest ll1 ll2) gg1 gg2
      | (ll1,gg1) <- glist , (ll2,gg2) <- glist ]
    where
    testLab ll1 ll2 = ll1 ++ "-" ++ ll2
    nodeTest  ll1 ll2 = (ll1 == ll2)        ||
            (ll1,ll2) `elem` grapheqlist ||
            (ll2,ll1) `elem` grapheqlist

-- Selected tests for debugging

geq12, geq21, geq22, geq23, geq14, geq24, geq77, geq78, geq87 :: Test
geq12 = testGraphEq "g1-g2" False g1 g2
geq21 = testGraphEq "g2-g1" False g2 g1
geq22 = testGraphEq "g2-g2" True  g2 g2
geq23 = testGraphEq "g2-g3" True  g2 g3
geq14 = testGraphEq "g1-g4" False g1 g4
geq24 = testGraphEq "g2-g4" False g2 g4
geq77 = testGraphEq "g7-g7" True  g7 g7
geq78 = testGraphEq "g7-g8" True  g7 g8
geq87 = testGraphEq "g8-g7" True  g8 g7


------------------------------------------------------------
--  More graph equality tests
------------------------------------------------------------
--
--  These tests are based on the 10-node, triply connected
--  graph examples in Jeremy Carroll's paper on matching RDF
--  graphs.

--  Graph pattern 1:
--  pentangle-in-pentangle, corresponding vertices linked upward

v101, v102, v103, v104, v105, v106, v107, v108,
  v109, v110 :: LabelMem
v101  = LV "v101"
v102  = LV "v102"
v103  = LV "v103"
v104  = LV "v104"
v105  = LV "v105"
v106  = LV "v106"
v107  = LV "v107"
v108  = LV "v108"
v109  = LV "v109"
v110  = LV "v110"

p101, p102, p103, p104, p105, p106, p107, p108,
  p109, p110, p111, p112, p113, p114, p115 :: LabelMem
p101  = LV "p101"
p102  = LV "p102"
p103  = LV "p103"
p104  = LV "p104"
p105  = LV "p105"
p106  = LV "p106"
p107  = LV "p107"
p108  = LV "p108"
p109  = LV "p109"
p110  = LV "p110"
p111  = LV "p111"
p112  = LV "p112"
p113  = LV "p113"
p114  = LV "p114"
p115  = LV "p115"

t10102, t10203, t10304, t10405, t10501, t10106,
  t10207, t10308, t10409, t10510, t10607,
  t10708, t10809, t10910, t11006 :: Statement
t10102 = arc v101 p101 v102
t10203 = arc v102 p102 v103
t10304 = arc v103 p103 v104
t10405 = arc v104 p104 v105
t10501 = arc v105 p105 v101
t10106 = arc v101 p106 v106
t10207 = arc v102 p107 v107
t10308 = arc v103 p108 v108
t10409 = arc v104 p109 v109
t10510 = arc v105 p110 v110
t10607 = arc v106 p111 v107
t10708 = arc v107 p112 v108
t10809 = arc v108 p113 v109
t10910 = arc v109 p114 v110
t11006 = arc v110 p115 v106

--  Graph pattern 2:
--  pentangle-in-pentangle, corresponding vertices linked downward

v201, v202, v203, v204, v205, v206, v207, v208,
  v209, v210 :: LabelMem
v201  = LV "v201"
v202  = LV "v202"
v203  = LV "v203"
v204  = LV "v204"
v205  = LV "v205"
v206  = LV "v206"
v207  = LV "v207"
v208  = LV "v208"
v209  = LV "v209"
v210  = LV "v210"

p201, p202, p203, p204, p205, p206, p207, p208,
  p209, p210, p211, p212, p213, p214, p215 :: LabelMem
p201  = LV "p201"
p202  = LV "p202"
p203  = LV "p203"
p204  = LV "p204"
p205  = LV "p205"
p206  = LV "p206"
p207  = LV "p207"
p208  = LV "p208"
p209  = LV "p209"
p210  = LV "p210"
p211  = LV "p211"
p212  = LV "p212"
p213  = LV "p213"
p214  = LV "p214"
p215  = LV "p215"

t20102, t20203, t20304, t20405, t20501, t20601,
  t20702, t20803, t20904, t21005, t20607,
  t20708, t20809, t20910, t21006 :: Statement
t20102 = arc v201 p201 v202
t20203 = arc v202 p202 v203
t20304 = arc v203 p203 v204
t20405 = arc v204 p204 v205
t20501 = arc v205 p205 v201
t20601 = arc v206 p206 v201
t20702 = arc v207 p207 v202
t20803 = arc v208 p208 v203
t20904 = arc v209 p209 v204
t21005 = arc v210 p210 v205
t20607 = arc v206 p211 v207
t20708 = arc v207 p212 v208
t20809 = arc v208 p213 v209
t20910 = arc v209 p214 v210
t21006 = arc v210 p215 v206

--  Graph pattern 3:
--  star-in-pentangle, corresponding vertices linked toward star
--  Although this graph is similarly linked to patterns 1 and 2,
--  it is topologically different as it contains circuits only of
--  length 5, where the others have circuits of length 4 and 5
--  (ignoring direction of arcs)

v301, v302, v303, v304, v305, v306, v307, v308,
  v309, v310 :: LabelMem
v301  = LV "v301"
v302  = LV "v302"
v303  = LV "v303"
v304  = LV "v304"
v305  = LV "v305"
v306  = LV "v306"
v307  = LV "v307"
v308  = LV "v308"
v309  = LV "v309"
v310  = LV "v310"

p301, p302, p303, p304, p305, p306, p307, p308,
  p309, p310, p311, p312, p313, p314, p315 :: LabelMem
p301  = LV "p301"
p302  = LV "p302"
p303  = LV "p303"
p304  = LV "p304"
p305  = LV "p305"
p306  = LV "p306"
p307  = LV "p307"
p308  = LV "p308"
p309  = LV "p309"
p310  = LV "p310"
p311  = LV "p311"
p312  = LV "p312"
p313  = LV "p313"
p314  = LV "p314"
p315  = LV "p315"

t30102, t30203, t30304, t30405, t30501, t30106,
  t30207, t30308, t30409, t30510, t30608,
  t30709, t30810, t30906, t31007 :: Statement
t30102 = arc v301 p301 v302
t30203 = arc v302 p302 v303
t30304 = arc v303 p303 v304
t30405 = arc v304 p304 v305
t30501 = arc v305 p305 v301
t30106 = arc v301 p306 v306
t30207 = arc v302 p307 v307
t30308 = arc v303 p308 v308
t30409 = arc v304 p309 v309
t30510 = arc v305 p310 v310
t30608 = arc v306 p311 v308
t30709 = arc v307 p312 v309
t30810 = arc v308 p313 v310
t30906 = arc v309 p314 v306
t31007 = arc v310 p315 v307

--  Graph pattern 4:
--  pentangle-in-pentangle, corresponding vertices linked upward
--  The vertices 6-10 are linked in reverse order to the
--  corresponding vertices 1-5.

v401, v402, v403, v404, v405, v406, v407, v408,
  v409, v410 :: LabelMem
v401  = LV "v401"
v402  = LV "v402"
v403  = LV "v403"
v404  = LV "v404"
v405  = LV "v405"
v406  = LV "v406"
v407  = LV "v407"
v408  = LV "v408"
v409  = LV "v409"
v410  = LV "v410"

p401, p402, p403, p404, p405, p406, p407, p408,
  p409, p410, p411, p412, p413, p414, p415 :: LabelMem
p401  = LV "p401"
p402  = LV "p402"
p403  = LV "p403"
p404  = LV "p404"
p405  = LV "p405"
p406  = LV "p406"
p407  = LV "p407"
p408  = LV "p408"
p409  = LV "p409"
p410  = LV "p410"
p411  = LV "p411"
p412  = LV "p412"
p413  = LV "p413"
p414  = LV "p414"
p415  = LV "p415"

t40102, t40203, t40304, t40405, t40501, t40106,
  t40207, t40308, t40409, t40510, t41009,
  t40908, t40807, t40706, t40610:: Statement
t40102 = arc v401 p401 v402
t40203 = arc v402 p402 v403
t40304 = arc v403 p403 v404
t40405 = arc v404 p404 v405
t40501 = arc v405 p405 v401
t40106 = arc v401 p406 v406
t40207 = arc v402 p407 v407
t40308 = arc v403 p408 v408
t40409 = arc v404 p409 v409
t40510 = arc v405 p410 v410
t41009 = arc v410 p411 v409
t40908 = arc v409 p412 v408
t40807 = arc v408 p413 v407
t40706 = arc v407 p414 v406
t40610 = arc v406 p415 v410

--  Graph pattern 5:
--  Same as pattern 1, except same fixed property in all cases.

p5 :: LabelMem
p5    = LF "p5"

t50102, t50203, t50304, t50405, t50501, t50106, t50207,
  t50308, t50409, t50510, t50607, t50708, t50809,
  t50910, t51006 :: Statement
t50102 = arc v101 p5 v102
t50203 = arc v102 p5 v103
t50304 = arc v103 p5 v104
t50405 = arc v104 p5 v105
t50501 = arc v105 p5 v101
t50106 = arc v101 p5 v106
t50207 = arc v102 p5 v107
t50308 = arc v103 p5 v108
t50409 = arc v104 p5 v109
t50510 = arc v105 p5 v110
t50607 = arc v106 p5 v107
t50708 = arc v107 p5 v108
t50809 = arc v108 p5 v109
t50910 = arc v109 p5 v110
t51006 = arc v110 p5 v106

--  Graph pattern 6:
--  Same as pattern 5, with different variables

t60102, t60203, t60304, t60405, t60501, t60106,
  t60207, t60308, t60409, t60510, t60607, t60708,
  t60809, t60910, t61006 :: Statement
t60102 = arc v201 p5 v202
t60203 = arc v202 p5 v203
t60304 = arc v203 p5 v204
t60405 = arc v204 p5 v205
t60501 = arc v205 p5 v201
t60106 = arc v201 p5 v206
t60207 = arc v202 p5 v207
t60308 = arc v203 p5 v208
t60409 = arc v204 p5 v209
t60510 = arc v205 p5 v210
t60607 = arc v206 p5 v207
t60708 = arc v207 p5 v208
t60809 = arc v208 p5 v209
t60910 = arc v209 p5 v210
t61006 = arc v210 p5 v206

--

arcsToGraph :: (Ord a) => [Arc a] -> GraphMem a
arcsToGraph as = GraphMem { arcs = S.fromList as }

-- Very simple case

g100 :: GraphMem LabelMem
g100 = arcsToGraph
       [ t10102, t10203, t10304, t10405, t10501,
         t10607, t10708, t10809, t10910, t11006
       ]

g200 :: GraphMem LabelMem
g200 = arcsToGraph
       [ t20102, t20203, t20304, t20405, t20501,
         t20607, t20708, t20809, t20910, t21006
       ]

-- 10/3 node graph comparisons

g101 :: GraphMem LabelMem
g101 = arcsToGraph
       [ t10102, t10203, t10304, t10405, t10501,
         t10106, t10207, t10308, t10409, t10510,
         t10607, t10708, t10809, t10910, t11006 ]

g201 :: GraphMem LabelMem
g201 = arcsToGraph
       [ t20102, t20203, t20304, t20405, t20501,
         t20601, t20702, t20803, t20904, t21005,
         t20607, t20708, t20809, t20910, t21006 ]

g301 :: GraphMem LabelMem
g301 = arcsToGraph
       [ t30102, t30203, t30304, t30405, t30501,
         t30106, t30207, t30308, t30409, t30510,
         t30608, t30709, t30810, t30906, t31007 ]

g401 :: GraphMem LabelMem
g401 = arcsToGraph
       [ t40102, t40203, t40304, t40405, t40501,
         t40106, t40207, t40308, t40409, t40510,
         t40610, t40706, t40807, t40908, t41009 ]

g501 :: GraphMem LabelMem
g501 = arcsToGraph
       [ t50102, t50203, t50304, t50405, t50501,
         t50106, t50207, t50308, t50409, t50510,
         t50607, t50708, t50809, t50910, t51006 ]

g601 :: GraphMem LabelMem
g601 = arcsToGraph
       [ t60102, t60203, t60304, t60405, t60501,
         t60106, t60207, t60308, t60409, t60510,
         t60607, t60708, t60809, t60910, t61006 ]

-- Remove one arc from each

g102 :: GraphMem LabelMem
g102 = arcsToGraph
       [ t10102, t10203, t10304, t10405,
         t10106, t10207, t10308, t10409, t10510,
         t10607, t10708, t10809, t10910, t11006 ]

g202 :: GraphMem LabelMem
g202 = arcsToGraph
       [ t20102, t20203, t20304, t20405, t20501,
         t20601, t20702, t20803, t20904, t21005,
                 t20708, t20809, t20910, t21006 ]

g302 :: GraphMem LabelMem
g302 = arcsToGraph
       [ t20102, t20203, t20304, t20405, t20501,
         t20601, t20702, t20803, t20904,
         t20607, t20708, t20809, t20910, t21006 ]

-- Remove two adjacent arcs from each

g103 :: GraphMem LabelMem
g103 = arcsToGraph
       [ t10102, t10203, t10304,
         t10106, t10207, t10308, t10409, t10510,
         t10607, t10708, t10809, t10910, t11006 ]

g203 :: GraphMem LabelMem
g203 = arcsToGraph
       [ t20102, t20203, t20304, t20405, t20501,
         t20601, t20702, t20803, t20904, t21005,
         t20607, t20708,                 t21006 ]

g303 :: GraphMem LabelMem
g303 = arcsToGraph
       [ t20102, t20203, t20304, t20405, t20501,
         t20601, t20702, t20803, t20904,
         t20607, t20708, t20809,         t21006 ]

-- Remove two adjacent arcs from one, non-adjacent from another

g104 :: GraphMem LabelMem
g104 = arcsToGraph
       [ t10102, t10203, t10304,
         t10106, t10207, t10308, t10409, t10510,
         t10607, t10708, t10809, t10910, t11006 ]

g204 :: GraphMem LabelMem
g204 = arcsToGraph
       [ t20102, t20203, t20304, t20405, t20501,
         t20601, t20702, t20803,
         t20607, t20708, t20809, t20910, t21006 ]

-- Compare two rings of 5 with one ring of 10
-- (each node double-connected, but different overall topology)

t10901 :: Statement
t10901 = arc v109 p109 v101

g105 :: GraphMem LabelMem
g105 = arcsToGraph
       [ t10102, t10203, t10304, t10405,
                                 t10901, t10510,
         t10607, t10708, t10809,         t11006 ]

g205 :: GraphMem LabelMem
g205 = arcsToGraph
       [ t20102, t20203, t20304, t20405, t20501,
         t20607, t20708, t20809, t20910, t21006 ]

-- Reverse one arc from test 01
-- (also, rearrange arcs to catch ordering artefacts)

t20201 :: Statement
t20201 = arc v202 p201 v201

g106 :: GraphMem LabelMem
g106 = arcsToGraph
       [ t10102, t10203, t10304, t10405, t10501,
         t10106, t10207, t10308, t10409, t10510,
         t10607, t10708, t10809, t10910, t11006 ]

g206 :: GraphMem LabelMem
g206 = arcsToGraph
       [ t20607, t20708, t20809, t20910, t21006,
         t20601, t20702, t20803, t20904, t21005,
         t20102, t20203, t20304, t20405, t20501 ]

g306 :: GraphMem LabelMem
g306 = arcsToGraph
       [ t20607, t20708, t20809, t20910, t21006,
         t20601, t20702, t20803, t20904, t21005,
         t20201, t20203, t20304, t20405, t20501 ]

-- Similar tests to 02,03,04,
-- but add identified property rather than removing arcs

f01, f02 :: LabelMem
f01  = LF "f01"
f02  = LF "f02"

-- Fix one arc from each

f10102, f10501, f21006, f20510 :: Statement
f10102 = arc v101 f01 v102
f10501 = arc v105 f01 v101
f21006 = arc v210 f01 v206
f20510 = arc v205 f01 v210

g107 :: GraphMem LabelMem
g107 = arcsToGraph
       [ f10102, t10203, t10304, t10405, t10501,
         t10106, t10207, t10308, t10409, t10510,
         t10607, t10708, t10809, t10910, t11006 ]

g207 :: GraphMem LabelMem
g207 = arcsToGraph
       [ t10102, t10203, t10304, t10405, f10501,
         t10106, t10207, t10308, t10409, t10510,
         t10607, t10708, t10809, t10910, t11006 ]

g307 :: GraphMem LabelMem
g307 = arcsToGraph
       [ t20607, t20708, t20809, t20910, f21006,
         t20601, t20702, t20803, t20904, t21005,
         t20102, t20203, t20304, t20405, t20501 ]

g407 :: GraphMem LabelMem
g407 = arcsToGraph
       [ t20607, t20708, t20809, t20910, t21006,
         t20601, t20702, t20803, t20904, t21005,
         t20102, t20203, t20304, t20405, t20501 ]

-- Fix two adjacent arcs from each

f10203, f10405, f20910, f20601 :: Statement
f10203 = arc v102 f01 v103
f10405 = arc v104 f01 v105
f20910 = arc v209 f01 v210
f20601 = arc v206 f01 v201

g108 :: GraphMem LabelMem
g108 = arcsToGraph
       [ f10102, f10203, t10304, t10405, t10501,
         t10106, t10207, t10308, t10409, t10510,
         t10607, t10708, t10809, t10910, t11006 ]

g208 :: GraphMem LabelMem
g208 = arcsToGraph
       [ t10102, t10203, t10304, f10405, f10501,
         t10106, t10207, t10308, t10409, t10510,
         t10607, t10708, t10809, t10910, t11006 ]

g308 :: GraphMem LabelMem
g308 = arcsToGraph
       [ t20607, t20708, t20809, f20910, f21006,
         t20601, t20702, t20803, t20904, t21005,
         t20102, t20203, t20304, t20405, t20501 ]

g408 :: GraphMem LabelMem
g408 = arcsToGraph
       [ t20607, t20708, t20809, t20910, f21006,
         f20601, t20702, t20803, t20904, t21005,
         t20102, t20203, t20304, t20405, t20501 ]

-- Fix two adjacent arcs with different properties

g10203, g10102, g10405 :: Statement
g10203 = arc v102 f02 v103
g10102 = arc v101 f02 v102
g10405 = arc v104 f02 v105

g109, g209, g309 :: GraphMem LabelMem
g109 = arcsToGraph
       [ f10102, g10203, t10304, t10405, t10501,
         t10106, t10207, t10308, t10409, t10510,
         t10607, t10708, t10809, t10910, t11006 ]

g209 = arcsToGraph
       [ g10102, t10203, t10304, t10405, f10501,
         t10106, t10207, t10308, t10409, t10510,
         t10607, t10708, t10809, t10910, t11006 ]

g309 = arcsToGraph
       [ t10102, t10203, t10304, g10405, f10501,
         t10106, t10207, t10308, t10409, t10510,
         t10607, t10708, t10809, t10910, t11006 ]

mgeq00 :: Test
mgeq00 = testGraphEq "g100-g200" True  g100 g200

mgeq0112, mgeq0113, mgeq0114, mgeq0115, mgeq0116, mgeq0156 :: Test
mgeq0112 = testGraphEq "g101-g201" True  g101 g201
mgeq0113 = testGraphEq "g101-g301" False g101 g301
mgeq0114 = testGraphEq "g101-g401" False g101 g401
mgeq0115 = testGraphEq "g101-g501" False g101 g501
mgeq0116 = testGraphEq "g101-g601" False g101 g601
mgeq0156 = testGraphEq "g501-g601" True  g501 g601

mgeq0212, mgeq0213 :: Test
mgeq0212 = testGraphEq "g102-g202" True  g102 g202
mgeq0213 = testGraphEq "g102-g302" False g102 g302

mgeq0312, mgeq0313 :: Test
mgeq0312 = testGraphEq "g103-g203" True  g103 g203
mgeq0313 = testGraphEq "g103-g303" False g103 g303

mgeq04, mgeq05 :: Test
mgeq04 = testGraphEq "g104-g204" False g104 g204
mgeq05 = testGraphEq "g105-g205" False g105 g205

mgeq0612, mgeq0613 :: Test
mgeq0612 = testGraphEq "g106-g206" True  g106 g206
mgeq0613 = testGraphEq "g106-g306" False g106 g306

mgeq0712, mgeq0713, mgeq0714 :: Test
mgeq0712 = testGraphEq "g107-g207" True  g107 g207
mgeq0713 = testGraphEq "g107-g307" True  g107 g307
mgeq0714 = testGraphEq "g107-g407" False g107 g407

mgeq0812, mgeq0813, mgeq0814 :: Test
mgeq0812 = testGraphEq "g108-g208" True  g108 g208
mgeq0813 = testGraphEq "g108-g308" True  g108 g308
mgeq0814 = testGraphEq "g108-g408" False g108 g408

mgeq0912, mgeq0913 :: Test
mgeq0912 = testGraphEq "g109-g209" True  g109 g209
mgeq0913 = testGraphEq "g109-g309" False g109 g309

testGraphEqSuiteMore :: Test
testGraphEqSuiteMore = TestList
  [ mgeq00
  , mgeq0112, mgeq0113, mgeq0114, mgeq0115, mgeq0116, mgeq0156
  , mgeq0212, mgeq0213
  , mgeq0312, mgeq0313
  , mgeq04
  , mgeq05
  , mgeq0612, mgeq0613
  , mgeq0712, mgeq0713, mgeq0714
  , mgeq0812, mgeq0813, mgeq0814
  , mgeq0912, mgeq0913
  ]

------------------------------------------------------------
-- All tests
------------------------------------------------------------

allTests :: [TF.Test]
allTests = 
  [ -- testSelectSuite
    -- testSubsetSuite
    conv "Lab" testLabSuite
  , conv "Graph" testGraphSuite
  , conv "LabelEq" testLabelEqSuite
  , conv "LabelOrd" testLabelOrdSuite
    -- silly test of Eq instance
  , TF.testCase "arc neq"
    (assertBool "arc neq" (Arc True True True /= Arc True True False))
  , conv "StmtEq" testStmtEqSuite
  , conv "LabelMap" testLabelMapSuite
  , conv "GraphMatchSupport" testGraphMatchSupportSuite
  , conv "GraphMatchStep" testGraphMatchStepSuite
  , conv "GraphEq Part" testGraphEqSuitePart
  , conv "GraphEq" testGraphEqSuite
  , conv "GraphEq More" testGraphEqSuiteMore
    -- test of Foldable instance of Arc
  , TF.testCase "foldArc" ([1::Int,2,4] @=? F.fold (Arc [1::Int] [2] [4]))
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
