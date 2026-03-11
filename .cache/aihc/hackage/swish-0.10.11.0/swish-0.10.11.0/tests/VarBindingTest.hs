{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  VarBindingTest
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2013, 2021 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  OverloadedStrings
--
--  This module contains test cases for variable binding values and
--  variable binding modifier values.
--
--------------------------------------------------------------------------------

module Main where

import qualified Data.Set as S

import qualified Test.Framework as TF

import Swish.VarBinding
    ( VarBinding(..)
    , subBinding, nullVarBinding, makeVarBinding
    , boundVars, subBinding, makeVarBinding
    , applyVarBinding, joinVarBindings
    , VarBindingModify(..)
    , vbmCompatibility, vbmCompose
    , findCompositions, findComposition
    , makeVarFilterModify
    , makeVarTestFilter
    , varBindingId, varFilterDisjunction, varFilterConjunction
    , varFilterEQ, varFilterNE
    )

import Swish.RDF.Vocabulary (swishName)

import Data.List (union, intersect)
import Data.Maybe (isJust, isNothing, fromJust)

import Test.HUnit (Test(TestList))
import TestHelpers ( conv, test
                    , testEq 
                    , testEqv, testEqv2, testHasEqv, testMaybeEqv
                    , testJust, testNothing  
                    )

------------------------------------------------------------
--  Define and variable bindings
------------------------------------------------------------

vb1 :: VarBinding Int String
vb1    = makeVarBinding [(1,"a"),(2,"b"),(3,"c")]

vb1str :: String
vb1str = "[(1,\"a\"),(2,\"b\"),(3,\"c\")]"

vb2 :: VarBinding Int String
vb2    = makeVarBinding [(3,"c"),(2,"b"),(1,"a")]

vb2str :: String
vb2str = vb1str

vb3 :: VarBinding Int String
vb3    = makeVarBinding [(1,"a"),(2,"b"),(3,"c"),(4,"d"),(5,"e")]

vb3str :: String
vb3str = "[(1,\"a\"),(2,\"b\"),(3,\"c\"),(4,\"d\"),(5,\"e\")]"

vb4 :: VarBinding Int String
vb4 = nullVarBinding

vb4str :: String
vb4str = "[]"

vb5 :: VarBinding Int Int
vb5 = makeVarBinding [(1,11),(2,22),(3,33)]

vb6 :: VarBinding Int String
vb6 = makeVarBinding [(3,"cc"),(4,"dd"),(5,"ee")]

vb12, vb13, vb14, vb16, vb21, vb44 :: VarBinding Int String
vb12 = joinVarBindings vb1 vb2
vb13 = joinVarBindings vb1 vb3
vb14 = joinVarBindings vb1 vb4
vb16 = joinVarBindings vb1 vb6
vb21 = joinVarBindings vb2 vb1
vb44 = joinVarBindings vb4 vb4

vb12str, vb13str, vb14str, vb16str, vb21str, vb44str :: String
vb12str = vb1str
vb13str = vb3str
vb14str = vb1str
vb16str = "[(1,\"a\"),(2,\"b\"),(3,\"c\"),(4,\"dd\"),(5,\"ee\")]"
vb21str = vb2str
vb44str = vb4str

vbFull :: VarBinding a b -> Bool
vbFull = not . vbNull

testVarBindingSuite :: Test
testVarBindingSuite = 
  TestList
  [ test   "testVarBinding01" (vb1==vb2)
  , test   "testVarBinding02" (vb1/=vb3)
  , test   "testVarBinding03" (vb1/=vb4)
  , testEq "testVarBinding04" vb1str  $ show vb1
  , testEq "testVarBinding05" vb2str  $ show vb2
  , testEq "testVarBinding06" vb4str  $ show vb4
  , testEq "testVarBinding10" (S.fromList [1,2,3]) $ boundVars vb1
  , testEq "testVarBinding11" (S.fromList [3,2,1]) $ boundVars vb2
  , testEq "testVarBinding12" S.empty              $ boundVars vb4
  , test   "testVarBinding20" (subBinding vb1 vb2)
  , test   "testVarBinding21" (subBinding vb1 vb3)
  , test   "testVarBinding22" $ not (subBinding vb1 vb4)
  , test   "testVarBinding23" (subBinding vb2 vb1)
  , test   "testVarBinding24" $ not (subBinding vb3 vb1)
  , test   "testVarBinding25" (subBinding vb4 vb1)
  , test   "testVarBinding26" (subBinding vb4 vb4)
  , testEq "testVarBinding30"  0 $ applyVarBinding vb5 0
  , testEq "testVarBinding31" 11 $ applyVarBinding vb5 1
  , testEq "testVarBinding32" 22 $ applyVarBinding vb5 2
  , testEq "testVarBinding33" 33 $ applyVarBinding vb5 3
  , testEq "testVarBinding34"  4 $ applyVarBinding vb5 4
  , testEq "testVarBinding35" 11 $ applyVarBinding vb5 11
  , test   "testVarBinding40" (vbFull vb12)
  , test   "testVarBinding41" (vbFull vb13)
  , test   "testVarBinding42" (vbFull vb14)
  , test   "testVarBinding43" (vbFull vb16)
  , test   "testVarBinding44" (vbFull vb21)
  , test   "testVarBinding45" (vbNull vb44)
  , test   "testVarBinding50" (subBinding vb12 vb13)
  , test   "testVarBinding51" (subBinding vb12 vb14)
  , test   "testVarBinding52" (subBinding vb12 vb16)
  , test   "testVarBinding53" (subBinding vb12 vb21)
  , test   "testVarBinding54" $ not (subBinding vb12 vb44)
  , test   "testVarBinding55" $ not (subBinding vb13 vb12)
  , test   "testVarBinding56" (subBinding vb14 vb12)
  , test   "testVarBinding57" (subBinding vb44 vb12)
  , test   "testVarBinding58" $ not (subBinding vb16 vb12)
  , testEq "testVarBinding60" vb12str $ show vb12
  , testEq "testVarBinding61" vb13str $ show vb13
  , testEq "testVarBinding62" vb14str $ show vb14
  , testEq "testVarBinding63" vb16str $ show vb16
  , testEq "testVarBinding64" vb21str $ show vb21
  , testEq "testVarBinding65" vb44str $ show vb44
  , testEq "testVarBinding70" (Just "a")  $ vbMap vb16 1
  , testEq "testVarBinding71" (Just "c")  $ vbMap vb16 3
  , testEq "testVarBinding72" (Just "ee") $ vbMap vb16 5
  , testEq "testVarBinding73" Nothing     $ vbMap vb16 7
  ]

------------------------------------------------------------
--  Variable binding modifier tests
------------------------------------------------------------

vb1m :: VarBinding String Int
vb1m    = makeVarBinding [("a",1)]

vb2m :: VarBinding String Int
vb2m    = makeVarBinding [("a",1),("b",2)]

vb3m :: VarBinding String Int
vb3m    = makeVarBinding [("a",1),("c",3)]

vb4m :: VarBinding String Int
vb4m    = makeVarBinding [("b",2),("c",3)]

vb5m :: VarBinding String Int
vb5m    = makeVarBinding [("a",1),("b",2),("c",3)]

vb6m :: VarBinding String Int
vb6m    = makeVarBinding [("a",1),("b",2),("c",4)]

vb9m :: VarBinding String Int
vb9m    = makeVarBinding [("i",9)]

-- Add new bindings per vb9m
vbm1 :: VarBindingModify String Int
vbm1 = 
    let vcb = S.toList (boundVars vb9m)
    in VarBindingModify
           { vbmName  = swishName "vbm1"
           , vbmApply = map (`joinVarBindings` vb9m)
           , vbmVocab = vcb
           , vbmUsage = [vcb]
           }


-- Avoid pattern-match errors from ghc 9.2
get1 :: [a] -> a
get1 [x] = x
get1 _ = error "Expected single-element list"


vb1m1, vb2m1 :: VarBinding String Int
vb1m1 = get1 $ vbmApply vbm1 [vb1m]
vb2m1 = get1 $ vbmApply vbm1 [vb2m]

-- Filter for bindings that define a
vbm2 :: VarBindingModify String Int
vbm2 = VarBindingModify
    { vbmName  = swishName "vbm2"
    , vbmApply = filter (\vb -> isJust $ vbMap vb "a")
    , vbmVocab = ["a"]
    , vbmUsage = [[]]
    }

vb12m2 :: [VarBinding String Int]
vb12m2 = vbmApply vbm2 [vb1m,vb2m,vb9m]

-- Filter or add bindings so that a+b=c
vbm3 :: VarBindingModify String Int
vbm3 = VarBindingModify
    { vbmName  = swishName "vbm3"
    , vbmApply = sumBinding "a" "b" "c"
    , vbmVocab = ["a","b","c"]
    , vbmUsage = [[],["a"],["b"],["c"]]
    }

sumBinding :: String -> String -> String -> [VarBinding String Int]
    -> [VarBinding String Int]
sumBinding va vb vc = concatMap abSumc
    where
        abSumc :: VarBinding String Int -> [VarBinding String Int]
        abSumc vbind =
            abSumc1 (vbMap vbind va) (vbMap vbind vb) (vbMap vbind vc) vbind
        abSumc1 (Just a) (Just b) (Just c) vbind
            | (a+b) == c = [vbind]
            | otherwise  = []
        abSumc1 (Just a) (Just b) Nothing vbind  =
            [ joinVarBindings vbind  $ makeVarBinding [(vc,a+b)] ]
        abSumc1 (Just a) Nothing (Just c) vbind  =
            [ joinVarBindings vbind  $ makeVarBinding [(vb,c-a)] ]
        abSumc1 Nothing (Just b) (Just c) vbind  =
            [ joinVarBindings vbind  $ makeVarBinding [(va,c-b)] ]
        abSumc1 _ _ _ _ = []

vb16m3 :: [VarBinding String Int]
vb16m3 = vbmApply vbm3 [vb1m,vb2m,vb3m,vb4m,vb5m,vb6m]

testVarModifySuite :: Test
testVarModifySuite = 
  TestList
  [ testEq "testVarModifyName01"
      (swishName "vbm1") $ vbmName vbm1 
  , testEq "testVarModify01" (Just 1) $ vbMap vb1m1 "a"
  , testEq "testVarModify02" Nothing  $ vbMap vb1m1 "b"
  , testEq "testVarModify03" Nothing  $ vbMap vb2m1 "c"
  , testEq "testVarModify04" (Just 9) $ vbMap vb1m1 "i"
  , testEq "testVarModify05" (Just 1) $ vbMap vb2m1 "a"
  , testEq "testVarModify06" (Just 2) $ vbMap vb2m1 "b"
  , testEq "testVarModify07" Nothing  $ vbMap vb2m1 "c"
  , testEq "testVarModify08" (Just 9) $ vbMap vb2m1 "i"
  , testEq "testVarModify10" (Just ["i"]) $
      vbmCompatibility vbm1 ["a","b"]
  , testEq "testVarModify11" Nothing $
      vbmCompatibility vbm1 ["a","b","i"]
  , testEq "testVarModifyName02"
      (swishName "vbm2") $ vbmName vbm2
  , testEq "testVarModify20" 2 $ length vb12m2
  , testEq "testVarModify21" vb1m $ head vb12m2
  , testEq "testVarModify22" vb2m $ vb12m2!!1
  , testEq "testVarModify23" (Just []) $
      vbmCompatibility vbm2 ["a","b"]
  , testEq "testVarModify24" (Just []) $
      vbmCompatibility vbm2 ["a","b"]
  , testEq "testVarModify25" (Just []) $
      vbmCompatibility vbm2 ["a","b","i"]
  , testEq "testVarModify26" Nothing $
      vbmCompatibility vbm2 ["i"]
  , testEq "testVarModifyName03"
      (swishName "vbm3") $ vbmName vbm3
  , testEq "testVarModify30" 4 $ length vb16m3
  , testEq "testVarModify31" vb5m (head vb16m3)
  , testEq "testVarModify32" vb5m (vb16m3!!1)
  , testEq "testVarModify33" vb5m (vb16m3!!2)
  , testEq "testVarModify34" vb5m (vb16m3!!3)
  , testEq "testVarModify35" (Just ["c"]) $
      vbmCompatibility vbm3 ["a","b"]
  , testEq "testVarModify36" (Just ["b"]) $
      vbmCompatibility vbm3 ["a","c"]
  , testEq "testVarModify37" (Just ["a"]) $
      vbmCompatibility vbm3 ["b","c","i"]
  , testEq "testVarModify38" (Just []) $
      vbmCompatibility vbm3 ["i","c","a","b"]
  , testEq "testVarModify39" Nothing $
      vbmCompatibility vbm3 ["i","a"]
  , testEq "testVarModify40" Nothing $
      vbmCompatibility vbm3 ["i","b"]
  , testEq "testVarModify41" Nothing $
      vbmCompatibility vbm3 ["i","c"]
  , testEq "testVarModify42" Nothing $
      vbmCompatibility vbm3 ["i","d"]
  ]

------------------------------------------------------------
--  Variable binding modifier composition tests
------------------------------------------------------------

--  Given (1) a+b=c and (2) a+c=d, then:
--    a=1 b=2   =>   c=3 d=4   by (1) then (2)
--    a=1 c=3   =>   b=2 d=4   by (1) then (2) or (2) then (1)
--    a=1 d=4   =>   b=2 c=3   by (2) then (1)
--    b=2 c=3   =>   a=1 d=4   by (1) then (2)
--    b=2 d=4   =>   insufficient data
--    c=3 d=4   =>   a=1 b=2   by (2) then (1)


-- Filter or add bindings so that a+b=c
vbm4 :: VarBindingModify String Int
vbm4 = VarBindingModify
    { vbmName  = swishName "vbm4"
    , vbmApply = sumBinding "a" "c" "d"
    , vbmVocab = ["a","c","d"]
    , vbmUsage = [[],["a"],["c"],["d"]]
    }

getJust :: Maybe x -> x
getJust (Just x) = x
getJust _ = error "Expected something, got Nothing"


vbm34, vbm43 :: VarBindingModify String Int
vbm34 = getJust $ vbmCompose vbm3 vbm4
vbm43 = getJust $ vbmCompose vbm4 vbm3

vbm34vocab, vbm43vocab :: [String]
vbm34vocab = [ "a", "b", "c", "d"]
vbm43vocab = [ "a", "b", "c", "d"]

vbm34usage, vbm43usage :: [[String]]
vbm34usage = [ ["a","d"], ["b","d"], ["c","d"]
             , ["a"], ["b"], ["c"], ["d"], []
             ]
vbm43usage = [ ["a","b"], ["b","c"], ["b","d"]
             , ["a"], ["b"], ["c"], ["d"], []
             ]

vbab :: VarBinding String Int
vbab    = makeVarBinding [("a",1),("b",2)]

vbac :: VarBinding String Int
vbac    = makeVarBinding [("a",1),("c",3)]

vbad :: VarBinding String Int
vbad    = makeVarBinding [("a",1),("d",4)]

vbbc :: VarBinding String Int
vbbc    = makeVarBinding [("b",2),("c",3)]

vbbd :: VarBinding String Int
vbbd    = makeVarBinding [("b",2),("d",4)]

vbcd :: VarBinding String Int
vbcd    = makeVarBinding [("c",3),("d",4)]

vbabcd :: VarBinding String Int
vbabcd    = makeVarBinding [("a",1),("b",2),("c",3),("d",4)]

-- [[[need test for incompatible composition]]] --
--  Three ways to be incompatible:
--  (a) both modifers define same new output
--  (b) output from second modifier is input to first modifier

vbm5 :: VarBindingModify String Int
vbm5 = VarBindingModify
    { vbmName  = swishName "vbm5"
    , vbmApply = id                 -- incorrect: dummy for testing only
    , vbmVocab = ["a","b","c"]
    , vbmUsage = [["a"],["b"]]
    }

vbm6 :: VarBindingModify String Int
vbm6 = VarBindingModify
    { vbmName  = swishName "vbm6"
    , vbmApply = id                 -- incorrect: dummy for testing only
    , vbmVocab = ["a","b","c"]
    , vbmUsage = [["a","b"],["b","c"],["a","c"]]
    }

vbm7 :: VarBindingModify String Int
vbm7 = VarBindingModify
    { vbmName  = swishName "vbm7"
    , vbmApply = id                 -- incorrect: dummy for testing only
    , vbmVocab = ["a","b","c"]
    , vbmUsage = [["a"]]
    }

vbm8 :: VarBindingModify String Int
vbm8 = VarBindingModify
    { vbmName  = swishName "vbm8"
    , vbmApply = id                 -- incorrect: dummy for testing only
    , vbmVocab = ["b","c","d"]
    , vbmUsage = [["b"],["c"],["b","c"]]
    }

vbm56, vbm65, vbm78, vbm87 ::  Maybe (VarBindingModify String Int)

vbm56 = vbmCompose vbm5 vbm6
vbm65 = vbmCompose vbm6 vbm5
vbm78 = vbmCompose vbm7 vbm8
vbm87 = vbmCompose vbm8 vbm7

vbm87usage :: [[String]]
vbm87usage = [["a","b"],["a","c"],["a","b","c"]]

jvbm1id, jvbmid1 :: Maybe (VarBindingModify String Int)
jvbm1id    = vbmCompose vbm1 varBindingId
jvbmid1    = vbmCompose varBindingId vbm1

vb1m1id, vb2m1id, vb1mid1, vb2mid1 :: VarBinding String Int
vb1m1id = get1 $ vbmApply (fromJust jvbm1id) [vb1m]
vb2m1id = get1 $ vbmApply (fromJust jvbm1id) [vb2m]

vb1mid1 = get1 $ vbmApply (fromJust jvbmid1) [vb1m]
vb2mid1 = get1 $ vbmApply (fromJust jvbmid1) [vb2m]

testVarComposeSuite :: Test
testVarComposeSuite = 
  TestList
  [ testEq "testVarModifyName04" (swishName "vbm4") $ vbmName vbm4
  , testEq "testVarModifyName05" (swishName "_vbm4_vbm3_") $ vbmName vbm43
  , testEq "testVarModifyName06" (swishName "_vbm3_vbm4_") $ vbmName vbm34
  , testEq "testVarModifyName07" (swishName "_vbm1_varBindingId_") $
                        vbmName (fromJust jvbm1id)
  , testEq "testVarModifyName08" (swishName "_varBindingId_vbm1_") $
                        vbmName (fromJust jvbmid1)

  , testEqv "testVarCompose01" vbm34vocab $ vbmVocab vbm34
  , testEqv2 "testVarCompose02" vbm34usage $ vbmUsage vbm34
  , testMaybeEqv "testVarCompose03" (Just ["c","d"]) $
     vbmCompatibility vbm34 ["a","b"]
  , testMaybeEqv "testVarCompose04" (Just ["b","d"]) $
     vbmCompatibility vbm34 ["a","c"]
  , testMaybeEqv "testVarCompose05" Nothing $
     vbmCompatibility vbm34 ["a","d"]
  , testMaybeEqv "testVarCompose06" (Just ["a","d"]) $
     vbmCompatibility vbm34 ["b","c"]
  , testMaybeEqv "testVarCompose07" Nothing $
     vbmCompatibility vbm34 ["b","d"]
  , testMaybeEqv "testVarCompose08" Nothing $
     vbmCompatibility vbm34 ["c","d"]
  , testMaybeEqv "testVarCompose09" (Just ["a"]) $
     vbmCompatibility vbm34 ["b","c","d"]
  , testMaybeEqv "testVarCompose10" (Just ["b"]) $
     vbmCompatibility vbm34 ["a","c","d"]
  , testMaybeEqv "testVarCompose11" (Just ["c"]) $
     vbmCompatibility vbm34 ["a","b","d"]
  , testMaybeEqv "testVarCompose12" (Just ["d"]) $
     vbmCompatibility vbm34 ["a","b","c"]
  , testMaybeEqv "testVarCompose13" (Just []) $
     vbmCompatibility vbm34 ["a","b","c","d"]
  , testEqv "testVarCompose14" [vbabcd,vbabcd,vbabcd] $
     vbmApply vbm34 [vbab,vbac,vbbc]
  , testEqv "testVarCompose15" [] $
     vbmApply vbm34 [vbad,vbbd,vbcd]

  , testEqv "testVarCompose21" vbm43vocab $ vbmVocab vbm43
  , testEqv2 "testVarCompose22" vbm43usage $ vbmUsage vbm43
  , testMaybeEqv "testVarCompose23" Nothing $
     vbmCompatibility vbm43 ["a","b"]
  , testMaybeEqv "testVarCompose24" (Just ["b","d"]) $
     vbmCompatibility vbm43 ["a","c"]
  , testMaybeEqv "testVarCompose25" (Just ["b","c"]) $
     vbmCompatibility vbm43 ["a","d"]
  , testMaybeEqv "testVarCompose26" Nothing $
     vbmCompatibility vbm43 ["b","c"]
  , testMaybeEqv "testVarCompose27" Nothing $
     vbmCompatibility vbm43 ["b","d"]
  , testMaybeEqv "testVarCompose28" (Just ["a","b"]) $
     vbmCompatibility vbm43 ["c","d"]
  , testMaybeEqv "testVarCompose29" (Just ["a"]) $
     vbmCompatibility vbm43 ["b","c","d"]
  , testMaybeEqv "testVarCompose30" (Just ["b"]) $
     vbmCompatibility vbm43 ["a","c","d"]
  , testMaybeEqv "testVarCompose31" (Just ["c"]) $
     vbmCompatibility vbm43 ["a","b","d"]
  , testMaybeEqv "testVarCompose32" (Just ["d"]) $
     vbmCompatibility vbm43 ["a","b","c"]
  , testMaybeEqv "testVarCompose33" (Just []) $
     vbmCompatibility vbm43 ["a","b","c","d"]
  , testEqv "testVarCompose34" [] $
     vbmApply vbm43 [vbab,vbbc,vbbd]
  , testEqv "testVarCompose35" [vbabcd,vbabcd,vbabcd] $
     vbmApply vbm43 [vbac,vbad,vbcd]
    
  , test   "testVarCompose41" (isNothing vbm56)
  , test   "testVarCompose42" (isNothing vbm65)
  , test   "testVarCompose43" (isNothing vbm78)
  , test   "testVarCompose44" (isJust    vbm87)
  , testEqv2 "testVarCompose45" vbm87usage $
     vbmUsage (fromJust vbm87)

  , test   "testVarCompose51" $ isJust jvbm1id
  , test   "testVarCompose52" $ isJust jvbmid1

  , testEq "testVarCompose61" (Just 1) $ vbMap vb1m1id "a"
  , testEq "testVarCompose62" Nothing  $ vbMap vb1m1id "b"
  , testEq "testVarCompose63" Nothing  $ vbMap vb2m1id "c"
  , testEq "testVarCompose64" (Just 9) $ vbMap vb1m1id "i"
  , testEq "testVarCompose65" (Just 1) $ vbMap vb2m1id "a"
  , testEq "testVarCompose66" (Just 2) $ vbMap vb2m1id "b"
  , testEq "testVarCompose67" Nothing  $ vbMap vb2m1id "c"
  , testEq "testVarCompose68" (Just 9) $ vbMap vb2m1id "i"

  , testEq "testVarCompose71" (Just 1) $ vbMap vb1mid1 "a"
  , testEq "testVarCompose72" Nothing  $ vbMap vb1mid1 "b"
  , testEq "testVarCompose73" Nothing  $ vbMap vb2mid1 "c"
  , testEq "testVarCompose74" (Just 9) $ vbMap vb1mid1 "i"
  , testEq "testVarCompose75" (Just 1) $ vbMap vb2mid1 "a"
  , testEq "testVarCompose76" (Just 2) $ vbMap vb2mid1 "b"
  , testEq "testVarCompose77" Nothing  $ vbMap vb2mid1 "c"
  , testEq "testVarCompose78" (Just 9) $ vbMap vb2mid1 "i"

  ] 

------------------------------------------------------------
--  Modifier composition discovery tests
------------------------------------------------------------

--  vbm3: a+b=c (1)
--  vbm4: a+c=d (2)
--  vbm9: c+d=e (3)
--
--  a,b -> c,d,e  by (1,2,3)
--  a,c -> b,d,e  by (1,2,3)
--         d,b,e  by (2,1,3)
--         d,e,b  by (2,3,1)
--  a,d -> c,b,e  by (2,1,3)
--         c,e,b  by (2,3,1)
--  a,e -> None
--  b,c -> a,d,e  by (1,2,3)
--  b,d -> None
--  b,e -> None
--  c,d -> a,b,e  by (2,1,3)
--      -> a,e,a  by (2,3,1)
--      -> e,a,b  by (3,2,1)
--  c,e -> d,a,b  by (3,2,1)
--  d,e -> c,a,b  by (3,2,1)

vbm9 :: VarBindingModify String Int
vbm9 = VarBindingModify
    { vbmName  = swishName "vbm9"
    , vbmApply = sumBinding "c" "d" "e"
    , vbmVocab = ["c","d","e"]
    , vbmUsage = [[],["c"],["d"],["e"]]
    }

compab, compac, compad, compae,
  compba, compbc, compbd, compbe,
  compca, compcd, compce, compde :: [VarBindingModify String Int]
  
compab = findCompositions [vbm3,vbm4,vbm9] ["a","b"]    -- 1
compac = findCompositions [vbm3,vbm4,vbm9] ["a","c"]    -- 3
compad = findCompositions [vbm3,vbm4,vbm9] ["a","d"]    -- 2
compae = findCompositions [vbm3,vbm4,vbm9] ["a","e"]    -- 0
compba = findCompositions [vbm3,vbm4,vbm9] ["b","a"]    -- 1
compbc = findCompositions [vbm3,vbm4,vbm9] ["b","c"]    -- 1
compbd = findCompositions [vbm3,vbm4,vbm9] ["b","d"]    -- 0
compbe = findCompositions [vbm3,vbm4,vbm9] ["b","e"]    -- 0
compca = findCompositions [vbm3,vbm4,vbm9] ["c","a"]    -- 3
compcd = findCompositions [vbm3,vbm4,vbm9] ["c","d"]    -- 3
compce = findCompositions [vbm3,vbm4,vbm9] ["c","e"]    -- 1
compde = findCompositions [vbm3,vbm4,vbm9] ["d","e"]    -- 1

compvocab :: [String]
compvocab = ["a","b","c","d","e"]

compBindings :: [VarBinding String Int]
compBindings = map makeVarBinding
    [ [ ("a",1), ("b",2) ]
    , [ ("a",1), ("c",3) ]
    , [ ("a",1), ("d",4) ]
    , [ ("a",1), ("e",7) ]
    , [ ("b",2), ("c",3) ]
    , [ ("b",2), ("d",4) ]
    , [ ("b",2), ("e",7) ]
    , [ ("c",3), ("d",4) ]
    , [ ("c",3), ("e",7) ]
    , [ ("d",4), ("e",7) ]
    ]

compResult :: [VarBinding String Int]
compResult = map makeVarBinding
    [ [ ("a",1), ("b",2), ("c",3), ("d",4), ("e",7) ] ]

compApply :: [VarBindingModify String Int] -> [VarBinding String Int]
compApply vbms = vbmApply (head vbms) compBindings

jcompab, jcompac, jcompad, jcompae,
  jcompba, jcompbc, jcompbd, jcompbe,
  jcompca, jcompcd, jcompce, jcompde :: Maybe (VarBindingModify String Int)

jcompab = findComposition [vbm3,vbm4,vbm9] ["a","b"]    -- 1
jcompac = findComposition [vbm3,vbm4,vbm9] ["a","c"]    -- 3
jcompad = findComposition [vbm3,vbm4,vbm9] ["a","d"]    -- 1
jcompae = findComposition [vbm3,vbm4,vbm9] ["a","e"]    -- 0
jcompba = findComposition [vbm3,vbm4,vbm9] ["b","a"]    -- 1
jcompbc = findComposition [vbm3,vbm4,vbm9] ["b","c"]    -- 1
jcompbd = findComposition [vbm3,vbm4,vbm9] ["b","d"]    -- 0
jcompbe = findComposition [vbm3,vbm4,vbm9] ["b","e"]    -- 0
jcompca = findComposition [vbm3,vbm4,vbm9] ["c","a"]    -- 3
jcompcd = findComposition [vbm3,vbm4,vbm9] ["c","d"]    -- 3
jcompce = findComposition [vbm3,vbm4,vbm9] ["c","e"]    -- 1
jcompde = findComposition [vbm3,vbm4,vbm9] ["d","e"]    -- 1

testFindCompSuite :: Test
testFindCompSuite = 
  TestList
  [ testEq "testVarModifyName08" (swishName "__vbm4_vbm3__vbm9_") $
                        vbmName (head compad)
  , testEq "testVarModifyName08" (swishName "__vbm4_vbm9__vbm3_") $
                        vbmName (compad!!1)
 
  , testEq "testFindComp01" 1 (length compab)
  , testEq "testFindComp02" 3 (length compac)
  , testEq "testFindComp03" 2 (length compad)
  , testEq "testFindComp04" 0 (length compae)
  , testEq "testFindComp05" 1 (length compba)
  , testEq "testFindComp06" 1 (length compbc)
  , testEq "testFindComp07" 0 (length compbd)
  , testEq "testFindComp08" 0 (length compbe)
  , testEq "testFindComp09" 3 (length compca)
  , testEq "testFindComp10" 3 (length compcd)
  , testEq "testFindComp11" 1 (length compce)
  , testEq "testFindComp12" 1 (length compde)
    
  , testEqv "testFindComp21" compvocab $ vbmVocab (head compab)
  , testEqv "testFindComp22" compvocab $ vbmVocab (head compac)
  , testEqv "testFindComp23" compvocab $ vbmVocab (head compad)
  , testEqv "testFindComp24" compvocab $ vbmVocab (head compba)
  , testEqv "testFindComp25" compvocab $ vbmVocab (head compbc)
  , testEqv "testFindComp26" compvocab $ vbmVocab (head compca)
  , testEqv "testFindComp27" compvocab $ vbmVocab (head compcd)
  , testEqv "testFindComp28" compvocab $ vbmVocab (head compce)
  , testEqv "testFindComp29" compvocab $ vbmVocab (head compde)

  , testHasEqv "testFindComp31" ["c","d","e"] $ vbmUsage (head compab)
  , testHasEqv "testFindComp32" ["b","d","e"] $ vbmUsage (head compac)
  , testHasEqv "testFindComp33" ["b","c","e"] $ vbmUsage (head compad)
  , testHasEqv "testFindComp34" ["c","d","e"] $ vbmUsage (head compba)
  , testHasEqv "testFindComp35" ["a","d","e"] $ vbmUsage (head compbc)
  , testHasEqv "testFindComp36" ["b","d","e"] $ vbmUsage (head compca)
  , testHasEqv "testFindComp37" ["a","b","e"] $ vbmUsage (head compcd)
  , testHasEqv "testFindComp38" ["a","b","d"] $ vbmUsage (head compce)
  , testHasEqv "testFindComp39" ["a","b","c"] $ vbmUsage (head compde)

  , testEqv "testFindComp41" compResult (compApply compab)
  , testEqv "testFindComp42" compResult (compApply compac)
  , testEqv "testFindComp43" compResult (compApply compad)
  , testEqv "testFindComp44" compResult (compApply compba)
  , testEqv "testFindComp45" compResult (compApply compbc)
  , testEqv "testFindComp46" compResult (compApply compca)
  , testEqv "testFindComp47" compResult (compApply compcd)
  , testEqv "testFindComp48" compResult (compApply compce)
  , testEqv "testFindComp49" compResult (compApply compde)
    
  , testJust    "testFindComp51" jcompab
  , testJust    "testFindComp52" jcompac
  , testJust    "testFindComp53" jcompad
  , testNothing "testFindComp54" jcompae
  , testJust    "testFindComp55" jcompba
  , testJust    "testFindComp56" jcompbc
  , testNothing "testFindComp57" jcompbd
  , testNothing "testFindComp58" jcompbe
  , testJust    "testFindComp59" jcompca
  , testJust    "testFindComp60" jcompcd
  , testJust    "testFindComp61" jcompce
  , testJust    "testFindComp62" jcompde
  ]

------------------------------------------------------------
--  Variable binding filters
------------------------------------------------------------

testFilterBindings :: [VarBinding String Int]
testFilterBindings = map makeVarBinding
    [ [ ("a",0), ("b",2), ("c",2) ]
    , [ ("a",0), ("b",2), ("c",3) ]
    , [ ("a",1), ("b",2), ("c",2) ]
    , [ ("a",1), ("b",2), ("c",3) ]
    , [ ("a",1), ("b",2), ("c",0) ]
    , [ ("a",0), ("b",2), ("c",0) ]
    , [ ("a",4), ("b",2), ("c",4) ]
    , [ ("x",4), ("y",2), ("z",4) ]
    ]

filtertesta0 :: VarBindingModify String Int
filtertesta0 = makeVarFilterModify $
        makeVarTestFilter (swishName "filtertesta0") (==0) "a"
vba0 :: [VarBinding String Int]
vba0 = map makeVarBinding
    [ [ ("a",0), ("b",2), ("c",2) ]
    , [ ("a",0), ("b",2), ("c",3) ]
    , [ ("a",0), ("b",2), ("c",0) ]
    ]

filtertestc0 :: VarBindingModify String Int
filtertestc0 = makeVarFilterModify $
        makeVarTestFilter (swishName "filtertestc0") (==0) "c"
vbc0 :: [VarBinding String Int]
vbc0 = map makeVarBinding
    [ [ ("a",1), ("b",2), ("c",0) ]
    , [ ("a",0), ("b",2), ("c",0) ]
    ]

filtercompabeq :: VarBindingModify String Int
filtercompabeq = makeVarFilterModify $ varFilterEQ "a" "b"
vbabeq :: [VarBinding String Int]
vbabeq = map makeVarBinding
    [ ]

filtercompaceq :: VarBindingModify String Int
filtercompaceq = makeVarFilterModify $ varFilterEQ "a" "c"
vbaceq :: [VarBinding String Int]
vbaceq = map makeVarBinding
    [ [ ("a",0), ("b",2), ("c",0) ]
    , [ ("a",4), ("b",2), ("c",4) ]
    ]

filtercompbceq :: VarBindingModify String Int
filtercompbceq = makeVarFilterModify $ varFilterEQ "b" "c"
vbbceq :: [VarBinding String Int]
vbbceq = map makeVarBinding
    [ [ ("a",0), ("b",2), ("c",2) ]
    , [ ("a",1), ("b",2), ("c",2) ]
    ]

filtercompbcne :: VarBindingModify String Int
filtercompbcne = makeVarFilterModify $ varFilterNE "b" "c"
vbbcne :: [VarBinding String Int]
vbbcne = map makeVarBinding
    [ [ ("a",0), ("b",2), ("c",3) ]
    , [ ("a",1), ("b",2), ("c",3) ]
    , [ ("a",1), ("b",2), ("c",0) ]
    , [ ("a",0), ("b",2), ("c",0) ]
    , [ ("a",4), ("b",2), ("c",4) ]
    ]

filterdisjunct :: VarBindingModify String Int
filterdisjunct = makeVarFilterModify $
                 varFilterDisjunction
                    [ makeVarTestFilter (swishName "isZero") (==0) "a"
                    , varFilterEQ "a" "c"]

filterconjunct :: VarBindingModify String Int
filterconjunct = makeVarFilterModify $
                 varFilterConjunction
                    [ makeVarTestFilter (swishName "isZero") (==0) "a"
                    , varFilterEQ "a" "c"]
                    
                    
vbdisj, vbconj :: [VarBinding String Int]
vbdisj = vbaceq `union` vba0
vbconj = vbaceq `intersect` vba0

testFilterSuite :: Test
testFilterSuite = 
  TestList
  [ testEq "testFilterName01" (swishName "filtertesta0") $
      vbmName filtertesta0
  , testEq "testFilterName02" (swishName "filtertestc0") $
     vbmName filtertestc0
  , testEq "testFilterName03" (swishName "varFilterEQ") $
     vbmName filtercompabeq
  , testEq "testFilterName04" (swishName "varFilterNE") $
     vbmName filtercompbcne
  , testEq "testFilterName05" (swishName "varFilterDisjunction") $
     vbmName filterdisjunct
  , testEq "testFilterName06" (swishName "varFilterConjunction") $
     vbmName filterconjunct
    
  , testEqv "testFilter01" vba0   $ vbmApply filtertesta0   testFilterBindings
  , testEqv "testFilter02" vbc0   $ vbmApply filtertestc0   testFilterBindings
  , testEqv "testFilter03" vbabeq $ vbmApply filtercompabeq testFilterBindings
  , testEqv "testFilter04" vbaceq $ vbmApply filtercompaceq testFilterBindings
  , testEqv "testFilter05" vbbceq $ vbmApply filtercompbceq testFilterBindings
  , testEqv "testFilter06" vbbcne $ vbmApply filtercompbcne testFilterBindings
  , testEqv "testFilter07" vbdisj $ vbmApply filterdisjunct testFilterBindings
  , testEqv "testFilter08" vbconj $ vbmApply filterconjunct testFilterBindings

  , testEqv "testFilter10" testFilterBindings $
                vbmApply varBindingId testFilterBindings

  ]

------------------------------------------------------------
--  All tests
------------------------------------------------------------

allTests :: [TF.Test]
allTests = 
  [ conv "VarBinding" testVarBindingSuite
  , conv "VarModify" testVarModifySuite
  , conv "VarCompose" testVarComposeSuite
  , conv "FindComp" testFindCompSuite
  , conv "Filter" testFilterSuite
  ]

main :: IO ()
main = TF.defaultMain allTests

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--      2011, 2013, 2021 Douglas Burke
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
