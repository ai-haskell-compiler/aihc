{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  QNameTest
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2013, 2014 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  OverloadedStrings
--
--  This module defines test cases for QName data. It also throws in a few
--  tests for the Namespace module.
--
--------------------------------------------------------------------------------

module Main where

import qualified Data.Text as T

import qualified Test.Framework as TF

import Swish.Namespace (makeQNameScopedName, getQName, getScopedNameURI)
import Swish.QName
    ( QName
    , LName
    , newQName
    , qnameFromURI
    , getNamespace
    , getLocalName
    , getQNameURI
    , getLName
    )

import Data.Maybe (fromJust)

import Network.URI (URI, parseURIReference)

import Test.HUnit (Test(TestList))

import TestHelpers (conv, testCompare, testCompareEq)

------------------------------------------------------------
--  Define some common values
------------------------------------------------------------

toURI :: String -> URI
toURI = fromJust . parseURIReference

base1, base2, base3, base4, base5, base6, base7 :: URI
base1  = toURI "http://id.ninebynine.org/wip/2003/test/graph1/node#"
base2  = toURI "http://id.ninebynine.org/wip/2003/test/graph2/node/"
base3  = toURI "http://id.ninebynine.org/wip/2003/test/graph3/node"
base4  = toURI "http://id.ninebynine.org/wip/2003/test/graph3/nodebase"
base5  = toURI "http://id.ninebynine.org/wip/2003/test/graph5/"
base6  = toURI "file://home/swish/"
base7  = toURI "urn:long:separator:path" -- should this really be "urn:"?
  
qb1s1, qb2s2, qb3s3, qb3, qb3bm, qb4m, qb5, qb5s5, qb6, qb7 :: QName
qb1s1  = newQName base1 "s1"
qb2s2  = newQName base2 "s2"
qb3s3  = newQName base3 "s3"
qb3    = newQName base3 ""
qb3bm  = newQName base3 "basemore"
qb4m   = newQName base4 "more"
qb5    = newQName base5 ""
qb5s5  = newQName base5 "s5"
qb6    = newQName base6 "file.dat"
qb7    = newQName base7 ""

qb1st1, qb2st2, qb3st3 :: QName
qb1st1 = newQName base1 "st1"
qb2st2 = newQName base2 "st2"
qb3st3 = newQName base3 "st3"

------------------------------------------------------------
--  QName equality tests
------------------------------------------------------------

testQNameEq :: String -> Bool -> QName -> QName -> Test
testQNameEq = testCompareEq "QNameEq"
-- testQNameEq lbl eq n1 n2 = testIsEq "QNameEq" lbl eq (n1==n2)

qnlist :: [(String, QName)]
qnlist =
  [ ("qb1s1", qb1s1)
  , ("qb2s2", qb2s2)
  , ("qb3s3", qb3s3)
  , ("qb3",   qb3)
  , ("qb3bm", qb3bm)
  , ("qb4m",  qb4m)
  , ("qb5",   qb5)
  , ("qb5s5", qb5s5)
  , ("qb1st1",qb1st1)
  , ("qb2st2",qb2st2)
  , ("qb3st3",qb3st3)
  ]

qneqlist :: [(String, String)]
qneqlist =
  [ ("qb3bm","qb4m")
  ]

testQNameEqSuite :: Test
testQNameEqSuite = TestList
  [ testQNameEq (testLab l1 l2) (testEq  l1 l2) n1 n2
      | (l1,n1) <- qnlist , (l2,n2) <- qnlist ]
    where
    testLab l1 l2 = l1 ++ ("-"::String) ++ l2
    testEq  l1 l2 = (l1 == l2)        ||
            (l1,l2) `elem` qneqlist ||
            (l2,l1) `elem` qneqlist

------------------------------------------------------------
--  Alternative constructors
------------------------------------------------------------

nq1, nq2 :: QName
nq1 = newQName base1 "s1"
nq2 = newQName base1 "s2"

toQN :: String -> QName
toQN = fromJust . qnameFromURI . toURI

qu1, qu2, qu3, qu4, qu5, qu6, qu7 :: QName
qu1 = toQN "http://id.ninebynine.org/wip/2003/test/graph1/node#s1"
qu2 = toQN "http://id.ninebynine.org/wip/2003/test/graph2/node/s2"
qu3 = "http://id.ninebynine.org/wip/2003/test/graph3/node"
qu4 = "http://id.ninebynine.org/wip/2003/test/graph5/"
qu5 = "http://id.ninebynine.org/wip/2003/test/graph5/s5"
qu6 = "file://home/swish/file.dat"
qu7 = "urn:long:separator:path"

testMakeQNameSuite :: Test
testMakeQNameSuite = 
  TestList
  [ testQNameEq "testnq01" True  nq1 qb1s1
  , testQNameEq "testnq02" False nq2 qb1s1
  , testQNameEq "testqu01" True qb1s1 qu1
  , testQNameEq "testqu02" True qb2s2 qu2
  , testQNameEq "testqu03" True qb3   qu3
  , testQNameEq "testqu04" True qb5   qu4
  , testQNameEq "testqu05" True qb5s5 qu5
  , testQNameEq "testqu06" True qb6   qu6
  , testQNameEq "testqu07" True qb7   qu7
  ]

------------------------------------------------------------
--  Extract components
------------------------------------------------------------

testStringEq :: String -> String -> String -> Test
testStringEq = testCompare "StringEq"

testTextEq :: String -> T.Text -> T.Text -> Test
testTextEq = testCompare "TextEq"

testURIEq :: String -> String -> URI -> Test
testURIEq lbl uri = testCompare "URIEq" lbl (toURI uri)

testPartQNameSuite :: Test
testPartQNameSuite = 
  TestList
  [ testURIEq "testGetNamespace01"
        "http://id.ninebynine.org/wip/2003/test/graph1/node#" 
        (getNamespace qb1s1)
  , testURIEq "testGetNamespace02"
        "http://id.ninebynine.org/wip/2003/test/graph2/node/"
        (getNamespace qb2s2)
  , testURIEq "testGetNamespace03"
        "http://id.ninebynine.org/wip/2003/test/graph3/node"
        (getNamespace qb3s3)
  , testURIEq "testGetNamespace04"
        "http://id.ninebynine.org/wip/2003/test/graph3/node"
        (getNamespace qb3)
  , testTextEq "testGetLocalName01"
        "s1" (getLName (getLocalName qb1s1))
  , testTextEq "testGetLocalName02"
        "s2" (getLName (getLocalName qb2s2))
  , testTextEq "testGetLocalName03"
      "s3" (getLName (getLocalName qb3s3))
  , testTextEq "testGetLocalName04"
      "" (getLName (getLocalName qb3))
  , testURIEq "testGetQNameURI01"
      "http://id.ninebynine.org/wip/2003/test/graph1/node#s1"
      (getQNameURI qb1s1)
  , testURIEq "testGetQNameURI02"
      "http://id.ninebynine.org/wip/2003/test/graph2/node/s2"
      (getQNameURI qb2s2)
  , testURIEq "testGetQNameURI03"
      "http://id.ninebynine.org/wip/2003/test/graph3/nodes3"
      (getQNameURI qb3s3)
  , testURIEq "testGetQNameURI04"
      "http://id.ninebynine.org/wip/2003/test/graph3/node"
      (getQNameURI qb3)
  ]

------------------------------------------------------------
--  Maybe Qname comparison
------------------------------------------------------------

testMaybeQNameEq :: String -> Bool -> Maybe QName -> Maybe QName -> Test
testMaybeQNameEq = testCompareEq "MaybeQName"

testMaybeQNameEqSuite :: Test
testMaybeQNameEqSuite = 
  TestList
  [ testMaybeQNameEq "testMaybeQNameEq01" True
      (Just qb1s1) (Just qb1s1)
  , testMaybeQNameEq "testMaybeQNameEq02" False
      (Just qb1s1) (Just qb2s2)
  , testMaybeQNameEq "testMaybeQNameEq03" False
      (Just qb1s1) Nothing
  , testMaybeQNameEq "testMaybeQNameEq04" False
      Nothing (Just qb1s1)
  , testMaybeQNameEq "testMaybeQNameEq05" True
      Nothing Nothing
  ]

------------------------------------------------------------
--  QName ordering
------------------------------------------------------------

testQNameLe :: String -> Bool -> QName -> QName -> Test
testQNameLe lbl le n1 n2 = testCompare "QNameLE" lbl le (n1 <= n2)

testQNameLeSuite :: Test
testQNameLeSuite = 
  TestList
  [testQNameLe "testQNameLe01" True  qb3bm qb4m
  , testQNameLe "testQNameLe02" True  qb4m  qb3bm
  , testQNameLe "testQNameLe03" True  qb1s1 qb2s2
  , testQNameLe "testQNameLe04" False qb2s2 qb1s1
  ]
  
------------------------------------------------------------
--  Show QName
------------------------------------------------------------

testShowQNameSuite :: Test
testShowQNameSuite = 
  TestList
  [testStringEq "testShowQName01"
      "<http://id.ninebynine.org/wip/2003/test/graph1/node#s1>"
      (show qb1s1)
  , testStringEq "testShowQName02"
    "<http://id.ninebynine.org/wip/2003/test/graph2/node/s2>"
    (show qb2s2)
  , testStringEq "testShowQName03"
    "<http://id.ninebynine.org/wip/2003/test/graph3/node>"
    (show qb3)
  , testStringEq "testShowQName04"
    "<http://id.ninebynine.org/wip/2003/test/graph5/>"
    (show qb5)
  , testStringEq "testShowQName06"
    "<file://home/swish/file.dat>"
    (show qb6)
  , testStringEq "testShowQName07"
    "<urn:long:separator:path>"
    (show qb7)
  ]

------------------------------------------------------------
--  Split URI string into QName parts
------------------------------------------------------------

-- splitURI :: String -> ( String, String )
    -- splitURI "http://example.org/aaa#bbb" = ("http://example.org/aaa#","bbb")
    -- splitURI "http://example.org/aaa/bbb" = ("http://example.org/aaa/","bbb")
    -- splitURI "http://example.org/aaa/"    = ("http://example.org/aaa/","")

{-
testSplitURI :: String -> String -> ( String, String ) -> Test
testSplitURI label input ans =
    TestCase ( assertEqual label ans ( splitURI input ) )

as splitURI has now been moved into qnameFromURI we change the
test somewhat and also include a check of the
URI combination done by newQName (may be tested elsewhere).
-}

testSplitURI :: String -> String -> (String, LName) -> Test
testSplitURI lbl input (a,b) =
  let qn = newQName (toURI a) b
  in 
   TestList
   [ testCompare lbl ":split" qn ((fromJust . qnameFromURI . toURI) input)
   , testCompare lbl ":show"  input (show (getQNameURI qn))
   ]

testSplitURISuite :: Test
testSplitURISuite = 
  TestList
  [ testSplitURI "testSplitURI01"
      "http://example.org/aaa#bbb"
      ( "http://example.org/aaa#", "bbb" )
  , testSplitURI "testSplitURI02"
     "http://example.org/aaa/bbb"
     ( "http://example.org/aaa/", "bbb" )
  , testSplitURI "testSplitURI03"
     "http://example.org/aaa#"
     ( "http://example.org/aaa#", "" )
  , testSplitURI "testSplitURI04"
     "http://example.org/aaa/"
     ( "http://example.org/aaa/", "" )
     
  {- REMOVE the relative URI tests since it is not clear they make sense
        for QNames.

  , testSplitURI "testSplitURI05"
     "//example.org/aaa#bbb"
     ( "//example.org/aaa#", "bbb" )
  , testSplitURI "testSplitURI06"
     "aaa/bbb"
     ( "aaa/", "bbb" )
  , testSplitURI "testSplitURI07"
     "aaa/bbb/"
     ( "aaa/bbb/", "" )
     
     -- Thanks to Ian Dickinson of the HP Jena team for spotting this one:
     -- So what *is* the correct split here?
  , testSplitURI "testSplitURI08"
      "mortal"
      ( "", "mortal" )
  
    -}
  ]

------------------------------------------------------------
--  Scoped Name tests, via QName and URI
--  In reality this is testing qnameFromURI (or at least
--  that was the original motivation).
------------------------------------------------------------

-- simple round-trip tests
testSQRoundTrip :: String -> String -> Test
testSQRoundTrip lbl uri = 
  let u = (fromJust . parseURIReference) uri
      qn = (fromJust . qnameFromURI) u
      sn = makeQNameScopedName Nothing qn
  in TestList
     [ testCompare "SQ:URI"   lbl u  (getScopedNameURI sn)
     , testCompare "SQ:Qname" lbl qn (getQName sn)
     ]

testSNameTTSuite :: Test
testSNameTTSuite =
  TestList
  [ testSQRoundTrip "null" ""
  , testSQRoundTrip "frag1"  "/" -- Should relative fragments be supported?
  , testSQRoundTrip "frag2a"  "/foo"
  , testSQRoundTrip "frag2b"  "/foo/"
  , testSQRoundTrip "frag3"  "/foo/bar"
  , testSQRoundTrip "frag4a"  "/foo/bar#"
  , testSQRoundTrip "frag4b"  "/foo/bar#fragid"
  , testSQRoundTrip "http1a" "http://example.com"
  , testSQRoundTrip "http1b" "http://example.com/"
  , testSQRoundTrip "http2" "http://example.com/foo/bar/"
  , testSQRoundTrip "http3" "http://example.com/foo/bar/bar"
  , testSQRoundTrip "http4a" "http://example.com/foo/bar/bar#"
  , testSQRoundTrip "http4b" "http://example.com/foo/bar/bar#fragid"
  , testSQRoundTrip "https1" "https://joeuser@example.com/foo/bar"
  , testSQRoundTrip "file1"  "file:///dev/null"
  , testSQRoundTrip "urn1"   "URN:foo:a123,456"
  , testSQRoundTrip "urn2"   "urn:foo:a123%2C456"
  ]

------------------------------------------------------------
--  All tests
------------------------------------------------------------

allTests :: [TF.Test]
allTests = 
  [ conv "QNameEq" testQNameEqSuite
  , conv "MakeQName" testMakeQNameSuite
  , conv "PartQName" testPartQNameSuite
  , conv "MaybeQNameEq" testMaybeQNameEqSuite
  , conv "QNameLe" testQNameLeSuite
  , conv "ShowQName" testShowQNameSuite
  , conv "SplitURI" testSplitURISuite
  , conv "SNameTT" testSNameTTSuite
  ]

main :: IO ()
main = TF.defaultMain allTests

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--      2011, 2013 Douglas Burke
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
