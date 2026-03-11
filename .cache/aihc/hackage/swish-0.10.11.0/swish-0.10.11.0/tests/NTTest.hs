{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  NTTest
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012, 2013 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  OverloadedString
--
--  This Module contains test cases for the NTriples parsing and formatting modules.
--
--------------------------------------------------------------------------------

module Main where

import qualified Data.Set as S
import qualified Data.Text.Lazy as T

import qualified Test.Framework as TF
import qualified Test.Framework.Providers.HUnit as TF

import Swish.GraphClass (arc)

import Swish.RDF.Parser.NTriples (parseNT)
import Swish.RDF.Formatter.NTriples (formatGraphAsLazyText)

import Swish.RDF.Graph
  ( RDFGraph, RDFLabel(..)
    , emptyRDFGraph 
    , toRDFGraph
    )

import Swish.RDF.Vocabulary (toLangTag, rdfXMLLiteral)

import Test.HUnit
    ( Test(TestCase,TestList)
    , (@=?)
    , assertEqual )

import Data.Maybe (fromJust)

import TestHelpers (conv)

------------------------------------------------------------
--  Parser tests
------------------------------------------------------------

-- check that parsing the input creates the expected graph

checkGraph :: String -> T.Text -> RDFGraph -> Test
checkGraph lab inp gr =
    TestList
      [ TestCase ( assertEqual ("parse-failed:"++lab) noError pe )
      , TestCase ( assertEqual ("parse-result:"++lab) gr pg )
      ]
    where
      (pe,pg) = case parseNT inp of
        Right g -> (noError, g)
        Left  s -> (s, emptyRDFGraph)
            
noError :: String
noError = ""

-- | Take the input graph and parse it (the first
--   graph), then convert this back to text and parse
--   the result, to give the second graph. The
--   two strings are the errors from the parses,
--   if any.
doRoundTrip :: T.Text -> (RDFGraph, RDFGraph, String, String)
doRoundTrip inp =
  let (pErr1, pGr1) = case parseNT inp of
        Right g -> (noError, g)
        Left  s -> (s, emptyRDFGraph)
        
      inp2 = formatGraphAsLazyText pGr1
      
      (pErr2, pGr2) = case parseNT inp2 of
        Right g -> (noError, g)
        Left  s -> (s, emptyRDFGraph)

  in (pGr1, pGr2, pErr1, pErr2)

-- check that the 
--    parseNT input == parseNT (formatGraph (parse NT input))
--
roundTrip :: String -> T.Text -> Test
roundTrip lbl inp = 
  let (pGr1, pGr2, pErr1, pErr2) = doRoundTrip inp
        
  in TestList
    [ TestCase (assertEqual ("roundTrip-parsing1:"++lbl) noError pErr1)
    , TestCase (assertEqual ("roundTrip-parsing2:"++lbl) noError pErr2)
    , TestCase (assertEqual ("roundTrip-graph:"++lbl)    pGr1    pGr2)
    ]

roundTripTF :: String -> T.Text -> TF.Test
roundTripTF lbl inp = 
  let (pGr1, pGr2, pErr1, pErr2) = doRoundTrip inp
        
  in TF.testGroup ("roundTrip:" ++ lbl)
     [ TF.testCase "parsing1" (noError @=? pErr1)
     , TF.testCase "parsing2" (noError @=? pErr2)
     , TF.testCase "graph"    (pGr1    @=? pGr2)
     ]


------------------------------------------------------------
--  Rather than bother with locating an external file,
--  include it directly.
--
--  This is the contents of
--    http://www.w3.org/2000/10/rdf-tests/rdfcore/ntriples/test.nt
--  retrieved on 2011-03-23 11:25:46
--
------------------------------------------------------------

w3cTest :: T.Text
w3cTest = "#\n# Copyright World Wide Web Consortium, (Massachusetts Institute of\n# Technology, Institut National de Recherche en Informatique et en\n# Automatique, Keio University).\n#\n# All Rights Reserved.\n#\n# Please see the full Copyright clause at\n# <http://www.w3.org/Consortium/Legal/copyright-software.html>\n#\n# Test file with a variety of legal N-Triples\n#\n# Dave Beckett - http://purl.org/net/dajobe/\n# \n# $Id: test.nt,v 1.7 2003/10/06 15:52:19 dbeckett2 Exp $\n# \n#####################################################################\n\n# comment lines\n  \t  \t   # comment line after whitespace\n# empty blank line, then one with spaces and tabs\n\n         \t\n<http://example.org/resource1> <http://example.org/property> <http://example.org/resource2> .\n_:anon <http://example.org/property> <http://example.org/resource2> .\n<http://example.org/resource2> <http://example.org/property> _:anon .\n# spaces and tabs throughout:\n \t <http://example.org/resource3> \t <http://example.org/property>\t <http://example.org/resource2> \t.\t \n\n# line ending with CR NL (ASCII 13, ASCII 10)\n<http://example.org/resource4> <http://example.org/property> <http://example.org/resource2> .\r\n\n# 2 statement lines separated by single CR (ASCII 10)\n<http://example.org/resource5> <http://example.org/property> <http://example.org/resource2> .\r<http://example.org/resource6> <http://example.org/property> <http://example.org/resource2> .\n\n\n# All literal escapes\n<http://example.org/resource7> <http://example.org/property> \"simple literal\" .\n<http://example.org/resource8> <http://example.org/property> \"backslash:\\\\\" .\n<http://example.org/resource9> <http://example.org/property> \"dquote:\\\"\" .\n<http://example.org/resource10> <http://example.org/property> \"newline:\\n\" .\n<http://example.org/resource11> <http://example.org/property> \"return\\r\" .\n<http://example.org/resource12> <http://example.org/property> \"tab:\\t\" .\n\n# Space is optional before final .\n<http://example.org/resource13> <http://example.org/property> <http://example.org/resource2>.\n<http://example.org/resource14> <http://example.org/property> \"x\".\n<http://example.org/resource15> <http://example.org/property> _:anon.\n\n# \\u and \\U escapes\n# latin small letter e with acute symbol \\u00E9 - 3 UTF-8 bytes #xC3 #A9\n<http://example.org/resource16> <http://example.org/property> \"\\u00E9\" .\n# Euro symbol \\u20ac  - 3 UTF-8 bytes #xE2 #x82 #xAC\n<http://example.org/resource17> <http://example.org/property> \"\\u20AC\" .\n# resource18 test removed\n# resource19 test removed\n# resource20 test removed\n\n# XML Literals as Datatyped Literals\n<http://example.org/resource21> <http://example.org/property> \"\"^^<http://www.w3.org/2000/01/rdf-schema#XMLLiteral> .\n<http://example.org/resource22> <http://example.org/property> \" \"^^<http://www.w3.org/2000/01/rdf-schema#XMLLiteral> .\n<http://example.org/resource23> <http://example.org/property> \"x\"^^<http://www.w3.org/2000/01/rdf-schema#XMLLiteral> .\n<http://example.org/resource23> <http://example.org/property> \"\\\"\"^^<http://www.w3.org/2000/01/rdf-schema#XMLLiteral> .\n<http://example.org/resource24> <http://example.org/property> \"<a></a>\"^^<http://www.w3.org/2000/01/rdf-schema#XMLLiteral> .\n<http://example.org/resource25> <http://example.org/property> \"a <b></b>\"^^<http://www.w3.org/2000/01/rdf-schema#XMLLiteral> .\n<http://example.org/resource26> <http://example.org/property> \"a <b></b> c\"^^<http://www.w3.org/2000/01/rdf-schema#XMLLiteral> .\n<http://example.org/resource26> <http://example.org/property> \"a\\n<b></b>\\nc\"^^<http://www.w3.org/2000/01/rdf-schema#XMLLiteral> .\n<http://example.org/resource27> <http://example.org/property> \"chat\"^^<http://www.w3.org/2000/01/rdf-schema#XMLLiteral> .\n# resource28 test removed 2003-08-03\n# resource29 test removed 2003-08-03\n\n# Plain literals with languages\n<http://example.org/resource30> <http://example.org/property> \"chat\"@fr .\n<http://example.org/resource31> <http://example.org/property> \"chat\"@en .\n\n# Typed Literals\n<http://example.org/resource32> <http://example.org/property> \"abc\"^^<http://example.org/datatype1> .\n# resource33 test removed 2003-08-03\n"  
  
------------------------------------------------------------
--  Define some common values
------------------------------------------------------------

s1, p1, p2, o1 :: RDFLabel
s1 = Res "urn:b#s1" -- rely on IsString to convert to ScopedName
p1 = Res "urn:b#p1"
p2 = Res "http://example.com/pred2"
o1 = Res "urn:b#o1"
{-
s1 = Res $ makeURIScopedName "urn:b#s1"
p1 = Res $ makeURIScopedName "urn:b#p1"
p2 = Res $ makeURIScopedName "http://example.com/pred2"
o1 = Res $ makeURIScopedName "urn:b#o1"
-}

l0, l1, l2, l3, l4 :: RDFLabel
l0 = Lit ""
l1 = Lit "l1"
l2 = Lit "l2-'\"line1\"'\n\nl2-'\"\"line2\"\"'"
l3 = Lit "l3--\r\"'\\--\x20&--\x17A&--"
l4 = Lit "l4 \\"

lfr, lgben, lxml1, lxml2 :: RDFLabel
lfr    = LangLit "chat"           $ fromJust $ toLangTag "fr"
lgben  = LangLit "football"       $ fromJust $ toLangTag "en-gb"
lxml1  = TypedLit "<br/>"         rdfXMLLiteral
lxml2  = TypedLit "<em>chat</em>" rdfXMLLiteral

b1 , b2 :: RDFLabel
b1 = Blank "x1"
b2 = Blank "genid23"

------------------------------------------------------------
--  Construct graphs for testing
------------------------------------------------------------

g0 :: RDFGraph
g0 = toRDFGraph S.empty

mkGr1 :: RDFLabel -> RDFLabel -> RDFLabel -> RDFGraph
mkGr1 s p o = toRDFGraph $ S.singleton $ arc s p o

g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12 :: RDFGraph
g1 = mkGr1 s1 p1 o1
g2 = mkGr1 s1 p1 l0
g3 = mkGr1 s1 p1 l1
g4 = mkGr1 s1 p1 l2
g5 = mkGr1 s1 p1 l3
g6 = mkGr1 s1 p1 lfr
g7 = mkGr1 s1 p1 lgben
g8 = mkGr1 s1 p1 lxml1
g9 = mkGr1 s1 p1 lxml2
g10 = mkGr1 s1 p1 b1
g11 = mkGr1 b2 p1 b1
g12 = mkGr1 s1 p1 l4

gm1 :: RDFGraph
gm1 = toRDFGraph $ S.fromList [arc b2 p2 b1, arc b2 p1 o1]

------------------------------------------------------------
--  Input documents
------------------------------------------------------------

empty1, empty2, empty3, empty4, empty5 :: T.Text

{-
empty3 and empty4 are not valid NTriple documents since they do
not end with a \n, but we support this for now.
-}
empty1 = ""
empty2 = "\n"
empty3 = "  \n  "
empty4 = "# a comment"
empty5 = "\n   # a comment\n "

graph1, graph2, graph3, graph4, graph5, graph6, graph7, graph8,
  graph9, graph10, graph11, graph12 :: T.Text

graph1 = "<urn:b#s1> <urn:b#p1> <urn:b#o1>."
graph2 = "<urn:b#s1> <urn:b#p1>  \"\"."
graph3 = "<urn:b#s1> <urn:b#p1> \"l1\" . "
graph4 = "<urn:b#s1> <urn:b#p1> \"l2-'\\\"line1\\\"'\\n\\nl2-'\\\"\\\"line2\\\"\\\"'\"."
graph5 = "<urn:b#s1> <urn:b#p1>  \"l3--\\r\\\"'\\\\--\\u0020&--\\U0000017A&--\" ."
graph6 = "<urn:b#s1> <urn:b#p1> \"chat\"@fr."
graph7 = "<urn:b#s1> <urn:b#p1> \"football\"@en-gb . "
graph8 = "<urn:b#s1> <urn:b#p1> \"<br/>\"^^<http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral>."
graph9 = "<urn:b#s1> <urn:b#p1> \"<em>chat</em>\"^^<http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral>."
graph10 = "<urn:b#s1> <urn:b#p1> _:x1 . "
graph11 = "_:genid23  <urn:b#p1> _:x1 . "
graph12 = "<urn:b#s1> <urn:b#p1> \"l4 \\\\\" ."

graphm1, graphm1r :: T.Text

graphm1 = "_:genid23 <urn:b#p1> <urn:b#o1> .\n\n # test \n_:genid23  <http://example.com/pred2>  _:x1 .\n\n"
graphm1r = "_:genid23 <http://example.com/pred2> _:x1.\n_:genid23  <urn:b#p1> <urn:b#o1>.\n"

------------------------------------------------------------
--  Parser tests
------------------------------------------------------------

rTests :: Test
rTests = TestList 
         [ roundTrip "empty1" empty1
         , roundTrip "empty2" empty2
         , roundTrip "empty3" empty3
         , roundTrip "empty4" empty4
         , roundTrip "empty5" empty5
         , roundTrip "graph1" graph1 
         , roundTrip "graph2" graph2 
         , roundTrip "graph3" graph3 
         , roundTrip "graph4" graph4 
         , roundTrip "graph5" graph5 
         , roundTrip "graph6" graph6 
         , roundTrip "graph7" graph7 
         , roundTrip "graph8" graph8 
         , roundTrip "graph9" graph9 
         , roundTrip "graph10" graph10 
         , roundTrip "graph11" graph11 
         , roundTrip "graph12" graph12 
         , roundTrip "graphm1" graphm1 
         , roundTrip "graphm1r" graphm1r 
         , roundTrip "W3C test" w3cTest
         ]

eTests :: Test
eTests = TestList 
         [ checkGraph "empty1" empty1 g0
         , checkGraph "empty2" empty2 g0
         , checkGraph "empty3" empty3 g0
         , checkGraph "empty4" empty4 g0
         , checkGraph "empty5" empty5 g0
         ]
         
gTests :: Test
gTests = TestList 
         [ checkGraph "graph1" graph1 g1
         , checkGraph "graph2" graph2 g2
         , checkGraph "graph3" graph3 g3
         , checkGraph "graph4" graph4 g4
         , checkGraph "graph5" graph5 g5
         , checkGraph "graph6" graph6 g6
         , checkGraph "graph7" graph7 g7
         , checkGraph "graph8" graph8 g8
         , checkGraph "graph9" graph9 g9
         , checkGraph "graph10" graph10 g10
         , checkGraph "graph11" graph11 g11
         , checkGraph "graph12" graph12 g12
         , checkGraph "graphm1" graphm1 gm1
         , checkGraph "graphm1r" graphm1r gm1
         ]

allTests :: [TF.Test]
allTests = 
  [ conv "r" rTests
  , conv "e" eTests
  , conv "g" gTests
  , roundTripTF "langtag" "<urn:a> <urn:b> \"Foo .\"@en-UK."
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
