{-# LANGUAGE OverloadedStrings #-}

module Zenacy.HTML.Internal.Zip.Tests
  ( testZip
  ) where

import Zenacy.HTML
import Control.Monad
  ( (>=>)
  )
import Control.Monad.Writer
  ( Writer
  , execWriter
  , tell
  )
import Data.Maybe
  ( fromJust
  )
import Test.Framework
  ( Test
  , testGroup
  )
import Test.Framework.Providers.HUnit
  ( testCase
  )
import Test.HUnit
  ( assertBool
  , assertEqual
  , assertFailure
  )
import Data.Text
  ( Text
  )
import qualified Data.Text as T
  ( concat
  )

testZip :: Test
testZip = testGroup "Zenacy.HTML.Internal.Zip"
  [ testFind
  , testFirst
  , testLast
  , testParent
  , testRoot
  , testModify
  , testDelete
  , testNext
  , testPrev
  , testGet
  , testInsertBefore
  , testInsertAfter
  , testUnzip
  , testStep
  , testSearch
  , testContentLeft
  , testContentRight
  , testDropLeft
  , testDropRight
  , testPruneLeft
  , testPruneRight
  , testIndex
  , testPath
  , testPathFind
  , testTest
  , testIterModify
  ]

h :: HTMLNode
h = htmlParseEasy "<h1></h1><p><a href='bbb'>AAA</a><span></span><br><img></p>"

z :: HTMLZipper
z = htmlZip h

f :: Text -> HTMLZipper -> Maybe HTMLZipper
f x = htmlZipFind $ htmlElemHasName x

n :: HTMLZipper -> Text
n = htmlElementName . htmlZipNode

g :: HTMLZipper -> Maybe Text
g = Just . n

testFind :: Test
testFind = testCase "zip find" $ do
  assertEqual "TEST 1" (Just "html")
    ((f "html" >=> g) z)
  assertEqual "TEST 2" (Just "body")
    ((f "html" >=> f "body" >=> g) z)
  assertEqual "TEST 3" (Just "h1")
    ((f "html" >=> f "body" >=> f "h1" >=> g) z)
  assertEqual "TEST 4" (Just "p")
    ((f "html" >=> f "body" >=> f "p" >=> g) z)
  assertEqual "TEST 5" (Just "a")
    ((f "html" >=> f "body" >=> f "p" >=> f "a" >=> g) z)
  assertEqual "TEST 6" (Just "span")
    ((f "html" >=> f "body" >=> f "p" >=> f "span" >=> g) z)
  assertEqual "TEST 7" (Just "br")
    ((f "html" >=> f "body" >=> f "p" >=> f "br" >=> g) z)
  assertEqual "TEST 8" (Just "img")
    ((f "html" >=> f "body" >=> f "p" >=> f "img" >=> g) z)
  assertEqual "TEST 9" Nothing
    ((f "x" >=> g) z)

testFirst :: Test
testFirst = testCase "zip first" $ do
  assertEqual "TEST 1" (Just "h1") $
    (f "html" >=> f "body" >=> htmlZipFirst >=> g) z
  assertEqual "TEST 2" (Just "a") $
    (f "html" >=> f "body" >=> f "p" >=> htmlZipFirst >=> g) z

testLast :: Test
testLast = testCase "zip last" $ do
  assertEqual "TEST 1" (Just "p") $
    (f "html" >=> f "body" >=> htmlZipLast >=> g) z
  assertEqual "TEST 2" (Just "img") $
    (f "html" >=> f "body" >=> f "p" >=> htmlZipLast >=> g) z

testParent :: Test
testParent = testCase "zip parent" $ do
  let p = htmlZipParent
  let q = f "html" >=> f "body" >=> f "p" >=> f "br"
  assertEqual "TEST 1" (Just "p") $ (q >=> p >=> g) z
  assertEqual "TEST 2" (Just "body") $ (q >=> p >=> p >=> g) z
  assertEqual "TEST 3" (Just "html") $ (q >=> p >=> p >=> p >=> g) z

testRoot :: Test
testRoot = testCase "zip root" $ do
  case (f "html" >=> f "body" >=> f "p" >=> f "br") z of
    Nothing ->
      assertFailure "TEST 1"
    Just z' ->
      case htmlZipNode (htmlZipRoot z') of
        HTMLDocument {} ->
          assertBool "TEST 2" True
        _ ->
          assertFailure "TEST 3"

testModify :: Test
testModify = testCase "zip modify" $ do
  case (f "html" >=> f "body" >=> f "h1") z of
    Nothing ->
      assertFailure "TEST 1"
    Just z' -> do
      let r y = htmlZipModify (\x -> x { htmlElementName = y })
      assertEqual "TEST 2" "h1" $ n z'
      assertEqual "TEST 3" "h2" $ n $ r "h2" z'

testDelete :: Test
testDelete = testCase "zip delete" $ do
  let q = f "html" >=> f "body" >=> htmlZipDelete
  let h' = htmlUnzip $ fromJust $ q z
  assertEqual "TEST 1" "<html><head></head></html>" $ htmlRender h'

testNext :: Test
testNext = testCase "zip next" $ do
  let t = htmlZipNext
  let q = f "html" >=> f "body" >=> f "p" >=> htmlZipFirst
  assertEqual "TEST 1" (Just "a") $
    (q >=> g) z
  assertEqual "TEST 2" (Just "span") $
    (q >=> t >=> g) z
  assertEqual "TEST 3" (Just "br") $
    (q >=> t >=> t >=> g) z
  assertEqual "TEST 4" (Just "img") $
    (q >=> t >=> t >=> t >=> g) z
  assertEqual "TEST 5" Nothing $
    (q >=> t >=> t >=> t >=> t >=> g) z

testPrev :: Test
testPrev = testCase "zip prev" $ do
  let t = htmlZipPrev
  let q = f "html" >=> f "body" >=> f "p" >=> htmlZipLast
  assertEqual "TEST 1" (Just "img") $
    (q >=> g) z
  assertEqual "TEST 2" (Just "br") $
    (q >=> t >=> g) z
  assertEqual "TEST 3" (Just "span") $
    (q >=> t >=> t >=> g) z
  assertEqual "TEST 4" (Just "a") $
    (q >=> t >=> t >=> t >=> g) z
  assertEqual "TEST 5" Nothing $
    (q >=> t >=> t >=> t >=> t >=> g) z

testGet :: Test
testGet = testCase "zip get" $ do
  let q = f "html" >=> f "body" >=> f "p"
  assertEqual "TEST 1" (Just "a") $
    (q >=> htmlZipGet 0 >=> g) z
  assertEqual "TEST 2" (Just "span") $
    (q >=> htmlZipGet 1 >=> g) z
  assertEqual "TEST 3" (Just "br") $
    (q >=> htmlZipGet 2 >=> g) z
  assertEqual "TEST 4" (Just "img") $
    (q >=> htmlZipGet 3 >=> g) z
  assertEqual "TEST 5" Nothing $
    (q >=> htmlZipGet 4 >=> g) z

testInsertBefore :: Test
testInsertBefore = testCase "zip insert before" $ do
  let q = f "html" >=> f "body" >=> htmlZipLast
  let e = htmlDefaultElement { htmlElementName = "h2" }
  assertEqual "TEST 1" (Just "h2") $
    (q >=> htmlZipInsertBefore e >=> htmlZipPrev >=> g) z

testInsertAfter :: Test
testInsertAfter = testCase "zip insert after" $ do
  let q = f "html" >=> f "body" >=> htmlZipFirst
  let e = htmlDefaultElement { htmlElementName = "h2" }
  assertEqual "TEST 1" (Just "h2") $
    (q >=> htmlZipInsertAfter e >=> htmlZipNext >=> g) z

testUnzip :: Test
testUnzip = testCase "zip unzip" $ do
  let q = f "html" >=> f "body" >=> f "p" >=> f "a" >=>
          pure . htmlZipModify (htmlElemAttrRemove "href")
  let h' = htmlUnzip $ fromJust $ q z
  assertEqual "TEST 1"
    "<html><head></head><body>\
    \<h1></h1><p><a>AAA</a><span></span><br><img></p>\
    \</body></html>" $
    htmlRender h'

testStep :: Test
testStep = testCase "zip step" $ do
  assertEqual "TEST 1"
    ["html","head","body","h1","p","a","span","br","img",
     "div","p","span","a","img"]
    $ w htmlZipStepNext
  assertEqual "TEST 2"
    ["html","body","div","p","span","img","a","p",
     "img","br","span","a","h1","head"]
    $ w htmlZipStepBack
  where
    w h' = execWriter $ f h' z
    f :: (HTMLZipper -> Maybe HTMLZipper) -> HTMLZipper -> Writer [Text] HTMLZipper
    f h z' = case h z' of
      Nothing -> return z'
      Just x -> tell [n x] >> f h x
    z = htmlZip $ htmlParseEasy $ T.concat
        [ "<h1></h1>"
        , "<p><a></a><span></span><br><img></p>"
        , "<div><p><span><a></a><img></span></p></div>"
        ]

testSearch :: Test
testSearch = testCase "zip search" $ do
  assertEqual "TEST 201" "h1"   $ fa (t "h1")
  assertEqual "TEST 202" "a"    $ fa (i "1")
  assertEqual "TEST 203" "a"    $ fa (i "2")
  assertEqual "TEST 204" "span" $ fa (t "span")
  assertEqual "TEST 205" "h1"   $ fa (i "3")
  assertEqual "TEST 206" "h1"   $ ba (t "h1")
  assertEqual "TEST 207" "a"    $ ba (i "1")
  assertEqual "TEST 208" "a"    $ ba (i "2")
  assertEqual "TEST 209" "span" $ ba (t "span")
  assertEqual "TEST 210" "h1"   $ ba (i "3")
  assertEqual "TEST 211" "img"  $ ba (i "8")
  assertEqual "TEST 212" "img"  $ fa (i "8")

  assertEqual "TEST 301" "h1" $
    m $ (sf (i "8") >=> sb (i "3")) z
  assertEqual "TEST 302" "span" $
    m $ (sf (i "8") >=> ib (i "7")) z
  assertEqual "TEST 303" "img" $
    m $ (sf (t "img") >=> sf (t "img")) z
  where
    sf = htmlZipSearch htmlZipStepNext
    sb = htmlZipSearch htmlZipStepBack
    fa h = m $ sf h z
    ba h = m $ sb h z

    ib :: (HTMLZipper -> Bool) -> HTMLZipper -> Maybe HTMLZipper
    ib f = htmlIterSearch htmlIterBack f . htmlIter >=> Just . htmlIterZipper

    i x = htmlElemHasAttrVal "id" x . htmlZipNode
    t x = htmlElemHasName x . htmlZipNode
    m = maybe "" n
    z = htmlZip
      . htmlParseEasy
      . T.concat
      $ [ "<div id='4'></div>"
        , "<h1 id='3'></h1>"
        , "<p><a id='1'></a><span></span><br><img></p>"
        , "<div><p><span id='7'><a id='2'></a><img id='8'></span></p></div>"
        ]

testContentLeft :: Test
testContentLeft = testCase "zip content left" $ do
  let q x = f "html" >=> f "body" >=> f "p" >=> f x >=> pure . htmlZipContentLeft
  let r x = map htmlElemName $ fromJust $ q x z
  assertEqual "TEST 1" [] $ r "a"
  assertEqual "TEST 2" ["a"] $ r "span"
  assertEqual "TEST 3" ["a","span"] $ r "br"
  assertEqual "TEST 4" ["a","span","br"] $ r "img"

testContentRight :: Test
testContentRight = testCase "zip content right" $ do
  let q x = f "html" >=> f "body" >=> f "p" >=> f x >=> pure . htmlZipContentRight
  let r x = map htmlElemName $ fromJust $ q x z
  assertEqual "TEST 1" ["span","br","img"] $ r "a"
  assertEqual "TEST 2" ["br","img"] $ r "span"
  assertEqual "TEST 3" ["img"] $ r "br"
  assertEqual "TEST 4" [] $ r "img"

testDropLeft :: Test
testDropLeft = testCase "zip drop left" $ do
  let q x = f "html" >=> f "body" >=> f "p" >=> f x >=> htmlZipDropLeft
  let r x = htmlRender $ htmlUnzip $ fromJust $ q x $ z
  assertEqual "TEST 1"
    "<html><head></head><body>\
    \<h1></h1><p><a href=\"bbb\">AAA</a><span></span><br><img></p>\
    \</body></html>" $
    r "a"
  assertEqual "TEST 2"
    "<html><head></head><body>\
    \<h1></h1><p><span></span><br><img></p>\
    \</body></html>" $
    r "span"
  assertEqual "TEST 3"
    "<html><head></head><body>\
    \<h1></h1><p><br><img></p>\
    \</body></html>" $
    r "br"
  assertEqual "TEST 4"
    "<html><head></head><body>\
    \<h1></h1><p><img></p>\
    \</body></html>" $
    r "img"

testDropRight :: Test
testDropRight = testCase "zip drop right" $ do
  let q x = f "html" >=> f "body" >=> f "p" >=> f x >=> htmlZipDropRight
  let r x = htmlRender $ htmlUnzip $ fromJust $ q x $ z
  assertEqual "TEST 1"
    "<html><head></head><body>\
    \<h1></h1><p><a href=\"bbb\">AAA</a></p>\
    \</body></html>" $
    r "a"
  assertEqual "TEST 2"
    "<html><head></head><body>\
    \<h1></h1><p><a href=\"bbb\">AAA</a><span></span></p>\
    \</body></html>" $
    r "span"
  assertEqual "TEST 3"
    "<html><head></head><body>\
    \<h1></h1><p><a href=\"bbb\">AAA</a><span></span><br></p>\
    \</body></html>" $
    r "br"
  assertEqual "TEST 4"
    "<html><head></head><body>\
    \<h1></h1><p><a href=\"bbb\">AAA</a><span></span><br><img></p>\
    \</body></html>" $
    r "img"

testPruneLeft :: Test
testPruneLeft = testCase "zip prune left" $ do
  assertEqual "TEST 1"
    "<body><div><div><h1></h1><img></div><p></p></div></body>" $
    run $ f "div" >=> f "div" >=> f "h1" >=> htmlZipPruneLeft
  assertEqual "TEST 2"
    "<body><div><div><img></div><p></p></div></body>" $
    run $ f "div" >=> f "div" >=> f "img" >=> htmlZipPruneLeft
  assertEqual "TEST 3"
    "<body><div><p></p></div></body>" $
    run $ f "div" >=> htmlZipFirst >=> htmlZipNext >=> htmlZipNext
      >=> htmlZipPruneLeft
  where
    run g = htmlRender $ htmlUnzip $ fromJust $ g $ fromJust $
      (htmlZipM >=> f "html" >=> f "body" >=> htmlZipNodeM >=> htmlZipM) $
      htmlParseEasy
        "<div>\
        \<p></p>\
        \<div>\
        \<span></span><h1></h1><img>\
        \</div>\
        \<p></p>\
        \</div>"

testPruneRight :: Test
testPruneRight = testCase "zip prune right" $ do
  assertEqual "TEST 1"
    "<body><div><p></p><div><span></span><h1></h1></div></div></body>" $
    run $ f "div" >=> f "div" >=> f "h1" >=> htmlZipPruneRight
  assertEqual "TEST 2"
    "<body><div><p></p><div><span></span><h1></h1><img></div></div></body>" $
    run $ f "div" >=> f "div" >=> f "img" >=> htmlZipPruneRight
  assertEqual "TEST 3"
    "<body><div><p></p><div><span></span></div></div></body>" $
    run $ f "div" >=> f "div" >=> f "span" >=> htmlZipPruneRight
  assertEqual "TEST 4"
    "<body><div><p></p></div></body>" $
    run $ f "div" >=> htmlZipFirst >=> htmlZipPruneRight
  where
    run g = htmlRender $ htmlUnzip $ fromJust $ g $ fromJust $
      (htmlZipM >=> f "html" >=> f "body" >=> htmlZipNodeM >=> htmlZipM) $
      htmlParseEasy
        "<div>\
        \<p></p>\
        \<div>\
        \<span></span><h1></h1><img>\
        \</div>\
        \<p></p>\
        \</div>"

testIndex :: Test
testIndex = testCase "zip index" $ do
  assertEqual "TEST 1" (Just 0) $ g (i "1")
  assertEqual "TEST 2" (Just 0) $ g (i "2")
  assertEqual "TEST 3" (Just 1) $ g (i "8")
  assertEqual "TEST 4" (Just 1) $ g (i "3")
  assertEqual "TEST 4" (Just 0) $ g (i "4")
  assertEqual "TEST 5" (Just 3) $ g (i "5")
  where
    g f = maybe Nothing htmlZipIndex $ htmlZipSearch htmlZipStepNext f z
    i x = htmlElemHasAttrVal "id" x . htmlZipNode
    z = htmlZip
      . htmlParseEasy
      . T.concat
      $ [ "<div id='4'></div>"
        , "<h1 id='3'></h1>"
        , "<p><a id='1'></a><span></span><br><img id='5'></p>"
        , "<div><p><span id='7'><a id='2'></a><img id='8'></span></p></div>"
        ]

testPath :: Test
testPath = testCase "zip path" $ do
  assertEqual "TEST 1" (m [0,1,2,0]) $ g (i "1")
  assertEqual "TEST 2" (m [0,1,3,0,0,0]) $ g (i "2")
  assertEqual "TEST 3" (m [0,1,3,0,0,1]) $ g (i "8")
  assertEqual "TEST 4" (m [0,1,1]) $ g (i "3")
  assertEqual "TEST 4" (m [0,1,0]) $ g (i "4")
  assertEqual "TEST 5" (m [0,1,2,3]) $ g (i "5")
  where
    m = Just . HTMLZipPath
    g f = maybe Nothing (Just . htmlZipPath) $ htmlZipSearch htmlZipStepNext f z
    i x = htmlElemHasAttrVal "id" x . htmlZipNode
    z = htmlZip
      . htmlParseEasy
      . T.concat
      $ [ "<div id='4'></div>"
        , "<h1 id='3'></h1>"
        , "<p><a id='1'></a><span></span><br><img id='5'></p>"
        , "<div><p><span id='7'><a id='2'></a><img id='8'></span></p></div>"
        ]

testPathFind :: Test
testPathFind = testCase "zip path find" $ do
  assertEqual "TEST 1" (Just "a")    $ f [0,1,2,0]
  assertEqual "TEST 2" (Just "span") $ f [0,1,2,1]
  assertEqual "TEST 3" (Just "span") $ f [0,1,3,0,0]
  assertEqual "TEST 4" (Just "a")    $ f [0,1,3,0,0,0]
  assertEqual "TEST 5" (Just "img")  $ f [0,1,3,0,0,1]
  where
    f p = htmlZipPathFind (HTMLZipPath p) z >>= g
    z = htmlZip
      . htmlParseEasy
      . T.concat
      $ [ "<div id='4'></div>"
        , "<h1 id='3'></h1>"
        , "<p><a id='1'></a><span></span><br><img id='5'></p>"
        , "<div><p><span id='7'><a id='2'></a><img id='8'></span></p></div>"
        ]

testTest :: Test
testTest = testCase "zip test" $ do
  assertEqual "TEST 1" (Just True)
    ((name "body" >=> frst >=> name "h1" >=> next >=> name "p" >=> tlst >=> s) z')
  assertEqual "TEST 2" (Nothing)
    ((name "body" >=> frst >=> name "h1" >=> next >=> name "a" >=> tlst >=> s) z')
  assertEqual "TEST 3" (Nothing)
    ((name "body" >=> frst >=> name "h1" >=> next >=> name "p" >=> tfst >=> s) z')
  assertEqual "TEST 4" (Just "h1") $
    htmlZipM b >>=
    htmlZipTestName "body" >>=
    htmlZipFirst >>=
    htmlZipTestName "h1" >>= \n0 ->
    htmlZipNext n0 >>=
    htmlZipTestName "p" >>=
    htmlZipTestLast >>
    pure (htmlElemName $ htmlZipNode n0)
  where
    b = fromJust $ htmlDocBody h
    z' = htmlZip b
    frst = htmlZipFirst
    next = htmlZipNext
    tfst = htmlZipTestFirst
    tlst = htmlZipTestLast
    name = htmlZipTestName
    s = const $ pure True

testIterModify :: Test
testIterModify = testCase "zip iter modify" $ do
  assertEqual "TEST 1" n' (f' b')
  where
    b' = fromJust $ htmlDocBody h
    n' = "newname"
    f' = ( htmlElemName
        . htmlUnzip
        . htmlIterZipper
        . htmlIterModify (htmlZipModify (htmlElemRename n'))
        . htmlIter
        . htmlZip
        )
