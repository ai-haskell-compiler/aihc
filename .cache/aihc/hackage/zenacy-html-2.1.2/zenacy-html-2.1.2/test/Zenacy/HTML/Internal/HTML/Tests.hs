{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Zenacy.HTML.Internal.HTML.Tests
  ( testHtml
  ) where

import Zenacy.HTML
import Data.Default
  ( Default(..)
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
  ( pack
  )
import Text.RawString.QQ

testHtml :: Test
testHtml = testGroup "Zenacy.HTML.Internal.HTML"
  [ testBasic
  , testAA1
  , testAA2
  , testOmit
  , testTableSparse
  , testMath
  , testScript
  , testInput
  , testList
  , testScriptBeforeHead
  , testComment
  , testEntity
  , testAfterHead
  ]

genNormal x =
  case htmlParse def x of
    Left e -> T.pack $ show e
    Right r -> htmlRender $ htmlResultDocument r

genPretty x =
  case htmlParse def x of
    Left e -> T.pack $ show e
    Right r -> htmlRenderPretty $ htmlResultDocument r

genEntity x y =
  case htmlParse (def { htmlOptionIgnoreEntities = y }) x of
    Left e -> T.pack $ show e
    Right r -> htmlRender $ htmlResultDocument r

testBasic :: Test
testBasic = testCase "html basic" $ do
  assertEqual "TEST 1" htmlPretty $ genPretty htmlIn
  assertEqual "TEST 2" htmlNormal $ genNormal htmlIn

htmlIn =
  "<!DOCTYPE html>\
  \<head><title>TITLE</title></head>\
  \<body><p>HEY</p><br/>\
  \<template><br/><div></div><p></p><p>YOU</p><img/></template>\
  \<!-- test markup -->\
  \<table class=\"class\" style=\"red\">\
  \<tr><td></td><td id=\"2\">MEGADETH</td></tr>\
  \</table>\
  \</body>"

htmlPretty =
  "<!DOCTYPE html>\n\
  \<html>\n\
  \ <head>\n\
  \  <title>TITLE</title>\n\
  \ </head>\n\
  \ <body>\n\
  \  <p>HEY</p>\n\
  \  <br>\n\
  \  <template>\n\
  \   <br>\n\
  \   <div></div>\n\
  \   <p></p>\n\
  \   <p>YOU</p>\n\
  \   <img>\n\
  \  </template>\n\
  \  <!-- test markup -->\n\
  \  <table class=\"class\" style=\"red\">\n\
  \   <tbody>\n\
  \    <tr>\n\
  \     <td></td>\n\
  \     <td id=\"2\">MEGADETH</td>\n\
  \    </tr>\n\
  \   </tbody>\n\
  \  </table>\n\
  \ </body>\n\
  \</html>"

htmlNormal =
  "<!DOCTYPE html>\
  \<html>\
  \<head>\
  \<title>TITLE</title>\
  \</head>\
  \<body>\
  \<p>HEY</p>\
  \<br>\
  \<template>\
  \<br>\
  \<div></div>\
  \<p></p>\
  \<p>YOU</p>\
  \<img>\
  \</template>\
  \<!-- test markup -->\
  \<table class=\"class\" style=\"red\">\
  \<tbody>\
  \<tr>\
  \<td></td>\
  \<td id=\"2\">MEGADETH</td>\
  \</tr>\
  \</tbody>\
  \</table>\
  \</body>\
  \</html>"

testAA1 :: Test
testAA1 = testCase "html adoption agency 1" $ do
  assertEqual "TEST 1" htmlAA1Pretty $ genPretty htmlAA1In
  assertEqual "TEST 2" htmlAA1Normal $ genNormal htmlAA1In

htmlAA1In =
  "<!DOCTYPE html>\
  \<head><title>AA Case 1</title></head>\
  \<body>\
  \<p>1<b>2<i>3</b>4</i>5</p>\
  \</body>\
  \"

htmlAA1Pretty =
  "<!DOCTYPE html>\n\
  \<html>\n\
  \ <head>\n\
  \  <title>AA Case 1</title>\n\
  \ </head>\n\
  \ <body>\n\
  \  <p>\n\
  \   1\n\
  \   <b>\n\
  \    2\n\
  \    <i>3</i>\n\
  \   </b>\n\
  \   <i>4</i>\n\
  \   5\n\
  \  </p>\n\
  \ </body>\n\
  \</html>"

htmlAA1Normal =
  "<!DOCTYPE html>\
  \<html>\
  \<head>\
  \<title>AA Case 1</title>\
  \</head>\
  \<body>\
  \<p>\
  \1\
  \<b>\
  \2\
  \<i>3</i>\
  \</b>\
  \<i>4</i>\
  \5\
  \</p>\
  \</body>\
  \</html>"

testAA2 :: Test
testAA2 = testCase "html adoption agency 2" $ do
  assertEqual "TEST 1" htmlAA2Pretty $ genPretty htmlAA2In
  assertEqual "TEST 2" htmlAA2Normal $ genNormal htmlAA2In

htmlAA2In =
  "<!DOCTYPE html>\
  \<b>1<p>2</b>3</p>\
  \"

htmlAA2Pretty =
  "<!DOCTYPE html>\n\
  \<html>\n\
  \ <head></head>\n\
  \ <body>\n\
  \  <b>1</b>\n\
  \  <p>\n\
  \   <b>2</b>\n\
  \   3\n\
  \  </p>\n\
  \ </body>\n\
  \</html>"

htmlAA2Normal =
  "<!DOCTYPE html>\
  \<html>\
  \<head></head>\
  \<body>\
  \<b>1</b>\
  \<p>\
  \<b>2</b>\
  \3\
  \</p>\
  \</body>\
  \</html>"

testOmit :: Test
testOmit = testCase "html omit" $ do
  assertEqual "TEST 1" htmlOmitOut $ genNormal htmlOmitIn1
  assertEqual "TEST 2" htmlOmitOut $ genNormal htmlOmitIn2
  assertEqual "TEST 3" htmlOmit3Out $ genNormal htmlOmit3In

htmlOmitIn1 =
  "<!DOCTYPE HTML><title>Hello</title><p>example.</p>"

htmlOmitIn2 =
  "<!DOCTYPE HTML><title>Hello</title><p>example."

htmlOmitOut =
  "<!DOCTYPE html>\
  \<html><head><title>Hello</title>\
  \</head><body><p>example.</p></body></html>"

htmlOmit3In =
  "<!DOCTYPE HTML>\
  \<html lang=\"en\"><title>Hello</title>\
  \<body class=\"demo\"><p>example."

htmlOmit3Out =
  "<!DOCTYPE html>\
  \<html lang=\"en\"><head><title>Hello</title></head>\
  \<body class=\"demo\"><p>example.</p></body></html>"

testTableSparse :: Test
testTableSparse = testCase "html table sparse" $ do
  assertEqual "TEST 1"
    htmlTableSparseOut $ genNormal htmlTableSparseIn

htmlTableSparseIn =
  "<!DOCTYPE html>\
  \<table>\
  \ <caption>37547 TEE Electric Powered Rail Car Train Functions\
  \ <colgroup><col><col><col>\
  \ <thead>\
  \  <tr> <th>Function                    <th>Control Unit     <th>Central\
  \ <tbody>\
  \  <tr> <td>Headlights                  <td>✔                <td>✔\
  \  <tr> <td>Interior Lights             <td>✔                <td>✔\
  \  <tr> <td>Electric locomotive sounds  <td>✔                <td>✔\
  \  <tr> <td>Engineer's cab lighting     <td>                 <td>✔\
  \  <tr> <td>Station Announce - Swiss    <td>                 <td>✔\
  \</table>"

htmlTableSparseOut =
  "<!DOCTYPE html>\
  \<html><head></head><body>\
  \<table>\
  \ <caption>37547 TEE Electric Powered Rail Car Train Functions\
  \ </caption>\
  \<colgroup><col><col><col>\
  \ </colgroup><thead>\
  \  <tr> \
  \<th>Function                    </th>\
  \<th>Control Unit     </th>\
  \<th>Central </th>\
  \</tr>\
  \</thead><tbody>\
  \  <tr>\
  \ <td>Headlights                  </td>\
  \<td>\10004                </td>\
  \<td>\10004  </td>\
  \</tr><tr>\
  \ <td>Interior Lights             </td>\
  \<td>\10004                </td>\
  \<td>\10004  </td>\
  \</tr><tr>\
  \ <td>Electric locomotive sounds  </td>\
  \<td>\10004                </td>\
  \<td>\10004  </td>\
  \</tr><tr>\
  \ <td>Engineer's cab lighting     </td>\
  \<td>                 </td>\
  \<td>\10004  </td>\
  \</tr><tr>\
  \ <td>Station Announce - Swiss    </td>\
  \<td>                 </td>\
  \<td>\10004</td>\
  \</tr></tbody></table></body></html>"

testMath :: Test
testMath = testCase "html math" $ do
  assertEqual "TEST 1" htmlMathOut $ genNormal htmlMathIn

htmlMathIn =
  "<!DOCTYPE html>\
  \<p>You can add a string to a number, but this stringifies the number:</p>\
  \<math>\
  \<ms><![CDATA[x<y]]></ms>\
  \<mo>+</mo>\
  \<mn>3</mn>\
  \<mo>=</mo>\
  \<ms><![CDATA[x<y3]]></ms>\
  \</math>"

htmlMathOut =
  "<!DOCTYPE html>\
  \<html><head></head><body>\
  \<p>You can add a string to a number, but this stringifies the number:</p>\
  \<math>\
  \<ms>x&lt;y</ms>\
  \<mo>+</mo><mn>3</mn><mo>=</mo>\
  \<ms>x&lt;y3</ms>\
  \</math></body></html>"

testScript :: Test
testScript = testCase "html script" $ do
  assertEqual "TEST 1" htmlScript1Out $ genNormal htmlScript1In
  assertEqual "TEST 2" htmlScript2Out $ genNormal htmlScript2In
  assertEqual "TEST 3" htmlScript3Out $ genNormal htmlScript3In
  assertEqual "TEST 4" htmlScript4Out $ genNormal htmlScript4In

htmlScript1In =
  "<!DOCTYPE html>\
  \<script>\
  \document.write('<p>');\
  \</script>"

htmlScript1Out =
  "<!DOCTYPE html>\
  \<html>\
  \<head>\
  \<script>document.write('<p>');</script>\
  \</head>\
  \<body></body>\
  \</html>"

htmlScript2In =
  "<!DOCTYPE html>\
  \<div id=a>\
  \<script>\
  \ var div = document.getElementById('a');\
  \ parent.document.body.appendChild(div);\
  \</script>\
  \<script>\
  \ alert(document.URL);\
  \</script>\
  \</div>\
  \<script>\
  \ alert(document.URL);\
  \</script>"

htmlScript2Out =
  "<!DOCTYPE html>\
  \<html><head></head><body>\
  \<div id=\"a\">\
  \<script>\
  \ var div = document.getElementById('a');\
  \ parent.document.body.appendChild(div);\
  \</script>\
  \<script>\
  \ alert(document.URL);\
  \</script>\
  \</div>\
  \<script>\
  \ alert(document.URL);\
  \</script>\
  \</body></html>"

htmlScript3In =
  "<!DOCTYPE html>\
  \<body><script>\
  \ <!-- comm -->\
  \ var div = \"div\";\
  \ var d2 = \"<div>\";\
  \ var d3 = \"</div>\";\
  \</script></body>"

htmlScript3Out =
  "<!DOCTYPE html>\
  \<html><head></head><body>\
  \<script>\
  \ <!-- comm -->\
  \ var div = \"div\";\
  \ var d2 = \"<div>\";\
  \ var d3 = \"</div>\";\
  \</script>\
  \</body></html>"

htmlScript4In =
  [r|<script>
      var a = "<div id=\""+id+"\" class=\"ad "+divClass+"\">"+adString+"</div>";
     </script>|]

htmlScript4Out =
  [r|<html><head><script>
      var a = "<div id=\""+id+"\" class=\"ad "+divClass+"\">"+adString+"</div>";
     </script></head><body></body></html>|]

testInput :: Test
testInput = testCase "html input" $ do
  assertEqual "TEST 1" htmlInputOut $ genNormal htmlInputIn

htmlInputIn =
  "<!DOCTYPE html>\
  \<input disabled>\
  \<input value=yes>\
  \<input type='checkbox'>\
  \<input name=\"be evil\">"

htmlInputOut =
  "<!DOCTYPE html>\
  \<html><head></head><body>\
  \<input disabled=\"\">\
  \<input value=\"yes\">\
  \<input type=\"checkbox\">\
  \<input name=\"be evil\">\
  \</body></html>"

testList :: Test
testList = testCase "html list" $ do
  assertEqual "TEST 1" htmlList1Out $ genNormal htmlList1In

htmlList1In =
  "<!DOCTYPE html>\
  \<ul>\
  \<li>A\
  \<li>A\
  \<li>A\
  \</ul>"

htmlList1Out =
  "<!DOCTYPE html>\
  \<html><head></head><body>\
  \<ul><li>A</li><li>A</li><li>A</li></ul>\
  \</body></html>"

testScriptBeforeHead :: Test
testScriptBeforeHead = testCase "html script before head" $ do
  assertEqual "TEST 1"
    htmlScriptBeforeHeadOut $ genNormal htmlScriptBeforeHeadIn

htmlScriptBeforeHeadIn =
  "<!DOCTYPE html>\
  \<script type=\"text/javascript\">var a = 1;</script>\
  \<head></head>\
  \<body></body>\
  \"

htmlScriptBeforeHeadOut =
  "<!DOCTYPE html>\
  \<html>\
  \<head>\
  \<script type=\"text/javascript\">var a = 1;</script>\
  \</head>\
  \<body></body>\
  \</html>"

testComment :: Test
testComment = testCase "html comment" $ do
  assertEqual "TEST 1"
    htmlCommentOut $ genNormal htmlCommentIn

htmlCommentIn =
  "<!DOCTYPE html>\
  \<!--[if lt IE 7]>\
  \<html class=\"no-js ie ie6 lt-ie9 lt-ie8 lt-ie7\"\
  \ xmlns=\"http://www.w3.org/1999/xhtml\"\
  \ xmlns:og=\"http://opengraphprotocol.org/schema/\"\
  \ xmlns:fb=\"http://www.facebook.com/2008/fbml\"\
  \ lang=\"en-US\">\
  \ <![endif]-->\
  \<!-- start wired/views/global/partial-header-meta.php -->\
  \<head itemscope itemtype=\"http://schema.org/WebSite\"\
  \ profile=\"http://gmpg.org/xfn/11\">"

htmlCommentOut =
  "<!DOCTYPE html>\
  \<!--[if lt IE 7]>\
  \<html class=\"no-js ie ie6 lt-ie9 lt-ie8 lt-ie7\"\
  \ xmlns=\"http://www.w3.org/1999/xhtml\"\
  \ xmlns:og=\"http://opengraphprotocol.org/schema/\"\
  \ xmlns:fb=\"http://www.facebook.com/2008/fbml\"\
  \ lang=\"en-US\">\
  \ <![endif]-->\
  \<!-- start wired/views/global/partial-header-meta.php -->\
  \<html>\
  \<head itemscope=\"\"\
  \ itemtype=\"http://schema.org/WebSite\"\
  \ profile=\"http://gmpg.org/xfn/11\">\
  \</head><body></body></html>"

testEntity :: Test
testEntity = testCase "html entity" $ do
  assertEqual "TEST 1"
    htmlEntity1Out $ genEntity htmlEntity1In False
  assertEqual "TEST 2"
    htmlEntity2Out $ genEntity htmlEntity1In True
  assertEqual "TEST 3"
    htmlEntityNum1Out $ genEntity htmlEntityNumIn False
  assertEqual "TEST 4"
    htmlEntityNum2Out $ genEntity htmlEntityNumIn True

htmlEntity1In =
  "<!DOCTYPE html>\
  \<p id=\"&amp;\">&amp;</p>"

htmlEntity1Out =
  "<!DOCTYPE html>\
  \<html>\
  \<head>\
  \</head>\
  \<body>\
  \<p id=\"&\">&</p>\
  \</body>\
  \</html>"

htmlEntity2Out =
  "<!DOCTYPE html>\
  \<html>\
  \<head>\
  \</head>\
  \<body>\
  \<p id=\"&amp;\">&amp;</p>\
  \</body>\
  \</html>"

htmlEntityNumIn =
  "<!DOCTYPE html><p>&#x2212;&#x0394;&#160;</p>"

htmlEntityNum1Out =
  "<!DOCTYPE html><html><head></head><body>\
  \<p>\8722\916&nbsp;</p></body></html>"

htmlEntityNum2Out =
  "<!DOCTYPE html><html><head></head><body>\
  \<p>&#x2212;&#x0394;&#160;</p></body></html>"

testAfterHead :: Test
testAfterHead = testCase "html after head" $ do
  assertEqual "TEST 1" htmlAfterHead1Out $ genNormal htmlAfterHead1In

htmlAfterHead1In =
  "<!DOCTYPE html>\
  \<html>\
  \<head>\
  \</head>\
  \<script></script>\
  \<body>\
  \</body>\
  \</html>"

htmlAfterHead1Out =
  "<!DOCTYPE html>\
  \<html>\
  \<head>\
  \<script></script>\
  \</head>\
  \<body>\
  \</body>\
  \</html>"
