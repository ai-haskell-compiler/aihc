{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Examples
  ( testExamples
  ) where

import Test.Framework
  ( Test
  , testGroup
  )
import Test.Framework.Providers.HUnit
  ( testCase
  )
import Test.HUnit
  ( assertEqual
  )
import Data.Text
  ( Text
  )
import qualified Data.Text as T
  ( pack
  )
import Zenacy.HTML

testExamples :: Test
testExamples = testGroup "Examples"
  [ testMisnested
  , testUnexpected
  , testUnclosed
  ]

testMisnested :: Test
testMisnested = testCase "Misnested tags" $ do
  flip (assertEqual "13.2.10.1 Misnested tags")
    (htmlRender $ htmlParseEasy
    "<html><head></head><body><p>1<b>2<i>3</b>4</i>5</p></body></html>")
    "<html><head></head><body><p>1<b>2<i>3</i></b><i>4</i>5</p></body></html>"
  flip (assertEqual "13.2.10.2 Misnested tags")
    (htmlRender $ htmlParseEasy
    "<html><head></head><body><b>1<p>2</b>3</p></body></html>")
    "<html><head></head><body><b>1</b><p><b>2</b>3</p></body></html>"

testUnexpected :: Test
testUnexpected = testCase "Unexpected markup in tables" $ do
  flip (assertEqual "13.2.10.3 Unexpected markup in tables")
    (htmlRender $ htmlParseEasy
    "<html><head></head><body><table><b><tr><td>aaa</td></tr>bbb</table>ccc</body></html>")
    "<html><head></head><body>\
    \<b></b><b>bbb</b><table><tbody><tr><td>aaa</td></tr></tbody></table><b>ccc</b>\
    \</body></html>"

testUnclosed :: Test
testUnclosed = testCase "Unclosed formatting elements" $ do
  flip (assertEqual "13.2.10.6 Unclosed formatting elements")
    (htmlRender $ htmlParseEasy
    "<!DOCTYPE html>\
    \<p><b class=x><b class=x><b><b class=x><b class=x><b>X\
    \<p>X\
    \<p><b><b class=x><b>X\
    \<p></b></b></b></b></b></b>X")

    "<!DOCTYPE html><html><head></head><body>\
    \<p>\
      \<b class=\"x\">\
        \<b class=\"x\">\
          \<b>\
            \<b class=\"x\">\
              \<b class=\"x\">\
                \<b>X</b>\
              \</b>\
            \</b>\
          \</b>\
        \</b>\
      \</b>\
    \</p>\
    \<p>\
      \<b class=\"x\">\
        \<b>\
          \<b class=\"x\">\
            \<b class=\"x\">\
              \<b>X</b>\
            \</b>\
          \</b>\
        \</b>\
      \</b>\
    \</p>\
    \<p>\
      \<b class=\"x\">\
        \<b>\
          \<b class=\"x\">\
            \<b class=\"x\">\
              \<b>\
                \<b>\
                  \<b class=\"x\">\
                    \<b>X</b>\
                  \</b>\
                \</b>\
              \</b>\
            \</b>\
          \</b>\
        \</b>\
      \</b>\
    \</p>\
    \<p>X</p>\
    \</body></html>"
