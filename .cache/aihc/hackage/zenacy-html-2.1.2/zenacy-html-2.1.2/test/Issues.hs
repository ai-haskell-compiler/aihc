{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Issues
  ( testIssues
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

testIssues :: Test
testIssues = testGroup "Issues"
  [ testIssue01
  ]

testIssue01 :: Test
testIssue01 = testCase "issue 01" $ do
  flip (assertEqual "Issue 1 case 1")
    (htmlRender $ htmlParseEasy
    "<html><body> <p><i><i>a<i>b<i></i></i></i></i></p> </body></html>")
    "<html><head></head><body> <p><i><i>a<i>b<i></i></i></i></i></p> </body></html>"
  flip (assertEqual "Issue 1 case 2")
    (htmlRender $ htmlParseEasy
    "<html><body>x<p><i><i>a<i>b<i></i></i></i></i></p>y</body></html>")
    "<html><head></head><body>x<p><i><i>a<i>b<i></i></i></i></i></p>y</body></html>"
