{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Attoparsec.ByteString.Streaming as AS
import qualified Data.ByteString.Streaming as BS
import           Streaming
import qualified Streaming.Prelude as S
import           Test.Tasty
import           Test.Tasty.HUnit

---

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testCase "parse" $ do
      (r,rest1) <- AS.parse (A.scientific <* A.many' A.space) "12.3  4.56  78.3"
      (s,rest2) <- AS.parse (A.scientific <* A.many' A.space) rest1
      (t,rest3) <- AS.parse (A.scientific <* A.many' A.space) rest2
      r @?= Right 12.3
      s @?= Right 4.56
      t @?= Right 78.3
      BS.null_ rest3 >>= assertBool "Should be empty"
  , testCase "Simple parsing" $ do
      s <- S.sum_ . void . AS.parsed (A.scientific <* A.many' A.space) $ "12.3  4.56  78.9"
      s @?= (12.3 + 4.56 + 78.9)
  ]
