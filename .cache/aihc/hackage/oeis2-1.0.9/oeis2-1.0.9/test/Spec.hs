{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson.Lens
import           Data.Aeson.Types
import           Data.List
import           Data.Maybe            (fromJust)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import qualified Data.Vector           as V
import           System.IO
import           System.IO.Unsafe      (unsafePerformIO)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Gen

import           Math.OEIS
import           Math.OEIS.Internal


main :: IO ()
main = do
  hspec specLookupSeq
  hspec specGetSeqData
  hspec specExtendSeq

specLookupSeq :: Spec
specLookupSeq = describe "Test for lookupSeq" $ do
  it "Compare lookup by ID with by SubSeq" $
    lookupSeq (ID "A000027") `shouldBe` lookupSeq (SubSeq [1, 2, 3, 4, 5, 6, 7])
  modifyMaxSuccess (const 10) $ prop "Compare lookup by ID with by SubSeq -2" $
    \x ->
    let rs   = unsafePerformIO $ generate $ resize 5 $ vectorOf x $ choose (1, 10)
        seq1 = lookupSeq (SubSeq rs)
        num  = maybe "" number seq1
        seq2 = lookupSeq (ID num)
    in seq2 `shouldBe` seq1
  it "Get Maple function" $
    maple <$> lookupSeq (ID "A000027") `shouldBe`
    Just ["A000027 := n->n; seq(A000027(n), n=1..100);"]
  it "No search results" $
    lookupSeq (SubSeq [1,3,4,5,4,3,6,10000000]) `shouldBe` Nothing

specGetSeqData :: Spec
specGetSeqData = describe "Test for getSeqData" $ do
  it "Get SeqData" $
    getSeqData (ID "A000027")`shouldBe` Just [1..77]
  it "No SeqData" $
    getSeqData (SubSeq [1,3,4,5,4,3,6,10000000]) `shouldBe` Nothing

specExtendSeq :: Spec
specExtendSeq = describe "Test for extendSeq" $ do
  modifyMaxSuccess (const 10) $ prop "Extend Seq" $
    \x ->
    let rs = unsafePerformIO $ generate $ resize 5 $ vectorOf x $ choose (1, 10)
        seq = extendSeq rs
    in rs `isInfixOf` seq
  it "No extension" $
    extendSeq [1,3,4,5,4,3,6,10000000] `shouldBe` [1,3,4,5,4,3,6,10000000]
