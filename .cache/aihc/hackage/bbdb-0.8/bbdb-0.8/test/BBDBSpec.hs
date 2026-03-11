module BBDBSpec where

import Database.BBDB
import Test.Hspec

spec :: Spec
spec = do
  bbdbString <- runIO $ readFile "test/sampleData.txt"
  describe "Read BBDB file" $ do
    let
      Right bbdb = parseBBDB bbdbString
      b = justEntries bbdb
      me = head b
    it "can parse the sample file as a String" $ do
      (length bbdbString) `shouldBe` 942
      (length bbdb) `shouldBe` 4
    it "can write it back as a lisp string" $ do
      let w = asLisp bbdb
      w == bbdbString `shouldBe` True
    it "found my first name" $ do
      firstName me `shouldBe` (Just "Henry")
    it "found my email"  $ do 
      net me `shouldBe` (Just ["nadine.and.henry@pobox.com"])
    it "found the tennis note" $ do
      getNote "tennis" me `shouldBe` (Just "chapala")
    it "did not find the xyzzy note" $ do
      getNote "xyzzy" me `shouldBe` Nothing
    it "found the generic record" $ do
      let
        wanted x = lastName x == Just "lastname"
        r = filterBBDB wanted bbdb
      length r `shouldBe` 1
