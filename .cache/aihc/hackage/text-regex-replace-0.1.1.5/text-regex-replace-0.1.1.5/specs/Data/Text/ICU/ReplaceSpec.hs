{-# LANGUAGE OverloadedStrings #-}


module Data.Text.ICU.ReplaceSpec where


import           Data.Text.ICU.Replace

import           Test.Hspec


spec :: Spec
spec = do
    describe "replace" $ do
        it "should replace the first match." $
            replace "a" "b" " aaa" `shouldBe` " baa"
        it "should include the prefix before the first match." $
            replace "\\d{2}" "R1" "abc: 10 12" `shouldBe` "abc: R1 12"
    describe "replaceAll" $ do
        it "should replace all matches." $
            replaceAll "a" "b" " aaa" `shouldBe` " bbb"
        it "should replace all matches with stuff in between." $
            replaceAll "a" "b" " a a a " `shouldBe` " b b b "
        it "should include the prefix before the first match." $
            replaceAll "\\d{2}" "R1" "abc: 10 12" `shouldBe` "abc: R1 R1"
        it "should include the prefix before the last match." $
            replaceAll "\\d{2}" "R1" "abc: 10 12 14" `shouldBe` "abc: R1 R1 R1"

    describe "rgroup" $
        it "should create a Replace that replaces an RE group." $
            let r = rgroup 1
            in  replace "a(b)c" r "abcabcabc" `shouldBe` "babcabc"
    describe "rtext" $
        it "should create a Replace that replaces with a static Text." $
            let r = rtext "d"
            in  replace "a(b)c" r "abcabcabc" `shouldBe` "dabcabc"
    describe "rstring" $
        it "should create a Replace that replaces with a static String." $
            let r = rstring "e"
            in  replace "a(b)c" r "abcabcabc" `shouldBe` "eabcabc"
    describe "rfn" $
        it "should create a Replace that uses a function that returns a Builder." $
            let r = rfn $ const "f"
            in  replace "a(b)c" r "abcabcabc" `shouldBe` "fabcabc"
    describe "rtfn" $
        it "should create a Replace that uses a function that returns a Text." $
            let r = rtfn $ const "g"
            in  replace "a(b)c" r "abcabcabc" `shouldBe` "gabcabc"
    describe "rbuilder" $
        it "should create a Replace that replaces with a static Builder." $
            let r = rbuilder "h"
            in  replace "a(b)c" r "abcabcabc" `shouldBe` "habcabc"

    describe "parser" $ do
        it "should create a Replace that has replaces with a static Text." $
            replace "a(b)c" "xyz" "abcabcabc" `shouldBe` "xyzabcabc"
        it "should create a Replace that replaces with an RE group." $
            replace "a(b)c" "$1" "abcabcabc" `shouldBe` "babcabc"
        it "should create a Replace that replaces with a dollar sign." $
            replace "a(b)c" "$$" "abcabcabc" `shouldBe` "$abcabc"
        it "should create a Replace that combines other Replaces." $
            replace "a(b)c" "$$ hi $1 bye " "abcabcabc" `shouldBe`
                "$ hi b bye abcabc"

        it "should handle examples in the comments." $ do
            replaceAll "a" "b" "aaa" `shouldBe` "bbb"
            replaceAll "ab" "ba" "cdababcd" `shouldBe` "cdbabacd"
            replaceAll "(.*), (.*)" "$2 $1" "Beeblebrox, Zaphod" `shouldBe`
                "Zaphod Beeblebrox"
            replaceAll "4(\\d)" "${1}4" "7458" `shouldBe` "7548"
            replaceAll "(\\d+\\.\\d+)" "$$$1" "9.99" `shouldBe` "$9.99"
