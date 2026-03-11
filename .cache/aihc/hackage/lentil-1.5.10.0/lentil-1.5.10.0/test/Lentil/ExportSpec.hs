module Lentil.ExportSpec where


import Test.Hspec

import Lentil.Types
import Lentil.Export


is :: [Issue]
is = [Issue "./file" 1 (Just "da") [Tag "a", Tag "c"],
      Issue "file" 2 (Just "db") [Tag "e:f"]]

csv :: String
csv = "\"Filepath\",\"Row\",\"Description\",\"Tags\"\n\
      \\"file\",\"1\",\"da\",\"a c\"\n\
      \\"file\",\"2\",\"db\",\"e:f\""

cmp :: String
cmp = unlines
        ["file:1: da [a] [c]",
         "file:2: db [e:f]"]

xml :: String
xml = concat
        ["<issues><file><filename><![CDATA[file]]></filename>"
        , "<issue><row>1</row><description><![CDATA[da]]></description>"
        , "<tags><tag>a</tag><tag>c</tag></tags>"
        , "</issue>"
        , "<issue><row>2</row><description><![CDATA[db]]></description>"
        , "<tags><tag>e:f</tag></tags>"
        , "</issue>"
        , "</file></issues>", "\n"]

file :: String
file = unlines
         ["./file",
          "file"]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "tags2String" $ do
    it "converts tags to plain string, intercalated by ' '" $
      tags2String [Tag "a", Tag "b"] `shouldBe` "a b"

  describe "tags2StringPretty" $ do
    it "converts tags to string, with open/close delimiters" $
      tags2StringPretty [Tag "a", Tag "b"] `shouldBe` "[a] [b]"

  describe "issues2CSV" $ do
    it "exports issues to CSV" $
      issues2CSV is `shouldBe` csv

  describe "issues2Compiler" $ do
    it "exports issues to compiler-like output format" $
      issues2Compiler is `shouldBe` cmp

  describe "issues2Xml" $ do
    it "exports issues to xml output format" $
      issues2Xml is `shouldBe` xml

  describe "issues2File" $ do
    it "exports file list" $
      issues2File is `shouldBe` file
    it "does not duplicate files" $
      issues2File (is++is) `shouldBe` file

