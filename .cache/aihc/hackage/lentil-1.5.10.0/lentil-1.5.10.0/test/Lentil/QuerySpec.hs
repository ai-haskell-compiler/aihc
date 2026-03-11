module Lentil.QuerySpec where

import Test.Hspec

import Lentil.Types
import Lentil.Query

-- Parsing tests

main :: IO ()
main = hspec spec


spec :: Spec
spec = do

  let i1 = Issue "a.hs" 1 (Just "issue 1")  [Tag "ta", Tag "fl:va"]
      i2 = Issue "a.hs" 2 (Just "issue 12") [Tag "tb"]
      i3 = Issue "b.hs" 1 (Just "issue 3")  [Tag "tb", Tag "fl:vb"]
      ix = Issue "b.hs" 1 (Just "issue n")  []
      iz = Issue "b.hs" 1 Nothing           []
      is = [i1, i2, i3]

  describe "filter*" $ do
    it "filters by Filepath" $
      filterFilepath "a.hs" is `shouldBe` [i1, i2]
    it "filters by Description" $
      filterDescription "3" is `shouldBe` [i3]
    it "filters by Tag" $
      filterTags "tb" is `shouldBe` [i2, i3]
    it "filters by Tag 'label'" $
      filterTags "fl:" is `shouldBe` [i1, i3]
    it "filters by using regexps" $
      filterTags "(ta|tb)" is `shouldBe` [i1, i2, i3]

  describe "negFilter" $ do
    it "negates a filter" $
      negFilter (filterTags "fl:") is `shouldBe` [i2]
    it "cancels itself on a double negation" $
      negFilter (negFilter (filterTags "fl:")) is
                `shouldBe` filterTags "fl:" is
    it "filters by empty values (description)" $
      negFilter (filterDescription ".") (iz:is) `shouldBe` [iz]
    it "filters by empty values (tags)" $
      negFilter (filterTags ".") (ix:is) `shouldBe` [ix]

  describe "filter(And|Or)" $ do
    it "chains (boolean and) multiple filters" $
      filterAnd [filterFilepath "a.hs", filterTags "b"] is
                `shouldBe` [i2]
    it "chains (boolean or) multiple filters" $
      filterOr [filterFilepath "a.hs", filterTags "b"] is
                `shouldBe` [i1, i2, i3]

  describe "sortIssues" $ do
    it "sorts Issues" $
      sortIssues iFile Asc is `shouldBe` [i1, i2, i3]
    it "sorts Issues in descending order" $
      sortIssues iFile Desc is `shouldBe` [i3, i2, i1]
    it "smart sorts strings containing naturals" $
      sortIssues (maybe "" id . iDesc) Asc is `shouldBe` [i1, i3, i2]

  describe "sortTag" $ do
    it "sorts by partial tag, empty fields first" $
      sortTag "fl:" Asc is `shouldBe` [i2, i1, i3]

  describe "chainSorts" $ do
    it "sequences sort functions" $
      chainSorts is [sortIssues iFile Desc,
                     sortIssues (maybe "" id . iDesc) Desc]
                 `shouldBe` [i3, i2, i1]

  describe "groupIssues" $ do
    it "group isses" $
      groupIssues iFile is `shouldBe` [[i1, i2], [i3]]

  describe "tagPop" $ do
    it "outputs tag popularity list" $
      tagPop is `shouldBe` [(Tag "tb", 2), (Tag "ta", 1),
                            (Tag "fl:vb", 1), (Tag "fl:va", 1)]

