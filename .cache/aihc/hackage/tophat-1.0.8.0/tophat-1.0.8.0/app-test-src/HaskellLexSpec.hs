module HaskellLexSpec where

import Data.List (foldl')
import Test.Hspec
import HaskellLex


evolve :: SourceState -> String -> SourceState
evolve = foldl' evolveSource

spec :: Spec
spec =

  describe "evolveSource" $ do

    it "enters line comments" $
      evolve InCode "x + 1 -- comment" `shouldBe` InLineComment
    
    it "leaves line comments" $
      evolve InCode "x + 1 -- comment\ny + 1" `shouldBe` InCode

    it "enters block comments" $
      evolve InCode "x + 1 {- comment" `shouldBe` InBlockComment 0
