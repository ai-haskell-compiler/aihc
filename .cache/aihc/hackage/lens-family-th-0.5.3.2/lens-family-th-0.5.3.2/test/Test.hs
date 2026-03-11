{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Char as Char
import qualified Lens.Family2.TH as LFTH

import Data.Functor.Constant (Constant(..))
import Data.Functor.Identity (Identity(..))
import Test.Hspec (hspec, describe, it, shouldBe)


-- operators copied from lens-family-core
x ^. l = getConstant $ l Constant x
l %~ f = runIdentity . l (Identity . f)
l .~ b = l %~ const b


data Pair a b = Pair { _pairL :: a, _pairR :: b }
              deriving (Eq, Show, Read, Ord)
$(LFTH.makeLenses ''Pair)

data Opt b c d = A | B b | CD c d Int
               deriving (Eq, Show, Read, Ord)
$(LFTH.makeTraversals ''Opt)

type OptInts = Opt Int Int Int

p :: Pair Char Int
p = Pair '1' 1

main = hspec $ do
  describe "makeLenses" $ do
    it "makes lenses that function with lens-family operators" $ do
      (p ^. pairL) `shouldBe` '1'
      (p ^. pairR) `shouldBe` (1 :: Int)
      ((pairL %~ Char.digitToInt) p) `shouldBe` (Pair 1 1 :: Pair Int Int)
      ((pairR %~ Char.intToDigit) p) `shouldBe` Pair '1' '1'
      ((pairL .~ "foo") p) `shouldBe` Pair "foo" (1 :: Int)
      ((pairR .~ "bar") p) `shouldBe` Pair '1' "bar"
  describe "makeTraversals" $ do
    it "makes traversals that function with lens-family operators" $ do
      (_B %~ (+ (1 :: Int)) $ (A :: OptInts)) `shouldBe` (A :: OptInts)
      (_B %~ (+ (1 :: Int)) $ (B 3 :: OptInts)) `shouldBe` (B 4 :: OptInts)
      (_B %~ (+ (1 :: Int)) $ (CD 3 4 5 :: OptInts))
        `shouldBe`  (CD 3 4 5 :: OptInts)
