{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Regression tests for "Language.Haskell.TH.Syntax.Compat".
module Language.Haskell.TH.Syntax.CompatSpec (main, spec) where

import Control.Exception (evaluate)
import Control.Monad.State

import Language.Haskell.TH.Syntax hiding (newName)
import Language.Haskell.TH.Syntax.Compat

import Test.Hspec

import Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "newName" $
    it "works on something that isn't a Quasi instance" $
      runPureQ (newName "a") `shouldBe` mkNameU "a" 0

  describe "unsafeQToQuote" $ do
    it "works on Quasi-less expressions" $ do
      $(unsafeQToQuote [| "abc" |])         `shouldBe` "abc"
      runPureQ (unsafeQToQuote [| "abc" |]) `shouldBe` LitE (StringL "abc")

    it "errors on Quasi-ful expressions" $
      evaluate (runPureQ (unsafeQToQuote (qReport True "Explosion in 3... 2... 1...")))
        `shouldThrow` errorCall "`unsafeQToQuote` does not support code that uses qReport"

  describe "IsCode" $
    it "manipulates typed TH expressions in a backwards-compatible way" $
      $$(fromCode (toCode [|| "abc" ||])) `shouldBe` "abc"

  describe "joinSplice" $
    it "allows intermixing typed TH splices with monadic computations in a convenient way" $
      $$(joinSplice (do { x <- return "abc"; return [|| x ||] })) `shouldBe` "abc"

  describe "liftSplice" $
    it "allows intermixing typed TH splices with monadic computations in a convenient way" $
      $$(liftSplice (do { x <- return "abc"; examineSplice [|| x ||] })) `shouldBe` "abc"

  describe "liftTypedFromUntypedSplice" $
    it "allows defining liftTyped in a convenient, backwards-compatible way" $
      $$(liftTypedFromUntypedSplice MkFoo) `shouldBe` MkFoo

  describe "unTypeSplice" $
    it "allows unwrapping Code in a convenient, backwards-compatible way" $
      $$(unsafeSpliceCoerce (return . ListE =<< traverse unTypeSplice [ [|| "abc" ||] ]) :: SpliceQ [String])
        `shouldBe` ["abc"]

newtype PureQ a = MkPureQ (State Uniq a)
  deriving (Functor, Applicative, Monad, MonadState Uniq)

runPureQ :: PureQ a -> a
runPureQ m = case m of MkPureQ m' -> evalState m' 0

instance Quote PureQ where
  newName s = state $ \i -> (mkNameU s i, i + 1)
