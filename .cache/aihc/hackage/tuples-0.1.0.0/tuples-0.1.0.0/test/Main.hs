{-# language DerivingStrategies #-}
{-# language DerivingVia #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.Tuple.Types (IntPair(..),WordPair(..))
import Data.Tuple.Types (IntTriple(..),WordTriple(..))
import Data.Tuple.Types (DoubleTriple(..),DoublePair(..))

import Control.Applicative (liftA2,liftA3)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable,typeRep)
import Test.QuickCheck (Arbitrary,arbitrary)
import Test.QuickCheck.Classes (primLaws)
import Test.Tasty (TestTree,defaultMain,testGroup)

import qualified Test.QuickCheck.Classes as QCC
import qualified Test.Tasty.QuickCheck as TQC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tuples"
  [ laws @IntTriple [primLaws]
  , laws @WordTriple [primLaws]
  , laws @DoubleTriple [primLaws]
  , laws @IntPair [primLaws]
  , laws @WordPair [primLaws]
  , laws @DoublePair [primLaws]
  ]

laws :: forall a. Typeable a => [Proxy a -> QCC.Laws] -> TestTree
laws = testGroup (show (typeRep (Proxy :: Proxy a))) . map
  ( \f -> let QCC.Laws name pairs = f (Proxy :: Proxy a) in
    testGroup name (map (uncurry TQC.testProperty) pairs)
  )

instance Arbitrary IntTriple where
  arbitrary = liftA3 IntTriple arbitrary arbitrary arbitrary
instance Arbitrary WordTriple where
  arbitrary = liftA3 WordTriple arbitrary arbitrary arbitrary
instance Arbitrary DoubleTriple where
  arbitrary = liftA3 DoubleTriple arbitrary arbitrary arbitrary
instance Arbitrary DoublePair where
  arbitrary = liftA2 DoublePair arbitrary arbitrary
instance Arbitrary IntPair where
  arbitrary = liftA2 IntPair arbitrary arbitrary
instance Arbitrary WordPair where
  arbitrary = liftA2 WordPair arbitrary arbitrary

