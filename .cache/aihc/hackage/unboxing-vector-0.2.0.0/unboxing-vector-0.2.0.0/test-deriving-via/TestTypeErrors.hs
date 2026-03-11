-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}
module TestTypeErrors where
import Test.HUnit
import Test.ShouldNotTypecheck
import qualified Data.Vector.Unboxing as V
import GHC.Generics

data Animal = Dog | Cat
  deriving (Eq,Show,Generic)
  deriving V.Unboxable via V.Generics Animal

testTypeErrors :: Test
testTypeErrors = TestList [TestLabel "Test generic deriving for a sum type" $ TestCase $ shouldNotTypecheck (V.singleton Dog)
                          ]

