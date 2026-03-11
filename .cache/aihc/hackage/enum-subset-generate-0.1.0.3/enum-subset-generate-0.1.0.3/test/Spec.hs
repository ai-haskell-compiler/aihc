{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

import Generic.Random
import GHC.Generics(Generic)
import Test.Hspec
import Test.QuickCheck

import Data.MakeEnum
import Data.MakeEnum.Options

import Enum

makeEnumWith ''OtherModuleEnum ['OMEUnknown]
  defaultOptions { newEnumName = Just "OtherModuleEnumDerived"
                 , deriveClasses = [''Eq, ''Ord, ''Show, ''Generic]
                 }

instance Arbitrary OtherModuleEnumDerived where
  arbitrary = genericArbitrary uniform

data SameModuleEnum = SMEUnknown | SMEVar1 | SMEVar2 | SMEVar3 deriving (Eq, Ord, Show)

makeEnumWith ''SameModuleEnum ['SMEUnknown]
  defaultOptions { newEnumName = Just "SameModuleEnumDerived"
                 , ctorNameModifier = ("Der" ++)
                 , deriveClasses = [''Eq, ''Ord, ''Show, ''Generic]
                 }

instance Arbitrary SameModuleEnumDerived where
  arbitrary = genericArbitrary uniform

data EnumWithFields = EWFVar1 String
                    | EWFVar2 Int Int
                    | EWFUnknown
                    deriving (Eq, Ord, Show)

makeEnumWith ''EnumWithFields ['EWFUnknown]
  defaultOptions { newEnumName = Just "EnumWithFieldsDerived"
                 , ctorNameModifier = ("Der" ++)
                 , deriveClasses = [''Eq, ''Ord, ''Show, ''Generic]
                 }

instance Arbitrary EnumWithFieldsDerived where
  arbitrary = genericArbitrary uniform

runTest :: (Arbitrary d, Eq d, Show d) => String -> s -> (s -> Maybe d) -> (d -> s) -> SpecWith ()
runTest str missing from to = describe str $ do
  it "maps missing cases correctly" $ from missing `shouldBe` Nothing
  it "converts the subset fine" $ property $ \x -> from (to x) == Just x

main :: IO ()
main = hspec $ do
  runTest "OtherModuleEnum" OMEUnknown fromOtherModuleEnum toOtherModuleEnum
  runTest "SameModuleEnum" SMEUnknown fromSameModuleEnum toSameModuleEnum
  runTest "EnumWithFields" EWFUnknown fromEnumWithFields toEnumWithFields
