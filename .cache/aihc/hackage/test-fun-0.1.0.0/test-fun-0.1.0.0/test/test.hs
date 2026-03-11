{-# LANGUAGE
    DeriveGeneric,
    RankNTypes,
    TypeApplications,
    TypeOperators #-}

module Main where

import Data.Foldable (for_)
import GHC.Generics (Generic)

import Test.Tasty
import Test.Tasty.HUnit

import Test.Fun.Internal.CoGen (Co)
import Test.Fun.Internal.Generic ((:+)(..), cogenGeneric)
import Test.Fun.Internal.Pretty
import Test.Fun.Internal.Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
  [ testFunctionPretty
  , testFunctionApply
  ]

prettyFun_ :: (a :-> String) -> String
prettyFun_ = prettyFun tConst

testFunctionPretty :: TestTree
testFunctionPretty = testGroup "pretty"
  [ testCase "case"
      $ "case a0 :: Either _ _ of { Left a1 -> 0 ; Right a1 -> case a1 of {} }"
      @=? prettyFun_
        (Case "Either _ _" id
          (Alt
            (Pat "Left" (Field (NoField (Const "0"))))
            (Pat "Right" (Field (NoField (Absurd id))))) "0")
  , testCase "coapply"
      $ "case a0 0 of {}"
      @=? prettyFun_ (CoApply hardConcrete (0 :: Int) id (Absurd id))
  , testCase "apply"
      $ "case f a0 of {}"
      @=? prettyFun_ (Apply "f" id (Absurd id))
  , testCase "case-Integer"
      $ "case a0 :: Integer of { -1 -> -1 ; 0 -> 0 ; 1 -> 1 ; _ -> 2 }"
      @=? prettyFun_
        (CaseInteger "Integer" id
          (binAlt "0" (binAlt "1" z z) (binAlt "-1" z z)) "2")
  , testCase "case-Integer-big"
      $ "case a0 :: Integer of { "
        ++ concat [ m ++ " -> " ++ m ++ " ; " | n <- [-7 .. 7 :: Int], let m = show n ]
        ++ "_ -> 22 }"
      @=? prettyFun_ bigFun
  ]

testFunctionApply :: TestTree
testFunctionApply = testCase "apply" $ do
  for_ ([-7 .. 7]) $ \i -> do
    show i @=? applyFun bigFun i

bigFun :: Integer :-> String
bigFun = CaseInteger "Integer" id
  (binAlt "0"
    (binAlt "1"
      (binAlt "2" (binAlt "4" z z) (binAlt "6" z z))
      (binAlt "3" (binAlt "5" z z) (binAlt "7" z z)))
    (binAlt "-1"
      (binAlt "-2" (binAlt "-4" z z) (binAlt "-6" z z))
      (binAlt "-3" (binAlt "-5" z z) (binAlt "-7" z z)))) "22"

z :: Bin r
z = BinEmpty

-- Examples

data These a b = This a | That b | Those a b
  deriving Generic

cogenThese ::
  forall a b gen.
  Applicative gen =>
  (forall r. Co gen a r) ->
  (forall r. Co gen b r) ->
  (forall r. Co gen (These a b) r)
cogenThese cogenA cogenB = cogenGeneric cs where
  cs = cogenA :+ cogenB :+ (cogenA . cogenB) :+ ()

data Small a = Zero | One a | Two a a
  deriving Generic

cogenSmall ::
  forall a gen.
  Applicative gen =>
  (forall r. Co gen a r) ->
  (forall r. Co gen (Small a) r)
cogenSmall cogenA = cogenGeneric cs where
  cs = id :+ cogenA :+ (cogenA . cogenA) :+ ()
