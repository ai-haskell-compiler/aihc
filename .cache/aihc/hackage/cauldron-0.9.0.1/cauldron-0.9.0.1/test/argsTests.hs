{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import Cauldron.Args
import Control.Exception
import Data.Dynamic
import Data.Function ((&))
import Data.Proxy
import Data.Typeable (typeRep)
import Test.Tasty
import Test.Tasty.HUnit

type Text = String

data A = A

data B = B

data C = C

makeC :: A -> B -> ([Text], C)
makeC _ _ = (["monoid"], C)

argsForC :: Args (Regs C)
argsForC = do
  ~(reg1, bean) <- makeC <$> arg <*> arg
  tell1 <- foretellReg
  pure do
    tell1 reg1
    pure bean

data L1 = L1

data L2 = L2

makeL2 :: L1 -> L2
makeL2 !L1 = L2

throwyArgs :: Args L2
throwyArgs = makeL2 <$> arg

tests :: TestTree
tests =
  testGroup
    "All"
    [ testCase "withRegs" do
        let (beans, C) =
              argsForC
                & runArgs (taste $ fromDynList [toDyn A, toDyn B])
                & runRegs (getRegsReps argsForC)
        Just m <- pure do taste @[Text] beans
        assertEqual
          "monoid"
          ["monoid"]
          m,
      testCase "throwy" do
        r <- try $ evaluate $ runArgs Nothing throwyArgs
        case r of
          Left (LazilyReadBeanMissing tr) | tr == (typeRep (Proxy @L1)) -> pure ()
          _ -> assertFailure "expected exception did not happen"
    ]

main :: IO ()
main = defaultMain tests
