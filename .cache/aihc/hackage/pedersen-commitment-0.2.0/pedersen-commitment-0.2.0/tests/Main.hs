{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (
  main,
) where

import Protolude

import qualified Crypto.PubKey.ECC.Prim as ECC

import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic as QM

import Example (micpWrapper, micpComponents)

import Pedersen
import PrimeField
import qualified VectorPedersen as VP

suite :: TestTree
suite = testGroup "Test Suite" [
    testGroup "Units"
      [vectorPedersenTests]
      -- [ pedersenTests
      -- , micpTests
      -- , vectorPedersenTests
      -- ]
  ]

pedersenTests :: TestTree
pedersenTests = testGroup "Pedersen Commitment Scheme"
  [ localOption (QuickCheckTests 50) $
      testProperty "x == Open(Commit(x),r)" $ monadicIO $ do
        (a, cp) <- liftIO $ setup 256
        x <- liftIO $ randomInZq $ pedersenSPF cp
        pc <- liftIO $ commit x cp
        QM.assert $ open cp (commitment pc) (reveal pc)

  , testCaseSteps "Additive Homomorphic Commitments" $ \step -> do
      step "Generating commit params..."
      (a,cp) <- setup 256
      let spf = pedersenSPF cp

      step "Generating two random numbers in Zp to commit..."
      x <- randomInZq spf
      y <- randomInZq spf

      step "Committing the two random numbers..."
      px@(Pedersen cx rx) <- commit x cp
      py@(Pedersen cy ry) <- commit y cp

      step "Verifying Additive Homomorphic property..."
      let cz = addCommitments cp cx cy
      let pz = verifyAddCommitments cp px py
      assertAddHomo $ cz == commitment pz

  , testProperty "x == Open(Commit(x),r) (EC) " $
      monadicIO $ do
        cp <- liftIO $ ecSetup Nothing -- uses SECP256k1 by default
        x <- liftIO $ ECC.scalarGenerate $ ecCurve cp
        pc <- liftIO $ ecCommit x cp
        QM.assert $ ecOpen cp (ecCommitment pc) (ecReveal pc)

  , testCaseSteps "Additive Homomorphic Commitments (EC) " $ \step -> do
      step "Generating commit params..."
      ecp <- ecSetup Nothing
      let curve = ecCurve ecp

      step "Generating two random numbers in Ep (EC prime field order q)..."
      x <- ECC.scalarGenerate curve
      y <- ECC.scalarGenerate curve

      step "Committing the two random numbers..."
      px@(ECPedersen cx rx) <- ecCommit x ecp
      py@(ECPedersen cy ry) <- ecCommit y ecp

      step "Verifying Additive Homomorphic property..."
      let cz = ecAddCommitments ecp cx cy
      let pz = ecVerifyAddCommitments ecp px py
      assertAddHomo $ cz == ecCommitment pz

  , testCaseSteps "Additive Homomorphic property (EC) | nG + C(x) == (x + n)G + rH" $ \step -> do
      step "Generating commit params..."
      ecp <- ecSetup Nothing
      let curve = ecCurve ecp

      step "Generating a random number to commit..."
      x <- ECC.scalarGenerate curve
      step "Committing the the random number..."
      px@(ECPedersen cx rx) <- ecCommit x ecp

      step "Generating a random number to add to the commitment..."
      n <- ECC.scalarGenerate curve

      step "Verifying the Additive homomorphic property"
      let cy = ecAddInteger ecp cx n
      let py = ecVerifyAddInteger ecp px n
      assertAddHomo $ cy == ecCommitment py

  ]
  where
    assertAddHomo :: Bool -> IO ()
    assertAddHomo = assertBool "Additive homomorphic property doesn't hold."

micpTests :: TestTree
micpTests = testGroup "Mutually Independent Commitment Protocol"
  [ testCase "Testing MICP Components" $
      assertBool "MICP Components test failed!" =<< micpComponents 256
  , testCase "Testing MICP Wrapper" $
      assertBool "MICP Wrapper test failed!" =<< micpWrapper 256
  ]


vectorPedersenTests :: TestTree
vectorPedersenTests = testGroup "Vector Pedersen Commitment"
  [
  localOption (QuickCheckTests 10) $
      testProperty "xs == Open(Commit(xs),r) (EC) " $ \n ->
        monadicIO $ do
          cp <- liftIO $ ecSetup Nothing -- uses SECP256k1 by default
          v <- liftIO $ VP.scalarGenerateN (ecCurve cp) n
          pc <- liftIO $ VP.ecCommit v cp
          QM.assert $ VP.ecOpen cp (VP.ecCommitment pc) (VP.ecReveal pc)

  , localOption (QuickCheckTests 10) $
      testProperty "Additive Homomorphic Commitments (EC) " $ \n ->
        monadicIO $ do
          cp <- liftIO $ ecSetup Nothing
          v <- liftIO $ VP.scalarGenerateN (ecCurve cp) n
          w <- liftIO $ VP.scalarGenerateN (ecCurve cp) n

          pv@(VP.ECPedersen cv rv) <- liftIO $ VP.ecCommit v cp
          pw@(VP.ECPedersen cw rw) <- liftIO $ VP.ecCommit w cp

          let cz = ecAddCommitments cp cv cw
          let pz = VP.ecVerifyAddCommitments cp pv pw
          QM.assert $ cz == VP.ecCommitment pz

   , localOption (QuickCheckTests 10) $
      testProperty "Additive Homomorphic property (EC) | wG + C(v) == (v + w)G + rH" $ \n ->
        monadicIO $ do
          cp <- liftIO $ ecSetup Nothing

          v <- liftIO $ VP.scalarGenerateN (ecCurve cp) n
          pv@(VP.ECPedersen cv rv) <- liftIO $ VP.ecCommit v cp

          w <- liftIO $ VP.scalarGenerateN (ecCurve cp) n

          let cw = VP.ecAddVector cp cv w
          let pw = VP.ecVerifyAddVector cp pv w

          QM.assert $ cw == VP.ecCommitment pw
    ]


main :: IO ()
main = defaultMain suite
