module Main where

import Protolude

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic

import qualified Data.ByteString as BS

import Crypto.Hash.MerkleTree

newtype MerkleTest = MerkleTest Int
  deriving (Show)

instance Arbitrary MerkleTest where
  arbitrary = MerkleTest <$> choose (100,10000)

main :: IO ()
main = defaultMain merkleTests

merkleTests :: TestTree
merkleTests = testGroup "Merkle Tree & Proof tests"
  [ testProperty "Random Tree/Proof construction and validation:" $ \(MerkleTest n) ->
      monadicIO $ assert =<< liftIO (testMerkleProofN n)
  ]
