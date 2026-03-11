{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
module GenericRLPTest where

import           Control.Monad             (forM_, when)
import           Data.Either               (isRight)
import qualified Data.Map                  as M
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE

import qualified Data.ByteString           as S
import qualified Data.ByteString.Base16    as SB16
import qualified Data.ByteString.Char8     as S8

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import qualified Test.Tasty                as Tasty
import           Test.Tasty.Hspec

import           Data.RLP

import           Data.Proxy
import           GHC.Generics

newtype PositiveInteger = PositiveInteger Integer
  deriving (Eq, Read, Show, Generic, RLPEncodable)
instance Arbitrary PositiveInteger where
  arbitrary = PositiveInteger . abs <$> arbitrary

newtype RLPByteString = RLPByteString S8.ByteString
  deriving (Eq, Read, Show, Generic, RLPEncodable)
instance Arbitrary RLPByteString where
  arbitrary = do
    len <- arbitrary
    RLPByteString . S8.pack <$> vector len

data ArglessUnit = ArglessUnit deriving (Show, Eq, Generic)
instance RLPEncodable ArglessUnit
instance Arbitrary ArglessUnit where
  arbitrary = return ArglessUnit

newtype OneArgUnit a = OneArgUnit PositiveInteger deriving (Show, Eq, Generic)
instance (RLPEncodable a) => RLPEncodable (OneArgUnit a)
instance (Arbitrary a) => Arbitrary (OneArgUnit a) where
  arbitrary = OneArgUnit <$> arbitrary

data ArglessOneArgSum a = AOALeft
                        | AOARight a
                      deriving (Show, Eq, Generic)
instance (RLPEncodable a) => RLPEncodable (ArglessOneArgSum a)
instance (Arbitrary a) => Arbitrary (ArglessOneArgSum a) where
  arbitrary = do
    lOrR <- arbitrary
    if lOrR
      then return AOALeft
      else AOARight <$> arbitrary

data TwoSingleArgSummands a b = TSASLeft PositiveInteger
                              | TSASRight PositiveInteger
                              deriving (Show, Eq, Generic)
instance (RLPEncodable a, RLPEncodable b) => RLPEncodable (TwoSingleArgSummands a b)
instance (Arbitrary a, Arbitrary b) => Arbitrary (TwoSingleArgSummands a b) where
  arbitrary = do
    lOrR <- arbitrary
    if lOrR
      then TSASLeft <$> arbitrary
      else TSASRight <$> arbitrary

data TreeLikeStructure a = TLSBranch (TreeLikeStructure a) (TreeLikeStructure a)
                         | TLSLeaf a
                         | TLSTerminal
                         deriving (Show, Eq, Generic)
instance (RLPEncodable a) => RLPEncodable (TreeLikeStructure a)
instance (Arbitrary a) => Arbitrary (TreeLikeStructure a) where
  arbitrary = do
    isTerm <- arbitrary
    if isTerm
      then return TLSTerminal
      else do
        isBranch <- arbitrary
        if isBranch
          then TLSBranch <$> arbitrary <*> arbitrary
          else TLSLeaf <$> arbitrary

data AComplexRecord = AComplexRecord { aComplexString :: RLPByteString
                                     , aComplexInt    :: PositiveInteger
                                     }
                                     deriving (Show, Eq, Generic)
instance RLPEncodable AComplexRecord
instance Arbitrary AComplexRecord where
  arbitrary = AComplexRecord <$> arbitrary <*> arbitrary

data ANestedRecord = ANestedRecord { aNestedString        :: RLPByteString
                                   , aNestedComplesRecord :: AComplexRecord
                                   }
                                   deriving (Show, Eq, Generic)
instance RLPEncodable ANestedRecord
instance Arbitrary ANestedRecord where
  arbitrary = ANestedRecord <$> arbitrary <*> arbitrary

genericSpec :: Spec
genericSpec = describe "Generic instances" $ do
  identityWithQuickcheck (Proxy @ArglessUnit                                           ) "Argless unitary datatype"
  identityWithQuickcheck (Proxy @(OneArgUnit PositiveInteger)                          ) "[Int] One-arg unitary datatype"
  identityWithQuickcheck (Proxy @(ArglessOneArgSum PositiveInteger)                    ) "[Int] Sum type of argless and one-arg constructors"
  identityWithQuickcheck (Proxy @(TwoSingleArgSummands PositiveInteger PositiveInteger)) "[Int] Sum type of two one-arg constructors"
  identityWithQuickcheck (Proxy @(TreeLikeStructure PositiveInteger)                   ) "[Int] A tree-like structure"
  identityWithQuickcheck (Proxy @(OneArgUnit RLPByteString)                            ) "[BS] One-arg unitary datatype"
  identityWithQuickcheck (Proxy @(ArglessOneArgSum RLPByteString)                      ) "[BS] Sum type of argless and one-arg constructors"
  identityWithQuickcheck (Proxy @(TwoSingleArgSummands RLPByteString RLPByteString)    ) "[BS/BS] Sum type of two one-arg constructors"
  identityWithQuickcheck (Proxy @(TreeLikeStructure RLPByteString)                     ) "[BS] A tree-like structure"
  identityWithQuickcheck (Proxy @(TwoSingleArgSummands RLPByteString PositiveInteger)  ) "[BS/Int] Sum type of two one-arg constructors"
  identityWithQuickcheck (Proxy @(TwoSingleArgSummands PositiveInteger RLPByteString)  ) "[Int/BS] Sum type of two one-arg constructors"
  identityWithQuickcheck (Proxy @AComplexRecord                                        ) "A record with named fields"
  identityWithQuickcheck (Proxy @ANestedRecord                                         ) "A record with another record inside of it"

maxQuickchecks :: Int
maxQuickchecks = 100

identityWithQuickcheck :: forall a proxy.
                          ( Arbitrary a
                          , Eq a
                          , Show a
                          , RLPEncodable a
                          )
                       => proxy a
                       -> String
                       -> Spec
identityWithQuickcheck _ name =
  describe name . forM_ [1..maxQuickchecks] $ \n -> do
    describe (show n ++ "/" ++ show maxQuickchecks) $ do
      theA :: a <- runIO (generate arbitrary)
      satisfiesIdentity theA

satisfiesIdentity :: forall a.
                   ( RLPEncodable a
                   , Eq a
                   , Show a
                   )
                  => a
                  -> Spec
satisfiesIdentity a = do
  let encoded = rlpEncode a
      decoded = rlpDecode encoded

  it "can successfully decode a previously encoded object" $
    decoded `shouldSatisfy` isRight

  let Right asRight = decoded
  it "maintains identity after encoding and subsequently decoding" $
    asRight `shouldBe` a

