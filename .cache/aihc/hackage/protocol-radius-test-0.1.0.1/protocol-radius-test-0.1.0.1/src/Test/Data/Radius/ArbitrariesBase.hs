{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}

module Test.Data.Radius.ArbitrariesBase (
  genPacket,

  genSizedString,
  genAtText, genAtString,
  ) where

import Test.QuickCheck (Arbitrary (..), Gen, elements, choose)

import Control.Applicative ((<$>), pure, (<*>))
import Control.Monad (replicateM)
import Data.String (IsString, fromString)
import qualified Data.ByteString as BS
import Data.Word (Word16)
import Data.Serialize.Put (Put, runPut)

import Data.Radius.Scalar
  (Bin128, word64Bin128, AtText (..), AtString (..), AtInteger (..), AtIpV4 (..))
import Data.Radius.Packet (codeFromWord, Code, Header(..), Packet (..))
import qualified Data.Radius.Attribute as Attribute
import qualified Data.Radius.StreamPut as Put


instance Arbitrary Code where
  arbitrary = codeFromWord <$> arbitrary

instance Arbitrary Bin128 where
  arbitrary = word64Bin128 <$> arbitrary <*> arbitrary

instance Arbitrary Attribute.Number where
  arbitrary =
    elements
    [ c
    | w <- [0  .. 255]
    , let c = Attribute.fromWord w
    , c /= Attribute.VendorSpecific
    ]

genSizedList :: Arbitrary a => Int -> Gen [a]
genSizedList n =
  elements [0..n] >>= (`replicateM` arbitrary)

genSizedString :: IsString a => Int -> Gen a
genSizedString n = fromString <$> genSizedList n

genAtText :: Int -> Gen AtText
genAtText n = AtText <$> genSizedString (n  `quot` 4)

instance Arbitrary AtText where
  arbitrary = genAtText (255 - 1 - 1) {- USE CAREFULLY with vendor specific. -}

genAtString :: Int -> Gen AtString
genAtString n = AtString <$> genSizedString n

instance Arbitrary AtString where
  arbitrary = genAtString (255 - 1 - 1) {- USE CAREFULLY with vendor specific. -}

instance Arbitrary AtInteger where
  arbitrary = AtInteger <$> arbitrary

instance Arbitrary AtIpV4 where
  arbitrary = AtIpV4 <$> arbitrary


genHeader :: Word16 -> Gen Header
genHeader len =
  Header <$> arbitrary <*> arbitrary <*> pure len <*> arbitrary

-- Random header, may be wrong length
instance Arbitrary Header where
  arbitrary = genHeader =<< arbitrary

pseudoHeader :: Header
pseudoHeader = Header (codeFromWord 0) 0 0 (word64Bin128 0 0)

genCountedPacket :: Arbitrary a
                 => Int
                 -> (a -> Put)
                 -> Gen (Packet [a], Int)
genCountedPacket ac encodeA = do
  attrs <- replicateM ac arbitrary
  let len = BS.length . runPut . Put.packet (mapM_ encodeA) $ Packet pseudoHeader attrs
  (,) <$> (Packet <$> (genHeader $ fromIntegral len) <*> pure attrs) <*> pure len

genPacket :: Arbitrary a
          => (a -> Put)
          -> Gen (Packet [a])
genPacket encodeA = do
  ac <- choose (0, 31)
  (p, len) <- genCountedPacket ac encodeA
  if len <= 4096
    then pure p
    else do
    ac <- choose (0, 15)
    (p, len) <- genCountedPacket ac encodeA
    if len <= 4096
      then pure p
      else error "genPacket: this should not happen broken size property (header-size + 256 * 15 < 4096)."
