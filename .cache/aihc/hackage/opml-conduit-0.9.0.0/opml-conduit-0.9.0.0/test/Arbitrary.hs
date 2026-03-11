{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | External 'Arbitrary' instances used by OPML types.
-- All instances are defined through the 'OpmlGen' wrapper to avoid conflicts.
module Arbitrary (module Arbitrary) where

-- {{{ Imports
import           Data.ByteString           (ByteString)
import           Data.Char
import           Data.List.NonEmpty
import           Data.Maybe
import           Data.Text                 as Text (Text, find, pack)
import           Data.Text.Encoding
import           Data.Time.Clock
import           Data.Tree
import           Data.Version
import           Refined                   hiding (NonEmpty)
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Text.OPML.Types
import           URI.ByteString
-- }}}

#if MIN_VERSION_base(4,9,0)
#else
deriving instance Generic Version
#endif

-- | OPML version may only be @1.0@, @1.1@ or @2.0@
genOPMLVersion :: Gen Version
genOPMLVersion = Version <$> elements [ [1, 0], [1, 1], [2, 0] ] <*> pure []

-- | Reasonable enough 'URI' generator.
instance Arbitrary URI where
  arbitrary = URI <$> arbitrary <*> arbitrary <*> genPath <*> arbitrary <*> (Just <$> genFragment)
  shrink (URI a b c d e) = URI <$> shrink a <*> shrink b <*> shrink c <*> shrink d <*> shrink e

-- | Reasonable enough 'Authority' generator.
instance Arbitrary Authority where
  arbitrary = Authority <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

genFragment :: Gen ByteString
genFragment = encodeUtf8 . pack <$> listOf1 genAlphaNum

instance Arbitrary Host where
  arbitrary = Host . encodeUtf8 . pack <$> listOf1 genAlphaNum
  shrink = genericShrink

genPath :: Gen ByteString
genPath = encodeUtf8 . pack . ("/" ++) <$> listOf1 genAlphaNum

instance Arbitrary Port where
  arbitrary = do
    Positive port <- arbitrary
    return $ Port port

instance Arbitrary Query where
  arbitrary = do
    a <- listOf1 (encodeUtf8 . pack <$> listOf1 genAlphaNum)
    b <- listOf1 (encodeUtf8 . pack <$> listOf1 genAlphaNum)
    return $ Query $ Prelude.zip a b
  shrink = genericShrink

instance Arbitrary Scheme where
  arbitrary = Scheme . encodeUtf8 . pack <$> listOf1 (choose('a', 'z'))
  shrink = genericShrink

instance Arbitrary UserInfo where
  arbitrary = do
    a <- encodeUtf8 . pack <$> listOf1 genAlphaNum
    b <- encodeUtf8 . pack <$> listOf1 genAlphaNum
    return $ UserInfo a b
  shrink = genericShrink


-- | Generates 'UTCTime' with rounded seconds.
genTime :: Gen UTCTime
genTime = do
  (UTCTime d s) <- arbitrary
  return $ UTCTime d $ fromIntegral (round s :: Int)

-- | Generates 'OutlineBase''s categories.
-- This generator makes sure that the result has no @,@ nor @/@ characters, since those are used as separators.
genCategoryPath :: Gen (NonEmpty (Refined (Not Null) Text))
genCategoryPath = (:|) <$> genCategory <*> listOf genCategory where
  genCategory = arbitrary `suchThat` (isNothing . find (\c -> c == ',' || c == '/') . unrefine)

-- | Alpha-numeric generator.
genAlphaNum :: Gen Char
genAlphaNum = oneof [choose('a', 'z'), suchThat arbitrary isDigit]


instance Arbitrary OpmlHead where
  arbitrary = OpmlHead <$> arbitrary
                       <*> (Just <$> genTime)
                       <*> (Just <$> genTime)
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
  shrink = genericShrink

instance Arbitrary OutlineBase where
  arbitrary = OutlineBase <$> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> (Just <$> genTime)
                          <*> listOf genCategoryPath
  shrink (OutlineBase a b c d e) = OutlineBase <$> [a]
                                               <*> shrink b
                                               <*> shrink c
                                               <*> shrink d
                                               <*> [e]

instance Arbitrary OutlineSubscription where
  arbitrary = OutlineSubscription <$> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
  shrink = genericShrink

instance Arbitrary OpmlOutline where
  arbitrary = oneof [ OpmlOutlineGeneric <$> arbitrary <*> arbitrary
                    , OpmlOutlineLink <$> arbitrary <*> arbitrary
                    , OpmlOutlineSubscription <$> arbitrary <*> arbitrary
                    ]
  shrink = genericShrink

instance Arbitrary Opml where
  arbitrary = do
    degree <- choose (0, 100)
    Opml <$> genOPMLVersion
         <*> arbitrary
         <*> vectorOf degree (genOutlineTree 1)
  shrink (Opml a b c) = Opml <$> [a] <*> shrink b <*> shrink c

-- | Generate a tree of outlines with the given maximum depth.
-- This generator makes sure that only 'OpmlOutlineGeneric' may have children.
genOutlineTree :: Int -> Gen (Tree OpmlOutline)
genOutlineTree n = do
  root <- arbitrary
  degree <- choose (0, 100)
  case (n > 1, root) of
    (True, OpmlOutlineGeneric _ _)  -> Node <$> pure root <*> vectorOf degree (genOutlineTree (n-1))
    (False, OpmlOutlineGeneric _ _) -> return $ Node root []
    _                               -> return $ Node root []
