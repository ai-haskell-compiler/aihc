{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module HiFileParserSpec (spec) where

import qualified HiFileParser          as Iface
import           RIO
import           Test.Hspec            (Spec, describe, it, shouldBe)

type Version = String
type Directory = FilePath
type Usage = String
type Module = ByteString

-- | GHC x.y.z is represented as \"ghcxyyz\" where yy is padded with zeros.
versions32 :: [Version]
versions32 =
  [ "ghc7103"  -- Last in GHC 7.10 series, using GHC 7.8.1 format
  , "ghc8002"  -- Last in GHC 8.0 series, using GHC 8.0.1 format
  , "ghc8022"  -- Last in GHC 8.2 series, using GHC 8.2.1 format
  , "ghc8044"  -- Last in GHC 8.4 series, using GHC 8.4.1 format
  ]

-- | GHC x.y.z is represented as \"ghcxyyz\" where yy is padded with zeros.
versions64 :: [Version]
versions64 =
  [ "ghc8022"  -- Last in GHC 8.2 series, using GHC 8.0.1 format
  , "ghc8044"  -- Last in GHC 8.4 series, using GHC 8.4.1 format
  , "ghc8065"  -- Last in GHC 8.6 series, using GHC 8.6.1 format
  , "ghc8084"  -- Last in GHC 8.8 series, using GHC 8.6.1 format
  , "ghc8107"  -- Last in GHC 8.10 series, using GHC 8.10.1 format
  , "ghc9002"  -- Last in GHC 9.0 series, using GHC 9.0.1 format
  , "ghc9028"  -- Last in GHC 9.2 series, using GHC 9.0.1 format
  , "ghc9044"  -- Last using GHC 9.4.1 format
  , "ghc9048"  -- Last in GHC 9.4 series, using GHC 9.4.5 format
  , "ghc9067"  -- Last in GHC 9.6 series, using GHC 9.4.5 format
  , "ghc9084"  -- Last in GHC 9.8 series, using GHC 9.8.1 format
  , "ghc9102"  -- Last in GHC 9.10 series, using GHC 9.8.1 format
  , "ghc9122"  -- Last in GHC 9.12 series, using GHC 9.12 format
  , "ghc9140"  -- First in GHC 9.14 series, using GHC 9.14 format
  ]

spec :: Spec
spec = describe "should successfully deserialize interface for" $ do
   traverse_ (deserialize check32 . ("x32/" <>)) versions32
   traverse_ (deserialize check64 . ("x64/" <>)) versions64

check32 :: Iface.Interface -> IO ()
check32 iface = do
    hasExpectedUsage "some-dependency.txt" iface `shouldBe` True

check64 :: Iface.Interface -> IO ()
check64 iface = do
    hasExpectedUsage "Test.h" iface `shouldBe` True
    hasExpectedUsage "README.md" iface `shouldBe` True
    hasExpectedModule "X" iface `shouldBe` True

deserialize :: (Iface.Interface -> IO ()) -> Directory -> Spec
deserialize check d = do
    it d $ do
        let ifacePath = "test-files/iface/" <> d <> "/Main.hi"
        result <- Iface.fromFile ifacePath
        case result of
          (Left msg)    -> fail msg
          (Right iface) -> check iface

-- | `Usage` is the name given by GHC to TH dependency
hasExpectedUsage :: Usage -> Iface.Interface -> Bool
hasExpectedUsage u =
    elem u . fmap Iface.unUsage . Iface.unList . Iface.usage

hasExpectedModule :: Module -> Iface.Interface -> Bool
hasExpectedModule m =
    elem m . fmap fst . Iface.unList . Iface.dmods . Iface.deps
