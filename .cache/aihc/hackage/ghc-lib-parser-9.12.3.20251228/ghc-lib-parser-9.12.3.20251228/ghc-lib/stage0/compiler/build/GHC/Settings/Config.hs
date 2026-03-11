module GHC.Settings.Config
  ( module GHC.Version
  , cBuildPlatformString
  , cHostPlatformString
  , cProjectName
  , cBooterVersion
  , cStage
  , cProjectUnitId
  ) where

import GHC.Prelude.Basic

import GHC.Version

cBuildPlatformString :: String
cBuildPlatformString = "x86_64-apple-darwin"

cHostPlatformString :: String
cHostPlatformString = "x86_64-apple-darwin"

cProjectName          :: String
cProjectName          = "The Glorious Glasgow Haskell Compilation System"

cBooterVersion        :: String
cBooterVersion        = "9.10.1"

cStage                :: String
cStage                = show (1 :: Int)

cProjectUnitId :: String
cProjectUnitId = "ghc-9.12.3-inplace"
