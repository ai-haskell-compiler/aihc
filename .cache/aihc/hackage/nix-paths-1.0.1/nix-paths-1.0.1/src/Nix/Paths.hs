{-# LANGUAGE CPP #-}

module Nix.Paths ( nixInstantiate, nixBuild, nixEnv, nixStore, nixHash ) where

-- | Complete path to the @nix-instantiate@ executable.
nixInstantiate :: FilePath
nixInstantiate = PATH_TO_NIX_INSTANTIATE

-- | Complete path to the @nix-build@ executable.
nixBuild :: FilePath
nixBuild = PATH_TO_NIX_BUILD

-- | Complete path to the @nix-env@ executable.
nixEnv :: FilePath
nixEnv = PATH_TO_NIX_ENV

-- | Complete path to the @nix-store@ executable.
nixStore :: FilePath
nixStore = PATH_TO_NIX_STORE

-- | Complete path to the @nix-hash@ executable.
nixHash :: FilePath
nixHash = PATH_TO_NIX_HASH
