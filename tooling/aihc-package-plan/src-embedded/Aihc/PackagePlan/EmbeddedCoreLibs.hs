{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Aihc.PackagePlan.EmbeddedCoreLibs
-- Description : The core-libs sources in the executable
--
-- A release build puts the files under @core-libs@ into the executable, so
-- that the compiler needs no checkout and no download to find its core
-- libraries. The @embed-core-libs@ flag selects this module.
module Aihc.PackagePlan.EmbeddedCoreLibs
  ( embeddedCoreLibs,
  )
where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir, makeRelativeToProject)

-- | Each file under @core-libs@, with its path relative to that directory.
embeddedCoreLibs :: Maybe [(FilePath, ByteString)]
embeddedCoreLibs = Just $(makeRelativeToProject "../../core-libs" >>= embedDir)
