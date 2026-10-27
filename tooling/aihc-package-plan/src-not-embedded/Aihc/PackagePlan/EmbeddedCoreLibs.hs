-- |
-- Module      : Aihc.PackagePlan.EmbeddedCoreLibs
-- Description : No core-libs sources in the executable
--
-- A development build finds the core libraries in its checkout. The
-- @embed-core-libs@ flag selects the module that puts them into the
-- executable.
module Aihc.PackagePlan.EmbeddedCoreLibs
  ( embeddedCoreLibs,
  )
where

import Data.ByteString (ByteString)

-- | No files: this build has no core-libs sources in it.
embeddedCoreLibs :: Maybe [(FilePath, ByteString)]
embeddedCoreLibs = Nothing
