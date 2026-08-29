module Aihc.Cli.ArtifactCache
  ( ArtifactCache,
    artifactCache,
    loadArtifact,
  )
where

import Control.Exception (IOException, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS

newtype ArtifactCache = ArtifactCache
  { cacheReadsEnabled :: Bool
  }

artifactCache :: Bool -> ArtifactCache
artifactCache = ArtifactCache

loadArtifact :: ArtifactCache -> FilePath -> (ByteString -> Either String artifact) -> (artifact -> Bool) -> IO (Maybe artifact)
loadArtifact cache path decode valid
  | not (cacheReadsEnabled cache) = pure Nothing
  | otherwise = do
      readResult <- try (BS.readFile path) :: IO (Either IOException ByteString)
      pure $ do
        bytes <- either (const Nothing) Just readResult
        artifact <- either (const Nothing) Just (decode bytes)
        if valid artifact then Just artifact else Nothing
