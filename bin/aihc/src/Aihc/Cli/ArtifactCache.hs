module Aihc.Cli.ArtifactCache (loadArtifact) where

import Control.Exception (IOException, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS

loadArtifact :: Bool -> FilePath -> (ByteString -> Either String artifact) -> (artifact -> Bool) -> IO (Maybe artifact)
loadArtifact cacheReadsEnabled path decode valid
  | not cacheReadsEnabled = pure Nothing
  | otherwise = do
      readResult <- try (BS.readFile path) :: IO (Either IOException ByteString)
      pure $ do
        bytes <- either (const Nothing) Just readResult
        artifact <- either (const Nothing) Just (decode bytes)
        if valid artifact then Just artifact else Nothing
