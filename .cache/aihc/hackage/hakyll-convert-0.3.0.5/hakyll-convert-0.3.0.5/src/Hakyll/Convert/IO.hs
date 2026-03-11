{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Convert.IO where

import qualified Data.ByteString as B
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import Hakyll.Convert.Common (DistilledPost (..))
import Hakyll.Convert.OutputFormat (formatPath)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, takeFileName, (<.>), (</>))

-- | Save a post along with its comments in a format that Hakyll understands.
--
-- Returns the filename of the file that was written.
savePost :: FilePath -> T.Text -> T.Text -> DistilledPost -> IO FilePath
savePost odir oformat ext post = do
  createDirectoryIfMissing True (takeDirectory fname)
  B.writeFile fname . T.encodeUtf8 $
    T.unlines
      [ "---",
        metadata "title" (formatTitle (dpTitle post)),
        metadata "published" (formatDate (dpDate post)),
        metadata "categories" (formatTags (dpCategories post)),
        metadata "tags" (formatTags (dpTags post)),
        "---",
        "",
        formatBody (dpBody post)
      ]

  return fname
  where
    metadata k v = k <> ": " <> v
    --
    fname = odir </> postPath <.> (T.unpack ext)
    postPath = T.unpack $ fromJust $ formatPath oformat post
    --
    formatTitle (Just t) = t
    formatTitle Nothing =
      "untitled (" <> T.unwords firstFewWords <> "…)"
      where
        firstFewWords = T.splitOn "-" . T.pack $ takeFileName postPath
    formatDate = T.pack . formatTime defaultTimeLocale "%FT%TZ" -- for hakyll
    formatTags = T.intercalate ","
    formatBody = id
