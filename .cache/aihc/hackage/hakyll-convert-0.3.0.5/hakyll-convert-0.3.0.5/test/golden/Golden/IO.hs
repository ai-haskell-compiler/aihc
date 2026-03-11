{-# LANGUAGE OverloadedStrings #-}

module Golden.IO where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Default (def)
import qualified Data.Time.Calendar as Calendar
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Hakyll.Convert.Common (DistilledPost (..))
import Hakyll.Convert.IO (savePost)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

goldenTests :: TestTree
goldenTests =
  testGroup
    "IO.savePost"
    [ writesUntitledPost,
      writesPostWithTitle
    ]

writesUntitledPost :: TestTree
writesUntitledPost =
  goldenVsString
    "Writes untitled post to a file"
    "test/golden/data/io-000/untitled-post.golden"
    ( withSystemTempDirectory "hakyll-convert" $ \tempDir -> do
        let output_format = "output"
        let file_extension = "html"
        let post =
              def
                { dpUri = "https://example.com/~joe/2011/01/02/just-testing.php",
                  dpDate = fromGregorian 2011 1 2 3 14 59,
                  dpCategories = ["Category 1", "Category 2"],
                  dpTags = ["Tagged", "with", "<3"],
                  dpBody = "<p>This tool is <em>awesome</em>!</p>"
                }

        -- Ignore the generated filename -- we'll just check if the file is at
        -- the expected place instead.
        _filename <- savePost tempDir output_format file_extension post

        let filename = tempDir </> "output.html"
        LBS.readFile filename
    )

writesPostWithTitle :: TestTree
writesPostWithTitle =
  goldenVsString
    "Writes post with title to a file"
    "test/golden/data/io-000/post-with-title.golden"
    ( withSystemTempDirectory "hakyll-convert" $ \tempDir -> do
        let output_format = "output"
        let file_extension = "aspx"
        let post =
              def
                { dpUri = "https://example.com/~joe/a%20joke.php",
                  dpDate = fromGregorian 1999 12 31 23 59 1,
                  dpCategories = ["jokes"],
                  dpTags = ["non-funny", "unoriginal"],
                  dpTitle = Just "And now for something completely different…",
                  dpBody = "Wonder what it is?"
                }

        -- Ignore the generated filename -- we'll just check if the file is at
        -- the expected place instead.
        _filename <- savePost tempDir output_format file_extension post

        let filename = tempDir </> "output.aspx"
        LBS.readFile filename
    )

fromGregorian :: Integer -> Int -> Int -> Int -> Int -> Int -> UTCTime
fromGregorian year month day hours minutes seconds =
  let utcDay = Calendar.fromGregorian year month day
      utcSeconds = secondsToDiffTime $ fromIntegral $ seconds + 60 * (minutes + 60 * hours)
   in UTCTime utcDay utcSeconds
