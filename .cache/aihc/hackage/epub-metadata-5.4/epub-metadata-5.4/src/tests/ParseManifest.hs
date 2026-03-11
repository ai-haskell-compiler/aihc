module ParseManifest
   ( tests )
   where

import Control.Monad.Except
import System.FilePath
import Test.HUnit

import Codec.Epub.Data.Manifest
import Codec.Epub.Parse


tests :: Test
tests = TestList
   [ testSeveral
   , testMissing
   ]


{- A fairly comprehensive test containing several manifest items
-}
testSeveral :: Test
testSeveral = TestCase $ do
   xmlString <- readFile $ "util" </> "resources" </> "epub2-full.opf"
   actual <- runExceptT $ getManifest xmlString
   let expected =
         Right $ Manifest [ ManifestItem 
                  { mfiId = "ncx"
                  , mfiHref = "toc.ncx"
                  , mfiMediaType = "application/x-dtbncx+xml"
                  }
               , ManifestItem 
                  { mfiId = "titlePage"
                  , mfiHref = "content/titlePage.html"
                  , mfiMediaType = "application/xhtml+xml"
                  }
               , ManifestItem 
                  { mfiId = "someContent"
                  , mfiHref = "content/someContent.html"
                  , mfiMediaType = "application/xhtml+xml"
                  }
               ]
   assertEqual "several manifest items" expected actual


{- Test data devoid of any manifest items
-}
testMissing :: Test
testMissing = TestCase $ do
   xmlString <- readFile $ "util" </> "resources" </> "epub2-missingAll.opf"
   actual <- runExceptT $ getManifest xmlString
   let expected =
         Right $ Manifest []
   assertEqual "missing entirely" expected actual
