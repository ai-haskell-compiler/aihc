module ParseSpine
   ( tests )
   where

import Control.Monad.Except
import Control.Monad.IO.Class ( liftIO )
import System.FilePath
import Test.HUnit

import Codec.Epub.Data.Spine
import Codec.Epub.Parse


tests :: Test
tests = TestList
   [ testFull
   , testMinimal
   , testMissingAll
   ]


{- A test containing a couple of spine items
-}
testFull :: Test
testFull = TestCase $ do
   xmlString <- readFile $ "util" </> "resources" </> "epub2-full.opf"
   actual <- runExceptT $ getSpine xmlString
   let expected =
         Right Spine
            { spineToc = "ncx"
            , spineItemrefs = 
               [ SpineItemref {siIdRef = "titlePage", siLinear = Nothing}
               , SpineItemref {siIdRef = "someContent", siLinear = Nothing}
               ]
            }
   assertEqual "some spine items" expected actual


{- Minimal spine, only the spineToc is required
-}
testMinimal :: Test
testMinimal = TestCase $ do
   xmlString <- liftIO $ readFile $ "util" </> "resources" </> "epub2-minimal.opf"
   actual <- runExceptT $ getSpine xmlString
   let expected = Right Spine {spineToc = "ncx", spineItemrefs = []}
   assertEqual "minimal" expected actual


{- Missing any spine info at all
-}
testMissingAll :: Test
testMissingAll = TestCase $ do
   xmlString <- readFile $ "util" </> "resources" </> "epub2-missingAll.opf"
   actual <- runExceptT $ getSpine xmlString
   let expected = Right Spine {spineToc = "", spineItemrefs = []}
   assertEqual "missing all" expected actual
