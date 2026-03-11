module Archive
   ( tests )
   where

import Codec.Archive.Zip
import Control.Monad.Except
import Data.List ( isPrefixOf )
import Data.Maybe ( listToMaybe )
import System.Directory
import System.FilePath
import Test.HUnit

import Codec.Epub.Data.Package
import Codec.Epub.IO
import Codec.Epub.Parse


tests :: Test
tests = TestList
   [ testMkArchive
   , testDamagedZip
   , testIllegalCharsBeforeDecl
   ]


{- Test that the mimetype file is the first Entry in archives we create
-}
testMkArchive :: Test
testMkArchive = TestCase $ do
   origDir <- getCurrentDirectory
   a <- mkEpubArchive $ "util" </> "resources" </> "bookfiles"
   setCurrentDirectory origDir
   let maybeFirstFile = listToMaybe . filesInArchive $ a

   assertEqual "mimetype file is FIRST" (Just "mimetype") maybeFirstFile


{- Occasionally epub zip files come along that are damaged in this
   way. It's not fatal to the UNIX zip utility or to book readers, but had
   to be specially handled in the Haskell zip-archive library or it causes
   a fatal exception.
-}
testDamagedZip :: Test
testDamagedZip = TestCase $ do
   let label = "damaged zip"
   let expectedErrorPrefix = "Data.Binary.Get.runGet at position 138: Did not find end of central directory signature"
   actual <- runExceptT $ getPkgXmlFromZip $ "util" </> "resources"
      </> "damagedZipCentralDir.epub"
   case actual of
      Left actualMessage -> assertBool label $ isPrefixOf expectedErrorPrefix actualMessage
      Right _ -> assertFailure label


{- Found books coming from Barnes & Noble (for their NOOK reader) to
   contain illegal characters before the XML declaration. This is
   strictly not allowed by the XML specification. I am very
   disappointed with Barnes & Noble for selling garbage like this.
-}
testIllegalCharsBeforeDecl :: Test
testIllegalCharsBeforeDecl = TestCase $ do
   xmlString <- readFile $
      "util" </> "resources" </> "testIllegalCharsBeforeDecl.opf"
   actual <- runExceptT $ getPackage xmlString
   let expected =
         Right Package {pkgVersion = "2.0", pkgUniqueId = "uuid_id"}
   assertEqual "illegal chars before XML declaration" expected actual
