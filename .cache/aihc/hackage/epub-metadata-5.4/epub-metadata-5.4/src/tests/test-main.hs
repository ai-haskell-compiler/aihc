import System.Exit
import Test.HUnit hiding ( counts )

import qualified Archive
import qualified Epub2.ParseGuide
import qualified Epub2.ParseMetadata
import qualified Epub3.ParseMetadata
import qualified ParsePackage
import qualified ParseManifest
import qualified ParseSpine


main :: IO ()
main = do
   counts <- runTestTT tests
   exit $ testsPassed counts


exit :: Bool -> IO ()
exit True  = exitWith ExitSuccess
exit False = exitWith $ ExitFailure 1


testsPassed :: Counts -> Bool
testsPassed (Counts _ _ e f) = (e == 0) && (f == 0)


tests :: Test
tests = TestList
   [ Archive.tests
   , Epub2.ParseGuide.tests
   , Epub2.ParseMetadata.tests
   , Epub3.ParseMetadata.tests
   , ParsePackage.tests
   , ParseManifest.tests
   , ParseSpine.tests
   ]
