{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>),(<*))
#endif
import Control.Exception (SomeException, bracket, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (ord)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO (Handle, SeekMode(..), hFlush, hSeek, stdout)
import System.IO.Temp (withSystemTempFile)
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import ParamTree

genTestCase :: Int -> Bool -> Char -> Int -> String -> TestTree
genTestCase _ _ _ _ name = testCase name $ return ()

paramTreeTests :: IO ()
paramTreeTests = defaultMain $ testTree genTestCase params
  where
    testTree = growTree (Just "/") testGroup "paramtree"

    params :: Params '[] -> Params [Int, Bool, Char, Int]
    params = simpleParam "Int #1" [1,2]
           . paramSets
                [ simpleParam "Bool" [True] . simpleParam "Char" "xy"
                , simpleParam "Bool" [True,False] . simpleParam "Char" "a"
                ]
           . simpleParam "Int #2" [42,1337]

withCapturedIO :: IO () -> IO BS.ByteString
withCapturedIO act = withSystemTempFile "golden.test" $ \_ hnd -> do
    let redirect :: IO Handle
        redirect = do
            hFlush stdout
            hDuplicate stdout <* hDuplicateTo hnd stdout

        undo :: Handle -> IO ()
        undo h = hFlush stdout >> hDuplicateTo h stdout

    bracket redirect undo $ \_ -> try act :: IO (Either SomeException ())

    hSeek hnd AbsoluteSeek 0
    BS.hGetContents hnd

runGolden :: String -> BS.ByteString -> TestTree
runGolden name rawOutput = goldenVsString name goldenFile (return lazyOutput)
  where
    goldenFile = "tests/" ++ name ++ ".golden"
    output = BS.take (BS.length rawOutput - 9) rawOutput
    lazyOutput = LBS.fromStrict $ output `BS.snoc` fromIntegral (ord '\n')

main :: IO ()
main = do
    tests <- map (uncurry runGolden) <$> mapM run testPairs
    defaultMain . testGroup "Tests" $ tests
  where
    run (x, act) = (,) x <$> withCapturedIO act
    testPairs = [ ("paramtree", paramTreeTests) ]
