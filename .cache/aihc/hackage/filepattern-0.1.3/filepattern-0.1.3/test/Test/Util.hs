{-# LANGUAGE RecordWildCards, ConstraintKinds #-}

module Test.Util(
    assertBool, (#=),
    matchY, matchN,
    arity,
    substitute, substituteErr,
    stepNext,
    getDirectory,
    TestData(..), unsafeTestData,
    FP.StepNext(..)
    ) where

import Control.Exception.Extra
import Control.Monad.Extra
import Data.List.Extra
import Data.IORef.Extra
import System.Directory
import System.FilePath
import System.FilePattern(FilePattern)
import qualified System.FilePattern as FP
import qualified System.FilePattern.Directory as FP
import System.IO.Extra
import System.IO.Unsafe


---------------------------------------------------------------------
-- COLLECT TEST DATA

data TestData = TestData
    {testDataCases :: {-# UNPACK #-} !Int
    ,testDataPats :: [FilePattern]
    ,testDataPaths :: [FilePath]
    }

{-# NOINLINE testData #-}
testData :: IORef TestData
testData = unsafePerformIO $ newIORef $ TestData 0 [] []

addTestData :: [FilePattern] -> [FilePath] -> IO ()
addTestData pats paths = atomicModifyIORef'_ testData f
    where f TestData{..} = TestData (testDataCases+1) (reverse pats ++ testDataPats) (reverse paths ++ testDataPaths)

unsafeTestData :: IO TestData
unsafeTestData = atomicModifyIORef' testData $ \t -> (TestData 0 [] [], f t)
    where f TestData{..} = TestData testDataCases (nubSort $ reverse testDataPats) (nubSort $ reverse testDataPaths)


---------------------------------------------------------------------
-- TEST UTILITIES

assertBool :: Partial => Bool -> String -> [String] -> IO ()
assertBool b msg fields = unless b $ error $ unlines $
    ("ASSERTION FAILED: " ++ msg) : fields

assertException :: (Show a, Partial) => IO a -> [String] -> String -> [String] -> IO ()
assertException a parts msg fields = do
    res <- try_ $ evaluate . length . show =<< a
    case res of
        Left e -> assertBool (all (`isInfixOf` show e) parts) msg $ ["Expected" #= parts, "Got" #= e] ++ fields
        Right _ -> assertBool False msg $ ["Expected" #= parts, "Got" #= "<No exception>"] ++ fields

(#=) :: Show a => String -> a -> String
(#=) a b = a ++ ": " ++ show b


---------------------------------------------------------------------
-- TEST WRAPPERS

match :: Partial => FilePattern -> FilePath -> Maybe [String] -> IO ()
match pat path want = do
    addTestData [pat] [path]
    let got = FP.match pat path
    assertBool (want == got) "match" ["Pattern" #= pat, "Path" #= path, "Expected" #= want, "Got" #= got]

matchY :: Partial => FilePattern -> FilePath -> [String] -> IO ()
matchY pat path xs = match pat path $ Just xs

matchN :: Partial => FilePattern -> FilePath -> IO ()
matchN pat path = match pat path Nothing


arity :: Partial => FilePattern -> Int -> IO ()
arity pat want = do
    addTestData [pat] []
    let got = FP.arity pat
    assertBool (want == got) "arity" ["Pattern" #= pat, "Expected" #= want, "Got" #= got]


substitute :: Partial => FilePattern -> [String] -> FilePath -> IO ()
substitute pat parts want = do
    addTestData [pat] [want]
    let got = FP.substitute pat parts
    assertBool (want == got) "substitute" ["Pattern" #= pat, "Parts" #= parts, "Expected" #= want, "Got" #= got]

substituteErr :: Partial => FilePattern -> [String] -> [String] -> IO ()
substituteErr pat parts want = do
    addTestData [pat] []
    assertException (pure $ FP.substitute pat parts) want "substitute" ["Pattern" #= pat, "Parts" #= parts]


stepNext :: [FilePattern] -> [String] -> FP.StepNext -> IO ()
stepNext pat path want = do
    addTestData pat []
    let got = f (FP.step_ pat) path
    assertBool (want == got) "stepNext" ["Pattern" #= pat, "Path" #= path, "Expected" #= want, "Got" #= got]
    where
        f FP.Step{..} [] = stepNext
        f FP.Step{..} (x:xs) = f (stepApply x) xs


getDirectory :: [FilePattern] -> [FilePattern] -> [FilePath] -> [FilePath] -> IO ()
getDirectory match ignore want avoid =
    withTempDir $ \root -> do
        forM_ (want ++ avoid) $ \x -> do
            createDirectoryIfMissing True $ root </> takeDirectory x
            writeFile (root </> x) ""
        got <- FP.getDirectoryFilesIgnore root match ignore
        assertBool (want == got) "getDirectory" ["Root" #= root, "Match" #= match, "Ignore" #= ignore, "Want" #= want, "Got" #= got, "Avoid" #= avoid]
