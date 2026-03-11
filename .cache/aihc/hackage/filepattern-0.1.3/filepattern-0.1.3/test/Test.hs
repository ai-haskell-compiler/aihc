{-# LANGUAGE RecordWildCards, TupleSections #-}

module Main(main) where

import Control.Monad.Extra
import Data.List.Extra
import Data.Functor
import Data.Tuple.Extra
import qualified Test.Util as T
import Data.Maybe
import System.FilePattern as FilePattern
import System.FilePath(isPathSeparator)
import System.IO.Unsafe
import Test.QuickCheck
import Test.Cases
import Prelude


---------------------------------------------------------------------
-- TEST UTILITIES

newtype ArbPattern = ArbPattern FilePattern deriving (Show,Eq)
newtype ArbPath    = ArbPath    FilePath    deriving (Show,Eq)

-- Since / and * are the only "interesting" elements, just add ab to round out the set

instance Arbitrary ArbPattern where
    arbitrary = fmap (ArbPattern . concat) $ listOf $ elements $ "**" : map (:[]) "\\/*ab."
    shrink (ArbPattern x) = map ArbPattern $ shrinkList (\x -> ['/' | x == '\\']) x

instance Arbitrary ArbPath where
    arbitrary = fmap ArbPath $ listOf $ elements "\\/ab."
    shrink (ArbPath x) = map ArbPath $ shrinkList (\x -> ['/' | x == '\\']) x


runStepSimple :: FilePattern -> FilePath -> Maybe [String]
runStepSimple pat path = f (step_ [pat]) $ split isPathSeparator path
    where
        f Step{..} [] = snd <$> listToMaybe stepDone
        f Step{..} (x:xs) = f (stepApply x) xs

runStepComplex :: FilePattern -> FilePath -> Maybe [String]
runStepComplex pat path = fmap thd3 $ listToMaybe $ matchMany [((), pat)] [((), path)]


---------------------------------------------------------------------
-- DRIVER

main :: IO ()
main = do
    putStrLn "Testing..."
    testCases
    T.TestData{..} <- T.unsafeTestData
    putStrLn $ "Passed " ++ show testDataCases ++ " specific cases"
    -- when False $ dot $ testWalk s
    testProperties $ testDataPats ++ testDataPaths
    putStrLn "SUCCESS (all tests completed)"


testProperties :: [String] -> IO ()
testProperties xs = do
    resOne <- fmap (catMaybes . concat) $ forM (zipFrom 1 xs) $ \(ix,x) -> forM (zipFrom 1 xs) $ \(iy,y) -> fmap (ix,iy,) <$> prop x y
    let resMany = matchMany (zipFrom 1 xs) (zipFrom 1 xs)
    T.assertBool (sort resOne == sort resMany) "matchMany" []
    putStrLn $ "Passed " ++ show (length xs ^ 2) ++ " properties on specific cases"
    Success{} <- quickCheckWithResult stdArgs{maxSuccess=10000} $ \(ArbPattern p) (ArbPath x) ->
        (if p ?== x then label "match" else property) $ unsafePerformIO $ prop p x >> pure True
    pure ()
    where
        prop :: FilePattern -> FilePath -> IO (Maybe [String])
        prop pat file = do
            let ans = match pat file
            let fields = ["Pattern" T.#= pat, "File" T.#= file, "Match" T.#= ans]
            whenJust ans $ \ans -> T.assertBool (length ans == arity pat) "arity" fields
            let res = pat ?== file in T.assertBool (res == isJust ans) "?==" $ fields ++ ["?==" T.#= res]
            let res = runStepSimple  pat file in T.assertBool (res == ans) "step (simple)" $ fields ++ ["step" T.#= res]
            let res = runStepComplex pat file in T.assertBool (res == ans) "step (complex)" $ fields ++ ["step" T.#= res]
            let norm = (\x -> if null x then [""] else x) . filter (/= ".") . split isPathSeparator
            when (isJust ans) $ let res = substitute pat (fromJust $ FilePattern.match pat file) in
                T.assertBool (norm res == norm file) "substitute" $ fields ++ ["Match" T.#= FilePattern.match pat file, "Got" T.#= res, "Input (norm)" T.#= norm file, "Got (norm)" T.#= norm res]
            pure ans
