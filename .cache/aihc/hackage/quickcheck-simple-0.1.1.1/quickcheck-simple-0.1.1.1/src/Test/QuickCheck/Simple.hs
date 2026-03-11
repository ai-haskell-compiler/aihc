-- |
-- Module      : Test.QuickCheck.Simple
-- Copyright   : 2015-2019 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains definitions of test properties and default-mains
-- using QuickCheck library.
module Test.QuickCheck.Simple
       ( Property (..)
       , boolTest', boolTest
       , eqTest', eqTest
       , qcTest

       , Test, TestError (..)
       , runTest_, runTest
       , defaultMain_, defaultMain, verboseMain

       , defaultMain'
       ) where

import Control.Applicative ((<$>))
import Control.Monad (unless)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Test.QuickCheck
  (Testable, Result (..), quickCheckResult, label)
import qualified Test.QuickCheck as QC


-- | Property type. 'Bool' or 'Testable' of QuickCheck.
data Property
  = Bool (Maybe String {- verbose error message -}) Bool
  | QuickCheck QC.Property

-- | Property with label string
type Test = (String {- label -}, Property)

mkBoolTest :: String -> Maybe String -> Bool -> Test
mkBoolTest n m = ((,) n) . Bool m

-- | 'Bool' specialized property with message for False case
boolTest' :: String
          -> String
          -> Bool
          -> Test
boolTest' n m = mkBoolTest n (Just m)

-- | 'Bool' specialized property
boolTest :: String
         -> Bool
         -> Test
boolTest n = mkBoolTest n Nothing

-- | 'Eq' specialized property with explicit passing
eqTest' :: (a -> a -> Bool) -> (a -> String) -> String -> a -> a -> Test
eqTest' eq show' n x y = boolTest' n msg $ x `eq` y where
  msg = unlines [show' x, "** NOT EQUALS **", show' y]

-- | 'Eq' specialized property
eqTest :: (Eq a, Show a) => String -> a -> a -> Test
eqTest = eqTest' (==) show

-- | QuickCheck 'Testable' property
qcTest :: Testable prop
       => String
       -> prop
       -> Test
qcTest n = ((,) n) . QuickCheck . label n

--------------------------------------------------------------------------------

-- | Test failure result.
data TestError
  = BFalse (Maybe String {- verbose error message -})
  | QCError Result
  deriving Show

putErrorLn :: String -> IO ()
putErrorLn = putStrLn . ("*** " <>)

printVerbose :: String -> TestError -> IO ()
printVerbose lb te = case te of
    BFalse m   ->  maybe (return ()) format m
    QCError r  ->  format $ show r
  where
    format s =
      mapM_ putErrorLn
      $ ("label: " <> lb <> ":") : (map ("  " <>) $ lines s)

runBool :: String
        -> Maybe String -- ^ verbose error message. Nothing corresponds to not verbose.
        -> Bool
        -> IO (Maybe TestError)
runBool lb vmsg = d  where
  d True  =  do
    putStrLn $ "+++ OK, success (" <> lb <> ")"
    return   Nothing
  d False =  do
    putErrorLn $ "Failed! (" <> lb <> ")"
    let r = BFalse vmsg
    printVerbose lb r
    return $ Just r

runQcProp :: Bool -- ^ verbose flag
          -> String
          -> QC.Property
          -> IO (Maybe TestError)
runQcProp verbose lb p = err =<< quickCheckResult p  where
  err (Success {})  =
    return   Nothing
  err x             =  do
    let r = QCError x
    if verbose
      then printVerbose lb r            -- this action show label
      else putErrorLn $ "label: " <> lb -- quickcheck does not show label
    return $ Just r

runProp :: Bool
         -> String
         -> Property
         -> IO (Maybe TestError)
runProp verbose lb prop = case prop of
  Bool m b      ->  runBool lb (if verbose then m else Nothing) b
  QuickCheck p  ->  runQcProp verbose lb p

-- | Run a single test suite.
runTest_ :: Bool                 -- ^ verbose flag
         -> Test                 -- ^ property to test
         -> IO (Maybe TestError) -- ^ result action, and may be failure result
runTest_ verbose = uncurry $ runProp verbose

-- | Not verbose version of runTest_
runTest :: Test                 -- ^ property to test
        -> IO (Maybe TestError) -- ^ result action, and may be failure result
runTest = runTest_  False

-- | Default main to run test suites.
defaultMain_ :: Bool -> [Test] -> IO ()
defaultMain_ verbose xs = do
  es <- catMaybes <$> mapM (runTest_ verbose) xs
  unless (null es) $ fail "Some failures are found."

defaultMain' :: Bool -> [Test] -> IO ()
defaultMain' = defaultMain_
{-# DEPRECATED defaultMain' "Use defaultMain_ instead of this." #-}

-- | Not verbose version of 'defaultMain''.
defaultMain :: [Test] -> IO ()
defaultMain = defaultMain_ False

-- | Verbose verison of defaultMain
verboseMain :: [Test] -> IO ()
verboseMain = defaultMain_ True
