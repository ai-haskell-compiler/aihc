-- | Tests can be structured as nested 'it' / 'describe' statements
-- 
--   E.g.
-- 
--   > microspec $ do
--   >    describe "plus" $ do
--   >       it "adds positive numbers" $ do
--   >          it "does 1 + 1" $
--   >             1 + 1 === 2
--   >          it "does 2 + 2" $
--   >             2 + 2 === 4
--   >       it "is commutative" $
--   >          \x y -> x + y === y + (x :: Int)
-- 
--   ...which will return, nicely in green instead of bold:
-- 
--   @
--   plus
--     adds positive numbers
--       __does 1 + 1__
--       __does 2 + 2__
--     __is commutative__
-- 
--     -----
--   Runtime: 0.00943336s
--   __Successes: 3, Pending: 0, Failures: 0__
--   @


{-# LANGUAGE
     FlexibleInstances
   , LambdaCase
   #-}

module Test.Microspec (
     -- * Specification
     microspec
   , microspecWith
   , describe
   , it
   , pending
   , prop
   , Microspec
   , MTestable

     -- * Configuration
   , MArgs(..)
   , defaultMArgs

     -- * Compatibility
   , shouldBe
   , shouldSatisfy

     -- Reexports
   , module Test.QuickCheck
   , module Test.QuickCheck.Modifiers
   , module Test.QuickCheck.Monadic
   -- , module Test.QuickCheck.Property
   ) where

-- For older GHCs (7.8 and below).
-- When we stop supporting them, remove:
import Control.Applicative (Applicative(..))

import Control.Monad
import Data.Char (isSpace)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Time (getCurrentTime, diffUTCTime)
import System.Exit (exitWith, ExitCode(ExitFailure))
-- import Data.Time (getCurrentTime, diffUTCTime)
import Test.QuickCheck as QC
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Monadic
-- import Test.QuickCheck.Property


-- Basically a writer monad:

-- | A series of tests, to run with 'microspec'
data Microspec a = Microspec [TestTree Property] a

data TestTree x
   = TestBranch String [TestTree x]
   | TestLeaf String (Either Pending x)

-- If you like the word 'pending', this is the place for you!:
data Pending = Pending
-- | Describe a test as unwritten, e.g.:
-- 
--   > describe "meaning of life" $ pending
pending :: Pending
pending = Pending


---------- User-facing:

-- | Run your spec. Put this at the top level, e.g.:
-- 
--   > main = microspec $ do
--   >    describe "plus 1" $
--   >       3 + 1 === 4
microspec :: Microspec () -> IO ()
microspec = microspecWith defaultMArgs

-- | 'microspec' with 'MArgs'
microspecWith :: MArgs -> Microspec () -> IO ()
microspecWith args (Microspec specs ()) = do
   putStrLn ""
   startTime <- getCurrentTime

   results <- forM specs $ \test -> do
      runTestWith args 0 test

   let resultCount :: ResultCounts
       resultCount = joinResultList {- mconcat -} $ map countResults results
   endTime <- getCurrentTime
   when ((numPending resultCount + numFailures resultCount) /= 0) $
      putStrLn "\n  ----- Failures and pending:\n"

   forM_ (pruneOutSuccesses results) $ \x -> do
      printAllTestResults 0 x
      putStrLn ""

   putStrLn $ "\n  -----\nRuntime: " ++ show (diffUTCTime endTime startTime)
   let colorF :: String -> String
       colorF = case resultCount of
          ResultCounts { numPending = 0, numFailures = 0 } -> inGreen
          ResultCounts { numFailures = 0 } -> inYellow
          _ -> inRed
   putStrLn $ colorF $
         "Successes: " ++ show (numSuccesses resultCount)
      ++ ", Pending: " ++ show (numPending resultCount)
      ++ ", Failures: " ++ show (numFailures resultCount)
   when (numFailures resultCount /= 0) $
      exitWith $ ExitFailure 1

-- TODO: maybe can separate producer and consumer here
-- Only reason not to is if we wouldn't get incremental printing of results
runTestWith :: MArgs -> Int -> TestTree Property -> IO (TestTree QC.Result)
runTestWith args depth = \case
   TestLeaf testLabel (Right aProp) -> do
      let timeoutMaybe = case _mArgs_timeoutSecs args of
           Nothing -> id
           Just numSecs -> within $ fromEnum $ numSecs * (10^(6::Int))
      result <- quickCheckWithResult (_mArgs_qcArgs args) $ timeoutMaybe aProp
      let r = TestLeaf testLabel (Right result)
      printSingleTestResult depth r
      pure r
   TestLeaf testLabel (Left Pending) -> do
      let r = TestLeaf testLabel (Left Pending)
      printSingleTestResult depth r
      pure r
   TestBranch testLabel forest -> do
      printSingleTestResult depth $ TestBranch testLabel [] -- Kinda kludge
      results <- forM forest $ runTestWith args (depth + 1)
      pure $ TestBranch testLabel results

printAllTestResults :: Int -> TestTree QC.Result -> IO ()
printAllTestResults depth = \case
   b@(TestBranch _ forest) -> do
      printSingleTestResult depth b
      mapM_ (printAllTestResults (depth + 1)) forest
   l@(TestLeaf{}) -> printSingleTestResult depth l

printSingleTestResult :: Int -> TestTree QC.Result -> IO ()
printSingleTestResult depth resultTree = do
   putStr $ indentationFor depth
   case resultTree of
      TestLeaf testLabel (Right result) -> do
         putStrLn $ showResult (labelStr testLabel) result
      TestLeaf testLabel (Left Pending) -> do
         putStrLn $ inYellow (labelStr testLabel) ++ " - " ++ inYellow "PENDING"
      TestBranch testLabel _ -> do
         putStrLn $ labelStr testLabel
 where
   indentationFor :: Int -> String
   indentationFor n = replicate (n*2) ' ' -- ++ "- "

   showResult :: String -> QC.Result -> String
   showResult testLabel = \case
       -- note: if we wanted to show quickcheck labels, this is where we would:
      Success {} ->
         inGreen testLabel
      failure@(Failure{theException=Nothing}) ->
         inRed testLabel ++ " - "++inRed (replaceNewline (output failure))
      failure {- @(Failure{}) -} ->
         inRed testLabel ++" - "++inRed (replaceNewline (output failure))
   replaceNewline :: String -> String
   replaceNewline = concatMap $ \case '\n' -> " | " ; x -> [x]
   labelStr :: String -> String
   labelStr s = case filter (not . isSpace) s of
      "" -> "(untitled)"
      _ -> s

-- At the end of the test run, after printing the full results,  we print all of
--   the tests which didn't succeed. We get those here:
pruneOutSuccesses :: [TestTree QC.Result] -> [TestTree QC.Result]
pruneOutSuccesses l = mapMaybe f l
 where
   f :: TestTree QC.Result -> Maybe (TestTree QC.Result)
   f = \case
      TestLeaf _ (Right Success{}) -> Nothing
       -- TODO: might want to explicitly pattern-match here:
      x@(TestLeaf _ (Right _)) -> Just x
      x@(TestLeaf _ (Left Pending)) -> Just x
      TestBranch theLabel xs -> case pruneOutSuccesses xs of
         [] -> Nothing
         leftover -> Just $ TestBranch theLabel leftover
      

---------- Handy

-- | An alias for 'describe'. Usually used inside a 'describe' block:
-- 
--   >  describe "replicate" $ do
--   >     it "doubles with 2" $
--   >        replicate 2 'x' === "xx"
--   >     it "creates a list of the right size" $
--   >        \(Positive n) -> length (replicate n 'x') === n
it :: MTestable t => String -> t -> Microspec ()
it = describe


---------- Constructing a test tree:

-- | Something which can be tested
-- 
--   Note both Bools and Properties can be tested, but only Properties show
--   the values that weren't equal
-- 
--   For both unit tests and property tests, if you want to see the outputs
--   of failed tests use 'Test.QuickCheck.==='. If you just want to test for
--   equality, use 'Prelude.=='.
-- 
--   For example, the outputs of running:
-- 
--   @
--   microspec $ do
--      describe "baddies" $ do
--         it "isn't 1 =="  $ 0 == (1 :: Int)
--         it "isn't 1 ===" $ 0 === (1 :: Int)
--         it "isn't always 1 =="  $ \x -> x == (1 :: Int)
--         it "isn't always 1 ===" $ \x -> x === (1 :: Int)
--   @
-- 
--   are:
-- 
--   @
--   isn't 1 == - *** Failed! Falsifiable (after 1 test)
--   isn't 1 === - *** Failed! Falsifiable (after 1 test):  | 0 /= 1
--   isn't always 1 == - *** Failed! Falsifiable (after 1 test):  | 0
--   isn't always 1 === - *** Failed! Falsifiable (after 1 test):  | 0 | 0 /= 1
--   @


class MTestable t where
   -- | Describe a test, e.g.:
   -- 
   --   > describe "reverse 'foo' is 'oof'" $
   --   >    reverse "foo" === "oof"
   describe :: String -> t -> Microspec ()
instance MTestable Property where
   describe testLabel aProp =
      Microspec [TestLeaf testLabel (Right aProp)] ()
instance MTestable Bool where
   describe testLabel bool =
      describe testLabel $ QC.property bool
instance MTestable (TestTree Property) where
   describe testLabel x =
      Microspec [TestBranch testLabel [x]] ()
instance MTestable Pending where
   describe testLabel pend =
      Microspec [TestLeaf testLabel (Left pend)] ()
instance MTestable (Microspec ()) where
   describe testLabel (Microspec forest ()) =
      Microspec [TestBranch testLabel forest] ()
instance (Arbitrary a, Show a, Testable prop) => MTestable (a -> prop) where
   describe testLabel f =
      describe testLabel $ QC.property f

data ResultCounts
   = ResultCounts {
     numSuccesses :: Int
   , numFailures :: Int
   , numPending :: Int
   } deriving (Show)

-- For later, when we don't need to import 'semigroup' for older packages:
{-
-- This might not be the most efficient, but a quick idea:
instance Monoid ResultCounts where
-}
-- "mempty":
emptyResults :: ResultCounts
emptyResults =
   ResultCounts 0 0 0

-- "mappend":
joinResults :: ResultCounts -> ResultCounts -> ResultCounts
(ResultCounts a0 b0 c0) `joinResults` (ResultCounts a1 b1 c1) =
   ResultCounts (a0+a1) (b0+b1) (c0+c1)


-- This is obv mconcat:
joinResultList :: [ResultCounts] -> ResultCounts
joinResultList = foldl' joinResults (ResultCounts 0 0 0)

countResults :: TestTree QC.Result -> ResultCounts
countResults = \case
   TestLeaf _ (Right Success{}) ->
      emptyResults {- mempty -} { numSuccesses = 1 }
   TestLeaf _ (Right _) ->
      emptyResults {- mempty -} { numFailures = 1 }
   TestLeaf _ (Left Pending) ->
      emptyResults {- mempty -} { numPending = 1 }
   TestBranch _ ts ->
      joinResultList {- mconcat -} $ map countResults ts

instance Show (TestTree x) where
 show = \case
   TestBranch testLabel subs ->
      "Branch "++show testLabel++" "++show subs
   TestLeaf testLabel _ ->
      "Leaf " ++ show testLabel

instance Functor Microspec where
   fmap f (Microspec forest a) =
      Microspec forest (f a)
instance Applicative Microspec where
   pure a = Microspec [] a
   f <*> a =
      let Microspec forest0 f' = f
          Microspec forest1 a' = a
      in Microspec (forest0 ++ forest1) (f' a')
instance Monad Microspec where
   return = pure
   ma >>= f =
      let Microspec forest0 a = ma
          Microspec forest1 b = f a
      in Microspec (forest0 ++ forest1) b



---------- Configuration:

-- | Default arguments. Calling \"microspec\" is the same as calling
--   \"microspecWith defaultMArgs\".
defaultMArgs :: MArgs
defaultMArgs = MArgs {
    _mArgs_timeoutSecs = Nothing -- Just 60
   ,_mArgs_qcArgs = QC.stdArgs { chatty = False }
   }

-- | Tweak how tests are run, with 'microspecWith'.
data MArgs = MArgs {
    _mArgs_timeoutSecs :: Maybe Double -- ^ Number of seconds before each
                                        --   test times out. If you want to
                                        --   do this on a per-test basis, try
                                        --   'Test.QuickCheck.Property.within'
   ,_mArgs_qcArgs :: QC.Args -- ^ Arguments to use with QuickCheck tests
   }
 deriving (Show, Read) -- , Eq, Ord)



---------- Pretty-printing:

inRed, inGreen, inYellow :: String -> String
[inRed,inGreen, inYellow] =
   (`map` [31,32,33]) $ \colorNum ->
      \s -> "\ESC["++show (colorNum::Int)++"m"++s++"\ESC[m"



---------- HSpec compatibility

-- | Hspec compatibility. Equivalent to using 'Test.QuickCheck.==='
shouldBe :: (Eq x, Show x) => x -> x -> Property
shouldBe = (===)

-- | @since 0.2.1.0
shouldSatisfy :: Show x => x -> (x -> Bool) -> Property
shouldSatisfy x predicate =
   counterexample ("Predicate failed on: "++show x) (predicate x)

-- | Note that you don't need to use this to create a test, e.g.:
-- 
--   > describe "reverse preserves length" $
--   >    \l -> length (reverse l) === length l
prop :: MTestable prop => String -> prop -> Microspec ()
prop = describe
