{-# LANGUAGE CPP, DeriveDataTypeable, MonoLocalBinds, NamedFieldPuns,
             ScopedTypeVariables #-}

-- TODO: Tests passed even though for finiteTreeOf and finiteListOf
-- transform was only applied once at the top-level!

-- | Tests for "Test.ChasingBottoms.ContinuousFunctions". So far the
-- tests are rather weak.

module Test.ChasingBottoms.ContinuousFunctions.Tests (tests) where

import Test.ChasingBottoms.ContinuousFunctions
import Test.ChasingBottoms.IsBottom
import Test.ChasingBottoms.SemanticOrd
import Test.ChasingBottoms.TestUtilities
import qualified Test.ChasingBottoms.TestUtilities.Generators as Gen
import Test.ChasingBottoms.TestUtilities.Generators (Tree(..))
import Test.ChasingBottoms.ApproxShow
import Data.Generics
import Test.QuickCheck
import Test.ChasingBottoms.TestUtilities
import Control.Arrow
import Control.Monad
import Data.List
#if MIN_VERSION_QuickCheck(2,12,0)
import qualified Data.Map.Strict as Map
#endif
import Data.Ratio

------------------------------------------------------------------------
-- Example data type

finiteTreeOf :: MakeResult a -> MakeResult (Tree a)
finiteTreeOf makeResult = sized' tree
  where
  tree size = transform $
    if size == 0 then
      baseCase
     else
      frequency' [ (1, baseCase)
                 , (1, liftM2 Branch tree' tree')
                 ]
    where
    tree' = tree (size `div` 2)

    baseCase =
      frequency' [ (1, return bottom)
                 , (2, liftM Leaf makeResult)
                 ]

------------------------------------------------------------------------
-- Helpers

integer :: Gen Integer
integer = frequency [ (1, return bottom)
                    , (9, arbitrary)
                    ]

length' :: Num b => [a] -> b
length' xs | isBottom xs = 0
length' []               = 1
length' (x:xs)           = 1 + length' xs

depth :: (Ord b, Num b) => Tree a -> b
depth t | isBottom t = 0
depth (Leaf {})      = 1
depth (Branch l r)   = 1 + (depth l `max` depth r)

------------------------------------------------------------------------
-- Tests

-- Interesting properties for the function generators:
--
-- * Surjectivity.
--
-- * Decent distribution.
--
-- How do we test these properties?

type DistributionTest = Int -> [(String, Double)] -> (Bool, String)

testDistribution :: Testable a => DistributionTest -> a -> IO Bool
testDistribution test t = do
  result <- run t
  let (ok, msg) = apply test result
  unless ok $ putStrLn msg
  return ok
  where
#if MIN_VERSION_QuickCheck(2,12,0)
  convert numTests labels =
    map (\(x, f) -> (x, fromIntegral f / fromIntegral numTests)) $
    Map.toList $
    Map.fromListWith (+)
      [ (l, n) | (ls, n) <- Map.toList labels, l <- ls ]
#else
  convert _ labels = labels
#endif

  apply test Success{numTests, labels} = test numTests
                                              (convert numTests labels)
  apply _    _                         = (False, "Test failed.")

spread :: DistributionTest
spread numTests labels =
  (uniqueShare >= 3%4, "uniqueShare: " ++ show uniqueShare)
  where
  noUniqueArgs = length labels
  uniqueShare  = noUniqueArgs % numTests

len :: Integer -> Double -> Integer -> DistributionTest
len max avg short numTests labels =
  ( maxLen >= max && averageLen >= avg && shortShare >= 0.1
  , "maxLen: " ++ show maxLen ++
    ", averageLen: " ++ show averageLen ++
    ", shortShare: " ++ show shortShare
  )
  where
  lengths    = map (read *** id) labels :: [(Integer, Double)]
  maxLen     = maximum $ map fst lengths
  averageLen = sum $ map (\(n, f) -> fromInteger n * f) lengths
  shortShare = sum . map snd . filter ((<= short) . fst) $ lengths

-- | We want to make sure that we can generate many different kinds of
-- lazy functions.

prop_many_functions_rather_lazy = testDistribution spread $
  forAll (functionTo (finiteTreeOf (finiteTreeOf flat))) $
    \(f :: Tree Integer -> Tree (Tree Bool)) ->
      f bottom /=! bottom && f (Leaf bottom) <! f (Leaf 1) ==>
      collect (map (approxShow 100 . f) [bottom, Leaf bottom, Leaf 1]) $
        True

-- | The generated lists should not be too short.

prop_lists_have_decent_length = testDistribution (len 20 5 5) $
  forAll (functionTo (finiteListOf flat)) $ \(f :: Integer -> [Bool]) ->
  forAll integer $ \(i :: Integer) ->
    collect (length' (f i) :: Integer) $
      True

-- | The generated trees should not be too shallow.

prop_trees_have_decent_depth = testDistribution (len 6 2 2) $
  forAll (functionTo (finiteTreeOf flat)) $ \(f :: Integer -> Tree Bool) ->
  forAll integer $ \(i :: Integer) ->
    collect (depth (f i) :: Integer) $
      True

-- | In one version of Data.Generics the following equations were
-- valid:
--
--   * @'toConstr' ('bottom' :: ())  = 'toConstr' ()@
--
--   * @'toConstr' ('bottom' :: One) = _|_@
--
-- 'toConstr' should be strict. There is a workaround for this (using
-- seq) in "Test.ChasingBottoms.ContinuousFunctions", and the
-- following two tests check that this workaround works.

data One
  = One
    deriving (Typeable, Data)

prop_some_lazy_unit =
  forAll (functionTo (finiteTreeOf flat)) $ \(f :: () -> Tree Bool) ->
    f bottom <! f () ==>
      True

prop_some_lazy_One =
  forAll (functionTo (finiteTreeOf flat)) $ \(f :: One -> Tree Bool) ->
    f bottom <! f One ==>
      True

-- | Example from documentation. Here mostly to check that it type
-- checks.

prop_example_works =
  forAll (functionTo (finiteTreeOf flat)) $
    \(f :: Bool -> Tree Integer) ->
      f bottom <=! f True && f bottom <=! f False

-- | Generated functions should be monotone.

prop_functions_monotone =
  forAll (functionTo (finiteListOf flat))
    $ \(f :: Tree Integer -> [Bool]) ->
      forAll (pair (Gen.finiteTreeOf Gen.integer)
                   (Gen.geTreeOf Gen.integer Gen.geInteger Gen.finiteTreeOf))
        $ \(x, y) ->
          x <=! y && f x <=! f y

------------------------------------------------------------------------
-- | All tests collected together.

tests :: IO Bool
tests = do
  b1 <- fmap and $ sequence theIOTests
  b2 <- runQuickCheckTests $ map run $ concat theTests
  return (b1 && b2)
  where
    theIOTests :: [IO Bool]
    theIOTests = []

    -- Disabled, because occasionally one or more of the tests failed,
    -- and (at the time of writing in 2015 and 2017) I have no
    -- interest in fixing test suite bugs in old, unfinished and
    -- experimental code. Known problems (in code that has later been
    -- changed due to changes to QuickCheck):
    -- * Division by zero, presumably because noArgs is 0.
    -- * After reducing maxSuccess from 1000 to 100 I once observed
    --   that "averageLen" was 199 % 100, but if I am not mistaken the
    --   test requires it to be >= 2.

    -- theIOTests = [ prop_many_functions_rather_lazy
    --              , prop_lists_have_decent_length
    --              , prop_trees_have_decent_depth
    --              ]

    theTests :: [[Property]]
    theTests = [ [ prop_example_works
                 , prop_some_lazy_unit
                 , prop_some_lazy_One
                 , prop_functions_monotone
                 ]
               ]

------------------------------------------------------------------------
-- Manual inspection of function tables

viewFun :: (ApproxShow b, Data a) => MakeResult b -> [a] -> IO ()
viewFun (makeResult :: MakeResult b) (inputs :: [a]) = quickCheck $
  forAll (functionTo makeResult) $ \(f :: a -> b) ->
    collect (map (approxShow 5 . f) inputs) $ True

bool       = undefined :: Bool
int        = undefined :: Int
float      = undefined :: Float
treeOfBool = undefined :: Tree Bool

test0 = viewFun (flat :: MakeResult Bool) [bottom, False, True]
test1 = viewFun (finiteTreeOf flat :: MakeResult (Tree Bool))
                [bottom, False, True]
test2 = viewFun (finiteTreeOf flat :: MakeResult (Tree Bool))
                [bottom, Leaf bottom, Leaf False]

test4 = viewFun (flat :: MakeResult Int) [bottom, False, True]
test5 = viewFun (flat :: MakeResult Float) [bottom, False, True]
