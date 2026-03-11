{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Bench
import Control.Monad
import Data.Graph.Permutation
import Data.Graph.Construction
import Data.Graph.Automorphism
import Data.Graph
import Data.List.Extra                  (nubOrd)
import Data.Array
import Shuffle
import Test.QuickCheck
import Sudoku

import System.Random

naut1 :: Graph
naut1 = buildG (1,6) [(1,2), (2,3), (3,1), (1,4), (2,5), (5,6)]

naut2 :: Graph
naut2 = buildG (1,7)
        [(1,2), (2,3), (3,4), (4,5), (5,1),
         (3,6), (6,7), (7,4),
         (2,6)]

nex1 :: Graph
nex1 = buildG (1, 6)
      [ (1,2), (2,3), (3,1),
        (1,4), (2,5), (3,6) ]

cleanG :: Array Vertex [Vertex] -> Array Vertex [Vertex]
cleanG = fmap nubOrd

cae :: Array Vertex [Vertex]
cae = cleanG $ undirG $ buildG (0,9) $ edges (prismG 4) ++
         [ (8,0), (8,1), (8,2), (8,3),
           (9,4), (9,5), (9,6), (9,7) ]

canonicGraph' :: Array Vertex [Vertex] -> Graph
canonicGraph' = withUnitPartition canonicGraph

main :: IO ([Char], [(String, Result)])
main = runBenchmark $ test_canonic "default" canonicGraph'

prop_canonicLabelling
  :: Eq a
  => (Graph -> a)
  -> Array Int [Vertex] -> Property
prop_canonicLabelling canonic gr
    = forAll (arbitraryPerm (bounds gr)) (\p -> canonic (applyPerm p gr) == canonic gr)

runOneTest :: Testable prop => (prop, String) -> Benchmark Result
runOneTest (test, name) = withLab name $
                          timeIO $
    do putStr $ name ++ "..."
       quickCheckWithResult stdArgs {
         maxDiscardRatio = 20,
         maxSuccess = 200
         } test

runTests :: Testable prop
  => String
  -> [(prop, String)]
  -> Benchmark ([Char], [(String, Result)])
runTests groupName propTests =
    do blift $ putStrLn $ "Running tests " ++ groupName
       results <- mapM runOneTest propTests
       let ok = all isSuccess results
       let failing = filter (not . isSuccess . snd) $ zip (map snd propTests) results
       blift $ putStrLn $ "RESULT: " ++ groupName ++ " " ++ if ok then "PASSED" else "FAILED"
       return (groupName, failing)

test_canonic
  :: Eq a
  => String
  -> (Graph -> a)
  -> Benchmark ([Char], [(String, Result)])
test_canonic name canonic
    = withLab name $ runTests name [(prop_canonicLabelling canonic gr,n) | (gr,n) <- graphs]
      where graphs = [
                      (naut1,"naut1"), (naut2,"naut2"), (nex1,"nex1"), (cae,"cae"),
                      (undirG $ hCubeG 5, "hcube"),
                      (undirG $ cycleG 60, "cycle"),
                      (tensorG [10,10], "grid"),
                      (sudokuG, "sudoku"),
                      (emptyG 13, "empty"),
                      (unionG arcG (productG (cliqueG (1,4)) (linearG 13)), "deck1"),
                      (unionG arcG (productG (cliqueG (1,4)) (emptyG 7)), "deck0")
                     ]

process :: Graph -> IO ()
process (gr0::Graph) =
     do randG <- newStdGen
        let perm :: Permutation = randomPerm (bounds gr0) randG
        let gr :: Graph = applyPerm perm gr0
        putStrLn "Shuffled with"
        print perm
        putStrLn "initial:"
        print gr
        let (aut, result) = withUnitPartition automorphisms gr
        putStrLn "Automorphism group generator:"
        print aut
        putStrLn "Canonic labeling:"
        print result

-- | Generates a random permutation in the given bounds.
randomPerm :: RandomGen g => (Vertex, Vertex) -> g -> Permutation
randomPerm bnds@(low, high) g = listArray bnds $ shuffle1 (range bnds) (randomList (high-low) g)

arbitraryPerm :: (Int, Int) -> Gen (Array Int Int)
arbitraryPerm bnds@(low, high) = return (listArray bnds . shuffle1 (range bnds)) `ap` shuffleList (high-low)

randomList :: RandomGen g => Int -> g -> [Int]
randomList 0 _ = []
randomList n g = x:randomList (n-1) g'
    where (x, g') = randomR (0,n) g

shuffleList :: (Eq t, Num t, Random t) => t -> Gen [t]
shuffleList 0 = return []
shuffleList n = return (:) `ap` choose (0,n) `ap` shuffleList (n-1)

