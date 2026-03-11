{- |
Choose a set of words so that each alphabet is contained exactly once.

<https://en.wikipedia.org/wiki/Pangram>

This example illustrates the mose.

Contributed by Takayuki Muranushi.
-}
module Main where

import qualified Math.SetCover.Exact as ESC
import qualified Data.Set as Set
import Data.Set (Set, )


{- |
Define the customized 'Assign' type synonym,
that contains the problem-specific label type
and the representation of the set chosen for this problem.
-}
type Assign = ESC.Assign String (Set Char)

{- |
Helper function that creates a value of type 'Assign'.
-}
assign :: String -> Assign
assign str = ESC.assign str $ Set.fromList str

{- |
List of candidate subsets.
The set to be covered is implicitly given as the union of all assigns.
-}
assigns :: [Assign]
assigns = map assign
   ["a", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog",
    "cwm", "fjord", "bank", "glyphs", "vext", "quiz", "veg", "balks", "nth", "pyx"]

{- |
Pretty printer function for a solution.
-}
pprint :: [String] -> IO ()
pprint strs = putStrLn $ unwords strs

{- |
The function @partitions :: [Assign] -> [[label]]@
takes the list of the subsets, and returns all solutions.
-}
main :: IO ()
main = mapM_ pprint $ ESC.partitions assigns


{-
$ runhaskell example/Pangram.hs
vext glyphs bank fjord quiz cwm
pyx nth veg balks fjord quiz cwm

Note that 'partitions' searches for the exact subsets,
while the famous "quick brown fox ..." sentence contains many duplicate alphabets.

Prelude> sort "a quick brown fox jumps over the lazy dog"
"        aabcdeefghijklmnoooopqrrstuuvwxyz"
-}
