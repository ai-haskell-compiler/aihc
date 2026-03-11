module Main (main) where

import Data.List qualified as List
import Data.Word
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Main
import Hedgehog.Range qualified as Range
import List.Shuffle qualified as List
import System.Random qualified as Random

main :: IO ()
main = do
  defaultMain [checkParallel (Group "tests" (map (\(name, prop) -> (name, withTests 10000 (property prop))) tests))]

tests :: [(PropertyName, PropertyT IO ())]
tests =
  [ ( "shuffle preserves list elements",
      do
        list <- generateList
        gen <- generateGen
        List.sort (List.shuffle_ list gen) === List.sort list
    ),
    ( "sample returns the requested number of elements",
      do
        list <- generateList
        gen <- generateGen
        n <- forAll (Gen.int (Range.linearFrom 0 (-30) (30)))
        length (List.sample_ n list gen) === (min (length list) . max 0) n
    ),
    ( "sample returns a subset of list elements",
      do
        list <- generateList
        gen <- generateGen
        n <- forAll (Gen.int (Range.linearFrom 0 (-30) (30)))
        assert (List.sort (List.sample_ n list gen) `isSubsetOf` List.sort list)
    )
  ]

generateList :: PropertyT IO [Word8]
generateList =
  forAll (Gen.list (Range.linear 0 200) (Gen.word8 Range.linearBounded))

generateGen :: PropertyT IO Random.StdGen
generateGen =
  Random.mkStdGen <$> forAll (Gen.int Range.constantBounded)

-- precondition: lists are sorted
isSubsetOf :: (Ord a) => [a] -> [a] -> Bool
isSubsetOf [] _ = True
isSubsetOf (_ : _) [] = False
isSubsetOf (x : xs) (y : ys) =
  case compare x y of
    LT -> False
    EQ -> isSubsetOf xs ys
    GT -> isSubsetOf (x : xs) ys
