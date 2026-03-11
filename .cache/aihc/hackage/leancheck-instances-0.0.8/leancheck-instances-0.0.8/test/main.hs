-- Copyright (c) 2015-2022 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import System.Exit (exitFailure)
import Data.List (elemIndices)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Set (Set)
import Data.Map (Map)
import Data.Sequence (Seq)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Numeric.Natural

import Test.LeanCheck
import Test.LeanCheck.Instances

main :: IO ()
main =
  case elemIndices False (tests 1000) of
  [] -> putStrLn "Tests passed!"
  is -> putStrLn ("Failed tests:" ++ show is) >> exitFailure

tests :: Int -> [Bool]
tests n =
  [ True

  , fails n $ \ts -> T.words (T.unwords ts) == ts
  , fails n $ \t ->   T.unwords (T.words t) == t
  , holds n $ \ts ->   T.words (T.unwords (T.words ts)) == T.words ts

  , holds n $ \x y -> x + y == y + (x :: Natural)
  , fails n $ \x y -> x + y == (x :: Natural)

  , fails n $ \bs -> BS.reverse bs == bs
  , holds n $ \bs -> BS.reverse (BS.reverse bs) == bs

  , fails n $ \bs -> BL.reverse bs == bs
  , holds n $ \bs -> BL.reverse (BL.reverse bs) == bs

  , holds n $ \s -> Seq.reverse (Seq.reverse s :: Seq Int) == s
  , fails n $ \s -> Seq.reverse (s :: Seq Int) == s

  , holds n $ \a b -> a `Set.union` b == b `Set.union` (a :: Set Int)
  , fails n $ \a b -> a `Set.union` b == (a :: Set Int)

  , fails n $ \m1 m2 -> m1 `Map.union` m2 == m2 `Map.union` (m1 :: Map Int Int)
  , holds n $ \m1 -> m1 `Map.union` m1 == (m1 :: Map Int Int)
  ]
