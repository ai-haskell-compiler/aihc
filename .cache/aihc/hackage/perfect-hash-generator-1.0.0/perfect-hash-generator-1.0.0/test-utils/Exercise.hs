-- | This module is not used by the main library for constructing perfect hashes;
-- it just provides convenience functions for use by the tests and demonstration executables.
module Exercise where

import           Control.Monad            (unless)
import           Data.Foldable            (traverse_)
import qualified Data.Map as Map
import           Data.Map                 (Map)
import           Data.IntSet              (IntSet)
import qualified Data.IntSet              as IntSet
import           System.Random            (RandomGen, mkStdGen, random)

import qualified Data.PerfectHash.Hashing as Hashing
import qualified Data.PerfectHash.Lookup  as Lookup


-- | genericized to facilitate benchmarking
testLookupsHelper
  :: (Show b, Eq b, Show a, Hashing.ToHashableChunks a)
  => (a -> b) -- ^ lookup function
  -> Map a b
  -> Either String ()
testLookupsHelper lookup_function =
  traverse_ check_entry . Map.toList
  where
    check_entry (word, source_index) = unless (lookup_result == source_index) $
      Left $ unwords [
          "Result for key"
        , show word
        , "had incorrect index"
        , show lookup_result
        , "; should have been"
        , show source_index
        ]
      where
        lookup_result = lookup_function word


testHashMapLookups
  :: (Show b, Eq b, Show a, Ord a, Hashing.ToHashableChunks a)
  => Map a b
  -> Either String ()
testHashMapLookups hash_map = testLookupsHelper
  (\x -> Map.findWithDefault (error "not found") x hash_map)
  hash_map


testPerfectLookups
  :: (Show b, Eq b, Show a, Hashing.ToHashableChunks a)
  => Lookup.LookupTable b
  -> Map a b
  -> Either String ()
testPerfectLookups = testLookupsHelper . Lookup.lookup


-- | Generate a map of words from a file to their line numbers.
--
-- Intended for use with @\"/usr/share/dict/words\"@.
wordsFromFile :: FilePath -> IO [(String, Int)]
wordsFromFile path = do
  file_lines <- readFile path
  return $ zip (lines file_lines) [1..]


-- * Random integers

data RandIntAccum t = RandIntAccum
  t -- ^ random number generator
  Int -- ^ max count
  IntSet -- ^ accumulated unique random numbers


mkIntMapTuples :: Int -> Map Int Int
mkIntMapTuples valueCount = Map.fromList $ zip random_ints [1..]
  where
    seed_value = RandIntAccum (mkStdGen 0) valueCount IntSet.empty
    random_ints = IntSet.toList $ getUniqueRandomIntegers seed_value


-- | Since computing the size of the set is O(N), we
-- maintain the count separately.
getUniqueRandomIntegers :: RandomGen t => RandIntAccum t -> IntSet
getUniqueRandomIntegers (RandIntAccum std_gen count current_set) =

  if count == 0
    then current_set
    else getUniqueRandomIntegers newstate

  where
    (next_int, next_std_gen) = random std_gen

    a = RandIntAccum next_std_gen
    newstate = if IntSet.member next_int current_set
      then a count current_set
      else a (count - 1) (IntSet.insert next_int current_set)

-- * Other utilities

eitherExit :: Either String b -> IO ()
eitherExit x = case x of
  Left err -> error err
  Right _ -> return ()