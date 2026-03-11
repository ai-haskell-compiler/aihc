module Main where

import qualified Data.Vector           as Vector
import           System.CPUTime
import           Text.Printf
import Options.Applicative

import qualified Data.PerfectHash.Construction as Construction
import qualified Data.PerfectHash.Lookup       as Lookup
import qualified Data.PerfectHash.Types.Nonces as Nonces

import qualified Exercise


defaultValueCount :: Int
defaultValueCount = 250000


data DemoOptions = DemoOptions {
    valueCount :: Int
  , debugEnabled :: Bool
  }


optionsParser :: Parser DemoOptions
optionsParser = DemoOptions
  <$> option auto
      ( long "count"
      <> help "Value count"
      <> value defaultValueCount)
  <*> switch
      ( long "debug"
      <> help "enable debug mode")


main :: IO ()
main = run =<< execParser opts
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Test the hashing on integers"
     <> header "int test" )


doTimed :: Either String a -> IO Double
doTimed go = do
  start <- getCPUTime
  Exercise.eitherExit go
  end   <- getCPUTime
  return $ fromIntegral (end - start) / (10^12)


run (DemoOptions valueCount _debugEnabled) = do
  putStrLn $ unwords [
      "Keys size:"
    , show $ length intMapTuples
    ]

  let lookup_table = Construction.createMinimalPerfectHash intMapTuples

  putStrLn $ unwords [
      "Finished computing lookup table with"
    , show $ Lookup.size lookup_table
    , "entries."
    ]

  let direct_mapping_nonces = Vector.filter Nonces.isDirectSlot $ Lookup.nonces lookup_table
      direct_mapping_count = Vector.length direct_mapping_nonces
      total_count = length intMapTuples
      direct_mapping_percentage = 100 * direct_mapping_count `div` total_count

  putStrLn $ unwords [
      "There were"
    , show $ Vector.length direct_mapping_nonces
    , "(" ++ show direct_mapping_percentage ++ "%)"
    , "lookup entries with direct mappings."
    ]

  putStrLn "Testing perfect hash lookups..."
  diff1 <- doTimed $ Exercise.testPerfectLookups lookup_table intMapTuples
  putStrLn $ printf "Computation time: %0.3f sec\n" diff1

  putStrLn "Testing HashMap lookups..."
  diff2 <- doTimed $ Exercise.testHashMapLookups intMapTuples
  putStrLn $ printf "Computation time: %0.3f sec\n" diff2

  putStrLn "Done."

  where
    intMapTuples = Exercise.mkIntMapTuples valueCount
