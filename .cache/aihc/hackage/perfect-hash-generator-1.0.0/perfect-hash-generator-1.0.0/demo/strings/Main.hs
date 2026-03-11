module Main where

import           Control.Monad                 (when)
import qualified Data.Map as Map
import Options.Applicative
import qualified Data.PerfectHash.Construction as Construction
import qualified Data.PerfectHash.Lookup       as Lookup
import qualified Exercise


defaultDictionaryPath :: FilePath
defaultDictionaryPath = "/usr/share/dict/words"


data DemoOptions = DemoOptions {
    dictionaryPath :: FilePath
  , debugEnabled :: Bool
  }


optionsParser :: Parser DemoOptions
optionsParser = DemoOptions
  <$> strOption
      ( long "dictionary"
      <> help "Dictionary path"
      <> value defaultDictionaryPath)
  <*> switch
      ( long "debug"
      <> help "enable debug mode")


main :: IO ()
main = run =<< execParser opts
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Test the hashing on strings"
     <> header "string test" )


run (DemoOptions dictionaryPath enableDebug) = do

  word_index_tuples <- Exercise.wordsFromFile dictionaryPath

  putStrLn $ unwords [
      "Words size:"
    , show $ length word_index_tuples
    ]

  let lookup_table = Construction.createMinimalPerfectHash $
        Map.fromList word_index_tuples

  putStrLn $ unwords [
      "Finished computing lookup table with"
    , show $ Lookup.size lookup_table
    , "entries."
    ]

  when enableDebug $ do
    putStrLn $ unwords ["Vector G:", show $ Lookup.nonces lookup_table]
    putStrLn $ unwords ["Vector V:", show $ Lookup.values lookup_table]

  Exercise.eitherExit $ Exercise.testPerfectLookups lookup_table $
    Map.fromList word_index_tuples
