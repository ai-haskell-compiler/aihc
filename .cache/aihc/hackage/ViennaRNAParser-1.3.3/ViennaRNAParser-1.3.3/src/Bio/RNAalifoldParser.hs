-- | Parse RNAalifold output
--   For more information on RNAalifold consult: <<http://www.tbi.univie.ac.at/RNA/RNAalifold.html>

module Bio.RNAalifoldParser (
                       systemRNAalifold,
                       parseRNAalifold,
                       readRNAalifold,                                 
                       module Bio.RNAalifoldData
                      ) where

import Bio.RNAalifoldData
import Bio.ViennaRNAParserLibrary
import Text.ParserCombinators.Parsec
import System.Process
import System.Exit
import qualified Control.Exception.Base as CE

--- | Run external RNAalifold command and read the output into the corresponding datatype
systemRNAalifold :: String -> String -> String -> IO ExitCode
systemRNAalifold options inputFilePath outputFilePath = system ("RNAalifold " ++ options  ++ " " ++ inputFilePath  ++ " > " ++ outputFilePath)

-- | Parse the consenus of RNAz results         
genParserRNAalifold :: GenParser Char st RNAalifold
genParserRNAalifold = do
  _sequence <- many1 (noneOf "\n")                
  newline
  secondaryStructure <- many1 (oneOf "&().,")
  string (" (")
  optional space
  foldingEnergy <- many1 (oneOf "-.1234567890")
  string " ="
  many1 space
  initialFoldingEnergy <- many1 (noneOf " ")
  string (" +")
  many1 space
  covarianceContributionEnergy <- many1 (noneOf ")")
  string (")")
  many1 space
  eof
  return $ RNAalifold _sequence secondaryStructure (readDouble foldingEnergy) (readDouble initialFoldingEnergy) (readDouble covarianceContributionEnergy)
   
-- | parse RNAalifold output from input string
parseRNAalifold :: [Char] -> Either ParseError RNAalifold
parseRNAalifold input = parse genParserRNAalifold "genParseRNAalifold" input

-- | parse RNAalifold output from input filePath                      
readRNAalifold :: String -> IO (Either ParseError RNAalifold)
readRNAalifold filePath = do
  parsedFile <- parseFromFile genParserRNAalifold filePath
  CE.evaluate parsedFile

