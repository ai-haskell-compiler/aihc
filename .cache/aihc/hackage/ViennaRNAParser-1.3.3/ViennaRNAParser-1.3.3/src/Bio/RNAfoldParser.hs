-- | Parse RNAfold output
--   For more information on RNAfold consult: <http://www.tbi.univie.ac.at/RNA/RNAfold>

module Bio.RNAfoldParser (
                       systemRNAfold,
                       parseRNAfold,
                       readRNAfold,
                       systemRNAfoldOptions,
                       parseRNAMEAfold,
                       readRNAMEAfold,
                       module Bio.RNAfoldData
                      ) where

import Bio.RNAfoldData
import Text.ParserCombinators.Parsec
import Bio.ViennaRNAParserLibrary
import System.Process 
import System.Exit
import qualified Control.Exception.Base as CE

-- | Run external RNAfold command and read the output into the corresponding datatype
systemRNAfold :: String -> String -> IO ExitCode
systemRNAfold inputFilePath outputFilePath = system ("RNAfold --noPS  <" ++ inputFilePath  ++ " >" ++ outputFilePath)

-- | Run external RNAfold command and read the output into the corresponding datatype
systemRNAfoldOptions :: String -> String -> String -> IO ExitCode
systemRNAfoldOptions foldoptions inputFilePath outputFilePath = system ("RNAfold --noPS " ++ foldoptions ++  "  <" ++ inputFilePath  ++ " >" ++ outputFilePath)

-- | Parse the RNAfold results
genParserRNAfold :: GenParser Char st RNAfold
genParserRNAfold = do
  string (">") 
  _sequenceIdentifier <- many1 (noneOf "\n")                
  newline
  _sequence <- many1 (noneOf "\n")                
  newline 
  _secondaryStructure <- many1 (oneOf "&().,")
  space
  string ("(")
  _foldingEnergy <- many1 (noneOf ")")
  string (")")
  return $ RNAfold _sequenceIdentifier _sequence _secondaryStructure (readDouble _foldingEnergy)  

-- | parse RNAfold output from input string
parseRNAfold :: [Char] -> Either ParseError RNAfold
parseRNAfold input = parse genParserRNAfold "genParseRNAfold" input

-- | parse RNAfold output from input filePath                      
readRNAfold :: String -> IO (Either ParseError RNAfold)                  
readRNAfold filePath = do
  parsedFile <- parseFromFile genParserRNAfold filePath
  CE.evaluate parsedFile


-- | Parse the RNAfold maximum expected accuracy results
genParserRNAMEAfold :: GenParser Char st RNAfoldMEA
genParserRNAMEAfold = do
  string (">") 
  _sequenceIdentifier <- many1 (noneOf "\n")                
  newline
  _sequence <- many1 (noneOf "\n")                
  newline
  --MFE
  _mfestructure <- many1 (oneOf "&().,")
  space
  string ("(")
  _mfefoldingEnergy <- many1 (noneOf ")")
  string (")")
  --coarse representation
  newline 
  _coarsestructure <- many1 (oneOf "&().,")
  space
  string ("[")
  _coarsefoldingEnergy <- many1 (noneOf "]")
  string ("]")
  --centroid structure
  newline 
  _centroidstructure <- many1 (oneOf "&().,")
  space
  string ("{")
  _centroidfoldingEnergy <- many1 (noneOf " ")
  string (" ")
  _centroiddistance <- many1 (noneOf "}")
  string ("}")
  --MEA
  newline
  _meastructure <- many1 (oneOf "&().,")
  space
  string ("{")
  _meafoldingenergy <- many1 (noneOf " ")
  string (" ")
  _meadistance <- many1 (noneOf "}")
  string ("}")
  newline
  string " frequency of mfe structure in ensemble "
  _mfefreq <- many1 (noneOf ";")
  string "; ensemble diversity "
  _ensemblediversity <- many1 (noneOf " ")
  string " "
  return $ RNAfoldMEA _sequenceIdentifier _sequence _mfestructure (readDouble _mfefoldingEnergy) _coarsestructure (readDouble _coarsefoldingEnergy) _centroidstructure (readDouble _centroidfoldingEnergy) (readDouble _centroiddistance) _meastructure (readDouble _meafoldingenergy) (readDouble _meadistance) (readDouble _mfefreq) (readDouble _ensemblediversity)

-- | parse RNAfold output from input string
parseRNAMEAfold :: [Char] -> Either ParseError RNAfoldMEA
parseRNAMEAfold input = parse genParserRNAMEAfold "genParseRNAfold" input

-- | parse RNAfold output from input filePath                      
readRNAMEAfold :: String -> IO (Either ParseError RNAfoldMEA)                  
readRNAMEAfold filePath = do
  parsedFile <- parseFromFile genParserRNAMEAfold filePath
  CE.evaluate parsedFile
