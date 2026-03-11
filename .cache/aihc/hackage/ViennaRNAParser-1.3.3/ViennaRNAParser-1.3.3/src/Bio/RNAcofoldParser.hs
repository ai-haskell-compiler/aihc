-- | Parse RNAfold output
--   For more information on RNAcofold consult: <http://www.tbi.univie.ac.at/RNA/RNAcofold.html>

module Bio.RNAcofoldParser (
                       systemRNAcofold,
                       parseRNAcofold,
                       readRNAcofold,                                   
                       module Bio.RNAcofoldData
                      ) where

import Bio.RNAcofoldData
import Bio.ViennaRNAParserLibrary
import Text.ParserCombinators.Parsec
import System.Process 
import System.Exit
import qualified Control.Exception.Base as CE

-- | Run external RNAcofold command 
systemRNAcofold :: String -> String -> IO ExitCode
systemRNAcofold inputFilePath outputFilePath = system ("RNAcofold  <" ++ inputFilePath  ++ " >" ++ outputFilePath)

-- | Parse the consenus of RNAcofold results         
genParserRNAcofold :: GenParser Char st RNAcofold
genParserRNAcofold = do 
  _coFoldSequence1 <- many1 (oneOf "NATUGCatugc")
  string "&"
  _coFoldSequence2 <- many1 (oneOf "NATUGCatugc")
  newline
  _coFoldSecondaryStructure1 <- many1 (oneOf "().,")
  string "&"
  _coFoldSecondaryStructure2 <- many1 (oneOf "().,")
  space
  string ("(")
  optional space
  _coFoldingEnergy <- many1 (oneOf "-1234567890.")
  string (")")
  return $ RNAcofold _coFoldSequence1 _coFoldSequence2 _coFoldSecondaryStructure1 _coFoldSecondaryStructure2 (readDouble _coFoldingEnergy)

-- | parse RNAcofold output from input string
parseRNAcofold :: [Char] -> Either ParseError RNAcofold
parseRNAcofold input = parse genParserRNAcofold "genParseRNAcofold" input

-- | parse RNAcofold output from input filePath                      
readRNAcofold :: String -> IO (Either ParseError RNAcofold)                  
readRNAcofold filePath = do
  parsedFile <- parseFromFile genParserRNAcofold filePath
  CE.evaluate parsedFile
