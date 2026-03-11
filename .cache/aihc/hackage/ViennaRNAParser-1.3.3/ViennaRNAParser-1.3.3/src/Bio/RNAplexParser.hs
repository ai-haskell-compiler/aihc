-- | Parse RNAplex output
--   For more information on RNAplex consult: <http://www.bioinf.uni-leipzig.de/Software/RNAplex/>

module Bio.RNAplexParser (
                       parseRNAplex,
                       readRNAplex,                                   
                       module Bio.RNAplexData
                      ) where

import Bio.RNAplexData
import Bio.ViennaRNAParserLibrary
import Text.ParserCombinators.Parsec
import Text.Parsec.Numbers
import Control.Monad

-- | Parse the input as list of RNAplexInteraction datatype
parseRNAplexOutput :: GenParser Char st [RNAplexInteraction]
parseRNAplexOutput = do
  rnaPlexInteractions <- many1 (try parseRNAplexInteraction)   
  eof  
  return $ rnaPlexInteractions

-- | Parse the consenus of RNAplex results         
parseRNAplexInteraction :: GenParser Char st RNAplexInteraction
parseRNAplexInteraction = do
  string (">") 
  _targetIdentifier <- many1 (noneOf "\n")                
  newline
  string (">") 
  _queryIdentifier <- many1 (noneOf "\n")                
  newline 
  _secondaryStructure <- many1 (oneOf "&().,")
  many1 space
  _targetDuplexBegin <- parseIntegral
  char ','
  _targetDuplexEnd <- parseIntegral
  many1 space
  char ':'
  many1 space
  _queryDuplexBegin <- parseIntegral
  char ','
  _queryDuplexEnd <- parseIntegral
  many1 space
  char '('
  optional (string " ")
  _duplexEnergy <- parseFloat
  optional (string " ")
  optional (char '=')
  optional (string " ")
  _duplexEnergyWithoutAccessiblity <- optionMaybe (try parseFloat)
  optional (string " ")
  optional (char '+')
  optional (many1 (string " "))
  _queryAccessiblity <- optionMaybe (try parseFloat) 
  optional (string " ")
  optional (char '+')
  optional (many1 space)
  _targetAccessibility <- optionMaybe (try parseFloat)
  char ')'
  many (oneOf " ")
  optional (string "i:")
  _prefilterStart <- optionMaybe (try parseIntegral)
  optional (string ",")
  optional (string "j:")
  _prefilterEnd <- optionMaybe (try parseIntegral)
  many (oneOf " ")
  optional (string "<")
  _prefilterEnergy <- optionMaybe (try parseFloat)
  optional (string ">")
  newline
  return $ RNAplexInteraction _targetIdentifier _queryIdentifier _secondaryStructure _targetDuplexBegin _targetDuplexEnd  _queryDuplexBegin  _queryDuplexEnd  _duplexEnergy _duplexEnergyWithoutAccessiblity _queryAccessiblity _targetAccessibility _prefilterStart _prefilterEnd _prefilterEnergy

-- | parse RNAplexOutput from input string
parseRNAplex :: [Char] -> Either ParseError [RNAplexInteraction]
parseRNAplex input = parse parseRNAplexOutput "parseRNAplexOutput" input

-- | parse from input filePath                      
readRNAplex :: String -> IO (Either ParseError [RNAplexInteraction])                  
readRNAplex filePath = parseFromFile parseRNAplexOutput filePath
