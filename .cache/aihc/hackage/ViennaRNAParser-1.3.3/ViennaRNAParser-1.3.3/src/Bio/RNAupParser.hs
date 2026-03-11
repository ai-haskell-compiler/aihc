-- | Parse RNAup interaction_first output
--   For more information on RNAup consult: <http://www.tbi.univie.ac.at/RNA/RNAup.html>

module Bio.RNAupParser (
                       parseRNAup,
                       readRNAup,                                   
                       module Bio.RNAupData
                      ) where

import Bio.RNAupData
import Bio.ViennaRNAParserLibrary
import Text.ParserCombinators.Parsec    
import Control.Monad

-- | Parse the input as list of RNAupInteraction datatype
parseRNAupOutput :: GenParser Char st [RNAupInteraction]
parseRNAupOutput = do
  string (">")
  queryIdentifier <- many1 (noneOf "\n")
  newline
  rnaUpInteractions <- many1 (try (parseRNAupInteraction queryIdentifier))   
  eof  
  return $ rnaUpInteractions

-- | Parse the consenus of RNAup results         
parseRNAupInteraction :: String -> GenParser Char st RNAupInteraction
parseRNAupInteraction queryIdentifier = do
  string (">") 
  targetIdentifier <- many1 (noneOf "\n")                
  newline
  rnaupInteractionRegions <- many1 (try parseRNAupInteractionRegion)
  return $ RNAupInteraction queryIdentifier targetIdentifier rnaupInteractionRegions

-- | Parse a RNAupInteractionRegion
parseRNAupInteractionRegion :: GenParser Char st RNAupInteractionRegion
parseRNAupInteractionRegion = do
  upsecondaryStructure <- many1 (oneOf "&().,")
  many1 space
  uptargetDuplexBegin <- many1 digit
  char ','
  uptargetDuplexEnd <- many1 digit
  many1 space
  char ':'
  many1 space
  upqueryDuplexBegin <- many1 digit
  char ','
  upqueryDuplexEnd <- many1 digit
  many1 space
  char '('
  upduplexEnergy <- many1 (noneOf (" )"))
  optional space
  optional (char '=')
  optional space
  upduplexEnergyWithoutAccessiblity <- optionMaybe (try (many1 (noneOf (" )"))))
  optional space 
  optional (char '+')
  optional (many1 space)
  upqueryAccessiblity <- optionMaybe (try (many1 (noneOf (" )")))) 
  optional space 
  optional (char '+')
  optional (many1 space)
  uptargetAccessibility <- optionMaybe (try (many1 (noneOf (")"))))
  char ')'
  newline
  querySequence <- many1 (noneOf ("&"))
  char '&'
  targetSequence <- many1 (noneOf ("\n"))
  newline
  _upOutputFileName <- optionMaybe (try parseRNAupFileName)
  return $ RNAupInteractionRegion upsecondaryStructure (readInt uptargetDuplexBegin) (readInt uptargetDuplexEnd) (readInt upqueryDuplexBegin) (readInt upqueryDuplexEnd) (readDouble upduplexEnergy) (liftM readDouble upduplexEnergyWithoutAccessiblity) (liftM readDouble upqueryAccessiblity) (liftM readDouble uptargetAccessibility) querySequence targetSequence _upOutputFileName

-- | Parse a RNAupFileName
parseRNAupFileName :: GenParser Char st String
parseRNAupFileName = do
  string "RNAup output in file: "
  fileName <- many1 (noneOf ("\n>"))
  newline
  return fileName

-- | parse RNAupOutput from input string
parseRNAup :: [Char] -> Either ParseError [RNAupInteraction]
parseRNAup input = parse parseRNAupOutput "parseRNAupOutput" input

-- | parse from input filePath                      
readRNAup :: String -> IO (Either ParseError [RNAupInteraction])                  
readRNAup filePath = parseFromFile parseRNAupOutput filePath
