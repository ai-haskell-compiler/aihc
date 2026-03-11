
-- | Parse RNAz output
--   For more information on RNAz consult: <http://www.tbi.univie.ac.at/~wash/RNAz
module Bio.RNAzParser (
                       systemRNAz,
                       parseRNAz,
                       readRNAz,                                   
                       module Bio.RNAzData
                      ) where

import Bio.RNAzData
import Bio.ViennaRNAParserLibrary
import Text.ParserCombinators.Parsec
import System.Process
import System.Exit
import qualified Control.Exception.Base as CE

-- | Run external RNAz command and read the output into the corresponding datatype
systemRNAz :: String -> String -> String -> IO ExitCode
systemRNAz options inputFilePath outputFilePath = system ("RNAz " ++ options ++ " " ++ inputFilePath ++ " >" ++ outputFilePath)

-- | Parse the input as RNAz datatype
genParseRNAz :: GenParser Char st RNAz
genParseRNAz = do
  char '\n'
  many1 (oneOf "# ")
  string "RNAz"
  space
  _version <- many1 (noneOf " ")
  space
  space
  many1 (char '#')
  newline  
  space
  _sequences <- parseRNAzIntField "Sequences:"
  _columns <- parseRNAzIntField "Columns:"
  _readingDirection <- parseRNAzStringField "Reading direction:"
  _meanPairwiseIdentity <- parseRNAzDoubleField "Mean pairwise identity:"
  _shannonEntropy <- parseRNAzDoubleField "Shannon entropy:" 
  _gcContent <- parseRNAzDoubleField "G+C content:"
  _meanSingleSequenceMFE <- parseRNAzDoubleField "Mean single sequence MFE:"
  _consensusMFE <- parseRNAzDoubleField "Consensus MFE:"
  _energyContribution <- parseRNAzDoubleField "Energy contribution:"
  _covarianceContribution <- parseRNAzDoubleField "Covariance contribution:"
  _combinationsPair <- parseRNAzDoubleField "Combinations/Pair:"
  _meanZScore <- parseRNAzDoubleField "Mean z-score:"
  _structureConservationIndex <- parseRNAzDoubleField "Structure conservation index:"
  _backgroundModel <- parseRNAzStringField "Background model:"
  _decisionModel <- parseRNAzStringField "Decision model:"
  _svmDecisionValue <-  parseRNAzDoubleField "SVM decision value:"
  _svmRNAClassProbability <- parseRNAzDoubleField "SVM RNA-class probability:"
  _prediction <- parseRNAzStringField "Prediction:"
  _ <- many (try (parseRNAzStringField "WARNING:"))
  newline
  many1 (char '#') 
  newline
  newline
  _rnaZResults  <- many1 (try parseRNAzResult)
  _rnaZConsensus <- parseRNAzConsensus         
  return $ RNAz _version _sequences _columns _readingDirection _meanPairwiseIdentity _shannonEntropy _gcContent _meanSingleSequenceMFE _consensusMFE _energyContribution _covarianceContribution _combinationsPair _meanZScore _structureConservationIndex _backgroundModel _decisionModel _svmDecisionValue _svmRNAClassProbability _prediction _rnaZResults _rnaZConsensus

-- | Parse a RNAz field containing a Double 
parseRNAzDoubleField :: String -> GenParser Char st Double
parseRNAzDoubleField fieldname = do
  optional space
  string fieldname
  many1 space
  double <- (many1 (noneOf " ")) 
  space
  return $ (readDouble double)

-- | Parse a RNAz field containing a String         
parseRNAzStringField :: String -> GenParser Char st String
parseRNAzStringField fieldname = do
  optional space
  string fieldname
  space
  stringField <- many1 (noneOf "\n")
  space
  return $ stringField          

-- | Parse a RNAz field containing a Int          
parseRNAzIntField :: String -> GenParser Char st Int
parseRNAzIntField fieldname = do
  optional space
  string fieldname
  space
  int <- many1 (noneOf " ")
  space
  return $ (readInt int)

-- | Parse a RNAz result        
parseRNAzResult :: GenParser Char st RNAzResult
parseRNAzResult = do
  _header <- many1 (noneOf "\n")
  newline 
  notFollowedBy (string (">consensus"))
  _resultSequence <- parseNucleotideAlignmentEntry
  newline        
  _dotBracket <- many1 (oneOf "-().,")
  space
  char ('(')
  space
  _mfe <- many1 (noneOf ",")
  char ','
  space
  string ("z-score")
  space
  char '='
  space
  _zscore <- many1 (noneOf ",")
  char ','
  space
  _zScoreCalculationApproach <- choice [char 'S', char 'R'] --oneOf "RS"
  char (')')
  newline
  return $ RNAzResult _header _resultSequence _dotBracket (readDouble _mfe) (readDouble _zscore) _zScoreCalculationApproach

-- | Parse the consenus of RNAz results         
parseRNAzConsensus :: GenParser Char st RNAzConsensus
parseRNAzConsensus = do
  string (">consensus")
  newline
  _consensusSequence <- parseNucleotideAlignmentEntry
  newline          
  _dotBracket <- many1 (oneOf "().,")
  many (try (char ' '))
  char '('
  many (try (char ' '))
  many1 (oneOf "-1234567890.")
  many (try (char ' '))
  char '='
  many (try (char ' '))
  many1 (oneOf "-1234567890.")
  many (try (char ' '))
  char '+'
  many (try (char ' '))
  many1 (noneOf ")")
  char ')'
  many (try (char ' '))
  newline
  eof   
  return $ RNAzConsensus _consensusSequence _dotBracket
         
-- | parse RNAz from input string
parseRNAz :: [Char] -> Either ParseError RNAz
parseRNAz input = parse genParseRNAz "parseRNAz" input

-- | parse from input filePath                      
readRNAz :: String -> IO (Either ParseError RNAz)                  
readRNAz filePath = do 
  parsedFile <- parseFromFile genParseRNAz filePath
  CE.evaluate parsedFile
