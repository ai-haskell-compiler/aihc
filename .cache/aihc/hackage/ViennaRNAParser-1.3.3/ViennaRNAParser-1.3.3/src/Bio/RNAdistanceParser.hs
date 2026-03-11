-- | Parse RNAdistance output
--   For more information on RNAdistance consult: <<http://www.tbi.univie.ac.at/RNA/RNAdistance.html>

module Bio.RNAdistanceParser (
                       parseRNAdistance,
                       readRNAdistance,
                       module Bio.RNAdistanceData
                      ) where

import Bio.RNAdistanceData
import Bio.ViennaRNAParserLibrary
import Text.ParserCombinators.Parsec     

-- | Parse the consenus of RNAz results         
genParserRNAdistance :: GenParser Char st RNAdistance
genParserRNAdistance = do
  oneOf "fhwcFHWCP"  
  string (": ")              
  distance <- many1 (noneOf " ")
  optional (many1 space)
  eof
  return $ RNAdistance (readInt distance)

-- | parse RNAdistance output from input string
parseRNAdistance :: [Char] -> Either ParseError RNAdistance
parseRNAdistance input = parse genParserRNAdistance "genParseRNAdistance" input

-- | parse RNAdistance output from input filePath                      
readRNAdistance :: String -> IO (Either ParseError RNAdistance)                  
readRNAdistance filePath = parseFromFile genParserRNAdistance filePath

