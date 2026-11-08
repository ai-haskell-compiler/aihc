module Text.Read.Lex
  ( Lexeme (..),
    expect,
    hsLex,
    numberToInteger,
    numberToFixed,
    numberToRational,
  )
where

import GHC.Base (Monad (..), String)
import GHC.Read.Lex (Lexeme (..), expectP, lexP, numberToFixed, numberToInteger, numberToRational)
import Text.ParserCombinators.ReadP (ReadP, gather, skipSpaces)
import Text.ParserCombinators.ReadPrec (minPrec, readPrec_to_P)

-- | Skip white space and read the given lexeme. Fail on a different lexeme.
expect :: Lexeme -> ReadP ()
expect lexeme = readPrec_to_P (expectP lexeme) minPrec

-- | Skip white space and read one Haskell lexeme. Give the text of the lexeme.
hsLex :: ReadP String
hsLex = do
  skipSpaces
  (text, _) <- gather (readPrec_to_P lexP minPrec)
  return text
