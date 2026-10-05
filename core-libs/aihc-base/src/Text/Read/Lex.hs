module Text.Read.Lex
  ( Lexeme (..),
    expect,
    numberToInteger,
    numberToFixed,
    numberToRational,
  )
where

import GHC.Read.Lex (Lexeme (..), expectP, numberToFixed, numberToInteger, numberToRational)
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadPrec (minPrec, readPrec_to_P)

-- | Skip white space and read the given lexeme. Fail on a different lexeme.
expect :: Lexeme -> ReadP ()
expect lexeme = readPrec_to_P (expectP lexeme) minPrec
