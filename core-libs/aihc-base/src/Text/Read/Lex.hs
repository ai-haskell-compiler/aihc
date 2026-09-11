module Text.Read.Lex
  ( Lexeme (..),
    numberToInteger,
    numberToFixed,
    numberToRational,
  )
where

import GHC.Read.Lex (Lexeme (..), numberToFixed, numberToInteger, numberToRational)
