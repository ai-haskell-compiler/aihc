module Text.Read
  ( Read (..),
    ReadS,
    read,
    reads,
    Lexeme (..),
    lexP,
    parens,
    readListDefault,
    readListPrecDefault,
  )
where

import GHC.Read
  ( Read (..),
    ReadS,
    lexP,
    parens,
    readListDefault,
    readListPrecDefault,
  )
import GHC.Read.Lex (Lexeme (..))
import Prelude (read, reads)
