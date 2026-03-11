-- |
--
-- Module:      Language.Egison.Parser.Pattern.Prim.ParseMode
-- Description: Parser configuration
-- Stability:   experimental
--
-- A parser configuration type, that contains a set of external parsers

module Language.Egison.Parser.Pattern.Prim.ParseMode
  ( ParseMode(..)
  , ParseFixity(..)
  , ExtParser
  )
where

import           Language.Egison.Syntax.Pattern.Fixity
                                                ( Fixity )
import           Language.Egison.Parser.Pattern.Prim.Source
                                                ( Tokens )


-- | @'ExtParser' s a@ is a type for externally provided parser of @a@.
type ExtParser s a = Tokens s -> Either String a

-- | Fixity of infix operators.
data ParseFixity n s =
  ParseFixity { fixity :: Fixity n
              , parser :: ExtParser s ()
              }

-- | Parser configuration.
data ParseMode n v e s
  = ParseMode { filename        :: FilePath
              , fixities        :: [ParseFixity n s]
              , blockComment    :: Maybe (Tokens s, Tokens s)
              , lineComment     :: Maybe (Tokens s)
              , varNameParser   :: ExtParser s v
              , nameParser      :: ExtParser s n
              , valueExprParser :: ExtParser s e
              }
