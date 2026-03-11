-- |
--
-- Module:      Language.Egison.Parser.Pattern.Combinator
-- Description: Useful combinators to build lexeme parsers
-- Stability:   experimental
--
-- A helper module to build lexeme parsers.
--
-- Note that functions exported from this module produce lexeme parsers (i.e. skip spaces after that),
-- while parsers from 'Language.Egison.Pattern.Parser.Prim' do not.

module Language.Egison.Parser.Pattern.Combinator
  ( token
  , symbol
  , parens
  )
where

import           Control.Monad.Combinators      ( between )
import           Data.Functor                   ( void )

import           Language.Egison.Parser.Pattern.Prim
                                                ( Parse
                                                , Source
                                                , Token
                                                , Tokens
                                                , lexeme
                                                , single
                                                , chunk
                                                )
import qualified Language.Egison.Parser.Pattern.Token
                                               as Token
                                                ( parenLeft
                                                , parenRight
                                                )


-- | Parser for a single token.
token :: Source s => Token s -> Parse n v e s ()
token = void . lexeme . single

-- | Parser for a chunk of tokens.
symbol :: Source s => Tokens s -> Parse n v e s ()
symbol = void . lexeme . chunk

-- | Wrap a parser with parentheses.
parens :: Source s => Parse n v e s a -> Parse n v e s a
parens = between (token Token.parenLeft) (token Token.parenRight)
