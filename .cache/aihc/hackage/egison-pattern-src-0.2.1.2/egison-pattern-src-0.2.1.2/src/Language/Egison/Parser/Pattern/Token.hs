-- |
--
-- Module:      Language.Egison.Parser.Pattern.Token
-- Description: Tokens in expressions
-- Stability:   experimental
--
-- This module defines a set of tokens for token types.

module Language.Egison.Parser.Pattern.Token
  ( IsToken(..)
  )
where

import qualified Data.Char                     as Char
                                                ( isSpace )


-- | Provide a set of tokens needed to parse pattern expressions.
class IsToken c where
  isSpace :: c -> Bool
  newline :: c
  parenLeft :: c
  parenRight :: c
  underscore :: c
  hash :: c
  question :: c
  exclamation :: c
  and :: c
  vertical :: c
  dollar :: c
  bracketLeft :: c
  bracketRight :: c
  comma :: c

instance IsToken Char where
  isSpace      = Char.isSpace
  newline      = '\n'
  parenLeft    = '('
  parenRight   = ')'
  underscore   = '_'
  hash         = '#'
  question     = '?'
  exclamation  = '!'
  and          = '&'
  vertical     = '|'
  dollar       = '$'
  bracketLeft  = '['
  bracketRight = ']'
  comma        = ','
