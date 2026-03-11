{-# LANGUAGE TemplateHaskell #-}

module Text.Scanf.TH where

import Data.Char (isSpace)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

import Text.Scanf.Internal

formatString :: String -> Q Exp
formatString "" = [|Empty|]
formatString ('%' : s1) =
  case s1 of
    'c' : s' -> [|char $(formatString s')|]
    'd' : s' -> [|int $(formatString s')|]
    'f' : s' -> [|double $(formatString s')|]
    'l' : s' -> [|integer $(formatString s')|]
    's' : s' -> [|string $(formatString s')|]
    '%' : s' -> [|constant "%" $(formatString s')|]
    _ -> error "Invalid format string"
formatString s@(c : _) | isSpace c =
  let (s0, s') = span isSpace s
  in [|whitespace $(lift s0) $(formatString s')|]
formatString s =
  let (s0, s') = break (\c -> isSpace c || c == '%') s
  in [|constant $(lift s0) $(formatString s')|]

-- | Parse a typed 'Format' string.
--
-- The following conversion strings are supported:
--
-- - @%d@: signed integer ('Int')
-- - @%l@: signed integer ('Integer', unbounded)
-- - @%f@: floating point ('Double')
-- - @%s@: string of non-space characters ('String')
-- - @%c@: single character ('Char')
-- - @%%@: parse/print a literal percent character
--
-- N.B.: in 'scanf', spaces in the format string match any number of whitespace
-- character until the next nonspace character.
--
-- @
-- ['fmt'|%d lazy %s and %d strict %s|]
--   :: 'Format' ('Int' ':+' 'String' ':+' 'Int' ':+' 'String' ':+' ())
-- @
fmt :: QuasiQuoter
fmt = QuasiQuoter
  { quoteExp = formatString
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }
