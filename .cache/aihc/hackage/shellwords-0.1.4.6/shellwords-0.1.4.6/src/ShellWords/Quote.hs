{-# LANGUAGE NamedFieldPuns #-}

module ShellWords.Quote
  ( -- * Quoting for shells
    quote
  , join
  ) where

import Prelude

-- | How to escape a `String` for @sh(1)@.
data EscapeStyle
  = -- | No escaping.
    NoEscaping
  | -- | Wrapped in single quotes.
    SingleQuoted
  | -- | Wrapped in single quotes
    Mixed

-- | Internal state for `escapeStyle`.
data EscapeStyleState
  = EscapeStyleState
  { hasSpecial :: Bool
  , hasNewline :: Bool
  , hasSingleQuote :: Bool
  }

-- | Determine how to escape a `String`.
escapeStyle :: String -> EscapeStyle
escapeStyle "" = SingleQuoted
escapeStyle str =
  let
    isOtherSpecial :: Char -> Bool
    isOtherSpecial c = c `elem` ("|&;<>()$`\\\" \t*?[#~=%" :: String)

    helper :: EscapeStyleState -> String -> EscapeStyle
    helper state (c : s)
      | c == '\n' = helper state {hasNewline = True, hasSpecial = True} s
      | c == '\'' = helper state {hasSingleQuote = True, hasSpecial = True} s
      | isOtherSpecial c = helper state {hasSpecial = True} s
      | otherwise = helper state s
    helper EscapeStyleState {hasSpecial, hasNewline, hasSingleQuote} []
      | not hasSpecial = NoEscaping
      | hasNewline && not hasSingleQuote = SingleQuoted
      | otherwise = Mixed
  in
    helper
      EscapeStyleState
        { hasSpecial = False
        , hasNewline = False
        , hasSingleQuote = False
        }
      str

-- | Escape special characters in a string, so that it will retain its literal
-- meaning when used as a part of command in a Unix shell.
--
-- It tries to avoid introducing any unnecessary quotes or escape characters,
-- but specifics regarding quoting style are left unspecified.
quote :: String -> String
quote str =
  case escapeStyle str of
    NoEscaping -> str
    SingleQuoted -> "'" <> str <> "'"
    Mixed ->
      let
        quoteMixed :: String -> String
        quoteMixed [] = []
        quoteMixed ('\'' : s) = "'\\''" <> quoteMixed s
        quoteMixed (c : s) = c : quoteMixed s
      in
        "'" <> quoteMixed str <> "'"

-- | Joins arguments into a single command line suitable for execution in a Unix shell.
--
-- Each argument is quoted using `quote` to preserve its literal meaning when
-- parsed by Unix shell.
--
-- Note: This function is essentially an (infallible) inverse of `parse`.
join :: [String] -> String
join = unwords . map quote
