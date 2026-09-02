module GHC.Show
  ( Show (..),
    ShowS,
    showChar,
    showParen,
    shows,
    showString,
    appPrec,
    showMultiLineString,
    intToDigit,
  )
where

import GHC.Char (chr)
import Prelude (Bool (..), Char, Int, Num (..), Ord (..), Show (..), ShowS, String, errorWithoutStackTrace, showChar, showParen, showString, shows, (&&))

appPrec :: Int
appPrec = 10

showMultiLineString :: String -> [String]
showMultiLineString value = [show value]

intToDigit :: Int -> Char
intToDigit digit =
  case (0 <= digit && digit < 10, 10 <= digit && digit < 16) of
    (True, _) -> chr (48 + digit)
    (_, True) -> chr (87 + digit)
    _ -> errorWithoutStackTrace "Char.intToDigit: not a digit"
