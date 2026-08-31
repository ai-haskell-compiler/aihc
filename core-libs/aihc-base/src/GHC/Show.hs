module GHC.Show
  ( Show (..),
    ShowS,
    showChar,
    showParen,
    shows,
    showString,
    appPrec,
    showMultiLineString,
  )
where

import Prelude (Int, Show (..), ShowS, String, showChar, showParen, showString, shows)

appPrec :: Int
appPrec = 10

showMultiLineString :: String -> [String]
showMultiLineString value = [show value]
