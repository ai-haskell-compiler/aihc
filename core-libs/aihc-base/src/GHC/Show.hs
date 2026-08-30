module GHC.Show
  ( Show (..),
    ShowS,
    showChar,
    showParen,
    shows,
    showString,
    appPrec,
  )
where

import Prelude (Show (..), ShowS, showChar, showParen, showString, shows)

appPrec :: Int
appPrec = 10
