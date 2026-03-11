module Text.HTML.Tagchup.Character where

import qualified Text.HTML.Basic.Character as HTMLChar


class C char where
   fromChar :: Char -> char

instance C Char where
   fromChar = id

instance C HTMLChar.T where
   fromChar = HTMLChar.Unicode
