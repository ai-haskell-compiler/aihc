module Sound.MED.Basic.Human where

class Human a where
  human :: a -> String


bold :: String -> String
bold s = bold_on ++ s ++ bold_off

bold_on, bold_off :: String
bold_on  = "\ESC[1m"
bold_off = "\ESC[22m"
