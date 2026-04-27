{- ORACLE_TEST pass -}
module Sound.ALSA.Exception where

class Exception a

data T = T

instance Exception T where

checkResult :: Integral a => String -> a -> IO a
checkResult = undefined
