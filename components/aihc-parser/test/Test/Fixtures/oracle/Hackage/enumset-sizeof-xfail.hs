{- ORACLE_TEST pass -}
module EnumsetTakeWhileSizeOfXFail where

f x = takeWhile (< sizeOf x * 8)
