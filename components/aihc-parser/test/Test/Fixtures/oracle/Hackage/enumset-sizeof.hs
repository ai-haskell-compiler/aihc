{- ORACLE_TEST pass -}
module EnumsetTakeWhileSizeOf where

f x = takeWhile (< sizeOf x * 8)
