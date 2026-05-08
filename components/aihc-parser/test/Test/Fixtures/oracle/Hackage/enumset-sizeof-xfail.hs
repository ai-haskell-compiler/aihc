{- ORACLE_TEST xfail enumset: parser requires parenthesized right side of `< sizeOf x * 8` expression -}
module EnumsetTakeWhileSizeOfXFail where

f x = takeWhile (< sizeOf x * 8)
