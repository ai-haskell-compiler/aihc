{- ORACLE_TEST xfail Unicode operator ‼ (U+203C DOUBLE EXCLAMATION MARK) not recognized as varsym -}
module UnicodeOperatorDoubleExclamation where

(‼) :: [a] -> Int -> a
(‼) = (!!)
