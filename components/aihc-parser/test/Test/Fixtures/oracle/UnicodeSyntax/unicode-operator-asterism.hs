{- ORACLE_TEST xfail Unicode operator ⁂ (U+2042 ASTERISM) not recognized as varsym -}
module UnicodeOperatorAsterism where

(⁂) :: Int -> Int -> Int
(⁂) = (+)
