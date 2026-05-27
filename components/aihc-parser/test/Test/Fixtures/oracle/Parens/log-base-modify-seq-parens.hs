{- ORACLE_TEST pass -}

module MonoidAppendParen where

appendToRef ref msg' = ref (<> msg' <> "\n")
