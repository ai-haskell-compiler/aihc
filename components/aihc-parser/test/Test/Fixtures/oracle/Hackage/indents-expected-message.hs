{- ORACLE_TEST pass -}
module IndentsExpectedMessage where

f ref pos =
  (<?> prettyIndentation ref ++ " (started at line " ++ prettyLine ref ++ ")")
    (unexpected pos)
