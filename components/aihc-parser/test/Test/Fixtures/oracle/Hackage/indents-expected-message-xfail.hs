{- ORACLE_TEST pass -}
module IndentsExpectedMessageXFail where

f ref pos =
  (<?> prettyIndentation ref ++ " (started at line " ++ prettyLine ref ++ ")")
    (unexpected pos)
