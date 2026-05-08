{- ORACLE_TEST xfail indents: parser adds parentheses around the concatenation expression for message in the parser expectation operator -}
module IndentsExpectedMessageXFail where

f ref pos =
  (<?> prettyIndentation ref ++ " (started at line " ++ prettyLine ref ++ ")")
    (unexpected pos)
