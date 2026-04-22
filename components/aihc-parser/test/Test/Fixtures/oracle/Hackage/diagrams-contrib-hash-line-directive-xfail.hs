{- ORACLE_TEST xfail lexer treats # at start of continuation line followed by 'line' as a CPP directive -}
module A where
r = x
  # line
