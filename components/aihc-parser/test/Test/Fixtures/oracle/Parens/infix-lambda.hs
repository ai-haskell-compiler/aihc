{- ORACLE_TEST xfail "we add parens around lambdas when we shouldn't" -}
module M where

warnIfNullable r = when True $ P $ \s ->
  Right (s {warnings = WarnNullableRExp pos w : warnings s}, ())
