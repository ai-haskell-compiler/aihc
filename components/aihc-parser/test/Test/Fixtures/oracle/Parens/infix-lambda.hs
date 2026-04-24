{- ORACLE_TEST pass -}
module M where

warnIfNullable r = when True $ P $ \s ->
  Right (s {warnings = WarnNullableRExp pos w : warnings s}, ())
