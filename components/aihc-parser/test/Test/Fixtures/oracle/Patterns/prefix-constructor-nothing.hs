{- ORACLE_TEST xfail emojis prefix constructor patterns roundtrip with Nothing arguments -}
module PrefixConstructorNothing where

data T = T (Maybe Int) (Maybe Int)

f (T Nothing Nothing) = ()
f _ = ()
