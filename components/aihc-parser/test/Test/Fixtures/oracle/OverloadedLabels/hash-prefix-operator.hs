{- ORACLE_TEST pass -}
{-# LANGUAGE OverloadedLabels #-}
module HashPrefixOperator where

-- '#' followed by a symbol character should be parsed as a variable operator,
-- not as an overloaded label. GHC accepts this even with OverloadedLabels enabled.
(#⥹) = (+)

x = 1 #⥹ 2
