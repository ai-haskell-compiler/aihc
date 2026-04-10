{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}

module WildcardInfixPatterns where

-- Both sides wildcards
_ #|# _ = True

-- Left wildcard, right variable
_ #|# y = y

-- Left variable, right wildcard  
x #|# _ = x

-- Both variables (already worked)
a #|# b = a

-- With constructor patterns
_ #|#: _ = []
x #|#: xs = x : xs

-- With nested patterns
_ #||# (Just _) = True
(Just _) #||# _ = True
