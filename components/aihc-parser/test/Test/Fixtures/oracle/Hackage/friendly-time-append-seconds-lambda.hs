{- ORACLE_TEST xfail Guard roundtrip due to parenthesized section on string concat rhs -}
module FriendlyTimeAppendSecondsLambda where

secondsAgo = \f -> (++ " seconds" ++ dir f)

