{- ORACLE_TEST pass -}
module FriendlyTimeAppendSecondsLambda where

secondsAgo = \f -> (++ " seconds" ++ dir f)
