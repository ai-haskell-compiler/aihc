{- ORACLE_TEST xfail parser rejects type annotation in if-then branch expression -}
module SlackWebTypeAnnotationInIfThenElse where

f = if x then y :: T else z
