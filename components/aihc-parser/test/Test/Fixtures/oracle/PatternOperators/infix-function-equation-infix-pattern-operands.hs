{- ORACLE_TEST pass -}
module InfixFunctionEquationInfixPatternOperands where

data List a = Nil | a :&: List a

a :&: as == b :&: bs = ()
