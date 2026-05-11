{- ORACLE_TEST pass -}
module M where

x (_ :+ -0) = 0
x (-0 :+ _) = 0

_ + -0 = 0
-0 + _ = 0
