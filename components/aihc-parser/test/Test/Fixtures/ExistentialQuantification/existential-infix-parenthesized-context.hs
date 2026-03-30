{- ORACLE_TEST
id: existential-infix-parenthesized-context
category: declarations
expected: pass
-}
{-# LANGUAGE ExistentialQuantification #-}

module ExistentialInfixParenthesizedContext where

data A = forall yk. ((BgpBGa whd1 UdHfta), (F)) => a `A` A

useA :: A -> String
useA _ = "ok"
