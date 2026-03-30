{- ORACLE_TEST
id: unicode-bind-arrow
category: do-blocks
expected: pass
-}
{-# LANGUAGE UnicodeSyntax #-}

module UnicodeSyntaxDo where

testDo ∷ IO ()
testDo = do
  x ← return 42
  y ← return 99
  return ()
