{- ORACLE_TEST
id: pat-labeled
category: patterns
expected: pass
-}
module P8 where

data Pair = Pair { left :: Int, right :: Int }

leftValue Pair { left = x, right = _ } = x
isPair Pair {} = True
