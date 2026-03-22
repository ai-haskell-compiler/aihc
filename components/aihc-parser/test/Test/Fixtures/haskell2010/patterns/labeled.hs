module P8 where

data Pair = Pair {left :: Int, right :: Int}

leftValue Pair {left = x, right = _} = x

isPair Pair {} = True
