{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}
module ParenSectionMultipleOps where

-- Multiple infix operators followed by trailing section operator
test1 = (a . b . c <$>)
test2 = (a + b -)
test3 = (a `f` b `g` c <*>)

-- Nested sections with infix
test4 = ((x + y) <$>)
test5 = ((a . b) <*>)

-- Complex real-world pattern
writePandocWith f g =
    (foo . f . g <$>)
    undefined
