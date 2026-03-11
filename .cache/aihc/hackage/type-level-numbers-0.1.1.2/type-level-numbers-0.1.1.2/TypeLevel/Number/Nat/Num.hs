{-# LANGUAGE TemplateHaskell #-}
module TypeLevel.Number.Nat.Num where

import TypeLevel.Number.Nat

-- type N0 = $(natT 0)
-- type N1 = $(natT 1)
-- type N2 = $(natT 2)
-- type N3 = $(natT 3)
-- type N4 = $(natT 4)
-- type N5 = $(natT 5)
-- type N6 = $(natT 6)
-- type N7 = $(natT 7)
-- type N8 = $(natT 8)
-- type N9 = $(natT 9)

-- Workaround for GHC bug #4364
--   http://hackage.haskell.org/trac/ghc/ticket/4364
type N0 =            Z
type N1 =          I Z
type N2 =       O (I Z)
type N3 =       I (I Z)
type N4 =    O (O (I Z))
type N5 =    I (O (I Z))
type N6 =    O (I (I Z))
type N7 =    I (I (I Z))
type N8 = O (O (O (I Z)))
type N9 = I (O (O (I Z)))

n0 :: N0; n0 = undefined
n1 :: N1; n1 = undefined
n2 :: N2; n2 = undefined
n3 :: N3; n3 = undefined
n4 :: N4; n4 = undefined
n5 :: N5; n5 = undefined
n6 :: N6; n6 = undefined
n7 :: N7; n7 = undefined
n8 :: N8; n8 = undefined
n9 :: N9; n9 = undefined
