module Data.Bits.HT where

import Data.Bits (Bits, shiftL, shiftR)


infixl 7 .<<., .>>.

{- |
Infix variant of 'shiftL'.
Precedence is chosen like multiplication since @a .<<. k == a * 2^k@.
-}
(.<<.) :: Bits a => a -> Int -> a
(.<<.) = shiftL

{- |
Infix variant of 'shiftR'.
Precedence is chosen like division since @a .>>. k == a / 2^k@.
-}
(.>>.) :: Bits a => a -> Int -> a
(.>>.) = shiftR
