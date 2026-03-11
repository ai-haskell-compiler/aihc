{-# LANGUAGE Rank2Types #-}
module Combinatorics.Battleship.Size where


data Zero = Zero
data Succ n = Succ n

type N0  = Zero
type N1  = Succ N0  ; type P1 w  = Succ w
type N2  = Succ N1  ; type P2 w  = Succ (P1  w)
type N3  = Succ N2  ; type P3 w  = Succ (P2  w)
type N4  = Succ N3  ; type P4 w  = Succ (P3  w)
type N5  = Succ N4  ; type P5 w  = Succ (P4  w)
type N6  = Succ N5  ; type P6 w  = Succ (P5  w)
type N7  = Succ N6  ; type P7 w  = Succ (P6  w)
type N8  = Succ N7  ; type P8 w  = Succ (P7  w)
type N9  = Succ N8  ; type P9 w  = Succ (P8  w)
type N10 = Succ N9  ; type P10 w = Succ (P9  w)
type N11 = Succ N10 ; type P11 w = Succ (P10 w)
type N12 = Succ N11 ; type P12 w = Succ (P11 w)

n0 :: Size N0; n0 = size
n1 :: Size N1; n1 = size
n2 :: Size N2; n2 = size
n3 :: Size N3; n3 = size
n4 :: Size N4; n4 = size
n5 :: Size N5; n5 = size
n6 :: Size N6; n6 = size
n7 :: Size N7; n7 = size
n8 :: Size N8; n8 = size
n9 :: Size N9; n9 = size
n10 :: Size N10; n10 = size


newtype Size n = Size {getSize :: Int}

incSize :: Size n -> Size (Succ n)
incSize (Size n) = Size (n+1)

class Nat n where switch :: f Zero -> (forall m. Nat m => f (Succ m)) -> f n
instance Nat Zero where switch f _ = f
instance Nat n => Nat (Succ n) where switch _ f = f

size :: Nat n => Size n
size = switch (Size 0) (incSize size)


reifyInt :: Int -> (forall n. Nat n => Size n -> a) -> a
reifyInt n f =
   if n==0
     then f n0
     else reifyInt (n-1) $ f . incSize
