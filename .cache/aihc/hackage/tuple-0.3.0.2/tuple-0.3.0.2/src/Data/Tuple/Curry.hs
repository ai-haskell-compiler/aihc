{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Data.Tuple.Curry where
import Data.Tuple.OneTuple

-- | Tuple curry/uncurry.
class Curry a b | a -> b where
    curryN   :: a -> b
    uncurryN :: b -> a

instance Curry (OneTuple a -> b) (a -> b) where
    curryN f a = f (OneTuple a)
    uncurryN f ~(OneTuple a) = f a

--snip-----------------
---- Machine generated code below, see Tools/MkTuple.hs
---- mkTuple curry 32
instance Curry ((a1,a2) -> r) (a1->a2 -> r) where
    curryN f a1 a2 = f (a1,a2)
    uncurryN f ~(a1,a2) = f a1 a2
instance Curry ((a1,a2,a3) -> r) (a1->a2->a3 -> r) where
    curryN f a1 a2 a3 = f (a1,a2,a3)
    uncurryN f ~(a1,a2,a3) = f a1 a2 a3
instance Curry ((a1,a2,a3,a4) -> r) (a1->a2->a3->a4 -> r) where
    curryN f a1 a2 a3 a4 = f (a1,a2,a3,a4)
    uncurryN f ~(a1,a2,a3,a4) = f a1 a2 a3 a4
instance Curry ((a1,a2,a3,a4,a5) -> r) (a1->a2->a3->a4->a5 -> r) where
    curryN f a1 a2 a3 a4 a5 = f (a1,a2,a3,a4,a5)
    uncurryN f ~(a1,a2,a3,a4,a5) = f a1 a2 a3 a4 a5
instance Curry ((a1,a2,a3,a4,a5,a6) -> r) (a1->a2->a3->a4->a5->a6 -> r) where
    curryN f a1 a2 a3 a4 a5 a6 = f (a1,a2,a3,a4,a5,a6)
    uncurryN f ~(a1,a2,a3,a4,a5,a6) = f a1 a2 a3 a4 a5 a6
instance Curry ((a1,a2,a3,a4,a5,a6,a7) -> r) (a1->a2->a3->a4->a5->a6->a7 -> r) where
    curryN f a1 a2 a3 a4 a5 a6 a7 = f (a1,a2,a3,a4,a5,a6,a7)
    uncurryN f ~(a1,a2,a3,a4,a5,a6,a7) = f a1 a2 a3 a4 a5 a6 a7
instance Curry ((a1,a2,a3,a4,a5,a6,a7,a8) -> r) (a1->a2->a3->a4->a5->a6->a7->a8 -> r) where
    curryN f a1 a2 a3 a4 a5 a6 a7 a8 = f (a1,a2,a3,a4,a5,a6,a7,a8)
    uncurryN f ~(a1,a2,a3,a4,a5,a6,a7,a8) = f a1 a2 a3 a4 a5 a6 a7 a8
instance Curry ((a1,a2,a3,a4,a5,a6,a7,a8,a9) -> r) (a1->a2->a3->a4->a5->a6->a7->a8->a9 -> r) where
    curryN f a1 a2 a3 a4 a5 a6 a7 a8 a9 = f (a1,a2,a3,a4,a5,a6,a7,a8,a9)
    uncurryN f ~(a1,a2,a3,a4,a5,a6,a7,a8,a9) = f a1 a2 a3 a4 a5 a6 a7 a8 a9
instance Curry ((a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) -> r) (a1->a2->a3->a4->a5->a6->a7->a8->a9->a10 -> r) where
    curryN f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = f (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
    uncurryN f ~(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) = f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10
instance Curry ((a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) -> r) (a1->a2->a3->a4->a5->a6->a7->a8->a9->a10->a11 -> r) where
    curryN f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 = f (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11)
    uncurryN f ~(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) = f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11
instance Curry ((a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) -> r) (a1->a2->a3->a4->a5->a6->a7->a8->a9->a10->a11->a12 -> r) where
    curryN f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 = f (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12)
    uncurryN f ~(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) = f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12
instance Curry ((a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) -> r) (a1->a2->a3->a4->a5->a6->a7->a8->a9->a10->a11->a12->a13 -> r) where
    curryN f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 = f (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13)
    uncurryN f ~(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) = f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13
instance Curry ((a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) -> r) (a1->a2->a3->a4->a5->a6->a7->a8->a9->a10->a11->a12->a13->a14 -> r) where
    curryN f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 = f (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14)
    uncurryN f ~(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) = f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14
instance Curry ((a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) -> r) (a1->a2->a3->a4->a5->a6->a7->a8->a9->a10->a11->a12->a13->a14->a15 -> r) where
    curryN f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 = f (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15)
    uncurryN f ~(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) = f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15
instance Curry ((a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) -> r) (a1->a2->a3->a4->a5->a6->a7->a8->a9->a10->a11->a12->a13->a14->a15->a16 -> r) where
    curryN f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 = f (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16)
    uncurryN f ~(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) = f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16
instance Curry ((a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) -> r) (a1->a2->a3->a4->a5->a6->a7->a8->a9->a10->a11->a12->a13->a14->a15->a16->a17 -> r) where
    curryN f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 = f (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17)
    uncurryN f ~(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) = f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17
instance Curry ((a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) -> r) (a1->a2->a3->a4->a5->a6->a7->a8->a9->a10->a11->a12->a13->a14->a15->a16->a17->a18 -> r) where
    curryN f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 = f (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18)
    uncurryN f ~(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) = f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18
instance Curry ((a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) -> r) (a1->a2->a3->a4->a5->a6->a7->a8->a9->a10->a11->a12->a13->a14->a15->a16->a17->a18->a19 -> r) where
    curryN f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 = f (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19)
    uncurryN f ~(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) = f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19
instance Curry ((a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) -> r) (a1->a2->a3->a4->a5->a6->a7->a8->a9->a10->a11->a12->a13->a14->a15->a16->a17->a18->a19->a20 -> r) where
    curryN f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 = f (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20)
    uncurryN f ~(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) = f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20
instance Curry ((a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) -> r) (a1->a2->a3->a4->a5->a6->a7->a8->a9->a10->a11->a12->a13->a14->a15->a16->a17->a18->a19->a20->a21 -> r) where
    curryN f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 = f (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21)
    uncurryN f ~(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) = f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21
instance Curry ((a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) -> r) (a1->a2->a3->a4->a5->a6->a7->a8->a9->a10->a11->a12->a13->a14->a15->a16->a17->a18->a19->a20->a21->a22 -> r) where
    curryN f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 = f (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22)
    uncurryN f ~(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) = f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22
instance Curry ((a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23) -> r) (a1->a2->a3->a4->a5->a6->a7->a8->a9->a10->a11->a12->a13->a14->a15->a16->a17->a18->a19->a20->a21->a22->a23 -> r) where
    curryN f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 = f (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23)
    uncurryN f ~(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23) = f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23
instance Curry ((a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24) -> r) (a1->a2->a3->a4->a5->a6->a7->a8->a9->a10->a11->a12->a13->a14->a15->a16->a17->a18->a19->a20->a21->a22->a23->a24 -> r) where
    curryN f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 = f (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24)
    uncurryN f ~(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24) = f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24
instance Curry ((a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25) -> r) (a1->a2->a3->a4->a5->a6->a7->a8->a9->a10->a11->a12->a13->a14->a15->a16->a17->a18->a19->a20->a21->a22->a23->a24->a25 -> r) where
    curryN f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 = f (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25)
    uncurryN f ~(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25) = f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25
instance Curry ((a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) -> r) (a1->a2->a3->a4->a5->a6->a7->a8->a9->a10->a11->a12->a13->a14->a15->a16->a17->a18->a19->a20->a21->a22->a23->a24->a25->a26 -> r) where
    curryN f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 = f (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26)
    uncurryN f ~(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) = f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26
instance Curry ((a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) -> r) (a1->a2->a3->a4->a5->a6->a7->a8->a9->a10->a11->a12->a13->a14->a15->a16->a17->a18->a19->a20->a21->a22->a23->a24->a25->a26->a27 -> r) where
    curryN f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 = f (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27)
    uncurryN f ~(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) = f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27
instance Curry ((a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) -> r) (a1->a2->a3->a4->a5->a6->a7->a8->a9->a10->a11->a12->a13->a14->a15->a16->a17->a18->a19->a20->a21->a22->a23->a24->a25->a26->a27->a28 -> r) where
    curryN f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 = f (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28)
    uncurryN f ~(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) = f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28
instance Curry ((a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) -> r) (a1->a2->a3->a4->a5->a6->a7->a8->a9->a10->a11->a12->a13->a14->a15->a16->a17->a18->a19->a20->a21->a22->a23->a24->a25->a26->a27->a28->a29 -> r) where
    curryN f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 = f (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29)
    uncurryN f ~(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) = f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29
instance Curry ((a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) -> r) (a1->a2->a3->a4->a5->a6->a7->a8->a9->a10->a11->a12->a13->a14->a15->a16->a17->a18->a19->a20->a21->a22->a23->a24->a25->a26->a27->a28->a29->a30 -> r) where
    curryN f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 = f (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30)
    uncurryN f ~(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) = f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30
instance Curry ((a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) -> r) (a1->a2->a3->a4->a5->a6->a7->a8->a9->a10->a11->a12->a13->a14->a15->a16->a17->a18->a19->a20->a21->a22->a23->a24->a25->a26->a27->a28->a29->a30->a31 -> r) where
    curryN f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 = f (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31)
    uncurryN f ~(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) = f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31
instance Curry ((a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) -> r) (a1->a2->a3->a4->a5->a6->a7->a8->a9->a10->a11->a12->a13->a14->a15->a16->a17->a18->a19->a20->a21->a22->a23->a24->a25->a26->a27->a28->a29->a30->a31->a32 -> r) where
    curryN f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 = f (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32)
    uncurryN f ~(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) = f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32
