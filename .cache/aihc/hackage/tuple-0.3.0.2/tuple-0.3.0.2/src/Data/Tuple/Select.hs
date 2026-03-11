{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
-- This module contains overloaded functions for selecting an element from a tuple.
-- The /selN/ function selects element /N/ from any supported tuple size.
module Data.Tuple.Select where
import Data.Tuple.OneTuple

instance Sel1 (OneTuple a) a where sel1 (OneTuple x) = x

--snip-----------------
---- Machine generated code below, see Tools/MkTuple.hs
---- mkTuple select 32
class Sel1 a b | a -> b where sel1 :: a -> b
instance Sel1 (a1,a2) a1 where sel1 (x,_) = x
instance Sel1 (a1,a2,a3) a1 where sel1 (x,_,_) = x
instance Sel1 (a1,a2,a3,a4) a1 where sel1 (x,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5) a1 where sel1 (x,_,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5,a6) a1 where sel1 (x,_,_,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5,a6,a7) a1 where sel1 (x,_,_,_,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5,a6,a7,a8) a1 where sel1 (x,_,_,_,_,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5,a6,a7,a8,a9) a1 where sel1 (x,_,_,_,_,_,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) a1 where sel1 (x,_,_,_,_,_,_,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) a1 where sel1 (x,_,_,_,_,_,_,_,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) a1 where sel1 (x,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) a1 where sel1 (x,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) a1 where sel1 (x,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) a1 where sel1 (x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) a1 where sel1 (x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) a1 where sel1 (x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) a1 where sel1 (x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) a1 where sel1 (x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) a1 where sel1 (x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) a1 where sel1 (x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) a1 where sel1 (x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23) a1 where sel1 (x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24) a1 where sel1 (x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25) a1 where sel1 (x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) a1 where sel1 (x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) a1 where sel1 (x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a1 where sel1 (x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a1 where sel1 (x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a1 where sel1 (x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a1 where sel1 (x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel1 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a1 where sel1 (x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x

class Sel2 a b | a -> b where sel2 :: a -> b
instance Sel2 (a1,a2) a2 where sel2 (_,x) = x
instance Sel2 (a1,a2,a3) a2 where sel2 (_,x,_) = x
instance Sel2 (a1,a2,a3,a4) a2 where sel2 (_,x,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5) a2 where sel2 (_,x,_,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5,a6) a2 where sel2 (_,x,_,_,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5,a6,a7) a2 where sel2 (_,x,_,_,_,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5,a6,a7,a8) a2 where sel2 (_,x,_,_,_,_,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5,a6,a7,a8,a9) a2 where sel2 (_,x,_,_,_,_,_,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) a2 where sel2 (_,x,_,_,_,_,_,_,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) a2 where sel2 (_,x,_,_,_,_,_,_,_,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) a2 where sel2 (_,x,_,_,_,_,_,_,_,_,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) a2 where sel2 (_,x,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) a2 where sel2 (_,x,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) a2 where sel2 (_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) a2 where sel2 (_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) a2 where sel2 (_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) a2 where sel2 (_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) a2 where sel2 (_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) a2 where sel2 (_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) a2 where sel2 (_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) a2 where sel2 (_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23) a2 where sel2 (_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24) a2 where sel2 (_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25) a2 where sel2 (_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) a2 where sel2 (_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) a2 where sel2 (_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a2 where sel2 (_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a2 where sel2 (_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a2 where sel2 (_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a2 where sel2 (_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel2 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a2 where sel2 (_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x

class Sel3 a b | a -> b where sel3 :: a -> b
instance Sel3 (a1,a2,a3) a3 where sel3 (_,_,x) = x
instance Sel3 (a1,a2,a3,a4) a3 where sel3 (_,_,x,_) = x
instance Sel3 (a1,a2,a3,a4,a5) a3 where sel3 (_,_,x,_,_) = x
instance Sel3 (a1,a2,a3,a4,a5,a6) a3 where sel3 (_,_,x,_,_,_) = x
instance Sel3 (a1,a2,a3,a4,a5,a6,a7) a3 where sel3 (_,_,x,_,_,_,_) = x
instance Sel3 (a1,a2,a3,a4,a5,a6,a7,a8) a3 where sel3 (_,_,x,_,_,_,_,_) = x
instance Sel3 (a1,a2,a3,a4,a5,a6,a7,a8,a9) a3 where sel3 (_,_,x,_,_,_,_,_,_) = x
instance Sel3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) a3 where sel3 (_,_,x,_,_,_,_,_,_,_) = x
instance Sel3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) a3 where sel3 (_,_,x,_,_,_,_,_,_,_,_) = x
instance Sel3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) a3 where sel3 (_,_,x,_,_,_,_,_,_,_,_,_) = x
instance Sel3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) a3 where sel3 (_,_,x,_,_,_,_,_,_,_,_,_,_) = x
instance Sel3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) a3 where sel3 (_,_,x,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) a3 where sel3 (_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) a3 where sel3 (_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) a3 where sel3 (_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) a3 where sel3 (_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) a3 where sel3 (_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) a3 where sel3 (_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) a3 where sel3 (_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) a3 where sel3 (_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23) a3 where sel3 (_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24) a3 where sel3 (_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25) a3 where sel3 (_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) a3 where sel3 (_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) a3 where sel3 (_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a3 where sel3 (_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a3 where sel3 (_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a3 where sel3 (_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a3 where sel3 (_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel3 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a3 where sel3 (_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x

class Sel4 a b | a -> b where sel4 :: a -> b
instance Sel4 (a1,a2,a3,a4) a4 where sel4 (_,_,_,x) = x
instance Sel4 (a1,a2,a3,a4,a5) a4 where sel4 (_,_,_,x,_) = x
instance Sel4 (a1,a2,a3,a4,a5,a6) a4 where sel4 (_,_,_,x,_,_) = x
instance Sel4 (a1,a2,a3,a4,a5,a6,a7) a4 where sel4 (_,_,_,x,_,_,_) = x
instance Sel4 (a1,a2,a3,a4,a5,a6,a7,a8) a4 where sel4 (_,_,_,x,_,_,_,_) = x
instance Sel4 (a1,a2,a3,a4,a5,a6,a7,a8,a9) a4 where sel4 (_,_,_,x,_,_,_,_,_) = x
instance Sel4 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) a4 where sel4 (_,_,_,x,_,_,_,_,_,_) = x
instance Sel4 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) a4 where sel4 (_,_,_,x,_,_,_,_,_,_,_) = x
instance Sel4 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) a4 where sel4 (_,_,_,x,_,_,_,_,_,_,_,_) = x
instance Sel4 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) a4 where sel4 (_,_,_,x,_,_,_,_,_,_,_,_,_) = x
instance Sel4 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) a4 where sel4 (_,_,_,x,_,_,_,_,_,_,_,_,_,_) = x
instance Sel4 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) a4 where sel4 (_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel4 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) a4 where sel4 (_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel4 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) a4 where sel4 (_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel4 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) a4 where sel4 (_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel4 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) a4 where sel4 (_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel4 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) a4 where sel4 (_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel4 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) a4 where sel4 (_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel4 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) a4 where sel4 (_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel4 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23) a4 where sel4 (_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel4 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24) a4 where sel4 (_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel4 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25) a4 where sel4 (_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel4 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) a4 where sel4 (_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel4 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) a4 where sel4 (_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel4 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a4 where sel4 (_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel4 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a4 where sel4 (_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel4 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a4 where sel4 (_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel4 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a4 where sel4 (_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel4 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a4 where sel4 (_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x

class Sel5 a b | a -> b where sel5 :: a -> b
instance Sel5 (a1,a2,a3,a4,a5) a5 where sel5 (_,_,_,_,x) = x
instance Sel5 (a1,a2,a3,a4,a5,a6) a5 where sel5 (_,_,_,_,x,_) = x
instance Sel5 (a1,a2,a3,a4,a5,a6,a7) a5 where sel5 (_,_,_,_,x,_,_) = x
instance Sel5 (a1,a2,a3,a4,a5,a6,a7,a8) a5 where sel5 (_,_,_,_,x,_,_,_) = x
instance Sel5 (a1,a2,a3,a4,a5,a6,a7,a8,a9) a5 where sel5 (_,_,_,_,x,_,_,_,_) = x
instance Sel5 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) a5 where sel5 (_,_,_,_,x,_,_,_,_,_) = x
instance Sel5 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) a5 where sel5 (_,_,_,_,x,_,_,_,_,_,_) = x
instance Sel5 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) a5 where sel5 (_,_,_,_,x,_,_,_,_,_,_,_) = x
instance Sel5 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) a5 where sel5 (_,_,_,_,x,_,_,_,_,_,_,_,_) = x
instance Sel5 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) a5 where sel5 (_,_,_,_,x,_,_,_,_,_,_,_,_,_) = x
instance Sel5 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) a5 where sel5 (_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) = x
instance Sel5 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) a5 where sel5 (_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel5 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) a5 where sel5 (_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel5 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) a5 where sel5 (_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel5 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) a5 where sel5 (_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel5 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) a5 where sel5 (_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel5 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) a5 where sel5 (_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel5 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) a5 where sel5 (_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel5 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23) a5 where sel5 (_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel5 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24) a5 where sel5 (_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel5 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25) a5 where sel5 (_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel5 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) a5 where sel5 (_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel5 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) a5 where sel5 (_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel5 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a5 where sel5 (_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel5 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a5 where sel5 (_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel5 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a5 where sel5 (_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel5 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a5 where sel5 (_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel5 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a5 where sel5 (_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x

class Sel6 a b | a -> b where sel6 :: a -> b
instance Sel6 (a1,a2,a3,a4,a5,a6) a6 where sel6 (_,_,_,_,_,x) = x
instance Sel6 (a1,a2,a3,a4,a5,a6,a7) a6 where sel6 (_,_,_,_,_,x,_) = x
instance Sel6 (a1,a2,a3,a4,a5,a6,a7,a8) a6 where sel6 (_,_,_,_,_,x,_,_) = x
instance Sel6 (a1,a2,a3,a4,a5,a6,a7,a8,a9) a6 where sel6 (_,_,_,_,_,x,_,_,_) = x
instance Sel6 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) a6 where sel6 (_,_,_,_,_,x,_,_,_,_) = x
instance Sel6 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) a6 where sel6 (_,_,_,_,_,x,_,_,_,_,_) = x
instance Sel6 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) a6 where sel6 (_,_,_,_,_,x,_,_,_,_,_,_) = x
instance Sel6 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) a6 where sel6 (_,_,_,_,_,x,_,_,_,_,_,_,_) = x
instance Sel6 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) a6 where sel6 (_,_,_,_,_,x,_,_,_,_,_,_,_,_) = x
instance Sel6 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) a6 where sel6 (_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) = x
instance Sel6 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) a6 where sel6 (_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) = x
instance Sel6 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) a6 where sel6 (_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel6 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) a6 where sel6 (_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel6 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) a6 where sel6 (_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel6 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) a6 where sel6 (_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel6 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) a6 where sel6 (_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel6 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) a6 where sel6 (_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel6 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23) a6 where sel6 (_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel6 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24) a6 where sel6 (_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel6 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25) a6 where sel6 (_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel6 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) a6 where sel6 (_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel6 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) a6 where sel6 (_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel6 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a6 where sel6 (_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel6 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a6 where sel6 (_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel6 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a6 where sel6 (_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel6 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a6 where sel6 (_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel6 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a6 where sel6 (_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x

class Sel7 a b | a -> b where sel7 :: a -> b
instance Sel7 (a1,a2,a3,a4,a5,a6,a7) a7 where sel7 (_,_,_,_,_,_,x) = x
instance Sel7 (a1,a2,a3,a4,a5,a6,a7,a8) a7 where sel7 (_,_,_,_,_,_,x,_) = x
instance Sel7 (a1,a2,a3,a4,a5,a6,a7,a8,a9) a7 where sel7 (_,_,_,_,_,_,x,_,_) = x
instance Sel7 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) a7 where sel7 (_,_,_,_,_,_,x,_,_,_) = x
instance Sel7 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) a7 where sel7 (_,_,_,_,_,_,x,_,_,_,_) = x
instance Sel7 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) a7 where sel7 (_,_,_,_,_,_,x,_,_,_,_,_) = x
instance Sel7 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) a7 where sel7 (_,_,_,_,_,_,x,_,_,_,_,_,_) = x
instance Sel7 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) a7 where sel7 (_,_,_,_,_,_,x,_,_,_,_,_,_,_) = x
instance Sel7 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) a7 where sel7 (_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) = x
instance Sel7 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) a7 where sel7 (_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) = x
instance Sel7 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) a7 where sel7 (_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) = x
instance Sel7 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) a7 where sel7 (_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel7 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) a7 where sel7 (_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel7 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) a7 where sel7 (_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel7 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) a7 where sel7 (_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel7 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) a7 where sel7 (_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel7 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23) a7 where sel7 (_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel7 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24) a7 where sel7 (_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel7 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25) a7 where sel7 (_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel7 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) a7 where sel7 (_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel7 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) a7 where sel7 (_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel7 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a7 where sel7 (_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel7 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a7 where sel7 (_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel7 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a7 where sel7 (_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel7 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a7 where sel7 (_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel7 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a7 where sel7 (_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x

class Sel8 a b | a -> b where sel8 :: a -> b
instance Sel8 (a1,a2,a3,a4,a5,a6,a7,a8) a8 where sel8 (_,_,_,_,_,_,_,x) = x
instance Sel8 (a1,a2,a3,a4,a5,a6,a7,a8,a9) a8 where sel8 (_,_,_,_,_,_,_,x,_) = x
instance Sel8 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) a8 where sel8 (_,_,_,_,_,_,_,x,_,_) = x
instance Sel8 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) a8 where sel8 (_,_,_,_,_,_,_,x,_,_,_) = x
instance Sel8 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) a8 where sel8 (_,_,_,_,_,_,_,x,_,_,_,_) = x
instance Sel8 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) a8 where sel8 (_,_,_,_,_,_,_,x,_,_,_,_,_) = x
instance Sel8 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) a8 where sel8 (_,_,_,_,_,_,_,x,_,_,_,_,_,_) = x
instance Sel8 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) a8 where sel8 (_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) = x
instance Sel8 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) a8 where sel8 (_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) = x
instance Sel8 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) a8 where sel8 (_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) = x
instance Sel8 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) a8 where sel8 (_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) = x
instance Sel8 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) a8 where sel8 (_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel8 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) a8 where sel8 (_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel8 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) a8 where sel8 (_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel8 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) a8 where sel8 (_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel8 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23) a8 where sel8 (_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel8 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24) a8 where sel8 (_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel8 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25) a8 where sel8 (_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel8 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) a8 where sel8 (_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel8 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) a8 where sel8 (_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel8 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a8 where sel8 (_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel8 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a8 where sel8 (_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel8 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a8 where sel8 (_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel8 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a8 where sel8 (_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel8 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a8 where sel8 (_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x

class Sel9 a b | a -> b where sel9 :: a -> b
instance Sel9 (a1,a2,a3,a4,a5,a6,a7,a8,a9) a9 where sel9 (_,_,_,_,_,_,_,_,x) = x
instance Sel9 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) a9 where sel9 (_,_,_,_,_,_,_,_,x,_) = x
instance Sel9 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) a9 where sel9 (_,_,_,_,_,_,_,_,x,_,_) = x
instance Sel9 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) a9 where sel9 (_,_,_,_,_,_,_,_,x,_,_,_) = x
instance Sel9 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) a9 where sel9 (_,_,_,_,_,_,_,_,x,_,_,_,_) = x
instance Sel9 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) a9 where sel9 (_,_,_,_,_,_,_,_,x,_,_,_,_,_) = x
instance Sel9 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) a9 where sel9 (_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) = x
instance Sel9 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) a9 where sel9 (_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) = x
instance Sel9 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) a9 where sel9 (_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) = x
instance Sel9 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) a9 where sel9 (_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) = x
instance Sel9 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) a9 where sel9 (_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) = x
instance Sel9 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) a9 where sel9 (_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel9 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) a9 where sel9 (_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel9 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) a9 where sel9 (_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel9 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23) a9 where sel9 (_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel9 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24) a9 where sel9 (_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel9 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25) a9 where sel9 (_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel9 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) a9 where sel9 (_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel9 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) a9 where sel9 (_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel9 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a9 where sel9 (_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel9 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a9 where sel9 (_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel9 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a9 where sel9 (_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel9 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a9 where sel9 (_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel9 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a9 where sel9 (_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x

class Sel10 a b | a -> b where sel10 :: a -> b
instance Sel10 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) a10 where sel10 (_,_,_,_,_,_,_,_,_,x) = x
instance Sel10 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) a10 where sel10 (_,_,_,_,_,_,_,_,_,x,_) = x
instance Sel10 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) a10 where sel10 (_,_,_,_,_,_,_,_,_,x,_,_) = x
instance Sel10 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) a10 where sel10 (_,_,_,_,_,_,_,_,_,x,_,_,_) = x
instance Sel10 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) a10 where sel10 (_,_,_,_,_,_,_,_,_,x,_,_,_,_) = x
instance Sel10 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) a10 where sel10 (_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) = x
instance Sel10 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) a10 where sel10 (_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) = x
instance Sel10 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) a10 where sel10 (_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) = x
instance Sel10 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) a10 where sel10 (_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) = x
instance Sel10 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) a10 where sel10 (_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) = x
instance Sel10 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) a10 where sel10 (_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) = x
instance Sel10 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) a10 where sel10 (_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel10 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) a10 where sel10 (_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel10 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23) a10 where sel10 (_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel10 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24) a10 where sel10 (_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel10 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25) a10 where sel10 (_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel10 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) a10 where sel10 (_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel10 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) a10 where sel10 (_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel10 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a10 where sel10 (_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel10 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a10 where sel10 (_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel10 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a10 where sel10 (_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel10 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a10 where sel10 (_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel10 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a10 where sel10 (_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x

class Sel11 a b | a -> b where sel11 :: a -> b
instance Sel11 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) a11 where sel11 (_,_,_,_,_,_,_,_,_,_,x) = x
instance Sel11 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) a11 where sel11 (_,_,_,_,_,_,_,_,_,_,x,_) = x
instance Sel11 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) a11 where sel11 (_,_,_,_,_,_,_,_,_,_,x,_,_) = x
instance Sel11 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) a11 where sel11 (_,_,_,_,_,_,_,_,_,_,x,_,_,_) = x
instance Sel11 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) a11 where sel11 (_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) = x
instance Sel11 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) a11 where sel11 (_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) = x
instance Sel11 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) a11 where sel11 (_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) = x
instance Sel11 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) a11 where sel11 (_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) = x
instance Sel11 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) a11 where sel11 (_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) = x
instance Sel11 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) a11 where sel11 (_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) = x
instance Sel11 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) a11 where sel11 (_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) = x
instance Sel11 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) a11 where sel11 (_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel11 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23) a11 where sel11 (_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel11 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24) a11 where sel11 (_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel11 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25) a11 where sel11 (_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel11 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) a11 where sel11 (_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel11 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) a11 where sel11 (_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel11 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a11 where sel11 (_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel11 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a11 where sel11 (_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel11 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a11 where sel11 (_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel11 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a11 where sel11 (_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel11 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a11 where sel11 (_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x

class Sel12 a b | a -> b where sel12 :: a -> b
instance Sel12 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) a12 where sel12 (_,_,_,_,_,_,_,_,_,_,_,x) = x
instance Sel12 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) a12 where sel12 (_,_,_,_,_,_,_,_,_,_,_,x,_) = x
instance Sel12 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) a12 where sel12 (_,_,_,_,_,_,_,_,_,_,_,x,_,_) = x
instance Sel12 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) a12 where sel12 (_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) = x
instance Sel12 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) a12 where sel12 (_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) = x
instance Sel12 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) a12 where sel12 (_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) = x
instance Sel12 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) a12 where sel12 (_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) = x
instance Sel12 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) a12 where sel12 (_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) = x
instance Sel12 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) a12 where sel12 (_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) = x
instance Sel12 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) a12 where sel12 (_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) = x
instance Sel12 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) a12 where sel12 (_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) = x
instance Sel12 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23) a12 where sel12 (_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel12 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24) a12 where sel12 (_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel12 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25) a12 where sel12 (_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel12 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) a12 where sel12 (_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel12 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) a12 where sel12 (_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel12 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a12 where sel12 (_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel12 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a12 where sel12 (_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel12 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a12 where sel12 (_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel12 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a12 where sel12 (_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel12 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a12 where sel12 (_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x

class Sel13 a b | a -> b where sel13 :: a -> b
instance Sel13 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) a13 where sel13 (_,_,_,_,_,_,_,_,_,_,_,_,x) = x
instance Sel13 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) a13 where sel13 (_,_,_,_,_,_,_,_,_,_,_,_,x,_) = x
instance Sel13 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) a13 where sel13 (_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) = x
instance Sel13 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) a13 where sel13 (_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) = x
instance Sel13 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) a13 where sel13 (_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) = x
instance Sel13 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) a13 where sel13 (_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) = x
instance Sel13 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) a13 where sel13 (_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) = x
instance Sel13 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) a13 where sel13 (_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) = x
instance Sel13 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) a13 where sel13 (_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) = x
instance Sel13 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) a13 where sel13 (_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) = x
instance Sel13 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23) a13 where sel13 (_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) = x
instance Sel13 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24) a13 where sel13 (_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel13 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25) a13 where sel13 (_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel13 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) a13 where sel13 (_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel13 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) a13 where sel13 (_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel13 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a13 where sel13 (_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel13 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a13 where sel13 (_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel13 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a13 where sel13 (_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel13 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a13 where sel13 (_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel13 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a13 where sel13 (_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x

class Sel14 a b | a -> b where sel14 :: a -> b
instance Sel14 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) a14 where sel14 (_,_,_,_,_,_,_,_,_,_,_,_,_,x) = x
instance Sel14 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) a14 where sel14 (_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) = x
instance Sel14 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) a14 where sel14 (_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) = x
instance Sel14 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) a14 where sel14 (_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) = x
instance Sel14 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) a14 where sel14 (_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) = x
instance Sel14 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) a14 where sel14 (_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) = x
instance Sel14 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) a14 where sel14 (_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) = x
instance Sel14 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) a14 where sel14 (_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) = x
instance Sel14 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) a14 where sel14 (_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) = x
instance Sel14 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23) a14 where sel14 (_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) = x
instance Sel14 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24) a14 where sel14 (_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) = x
instance Sel14 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25) a14 where sel14 (_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel14 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) a14 where sel14 (_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel14 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) a14 where sel14 (_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel14 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a14 where sel14 (_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel14 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a14 where sel14 (_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel14 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a14 where sel14 (_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel14 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a14 where sel14 (_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel14 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a14 where sel14 (_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x

class Sel15 a b | a -> b where sel15 :: a -> b
instance Sel15 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) a15 where sel15 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) = x
instance Sel15 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) a15 where sel15 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) = x
instance Sel15 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) a15 where sel15 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) = x
instance Sel15 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) a15 where sel15 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) = x
instance Sel15 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) a15 where sel15 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) = x
instance Sel15 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) a15 where sel15 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) = x
instance Sel15 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) a15 where sel15 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) = x
instance Sel15 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) a15 where sel15 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) = x
instance Sel15 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23) a15 where sel15 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) = x
instance Sel15 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24) a15 where sel15 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) = x
instance Sel15 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25) a15 where sel15 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) = x
instance Sel15 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) a15 where sel15 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel15 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) a15 where sel15 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel15 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a15 where sel15 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel15 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a15 where sel15 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel15 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a15 where sel15 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel15 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a15 where sel15 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel15 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a15 where sel15 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x

class Sel16 a b | a -> b where sel16 :: a -> b
instance Sel16 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) a16 where sel16 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) = x
instance Sel16 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) a16 where sel16 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) = x
instance Sel16 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) a16 where sel16 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) = x
instance Sel16 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) a16 where sel16 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) = x
instance Sel16 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) a16 where sel16 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) = x
instance Sel16 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) a16 where sel16 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) = x
instance Sel16 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) a16 where sel16 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) = x
instance Sel16 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23) a16 where sel16 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) = x
instance Sel16 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24) a16 where sel16 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) = x
instance Sel16 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25) a16 where sel16 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) = x
instance Sel16 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) a16 where sel16 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) = x
instance Sel16 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) a16 where sel16 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel16 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a16 where sel16 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel16 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a16 where sel16 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel16 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a16 where sel16 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel16 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a16 where sel16 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel16 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a16 where sel16 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x

class Sel17 a b | a -> b where sel17 :: a -> b
instance Sel17 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) a17 where sel17 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) = x
instance Sel17 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) a17 where sel17 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) = x
instance Sel17 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) a17 where sel17 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) = x
instance Sel17 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) a17 where sel17 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) = x
instance Sel17 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) a17 where sel17 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) = x
instance Sel17 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) a17 where sel17 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) = x
instance Sel17 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23) a17 where sel17 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) = x
instance Sel17 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24) a17 where sel17 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) = x
instance Sel17 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25) a17 where sel17 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) = x
instance Sel17 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) a17 where sel17 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) = x
instance Sel17 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) a17 where sel17 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) = x
instance Sel17 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a17 where sel17 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel17 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a17 where sel17 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel17 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a17 where sel17 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel17 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a17 where sel17 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel17 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a17 where sel17 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x

class Sel18 a b | a -> b where sel18 :: a -> b
instance Sel18 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) a18 where sel18 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) = x
instance Sel18 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) a18 where sel18 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) = x
instance Sel18 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) a18 where sel18 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) = x
instance Sel18 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) a18 where sel18 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) = x
instance Sel18 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) a18 where sel18 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) = x
instance Sel18 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23) a18 where sel18 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) = x
instance Sel18 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24) a18 where sel18 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) = x
instance Sel18 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25) a18 where sel18 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) = x
instance Sel18 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) a18 where sel18 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) = x
instance Sel18 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) a18 where sel18 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) = x
instance Sel18 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a18 where sel18 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) = x
instance Sel18 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a18 where sel18 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel18 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a18 where sel18 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel18 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a18 where sel18 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel18 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a18 where sel18 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = x

class Sel19 a b | a -> b where sel19 :: a -> b
instance Sel19 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) a19 where sel19 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) = x
instance Sel19 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) a19 where sel19 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) = x
instance Sel19 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) a19 where sel19 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) = x
instance Sel19 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) a19 where sel19 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) = x
instance Sel19 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23) a19 where sel19 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) = x
instance Sel19 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24) a19 where sel19 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) = x
instance Sel19 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25) a19 where sel19 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) = x
instance Sel19 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) a19 where sel19 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) = x
instance Sel19 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) a19 where sel19 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) = x
instance Sel19 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a19 where sel19 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) = x
instance Sel19 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a19 where sel19 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) = x
instance Sel19 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a19 where sel19 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel19 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a19 where sel19 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel19 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a19 where sel19 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) = x

class Sel20 a b | a -> b where sel20 :: a -> b
instance Sel20 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) a20 where sel20 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) = x
instance Sel20 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) a20 where sel20 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) = x
instance Sel20 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) a20 where sel20 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) = x
instance Sel20 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23) a20 where sel20 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) = x
instance Sel20 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24) a20 where sel20 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) = x
instance Sel20 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25) a20 where sel20 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) = x
instance Sel20 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) a20 where sel20 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) = x
instance Sel20 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) a20 where sel20 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) = x
instance Sel20 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a20 where sel20 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) = x
instance Sel20 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a20 where sel20 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) = x
instance Sel20 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a20 where sel20 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) = x
instance Sel20 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a20 where sel20 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) = x
instance Sel20 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a20 where sel20 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) = x

class Sel21 a b | a -> b where sel21 :: a -> b
instance Sel21 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) a21 where sel21 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) = x
instance Sel21 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) a21 where sel21 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) = x
instance Sel21 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23) a21 where sel21 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) = x
instance Sel21 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24) a21 where sel21 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) = x
instance Sel21 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25) a21 where sel21 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) = x
instance Sel21 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) a21 where sel21 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) = x
instance Sel21 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) a21 where sel21 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) = x
instance Sel21 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a21 where sel21 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) = x
instance Sel21 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a21 where sel21 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) = x
instance Sel21 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a21 where sel21 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) = x
instance Sel21 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a21 where sel21 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) = x
instance Sel21 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a21 where sel21 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) = x

class Sel22 a b | a -> b where sel22 :: a -> b
instance Sel22 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) a22 where sel22 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) = x
instance Sel22 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23) a22 where sel22 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) = x
instance Sel22 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24) a22 where sel22 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) = x
instance Sel22 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25) a22 where sel22 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) = x
instance Sel22 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) a22 where sel22 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) = x
instance Sel22 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) a22 where sel22 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) = x
instance Sel22 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a22 where sel22 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) = x
instance Sel22 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a22 where sel22 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) = x
instance Sel22 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a22 where sel22 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) = x
instance Sel22 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a22 where sel22 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) = x
instance Sel22 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a22 where sel22 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) = x

class Sel23 a b | a -> b where sel23 :: a -> b
instance Sel23 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23) a23 where sel23 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) = x
instance Sel23 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24) a23 where sel23 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) = x
instance Sel23 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25) a23 where sel23 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) = x
instance Sel23 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) a23 where sel23 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) = x
instance Sel23 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) a23 where sel23 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) = x
instance Sel23 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a23 where sel23 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) = x
instance Sel23 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a23 where sel23 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) = x
instance Sel23 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a23 where sel23 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) = x
instance Sel23 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a23 where sel23 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) = x
instance Sel23 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a23 where sel23 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) = x

class Sel24 a b | a -> b where sel24 :: a -> b
instance Sel24 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24) a24 where sel24 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) = x
instance Sel24 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25) a24 where sel24 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) = x
instance Sel24 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) a24 where sel24 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) = x
instance Sel24 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) a24 where sel24 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) = x
instance Sel24 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a24 where sel24 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) = x
instance Sel24 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a24 where sel24 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) = x
instance Sel24 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a24 where sel24 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) = x
instance Sel24 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a24 where sel24 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) = x
instance Sel24 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a24 where sel24 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) = x

class Sel25 a b | a -> b where sel25 :: a -> b
instance Sel25 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25) a25 where sel25 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) = x
instance Sel25 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) a25 where sel25 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) = x
instance Sel25 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) a25 where sel25 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) = x
instance Sel25 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a25 where sel25 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) = x
instance Sel25 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a25 where sel25 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) = x
instance Sel25 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a25 where sel25 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) = x
instance Sel25 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a25 where sel25 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) = x
instance Sel25 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a25 where sel25 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) = x

class Sel26 a b | a -> b where sel26 :: a -> b
instance Sel26 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26) a26 where sel26 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) = x
instance Sel26 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) a26 where sel26 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) = x
instance Sel26 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a26 where sel26 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) = x
instance Sel26 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a26 where sel26 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) = x
instance Sel26 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a26 where sel26 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) = x
instance Sel26 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a26 where sel26 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) = x
instance Sel26 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a26 where sel26 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) = x

class Sel27 a b | a -> b where sel27 :: a -> b
instance Sel27 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27) a27 where sel27 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) = x
instance Sel27 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a27 where sel27 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) = x
instance Sel27 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a27 where sel27 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) = x
instance Sel27 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a27 where sel27 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) = x
instance Sel27 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a27 where sel27 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) = x
instance Sel27 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a27 where sel27 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) = x

class Sel28 a b | a -> b where sel28 :: a -> b
instance Sel28 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28) a28 where sel28 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) = x
instance Sel28 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a28 where sel28 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) = x
instance Sel28 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a28 where sel28 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) = x
instance Sel28 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a28 where sel28 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) = x
instance Sel28 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a28 where sel28 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) = x

class Sel29 a b | a -> b where sel29 :: a -> b
instance Sel29 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29) a29 where sel29 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) = x
instance Sel29 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a29 where sel29 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) = x
instance Sel29 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a29 where sel29 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) = x
instance Sel29 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a29 where sel29 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) = x

class Sel30 a b | a -> b where sel30 :: a -> b
instance Sel30 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30) a30 where sel30 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) = x
instance Sel30 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a30 where sel30 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) = x
instance Sel30 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a30 where sel30 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) = x

class Sel31 a b | a -> b where sel31 :: a -> b
instance Sel31 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) a31 where sel31 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) = x
instance Sel31 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a31 where sel31 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) = x

class Sel32 a b | a -> b where sel32 :: a -> b
instance Sel32 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32) a32 where sel32 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) = x

