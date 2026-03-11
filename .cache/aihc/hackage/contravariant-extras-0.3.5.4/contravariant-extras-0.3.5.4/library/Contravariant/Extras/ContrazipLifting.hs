module Contravariant.Extras.ContrazipLifting where

import Contravariant.Extras.Contrazip
import Contravariant.Extras.Prelude

contrazipLifting2 :: (Divisible f) => (forall x. g x -> f x) -> g a1 -> g a2 -> f (a1, a2)
contrazipLifting2 fn a b = contrazip2 (fn a) (fn b)

contrazipLifting3 :: (Divisible f) => (forall x. g x -> f x) -> g a1 -> g a2 -> g a3 -> f (a1, a2, a3)
contrazipLifting3 fn a b c = contrazip3 (fn a) (fn b) (fn c)

contrazipLifting4 :: (Divisible f) => (forall x. g x -> f x) -> g a1 -> g a2 -> g a3 -> g a4 -> f (a1, a2, a3, a4)
contrazipLifting4 fn a b c d = contrazip4 (fn a) (fn b) (fn c) (fn d)

contrazipLifting5 :: (Divisible f) => (forall x. g x -> f x) -> g a1 -> g a2 -> g a3 -> g a4 -> g a5 -> f (a1, a2, a3, a4, a5)
contrazipLifting5 fn a b c d e = contrazip5 (fn a) (fn b) (fn c) (fn d) (fn e)

contrazipLifting6 :: (Divisible f) => (forall x. g x -> f x) -> g a1 -> g a2 -> g a3 -> g a4 -> g a5 -> g a6 -> f (a1, a2, a3, a4, a5, a6)
contrazipLifting6 fn a b c d e f = contrazip6 (fn a) (fn b) (fn c) (fn d) (fn e) (fn f)

contrazipLifting7 :: (Divisible f) => (forall x. g x -> f x) -> g a1 -> g a2 -> g a3 -> g a4 -> g a5 -> g a6 -> g a7 -> f (a1, a2, a3, a4, a5, a6, a7)
contrazipLifting7 fn a b c d e f g = contrazip7 (fn a) (fn b) (fn c) (fn d) (fn e) (fn f) (fn g)

contrazipLifting8 :: (Divisible f) => (forall x. g x -> f x) -> g a1 -> g a2 -> g a3 -> g a4 -> g a5 -> g a6 -> g a7 -> g a8 -> f (a1, a2, a3, a4, a5, a6, a7, a8)
contrazipLifting8 fn a b c d e f g h = contrazip8 (fn a) (fn b) (fn c) (fn d) (fn e) (fn f) (fn g) (fn h)

contrazipLifting9 :: (Divisible f) => (forall x. g x -> f x) -> g a1 -> g a2 -> g a3 -> g a4 -> g a5 -> g a6 -> g a7 -> g a8 -> g a9 -> f (a1, a2, a3, a4, a5, a6, a7, a8, a9)
contrazipLifting9 fn a b c d e f g h i = contrazip9 (fn a) (fn b) (fn c) (fn d) (fn e) (fn f) (fn g) (fn h) (fn i)

contrazipLifting10 :: (Divisible f) => (forall x. g x -> f x) -> g a1 -> g a2 -> g a3 -> g a4 -> g a5 -> g a6 -> g a7 -> g a8 -> g a9 -> g a10 -> f (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
contrazipLifting10 fn a b c d e f g h i j = contrazip10 (fn a) (fn b) (fn c) (fn d) (fn e) (fn f) (fn g) (fn h) (fn i) (fn j)
