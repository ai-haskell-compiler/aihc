--- Sample Sit file

{-# OPTIONS --experimental-irrelevance #-}
{-# OPTIONS --sized-types #-}

open import Base

--; --- Leibniz-equality

Eq : forall (A : Set) (a b : A) -> Set1 --;
Eq = \ A a b -> (P : A -> Set) -> (P a) -> P b

--; --- Reflexivity

refl : forall (A : Set) (a : A) -> Eq A a a --;
refl = \ A a P pa -> pa

--; --- Symmetry

sym : forall (A : Set) (a b : A) -> Eq A a b -> Eq A b a --;
sym = \ A a b eq P pb ->  eq (\ x -> P x -> P a) (\ pa -> pa) pb

--; --- Transitivity

trans : forall (A : Set) (a b c : A) -> Eq A a b -> Eq A b c -> Eq A a c --;
trans = \ A a b c p q P pa -> q P (p P pa)

--; --- Congruence

cong : forall (A B : Set) (f : A -> B) (a a' : A) -> Eq A a a' -> Eq B (f a) (f a') --;
cong = \ A B f a a' eq P pfa -> eq (\ x -> P (f x)) pfa

--; --- Addition

plus : forall .i -> Nat i -> Nat oo -> Nat oo --;
plus = \ i x y ->
  fix (\ i x -> Nat oo)
      (\ _ f -> \
              { (zero _)   -> y
              ; (suc _ x) -> suc oo (f x)
              })
      x

--; --- Unit tests for plus

inc : Nat oo -> Nat oo --;
inc = \ x -> suc oo x  --;

one : Nat oo        --;
one = inc (zero oo) --;

two : Nat oo  --;
two = inc one --;

three : Nat oo  --;
three = inc two --;

four : Nat oo  --;
four = inc three --;

five : Nat oo  --;
five = inc four --;

six : Nat oo  --;
six = inc five --;

plus_one_zero : Eq (Nat oo) (plus oo one (zero oo)) one --;
plus_one_zero = refl (Nat oo) one  --;

plus_one_one : Eq (Nat oo) (plus oo one one) two --;
plus_one_one = refl (Nat oo) two  --;

--; --- Reduction rules for plus

plus_red_zero : forall .i (y : Nat oo) -> Eq (Nat oo) (plus (i + 1) (zero i) y) y  --;
plus_red_zero = \ i y -> refl (Nat oo) y  --;

plus_red_suc : forall .i (x : Nat i) (y : Nat oo) -> Eq (Nat oo) (plus (i + 1) (suc i x) y) (suc oo (plus i x y))  --;
plus_red_suc = \ i x y -> refl (Nat oo) (suc oo (plus i x y))  --;

--; --- Law: x + 0 = x

plus_zero : forall .i (x : Nat i) -> Eq (Nat oo) (plus i x (zero oo)) x  --;
plus_zero = \ i x ->
  fix (\ i x -> Eq (Nat oo) (plus i x (zero oo)) x)
      (\ j f -> \
         { (zero _)  -> refl (Nat oo) (zero oo)
         ; (suc _ y) -> cong (Nat oo) (Nat oo) inc (plus j y (zero oo)) y (f y)
         })
      x

--; --- Law: x + suc y = suc x + y

plus_suc : forall .i (x : Nat i) (y : Nat oo) -> Eq (Nat oo) (plus i x (inc y)) (inc (plus i x y))  --;
plus_suc = \ i x y ->
  fix (\ i x -> Eq (Nat oo) (plus i x (inc y)) (inc (plus i x y)))
      (\ j f -> \
         { (zero _)  -> refl (Nat oo) (inc y)
         ; (suc _ x') -> cong (Nat oo) (Nat oo) inc (plus j x' (inc y)) (inc (plus j x' y)) (f x')
         })
      x

--; --- Another definition of addition

plus' : forall .i -> Nat i -> Nat oo -> Nat oo  --;
plus' = \ i x ->
  fix (\ i x -> Nat oo -> Nat oo)
      (\ _ f -> \
         { (zero _)  -> \ y -> y
         ; (suc _ x) -> \ y -> suc oo (f x y)
         })
      x

--; --- Predecessor

pred  : forall .i -> Nat i -> Nat i  --;
pred = \ i n ->
  fix (\ i _ -> Nat i)
      (\ i _ -> \{ (zero _) -> zero i ; (suc _ y) -> y })
      n

--; --- Subtraction

sub : forall .j -> Nat j -> forall .i -> Nat i -> Nat i  --;
sub = \ j y ->
  fix (\ _ _ -> forall .i -> Nat i -> Nat i)
      (\ _ f -> \
        { (zero _) -> \ i x -> x
        ; (suc _ y) -> \ i x -> f y i (pred i x)
        }) --- pred i (f y i x) })
      y

--; --- Lemma: x - x == 0

sub_diag : forall .i (x : Nat i) -> Eq (Nat oo) (sub i x i x) (zero oo)  --;
sub_diag = \ i x ->
  fix (\ i x -> Eq (Nat oo) (sub i x i x) (zero oo))
      (\ _ f -> \
        { (zero _) -> refl (Nat oo) (zero oo)
        ; (suc _ y) -> f y
        })
      x

--- Large eliminations

--; --- Varying arity

Fun : forall .i (n : Nat i) (A : Set) (B : Set) -> Set --;
Fun = \ i n A B ->
  fix (\ _ _ -> Set)
      (\ _ f -> \
         { (zero _) -> B
         ; (suc _ x) -> A -> f x
         })
      n

--; --- Type of n-ary Sum function

Sum : forall .i (n : Nat i) -> Set  --;
Sum = \ i n -> Nat oo -> Fun i n (Nat oo) (Nat oo)

--; --- n-ary summation function

sum : forall .i (n : Nat i) -> Sum i n --;
sum = \ _ n ->
  fix (\ i n -> Sum i n)
      (\ _ f -> \
        { (zero _) -> \ acc -> acc
        ; (suc _ x) -> \ acc -> \ k -> f x (plus oo k acc)
        })
      n

--; --- Testing sum

sum123 : Eq (Nat oo) (sum oo three (zero oo) one two three) six --;
sum123 = refl (Nat oo) six
