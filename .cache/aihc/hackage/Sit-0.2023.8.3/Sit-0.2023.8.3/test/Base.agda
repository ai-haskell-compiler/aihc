{-# OPTIONS --experimental-irrelevance #-}
{-# OPTIONS --sized-types #-}

open import Agda.Primitive
  public using (lzero; lsuc)

open import Agda.Builtin.Size
  public using (Size; ↑_) renaming (∞ to oo)

open import Agda.Builtin.Nat
  public using (suc) renaming (Nat to ℕ)

_+_ : Size → ℕ → Size
s + 0 = s
s + suc n = ↑ (s + n)

data Nat : ..(i : Size) → Set where
  zero : ∀ .i → Nat (↑ i)
  suc  : ∀ .i → Nat i → Nat (↑ i)

caseof : ∀{a b} {A : Set a} (B : A → Set b) → (x : A) → ((x : A) → B x) → B x
caseof B x f = f x

syntax caseof B x f = case x return B of f

fix : ∀{ℓ}
  (T : ..(i : Size) → Nat i → Set ℓ)
  (f : ∀ .j → ((x : Nat j) → T j x) → (x : Nat (↑ j)) → T (↑ j) x)
  .{i}
  (x : Nat i)
  → T i x
fix T f (zero j)  = f j (fix T f) (zero j)
fix T f (suc j n) = f j (fix T f) (suc j n)
