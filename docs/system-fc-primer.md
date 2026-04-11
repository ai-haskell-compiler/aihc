Here’s a **technical primer on System FC**, aimed at someone building or understanding a GHC-style typechecker.

---

# 🧠 System FC: A Technical Primer

**System FC** is the core intermediate language used by GHC. It extends **System F** with:

> **explicit coercions (proofs of type equality)**

This is what makes it powerful enough to support:

- GADTs
- type families
- newtypes
- type-safe casts

---

# 🧩 1. From System F → System FC

## System F gives you:

- polymorphism (`∀`)
- type abstraction/application

## System FC adds:

- **equality types** (`τ₁ ~ τ₂`)
- **coercion terms** (proofs of equality)
- **casts** (safe type conversion using proofs)

---

# 🔑 Core idea

> **Types can be equal in non-trivial ways, and we carry proofs of that equality explicitly.**

---

# 🧱 2. Core syntax (simplified)

## Types

```haskell
τ ::= α
    | T τ₁ ... τₙ
    | τ₁ → τ₂
    | ∀α. τ
```

Same as System F, but with equality types:

```haskell
φ ::= τ₁ ~ τ₂
```

---

## Terms

```haskell
e ::= x
    | λx : τ. e
    | e₁ e₂
    | Λα. e
    | e @τ
    | e ▷ γ         -- cast
    | let x = e₁ in e₂
```

New construct:

- `e ▷ γ` means: **cast `e` using coercion `γ`**

---

## Coercions

This is the key extension.

```haskell
γ ::= c                   -- coercion variable
    | refl τ              -- reflexivity
    | sym γ               -- symmetry
    | trans γ₁ γ₂         -- transitivity
    | T γ₁ ... γₙ         -- type constructor lifting
    | nth i γ             -- projection
    | axiom A τ₁ ... τₙ   -- type family / newtype axiom
```

Coercions are **proof objects** that witness:

```haskell
γ : τ₁ ~ τ₂
```

---

# ⚙️ 3. Typing rules (key ones)

## Cast

If:

```haskell
Γ ⊢ e : τ₁
Γ ⊢ γ : τ₁ ~ τ₂
```

Then:

```haskell
Γ ⊢ e ▷ γ : τ₂
```

---

## Coercion typing

Each coercion has a kind:

```haskell
Γ ⊢ γ : τ₁ ~ τ₂
```

Example:

```haskell
refl Int : Int ~ Int
```

---

# 🔄 4. Why coercions are needed

## 4.1 GADTs

Example:

```haskell
data T a where
  T1 :: Int -> T Bool
```

Pattern match:

```haskell
case x of
  T1 n -> ...
```

Inside the branch, we know:

```text
a ~ Bool
```

In System FC:

```haskell
T1 (co : a ~ Bool) (n : Int) -> ...
```

This `co` is a **coercion variable**.

---

## 4.2 Type families

Example:

```haskell
type family F a
type instance F Int = Bool
```

You get:

```haskell
axiom_F_Int : F Int ~ Bool
```

Used like:

```haskell
x ▷ axiom_F_Int
```

---

## 4.3 Newtypes

```haskell
newtype Age = MkAge Int
```

Gives coercion:

```haskell
co_Age : Age ~ Int
```

Used for zero-cost conversion.

---

# 🧠 5. Coercions are first-class (but erased)

Important properties:

- Coercions are:
  - **explicit**
  - **typed**
  - **checked**

- But:
  - **erased at runtime**

---

# 🔐 6. Safety guarantee

System FC ensures:

> If a coercion exists between `τ₁` and `τ₂`, then they are representationally equal.

So casts:

```haskell
e ▷ γ
```

are always safe — no runtime type errors.

---

# 🔄 7. Coercion structure in detail

## Reflexivity

```haskell
refl τ : τ ~ τ
```

---

## Symmetry

```haskell
sym γ : τ₂ ~ τ₁
```

---

## Transitivity

```haskell
trans γ₁ γ₂ : τ₁ ~ τ₃
```

if:

```haskell
γ₁ : τ₁ ~ τ₂
γ₂ : τ₂ ~ τ₃
```

---

## Type constructor lifting

If:

```haskell
γ₁ : τ₁ ~ σ₁
γ₂ : τ₂ ~ σ₂
```

Then:

```haskell
Maybe γ₁ : Maybe τ₁ ~ Maybe σ₁
(γ₁ → γ₂) : (τ₁ → τ₂) ~ (σ₁ → σ₂)
```

---

## Axioms

From type families:

```haskell
axiom F Int = Bool
```

becomes:

```haskell
axiom_F_Int : F Int ~ Bool
```

---

# 🧩 8. Evidence in System FC

System FC unifies:

| Concept           | Representation                |
| ----------------- | ----------------------------- |
| type classes      | dictionaries (ordinary terms) |
| equalities        | coercions                     |
| inference results | explicit evidence             |

---

# 🧠 9. Relation to OutsideIn(X)

OutsideIn(X):

- generates constraints
- solves them
- produces **evidence**

System FC:

- is where that evidence is _materialized_

So:

```text
OutsideIn(X) → System FC
```

---

# ⚙️ 10. Example: full elaboration

## Source

```haskell
type family F a
type instance F Int = Bool

f :: F Int -> Bool
f x = not x
```

---

## System FC

```haskell
f =
  \ (x : F Int) ->
    let co : F Int ~ Bool = axiom_F_Int in
    not (x ▷ co)
```

---

# 🔍 11. Kinds of equality

In full GHC, there are different notions:

- nominal equality (`~`)
- representational equality (`~R`)
- phantom equality

System FC tracks these distinctions.

---

# 🚧 12. What System FC does NOT do

- No type inference (all types explicit)
- No implicit constraints
- No overloading resolution

Those are handled earlier (OutsideIn).

---

# 🧠 13. Key mental model

> **System FC = System F + explicit equality proofs**

Or more precisely:

> **A typed lambda calculus where both values and type equalities are first-class (but erasable).**

---

# 📌 14. Why this design matters

It enables:

- safe GADT pattern matching
- type family computation
- zero-cost newtypes
- aggressive optimization (after erasing coercions)

---

# 🚀 15. One-line takeaway

> System FC is the language where **types, programs, and proofs of type equality all coexist explicitly**.
