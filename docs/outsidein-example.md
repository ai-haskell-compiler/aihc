Below is a fuller **end-to-end example** in markdown, showing:

1. a Haskell-like source program
2. the main constraints generated
3. how the solver discharges them
4. the resulting explicit core term

I’ll use a small, readable pseudo-core:

- `Λa.` for type abstraction
- `@Bool` for type application
- `\d : Eq a -> ...` for dictionary abstraction
- `co : a ~ Bool` for equality evidence
- `cast e by co` for coercion use

This is not exact GHC Core syntax, but it matches the key OutsideIn(X) ideas: **generate constraints, solve them, and elaborate with explicit evidence**. The paper presents OutsideIn(X) precisely as a constraint-based inference algorithm with a separate solver, and its implementation section explicitly includes **evidence**.

---

# End-to-end OutsideIn(X) example

## Source program

```haskell
data G a where
  GBool :: Eq Bool => Bool -> G Bool
  GInt  :: Eq Int  => Int  -> G Int

useG :: G a -> a -> Bool
useG g x =
  case g of
    GBool b -> x == b
    GInt  n -> x == n
```

This example is nice because it combines:

- **GADT branch refinement**
- **type class dictionaries**
- **equality evidence**
- **dictionary passing in the result**

---

# 1. What types are in scope?

Assume the class method:

```haskell
(==) :: forall t. Eq t => t -> t -> Bool
```

And think of the constructors as if they carried explicit evidence:

```haskell
GBool :: (Eq Bool) => Bool -> G Bool
GInt  :: (Eq Int)  => Int  -> G Int
```

Pattern matching on `GBool` should therefore introduce:

- a local equality showing the scrutinee type parameter is `Bool`
- a local dictionary `Eq Bool`

Similarly for `GInt`.

This matches the paper’s treatment of constructors that introduce **local constraints** during pattern matching, which is one of the central reasons implication constraints are needed in OutsideIn(X).

---

# 2. Typing goal

We want to check:

```haskell
useG :: G a -> a -> Bool
```

So inside the body, before the `case`, we have:

```text
g : G a
x : a
```

The whole `case` must return `Bool`.

---

# 3. Constraint generation

The interesting work happens branch-by-branch.

## Branch 1

```haskell
GBool b -> x == b
```

Pattern matching on `GBool` brings in:

- `b : Bool`

- given dictionary:

  ```text
  dEqBool : Eq Bool
  ```

- given equality:

  ```text
  coBool : a ~ Bool
  ```

Now typecheck `x == b`.

### Instantiate `(==)`

Instantiate:

```text
(==) :: forall t. Eq t => t -> t -> Bool
```

with a fresh type variable `β1`:

```text
(==) : Eq β1 => β1 -> β1 -> Bool
```

### Apply to arguments

From `x`, we get:

```text
x : a
```

So applying `(==)` to `x` requires:

```text
β1 ~ a
```

From `b`, we get:

```text
b : Bool
```

So applying to `b` requires:

```text
β1 ~ Bool
```

And there is a wanted class constraint:

```text
Eq β1
```

### Branch-local givens

But this branch has givens:

```text
a ~ Bool
Eq Bool
```

So the branch constraints are roughly:

```text
Given:
  a ~ Bool
  Eq Bool

Wanted:
  β1 ~ a
  β1 ~ Bool
  Eq β1
```

These are exactly the kind of constraints that, in OutsideIn(X), live under a branch-local implication. The paper describes implication constraints as the mechanism for handling local assumptions from pattern matching.

---

## Branch 2

```haskell
GInt n -> x == n
```

Similarly, pattern matching on `GInt` brings in:

- `n : Int`

- given dictionary:

  ```text
  dEqInt : Eq Int
  ```

- given equality:

  ```text
  coInt : a ~ Int
  ```

Instantiating `(==)` with fresh `β2` gives:

```text
Wanted:
  β2 ~ a
  β2 ~ Int
  Eq β2
```

under givens:

```text
Given:
  a ~ Int
  Eq Int
```

---

# 4. Why this program fails

Now step back and look at the whole function:

```haskell
useG :: G a -> a -> Bool
```

The first branch needs:

```text
a ~ Bool
```

The second branch needs:

```text
a ~ Int
```

But those equalities are only true **locally** inside each branch. There is no single global choice of `a` that makes both branches type the same way for arbitrary `G a -> a -> Bool`.

So this program is actually **not typable with that signature**.

This is exactly the kind of situation where OutsideIn(X) avoids making illegitimate global inferences from local constraints. One of the core insights of the paper is that local assumptions should not let you freely solve outer variables in arbitrary ways.

---

# 5. Fixing the example

Let’s repair it so it becomes typable.

## Revised source

```haskell
data G a where
  GBool :: Eq Bool => Bool -> G Bool
  GInt  :: Eq Int  => Int  -> G Int

useG :: G a -> Bool
useG g =
  case g of
    GBool b -> b == b
    GInt  n -> n == n
```

Now each branch returns `Bool` without trying to use a shared `x : a`.

This is much better for showing the full pipeline.

---

# 6. Constraint generation for the fixed example

## Function arguments

From the signature:

```text
g : G a
```

The whole `case` must return `Bool`.

---

## Branch 1: `GBool b -> b == b`

Pattern match introduces:

```text
b : Bool
dEqBool : Eq Bool
coBool : a ~ Bool
```

Instantiate `(==)` with fresh `β1`:

```text
(==) : Eq β1 => β1 -> β1 -> Bool
```

Applications to `b` and `b` generate:

```text
β1 ~ Bool
β1 ~ Bool
Eq β1
```

Given `Eq Bool`, the solver can discharge:

```text
β1 := Bool
Eq β1  solved by dEqBool
```

So branch 1 is fine.

---

## Branch 2: `GInt n -> n == n`

Pattern match introduces:

```text
n : Int
dEqInt : Eq Int
coInt : a ~ Int
```

Instantiate `(==)` with fresh `β2`:

```text
(==) : Eq β2 => β2 -> β2 -> Bool
```

Applications generate:

```text
β2 ~ Int
β2 ~ Int
Eq β2
```

Given `Eq Int`, the solver discharges:

```text
β2 := Int
Eq β2 solved by dEqInt
```

So branch 2 is fine too.

---

# 7. Solved evidence

After solving, the relevant evidence is:

## Branch 1

- use `dEqBool` as the dictionary argument to `(==)`
- `β1` solved to `Bool`

## Branch 2

- use `dEqInt` as the dictionary argument to `(==)`
- `β2` solved to `Int`

The equalities `a ~ Bool` and `a ~ Int` are present as branch-local evidence but are not needed by the branch bodies here. They still exist logically because the match refines the scrutinee’s type.

---

# 8. Elaborated core

A reasonable explicit core is:

```haskell
useG =
  Λa. \(g : G a) ->
    case g of
      GBool @(a) (coBool : a ~ Bool) (dEqBool : Eq Bool) (b : Bool) ->
        (==) @Bool dEqBool b b

      GInt @(a) (coInt : a ~ Int) (dEqInt : Eq Int) (n : Int) ->
        (==) @Int dEqInt n n
```

That is the key picture:

- `Λa` is the explicit type abstraction from the function type
- each constructor pattern introduces:
  - a coercion proof (`coBool`, `coInt`)
  - a dictionary (`dEqBool`, `dEqInt`)
  - the constructor field itself (`b`, `n`)

- calls to `(==)` are fully explicit:
  - type application
  - dictionary argument
  - value arguments

---

# 9. A type-family end-to-end example

Now let’s do the same with a type family.

## Source

```haskell
type family F a
type instance F Int = Bool

useF :: F Int -> Bool
useF x = not x
```

Assume:

```haskell
not :: Bool -> Bool
```

---

## Step 1: infer the body

We know from the signature:

```text
x : F Int
```

But `not` expects:

```text
Bool
```

So applying `not` to `x` generates the wanted equality:

```text
F Int ~ Bool
```

The paper explicitly discusses type families as giving rise to non-structural equality constraints, which are a major part of the intended `X` instantiation.

---

## Step 2: solve the family equality

From the axiom:

```haskell
type instance F Int = Bool
```

the solver derives coercion evidence:

```text
coFInt : F Int ~ Bool
```

---

## Step 3: elaborate

The core output becomes:

```haskell
useF =
  \(x : F Int) ->
    let coFInt : F Int ~ Bool = axiom_F_Int in
    not (cast x by coFInt)
```

So here the interesting evidence is not a dictionary but a **coercion from a type family reduction**.

---

# 10. A compact combined example

Here is the simplest compact summary of all three evidence forms.

## Source

```haskell
data Box a where
  MkBox :: Show a => a -> Box a

unboxShow :: Box a -> String
unboxShow b =
  case b of
    MkBox x -> show x
```

---

## Generated branch-local givens

Pattern matching on `MkBox` introduces:

```text
x : a
dShow : Show a
```

---

## Solved call to `show`

Instantiate:

```text
show :: forall t. Show t => t -> String
```

with `t := a`, yielding wanted:

```text
Show a
```

Solved by the branch-local given `dShow`.

---

## Core

```haskell
unboxShow =
  Λa. \(b : Box a) ->
    case b of
      MkBox @(a) (dShow : Show a) (x : a) ->
        show @a dShow x
```

This one is especially useful because it shows clearly that:

- type abstraction/application still exists
- class constraints become dictionary arguments
- GADT/existential matches can introduce evidence directly into scope

---

# Final takeaway

The full end-to-end OutsideIn(X) story is:

1. **Infer a type and generate wanted constraints**
2. **Represent branch-local assumptions as implications**
3. **Run the solver**
   - unify types
   - discharge dictionaries
   - reduce type family equalities

4. **Build explicit evidence**
   - type arguments
   - dictionaries
   - coercions

5. **Elaborate into explicit core**

So the output of an OutsideIn(X)-style typechecker is not just “System F with explicit type application.” It is a richer evidence-carrying core with:

- explicit type abstraction/application
- explicit dictionary abstraction/application
- explicit equality proofs and casts

That is the core architectural shift from HM/System F to OutsideIn(X).
