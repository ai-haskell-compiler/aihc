Below is a set of **worked, high-level examples** showing:

- a **Haskell-like input**
- the **main steps** the type checker takes
- a plausible **explicit core output**

I’ll keep the core language simple and consistent:

- `Λa.` = type abstraction
- `@Bool` = type application
- `\d : Eq a -> ...` = dictionary abstraction
- `co : a ~ Bool` = equality proof / coercion binder
- `cast e by co` = cast using a coercion

This is **not exactly GHC Core syntax**, but it matches the **OutsideIn(X)** idea: type checking produces an explicitly typed program with **type arguments, evidence arguments, and equality evidence**. The paper explicitly frames inference as **constraint generation + solving**, and its implementation section includes **evidence** as part of the design.

---

# Examples of OutsideIn(X) Elaboration

## 1. Simple polymorphism: type abstraction and application

This is the part most like ordinary System F.

### Input

```haskell
id x = x
unit = id False
```

---

### What the type checker does

#### For `id`

It introduces fresh type variables for the argument and result:

- `x : α`
- body `x : α`

So it infers:

```haskell
id :: forall a. a -> a
```

No class constraints, no equality constraints, no implications.

#### For `unit`

It looks up `id`, instantiates its polymorphic type with a fresh type variable `β`:

```haskell
id : β -> β
```

Then from the argument `False : Bool`, it gets the equality:

```text
β ~ Bool
```

The solver resolves that, so the application result has type `Bool`.

---

### Core output

```haskell
id =
  Λa. \(x : a) ->
    x

unit =
  id @Bool False
```

---

### Takeaway

This is the basic “make type arguments explicit” story:

- source polymorphism
- explicit type lambda
- explicit type application

That part still holds in OutsideIn(X).

---

## 2. Type class example: explicit dictionary argument

Now we add class constraints.

### Input

```haskell
eqSelf x = x == x
use = eqSelf True
```

Assume:

```haskell
(==) :: forall a. Eq a => a -> a -> Bool
```

---

### What the type checker does

#### For `eqSelf`

When it sees `(==)`, it instantiates the type:

```text
(==) : Eq α => α -> α -> Bool
```

Then applying it to `x` and `x` gives:

- `x : α`
- wanted constraint: `Eq α`

So it infers:

```haskell
eqSelf :: forall a. Eq a => a -> Bool
```

That constraint is not “just metadata”; it becomes an **extra argument** in the core.

#### For `use`

Instantiate `eqSelf` at `Bool`, producing a wanted constraint:

```text
Eq Bool
```

The solver finds the instance dictionary for `Eq Bool`.

---

### Core output

```haskell
eqSelf =
  Λa. \(dEq : Eq a) -> \(x : a) ->
    (==) @a dEq x x

use =
  eqSelf @Bool eqBoolDict True
```

Here `eqBoolDict` is the dictionary evidence for `Eq Bool`.

---

### Takeaway

With classes, elaboration is no longer just:

- explicit type abstraction/application

It also needs:

- **constraint abstraction/application**
- dictionary values as evidence

---

## 3. GADT example: local equality proof from pattern matching

Now we add a GADT.

### Input

```haskell
data T a where
  T1 :: Int -> T Bool
  T2 :: T a

isT1 :: T a -> Bool
isT1 v =
  case v of
    T1 n -> n > 0
    T2   -> True
```

---

### What the type checker does

The important part is the `T1` branch.

`T1` does not just tell us:

```text
n : Int
```

It also tells us that if the scrutinee has type `T a`, then in this branch:

```text
a ~ Bool
```

That equality is **local to the branch**. This is exactly the kind of local assumption that motivates OutsideIn(X): the paper introduces implication constraints to represent such branch-local reasoning.

So the type checker roughly does this:

1. Infer scrutinee type:

   ```text
   v : T a
   ```

2. For branch `T1 n -> ...`:
   - bind `n : Int`
   - add given equality:

     ```text
     a ~ Bool
     ```

3. Check `n > 0`
   - this is fine because `n : Int`

4. For branch `T2 -> True`
   - no extra equality needed

The overall result type is `Bool`.

---

### Core output

```haskell
isT1 =
  Λa. \(v : T a) ->
    case v of
      T1 @(a) (co : a ~ Bool) (n : Int) ->
        (>) @Int n 0

      T2 ->
        True
```

---

### What the extra `co` means

`co : a ~ Bool` is **proof/evidence** that inside the `T1` branch, `a` is equal to `Bool`.

In this particular example, the body does not need to cast anything with `co`, but the proof is still logically present.

---

## 4. GADT example where the equality proof is actually used

Let’s make the equality matter.

### Input

```haskell
data T a where
  T1 :: Int -> T Bool
  T2 :: T a

fromT :: T a -> a -> Bool
fromT v x =
  case v of
    T1 _ -> x
    T2   -> False
```

---

### What the type checker does

At top level, we are trying to check:

```text
fromT :: T a -> a -> Bool
```

In the `T1` branch we have the given equality:

```text
a ~ Bool
```

But `x` has type `a`, while the branch result must be `Bool`.

So the branch uses the equality proof to convert:

```text
x : a
```

into:

```text
x : Bool
```

This is where the equality evidence is operationally relevant.

---

### Core output

```haskell
fromT =
  Λa. \(v : T a) -> \(x : a) ->
    case v of
      T1 @(a) (co : a ~ Bool) (_ : Int) ->
        cast x by co

      T2 ->
        False
```

---

### Takeaway

GADTs do not just refine types “in the meta theory”.

They elaborate to explicit **equality evidence** that may be used in the program.

---

## 5. Type family example: normalize type family equality

Now let’s look at type families.

### Input

```haskell
type family F a
type instance F Int = Bool

negateF :: F Int -> Bool
negateF x = not x
```

Assume:

```haskell
not :: Bool -> Bool
```

---

### What the type checker does

The input says:

```text
x : F Int
```

But `not` expects:

```text
Bool
```

So the type checker generates the equality:

```text
F Int ~ Bool
```

The family axioms solve that equality.

Unlike ordinary unification, this is not “variable equals variable”; it comes from the type family reduction relation. OutsideIn(X) is designed to support such non-structural equalities in the constraint domain.

The solver produces evidence for that equality, which we can view as a coercion.

---

### Core output

```haskell
negateF =
  \(x : F Int) ->
    let co : F Int ~ Bool = axiom_F_Int in
    not (cast x by co)
```

Here `axiom_F_Int` stands for the coercion evidence arising from the type instance:

```haskell
type instance F Int = Bool
```

---

### Takeaway

Type families require:

- equality solving
- coercion evidence
- casts

This is beyond plain System F.

---

## 6. Type family example with polymorphism

Now combine family reduction with polymorphism.

### Input

```haskell
type family Elem c
type instance Elem [a] = a

headElem :: [a] -> Elem [a]
headElem xs = head xs
```

Assume:

```haskell
head :: forall a. [a] -> a
```

---

### What the type checker does

From `head xs`, we get result type `a`.

But the declared result is:

```text
Elem [a]
```

So the checker generates:

```text
a ~ Elem [a]
```

Using the family axiom:

```text
Elem [a] ~ a
```

it solves the equality.

---

### Core output

```haskell
headElem =
  Λa. \(xs : [a]) ->
    let co : a ~ Elem [a] = sym (axiom_Elem_List @a) in
    cast (head @a xs) by co
```

Depending on coercion direction, you might cast the other way around; the important point is that a coercion is used to reconcile the family-reduced type with the expected type.

---

## 7. Combined example: GADT + type class dictionary

This shows both branch-local givens and dictionary evidence.

### Input

```haskell
data Showable where
  MkShowable :: Show a => a -> Showable

display :: Showable -> String
display s =
  case s of
    MkShowable x -> show x
```

Assume:

```haskell
show :: forall a. Show a => a -> String
```

---

### What the type checker does

Pattern matching on `MkShowable` introduces:

- an existential type variable `a`

- a given dictionary:

  ```text
  dShow : Show a
  ```

- and a term binder:

  ```text
  x : a
  ```

Then `show x` generates a wanted:

```text
Show a
```

but that wanted is solved immediately from the given branch-local dictionary.

---

### Core output

```haskell
display =
  \(s : Showable) ->
    case s of
      MkShowable @a (dShow : Show a) (x : a) ->
        show @a dShow x
```

---

### Takeaway

Pattern matching on existential/GADT-like constructors may introduce:

- type binders
- dictionary binders
- equality binders

All of those become explicit in the core.

---

# Summary table

| Feature                | Surface meaning | Core mechanism                       |
| ---------------------- | --------------- | ------------------------------------ |
| Polymorphism           | `forall a`      | `Λa`, `@t`                           |
| Type class constraint  | `Eq a =>`       | dictionary abstraction/application   |
| GADT branch refinement | local equality  | coercion binder like `co : a ~ Bool` |
| Type family reduction  | `F a ~ τ`       | family axiom coercion + cast         |

---

# Big picture

A useful mental model is:

```haskell
source program
```

typechecks into something like:

```haskell
Λ type_vars.
\ dictionary_evidence.
\ value_args.
case ...
  with local coercions
  and local dictionaries
```

So the result of type checking in an OutsideIn(X)-style compiler is typically **not just a typed lambda term with explicit type arguments**. It is a term with explicit:

- type abstraction/application
- dictionary abstraction/application
- equality evidence / coercions
- casts where needed

That is the main shift from plain System F to a more evidence-carrying core, which is exactly the direction described by the OutsideIn(X) framework.
