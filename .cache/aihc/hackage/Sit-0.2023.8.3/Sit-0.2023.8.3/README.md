# Sit: size-irrelevant types
A prototype dependently-typed language with sized natural numbers

Sit parses and typechecks `.agda` that conform to the Sit language syntax.

Syntax (excerpt):
```agda
--- Lexical stuff

--- Single line comment
{- Block comment -}
--;               --- End of declaration (mandatory)
f_x'1             --- identifiers start with a letter, then have letters, digits, _ and '

--- Declarations

x : T --;         --- type signature
x = t --;         --- definition
open import M --; --- ignored, for Agda compatibility

--- Sit specifics

oo                --- infinity size
i + 1             --- successor size

Nat a             --- type of natural numbers below size a
zero a            --- number zero (a is size annotation)
suc a n           --- successor of n (a is size annotation)

forall .i  -> T   --- irrelevant size quantification
forall ..i -> T   --- relevant size quantification

fix T t n         --- recursive function over natural numbers
                  ---   T: return type
                  ---   t: functional
                  ---   n: natural number argument

\{ (zero _) -> t; (suc _ x) -> u }   --- case distinction function

--- Inherited Agda syntax

U -> T            --- non-dependent function type
(x y z : U) -> T  --- dependent function type
\ x y z -> t      --- lambda-abstraction
t u               --- application

Set               --- first universe
Set1              --- second universe
Set a             --- universe of level a

```

## Limitations

Sit only understands a tiny subset of the Agda language.
Sit does not understand layout, instead each declaration has to be terminated with
comment `--;`.

## Installation

Requires GHC and cabal, for instance via the Haskell Platform.
In a shell, type
```
  cabal install
```

## Test

In a shell, type
```
  Sit.bin test/Test.agda
```

## Example

This is the addition function in Sit:
```
--- Addition of natural numbers

plus : forall .i -> Nat i -> Nat oo -> Nat oo   --;
plus = \ i x y ->
  fix (\ i x -> Nat oo)
      (\ _ f -> \
        { (zero _)  -> y
        ; (suc _ x) -> suc oo (f x)
        })
      x
```
