# typecheck-plugin-nat-simple

## what's this

This package provide plugin which extend type checking of Nat.
The type checker can calculate only addition, subtraction and less or equal of
constants and variables.

(View sample code on directory sample/.)

## motivation

Suppose you need lengthed list. You define like following.
(View `sample/lengthed_tail.hs`)

```haskell
import GHC.TypeNats

infixr 6 :.

data List :: Nat -> * -> * where
        Nil :: List 0 a
	(:.) :: a -> List ln a -> List (ln + 1) a
```

And you want to define function `tail`.

```haskell
tail_ :: List (n + 1) a -> List n a
tail_ Nil = error "tail_: Nil"
tail_ (_ :. xs) = xs
```

But it cause type check error.

```
error:
  ・Could not deduce: ln ~ n
    from the context: (n + 1) ~ (ln + 1)
    ...
```

Type checker say "I cannot derive (ln == n) from (n + 1 == ln + 1)".
But it seems to be obvious.
You can use this plugin like following.

```haskell
{-# OPTIONS_GHC -fplugin=Plugin.TypeCheck.Nat.Simple #-}
```

Add it to top of the code, then type check succeed.

## more example

To show more example, I will use Data.Proxy.Proxy.
First examle is following (View `sample/mplus1_nplus1.hs`).

```haskell
foo :: (m + 1) ~ (n + 1) => Proxy m -> Proxy n
foo = id
```

If you don't use this plugin, then following error occur.

```
  ・Could not deduce: m ~ n
    from the context: (m + 1) ~ (n + 1)
    ...
```

Use this plugin, you can compile it.

Second example is following (View `sample/two_equations.hs`).

```haskell
foo :: ((x + y) ~ z, (w - y) ~ v) => Proxy (x + w) -> Proxy (z + v)
foo = id
```

Without this plugin, following error occur.

```
  ・Could not deduce: (x + w) ~ (z + v)
    from the context: ((x + y) ~ z, (w - y) ~ v)
    ...
```

Use this plugin, you can compile it.

## error and recovery

### type check error

See following code.

```haskell
foo :: Proxy (n - 1 + 1) -> Proxy n
foo = id
```

This cause type check error even if you use this plugin.

```
  ・Couldn't match type `n' with `(n - 1) + 1'
  ...
```

### research

What's wrong?
You can see type check log of this plugin like following.

```
% stack ghc sample/minus1plus1.hs -- -ddump-tc-trace 2>&1 | grep -A 20 'Plugin.TypeCheck.Nat.Simple'
...
givens ([Exp v 'Boolean]): given (Givens v): (Givens [])
exBool: not boolean: (n_a1s2[sk:1] - 1) + 1
wanted (Exp v 'Boolean): ((((Var n_a1s2[sk:1]) :- (Const 1)) :+ (Const 1)) :== (Var n_a1s2[sk:1]))
wanted (Wanted v): (Wanted [(0 == 0), (1 * n_a1s2[sk:1] >= 0), (-1 + 1 * n_a1s2[sk:1] >= 0), (1 * n_a1s2[sk:1] >= 0)])
canDerive1: (0 == 0) is self-contained
canDerive1: (1 * n_a1s2[sk:1] >= 0) is self-contained
canDerive1: (-1 + 1 * n_a1s2[sk:1] >= 0) cannot be derived from
canDerive1: (1 * n_a1s2[sk:1] >= 0) is self-contained
result: type checker: return False
...
```

See the line of `canDerive1: (- 1 + 1 * n_a1s2[sk:1] >= 0) cannot be derived from`.
It mean "`-1 + n_a1s2[sk:1]` should be greater or equal than 0. But no such context".

### try calculation more portably

You can try to caluculate more simply.

```
% stack ghci
> :set -XTypeApplications
> :module Data.Derivation.Parse Data.Derivation.CanDerive Control.Monad.Try Data.Maybe
> parseConstraint "n - 1 + 1 == n"
Just ((:==) ((:+) ((:=) (Var "n") (Const 1)) (Const 1)) (Var "n"))
> Just w = wanted @String <$> it
> gs = givens @String []
> fst . runTry $ uncurry canDerive =<< (,) <$> gs <*> w
Right False
```

The wanted constraint cannot be derived from empty given constraint.
Let's add `1 <= n` constraint.

```
> gs = given @String . maybeToList $ parseConstraint "1 <= n"
> fst . runTry $ uncurry canDerive =<< (,) <$> gs <*> w
Right True
```

OK! It succeed if you add `1 <= n` to given constraint.

### recovery

Let's add `1 <= n` context like following.

```haskell
foo :: 1 <= n => Proxy (n - 1 + 1) -> Proxy n
foo = id
```

Then it succeed to type check.
