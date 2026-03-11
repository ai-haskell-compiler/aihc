`Essentials` is a small `Prelude` alternative. It was born out of the experience
of maintaining a number of libraries that don't use a prelude at all, preferring
instead to use tightly limited explicit imports. When we do this, we find that
although the standard prelude contains many things that most modules can live
without, there is a handful of items that most code truly needs.

There are plenty of good reasons to use names that conflict with things in the
standard prelude. For example, the standard prelude has a `takeWhile` function
that deals with lists; a streaming library might define a function of the same
name that deals with effectful streams. The standard prelude has a `log`
function that is an abbreviation for 'logarithm'; a logging library might define
a function of the same name that writes an event to a log file. Some names,
however, are more sacrosanct. It would be generally unwise and unappreciated to
define anything named `pure` or `(>>=)`, for example. The guiding principle for
the `Essentials` module is that it includes things in the latter category.

## Function

```haskell
($) :: (a -> b) -> a -> b
(&) :: a -> (a -> b) -> b
```

`($)` and `(&)` are the same function, just flipped.
`(&)` has slightly higher operator precedence.

## Category

```haskell
id :: Category cat => cat a a
(>>>) :: Category cat => cat a b -> cat b c -> cat a c
(<<<), (.) :: Category cat => cat b c -> cat a b -> cat a c
```

Usually specialized as `cat ~ (->)`:

```haskell
id :: a -> a
(>>>) :: (a -> b) -> (b -> c) -> a -> c
(<<<), (.) :: (b -> c) -> (a -> b) -> a -> c
```

`(>>>)` and `(<<<)` are the same function, just flipped.

`(.)` is the same as `(<<<)`, but with higher operator precedence.

## Functor, Applicative, Monad

```haskell
pure :: Applicative f => a -> f a
```

```haskell
fmap, (<$>) :: Functor f => (a -> b) -> f a -> f b
(<&>) :: Functor f => f a -> (a -> b) -> f b
```

`(<$>)` is the same as `fmap`.
`(<$>)` and `(<&>)` are also the same function, just flipped.
`(<$>)` has higher operator precedence.

```haskell
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
(<**>) :: Applicative f => f a -> f (a -> b) -> f b
```

`(<*>)` and `(<**>)` are the same function, just flipped.

```haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
(=<<) :: Monad m => (a -> m b) -> m a -> m b

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
```

`(>>=)` is left associative; `(=<<)` is right associative.

These functions keep all effects but discard some values:

```haskell
(<$) :: Functor f => a -> f b -> f a
(<*) :: Applicative f => f a -> f b -> f a

($>) :: Functor f => f a -> b -> f b
(*>) :: Applicative f => f a -> f b -> f b

void :: Functor f => f a -> f ()
```

## Boole

```haskell
data Bool = False | True

otherwise = True
```

## Comparison

```haskell
(==), (/=) :: Eq a => a -> a -> Bool

(<), (>), (<=), (>=) :: Ord a => a -> a -> Bool
```

## Monoid

```haskell
(<>) :: Semigroup a => a -> a -> a
mempty :: Monoid a => a
```

## Traversal

```haskell
traverse :: Traversable t => Applicative f =>
    (a -> f b) -> t a -> f (t b)

traverse_ :: Foldable t => Applicative f =>
    (a -> f b) -> t a -> f ()
```

## Maybe

```haskell
data Maybe a =
    Nothing | Just a
```

```haskell
maybe :: b -> (a -> b) -> Maybe a -> b
```

## Void

```haskell
data Void
```

```haskell
absurd :: Void -> a
```

## Identity

```haskell
newtype Identity a =
    Identity {runIdentity :: a}
```

## Const

```haskell
newtype Const a b =
    Const {getConst :: a}
```

## Type classes

* `Semigroup`
* `Monoid`
* `Eq`
* `Ord`
* `Enum`
* `Bounded`
* `Show`

## Constructor classes

* `Functor`
* `Applicative`
* `Monad`
* `Foldable`
* `Traversable`

## Type

* `Type`

## Undefined

```haskell
undefined :: a
```

## Fixities

```haskell
infixr 0  $

infixl 1  &  <&>  >>=

infixr 1  =<<  <=<  >=>
               <<<  >>>

infixl 4  <$>  <$  $>
          <*>  *>  <*  <**>

infix  4  ==  /=
          <   >
          <=  >=

infixr 6  <>

infixr 9  .
```
