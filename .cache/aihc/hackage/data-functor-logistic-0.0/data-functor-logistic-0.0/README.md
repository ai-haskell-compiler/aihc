Logistic is to setters as Distributive is to getters
----

Distributive functors are containers where getters can be enumerated as their own types.

This is the definition of the `Distributive` class:

```haskell
class Functor g => Distributive g where
  distribute :: Functor f => f (g a) -> g (f a)
```

One easy-to-understand instance is `Complex`.

```haskell
data Complex a = !a :+ !a

realPart :: Complex a -> a
realPart (x :+ _) =  x

imagPart :: Complex a -> a
imagPart (_ :+ y) =  y

instance Distributive Complex where
  distribute wc = fmap realPart wc :+ fmap imagPart wc
```

Given any functor-wrapped value, `distribute` fmaps the getters of `Complex` to it.
`distribute id` instantiates it as the function (`(->) r`) functor. In this case, `distribute id` is equal to `realPart :+ imagPart`.

However, we cannot modify the elements this way because `distribute` passes getters but not setters.

Here we introduce a new `Logistic` class to provide settability to containers:

```haskell
class Functor t => Logistic t where
  deliver :: Contravariant f => f (t a -> t a) -> t (f (a -> a))
```

While the type of `deliver` is slightly more intimidating, it's actually very close to the `distribute`;
the `Functor` constraint is `Contravariant` instead and the contents are endomorphisms.

Here's the instance for `Complex`. `deliver f` contramaps a setter function to `f` for each field:

```haskell
instance Logistic Complex where
  deliver f
    = contramap (\g (a :+ b) -> g a :+ b) f
    :+ contramap (\g (a :+ b) -> a :+ g b) f
 ```

Instantiating the `Op` contravariant functor, it is trivial to obtain a collection of setters.

```haskell
newtype Op a b = Op { getOp :: b -> a }

setters :: Logistic t => t ((a -> a) -> t a -> t a)
setters = getOp <$> deliver (Op id)
```

```haskell
ghci> let setR :+ setI = setters
ghci> setR (+1) (0 :+ 1)
1 :+ 1
ghci> setI (+1) (0 :+ 1)
0 :+ 2
```

`deliver` has a generic default implementation which works for any single-constructor products.

This class can be useful to complement `Distributive`. Generalisation to higher-kinded data would also be interesting.
