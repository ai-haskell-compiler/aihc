# Nondeterminism

[![Build Status](http://circleci-badges-max.herokuapp.com/img/abarbu/nondeterminism-haskell/master?token=345e81d04c8b8609c50124632f8d1b594f476cb5)](https://circleci.com/gh/abarbu/nondeterminism-haskell/tree/master)

This package is available via [Hackage where its documentation resides](https://hackage.haskell.org/package/nondeterminism).

This provides nondeterministic computations in Haskell. It implements
an `Amb` monad in which you can perform nondeterministic choices along
with a monad transformer version, `AmbT`.

## Amb

An example which finds Pythagorean triplets up to a certain size, project Euler problem 9.

```haskell
import Control.Monad
import Control.Monad.Amb
pyTriple :: (Num t, Ord t) => t -> Amb r (t, t, t)
pyTriple n = do a <- anIntegerBetween 1 n
                b <- anIntegerBetween (a + 1) n
                c <- anIntegerBetween (b + 1) n
                when (a*a + b*b /= c*c) empty
                return (a,b,c)
length $ allValues $ pyTriple 100
```

More examples can be found in `tests/test.hs`.

## Future

 - allValues is not lazy in its return value
