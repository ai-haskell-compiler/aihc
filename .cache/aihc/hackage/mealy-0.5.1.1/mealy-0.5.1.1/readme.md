# mealy

[![Hackage](https://img.shields.io/hackage/v/mealy.svg)](https://hackage.haskell.org/package/mealy) [![Build Status](https://github.com/tonyday567/mealy/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/tonyday567/mealy/actions/workflows/haskell-ci.yml)

A 'Mealy' is a triple of functions

- (a -> b) **inject**: Convert (initial) input into the (initial) state type.
- (b -> a -> b) **step**: Update state given prior state and (new) input.
- (c -> b) **extract**: Convert state to the output type.

A sum, for example, looks like `M id (+) id` where the first id is the initial injection and the second id is the covariant extraction.

This library provides support for computing statistics (such as an average or a standard deviation) as current state within a mealy context.

## Usage

Usage is to supply a decay function representing the relative weights of recent values versus older ones, in the manner of exponentially-weighted averages. The library attempts to be polymorphic in the statistic which can be combined in applicative style.

```haskell
import Prelude
import Data.Mealy
```

```haskell
fold ((,) <$> ma 0.9 <*> std 0.9) [1..100::Double]
```

```
(91.00265621044142,9.472822805289121)
```

## Reference

[Finite State Transducers](https://stackoverflow.com/questions/27997155/finite-state-transducers-in-haskell)
