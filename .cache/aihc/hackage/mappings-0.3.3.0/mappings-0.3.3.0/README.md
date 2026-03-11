# Mappings

## What's it do?

This package does two jobs:

* It offers a general typeclass [`Mapping`](src/Data/Mapping.hs) for
  types which represent functions `k -> v` (for fixed `k`, but
  arbitrary ordered `v`).

  There are some fairly straightforward examples which build up
  mappings where `k` is `Either`, or a pair, or `Maybe`, or `Bool`.

* Three less trivial implementations are provided:

    * [Decision diagrams](src/Data/Mapping/Decision.hs), with nodes
       which may themselves be an arbitrary `Mapping` (there is some
       code for viewing these in the `examples` directory);

    * [Piecewise constant maps](src/Data/Mapping/Piecewise.hs) on an
       ordered domain `k`;

    * [Maps equipped with a default value](src/Data/Mapping/MapWithDefault.hs).


## Why did I bother?

The aim is to use decision diagrams with nodes that are piecewise
constant maps to store monomials for Grobner basis algorithms.

