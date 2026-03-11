Type-indexed maps [![Hackage](https://img.shields.io/hackage/v/type-map.svg)](https://hackage.haskell.org/package/type-map) [![Build Status](https://travis-ci.org/Lysxia/type-map.svg)](https://travis-ci.org/Lysxia/type-map)
=================

Maps whose keys are types.

This package includes:

- a dynamic type map using GHC's `Typeable` class,
  with a `Proxy`-based API (`Data.TypeMap.Dynamic`)
  or a `TypeApplications`-based API (`Data.TypeMap.Dynamic.Alt`);
  Supports arbitrary, user-defined mappings between keys and
  types of values via defunctionalization.

- a static type map, whose type is indexed by its list of keys
  (there are actually multiple implementations with different underlying
  representations (`[]`, `Map`, `Vector`)).

[Example using the dynamically-typed
interface](https://github.com/Lysxia/type-map/tree/master/examples/dynamic.hs).

See also [this package's description on Hackage](https://hackage.haskell.org/package/type-map).

Related
-------

- [typerep-map](https://hackage.haskell.org/package/typerep-map),
  a more performant dynamic type map.

Internal module policy
----------------------

Modules under `Data.TypeMap.Internal` are not subject to any versioning policy.
Breaking changes may apply to them at any time.

If something in those modules seems useful, please report it or create a pull
request to export it from an external module.
