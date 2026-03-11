[gdf-hackage]: https://hackage.haskell.org/package/generic-data-functions

# generic-data-asserts
Structural assertions on the generic data representations (`GHC.Generic.Rep a`).

Sometimes, we want to write generics that only work on certain data types with a
certain shape e.g. non-sum types (single constructor). Achieving this is fairly
straightforward if we add a `TypeError` constraint on the relevant generic
representation unwrapping instance (here, the `(:+:)` constructor sum type).

This library effectively pulls those checks out of generic code and runs them by
separately. This way, we can simplify our generics, and make them more flexible
(e.g. a user may choose whether to permit void types at compile time or not).

This began as a minor feature in my [generic-data-functions][gdf-hackage]
library.

## License
Provided under the MIT license. See `LICENSE` for license text.
