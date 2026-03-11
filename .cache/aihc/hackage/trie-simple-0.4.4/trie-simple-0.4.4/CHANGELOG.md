# 0.4.4

- ([#7](https://github.com/viercc/trie-simple/issues/17))
  Adds `Data.Trie.Map.lookupPrefixes` function

# 0.4.3

- No changes to the library itself
- Migrates the benchmark framework from `gauge` to `tasty-bench`

# 0.4.2

- Compatibility changes

  - Drops GHC older than 8.10 ("base-4.14")

- API changes

  - Addition of `Data.Trie.Map.fromListWith` and `Data.Trie.Map.fromAscListWith`
  - The behavior of `Data.Trie.Map.appendWith` is fully specified now.

- Additional instances

  - `Eq1, Eq2, Ord1, Ord2, Show1, Show2` from "base"
  - `IsList` from "base" (for `OverloadedLists` GHC extension)
  - `Hashable, Hashable1, Hashable2` from "hashable"
  - `FunctorWithIndex, FoldableWithIndex, TraversableWithIndex` from "indexed-traversable"
  - `Filterable(WithIndex), Witherable(WithIndex)` from "witherable"
  - `Semialign, Align, Zip` from "semialign"
  - `Matchable` from "matchable"

# 0.4.1.1

- Initial release.

