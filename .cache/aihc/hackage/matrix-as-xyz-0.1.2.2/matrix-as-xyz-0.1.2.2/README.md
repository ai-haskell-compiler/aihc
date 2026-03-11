# matrix-as-xyz

[![Continuous Integration status][status-png]][status]
[![Hackage page (downloads and API reference)][hackage-png]][hackage]
[![Hackage-Deps][hackage-deps-png]][hackage-deps]

Haskell Jones-Faithful notation (also called coordinate triplet) Library

## Quickstart

```haskell
repl> :m Data.Matrix.AsXYZ
repl> fromXYZ "x,y,z"
┌                         ┐
│ 1 % 1 0 % 1 0 % 1 0 % 1 │
│ 0 % 1 1 % 1 0 % 1 0 % 1 │
│ 0 % 1 0 % 1 1 % 1 0 % 1 │
│ 0 % 1 0 % 1 0 % 1 1 % 1 │
└                         ┘
```

```haskell
repl> :m Data.Matrix Data.Matrix.AsXYZ
repl> prettyXYZ . identity $ 4
"x,y,z"
```

If you wanna use newest version, edit stack.yaml like below.
```
extra-deps:
- location: https://github.com/narumij/matrix-as-xyz/archive/0.1.2.1.tar.gz
```

## License

See the [LICENSE](https://raw.githubusercontent.com/narumij/matrix-as-xyz/master/LICENSE)
file in the repository.

 [hackage]: http://hackage.haskell.org/package/matrix-as-xyz
 [hackage-png]: http://img.shields.io/hackage/v/matrix-as-xyz.svg
 [hackage-deps]: http://packdeps.haskellers.com/reverse/matrix-as-xyz
 [hackage-deps-png]: https://img.shields.io/hackage-deps/v/matrix-as-xyz.svg

 [status]: http://travis-ci.org/narumij/matrix-as-xyz?branch=master
 [status-png]: https://api.travis-ci.org/narumij/matrix-as-xyz.svg?branch=master
