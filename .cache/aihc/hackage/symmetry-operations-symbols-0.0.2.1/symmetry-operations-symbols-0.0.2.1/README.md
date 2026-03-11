# symmetry-operations-symbols

[![Continuous Integration status][status-png]][status]
[![Hackage page (downloads and API reference)][hackage-png]][hackage]
[![Hackage-Deps][hackage-deps-png]][hackage-deps]

Haskell Derivation of symbols and coordinate triplets Library

## Quickstart

Make new stack project and move to project directory.

```shell
% stack new symopRepl
% cd symopRepl
```

Edit extra-deps part of stack.yaml like below.

```
extra-deps:
- matrix-as-xyz-0.1.2.2
- symmetry-operations-symbols-0.0.2.1
```

Edit dependencies part of package.yaml like below.

```
dependencies:
- base >= 4.8 && < 5
- matrix-as-xyz >= 0.1.2 && < 2
- symmetry-operations-symbols >= 0.0 && < 0.1
```

Then start repl.

```shell
% stack repl
```

Setup packages and load modules.

```haskell
repl> :m Data.Matrix.AsXYZ Data.Matrix.SymmetryOperationsSymbols
```

Use like below.

```haskell
repl> fromMatrix' . fromXYZ $ "x,y,z"
" 1 "

repl> fromMatrix' . fromXYZ $ "-x,-y,z"
" 2  0,0,z"

repl> fromMatrix' . fromXYZ $ "-y,-x+1/2,z"
" g (-1/4,1/4,0) x+1/4,-x,z"
```

## References

1. W. Fischer. and E. Koch. (2006), Derivation of symbols and coordinate triplets International Tables for Crystallography (2006). Vol. A, Chapter 11.2, pp. 812–816.

2. Wondratschek, H. & Neubu ̈ser, J. (1967). Determination of the symmetry elements of a space group from the ‘general positions’ listed in International Tables for X-ray Crystallography, Vol. I. Acta Cryst. 23, 349–352.

## License

See the [LICENSE](https://raw.githubusercontent.com/narumij/symmetry-operations-symbols/master/LICENSE)
file in the repository.

 [hackage]: http://hackage.haskell.org/package/symmetry-operations-symbols
 [hackage-png]: http://img.shields.io/hackage/v/symmetry-operations-symbols.svg
 [hackage-deps]: http://packdeps.haskellers.com/reverse/symmetry-operations-symbols
 [hackage-deps-png]: https://img.shields.io/hackage-deps/v/symmetry-operations-symbols.svg

 [status]: http://travis-ci.org/narumij/symmetry-operations-symbols?branch=master
 [status-png]: https://api.travis-ci.org/narumij/symmetry-operations-symbols.svg?branch=master
