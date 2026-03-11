# simple-cabal

[![Hackage](https://img.shields.io/hackage/v/simple-cabal.svg)](https://hackage.haskell.org/package/simple-cabal)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/simple-cabal/badge/lts)](http://stackage.org/lts/package/simple-cabal)
[![Stackage Nightly](http://stackage.org/package/simple-cabal/badge/nightly)](http://stackage.org/nightly/package/simple-cabal)

A small compatibility wrapper over Cabal.

```
pkgid <- getPackageId
putStrLn $ showPkgId pkgid
```

```
cabalfile <- findCabalFile
genPkgDesc <- readGenericPackageDescription normal cabalfile
```
