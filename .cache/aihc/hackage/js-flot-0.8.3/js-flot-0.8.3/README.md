# js-flot [![Hackage version](https://img.shields.io/hackage/v/js-flot.svg?style=flat)](http://hackage.haskell.org/package/js-flot) [![Build Status](http://img.shields.io/travis/ndmitchell/js-flot.svg?style=flat)](https://travis-ci.org/ndmitchell/js-flot)

This package bundles the minified [Flot](http://www.flotcharts.org/) code
(a jQuery plotting library) into a Haskell package,
so it can be depended upon by Cabal packages. The first three components of
the version number match the upstream Flot version. The package is designed
to meet the redistribution requirements of downstream users (e.g. Debian).
As an example:

```haskell
import qualified Language.Javascript.Flot as Flot

main = do
    putStrLn $ "Flot version " ++ show Flot.version ++ " source:"
    putStrLn =<< readFile =<< Flot.file Flot.Flot
```

Note that Flot depends on jQuery, a Haskell version of which can be obtained
from the [`js-jquery` package](https://github.com/ndmitchell/js-jquery#readme).
