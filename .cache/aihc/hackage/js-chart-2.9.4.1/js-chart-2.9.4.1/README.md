# js-chart [![Hackage version](https://img.shields.io/hackage/v/js-chart.svg?label=Hackage)](https://hackage.haskell.org/package/js-chart) [![Stackage version](https://www.stackage.org/package/js-chart/badge/nightly?label=Stackage)](https://www.stackage.org/package/js-chart) [![Build status](https://img.shields.io/travis/jonascarpay/js-chart/master.svg?label=Build)](https://travis-ci.org/jonascarpay/js-chart)

This package bundles the minified [http://www.chartjs.org/](`chart.js`) code into a Haskell package, so it can be depended upon by Cabal packages.
The first three components of the version number match the upstream chart.js version.
The package is designed to meet the redistribution requirements of downstream users (e.g. Debian).
This package is a fork of [https://hackage.haskell.org/package/js-flot](js-flot) using `chart.js` instead of `flot`.

```haskell
import qualified Language.Javascript.Chart as Chart

main = do
    putStrLn $ "Chart version " ++ show Chart.version ++ " source:"
    putStrLn =<< readFile =<< Chart.file Chart.Chart
```

This package installs data files containing the `chart.js` sources, which must be available at runtime. If you want to produce an executable with no dependency on associated data files, you can use the [`file-embed`](https://hackage.haskell.org/package/file-embed) library:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed
import qualified Data.ByteString as BS
import qualified Language.Javascript.Chart as Chart
import Language.Haskell.TH.Syntax

main = print chartContents

chartContents :: BS.ByteString
chartContents = $(embedFile =<< runIO (Chart.file Chart.Chart))
```
