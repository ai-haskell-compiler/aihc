# js-dgtable [![Hackage version](https://img.shields.io/hackage/v/js-dgtable.svg?label=Hackage)](https://hackage.haskell.org/package/js-dgtable) [![Stackage version](https://www.stackage.org/package/js-dgtable/badge/nightly?label=Stackage)](https://www.stackage.org/package/js-dgtable) [![Build Status](https://img.shields.io/travis/ndmitchell/js-dgtable/master.svg)](https://travis-ci.org/ndmitchell/js-dgtable)

This package bundles the minified [jquery.dgtable](https://github.com/danielgindi/jquery.dgtable) code into a Haskell package,
so it can be depended upon by Cabal packages. The first three components of
the version number match the upstream jQuery version. The package is designed
to meet the redistribution requirements of downstream users (e.g. Debian).
As an example:

```haskell
import qualified Language.Javascript.DGTable as DGTable

main = do
    putStrLn $ "jquery.dgtable version " ++ show DGTable.version ++ " source:"
    putStrLn =<< readFile =<< DGTable.file
```

This package installs data files containing the jquery.dgtable sources, which must be available at runtime. If you want to produce an executable with no dependency on associated data files, you can use the [`file-embed`](https://hackage.haskell.org/package/file-embed) library:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed
import qualified Data.ByteString as BS
import qualified Language.Javascript.DGTable as DGTable
import Language.Haskell.TH.Syntax

main = print dgTableContents

dgTableContents :: BS.ByteString
dgTableContents = $(embedFile =<< runIO DGTable.file)
```
