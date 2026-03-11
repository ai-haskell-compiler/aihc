# co-log-polysemy

![Co-logo](https://user-images.githubusercontent.com/8126674/80955687-92f21a80-8df7-11ea-90d3-422dafdc8391.png)

[![GitHub CI](https://github.com/co-log/co-log-polysemy/workflows/CI/badge.svg)](https://github.com/co-log/co-log-polysemy/actions)
[![Hackage][hk-img-ps]][hk-ps]
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/co-log/co-log/blob/main/LICENSE)

`co-log-polysemy` package is a implementation of the 
[co-log-core](http://hackage.haskell.org/package/co-log-core) logging based on 
the [polysemy](http://hackage.haskell.org/package/polysemy) extensible 
effects library.


## How to use

`co-log-polysemy` is compatible with the following GHC
versions - [supported versions](https://matrix.hackage.haskell.org/#/package/co-log-polysemy)

In order to start using `co-log-polysemy` in your project, you
will need to set it up with these easy steps:

1. Add the dependency on `co-log-polysemy` in your project's
   `.cabal` file. For this, you should modify the `build-depends`
   section according to the below section:

   ```haskell
   build-depends: base ^>= LATEST_SUPPORTED_BASE
                , co-log-core ^>= LATEST_VERSION
                , polysemy ^>= LATEST_VERSION
   ```

2. To use this package, refer to the below example 

   ```haskell
   module Main (main) where

   import Prelude hiding (log)

   import Polysemy (Member, Sem, runM)

   import Colog.Core.IO (logStringStdout)
   import Colog.Polysemy (Log, log, runLogAction)


   example :: Member (Log String) r => Sem r ()
   example = do
        log @String "First message..."
        log @String "Second message..."

   main :: IO ()
   main = runM $ runLogAction @IO logStringStdout example
   ```


[hk-img-ps]: https://img.shields.io/hackage/v/co-log-polysemy.svg?logo=haskell
[hk-ps]: https://hackage.haskell.org/package/co-log-polysemy
