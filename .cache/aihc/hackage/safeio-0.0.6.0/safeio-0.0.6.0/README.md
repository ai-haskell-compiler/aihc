# SafeIO: Haskell library for safe (atomic) IO

[![Hackage](https://img.shields.io/hackage/v/safeio.svg)](https://hackage.haskell.org/package/safeio)
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/safeio.svg)](http://packdeps.haskellers.com/feed?needle=safeio)
[![Stackage (LTS)](http://stackage.org/package/safeio/badge/lts)](http://stackage.org/lts/package/safeio)
[![Travis](https://api.travis-ci.com/luispedro/safeio.png)](https://travis-ci.com/luispedro/safeio)
Atomic IO

This is a simple module, which enables writing in atomic mode. It
implements the following 4 step procedure:

1. Open a temporary file in the same directory as the final output.
2. Write to this temporary file.
3. Close and sync the file.
4. Atomically rename the file to its final destination.


## Example

Direct use:

    import System.IO.SafeWrite
    ...
    main = do
        withOutputFile "output.txt" $ \hout -> do
            hPutStrLn hout "Hello World"

Through [conduit](https://www.stackage.org/package/conduit):

    import qualified Data.Conduit as C
    import           Data.Conduit ((.|))
    import           Data.Conduit.SafeWrite
    
    main = C.runConduitRes $
        C.yield "Hello World" .| safeSinkFile "hello.txt"

In any case, only successful termination of the process will result in the
output file being written. Early termination by throwing an exception will
cause the temporary file to be removed and no output will be produced.

## Author

Luis Pedro Coelho | [Email](mailto:luis@luispedro.org) | [Twitter](https://twitter.com/luispedrocoelho)
