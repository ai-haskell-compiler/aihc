# wai-middleware-clacks

[![wai-middleware-clacks Build Status](https://travis-ci.org/prikhi/wai-middleware-clacks.svg?branch=master)](https://travis-ci.org/prikhi/wai-middleware-clacks)

*"You know they'll never really die while the Trunk is alive."*

`wai-middleware-clacks` is a middleware that inserts an arbitrary
`X-Clacks-Overhead` header into every Wai response. From
[GNU Terry Pratchett](http://gnuterrypratchett.com):

> In Terry Pratchett's Discworld series, the clacks are a series of semaphore
> towers loosely based on the concept of the telegraph. Invented by an
> artificer named Robert Dearheart, the towers could send messages "at the
> speed of light" using standardized codes. Three of these codes are of
> particular import:
>
> * G: send the message on
> * N: do not log the message
> * U: turn the message around at the end of the line and send it back again
>
> When Dearheart's son John died due to an accident while working on a clacks
> tower, Dearheart inserted John's name into the overhead of the clacks with a
> "GNU" in front of it as a way to memorialize his son forever (or for at least
> as long as the clacks are standing.)
>
> Keeping the legacy of Sir Terry Pratchett alive forever. For as long as his
> name is still passed along the Clacks, Death can't have him.


## Usage

To use this package to keep the legacy of Terry Pratchett alive, simply pass
your wai application to `clacks gnuTerryPratchett` before passing it to the
`run` function:

```haskell
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Clacks (clacks, gnuTerryPratchett)

import MyLib.App (myApp)

main :: IO ()
main = run 8080 $ clacks gnuTerryPratchett myApp
```

You can use the `Clacks` type to build a custom configuration for the `clacks`
function, allowing you to pass anything into the header:

```haskell
import Data.List.NonEmpty (NonEmpty(..))
import Network.Wai (Middleware)
import Network.Wai.Middleware.Clacks (Clacks(..), clacks)

myClacks :: Middleware
myClacks = clacks $ Clacks $ "GNU Ada Lovelace" :| ["GNU Hoban Washburne", "GNU Shephard Book"]
```


## Build

You can build the project with stack:

```
stack build
```

For development, you can enable fast builds with file-watching,
documentation-building, & test-running:

```
stack test --haddock --fast --file-watch --pedantic
```

To build & open the documentation, run

```
stack haddock --open wai-middleware-clacks
```


## LICENSE

BSD-3
