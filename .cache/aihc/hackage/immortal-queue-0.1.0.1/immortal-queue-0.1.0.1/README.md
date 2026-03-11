# Immortal Queue

[![immortal-queue Build Status](https://travis-ci.org/prikhi/immortal-queue.svg?branch=master)](https://travis-ci.org/prikhi/immortal-queue)


A Haskell library for building a pool of queue-processing worker threads,
leveraging the [immortal][immortal] package.


## Usage

To use this library, build an `ImmortalQueue` value describing how to
manipulate and process your queue. Then you start start the pool using the
`processImmortalQueue` function and close or kill it with `closeImmortalQueue`
or `killImmortalQueue`.

For a simple example using a `TQueue`, please refer to the [haddock
documentation][hackage] for the module.

For a more complex example that uses a persistent database as a queue backend,
see the [Southern Exposure Seed Exchange's Workers module][sese-workers].


## Developing

You can build the project with stack:

```
stack build
```

For development, you can enable fast builds with file-watching,
documentation-building, & test-running:
```
stack test --haddock --fast --file-watch --pedantic
````

To build & open the documentation, run:

```
stack haddock --open immortal-queue
````


## LICENSE

BSD-3

The original code for this package was lifted from [Southern Exposure Seed
Exchange's website][sese].


[hackage]: https://hackage.haskell.org/package/immortal-queue/docs/Control-Immortal-Queue.html
[sese-workers]: https://github.com/Southern-Exposure-Seed-Exchange/southernexposure.com/blob/develop/server/src/Workers.hs
[immortal]: https://hackage.haskell.org/package/immortal
[sese]: https://github.com/Southern-Exposure-Seed-Exchange/southernexposure.com
