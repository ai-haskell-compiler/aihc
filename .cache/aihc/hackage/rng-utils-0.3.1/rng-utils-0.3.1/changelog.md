0.3.1
* Update test suite to work with newer library versions

0.3.0
* Switch to IORef instead of MVar. This should provide better performance under high contention.
* Swap out internal RNG for System.Random. It turns out that the gains to be had by using mwc-random are outshadowed by the mutation inherent to this library, so it is preferable to leverage System.Random which appears to be more commonly used and has better coverage of types to generate.
* Add rngRIO and rngIO convenience functions for generating values.
