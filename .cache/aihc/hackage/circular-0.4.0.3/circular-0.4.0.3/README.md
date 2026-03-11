
# Circular

<p align="center"><img src="https://travis-ci.org/dschrempf/circular.svg?branch=master"/></p>

Circular stacks of fixed size.

Circular stacks are just normal vectors with a pointer to the last element.

Circular stacks may not be what you need because:

-   You need all values at a later time anyways.
-   You don't want a monadic work flow, because circular stacks use mutable
    vectors.

Circular stacks are useful to you because:

-   They have a fixed size and consequently have constant memory usage. Constant
    memory usage is important if values are gathered continuously but only a
    specific number of values is needed at a later time.
-   They are fast, especially when summary statistics need to be computed across
    the stack.

Elements pushed on a circular stack replace the oldest (deepest) elements on the
stack. QuickCheck and unit tests with HSpec give promising results &#x2014; have a
look yourself.

I use circular stacks, for example, as the data type for traces of Markov
chains. In this case, lists cannot be used reliably, because the space
requirement increases linearly with the chain length.

`Circular` is actively developed and functions may be removed, renamed, or
changed. Ideas are welcome!

