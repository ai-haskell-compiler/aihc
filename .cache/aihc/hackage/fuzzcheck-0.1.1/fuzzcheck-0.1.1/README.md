# FuzzCheck

FuzzCheck is a library much like QuickCheck, except that instead of test the
properties of pure functions, it tests the behavior of applicative or monadic
code.

For example, with QuickCheck you would check a property of a function as
follows:

    prop_reverse xs = xs == reverse (reverse xs)
    
    >>> quickCheck prop_reverse

This would generate a list of random length and contents, and ensure that the
stated property is maintained for each instance.

FuzzCheck is for testing monadic (or applicative) code, which may only be
testable in the context of other operations.  For example, let's test some
simple FFI code:

    prop_bs_ffi = do
        mem <- "allocate buffer" ?> pure malloc
        n <- "pick a number"     ?> return <$> gen (choose (40::Int,100))
        "poke"                   ?> poke <$> arg mem <*> arg n
        x <- "peek at memory"    ?> peek <$> arg mem
        "make sure it matches"   ?> (@?=) <$> arg x <*> arg n
        "free the buffer"        ?> free <$> arg mem

## FuzzCheck interface

There are just three special details introduced by FuzzCheck, the `?>`
operator, and the `arg`, `rand` and `gen` combinators.

    "label" ?> action
    
This runs a `Fuzz` action.  If an exception occurs, the label is printed
along with the exception.

    let x = "Hello"
    "label" ?> f <$> arg x
    
This executes a *monadic* function `f`, passing it the argument `x`.  This is
equivalent to using `f x` in the surrounding monad, except that if an
exception is generated, the error report looks like this:

    f "Hello": <text of actual exception here>

You may also use `rand`, which is just a shorter synonym for QuickCheck's
`arbitrary`, for generating a type-appropriate random value automatically:

    "label" ?> f <$> rand

Another option is to use `gen`, which takes for its argument any combinator
from QuickCheck that generates an appropriately typed `Gen` value.  For
example:

    "label" ?> f <$> gen (choose (1,10))
    
This tests `f` by passing it a randomly chosen integer from the given range.
If an exception occurs, the actual integer that caused the problem is shown:

    f 9: <text of actual exception here>
    
That's it.  To run the test, call `fuzzCheck` on the property:

    >>> fuzzCheck prop_bs_ffi2
    +++ OK, passed 100 tests.
    
You can use `fuzzCheck'` if you want to change the number of tests executed,
or if you want to associate cleanup code with the test after it runs, whether
or not it succeeds.

## Simplifying tests

The role of `?>` is to assign a label to each operation (to assist with error
reporting in case of failure), and to execute the `Fuzz` action in its
enclosing Monad.  A fuzz test may occur within any monad supporting `MonadIO`
and `MonadBaseControl IO` (for the purpose of catching exceptions), which
means that if we're testing code in IO, we can limit the use of `?>` to only
those cases we expect might fail:

    prop_bs_ffi2 = do
        mem <- malloc
        n <- "pick a number"  ?> return <$> gen (choose (40::Int,100))
        "poke"                ?> poke <$> arg mem <*> arg n
        x <- "peek at memory" ?> peek <$> arg mem
        x @?= n
        free mem

**NOTE**: Using `gen` does not mean that that specific function is invoked 100
times at that point in the monadic block.  Instead, the entire block passed to
`fuzzCheck` is executed 100 times, with each occurence of `gen` producing a
new value at each run.

## Integration with Hspec and HUnit

This all integrates quite nicely with Hspec and Hunit.  For example, this is
from the smoke tests for this library:

    hspec $ it "works with an FFI example" $ fuzzCheck $ do
        mem <- malloc
        n <- "pick a number"  ?> return <$> gen (choose (40::Int,100))
        "poke"                ?> poke <$> arg mem <*> arg n
        x <- "peek at memory" ?> peek <$> arg mem
        x @?= n
        free mem

## See also

Although this library was written before I had found
[this paper](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.118.9528)
and `Test.QuickCheck.Monadic`, the results are somewhat similar (the paper
uses `name` instead of `?>`, while `arg` is the same).

    
