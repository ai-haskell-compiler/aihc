
The `monad-coroutine` library, implemented by the `Control.Monad.Coroutine` module, provides a limited coroutine
functionality in Haskell. The centerpiece of the approach is the monad transformer `Coroutine`, which transforms an
arbitrary monadic computation into a suspendable and resumable one. The basic definition is simple:

    newtype Coroutine s m r = Coroutine {resume :: m (Either (s (Coroutine s m r)) r)}

    instance (Functor s, Monad m) => Monad (Coroutine s m) where
      return = Coroutine . return . Right
      t >>= f = Coroutine (resume t >>= either (return . Left . fmap (>>= f)) (resume . f))

## Suspension Functors

The Coroutine transformer type is parameterized by a functor. The functor in question wraps the resumption of a suspended coroutine, and it can carry other information as well. Module `Control.Monad.Coroutine.SuspensionFunctors` exports some useful functors, one of which is `Yield`:

    data Yield x y = Yield x y
    instance Functor (Yield x) where
      fmap f (Yield x y) = Yield x (f y)

A coroutine parameterized by this functor is a generator which yields a value every time it suspends. For example, the following function generates the program's command-line arguments:

    genArgs :: Coroutine (Yield String) IO ()
    genArgs = getArgs >>= mapM_ yield

The `Await` functor is dual to `Yield`; a coroutine that suspends using this functor is a consumer coroutine that on every suspension expects to be given a value before it resumes. The following example is a consumer coroutine that prints every received value to standard output:

    printer :: Show x => Coroutine (Await x) IO ()
    printer = await >>= print >> printer           

While these two are the most obvious suspension functors, any functor whatsoever can be used as a coroutine suspension functor.

## Running a coroutine

After a coroutine suspends, the suspension functor must be unpacked to get to the coroutine resumption. Here's an example of how the `printer` example could be run:

    printerFeeder :: Show x => [x] -> Coroutine (Await x) IO () -> IO ()
    printerFeeder [] _ = return ()
    printerFeeder (head:tail) printer = do p <- resume printer
                                           case p of Left (Await p') -> printerFeeder tail (p' head)
                                                     Right result -> return result

Alternatively, you can use the function `pogoStick` or `foldRun` to the same effect:

    printerFeeder feed printer = liftM snd $ foldRun f feed printer
      where f (head:tail) (Await p) = (tail, p head)
            f []          _         = ([], return ())


## Links

 * Download and API documentation is available at [Hackage](http://hackage.haskell.org/package/monad-coroutine)
 * The current source code is in a Darcs repository at <https://hub.darcs.net/blamario/SCC/>
