
The low-level libraries available with GHC provide two completely different ways to execute tasks in parallel: [par](http://hackage.haskell.org/packages/archive/parallel/1.1.0.1/doc/html/Control-Parallel.html#v:par) and [forkIO](http://www.haskell.org/ghc/docs/latest/html/libraries/base-4.2.0.0/Control-Concurrent.html#v%3AforkIO). The former can be applied to any pure computation, while the latter works only in the IO monad.

The purpose of the _monad-parallel_ library package is to unify the two approaches. For this purpose, it defines classes of monads that are capable of:

 * splitting or forking monadic computations to be performed in parallel, and
 * combining the results of those computations when they complete.

The way these classes are implemented by the IO monad (as well as any MonadIO instance) is different from the way they're implemented by stateless monads, but the classes abstract these implementation details away.

## Class `MonadFork`

A monad that's an instance of the `MonadFork` class supports method

    forkExec :: MonadFork m => m a -> m (m a)

This function launches the argument computation in a parallel thread and returns a handle to it. The main thread is free to perform some other task and then obtain the result of the parallel computation:

    task1 :: MonadFork m => m Int
    task2 :: MonadFork m => m Int
    task3 :: MonadFork m => m Int

    example = do handle1 <- forkExec task1
                 handle2 <- forkExec task2
                 result3 <- task3
                 result1 <- handle1
                 result2 <- handle2
                 return (result1 + result2 + result3)

## Class `MonadParallel`

A monad that's an instance of the `MonadParallel` class supports method

    bindM2 :: MonadParallel m => (a -> b -> m c) -> m a -> m b -> m c

This method executes its `m a` and `m b` arguments in parallel. Once they're both finished, _bindM2_ runs their results through its first argument and returns the final result. Note that `bindM2` can be implemented using `forkExec`, but not vice versa.

Based on this foundation, the library defines and exports other useful functions. These replicate the functionality of the `Control.Monad` module, but they execute their monadic arguments in parallel.

    bindM3      :: MonadParallel m => (a -> b -> c -> m d) -> m a -> m b -> m c -> m d
    liftM2      :: MonadParallel m => (a -> b -> c) -> m a -> m b -> m c
    liftM3      :: MonadParallel m => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
    ap          :: MonadParallel m => m (a -> b) -> m a -> m b
    sequence    :: MonadParallel m => [m a] -> m [a]
    sequence_   :: MonadParallel m => [m a] -> m () 
    mapM        :: MonadParallel m => (a -> m b) -> [a] -> m [b]
    replicateM  :: MonadParallel m => Int -> m a -> m [a]
    replicateM_ :: MonadParallel m => Int -> m a -> m ()

## Links

 * Download and API documentation is available from [Hackage](http://hackage.haskell.org/package/monad-parallel)
 * Source code repository: <https://hub.darcs.net/blamario/SCC/>
