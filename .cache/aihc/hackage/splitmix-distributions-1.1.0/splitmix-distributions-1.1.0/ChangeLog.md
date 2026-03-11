1.1

- add sampleRunT and samplesRunT: both return the final state of the PRNG as a tuple of Word64
- add unit test that asserts that sample runs are deterministic given the same starting PRNG state
- 

1.0

- add MonadReader r (GenT m) instance
- fixed MonadState (GenT m) instance via MonadTrans (i.e. as done in 'mtl')

0.9 :

- ensure it builds with ghc 8.6.5 (== stackage lts 14.27) as well
- relax lower bound

0.8 :

- add `exceptions` dependency
- add MonadThrow (GenT m) instance

0.7 :

- add sampleIO, samplesIO
- clarify pure vs IO-based sampling in the docs

0.6 :

- add Chinese Restaurant Process

0.5 :

- add Log-normal, Laplace, Weibull distributions

0.4 :

- add Discrete, Categorical, Zipf-Mandelbrot distributions

0.3 :

- type signatures of all generators are now parametrized over some Monad m rather than Identity. This allows for more flexibility on the use site.

0.2 :

- add Pareto, Dirichlet, multinomial distributions

