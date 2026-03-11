# Change log for the `probability` package

## 0.2.8

 * `Numeric.Probability.Either` -> `Control.Monad.Trans.Except`
   Few functions exposed `EitherT` in their signatures,
   but `Numeric.Probability.Either` was private.
   Thus its removal should not be noticed by library users.


## 0.2.7

 * support `random-1.2`


## 0.2.6

 * `instance Monad Distribution.T`:
   Remove definition of `fail`.
   This turns calls to `fail` into `error`s
   for GHCs prior to the "Monad Fail Proposal".
   Formerly it was an empty list,
   but this was bad since the probabilities in an empty list
   sum up to zero not one, thus breaking the invariant.
   Beginning with GHC-8.8 and the "Monad Fail Proposal"
   you can no longer accidentally call `fail`,
   since `Distribution.T` is not an instance of `MonadFail`.

 * `instance Monad Probability.EitherT`:
   Define `MonadFail` instance for GHC>=8.8.1.
