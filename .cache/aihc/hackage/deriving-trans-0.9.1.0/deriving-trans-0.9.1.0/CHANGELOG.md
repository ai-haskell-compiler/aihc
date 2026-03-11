# Revision history for deriving-trans

## 0.9.1.0 *12 Jul 2023*

* Support new optional dependency monad-logger `>= 0.3.38`.
* Add `MonadLogger` and `MonadLoggerIO` instances to `Elevator` and `ComposeT`.
* Add `MonadLogger` and `MonadLoggerIO` "base-case" instances for `LoggingT`, `NoLoggingT` and `WriterLoggingT`.

## 0.9.0.0 *05 Jul 2023*

* Drop support for `GHC < 9.6`.

## 0.8.1.0 *20 Feb 2023*

* Support new optional dependency logict `>= 0.8.0.0`.
* Add `MonadLogic` instances to `Elevator` and `ComposeT` including a `LogicT` "base-case" instance.
* Add `MonadMask` instances to `Elevator` and `ComposeT` including a `CatchT` "base-case" instance.
* Add `MonadPlus` "base-case" instances for `ExceptT`, `MaybeT`, `CatchT` and `LogicT`.

## 0.8.0.0 *31 Jan 2023*

* Update version boundaries of dependencies:
  - monad-control `>= 1.0.3`
  - monad-control-identity `>= 0.2.0.0`
  - transformers-base `>= 0.4.6`
  - primitive `>= 0.7.1.0`
  - unliftio-core `>= 0.2.0.0`
* Add common instances to `ComposeT`:
  - `Alternative` (recursive and "base-case")
  - `MonadFail` (recursive and "base-case")
  - `MonadFix` (recursive; didn't find any "base-case")
  - `MonadPlus` (determined by `Alternative`)
  - `MonadZip` (recursive; didn't find any "base-case")

## 0.7.0.0 *31 Jan 2023*

* Update required version bounds for `exceptions` to `>= 0.10.5`.
* Remove redundant `MonadIO` constraints from instances `StatefulGen`, `FrozenGen` and `RandomGenM`.
* Drop support for `GHC < 9.2`.

## 0.6.1.0 *27 Jan 2023*

* Add optional dependency `random >= 1.2`.
* Add `StatefulGen`, `FrozenGen` and `RandomGenM` instances to `Elevator` and `ComposeT`.
  There are no "base-case" instances because there are no related transformers, just base monads.

## 0.6.0.0 *23 Jan 2023*

* Update dependencies:
  - transformers `>= 0.6`
  - exceptions `>= 0.4` (optional)
  - mtl `>= 2.3` (optional)
  - primitive (optional)
  - resourcet `>= 1.2` (optional)
  - unliftio (optional)
* Add `MonadAccum` and `MonadSelect` instances to `Elevator` and `ComposeT`.
* Add "base-case" instances for `AccumT` and `SelectT`.
* Add "base-case" instances for the CPS versions of `WriterT` and `RWST`.
* Remove `Monad m` constraint from "base-case" instance for `ContT`.
* Add `MonadResource` instances to `Elevator` and `ComposeT` including a `ResourceT` "base-case" instance.

## 0.5.2.0 *17 Jan 2023*

* Add optional dependency `primitive`.
* Add `MonadPrim` instances to `Elevator` and `ComposeT`.

## 0.5.1.0 *11 Jan 2023*

* Introduce cabal flags `exceptions`, `mtl` and `unliftio`.
  These flags enable the dependency on additional optional packages, to reduce the requirement of orphan instances.
* Add `MonadUnliftIO` instances to `Elevator` and `ComposeT`.
* Add `MonadThrow` and `MonadCatch` instances to `Elevator` and `ComposeT`.
* Add `MonadThrow` and `MonadCatch` "base-case" instances for `CatchT` to `ComposeT`.

## 0.5.0.1 *04 Aug 2022*

* Improve Haddock examples.

## 0.5.0.0 *25 Jul 2022*

* Add module `Control.Monad.Trans.Compose.Infix`.
* Add module `Control.Monad.Trans.Compose.Stack`.
* Add module `Control.Monad.Trans.Compose.Transparent`.
* Improve and update Haddock documentation and examples.

## 0.4.0.0 *18 Jul 2022*

* Add dependency on monad-control-identity.
* Add instances for `MonadTransControlIdentity` and `MonadBaseControlIdentity`.
* Add `MonadZip` instance to `Elevator`.
* Constrain the `MonadFix` instance for `Elevator` with `MonadTransControlIdentity`.

## 0.3.2.0 *31 Mar 2022*

* Remove version boundaries of dependencies.

## 0.3.1.0 *08 Feb 2022*

* Add `MonadFix` instance to `Elevator`.

## 0.3.0.0 *04 Feb 2022*

* Add "base-case" instances for both lazy and strict transformers (from mtl) to `ComposeT`.
* Add `Alternative` and `MonadPlus` instances to `Elevator`.
* Add `MonadFail` instance to `Elevator`.
* Add `MonadCont` instances to `Elevator` and `ComposeT`.
* Add `MonadCont` "base-case" instance for `ContT` to `ComposeT`.
* Add `MonadRWS` instances to `Elevator` and `ComposeT`. It's not quite clear, whether this is necessary
* Add "base-case" instances for `RWST` to `ComposeT`.

## 0.2.2.1 *01 Feb 2022*

* Polish Haddock comments.

## 0.2.2.0 *30 Jan 2022*

* Add "base-case" instances for mtl's type classes to `ComposeT`.
* Add Haddock examples and improve comments.

## 0.2.1.0 *27 Jan 2022*

* Add `MonadTrans` and `MonadTransControl` instances to `Elevator`.
* Use StandaloneKindSignatures.
* Add Haddock documentation.

## 0.2.0.0 *25 Jan 2022*

* Completely change the idea of this project. Pretty much everything has been changed.

## 0.1.0.0 *19 Jun 2021*

* First version. Released on an unsuspecting world.
