# Shake+ - Super Powered Shake

Attempt at a batteries included Shake. We reexport replacements for the main
utility functions of Shake with the following adjustments whereever possible.

* Well-typed paths using the [path](https://hackage.haskell.org/package/path)
  library.
* New type classes `MonadAction`, `MonadUnliftAction` and `MonadRules` with
  concrete `ReaderT` transformers:
  * `RAction r a = RAction (ReaderT r Action a)` and
  * `ShakePlus r a = ShakePlus (ReaderT r Rules a)`
* `Text` instead of `String` wherever it is appropriate.

## Paths

Using the [path](https://hackage.haskell.org/package/path) library is kind of a
no brainer. I lose a lot of time to problems that could be avoided by using
this library, so it's everywhere. The names for these functions shadow the
existing names, so you may want to `import qualified Development.Shake` while
this library progresses if you have other `FilePath` based Shake rules that
you want to mix into your build.

The standard `Development.Shake.FilePath` functions for directory manipulation
are not re-exported in full, and you should use the functions in the path
library (such as `replaceExtension`) and other path-based libraries. This will
probably change.

`FilePattern`s are kept as-is, as `Path` is strongly normalizing it makes
sense to keep these as `Strings`.

## RAction

The `ReaderT r Action a` transformer (called `RAction`) is similar to the
[RIO](https://hackage.haskell.org/package/rio) type and should be used
similarly. In fact, you can reuse the logging functions from `RIO` within any
`RAction` block, which is one of the main motivators for having an `Action`
which is also a `MonadReader`. If you need to reuse an existing shake
`Action` in an `RAction`, use `liftAction`.

## runShakePlus

The main entry point to this library is the `runShakePlus` function, which
collapses a `ReaderT r Rules ()` to a `Rules ()` and passes the environment to
each underlying `RAction`. The `r`s in `ShakePlus` and the underlying
`RAction`s have to match. A typical setup might look like this.

```{.haskell}
let r = --setup env here
shake shakeOptions $ do

    -- include some regular shake rules.

    runShakePlus r $ do

      -- some shake-plus rules.
```
