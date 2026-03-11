# ChangeLog / ReleaseNotes


## Version 0.11.0.0

* New function application combinators (**new**):
    * `inbetween :: a -> b -> (a -> b -> r) -> r`
    * `(~$~) :: a -> b -> (a -> b -> r) -> r`
    * `(~$$~) :: b -> a -> (a -> b -> r) -> r`
    * `withIn :: ((a -> b -> r) -> r) -> (a -> b -> r) -> r`
    * `withReIn :: ((b -> a -> r) -> r) -> (a -> b -> r) -> r`
* Precursors to `Iso`, `Lens` and `Prism` (**new**):
    * `type PreIso r s t a b = ((b -> t) -> (s -> a) -> r) -> r`
    * `type PreIso' r s a = PreIso r s s a a`
    * `type PreLens r s t a b = ((b -> s -> t) -> (s -> a) -> r) -> r`
    * `type PreLens' r s a = PreLens r s s a a`
    * `type PrePrism r s t a b = ((b -> t) -> (s -> Either t a) -> r) -> r`
    * `type PrePrism' r s a = PrePrism r s s a a`
* Uploaded to Hackage: http://hackage.haskell.org/package/between-0.11.0.0


## Version 0.10.0.0

* Original implementation moved to module `Data.Function.Between.Lazy` and is
  now reexported by `Data.Function.Between`. (**new**)
* Implementation of strict variants of all functions defined in
  `Data.Function.Between.Lazy` module. These new functions use
  `(f . g) x = f '$!' g '$!' x` as definition for function composition where
  `$!` is strict application. (**new**)
* Uploaded to Hackage: http://hackage.haskell.org/package/between-0.10.0.0


## Version 0.9.0.2

* Minor documentation changes.
* Resolving some Haddock issues in documentation.
* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/between-0.9.0.2>


## Version 0.9.0.1

* Removing all INLINE and RULES. Tested it using [ghc-core][] with GHC 7.8.3
  (bundled with [Haskell Platform][] 2014.2.0.0) and it works well.
* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/between-0.9.0.1>


## Version 0.9.0.0

* First public release.
* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/between-0.9.0.0>



[Hackage]:
  http://hackage.haskell.org/
  "HackageDB (or just Hackage) is a collection of releases of Haskell packages."
[Haskell Platform]:
  http://www.haskell.org/platform/
  "The Haskell Platform"
[ghc-core]:
  http://hackage.haskell.org/package/ghc-core
  "Display GHC's core and assembly output in a pager"
