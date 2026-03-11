# ChangeLog / ReleaseNotes


## Version 0.4.0.0

*   Bumped lower bound of `dhall` library set to 1.23.0.  Version 1.22.0
    supported Dhall Standard 7.0.0, however, it failed to deserialise new-style
    enums properly, which was fixed in version 1.23.0.  (**breaking change**)

*   Dhall library restructured to fit with current Dhall best practices.

*   Bumped upper bound of `lattices` to support versions 2.\* (**breaking
    change**)

*   Bumped upper bound of `generic-lens` to support versions 2.\* (**change**)

*   Bumped upper bound of `safecopy` to support versions 0.10.\* (**change**)

*   Removed `data-default` dependency and associated instance for `Default`
    type class. (**breaking change**)


## Version 0.3.0.0

* Optional instances for `JoinSemiLattice`, `MeetSemiLattice`,
  `BoundedJoinSemiLattice`, `BoundedMeetSemiLattice`, `Lattice`, and
  `BoundedLattice`. Dependency on `lattices` package can be enabled using
  `-flattices` build flag. (**new**)
* Optional instances for `Dhall.Interpret` `Dhall.Inject` type classes.
  Dependency on `dhall` package can be enabled using `-fdhall` build flag.
  Enabled by default. (**new**)
* Optional instances for `Serialise` type class from `serialise` package that
  provides CBOR serialisation/deserialisation.  Dependency on `serialise`
  package can be enabled using `-fserialise` build flag.  Enabled by default.
  (**new**)
* `HasVerbosity` type class now provides default implementation for `verbosity`
   lens that uses `generic-lens`. (**change/new**)
* Dropped support for GHC \< 8.2.  As a consequence `Data`, `Generic`, and
  `Typeable` are always derived.  The last one is implied by the fact that GHC
  now always derives it. (**breaking change**)
* Uploaded to [Hackage][]: <http://hackage.haskell.org/package/verbosity-0.3.0.0>


## Version 0.2.3.0

* Introducing function
  `modifyVerbosity :: HasVerbosity s => (Verbosity -> Verbosity) -> s -> s`
  (**new**)
* Introducing optional instance for safecopy's `SafeCopy` type class. Dependency
  on `safecopy` package can be enabled using `-fsafecopy` build flag. (**new**)
* Uploaded to [Hackage][]: <http://hackage.haskell.org/package/verbosity-0.2.3.0>


## Version 0.2.2.0

* Relaxed `data-default-class` dependency that allows bilding with version
  0.1.\*. (**change**)
* Introducing functions (**new**):
    * `increment :: Verbosity -> Maybe Verbosity`
    * `increment' :: Verbosity -> Verbosity`
* Uploaded to [Hackage][]: <http://hackage.haskell.org/package/verbosity-0.2.2.0>


## Version 0.2.1.0

* Introducing optional instance for cereal's `Serialize` type class. Dependency
  on `cereal` package can be enabled using `-fcereal` build flag. (**new**)
* Uploaded to [Hackage][]: <http://hackage.haskell.org/package/verbosity-0.2.1.0>


## Version 0.2.0.0

* Introducing module `Data.Verbosity.Class` which contains definition of
  `HasVerbosity` type class. (**new**)
* Introducing function `fromInt :: Int -> Maybe Verbosity`. (**new**)
* Introducing function
  `parse :: (Eq string, IsString string) => string -> Maybe Verbosity`. (**new**)
* NFData instance, if compiled with `-fdeepseq`, which is the default case.
  (**new**)
* Depends on [transformers][] package in case [base][] <4.8. (**new**)
* Uploaded to [Hackage][]: <http://hackage.haskell.org/package/verbosity-0.2.0.0>


## Version 0.1.0.0

* First public release.
* Uploaded to [Hackage][]: <http://hackage.haskell.org/package/verbosity-0.1.0.0>


[base]:
  http://hackage.haskell.org/package/base
  "base package on Hackage"
[Hackage]:
  http://hackage.haskell.org/
  "HackageDB (or just Hackage) is a collection of releases of Haskell packages."
[transformers]:
  http://hackage.haskell.org/package/transformers
  "transformers package on Hackage"
