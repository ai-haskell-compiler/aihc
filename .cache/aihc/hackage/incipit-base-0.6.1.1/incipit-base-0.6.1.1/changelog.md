# 0.6.0.0

* Add polymorphic type family `Undefined` that emits a `TypeError`.

# 0.5.0.0

* Add type error concatenation families from `type-errors-pretty`.

# 0.4.0.0

* Add safe versions of `Foldable.minimum` et al.
* Export `showsPrec`, `showParen`, `showString`.

# 0.3.0.0

* Add safe versions of `Fractional` division methods.
* Add `fromText`.
* Remove module exports for `Prelude` to avoid a Stack bug.
* Rename `Incipit` modules to `IncipitBase` and `IncipitCore`.
* Export `withFrozenCallStack`.
* Add `leftA`, an `Either` combinator that runs an applicative action on a `Left`.

# 0.2.0.0

* Add safe, `Maybe` returning, versions of the methods of `Integral`.
* Hide constructors of effect GADTs.
* Reexport `Data.Text.Lazy.Builder`.
