# Revision history for cauldron

## 0.9.0.1

* compat with GHC 9.8.4.

## 0.9.0.0

* breaking change: `SomeRecipe` type hidden.
* breaking change: `recipe` now produces a `Cauldron`, not a `SomeRecipe`.
* breaking change: `withRecipe` is now `lookup` and works on `Cauldron`s.
* breaking change: `withRecipeCallstack` gone. Use `lookup`.
* breaking change: `IsList` instance for `Cauldron` has changed its `Item` type.
* breaking change: the `bean` field of `Recipe` is now called `bare`.
* new (|=|) and (䷱) operators.

## 0.8.1.0

* Add `Cauldron.Managed.runManaged`.

* `Monoid` and `Semigroup` instances for `Managed`, like the ones from the
  [managed](https://hackage.haskell.org/package/managed) library.

* Export `Cauldron.Args.Args` from `Cauldron`. This should make some IDE
  messages clearer.

## 0.8.0.0

* doc and test changes.

* re-export `arg` from Cauldron.

* breaking change: `MissingDependenciesError` now includes all the missing dependencies.

* breaking change: `DoubleDutyBeansError` is now a NonEmpty instead of a Map.

* Managed now has a MonadFail instance, like the one from the
  [managed](https://hackage.haskell.org/package/managed) library.

## 0.7.0.0

* Remove dependency on algebraic-graphs, copying those parts of the code that we used.
* Remove `cookTree` and `cookNonEmpty`. 
* Added `nest`.
* `cook` is now "typed": we pass the type of the bean we want to extract.
* `RecipeError` -> `CookingError`.
* Renamed `PrimaryBean` to `FinishedBean`.
* Renamed `SecondaryBean` to `AggregateBean`.
* Now the `Constructor`s don't depend directly on `SecondaryBean`/`AggregateBean`.
  There is a `PrimaryBean`/`FinishedBean` that points to the `SecondaryBean`/`AggregateBean`,
  and `Constructor`s depend on that.
* Rename `collapseToPrimaryBeans` to `collapseBeans`.
* Rename `removeSecondaryBeans` to `removeAggregates`.

## 0.6.1.0

* `ioEff` added to `Cauldron`.
* New module `Cauldron.Builder`.

## 0.6.0.0

* Remove sop-core dependency, incorporate just the needed functionality into the library.

  Also make the internals of the library less dependent on n-ary tuples. Now
  they are more of an added layer for convenience.

* The `cook` family of functions don't return a `DependencyGraph` anymore. Instead, the 
  graph can be obtained at any moment using `getDependencyGraph`, even for non-wireable `Cauldron`s.

* `BoiledBeans` is now just `Beans` and has its own module.

* A new `Cauldron.Args` module which defines the `Args` applicative.

* The way of creating `Constructor`s has been overhauled.

  `Packer` and `pack` are gone, along with `value`, `eff` and similar functions. The old `Regs` type is gone.

  To create `Constructor`s, now we should use `val` and `eff` along with `wire`.

* The `Bean` record is now called `Recipe`. There's also a `SomeRecipe` that hides the bean type parameter.
 
* New `ToRecipe` typeclass that helps treating single `Constructor`s as recipes-without-decos.

* The `Decos` type is now just a `Seq` of constructors of the same type.

* New `allowDepCycles` `Fire`.

* Now `DependencyGraph` renders all the dependencies, even those that are ignored during plan construction to allow for dependency cycles.

* New `Monoid` instance for `DependencyGraph`.

* `BadBeans` is now `RecipeError`. It has now an `Exception` instance and a pretty function.

* `exportToDot` is now `writeAsDot` and accepts a `RecipeError` to highlight problematic nodes.

* Now `Constructor`s and `Recipe`s keep track of the `CallStack` of when they were created. This is used
  by errors to print the relevant code locations.
  Because now we have code locations, `PathToCauldron` is no longer useful and has been removed.

## 0.4.0.0

* `exportToDot` takes a new parameter to configure how to print the steps. Before, only the TyCon was printed. Now, the full type is printed by default.

## 0.3.1.0 

* Now the `MissingDependencies` only includes keys with actual missing dependencies.

## 0.3.0.0 

* Add `cookNonEmpty` and `cookTree` for cooking hierarchies of 'Cauldron's.
* Rename `addLast` to `addOuter` and `addFirst` to `addInner`.
* Add a copy of the `Managed` type from ["managed"](https://hackage.haskell.org/package/managed).
* Change the nomenclature of the `pack-` related functions.
* Add the `Packer` type.
* Add `Fire` type to customize the handling of dependency cycles.

## 0.2.0.0 

* Decorators are no longer `Endo`s. They just take the decorated entity as a 
  regular parameter.

* Remove the applicative wrappers.

* Allow effectful constructors.

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
