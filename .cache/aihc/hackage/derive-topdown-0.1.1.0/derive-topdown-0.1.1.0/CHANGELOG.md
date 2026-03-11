derive-topdown
======
derive-topdown-0.1.0.0
--------------
- `0.1.0.0`
    * Almost a reimplementation the deriving and instance generation functions.
	* Implement class context generation. Now only necessary context type will be generated. 
    * This version only compatible for ghc-8.6 to ghc-9.10. ghc-8.4 and ghc-8.2 can build but not able to run test cases.


derive-topdown-0.0.3.0
--------------
- `0.0.3.0`
	* Adapt to GHC 9.0.

derive-topdown-0.0.2.1
--------------
- `0.0.2.1`
	* Remove unnecessary dependencies for testing.

derive-topdown-0.0.2.0
--------------
- `0.0.2.0`
	* Add deriving superclass functions. When you want to derive a class instance, not necessary to derive all its superclasses.

derive-topdown-0.0.1.0
--------------
- `0.0.1.0`
    * Imporve error message.Will print the types list when type is not data or newtype defined.

derive-topdown-0.0.0.9
--------------
- `0.0.0.9` 
    * Changed API for `strategy_derivng(s)`. Deriving strategy should be specified first as the default grammar of `deriving`.
    * Provided `deriving_with_breaks` function so that one can specify the types that prevent further standalone deriving declarations from generating process. This is mainly for `Generic` class. See the test of deriving `Generic` for `HsModule`.
    * Fixed Strategy deriving problem. Deriving with NewtypeStrategy on data type defined by `data` keyword will be ignored.
    * Fixed problem with class context generation for phantom types. Phantom types will not be in the context now.
    * Fixed `ArrowT` problem. It should be not a type variable and should appear in the context.
    * Handled data constructor which contains explicit `forall`.
    * Added test. It generates class instances for data types in `template-haskell` and `haskell-src`.

derive-topdown-0.0.0.7
--------------
- `0.0.0.7` is a more carefully implemented version of derive-topdown.
All APIs are **totally** changed compared with 0.0.0.2. Originally it did not work with types that contain type synonyms and polymorphic types. It can work for most common cases in this version now.

derive-topdown-0.0.0.2
--------------
- Fixed some cabal file and API problems.

derive-topdown-0.0.0.1
--------------
- This is only an experimental implementation for deriving class instances from top to bottom.
There is a lot of deficiencies.
