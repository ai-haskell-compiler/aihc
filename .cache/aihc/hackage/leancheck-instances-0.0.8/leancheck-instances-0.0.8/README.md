LeanCheck
=========

[![leancheck-instances's Build Status][build-status]][build-log]
[![leancheck-instances on Hackage][hackage-version]][leancheck-instances-on-hackage]
[![leancheck-instances on Stackage LTS][stackage-lts-badge]][leancheck-instances-on-stackage-lts]
[![leancheck-instances on Stackage Nightly][stackage-nightly-badge]][leancheck-instances-on-stackage-nightly]

This package extends [LeanCheck] by providing [`Listable`] instances for common types provided by the
Haskell Platform.

This package is to [LeanCheck] what [quickcheck-instances] is to [QuickCheck].
The current objective is to include all types supported by [quickcheck-instances].


Installing
----------

To install the latest leancheck-instances version from Hackage, just run:

	$ cabal update
	$ cabal install leancheck-instances


Examples
--------

Importing the library:

	> import Test.LeanCheck
	> import Test.LeanCheck.Instances

Checking properties of [`Text`]:

	> import qualified Data.Text as T
	> check $ \t -> T.reverse (T.reverse t) == t
	+++ OK, passed 200 tests.
	> check $ \t -> T.reverse t == t
	*** Failed! Falsifiable (after 6 tests):
	"a "

Enumerating [`Map`]s:

	> import Data.Map
	> list :: [Map Bool Bool]
	[ fromList []
	, fromList [(False,False)]
	, fromList [(False,True)]
	, fromList [(True,False)]
	, fromList [(True,True)]
	, fromList [(False,False),(True,False)]
	, fromList [(False,False),(True,True)]
	, fromList [(False,True),(True,False)]
	, fromList [(False,True),(True,True)]
	]
	> take 7 $ list :: [Map Int Int]
	[ fromList []
	, fromList [(0,0)]
	, fromList [(0,1)]
	, fromList [(1,0)]
	, fromList [(0,-1)]
	, fromList [(1,1)]
	, fromList [(0,0),(1,0)]
	]


Adding more instances
---------------------

Although the current objective is to include all types supported by
[quickcheck-instances], [leancheck-instances] only has about 10% of what is
needed.  Any help with new instances to increase that percentage will be
appreciated.

This section provides a quick guide on how to add new instances.

1. __Choose the type to support__
	Compare the instances provided on [quickcheck-instances] and
	[leancheck-instances] and choose any that has not been added to
	[leancheck-instances] yet.

2. __Create the module file if needed__
	If needed, create a module that will contain your instance following the
	same structure in [quickcheck-instances]:

       $ cat > src/Test/LeanCheck/Instances/Something.hs
       -- |
       -- Module      : Test.LeanCheck.Instances.Containers
       -- Copyright   : (c) 2019 Authors of leancheck-instances
       -- License     : 3-Clause BSD  (see the file LICENSE)
       -- Maintainer  : Rudy Matela <rudy@matela.com.br>
       --
       -- 'Listable' something.
       module Test.LeanCheck.Instances.Something () where

       import Test.LeanCheck
       import Something
       ^D

	Remember to:

	* Import your newly created module on `src/Test/LeanCheck/Instances.hs`

	* Add your newly created module to the `exposed-modules` list in
	  `leancheck-instances.cabal`.

	* You may need to add a package dependency to `build-depends` on
	  `leancheck-instances.cabal`.

	* (Optionally) run `make depend` to update the `mk/depend.mk` file.

3. __Create your instance__
	Open the relevant module with your favourite text editor and add your
	instance:

       instance ... => Listable Something where
         ...

	Check the existing modules and the [`Listable`] typeclass documentation for
	how to create one.

	Make sure your instance builds with `cabal build`.

4. __Create tests__
	Go into `tests/main.hs` and add two properties exercising your type, one
	that [`holds`] and one that [`fails`].  Make sure the tests pass by running
	`cabal test`.

5. (Optional) __Add diff-tests__

	* on `bench/tiers.hs` add an entry for your type;
	* add two matching entries on the `diff-test-tiers` and
	  `update-diff-test-tiers` Makefile targets.
	* run `make update-diff-test` to generate the reference output file.
	* run `make test` just to make sure the test is working.

6. __Submit a Pull Request__
	Then submit a [pull request on GitHub] and wait for your build to pass.
	Alternatively, send a patch via e-mail.


Further reading / see also
--------------------------

* [leancheck-instances' Haddock Documentation];
* [LeanCheck];
* [LeanCheck provider for Tasty]
  -- `$ cabal install tasty-leancheck` ;
* [LeanCheck provider for test-framework]
  -- `$ cabal install test-framework-leancheck` ;
* [LeanCheck provider for Hspec]
  -- `$ cabal install hspec-leancheck` .


[leancheck-instances' Haddock documentation]: https://hackage.haskell.org/package/leancheck-instances/docs/Test-LeanCheck-Instances.html
[LeanCheck's Haddock documentation]: https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html

[`Listable`]:       https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#t:Listable
[`holds`]:          https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#v:holds
[`fails`]:          https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#v:fails
[`counterExample`]: https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#v:counterExample
[`check`]:          https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#v:check
[`tiers`]:          https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#v:tiers
[`list`]:           https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#v:list
[`Text`]:           https://hackage.haskell.org/package/text/docs/Data-Text.html#t:Text
[`Map`]:            https://hackage.haskell.org/package/containers/docs/Data-Map-Lazy.html#t:Map

[LeanCheck provider for Tasty]:          https://hackage.haskell.org/package/tasty-leancheck
[LeanCheck provider for test-framework]: https://hackage.haskell.org/package/test-framework-leancheck
[LeanCheck provider for Hspec]:          https://hackage.haskell.org/package/hspec-leancheck
[LeanCheck]:                             https://github.com/rudymatela/leancheck
[QuickCheck]:                            https://hackage.haskell.org/package/QuickCheck
[quickcheck-instances]:                  https://hackage.haskell.org/package/quickcheck-instances
[leancheck-instances]:                   https://hackage.haskell.org/package/leancheck-instances

[build-log]:    https://github.com/rudymatela/leancheck-instances/actions/workflows/build.yml
[build-status]: https://github.com/rudymatela/leancheck-instances/actions/workflows/build.yml/badge.svg
[hackage-version]: https://img.shields.io/hackage/v/leancheck-instances.svg
[leancheck-instances-on-hackage]:          https://hackage.haskell.org/package/leancheck-instances
[stackage-lts-badge]:                      https://stackage.org/package/leancheck-instances/badge/lts
[stackage-nightly-badge]:                  https://stackage.org/package/leancheck-instances/badge/nightly
[leancheck-instances-on-stackage]:         https://stackage.org/package/leancheck-instances
[leancheck-instances-on-stackage-lts]:     https://stackage.org/lts/package/leancheck-instances
[leancheck-instances-on-stackage-nightly]: https://stackage.org/nightly/package/leancheck-instances
[pull request on GitHub]:                  https://github.com/rudymatela/leancheck-instances/pulls
