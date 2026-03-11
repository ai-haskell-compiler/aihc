test-framework-leancheck: LeanCheck support for test-framework
==============================================================

[![test-framework-leancheck's Build Status][build-status]][build-log]
[![test-framework-leancheck on Hackage][hackage-version]][test-framework-leancheck-on-hackage]
[![test-framework-leancheck on Stackage LTS][stackage-lts-badge]][test-framework-leancheck-on-stackage-lts]
[![test-framework-leancheck on Stackage Nightly][stackage-nightly-badge]][test-framework-leancheck-on-stackage-nightly]

[LeanCheck] support for the [test-framework] test framework.


Installing
----------

    $ cabal install test-framework-leancheck


Example
-------

Here's how your `test.hs` might look like:

```haskell
import Test.Framework
import Test.Framework.Providers.LeanCheck as LC
import Data.List

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ LC.testProperty "sort . sort == sort"
      $ \xs -> sort (sort xs :: [Int]) == sort xs
  , LC.testProperty "sort == id" -- not really, should fail
      $ \xs -> sort (xs :: [Int]) == xs
  ]
```

And here is the output for the above program:

```
$ ./eg/test
sort . sort == sort: [OK, passed 100 tests.]
sort == id: [Failed]
*** Failed! Falsifiable (after 7 tests):
[1,0]

         Properties  Total
 Passed  1           1
 Failed  1           1
 Total   2           2
```


Options
-------

Use `-a` or `--maximum-generated-tests` to configure
the maximum number of tests for each property.

```
$ ./eg/test -a5
sort . sort == sort: [OK, passed 5 tests.]
sort == id: [OK, passed 5 tests.]

         Properties  Total      
 Passed  2           2          
 Failed  0           0          
 Total   2           2          
```

Since LeanCheck is enumerative,
you may want to increase the default number of tests (100).
Arbitrary rule of thumb:

* between 200 to 500 on a developer machine;
* between 1000 and 5000 on the CI.

Your mileage may vary.


Further reading
---------------

* [test-framework-leancheck's Haddock documentation];
* [LeanCheck's Haddock documentation];
* [test-framework's Haddock documentation];
* [LeanCheck's README];
* [test-framework's official example];
* [Tutorial on property-based testing with LeanCheck].

[test-framework-leancheck's Haddock documentation]: https://hackage.haskell.org/package/test-framework-leancheck/docs/Test-Framework-Providers-LeanCheck.html
[LeanCheck's Haddock documentation]:      https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html
[test-framework's Haddock documentation]: https://hackage.haskell.org/package/test-framework/docs/Test-Framework.html
[LeanCheck's README]:                     https://github.com/rudymatela/leancheck#readme
[test-framework's official example]: https://raw.githubusercontent.com/haskell/test-framework/master/example/Test/Framework/Example.lhs
[tutorial on property-based testing with LeanCheck]: https://github.com/rudymatela/leancheck/blob/master/doc/tutorial.md

[test-framework]: https://github.com/haskell/test-framework
[LeanCheck]:      https://github.com/rudymatela/leancheck

[build-log]:     https://github.com/rudymatela/test-framework-leancheck/actions/workflows/build.yml
[build-status]:  https://github.com/rudymatela/test-framework-leancheck/actions/workflows/build.yml/badge.svg
[hackage-version]:                              https://img.shields.io/hackage/v/test-framework-leancheck.svg
[test-framework-leancheck-on-hackage]:          https://hackage.haskell.org/package/test-framework-leancheck
[stackage-lts-badge]:                           http://stackage.org/package/test-framework-leancheck/badge/lts
[stackage-nightly-badge]:                       http://stackage.org/package/test-framework-leancheck/badge/nightly
[test-framework-leancheck-on-stackage]:         http://stackage.org/package/test-framework-leancheck
[test-framework-leancheck-on-stackage-lts]:     http://stackage.org/lts/package/test-framework-leancheck
[test-framework-leancheck-on-stackage-nightly]: http://stackage.org/nightly/package/test-framework-leancheck
