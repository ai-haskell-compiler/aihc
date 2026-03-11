tasty-leancheck: LeanCheck support for Tasty
============================================

[![tasty-leancheck's Build Status][build-status]][build-log]
[![tasty-leancheck on Hackage][hackage-version]][tasty-leancheck-on-hackage]
[![tasty-leancheck on Stackage LTS][stackage-lts-badge]][tasty-leancheck-on-stackage-lts]
[![tasty-leancheck on Stackage Nightly][stackage-nightly-badge]][tasty-leancheck-on-stackage-nightly]

[LeanCheck] support for the [Tasty] test framework.
Tasty and healthy tests.


Installing
----------

    $ cabal install tasty-leancheck


Example
-------

(This example is intentionally similar to [Tasty's official example].)

Here's how your `test.hs` might look like:

```haskell
import Test.Tasty
import Test.Tasty.LeanCheck as LC
import Data.List

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Test properties checked by LeanCheck"
  [ LC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , LC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property do not hold
  , LC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 LC.==> x^n + y^n /= (z^n :: Integer)
  ]
```

And here is the output for the above program:

```
$ ./test
Test properties checked by LeanCheck
  sort == sort . reverse:  OK
    +++ OK, passed 200 tests.
  Fermat's little theorem: OK
    +++ OK, passed 200 tests.
  Fermat's last theorem:   FAIL
    *** Failed! Falsifiable (after 71 tests):
    0 0 0 3

1 out of 3 tests failed (0.00s)
```


Options
-------

The tasty-leancheck provider has only one option, `--leancheck-tests`:

```
$ ./test --leancheck-tests 10
Test properties checked by LeanCheck
  sort == sort . reverse:  OK
    +++ OK, passed 10 tests.
  Fermat's little theorem: OK
    +++ OK, passed 10 tests.
  Fermat's last theorem:   OK
    +++ OK, passed 10 tests.

All 3 tests passed (0.00s)
```


Further reading
---------------

* [tasty-leancheck's Haddock documentation];
* [LeanCheck's Haddock documentation];
* [Tasty's Haddock documentation];
* [LeanCheck's README];
* [Tasty's README];
* [Tutorial on property-based testing with LeanCheck].

[tasty-leancheck's Haddock documentation]: https://hackage.haskell.org/package/tasty-leancheck/docs/Test-Tasty-LeanCheck.html
[LeanCheck's Haddock documentation]: https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html
[Tasty's Haddock documentation]: https://hackage.haskell.org/package/tasty/docs/Test-Tasty.html
[LeanCheck's README]: https://github.com/rudymatela/leancheck#readme
[Tasty's README]: https://github.com/feuerbach/tasty#readme
[tutorial on property-based testing with LeanCheck]: https://github.com/rudymatela/leancheck/blob/master/doc/tutorial.md

[Tasty's official example]: https://github.com/feuerbach/tasty#example
[Tasty]:     https://github.com/feuerbach/tasty
[LeanCheck]: https://github.com/rudymatela/leancheck

[build-log]:     https://github.com/rudymatela/tasty-leancheck/actions/workflows/build.yml
[build-status]:  https://github.com/rudymatela/tasty-leancheck/actions/workflows/build.yml/badge.svg
[hackage-version]: https://img.shields.io/hackage/v/tasty-leancheck.svg
[tasty-leancheck-on-hackage]: https://hackage.haskell.org/package/tasty-leancheck
[stackage-lts-badge]:                  https://stackage.org/package/tasty-leancheck/badge/lts
[stackage-nightly-badge]:              https://stackage.org/package/tasty-leancheck/badge/nightly
[tasty-leancheck-on-stackage]:         https://stackage.org/package/tasty-leancheck
[tasty-leancheck-on-stackage-lts]:     https://stackage.org/lts/package/tasty-leancheck
[tasty-leancheck-on-stackage-nightly]: https://stackage.org/nightly/package/tasty-leancheck
