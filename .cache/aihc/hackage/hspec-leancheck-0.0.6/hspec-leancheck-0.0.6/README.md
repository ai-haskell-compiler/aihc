hspec-leancheck: LeanCheck support for Hspec
============================================

[![hspec-leancheck's Build Status][build-status]][build-log]
[![hspec-leancheck on Hackage][hackage-version]][hspec-leancheck-on-hackage]
[![hspec-leancheck on Stackage LTS][stackage-lts-badge]][hspec-leancheck-on-stackage-lts]
[![hspec-leancheck on Stackage Nightly][stackage-nightly-badge]][hspec-leancheck-on-stackage-nightly]

[LeanCheck] support for the [Hspec] test framework.


Installing
----------

    $ cabal install hspec-leancheck


Example
-------

Here's how your `spec.hs` might look like:

```haskell
import Test.Hspec
import Test.Hspec.LeanCheck as LC

import Data.List (sort)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "sort" $ do
    it "is idempotent" $
      LC.property $ \xs -> sort (sort xs :: [Int]) == sort xs
    it "is identity" $ -- not really
      LC.property $ \xs -> sort (xs :: [Int]) == xs
```

And here is the output for the above program:

```
$ ./eg/minimal

sort
  is idempotent
  is identity FAILED [1]

Failures:

  eg/minimal.hs:17:5: 
  1) sort is identity
       [1,0]

  To rerun use: --match "/sort/is identity/"

Randomized with seed 44182769

Finished in 0.0008 seconds
2 examples, 1 failure
```


Options
-------

Use `propertyWith` to configure the number of tests.


Further reading
---------------

* [hspec-leancheck's Haddock documentation];
* [LeanCheck's Haddock documentation];
* [Hspec's Haddock documentation];
* [LeanCheck's README];
* [Hspec's README];
* [Tutorial on property-based testing with LeanCheck].

[hspec-leancheck's Haddock documentation]: https://hackage.haskell.org/package/hspec-leancheck/docs/Test-Hspec-LeanCheck.html
[LeanCheck's Haddock documentation]: https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html
[Hspec's Haddock documentation]: https://hackage.haskell.org/package/hspec/docs/Test-Hspec.html
[LeanCheck's README]: https://github.com/rudymatela/leancheck#readme
[Hspec's README]: https://github.com/hspec/hspec#readme
[tutorial on property-based testing with LeanCheck]: https://github.com/rudymatela/leancheck/blob/master/doc/tutorial.md

[Hspec]:     https://hspec.github.io/
[LeanCheck]: https://github.com/rudymatela/leancheck

[build-log]:     https://github.com/rudymatela/hspec-leancheck/actions/workflows/build.yml
[build-status]:  https://github.com/rudymatela/hspec-leancheck/actions/workflows/build.yml/badge.svg
[hackage-version]: https://img.shields.io/hackage/v/hspec-leancheck.svg
[hspec-leancheck-on-hackage]: https://hackage.haskell.org/package/hspec-leancheck
[stackage-lts-badge]:                  https://stackage.org/package/hspec-leancheck/badge/lts
[stackage-nightly-badge]:              https://stackage.org/package/hspec-leancheck/badge/nightly
[hspec-leancheck-on-stackage]:         https://stackage.org/package/hspec-leancheck
[hspec-leancheck-on-stackage-lts]:     https://stackage.org/lts/package/hspec-leancheck
[hspec-leancheck-on-stackage-nightly]: https://stackage.org/nightly/package/hspec-leancheck
