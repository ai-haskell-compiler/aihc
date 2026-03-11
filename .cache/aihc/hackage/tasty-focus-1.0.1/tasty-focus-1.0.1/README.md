## tasty-focus
[![tasty-focus on hackage](https://img.shields.io/hackage/v/tasty-focus)](http://hackage.haskell.org/package/tasty-focus)
[![tasty-focus on Stackage Nightly](https://stackage.org/package/tasty-focus/badge/nightly)](https://stackage.org/nightly/package/tasty-focus)

Simple focus mechanism for `tasty`, similar to `hspec`.

Mark the root of your test tree with `withFocus`.
Then, if any of the subtrees of your test suite are marked with `focus`, only those test trees will be run.

The intended use case is similar to `--pattern`, but for quick `ghcid`-based feedback loops.
Using `focus` will throw a deprecation warning, so that together with `-Werror` you can check that you don't accidentally leave tests focused on CI.

### Example
In this example, only `barTests` will run. Removing either `focus` or `withFocus` will run the entire tree again.

```haskell
main :: IO ()
main = defaultMain . withFocus $
  testGroup "tests"
    [ fooTests
    , testGroup "subgroup"
      [ focus barTests
      , bazTests
	  ]
	, quuxTests
    ]
```
