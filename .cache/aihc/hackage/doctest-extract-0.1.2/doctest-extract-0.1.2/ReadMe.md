# Known Issues

*   For compatibility with original `doctest` parser
    you cannot write

    ~~~~
    {-# LANGUAGE MyPreferredExtension #-}
    ~~~~

    Instead you must write

    ~~~~
    :set -XMyPreferredExtension
    ~~~~

* In Literal Haskell files
  only `\\begin{code} ... \\end{code}` blocks are scanned,
  but not bird style code blocks.

* `prop>` supports multi-line code,
  but both original `doctest` and `haddock` do not support it.

* IO tests are not supported as `doctest` examples, so far.
  We need a syntactic distinction for IO tests,
  because `doctest-extract` does not employ a type-checker.
  We could mark IO tests
  with a specific `id` function, as in `ioTest $ runMyTest`
  or a type annotation, as in `runMyTest :: IO ()`.


# Tipps and Tricks

## How to disable selected tests?

For focussing on certain tests it can be useful to disable other ones.
We have not implemented a mechanism to disable parts of the test suite
in `doctest-extract`,
because this would require to implement a way to identify tests.
You can still disable some of the tests
without explicit support by `doctest-extraxt`.

* If you want to disable whole modules,
  you may make a copy of the auto-generated `Test/Main.hs`
  and remove the modules that you want to skip.

* For disabling all tests on a function
  you may turn a Haddock comment into a plain comment
  by removing the bar after the opening of the comment.

* For disabling individual tests you may prefix `>>>` and `prop>`
  with an asterisk or the like.

These tricks work best in conjunction with a revision control systen,
such that it always reminds you that there are tests disabled temporarily.
