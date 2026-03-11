# hspec-parsec

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/hspec-parsec.svg?style=flat)](https://hackage.haskell.org/package/hspec-parsec)

This package provides handy [Hspec](http://hspec.github.io/) expectations for testing
[Parsec](http://hackage.haskell.org/package/parsec) parsers.

## Usage

Add `hspec-parsec` to your test suite's dependencies:

```
  build-depends:       base
                     , hspec
                     , hspec-parsec
                     , parsec
```

… write some tests:

```haskell
import Test.Hspec
import Test.Hspec.Parsec
import Text.Parsec
import Text.Parsec.String (Parser)

main :: IO ()
main = hspec $ do
  describe "yamlBool" $ do
    let yamlBool' = parse yamlBool ""

    it "correctly parses \"True\"" $ do
      yamlBool' "True" `shouldParse` True

    it "doesn't parse \"yes\"" $ do
      yamlBool' `shouldFailOn` "yes"

yamlBool :: Parser Bool
yamlBool = 
        (choice (map string false) *> pure False)
    <|> (choice (map string true) *> pure True)
  where
    false = ["false", "False", "FALSE"]
    true = ["true", "True", "TRUE"]
```

… and run them:

```shell
$ stack test
hspec-parsec> test (suite: spec)


yamlBool
  correctly parses "True"
  doesn't parse "yes"

Finished in 0.0001 seconds
2 examples, 0 failures

hspec-parsec> Test suite spec passed
```

## Thanks

… to [Mark Karpov](https://github.com/mrkkrp), whose package
[`hspec-megaparsec`](https://hackage.haskell.org/package/hspec-megaparsec)
much inspired `hspec-parsec`!
