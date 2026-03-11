<div align="center">
    <img src="logo.png" width="830px" align="center" style="object-fit: cover"/>
</div>

[![Latest release](http://img.shields.io/github/release/pujoheadsoft/mockcat.svg)](https://github.com/pujoheadsoft/mockcat/releases)
[![Test](https://github.com/pujoheadsoft/mockcat/workflows/Test/badge.svg)](https://github.com/pujoheadsoft/mockcat/actions?query=workflow%3ATest+branch%3Amain)
[![](https://img.shields.io/hackage/v/mockcat)](https://hackage.haskell.org/package/mockcat)

## Overview
mockcat is a lightweight, declarative mocking & stubbing DSL for Haskell.

You describe expected applications using a left‑to‑right pipeline (`arg |> arg |> returnValue`) and then either:
* run function mocks directly, or
* generate typeclass mocks via Template Haskell (`makeMock`, `makePartialMock`) and run them inside `runMockT` with automatic verification.

[日本語版 README はこちら](https://github.com/pujoheadsoft/mockcat/blob/master/README-ja.md)

### Features
* Small surface: describe a case with `arg |> ... |> result` – no separate expectation language
* Concurrency‑safe counting: parallel applications don’t lose or double count
* Honest laziness: an unevaluated result doesn’t register as a call; nothing is forced behind your back
* Flexible returns: change results per argument or per occurrence (including identical args later)
* Partial mocking: generate only the methods you care about with `makePartialMock`
* Monadic return variation: for `IO a` you can vary the monadic result across calls
* Clear failures: messages show the raw arguments and matcher labels directly
* Low ceremony constant stubs: `createConstantMock` / `createNamedConstantMock`
* Template Haskell helpers: `makeMock` to cut boilerplate for typeclasses
* No hidden global state: tests stay isolated

### Tested Versions
mockcat is continuously tested in CI across these configurations (see `.github/workflows/Test.yml`):

| GHC | Cabal | OS |
|-----|-------|----|
| 9.2.8 | 3.10.3.0 / 3.12.1.0 | Ubuntu, macOS, Windows |
| 9.4.8 | 3.10.3.0 / 3.12.1.0 | Ubuntu, macOS, Windows |
| 9.6.3 | 3.10.3.0 / 3.12.1.0 | Ubuntu, macOS, Windows |
| 9.8.2 | 3.10.3.0 / 3.12.1.0 | Ubuntu, macOS, Windows |
| 9.10.1 | 3.10.3.0 / 3.12.1.0 | Ubuntu, macOS, Windows |

Notes:
* The `tested-with` stanza in the cabal file reflects the same GHC list.
* Other patch releases within the same minor series typically work; open an issue if you hit a snag.
* Older GHCs (< 9.2) are not targeted due to dependency bounds and modern TH/unliftio requirements.

Designed to be something you can pick up to stub one or two spots and then forget again – more a handy extension of regular Haskell testing than a new framework.

### Quick Start

Function mock:
```haskell
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = it "simple function mock" do
  m <- createMock $ "input" |> 42
  stubFn m "input" `shouldBe` 42
  m `shouldApplyTo` "input"
```

Typeclass mock:
```haskell
{-# LANGUAGE TemplateHaskell #-}
import Test.Hspec
import Test.MockCat

class Monad m => KV m where
  getKV :: String -> m (Maybe String)
  putKV :: String -> String -> m ()

makeMock [t|KV|]

program :: KV m => m (Maybe String)
program = do
  putKV "k" "v"
  getKV "k"

spec :: Spec
spec = it "kv" do
  r <- runMockT do
    _putKV ("k" |> "v" |> ())
    _getKV ("k" |> Just "v")
    program
  r `shouldBe` Just "v"
```

Non‑invocation (two styles):
```haskell
  _putKV ("x" |> "y" |> ()) `expectApplyTimes` 0  -- (legacy name: applyTimesIs)
  neverApply $ _putKV ("x" |> "y" |> ())
```

### Core Concepts (Brief)
* Pipeline DSL: `a |> b |> returnValue` describes one expected application.
* Matchers: `any`, `expect p label`, `expect_ p`, `$(expectByExpr [| predicate |])`.
* Argument‑dependent returns: `onCase` or `cases [...]` (including differing values for same arg on later occurrences).
* Counting: `expectApplyTimes` (legacy: `applyTimesIs`), `expectNever` (legacy: `neverApply`), plus `shouldApplyTimes*` predicates on mocks.
* Ordering: `shouldApplyInOrder`, `shouldApplyInPartialOrder`.
* Concurrency: Call recorded only when result evaluated; counting atomic.
* Partial mocks: `makePartialMock` for generating only some methods.
* Monadic return variation (`IO a`): enable with `implicitMonadicReturn` option.

### Concurrency & Laziness Semantics
Within `runMockT`:
1. Each evaluated application contributes 1 count (atomic IORef).
2. Unforced results are not recorded.
3. Order checks reflect evaluation order, not mere start time.
4. Finish async work before verification.
5. Fresh state per `runMockT` – no cross‑test leakage.

### Architecture Notes (≥ 0.5.3.0)
Refactor from `StateT` to `ReaderT (IORef [Definition])` to:
* Provide lawful `MonadUnliftIO` instance for safe `withRunInIO` / `async`.
* Remove lost/double count races via strict `atomicModifyIORef'`.
* Eliminate `unsafePerformIO` & hidden global state.
* Simplify TH output & reduce constraint noise.

Migration: internal `(modify/get)` swapped to `addDefinition/getDefinitions` (via `MonadMockDefs`). Public DSL unchanged.

<details>
<summary><strong>Update History</strong></summary>

* **0.5.3.0**: `MockT` now implements `MonadUnliftIO`; concurrency safety clarified; removal of `unsafePerformIO`.
* **0.5.0**: `IO a` stubs can return different values on successive applications.
* **0.4.0**: Partial mocks for typeclasses.
* **0.3.0**: Typeclass mocks.
* **0.2.0**: Argument‑dependent return values (including differing values for identical args).
* **0.1.0**: Initial release.
</details>

---
Below is the full expanded reference (legacy detailed sections). Collapse if you only need the quick start.

<details>
<summary><strong>Full Reference (expanded)</strong></summary>

<!-- BEGIN FULL REFERENCE (original content, lightly normalized) -->
## Examples
Stub Function
```haskell
-- create a stub function
stubFn <- createStubFn $ "value" |> True
-- assert
stubFn "value" `shouldBe` True
```
Verification
```haskell
-- create a mock
mock <- createMock $ "value" |> True
-- stub function
let stubFunction = stubFn mock
-- assert
stubFunction "value" `shouldBe` True
-- verify
mock `shouldApplyTo` "value"
```
Mock of Type Class
```haskell
result <- runMockT do
  -- stub functions
  _readFile $ "input.txt" |> pack "content"
  _writeFile $ "output.txt" |> pack "content" |> ()
  -- sut
  program "input.txt" "output.txt"

result `shouldBe` ()
```
## Stub Function Overview
Stub functions can be created with the `createStubFn` function.  
The arguments of `createStubFn` are the arguments expected to be applied, concatenated by `|>`, where the last value of `|>` is the return value of the function.
```haskell
createStubFn $ (10 :: Int) |> "return value"
```
The same is true for stub functions in typeclass mocks.
```haskell
runMockT do
  _readFile $ "input.txt" |> pack "content"
```
Expected arguments can also be specified as conditions.
```haskell
-- Conditions other than exact match
createStubFn $ any |> "return value"
createStubFn $ expect (> 5) "> 5" |> "return value"
createStubFn $ expect_ (> 5) |> "return value"
createStubFn $ $(expectByExpr [|(> 5)|]) |> "return value"
```
It is also possible to change the value returned depending on the argument.  
(It is also possible to return different values for the same argument.)
```haskell
-- Parameterized Stub
createStubFn do
  onCase $ "a" |> "return x"
  onCase $ "b" |> "return y"
createStubFn do
  onCase $ "arg" |> "x"
  onCase $ "arg" |> "y"
```
## Verification Overview
To verify the application of a stub function, first create a mock with the `createMock` function.  
Stub functions are retrieved from the mock with the `stubFn` function and used.  
Verification is performed on the mock.
```haskell
-- create a mock
mock <- createMock $ "value" |> True
-- stub function
let stubFunction = stubFn mock
-- assert
stubFunction "value" `shouldBe` True
-- verify
mock `shouldApplyTo` "value"
```
As with stub functions, conditions can be specified in the case of verification.
```haskell
mock `shouldApplyTo` any @String
mock `shouldApplyTo` expect_ (/= "not value")
mock `shouldApplyTo` $(expectByExpr [|(/= "not value")|])
```
You can also verify the number of times it has been applied.
```haskell
mock `shouldApplyTimes` (1 :: Int) `to` "value"
mock `shouldApplyTimesGreaterThan` (0 :: Int) `to` "value"
mock `shouldApplyTimesGreaterThanEqual` (1 :: Int) `to` "value"
mock `shouldApplyTimesLessThan` (2 :: Int) `to` "value"
mock `shouldApplyTimesLessThanEqual` (1 :: Int) `to` "value"
mock `shouldApplyTimesToAnything` (1 :: Int)
```
In the case of typeclass mocks, when `runMockT` is applied, verification that the prepared stub functions have been applied is performed automatically.
```haskell
result <- runMockT do
  _readFile $ "input.txt" |> pack "Content"
  _writeFile $ "output.text" |> pack "Content" |> ()
  operationProgram "input.txt" "output.text"

result `shouldBe` ()
```
## Mock of monad type class
### Example usage
For example, suppose the following monad type class `FileOperation` and a function `operationProgram` that uses `FileOperation` are defined.
```haskell
class Monad m => FileOperation m where
  readFile :: FilePath -> m Text
  writeFile :: FilePath -> Text -> m ()

operationProgram ::
  FileOperation m =>
  FilePath ->
  FilePath ->
  m ()
operationProgram inputPath outputPath = do
  content <- readFile inputPath
  writeFile outputPath content
```

You can generate a mock of the typeclass `FileOperation` by using the `makeMock` function as follows  
`makeMock [t|FileOperation|]`

Then following two things will be generated: 
1. a `MockT` instance of typeclass `FileOperation`
2. a stub function based on a function defined in the typeclass `FileOperation`  
  Stub functions are created as functions with `_` prefix added to the original function.  
  In this case, `_readFile` and `_writeFile` are generated.

Mocks can be used as follows.
```haskell
spec :: Spec
spec = do
  it "Read, and output files" do
    result <- runMockT do
      _readFile ("input.txt" |> pack "content")
      _writeFile ("output.txt" |> pack "content" |> ())
      operationProgram "input.txt" "output.txt"

    result `shouldBe` ()
```
Stub functions are passed arguments that are expected to be applied to the function, concatenated by `|>`.  
The last value of `|>` is the return value of the function.

Mocks are run with `runMockT`.

### Verification
After execution, the stub function is verified to see if it is applied as expected.  
For example, the expected argument of the stub function `_writeFile` in the above example is changed from `"content"` to `"edited content"`.
```haskell
result <- runMockT do
  _readFile ("input.txt" |> pack "content")
  _writeFile ("output.txt" |> pack "edited content" |> ())
  operationProgram "input.txt" "output.txt"
```
If you run the test, the test will fail and you will get the following error message.
```console
uncaught exception: ErrorCall
function `_writeFile` was not applied to the expected arguments.
  expected: "output.txt", "edited content"
  but got: "output.txt", "content"
```

Suppose also that you did not use the stub function corresponding to the function you are using in your test case, as follows
```haskell
result <- runMockT do
  _readFile ("input.txt" |> pack "content")
  -- _writeFile ("output.txt" |> pack "content" |> ())
  operationProgram "input.txt" "output.txt"
```
Again, when you run the test, the test fails and you get the following error message.
```console
no answer found stub function `_writeFile`.
```
## Verify the number of times applied
For example, suppose you want to write a test for not applying `_writeFile` if it contains a specific string as follows.
```haskell
operationProgram inputPath outputPath = do
  content <- readFile inputPath
  unless (pack "ngWord" `isInfixOf` content) $
    writeFile outputPath content
```
This can be accomplished by using the `expectApplyTimes` function (legacy name: `applyTimesIs`) as follows.
```haskell
import Test.MockCat as M
...
it "Read, and output files (contain ng word)" do
  result <- runMockT do
    _readFile ("input.txt" |> pack "contains ngWord")
    _writeFile ("output.txt" |> M.any |> ()) `expectApplyTimes` 0
    operationProgram "input.txt" "output.txt"

  result `shouldBe` ()
```
You can verify that it was not applied by specifying `0`.
Or you can use the `neverApply` function to accomplish the same thing.
```haskell
result <- runMockT do
  _readFile ("input.txt" |> pack "contains ngWord")
  neverApply $ _writeFile ("output.txt" |> M.any |> ())
  operationProgram "input.txt" "output.txt"
```
`M.any` is a parameter that matches any value.  
This example uses `M.any` to verify that the `writeFile` function does not apply to any value.
As described below, mockcat provides a variety of parameters other than `M.any`.

### Mock constant functions
mockcat can also mock constant functions.  
Let's mock `MonadReader` and use the `ask` stub function.
```haskell
data Environment = Environment { inputPath :: String, outputPath :: String }

operationProgram ::: MonadReader Environment m =>
  MonadReader Environment m =>
  FileOperation m =>
  m ()
operationProgram = do
  (Environment inputPath outputPath) <- ask
  content <- readFile inputPath
  writeFile outputPath content

makeMock [t|MonadReader Environment|]

spec :: Spec
spec = do
  it "Read, and output files (with MonadReader)" do
    r <- runMockT do
      _ask (Environment "input.txt" "output.txt")
      _readFile ("input.txt" |> pack "content")
      _writeFile ("output.txt" |> pack "content" |> ())
      operationProgram
    r `shouldBe` ()
```
Now let's try to avoid using `ask`.
```haskell
operationProgram = do
  content <- readFile "input.txt"
  writeFile "output.txt" content
```
Then the test run fails and you will see that the stub function was not applied.
```haskell
It has never been applied function `_ask`
```
### Mock that returns a value of type `IO a`.
Normally constant functions return the same value, but only for mocks that return a value of type `IO a`, you can create a mock that returns a different value each time it is applied.  
For example, suppose a typeclass `Teletype` and a function `echo` to be tested are defined.  
The `echo` will behave differently depending on the value returned by `readTTY`.
```haskell
class Monad m => Teletype m where
  readTTY :: m String
  writeTTY :: String -> m ()

echo :: Teletype m => m ()
echo = do
  i <- readTTY
  case i of
    "" -> pure ()
    _  -> writeTTY i >> echo
```
You will want to verify that if `readTTY` returns anything other than `""`, it is called recursively.  
To do this, we need to be able to have `readTTY` return different values in a single test.  
To achieve this, create a mock with the `implicitMonadicReturn` option.
Using `implicitMonadicReturn` allows stub functions to explicitly return monadic values.
```haskell
makeMockWithOptions [t|Teletype|] options { implicitMonadicReturn = False }
```
This allows the test to use `onCase` to have a behavior where the first application returns a value other than `""` and the second application returns `""`.
```haskell
result <- runMockT do
  _readTTY $ do
    onCase $ pure @IO "a"
    onCase $ pure @IO ""

  _writeTTY $ "a" |> pure @IO ()
  echo
result `shouldBe` ()
```
### Partial mocking
The `makePartialMock` function can be used to mock only a part of a function defined in a typeclass.
For example, suppose you have the following typeclasses and functions.  
`getUserInput` is the function to be tested.
```haskell
data UserInput = UserInput String deriving (Show, Eq)

class Monad m => UserInputGetter m where
  getInput :: m String
  toUserInput :: String -> m (Maybe UserInput)

getUserInput :: UserInputGetter m => m (Maybe UserInput)
getUserInput = do
  i <- getInput
  toUserInput i
```
In this example, we want to use real functions, so we define an `IO` instance as follows.
```haskell
instance UserInputGetter IO where
  getInput = getLine
  toUserInput "" = pure Nothing
  toUserInput a = (pure . Just . UserInput) a
```
The test will look like this.
```haskell
makePartialMock [t|UserInputGetter|]

spec :: Spec
spec = do
  it "Get user input (has input)" do
    a <- runMockT do
      _getInput "value"
      getUserInput
    a `shouldBe` Just (UserInput "value")

  it "Get user input (no input)" do
    a <- runMockT do
      _getInput ""
      getUserInput
    a `shouldBe` Nothing
```
### Rename stub functions
The prefix and suffix of the generated stub functions can optionally be changed.  
For example, the following will generate the functions `stub_readFile_fn` and `stub_writeFile_fn`.
```haskell
makeMockWithOptions [t|FileOperation|] options { prefix = "stub_", suffix = "_fn" }
```
If no options are specified, it defaults to `_`.
### Code generated by makeMock
Although you do not need to be aware of it, the `makeMock` function generates the following code.
```haskell
-- MockT instance
instance (Monad m) => FileOperation (MockT m) where
  readFile :: Monad m => FilePath -> MockT m Text
  writeFile :: Monad m => FilePath -> Text -> MockT m ()

_readFile :: (MockBuilder params (FilePath -> Text) (Param FilePath), Monad m) => params -> MockT m ()
_writeFile :: (MockBuilder params (FilePath -> Text -> ()) (Param FilePath :> Param Text), Monad m) => params -> MockT m ()
```
## Mocking functions
In addition to mocking monad type classes, mockcat can also mock regular functions.  
Unlike monad type mocks, the original function is not required.
### Example usage
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "usage example" do
    -- create a mock (applying "value" returns the pure value True)
    mock <- createMock $ "value" |> True
    -- extract a stub function from a mock
    let stubFunction = stubFn mock
    -- verify the result of applying the function
    stubFunction "value" `shouldBe` True
    -- verify that the expected value ("value") has been applied
    mock `shouldApplyTo` "value"
```
## Stub functions
To create a stub function directly, use the `createStubFn` function.  
If you don't need verification, you can use this one.
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "can generate stub functions" do
    -- generate
    f <- createStubFn $ "param1" |> "param2" |> pure @IO ()
    -- apply
    actual <- f "param1" "param2"
    -- Verification
    actual `shouldBe` ()
```
The `createStubFn` function is passed a sequence of `|>` arguments that the function is expected to apply.
The last value of `|>` is the return value of the function.
If the stub function is applied to an argument it is not expected to be applied to, an error is returned.
```console
Uncaught exception: ErrorCall
Expected arguments were not applied to the function.
  expected: "value"
  but got: "valuo"
```
### Named Stub Functions
You can name stub functions.
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "named stub" do
  f <- createNamedStubFn "named stub" $ "x" |> "y" |> True
    f "x" "z" `shouldBe` True
```
The error message printed when a stub function is not applied to an expected argument will include this name.
```console
uncaught exception: ErrorCall
Expected arguments were not applied to the function `named stub`.
  expected: "x","y"
  but got: "x","z"
```
### Constant stub functions
To create a stub function that returns a constant, use the `createConstantMock` or `createNamedConstantMock` function.  
```haskell
spec :: Spec
spec = do
  it "createConstantMock" do
    m <- createConstantMock "foo"
    stubFn m `shouldBe` "foo"
    shouldApplyToAnything m
  it "createNamedConstantMock" do
    m <- createNamedConstantMock "const" "foo"
    stubFn m `shouldBe` "foo"
    shouldApplyToAnything m
```
### Flexible stub functions
Flexible stub functions can be generated by giving the `createStubFn` function a conditional expression rather than a concrete value.  
This can be used to return expected values for arbitrary values or strings that match a specific pattern.  
This is also true for the stub function when generating a mock of a monad type.
### any
any matches any value.
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat
import Prelude hiding (any)

spec :: Spec
spec = do
  it "any" do
    f <- createStubFn $ any |> "return value"
    f "something" `shouldBe` "return value"
```
Since a function with the same name is defined in Prelude, we use import Prelude hiding (any).
### Condition Expressions
Using the expect function, you can handle arbitrary condition expressions.  
The expect function takes a condition expression and a label.  
The label is used in the error message if the condition is not met.
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "expect" do
    f <- createStubFn $ expect (> 5) "> 5" |> "return value"
    f 6 `shouldBe` "return value"
```
### Condition Expressions without Labels
`expect_` is a label-free version of expect.  
The error message will show [some condition].
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "expect_" do
    f <- createStubFn $ expect_ (> 5) |> "return value"
    f 6 `shouldBe` "return value"
```
### Condition Expressions using Template Haskell
Using expectByExp, you can handle condition expressions as values of type Q Exp.  
The error message will include the string representation of the condition expression.
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "expectByExpr" do
    f <- createStubFn $ $(expectByExpr [|(> 5)|]) |> "return value"
    f 6 `shouldBe` "return value"
```
### Stub functions that return different values for each argument applied
By applying the `createStubFn` function to a list of x |> y format, you can create a stub function that returns a different value for each argument you apply.
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat
import Prelude hiding (and)

spec :: Spec
spec = do
  it "multi" do
    f <- createStubFn do
      onCase $ "a" |> "return x"
      onCase $ "b" |> "return y"
    f "a" `shouldBe` "return x"
    f "b" `shouldBe` "return y"
```
Alternatively, you can use the `cases` function.
```haskell
f <-
  createStubFn $
    cases
      [ "a" |> "return x",
        "b" |> "return y"
      ]
f "a" `shouldBe` "return x"
f "b" `shouldBe` "return y"
```
### Stub functions that return different values when applied to the same argument
When the `createStubFn` function is applied to a list of x |> y format, with the same arguments but different return values, you can create stub functions that return different values when applied to the same arguments.
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat
import GHC.IO (evaluate)

spec :: Spec
spec = do
  it "Return different values for the same argument" do
    f <- createStubFn $ do
      onCase $ "arg" |> "x"
      onCase $ "arg" |> "y"
    v1 <- evaluate $ f "arg"
    v2 <- evaluate $ f "arg"
    v3 <- evaluate $ f "arg"
    v1 `shouldBe` "x"
    v2 `shouldBe` "y"
    v3 `shouldBe` "y" -- After the second time, "y" is returned.
```
### Verify that expected arguments are applied
The `shouldApplyTo` function can be used to verify that a stub function has been applied to the expected arguments.  
If you want to verify this, you need to create a mock with the `createMock` function instead of the `createStubFn` function.  
In this case, stub functions are taken from the mock with the `stubFn` function.
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "stub & verify" do
    mock <- createMock $ "value" |> True
    let stubFunction = stubFn mock
    stubFunction "value" `shouldBe` True
    mock `shouldApplyTo` "value"
```
### Note
The record that it has been applied is made at the time the return value of the stub function is evaluated.  
Therefore, verification must occur after the return value is evaluated.
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "Verification does not work" do
    mock <- createMock $ "expect arg" |> "return value"
    let _ = stubFn mock "expect arg"
    mock `shouldApplyTo` "expect arg"
```
```console
uncaught exception: ErrorCall
Expected arguments were not applied to the function.
  expected: "expect arg"
  but got: Never been called.
```
### Verify the number of times the stub function was applied to the expected argument
The number of times a stub function is applied to an expected argument can be verified with the `shouldApplyTimes` function.
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "shouldApplyTimes" do
    m <- createMock $ "value" |> True
    print $ stubFn m "value"
    print $ stubFn m "value"
    m `shouldApplyTimes` (2 :: Int) `to` "value"
```
### Verify that a function has been applied to something
You can verify that a function has been applied to something with the `shouldApplyToAnything` function.
### Verify the number of times a function has been applied to something
The number of times a function has been applied to something can be verified with the `shouldApplyTimesToAnything` function.
### Verify that stub functions are applied in the expected order
The `shouldApplyInOrder` function can be used to verify that the order in which they were applied is the expected order.
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Type Applications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "shouldApplyInOrder" do
    m <- createMock $ any |> True |> ()
    print $ stubFn m "a" True
    print $ stubFn m "b" True
    m
      `shouldApplyInOrder` [ "a" |> True,
                             "b" |> True
                           ]
```
### Verify that they were applied in the expected order (partial match)
While the `shouldApplyInOrder` function verifies the exact order of application,  
The `shouldApplyInPartialOrder` function allows you to verify that the order of application is partially matched.
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "shouldApplyInPartialOrder" do
    m <- createMock $ any |> True |> ()
    print $ stubFn m "a" True
    print $ stubFn m "b" True
    print $ stubFn m "c" True
    m
      `shouldApplyInPartialOrder` [ "a" |> True,
                                    "c" |> True
                                  ]
```
<!-- END FULL REFERENCE -->

</details>