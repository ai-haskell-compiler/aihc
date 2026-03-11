<div align="center">
    <img src="logo.png" width="830px" align="center" style="object-fit: cover"/>
</div>

[![Latest release](http://img.shields.io/github/release/pujoheadsoft/mockcat.svg)](https://github.com/pujoheadsoft/mockcat/releases)
[![Test](https://github.com/pujoheadsoft/mockcat/workflows/Test/badge.svg)](https://github.com/pujoheadsoft/mockcat/actions?query=workflow%3ATest+branch%3Amain)
[![](https://img.shields.io/hackage/v/mockcat)](https://hackage.haskell.org/package/mockcat)


## 概要
mockcat は Haskell 向けの小さなモック / スタブ DSL です。  
`arg |> arg |> 戻り値` というシンプルな書き方で「こう呼ばれたらこう返す」を並べ、関数ならそのまま使い、型クラスなら Template Haskell (`makeMock`, `makePartialMock`) で生成したスタブ関数を `runMockT` の中で走らせると自動で検証まで行われます。

### 特徴
* シンプル: `arg |> ... |> 戻り値` でスタブ関数をすぐ作れる。
* 柔軟な戻り値: 同じ引数でも呼び出しごとで値を変えたり、引数別に振り分けたりできる。
* 型クラスのモックを生成: Template Haskell によりボイラープレートを削減。
* 型クラスの部分モック: 必要な関数だけ差し替え、残りは本物で動かすことができる。
* 並列処理への対応: スタブ関数を並列に呼び出しても、正確な呼び出し回数の検証が行える。

<details>
<summary>更新履歴</summary>

- **0.6.0**: 並列処理に対応
- **0.5.3.0**: MockT が MonadUnliftIO のインスタンスになった
- **0.5.0**: `IO a`型のスタブ関数が、適用される度に異なる値を返すことができるようになった
- **0.4.0**: 型クラスの部分的なモックを作れるようになった
- **0.3.0**: 型クラスのモックを作れるようになった
- **0.2.0**: スタブ関数が、同じ引数に対して異なる値を返せるようになった
- **0.1.0**: 1st release
</details>

## 例
スタブ関数
```haskell
-- create a stub function
stubFn <- createStubFn $ "value" |> True
-- assert
stubFn "value" `shouldBe` True
```
適用の検証
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
型クラス
```haskell
result <- runMockT do
  -- stub functions
  _readFile $ "input.txt" |> pack "content"
  _writeFile $ "output.txt" |> pack "content" |> ()
  -- sut
  program "input.txt" "output.txt"

result `shouldBe` ()
```
## スタブ関数の概要
スタブ関数は`createStubFn`関数で生成することができます。
`createStubFn`の引数は、適用が期待される引数を `|>` で連結したもので、`|>` の最後の値が関数の返り値となります。
```haskell
createStubFn $ (10 :: Int) |> "return value"
```
これは型クラスのモックにおけるスタブ関数の場合も同様です。
```haskell
runMockT do
  _readFile $ "input.txt" |> pack "content"
```
期待される引数は、条件として指定することもできます。
```haskell
-- Conditions other than exact match
createStubFn $ any |> "return value"
createStubFn $ expect (> 5) "> 5" |> "return value"
createStubFn $ expect_ (> 5) |> "return value"
createStubFn $ $(expectByExpr [|(> 5)|]) |> "return value"
```
また、引数に応じて返す値を変えることも可能です。
（同じ引数に対して、別の値を返すことも可能できます。）
```haskell
-- Parameterized Stub
createStubFn do
  onCase $ "a" |> "return x"
  onCase $ "b" |> "return y"
createStubFn do
  onCase $ "arg" |> "x"
  onCase $ "arg" |> "y"
```
## 検証の概要
スタブ関数の適用を検証するには、まず`createMock`関数でモックを作ります。
スタブ関数はモックから`stubFn`関数で取り出して使います。
検証はモックに対して行います。
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
スタブ関数と同様に検証の場合も条件を指定することができます。
```haskell
mock `shouldApplyTo` any @String
mock `shouldApplyTo` expect_ (/= "not value")
mock `shouldApplyTo` $(expectByExpr [|(/= "not value")|])
```
また適用された回数を検証することもできます。
```haskell
mock `shouldApplyTimes` (1 :: Int) `to` "value"
mock `shouldApplyTimesGreaterThan` (0 :: Int) `to` "value"
mock `shouldApplyTimesGreaterThanEqual` (1 :: Int) `to` "value"
mock `shouldApplyTimesLessThan` (2 :: Int) `to` "value"
mock `shouldApplyTimesLessThanEqual` (1 :: Int) `to` "value"
mock `shouldApplyTimesToAnything` (1 :: Int)
```
型クラスのモックの場合は、`runMockT`を適用した際、用意したスタブ関数の適用が行われたかの検証が自動で行われます。
```haskell
result <- runMockT do
  _readFile $ "input.txt" |> pack "Content"
  _writeFile $ "output.text" |> pack "Content" |> ()
  operationProgram "input.txt" "output.text"

result `shouldBe` ()
```
## 型クラスのモック
ここからはより詳しく説明していきます。
### 例
例えば次のようなモナド型クラス`FileOperation`と、`FileOperation`を使う`operationProgram`という関数が定義されているとします。
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

次のように`makeMock`関数を使うことで、型クラス`FileOperation`のモックを生成することができます。  
`makeMock [t|FileOperation|]`

生成されるのものは次の2つです。
1. 型クラス`FileOperation`の`MockT`インスタンス
2. 型クラス`FileOperation`に定義されている関数を元としたスタブ関数  
  スタブ関数は元の関数の接頭辞に`_`が付与された関数として生成されます。  
  この場合`_readFile`と`_writeFile`が生成されます。

モックは次のように使うことができます。
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
スタブ関数には、関数の適用が期待される引数を `|>` で連結して渡します。  
`|>` の最後の値が関数の返り値となります。

モックは`runMockT`で実行します。

### 検証
実行の後、スタブ関数が期待通りに適用されたか検証が行われます。  
例えば、上記の例のスタブ関数`_writeFile`の適用が期待される引数を`"content"`から`"edited content"`に書き換えてみます。
```haskell
result <- runMockT do
  _readFile ("input.txt" |> pack "content")
  _writeFile ("output.txt" |> pack "edited content" |> ())
  operationProgram "input.txt" "output.txt"
```
テストを実行すると、テストは失敗し、次のエラーメッセージが表示されます。
```console
uncaught exception: ErrorCall
function `_writeFile` was not applied to the expected arguments.
  expected: "output.txt","edited content"
  but got: "output.txt","content"
```

また次のようにテスト対象で使用している関数に対応するスタブ関数を使用しなかったとします。
```haskell
result <- runMockT do
  _readFile ("input.txt" |> pack "content")
  -- _writeFile ("output.txt" |> pack "content" |> ())
  operationProgram "input.txt" "output.txt"
```
この場合もテストを実行すると、テストは失敗し、次のエラーメッセージが表示されます。
```console
no answer found stub function `_writeFile`.
```

### 適用回数を検証
例えば、次のように特定の文字列を含んでいる場合は`writeFile`を適用させない場合のテストを書きたいとします。
```haskell
operationProgram inputPath outputPath = do
  content <- readFile inputPath
  unless (pack "ngWord" `isInfixOf` content) $
    writeFile outputPath content
```

これは次のように`expectApplyTimes`関数（旧: `applyTimesIs`）を使うことで実現できます。
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
`0`を指定することで適用されなかったことを検証できます。

あるいは`neverApply`関数を使うことで同じことが実現できます。
```haskell
result <- runMockT do
  _readFile ("input.txt" |> pack "contains ngWord")
  neverApply $ _writeFile ("output.txt" |> M.any |> ())
  operationProgram "input.txt" "output.txt"
```

`M.any`は任意の値にマッチするパラメーターです。
この例では`M.any`を使って、あらゆる値に対して`writeFile`関数が適用されないことを検証しています。

後述しますが、mockcatは`M.any`以外にも様々なパラメーターを用意しています。

### 定数関数のモック
mockcatは定数関数もモックにできます。
`MonadReader`をモックにし、`ask`のスタブ関数を使ってみます。
```haskell
data Environment = Environment { inputPath :: String, outputPath :: String }

operationProgram ::
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
ここで、`ask`を使わないようにしてみます。
```haskell
operationProgram = do
  content <- readFile "input.txt"
  writeFile "output.txt" content
```
するとテスト実行に失敗し、スタブ関数が適用されなかったことが表示されます。
```haskell
It has never been applied function `_ask`
```
### `IO a`型の値を返すモック
通常定数関数は同じ値を返しますが、`IO a`型の値を返すモックの場合のみ、適用する度に別の値を返すようなモックを作ることができます。
例えば型クラス`Teletype`とテスト対象の関数`echo`が定義されているとします。
`echo`は、`readTTY`が返す値によって異なる動作をします。
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
  `readTTY`が`""`以外を返した場合は、再帰的に呼び出されることを検証したいでしょう。
  そのためには、一度のテストの中で`readTTY`が異なる値を返せる必要があります。
  これを実現するためには、`implicitMonadicReturn`オプションを指定してモックを作ります。
  `implicitMonadicReturn`を使うことで、スタブ関数が明示的にモナディックな値を返せるようになります。
```haskell
makeMockWithOptions [t|Teletype|] options { implicitMonadicReturn = False }
```
これによりテストでは、`onCase`を使って、1回目の適用では`""`以外の値を返し、2回目の適用では`""`を返すような動作をさせることが可能になります。
```haskell
result <- runMockT do
  _readTTY $ do
    onCase $ pure @IO "a"
    onCase $ pure @IO ""

  _writeTTY $ "a" |> pure @IO ()
  echo
result `shouldBe` ()
```
### 部分的なモック
`makePartialMock`関数を使うと、型クラスに定義された関数の一部だけをモックにできます。

例えば次のような型クラスと関数があったとします。  
`getUserInput`がテスト対象の関数です。
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
この例では、一部本物の関数を使いたいので、次のように`IO`インスタンスを定義します。
```haskell
instance UserInputGetter IO where
  getInput = getLine
  toUserInput "" = pure Nothing
  toUserInput a = (pure . Just . UserInput) a
```
テストは次のようになります。
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

### スタブ関数の名前を変える
生成されるスタブ関数の接頭辞と接尾辞はオプションで変更することができます。  
例えば次のように指定すると、`stub_readFile_fn`と`stub_writeFile_fn`関数が生成されます。
```haskell
makeMockWithOptions [t|FileOperation|] options { prefix = "stub_", suffix = "_fn" }
```
オプションが指定されない場合はデフォルトで`_`になります。

### makeMockが生成するコード
使用する上で意識する必要はありませんが、`makeMock`関数は次のようなコードを生成します。
```haskell
-- MockTインスタンス
instance (Monad m) => FileOperation (MockT m) where
  readFile :: Monad m => FilePath -> MockT m Text
  writeFile :: Monad m => FilePath -> Text -> MockT m ()

_readFile :: (MockBuilder params (FilePath -> Text) (Param FilePath), Monad m) => params -> MockT m ()
_writeFile :: (MockBuilder params (FilePath -> Text -> ()) (Param FilePath :> Param Text), Monad m) => params -> MockT m ()
```

## 関数のモック
mockcatはモナド型クラスのモックだけでなく、通常の関数のモックを作ることもできます。  
モナド型のモックとは異なり、元になる関数は不要です。

### 使用例
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "使い方の例" do
    -- モックの生成("value"を適用すると、純粋な値Trueを返す)
    mock <- createMock $ "value" |> True

    -- モックからスタブ関数を取り出す
    let stubFunction = stubFn mock

    -- 関数の適用結果を検証
    stubFunction "value" `shouldBe` True

    -- 期待される値("value")が適用されたかを検証
    mock `shouldApplyTo` "value"

```

### スタブ関数
スタブ関数を直接作るには `createStubFn` 関数を使います。  
検証が不要な場合は、こちらを使うとよいでしょう。
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "スタブ関数を生成することができる" do
    -- 生成
    f <- createStubFn $ "param1" |> "param2" |> pure @IO ()

    -- 適用
    actual <- f "param1" "param2"

    -- 検証
    actual `shouldBe` ()
```
`createStubFn` 関数には、関数が適用されることを期待する引数を `|>` で連結して渡します。
`|>` の最後の値が関数の返り値となります。

スタブ関数が期待されていない引数に適用された場合はエラーとなります。
```console
uncaught exception: ErrorCall
Expected arguments were not applied to the function.
  expected: "value"
  but got: "valuo"
```
### 名前付きスタブ関数
スタブ関数には名前を付けることができます。
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
期待した引数に適用されなかった場合に出力されるエラーメッセージには、この名前が含まれるようになります。
```console
uncaught exception: ErrorCall
Expected arguments were not applied to the function `named stub`.
  expected: "x","y"
  but got: "x","z"
```

### 定数スタブ関数
定数を返すようなスタブ関数を作るには`createConstantMock`もしくは`createNamedConstantMock`関数を使います。  

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

### 柔軟なスタブ関数
`createStubFn` 関数に具体的な値ではなく、条件式を与えることで、柔軟なスタブ関数を生成できます。  
これを使うと、任意の値や、特定のパターンに合致する文字列などに対して期待値を返すことができます。  
これはモナド型のモックを生成した際のスタブ関数も同様です。
### any
`any` は任意の値にマッチします。
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
Preludeに同名の関数が定義されているため、`import Prelude hiding (any)`としています。

### 条件式
`expect`関数を使うと任意の条件式を扱えます。  
`expect`関数は条件式とラベルをとります。  
ラベルは条件式にマッチしなかった場合のエラーメッセージに使われます。
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

### ラベルなし条件式
`expect_` は `expect` のラベルなし版です。  
エラーメッセージには [some condition] と表示されます。

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

### Template Haskellを使った条件式
`expectByExp`を使うと、`Q Exp`型の値として条件式を扱えます。  
エラーメッセージには条件式を文字列化したものが使われます。
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

### 適用される引数ごとに異なる値を返すスタブ関数
`onCase`関数を使うと引数ごとに異なる値を返すスタブ関数を作れます。
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "multi" do
    f <- createStubFn do
      onCase $ "a" |> "return x"
      onCase $ "b" |> "return y"

    f "a" `shouldBe` "return x"
    f "b" `shouldBe` "return y"
```

### 同じ引数に適用されたとき異なる値を返すスタブ関数
`onCase`関数を使うとき、引数が同じで返り値が異なるようにすると、同じ引数に適用しても異なる値を返すスタブ関数を作れます。
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat
import GHC.IO (evaluate)

spec :: Spec
spec = do
  it "Return different values for the same argument" do
    f <- createStubFn do
      onCase $ "arg" |> "x"
      onCase $ "arg" |> "y"

    -- Do not allow optimization to remove duplicates.
    v1 <- evaluate $ f "arg"
    v2 <- evaluate $ f "arg"
    v3 <- evaluate $ f "arg"
    v1 `shouldBe` "x"
    v2 `shouldBe` "y"
    v3 `shouldBe` "y" -- After the second time, “y” is returned.
```
あるいは`cases`関数を使うこともできます。
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

## 検証
### 期待される引数に適用されたか検証する
期待される引数に適用されたかは `shouldApplyTo` 関数で検証することができます。  
検証を行う場合は、`createStubFn` 関数ではなく `createMock` 関数でモックを作る必要があります。
この場合スタブ関数は `stubFn` 関数でモックから取り出して使います。
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "stub & verify" do
    -- create a mock
    mock <- createMock $ "value" |> True
    -- stub function
    let stubFunction = stubFn mock
    -- assert
    stubFunction "value" `shouldBe` True
    -- verify
    mock `shouldApplyTo` "value"
```
### 注
適用されたという記録は、スタブ関数の返り値が評価される時点で行われます。  
したがって、検証は返り値の評価後に行う必要があります。
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "Verification does not work" do
    mock <- createMock $ "expect arg" |> "return value"
    -- 引数の適用は行うが返り値は評価しない
    let _ = stubFn mock "expect arg"
    mock `shouldApplyTo` "expect arg"
```
```console
uncaught exception: ErrorCall
Expected arguments were not applied to the function.
  expected: "expect arg"
  but got: Never been called.
```

### 期待される引数に適用された回数を検証する
期待される引数が適用された回数は `shouldApplyTimes` 関数で検証することができます。
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

### 何かしらに適用されたかを検証する
関数が何かしらに適用されたかは、`shouldApplyToAnything`関数で検証することができます。

### 何かしらに適用された回数を検証する
関数が何かしらに適用されたかの回数は、`shouldApplyTimesToAnything`関数で検証することができます。

### 期待される順序で適用されたかを検証する
期待される順序で適用されたかは `shouldApplyInOrder` 関数で検証することができます。
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
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

### 期待される順序で適用されたかを検証する(部分一致)
`shouldApplyInOrder` 関数は適用の順序を厳密に検証しますが、  
`shouldApplyInPartialOrder` 関数は適用の順序が部分的に一致しているかを検証することができます。
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