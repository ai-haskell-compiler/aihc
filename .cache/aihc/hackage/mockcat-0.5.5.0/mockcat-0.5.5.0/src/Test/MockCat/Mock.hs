{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- | This module provides the following functions.

  - Create mocks that can be stubbed and verified.

  - Create stub function.

  - Verify applied mock function.
-}
module Test.MockCat.Mock
  ( Mock,
    MockBuilder,
    build,
    createMock,
    createNamedMock,
    createConstantMock,
    createNamedConstantMock,
    createStubFn,
    createNamedStubFn,
    stubFn,
    shouldApplyTo,
    shouldApplyTimes,
    shouldApplyInOrder,
    shouldApplyInPartialOrder,
    shouldApplyTimesGreaterThanEqual,
    shouldApplyTimesLessThanEqual,
    shouldApplyTimesGreaterThan,
    shouldApplyTimesLessThan,
    shouldApplyToAnything,
    shouldApplyTimesToAnything,
    to,
    onCase,
    cases,
    casesIO
  )
where

import Control.Monad (guard, when, ap)
import Data.Function ((&))
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.List (elemIndex, intercalate)
import Data.Maybe
import Data.Text (pack, replace, unpack)
import GHC.IO (unsafePerformIO)
import Test.MockCat.Cons
import Test.MockCat.Param
import Test.MockCat.ParamDivider
import Test.MockCat.AssociationList (AssociationList, lookup, update, insert, empty, member)
import Prelude hiding (lookup)
import GHC.Stack (HasCallStack)
import Control.Monad.Trans
import Control.Monad.State

data Mock fun params = Mock (Maybe MockName) fun (Verifier params)

type MockName = String

newtype Verifier params = Verifier (IORef (AppliedRecord params))

{- | Create a mock.
From this mock, you can generate stub functions and verify the functions.

  @
  import Test.Hspec
  import Test.MockCat
  ...
  it "stub & verify" do
    -- create a mock
    m \<- createMock $ "value" |\> True
    -- stub function
    let f = stubFn m
    -- assert
    f "value" \`shouldBe\` True
    -- verify
    m \`shouldApplyTo\` "value"
  @

  If you do not need verification and only need stub functions, you can use @'mockFun'@.

-}
createMock ::
  MockBuilder params fun verifyParams =>
  MonadIO m =>
  params ->
  m (Mock fun verifyParams)
createMock params = liftIO $ build Nothing params

{- | Create a constant mock.
From this mock, you can generate constant functions and verify the functions.

  @
  import Test.Hspec
  import Test.MockCat
  ...
  it "stub & verify" do
    m \<- createConstantMock "foo"
    stubFn m \`shouldBe\` "foo"
    shouldApplyToAnything m
  @
-}
createConstantMock :: MonadIO m => a -> m (Mock a ())
createConstantMock a = liftIO $ build Nothing $ param a

{- | Create a named mock. If the test fails, this name is used. This may be useful if you have multiple mocks.

  @
  import Test.Hspec
  import Test.MockCat
  ...
  it "named mock" do
    m \<- createNamedMock "mock" $ "value" |\> True
    stubFn m "value" \`shouldBe\` True
  @
-}
createNamedMock ::
  MockBuilder params fun verifyParams =>
  MonadIO m =>
  MockName ->
  params ->
  m (Mock fun verifyParams)
createNamedMock name params = liftIO $ build (Just name) params

-- | Create a named constant mock.
createNamedConstantMock :: MonadIO m => MockName -> fun -> m (Mock fun ())
createNamedConstantMock name a = liftIO $ build (Just name) (param a)

-- | Extract the stub function from the mock.
stubFn :: Mock fun v -> fun
stubFn (Mock _ f _) = f

{- | Create a stub function.
  @
  import Test.Hspec
  import Test.MockCat
  ...
  it "stub function" do
    f \<- createStubFn $ "value" |\> True
    f "value" \`shouldBe\` True
  @
-}
createStubFn ::
  MockBuilder params fun verifyParams =>
  MonadIO m =>
  params ->
  m fun
createStubFn params = stubFn <$> createMock params

-- | Create a named stub function.
createNamedStubFn ::
  MockBuilder params fun verifyParams =>
  MonadIO m =>
  String ->
  params ->
  m fun
createNamedStubFn name params = stubFn <$> createNamedMock name params

-- | Class for creating a mock corresponding to the parameter.
class MockBuilder params fun verifyParams | params -> fun, params -> verifyParams where
  -- build a mock
  build :: MonadIO m => Maybe MockName -> params -> m (Mock fun verifyParams)

-- instances
instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h, Show i, Eq i) =>
  MockBuilder
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h :> Param i :> Param r)
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h :> Param i)
  where
  build name params = do
    s <- liftIO $ newIORef appliedRecord
    makeMock
      name
      s
      ( \a2 b2 c2 d2 e2 f2 g2 h2 i2 ->
          unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2 :> p i2) s
      )

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h) =>
  MockBuilder
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h :> Param r)
    (a -> b -> c -> d -> e -> f -> g -> h -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h)
  where
  build name params = do
    s <- liftIO $ newIORef appliedRecord
    makeMock
      name
      s
      ( \a2 b2 c2 d2 e2 f2 g2 h2 ->
          unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2) s
      )

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g) =>
  MockBuilder
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param r)
    (a -> b -> c -> d -> e -> f -> g -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g)
  where
  build name params = do
    s <- liftIO $ newIORef appliedRecord
    makeMock
      name
      s
      ( \a2 b2 c2 d2 e2 f2 g2 ->
          unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2) s
      )

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f) =>
  MockBuilder
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param r)
    (a -> b -> c -> d -> e -> f -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f)
  where
  build name params = do
    s <- liftIO $ newIORef appliedRecord
    makeMock name s (\a2 b2 c2 d2 e2 f2 -> unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e) =>
  MockBuilder
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param r)
    (a -> b -> c -> d -> e -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e)
  where
  build name params = do
    s <- liftIO $ newIORef appliedRecord
    makeMock name s (\a2 b2 c2 d2 e2 -> unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d) =>
  MockBuilder
    (Param a :> Param b :> Param c :> Param d :> Param r)
    (a -> b -> c -> d -> r)
    (Param a :> Param b :> Param c :> Param d)
  where
  build name params = do
    s <- liftIO $ newIORef appliedRecord
    makeMock name s (\a2 b2 c2 d2 -> unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2 :> p d2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c) =>
  MockBuilder (Param a :> Param b :> Param c :> Param r) (a -> b -> c -> r) (Param a :> Param b :> Param c)
  where
  build name params = do
    s <- liftIO $ newIORef appliedRecord
    makeMock name s (\a2 b2 c2 -> unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2) s)

instance
  (Show a, Eq a, Show b, Eq b) =>
  MockBuilder (Param a :> Param b :> Param r) (a -> b -> r) (Param a :> Param b)
  where
  build name params = do
    s <- liftIO $ newIORef appliedRecord
    makeMock name s (\a2 b2 -> unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2) s)

instance
  (Show a, Eq a) =>
  MockBuilder (Param a :> Param r) (a -> r) (Param a)
  where
  build name params = do
    s <- liftIO $ newIORef appliedRecord
    makeMock name s (\a2 -> unsafePerformIO $ extractReturnValueWithValidate name params (p a2) s)

instance
  MockBuilder (Param r) r ()
  where
  build name params = do
    s <- liftIO $ newIORef appliedRecord
    let v = value params
    makeMock name s $ unsafePerformIO (do
      liftIO $ appendAppliedParams s ()
      pure v)

instance
  MockBuilder (IO r) (IO r) ()
  where
  build name a = do
    s <- liftIO $ newIORef appliedRecord
    makeMock name s (do
      liftIO $ appendAppliedParams s ()
      a)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h, Show i, Eq i) =>
  MockBuilder
    (Cases (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h :> Param i :> Param r) ())
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h :> Param i)
  where
  build name cases = do
    let params = runCase cases
    s <- liftIO $ newIORef appliedRecord
    makeMock name s (\a2 b2 c2 d2 e2 f2 g2 h2 i2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2 :> p i2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h) =>
  MockBuilder
    (Cases (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h :> Param r) ())
    (a -> b -> c -> d -> e -> f -> g -> h -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h)
  where
  build name cases = do
    let params = runCase cases
    s <- liftIO $ newIORef appliedRecord
    makeMock name s (\a2 b2 c2 d2 e2 f2 g2 h2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g) =>
  MockBuilder
    (Cases (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param r) ())
    (a -> b -> c -> d -> e -> f -> g -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g)
  where
  build name cases = do
    let params = runCase cases
    s <- liftIO $ newIORef appliedRecord
    makeMock name s (\a2 b2 c2 d2 e2 f2 g2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f) =>
  MockBuilder
    (Cases (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param r) ())
    (a -> b -> c -> d -> e -> f -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f)
  where
  build name cases = do
    let params = runCase cases
    s <- liftIO $ newIORef appliedRecord
    makeMock name s (\a2 b2 c2 d2 e2 f2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e) =>
  MockBuilder
    (Cases (Param a :> Param b :> Param c :> Param d :> Param e :> Param r) ())
    (a -> b -> c -> d -> e -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e)
  where
  build name cases = do
    let params = runCase cases
    s <- liftIO $ newIORef appliedRecord
    makeMock name s (\a2 b2 c2 d2 e2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d) =>
  MockBuilder
    (Cases (Param a :> Param b :> Param c :> Param d :> Param r) ())
    (a -> b -> c -> d -> r)
    (Param a :> Param b :> Param c :> Param d)
  where
  build name cases = do
    let params = runCase cases
    s <- liftIO $ newIORef appliedRecord
    makeMock name s (\a2 b2 c2 d2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c) =>
  MockBuilder
    (Cases (Param a :> Param b :> Param c :> Param r) ())
    (a -> b -> c -> r)
    (Param a :> Param b :> Param c)
  where
  build name cases = do
    let params = runCase cases
    s <- liftIO $ newIORef appliedRecord
    makeMock name s (\a2 b2 c2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2) s)

instance
  (Show a, Eq a, Show b, Eq b) =>
  MockBuilder (Cases (Param a :> Param b :> Param r) ()) (a -> b -> r) (Param a :> Param b)
  where
  build name cases = do
    let params = runCase cases
    s <- liftIO $ newIORef appliedRecord
    makeMock name s (\a2 b2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2) s)

instance
  (Show a, Eq a) =>
  MockBuilder (Cases (Param a :> Param r) ()) (a -> r) (Param a)
  where
  build name cases = do
    let params = runCase cases
    s <- liftIO $ newIORef appliedRecord
    makeMock name s (\a2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2) s)

instance
  MockBuilder (Cases (IO a) ()) (IO a) ()
  where
  build name cases = do
    let params = runCase cases
    s <- liftIO $ newIORef appliedRecord
    makeMock name s (do
      count <- readAppliedCount s ()
      let index = min count (length params - 1)
          r = safeIndex params index
      appendAppliedParams s ()
      incrementAppliedParamCount s ()
      fromJust r)
-- ------

p :: a -> Param a
p = param

makeMock :: MonadIO m => Maybe MockName -> IORef (AppliedRecord params) -> fun -> m (Mock fun params)
makeMock name l fn = pure $ Mock name fn (Verifier l)

extractReturnValueWithValidate ::
  ParamDivider params args (Param r) =>
  Eq args =>
  Show args =>
  Maybe MockName ->
  params ->
  args ->
  IORef (AppliedRecord args) ->
  IO r
extractReturnValueWithValidate name params inputParams s = do
  validateWithStoreParams name s (args params) inputParams
  pure $ returnValue params

findReturnValueWithStore ::
  Eq args =>
  Show args =>
  ParamDivider params args (Param r) =>
  Maybe MockName ->
  AppliedParamsList params ->
  args ->
  IORef (AppliedRecord args) ->
  IO r
findReturnValueWithStore name paramsList inputParams ref = do
  appendAppliedParams ref inputParams
  let expectedArgs = args <$> paramsList
  r <- findReturnValue paramsList inputParams ref
  maybe
    (errorWithoutStackTrace $ messageForMultiMock name expectedArgs inputParams)
    pure
    r

findReturnValue ::
  Eq args =>
  ParamDivider params args (Param r) =>
  AppliedParamsList params ->
  args ->
  IORef (AppliedRecord args) ->
  IO (Maybe r)
findReturnValue paramsList inputParams ref = do
  let matchedParams = filter (\params -> args params == inputParams) paramsList
  case matchedParams of
    [] -> pure Nothing
    _ -> do
      count <- readAppliedCount ref inputParams
      let index = min count (length matchedParams - 1)
      incrementAppliedParamCount ref inputParams
      pure $ returnValue <$> safeIndex matchedParams index

validateWithStoreParams :: (Eq a, Show a) => Maybe MockName -> IORef (AppliedRecord a) -> a -> a -> IO ()
validateWithStoreParams name ref expected actual = do
  validateParams name expected actual
  appendAppliedParams ref actual

validateParams :: (Eq a, Show a) => Maybe MockName -> a -> a -> IO ()
validateParams name expected actual =
  if expected == actual
    then pure ()
    else errorWithoutStackTrace $ message name expected actual

message :: Show a => Maybe MockName -> a -> a -> String
message name expected actual =
  intercalate
    "\n"
    [ "function" <> mockNameLabel name <> " was not applied to the expected arguments.",
      "  expected: " <> show expected,
      "   but got: " <> show actual
    ]

messageForMultiMock :: Show a => Maybe MockName -> [a] -> a -> String
messageForMultiMock name expecteds actual =
  intercalate
    "\n"
    [ "function" <> mockNameLabel name <> " was not applied to the expected arguments.",
      "  expected one of the following:",
      intercalate "\n" $ ("    " <>) . show <$> expecteds,
      "  but got:",
      ("    " <>) . show $ actual
    ]

mockNameLabel :: Maybe MockName -> String
mockNameLabel = maybe mempty (" " <>) . enclose "`"

enclose :: String -> Maybe String -> Maybe String
enclose e = fmap (\v -> e <> v <> e)

-- verify
data VerifyMatchType a = MatchAny a | MatchAll a

-- | Class for verifying mock function.
class Verify params input where
  -- | Verifies that the function has been applied to the expected arguments.
  shouldApplyTo :: HasCallStack => Mock fun params -> input -> IO ()

instance (Eq a, Show a) => Verify (Param a) a where
  shouldApplyTo v a = verify v (MatchAny (param a))

instance (Eq a, Show a) => Verify a a where
  shouldApplyTo v a = verify v (MatchAny a)

verify :: (Eq params, Show params) => Mock fun params -> VerifyMatchType params -> IO ()
verify (Mock name _ (Verifier ref)) matchType = do
  appliedParamsList <- readAppliedParamsList ref
  let result = doVerify name appliedParamsList matchType
  result & maybe (pure ()) (\(VerifyFailed msg) -> errorWithoutStackTrace msg)

newtype VerifyFailed = VerifyFailed Message

type Message = String

doVerify :: (Eq a, Show a) => Maybe MockName -> AppliedParamsList a -> VerifyMatchType a -> Maybe VerifyFailed
doVerify name list (MatchAny a) = do
  guard $ notElem a list
  pure $ verifyFailedMessage name list a
doVerify name list (MatchAll a) = do
  guard $ Prelude.any (a /=) list
  pure $ verifyFailedMessage name list a

verifyFailedMessage :: Show a => Maybe MockName -> AppliedParamsList a -> a -> VerifyFailed
verifyFailedMessage name appliedParams expected =
  VerifyFailed $
    intercalate
      "\n"
      [ "function" <> mockNameLabel name <> " was not applied to the expected arguments.",
        "  expected: " <> show expected,
        "   but got: " <> formatAppliedParamsList appliedParams
      ]

formatAppliedParamsList :: Show a => AppliedParamsList a -> String
formatAppliedParamsList appliedParams
  | length appliedParams == 0 = "It has never been applied"
  | length appliedParams == 1 = init . drop 1 . show $ appliedParams
  | otherwise = show appliedParams

_replace :: Show a => String -> a -> String
_replace r s = unpack $ replace (pack r) (pack "") (pack (show s))

class VerifyCount countType params a where
  -- | Verify the number of times a function has been applied to an argument.
  --
  -- @
  -- import Test.Hspec
  -- import Test.MockCat
  -- ...
  -- it "verify to applied times." do
  --   m \<- createMock $ "value" |\> True
  --   print $ stubFn m "value"
  --   print $ stubFn m "value"
  --   m \`shouldApplyTimes\` (2 :: Int) \`to\` "value" 
  -- @
  --
  shouldApplyTimes :: HasCallStack => Eq params => Mock fun params -> countType -> a -> IO ()

instance VerifyCount CountVerifyMethod (Param a) a where
  shouldApplyTimes v count a = verifyCount v (param a) count

instance VerifyCount Int (Param a) a where
  shouldApplyTimes v count a = verifyCount v (param a) (Equal count)

instance {-# OVERLAPPABLE #-} VerifyCount CountVerifyMethod a a where
  shouldApplyTimes v count a = verifyCount v a count

instance {-# OVERLAPPABLE #-} VerifyCount Int a a where
  shouldApplyTimes v count a = verifyCount v a (Equal count)

data CountVerifyMethod
  = Equal Int
  | LessThanEqual Int
  | GreaterThanEqual Int
  | LessThan Int
  | GreaterThan Int

instance Show CountVerifyMethod where
  show (Equal e) = show e
  show (LessThanEqual e) = "<= " <> show e
  show (LessThan e) = "< " <> show e
  show (GreaterThanEqual e) = ">= " <> show e
  show (GreaterThan e) = "> " <> show e

compareCount :: CountVerifyMethod -> Int -> Bool
compareCount (Equal e) a = a == e
compareCount (LessThanEqual e) a = a <= e
compareCount (LessThan e) a = a < e
compareCount (GreaterThanEqual e) a = a >= e
compareCount (GreaterThan e) a = a > e

verifyCount :: Eq params => Mock fun params -> params -> CountVerifyMethod -> IO ()
verifyCount (Mock name _ (Verifier ref)) v method = do
  appliedParamsList <- readAppliedParamsList ref
  let appliedCount = length (filter (v ==) appliedParamsList)
  if compareCount method appliedCount
    then pure ()
    else
      errorWithoutStackTrace $
        intercalate
          "\n"
          [ "function" <> mockNameLabel name <> " was not applied the expected number of times to the expected arguments.",
            "  expected: " <> show method,
            "   but got: " <> show appliedCount
          ]

to :: (a -> IO ()) -> a -> IO ()
to f = f

class VerifyOrder params input where
  -- | Verify functions are applied in the expected order.
  --
  -- @
  -- import Test.Hspec
  -- import Test.MockCat
  -- import Prelude hiding (any)
  -- ...
  -- it "verify order of apply" do
  --   m \<- createMock $ any |\> True |\> ()
  --   print $ stubFn m "a" True
  --   print $ stubFn m "b" True
  --   m \`shouldApplyInOrder\` ["a" |\> True, "b" |\> True]
  -- @
  shouldApplyInOrder :: HasCallStack => Mock fun params -> [input] -> IO ()

  -- | Verify that functions are applied in the expected order.
  --
  -- Unlike @'shouldApplyInOrder'@, not all applications need to match exactly.
  --
  -- As long as the order matches, the verification succeeds.
  shouldApplyInPartialOrder :: HasCallStack => Mock fun params -> [input] -> IO ()

instance (Eq a, Show a) => VerifyOrder (Param a) a where
  shouldApplyInOrder v a = verifyOrder ExactlySequence v $ param <$> a
  shouldApplyInPartialOrder v a = verifyOrder PartiallySequence v $ param <$> a

instance {-# OVERLAPPABLE #-} (Eq a, Show a) => VerifyOrder a a where
  shouldApplyInOrder = verifyOrder ExactlySequence
  shouldApplyInPartialOrder = verifyOrder PartiallySequence

data VerifyOrderMethod
  = ExactlySequence
  | PartiallySequence

verifyOrder ::
  Eq params =>
  Show params =>
  VerifyOrderMethod ->
  Mock fun params ->
  [params] ->
  IO ()
verifyOrder method (Mock name _ (Verifier ref)) matchers = do
  appliedParamsList <- readAppliedParamsList ref
  let result = doVerifyOrder method name appliedParamsList matchers
  result & maybe (pure ()) (\(VerifyFailed msg) -> errorWithoutStackTrace msg)

doVerifyOrder ::
  Eq a =>
  Show a =>
  VerifyOrderMethod ->
  Maybe MockName ->
  AppliedParamsList a ->
  [a] ->
  Maybe VerifyFailed
doVerifyOrder ExactlySequence name appliedValues expectedValues
  | length appliedValues /= length expectedValues = do
      pure $ verifyFailedOrderParamCountMismatch name appliedValues expectedValues
  | otherwise = do
      let unexpectedOrders = collectUnExpectedOrder appliedValues expectedValues
      guard $ length unexpectedOrders > 0
      pure $ verifyFailedSequence name unexpectedOrders
doVerifyOrder PartiallySequence name appliedValues expectedValues
  | length appliedValues < length expectedValues = do
      pure $ verifyFailedOrderParamCountMismatch name appliedValues expectedValues
  | otherwise = do
      guard $ isOrderNotMatched appliedValues expectedValues
      pure $ verifyFailedPartiallySequence name appliedValues expectedValues

verifyFailedPartiallySequence :: Show a => Maybe MockName -> AppliedParamsList a -> [a] -> VerifyFailed
verifyFailedPartiallySequence name appliedValues expectedValues =
  VerifyFailed $
    intercalate
      "\n"
      [ "function" <> mockNameLabel name <> " was not applied to the expected arguments in the expected order.",
        "  expected order:",
        intercalate "\n" $ ("    " <>) . show <$> expectedValues,
        "  but got:",
        intercalate "\n" $ ("    " <>) . show <$> appliedValues
      ]

isOrderNotMatched :: Eq a => AppliedParamsList a -> [a] -> Bool
isOrderNotMatched appliedValues expectedValues =
  isNothing $
    foldl
      ( \candidates e -> do
          candidates >>= \c -> do
            index <- elemIndex e c
            Just $ drop (index + 1) c
      )
      (Just appliedValues)
      expectedValues

verifyFailedOrderParamCountMismatch :: Maybe MockName -> AppliedParamsList a -> [a] -> VerifyFailed
verifyFailedOrderParamCountMismatch name appliedValues expectedValues =
  VerifyFailed $
    intercalate
      "\n"
      [ "function" <> mockNameLabel name <> " was not applied to the expected arguments in the expected order (count mismatch).",
        "  expected: " <> show (length expectedValues),
        "   but got: " <> show (length appliedValues)
      ]

verifyFailedSequence :: Show a => Maybe MockName -> [VerifyOrderResult a] -> VerifyFailed
verifyFailedSequence name fails =
  VerifyFailed $
    intercalate
      "\n"
      ( ("function" <> mockNameLabel name <> " was not applied to the expected arguments in the expected order.") : (verifyOrderFailedMesssage <$> fails)
      )

verifyOrderFailedMesssage :: Show a => VerifyOrderResult a -> String
verifyOrderFailedMesssage VerifyOrderResult {index, appliedValue, expectedValue} =
  let appliedCount = showHumanReadable (index + 1)
   in intercalate
        "\n"
        [ "  expected " <> appliedCount <> " applied: " <> show expectedValue,
          "   but got " <> appliedCount <> " applied: " <> show appliedValue
        ]
  where
    showHumanReadable :: Int -> String
    showHumanReadable 1 = "1st"
    showHumanReadable 2 = "2nd"
    showHumanReadable 3 = "3rd"
    showHumanReadable n = show n <> "th"

data VerifyOrderResult a = VerifyOrderResult
  { index :: Int,
    appliedValue :: a,
    expectedValue :: a
  }

collectUnExpectedOrder :: Eq a => AppliedParamsList a -> [a] -> [VerifyOrderResult a]
collectUnExpectedOrder appliedValues expectedValues =
  catMaybes $
    mapWithIndex
      ( \i expectedValue -> do
          let appliedValue = appliedValues !! i
          guard $ expectedValue /= appliedValue
          pure VerifyOrderResult {index = i, appliedValue, expectedValue}
      )
      expectedValues

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f xs = [f i x | (i, x) <- zip [0 ..] xs]

-- | Verify that the function has been applied to the expected arguments at least the expected number of times.
shouldApplyTimesGreaterThanEqual ::
  VerifyCount CountVerifyMethod params a =>
  Eq params =>
  Mock fun params ->
  Int ->
  a ->
  IO ()
shouldApplyTimesGreaterThanEqual m i = shouldApplyTimes m (GreaterThanEqual i)

-- | Verify that the function is applied to the expected arguments less than or equal to the expected number of times.
shouldApplyTimesLessThanEqual ::
  VerifyCount CountVerifyMethod params a =>
  Eq params =>
  Mock fun params ->
  Int ->
  a ->
  IO ()
shouldApplyTimesLessThanEqual m i = shouldApplyTimes m (LessThanEqual i)

-- | Verify that the function has been applied to the expected arguments a greater number of times than expected.
shouldApplyTimesGreaterThan ::
  VerifyCount CountVerifyMethod params a =>
  Eq params =>
  Mock fun params ->
  Int ->
  a ->
  IO ()
shouldApplyTimesGreaterThan m i = shouldApplyTimes m (GreaterThan i)

-- | Verify that the function has been applied to the expected arguments less than the expected number of times.
shouldApplyTimesLessThan ::
  VerifyCount CountVerifyMethod params a =>
  Eq params =>
  Mock fun params ->
  Int ->
  a ->
  IO ()
shouldApplyTimesLessThan m i = shouldApplyTimes m (LessThan i)

type AppliedParamsList params = [params]
type AppliedParamsCounter params = AssociationList params Int

data AppliedRecord params = AppliedRecord {
  appliedParamsList :: AppliedParamsList params,
  appliedParamsCounter :: AppliedParamsCounter params
}

appliedRecord :: AppliedRecord params
appliedRecord = AppliedRecord {
  appliedParamsList = mempty,
  appliedParamsCounter = empty
}

readAppliedParamsList :: IORef (AppliedRecord params) -> IO (AppliedParamsList params)
readAppliedParamsList ref = do
  record <- readIORef ref
  pure $ appliedParamsList record

readAppliedCount :: Eq params => IORef (AppliedRecord params) -> params -> IO Int
readAppliedCount ref params = do
  record <- readIORef ref
  let count = appliedParamsCounter record
  pure $ fromMaybe 0 (lookup params count)

appendAppliedParams :: IORef (AppliedRecord params) -> params -> IO ()
appendAppliedParams ref inputParams = do
  atomicModifyIORef' ref (\AppliedRecord {appliedParamsList, appliedParamsCounter} ->
    let newRecord = AppliedRecord {
          appliedParamsList = appliedParamsList ++ [inputParams],
          appliedParamsCounter = appliedParamsCounter
        }
    in (newRecord, ()))

incrementAppliedParamCount :: Eq params => IORef (AppliedRecord params) -> params -> IO ()
incrementAppliedParamCount ref inputParams = do
  atomicModifyIORef' ref (\AppliedRecord {appliedParamsList, appliedParamsCounter} ->
    let newRecord = AppliedRecord {
          appliedParamsList = appliedParamsList,
          appliedParamsCounter = incrementCount inputParams appliedParamsCounter
        }
    in (newRecord, ()))

incrementCount :: Eq k => k -> AppliedParamsCounter k -> AppliedParamsCounter k
incrementCount key list =
  if member key list then update (+ 1) key list
  else insert key 1 list

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs n
  | n < 0 = Nothing
  | otherwise = listToMaybe (drop n xs)

-- | Verify that it was apply to anything.
shouldApplyToAnything :: HasCallStack => Mock fun params -> IO ()
shouldApplyToAnything (Mock name _ (Verifier ref)) = do
  appliedParamsList <- readAppliedParamsList ref
  when (null appliedParamsList) $ error $ "It has never been applied function" <> mockNameLabel name

-- | Verify that it was apply to anything (times).
shouldApplyTimesToAnything :: Mock fun params -> Int -> IO ()
shouldApplyTimesToAnything (Mock name _ (Verifier ref)) count = do
  appliedParamsList <- readAppliedParamsList ref
  let appliedCount = length appliedParamsList
  when (count /= appliedCount) $
        errorWithoutStackTrace $
        intercalate
          "\n"
          [ "function" <> mockNameLabel name <> " was not applied the expected number of times.",
            "  expected: " <> show count,
            "   but got: " <> show appliedCount
          ]

newtype Cases a b = Cases (State [a] b)

instance Functor (Cases a) where
  fmap f (Cases s) = Cases (fmap f s)

instance Applicative (Cases a) where
  pure x = Cases $ pure x
  (<*>) = ap

instance Monad (Cases a) where
  (Cases m) >>= f = Cases $ do
    result <- m
    let (Cases newState) = f result
    newState

runCase :: Cases a b -> [a]
runCase (Cases s) = execState s []

{- | Make a case for stub functions.  
This can be used to create stub functions that return different values depending on their arguments.

  @
  it "test" do
    f <-
      createStubFn $ do
        onCase $ "a" |> "return x"
        onCase $ "b" |> "return y"

    f "a" `shouldBe` "return x"
    f "b" `shouldBe` "return y"
  @
-}
onCase :: a -> Cases a ()
onCase a = Cases $ do
  st <- get
  put (st ++ [a])

{- | Make a list of patterns of arguments and returned values.  
This can be used to create stub functions that return different values depending on their arguments.

  @
  it "test" do
    f <-
      createStubFn $ cases [
        "a" |> "return x",
        "b" |> "return y"
      ]

    f "a" `shouldBe` "return x"
    f "b" `shouldBe` "return y"
  @
-}
cases :: [a] -> Cases a ()
cases a = Cases $ put a

{- | IO version of @'cases'@.  
@casesIO ["a", ""]@ has the same meaning as @cases [ pure \@IO "a", pure \@IO ""]@.
-}
casesIO :: [a] -> Cases (IO a) ()
casesIO = Cases . (put . map pure)