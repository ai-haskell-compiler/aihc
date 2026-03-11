{-# LANGUAGE CPP #-}
module Test.Hspec.NeedEnvSpec (main, spec) where

import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf)
import Data.IORef (IORef, newIORef, modifyIORef, readIORef)
import System.SetEnv (setEnv, unsetEnv)
import Test.Hspec
import Test.Hspec.Core.Runner (hspecWithResult, Summary(..), Config(..), defaultConfig)
import Test.Hspec.Core.Formatters (Formatter)
import qualified Test.Hspec.Core.Formatters as F
import Test.Hspec.Core.Util (Path)

import Test.Hspec.NeedEnv
  ( EnvMode(..), needEnv, needEnvRead, needEnvHostPort
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "needEnv Need" $ do
    it "should get the specified env" $ do
      setEnv "HOGE" "hoge"
      successCase (needEnv Need) "HOGE" "hoge"
    it "should fail when the specified env is not present" $ do
      unsetEnv "HOGE"
      failCase (needEnv Need) "HOGE" (\msg -> "not set" `isInfixOf` msg)
  describe "needEnvRead Need" $ do
    it "should get and parse the specified env" $ do
      setEnv "FOOBAR" "100"
      successCase (needEnvRead Need) "FOOBAR" (100 :: Int)
    it "should fail when it fails to parse" $ do
      let needEnvInt :: String -> IO Int
          needEnvInt = needEnvRead Need
      setEnv "FOOBAR" "hoge"
      failCase needEnvInt "FOOBAR" (\msg -> "parse" `isInfixOf` msg)
  describe "needEnv Want" $ do
    it "should get the specified env" $ do
      setEnv "FOOBAR" "foo_bar"
      successCase (needEnv Want) "FOOBAR" "foo_bar"
    it "should make the test pending when the specified env is not present" $ do
      unsetEnv "FOOBAR"
      pendingCase (needEnv Want) "FOOBAR" (\msg -> "not set" `isInfixOf` msg)
  describe "needEnvRead Want" $ do
    it "should fail (even if it's Want) when it fails to parse the env" $ do
      let needEnvInt :: String -> IO Int
          needEnvInt = needEnvRead Want
      setEnv "FOOBAR" "hoge"
      failCase needEnvInt "FOOBAR" (\msg -> "parse" `isInfixOf` msg)
  describe "needEnvHostPort Need" $ do
    it "should get host and port from environment variables the common name prefix" $ do
      (conf, ref_results) <- configForTest
      setEnv "PREFIX_HOGE_HOST" "192.168.17.223"
      setEnv "PREFIX_HOGE_PORT" "1123"
      got_summary <- hspecWithResult conf $ before (needEnvHostPort Need "PREFIX_HOGE") $ do
        specify "dummny" $ \(host, port) -> do
          host `shouldBe` "192.168.17.223"
          port `shouldBe` 1123
      got <- readIORef ref_results
      map resultBody got `shouldBe` [ExampleSuccess]
      got_summary `shouldBe` Summary { summaryExamples = 1, summaryFailures = 0 }

successCase :: (Read a, Show a, Eq a) => (String -> IO a) -> String -> a -> IO ()
successCase envGet envkey envval = do
  (conf, ref_results) <- configForTest
  got_summary <- hspecWithResult conf (before (envGet envkey) $ sampleSpec envval)
  got <- readIORef ref_results
  map resultBody got `shouldBe` [ExampleSuccess]
  got_summary `shouldBe` Summary { summaryExamples = 1, summaryFailures = 0 }

failOrPendingCase :: (ExampleResultBody -> Maybe String) -> (String -> IO a) -> String -> (String -> Bool) -> Int -> IO ()
failOrPendingCase extractMsg envGet envkey andExpect exp_sum_failures = do
  let expect [body] = case extractMsg body of
        Nothing -> False
        Just msg -> (envkey `isInfixOf` msg) && andExpect msg
      expect _ = False
  (conf, ref_results) <- configForTest
  got_summary <- hspecWithResult conf (before (envGet envkey) sampleSuccess)
  got <- fmap (map resultBody) $ readIORef ref_results
  got `shouldSatisfy` expect
  got_summary `shouldBe` Summary { summaryExamples = 1, summaryFailures = exp_sum_failures }


failCase :: (String -> IO a) -> String -> (String -> Bool) -> IO ()
failCase envGet envkey andExpect = failOrPendingCase extract envGet envkey andExpect 1
  where
    extract (ExampleFailure mmsg) = mmsg
    extract _ = Nothing

pendingCase :: (String -> IO a) -> String -> (String -> Bool) -> IO ()
pendingCase envGet envkey andExpect = failOrPendingCase extract envGet envkey andExpect 0
  where
    extract (ExamplePending mmsg) = mmsg
    extract _ = Nothing

sampleSpec :: (Eq a, Show a) => a -> SpecWith a
sampleSpec expected = specify "dummy" $ \got -> got `shouldBe` expected

sampleSuccess :: SpecWith a
sampleSuccess = specify "dummy" $ const (True `shouldBe` True)

data ExampleResultBody = ExampleSuccess
                       | ExampleFailure (Maybe String)
                       | ExamplePending (Maybe String)
                       deriving (Show,Eq,Ord)

type ExampleResult = (Path, ExampleResultBody)

resultBody :: ExampleResult -> ExampleResultBody
resultBody (_, b) = b

#if MIN_VERSION_hspec_core(2,4,0)
failureReasonToString :: Either e F.FailureReason -> Maybe String
failureReasonToString (Right (F.Reason s)) = Just s
failureReasonToString _ = Nothing
#else
failureReasonToString :: Either e String -> Maybe String
failureReasonToString (Right s) = Just s
failureReasonToString _ = Nothing
#endif

formatterForTest :: IO (Formatter, IORef [ExampleResult])
formatterForTest = do
  ref_results <- newIORef []
  let putRet r = liftIO $ modifyIORef ref_results (\rs -> rs ++ [r])
#if MIN_VERSION_hspec_core(2,5,0)
      f = F.silent { F.exampleSucceeded = \p _ -> putRet (p, ExampleSuccess),
                     F.exampleFailed = \p _ fr -> putRet (p, ExampleFailure (failureReasonToString $ Right fr)),
                     F.examplePending = \p _ ms -> putRet (p, ExamplePending ms)
                   }
#else
      f = F.silent { F.exampleSucceeded = \p -> putRet (p, ExampleSuccess),
                     F.exampleFailed = \p efr -> putRet (p, ExampleFailure (failureReasonToString efr)),
                     F.examplePending = \p ms -> putRet (p, ExamplePending ms)
                   }
#endif
  return (f, ref_results)

configForTest :: IO (Config, IORef [ExampleResult])
configForTest = do
  (fmt, ref_results) <- formatterForTest
  let conf = defaultConfig { -- configIgnoreConfigFile = True, -- not in hspec-core-2.3.2
                             configDryRun = False,
                             configFormatter = Just fmt
                           }
  return (conf, ref_results)
