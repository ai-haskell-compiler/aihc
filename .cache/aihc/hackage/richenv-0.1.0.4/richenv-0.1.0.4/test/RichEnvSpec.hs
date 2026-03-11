module RichEnvSpec (spec) where

import ArbitraryInstances ()
import Data.Aeson qualified as JSON
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C8
import Data.HashMap.Strict qualified as HM
import Data.List (sort)
import Data.Text qualified as T
import Data.YAML (Pos)
import Data.YAML.Aeson qualified as Yaml
import GHC.Generics (Generic)
import RichEnv (clearEnvironment, setRichEnvFromCurrent, toEnvListFromCurrent)
import RichEnv.Types (Environment, Mappings (Mappings), Prefixes (Prefixes), RichEnv (..), Values (Values), defaultRichEnv, fromEnvironment, toEnvironment)
import System.Environment (getEnvironment, setEnv)
-- import System.Process (CreateProcess (env, std_out), StdStream (CreatePipe), proc, readCreateProcess)
import Test.Hspec (Expectation, Spec, context, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen)

spec :: Spec
spec = describe "RichEnv ops" $ do
  context "setting environment" $ do
    it "set a single environment variable through RichEnv" $ do
      clearEnv
      setRichEnvFromCurrent
        RichEnv
          { values = Values $ HM.fromList [("SOME", "var")],
            mappings = mempty,
            prefixes = mempty
          }
      testEnv [("SOME", "var")]
    it "set multiple environment variables through RichEnv" $ do
      clearEnv
      setRichEnvFromCurrent
        defaultRichEnv
          { values = Values $ HM.fromList [("SOME", "var"), ("OTHER", "othervar")]
          }
      testEnv [("SOME", "var"), ("OTHER", "othervar")]
    it "remaps existing environment variables" $ do
      clearEnv
      setTestEnv exampleEnv
      setRichEnvFromCurrent
        defaultRichEnv
          { mappings = Mappings $ HM.fromList [("SOME", "FOO")]
          }
      testEnv [("SOME", "bar")]
    it "remaps prefixed variables" $ do
      clearEnv
      setTestEnv exampleEnv
      setRichEnvFromCurrent
        defaultRichEnv
          { prefixes = Prefixes $ HM.fromList [("NEW_", ["PREFIXED_"])]
          }
      testEnv [("NEW_VAR", "content"), ("NEW_VAR2", "content2")]
  context "getting the environment variable list" $ do
    it "gets the environment variable list" $ do
      clearEnv
      setTestEnv exampleEnv
      testEnvList
        [("SOME", "bar")]
        defaultRichEnv
          { mappings = Mappings $ HM.fromList [("SOME", "FOO")]
          }
    it "gets the environment variable list with prefixes" $ do
      clearEnv
      setTestEnv exampleEnv
      testEnvList
        [("NEW_VAR", "content"), ("NEW_VAR2", "content2")]
        ( defaultRichEnv
            { prefixes = Prefixes $ HM.fromList [("NEW_", ["PREFIXED_"])]
            }
        )

  context "working with YAML" $ it "parses a YAML file into expected results" $ do
    clearEnv
    setTestEnv fileTestsBaseEnv
    let res = Yaml.decode1Strict yamlTestCase
    case res of
      Left err -> fail $ "Parse failed. State: " <> show err
      Right actual -> testEnvList fileTestsCaseExpected (environ actual)

  context "working with JSON" $ it "parses a JSON file into expected results" $ do
    clearEnv
    setTestEnv fileTestsBaseEnv
    let res = JSON.eitherDecodeStrict jsonTestCase :: Either String TestType
    case res of
      Left err -> fail err
      Right actual -> testEnvList fileTestsCaseExpected (environ actual)

  context "invariants" $ do
    prop "parsing YAML from and to a RichEnv should end in the original value" $ \re -> do
      let yaml = Yaml.encode1Strict re
      C8.writeFile "./testcase.yaml" yaml
      let res = Yaml.decode1Strict yaml :: Either (Pos, String) RichEnv
       in case res of
            Left err -> fail $ "Parse failed. State: " <> show err
            Right actual -> do
              actual `shouldBe` re
    prop "parsing JSON from and to a RichEnv should end in the original value" $ \re -> do
      let json = JSON.encode re
          res = JSON.eitherDecode' json :: Either String RichEnv
       in case res of
            Left err -> fail err
            Right actual -> do
              actual `shouldBe` re

--   context "Working with System.Process" $ do
--     it "should work with System.Process" $ do
--       clearEnv
--       setTestEnv exampleEnv
--       envList <-
--         toEnvListFromCurrent
--           ( RichEnv
--               { prefixes = Prefixes $ HM.singleton "NEW_" ["PREFIXED_"],
--                 mappings = Mappings $ HM.singleton "SOME" "FOO",
--                 values = Values $ HM.singleton "OTHER" "othervar"
--               }
--           )
--       let envProcess = (proc "env" []) {env = Just (fromEnvironment envList), std_out = CreatePipe}
--       out <- lines <$> readCreateProcess envProcess mempty
--       sort out `shouldBe` sort ["NEW_VAR=content", "NEW_VAR2=content2", "OTHER=othervar", "SOME=bar"]

exampleEnv :: [(T.Text, T.Text)]
exampleEnv = [("FOO", "bar"), ("BAZ", "qux"), ("PREFIXED_VAR", "content"), ("PREFIXED_VAR2", "content2")]

clearEnv :: IO ()
clearEnv = getEnvironment >>= clearEnvironment

setTestEnv :: Environment -> IO ()
setTestEnv = mapM_ (uncurry setEnv) . fromEnvironment

testEnv :: Environment -> Expectation
testEnv expected = getEnvironment >>= (`shouldBe` sort expected) . sort . toEnvironment

testEnvList :: Environment -> RichEnv -> Expectation
testEnvList expected re = toEnvListFromCurrent re >>= (`shouldBe` sort expected) . sort

newtype TestType = TestType
  { environ :: RichEnv
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

instance Arbitrary TestType where
  arbitrary :: Gen TestType
  arbitrary = TestType <$> arbitrary

-- Test cases that use Aeson's FromJSON/ToJSON instances

yamlTestCase :: B.ByteString
yamlTestCase =
  C8.pack $
    unlines
      [ "environ:",
        "  values:",
        "    SOME: somevar",
        "    OTHER: othervar",
        "  mappings:",
        "    FOO: SOME",
        "  prefixes:",
        "    NEW_: [PREFIXED_]"
      ]

jsonTestCase :: B.ByteString
jsonTestCase =
  C8.pack $
    unlines
      [ "{",
        "  \"environ\": {",
        "    \"values\": {",
        "      \"SOME\": \"somevar\",",
        "      \"OTHER\": \"othervar\"",
        "    },",
        "    \"mappings\": {",
        "      \"FOO\": \"SOME\"",
        "    },",
        "    \"prefixes\": {",
        "      \"NEW_\": [\"PREFIXED_\"]",
        "    }",
        "  }",
        "}"
      ]

fileTestsBaseEnv :: Environment
fileTestsBaseEnv = [("SOME", "bar"), ("OTHER", "othervar"), ("PREFIXED_VAR", "content"), ("PREFIXED_VAR2", "content2")]

fileTestsCaseExpected :: Environment
fileTestsCaseExpected =
  [ ("FOO", "bar"),
    ("OTHER", "othervar"),
    ("SOME", "somevar"),
    ("NEW_VAR", "content"),
    ("NEW_VAR2", "content2")
  ]
