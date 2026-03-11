module Synopsis (main,spec) where

import Control.Applicative ((<$>), (<*>))
import Test.Hspec (Spec, SpecWith, hspec, before, describe, it, shouldBe)
import Test.Hspec.NeedEnv (EnvMode(Need), needEnv, needEnvRead)

main :: IO ()
main = hspec spec

-- | Read environment variables for parameters necessary for testing.
getEnvs :: IO (String, Int)
getEnvs = (,)
          <$> needEnv mode "TEST_USER_NAME"
          <*> needEnvRead mode "TEST_SEED"
  where
    mode = Need

spec :: Spec
spec = before getEnvs $ specWithUserAndSeed
       -- ^ Use 'before' and similar functions to write 'SpecWith'
       -- that takes parameters.

-- | Test spec that depends on the environment variables.
specWithUserAndSeed :: SpecWith (String, Int)
specWithUserAndSeed = describe "funcUnderTest" $ do
  it "should do something" $ \(user_name, seed) -> do
    funcUnderTest user_name seed `shouldBe` "SOMETHING"

funcUnderTest :: String -> Int -> String
funcUnderTest = undefined
