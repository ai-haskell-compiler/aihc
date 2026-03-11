-- |
-- Module: Test.Hspec.NeedEnv
-- Description: Read environment variables for hspec tests
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
-- <https://github.com/debug-ito/hspec-need-env/tree/master/test/Synopsis.hs Synopsis>:
--
-- > module Synopsis (main,spec) where
-- > 
-- > import Control.Applicative ((<$>), (<*>))
-- > import Test.Hspec (Spec, SpecWith, hspec, before, describe, it, shouldBe)
-- > import Test.Hspec.NeedEnv (EnvMode(Need), needEnv, needEnvRead)
-- > 
-- > main :: IO ()
-- > main = hspec spec
-- > 
-- > -- | Read environment variables for parameters necessary for testing.
-- > getEnvs :: IO (String, Int)
-- > getEnvs = (,)
-- >           <$> needEnv mode "TEST_USER_NAME"
-- >           <*> needEnvRead mode "TEST_SEED"
-- >   where
-- >     mode = Need
-- > 
-- > spec :: Spec
-- > spec = before getEnvs $ specWithUserAndSeed
-- >        -- ^ Use 'before' and similar functions to write 'SpecWith'
-- >        -- that takes parameters.
-- > 
-- > -- | Test spec that depends on the environment variables.
-- > specWithUserAndSeed :: SpecWith (String, Int)
-- > specWithUserAndSeed = describe "funcUnderTest" $ do
-- >   it "should do something" $ \(user_name, seed) -> do
-- >     funcUnderTest user_name seed `shouldBe` "SOMETHING"
-- > 
-- > funcUnderTest :: String -> Int -> String
-- > funcUnderTest = undefined
--
-- This module exports 'needEnv' and other similar functions that read
-- environment variables in hspec tests. They are useful to write
-- tests that depend on some external entities, e.g. Web servers,
-- database servers and random number generators.
module Test.Hspec.NeedEnv
       ( -- * Basics
         EnvMode(..),
         needEnv,
         needEnvParse,
         needEnvRead,
         -- * Utilities
         needEnvHostPort
       ) where

import Control.Applicative ((<$>), (<*>))
import Data.Monoid ((<>))
import System.Environment (lookupEnv)
import Test.Hspec.Core.Spec (pendingWith)
import Test.Hspec.Expectations (expectationFailure)
import Text.Read (readEither)

-- | How to treat missing environment variable.
data EnvMode = Need
               -- ^ If the environment variable is not set, the test
               -- fails.
             | Want
               -- ^ If the environment variable is not set, the test
               -- gets pending.
             deriving (Show,Eq,Ord,Enum,Bounded)

-- | Get value of the specified environment variable. If the
-- environment variable is not set, it executes the action specified
-- by the 'EnvMode'.
needEnv :: EnvMode
        -> String -- ^ name of the environment variable
        -> IO String -- ^ value of the environment variable
needEnv mode envkey = do
  mval <- lookupEnv envkey
  case mval of
   Nothing -> do
     signalMsg ("Environment variable " <> envkey <> " is not set.")
     return ""
   Just str -> return str
  where
    signalMsg = case mode of
      Need -> expectationFailure
      Want -> pendingWith

-- | Get environment variable by 'needEnv', and parse the value. If it
-- fails to parse, the test fails.
needEnvParse :: EnvMode
             -> (String -> Either String a) -- ^ the parser of the environment variable
             -> String
             -> IO a
needEnvParse mode parseEnvVal envkey = do
  val_str <- needEnv mode envkey
  case parseEnvVal val_str of
   Right val -> return val
   Left e -> do
     let error_msg = "Fail to parse environment variable " <> envkey <> ": " <> e
     expectationFailure error_msg
     error error_msg

-- | Parse the environment variable with 'Read' class.
needEnvRead :: (Read a)
            => EnvMode -> String -> IO a
needEnvRead mode = needEnvParse mode readEither

-- | Get the pair of hostname and port number from environment
-- variables.
--
-- It reads environment variables @(prefix ++ \"_HOST\")@ and
-- @(prefix ++ \"_PORT\")@.
needEnvHostPort :: EnvMode
                -> String -- ^ prefix of environment variables
                -> IO (String,Int)
needEnvHostPort mode prefix = (,) <$> needStr "_HOST" <*> needInt "_PORT"
  where
    needStr suffix = needEnv mode (prefix ++ suffix)
    needInt suffix = needEnvRead mode (prefix ++ suffix)
