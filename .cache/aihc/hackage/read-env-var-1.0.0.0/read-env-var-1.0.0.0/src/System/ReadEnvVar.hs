{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

{-|
Module      : System.ReadEnvVar
Copyright   : (c) Dennis Gosnell, 2016
License     : BSD-style (see LICENSE file)
Maintainer  : cdep.illabout@gmail.com
Stability   : experimental
Portability : POSIX

This Haskell module exports functions for safely reading environment variables.

The @lookupEnv*@ functions are for reading in 'String'-like environment
variables (like hostnames, passwords, etc.), while the @readEnv*@ functions are
for reading in Haskell datatypes (like 'Int', 'Double', etc).

Most of these functions run in 'MonadIO'.  This means that they can be used
from any monad that implements 'MonadIO'.  This makes it easier to run in a
monad transformer stack.  If you're not familiar with 'MonadIO', you can just
think of all the functions as being 'IO' actions.

The 'lookupEnv', 'lookupEnvDef', and 'lookupEnvEx' functions all use 'IsString'
to generalize the return type.  This makes it more general than
"System.Environment"\'s 'Env.lookupEnv'.  It makes it possible to read in
things other than 'String's, like
<https://hackage.haskell.org/package/text/docs/Data-Text.html Text> or
<https://hackage.haskell.org/package/bytestring/docs/Data-ByteString.html ByteString>.
-}

module System.ReadEnvVar
    ( lookupEnv
    , lookupEnvDef
    , readEnv
    , readEnvDef
      -- * Unsafe functions
    , EnvVarDoesNotExistException(..)
    , lookupEnvEx
    , readEnvEx
    , EnvVarCannotBeReadException(..)
    , readEnvEx'
      -- * Re-exports
    , setEnv
    ) where

#if __GLASGOW_HASKELL__ < 710
-- We don't need this import for GHC 7.10 as it exports all required functions
-- from Prelude
import Control.Applicative
#endif

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow(throwM))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Data (Data)
import Data.Maybe (fromMaybe)
import Data.String (IsString(fromString))
import Data.Typeable (Typeable)
import System.Environment (setEnv)
import qualified System.Environment as Env
import Text.Read (readMaybe)

-- | Lookup a value from an environment variable and read it in with
-- 'readMaybe'.  If the environment variable doesn't exist, or it can't be
-- 'read', return the default value.  Like 'readEnv' but with a default
-- value.
--
-- Read an environment variable that exists:
--
-- >>> setEnv "TEST_ENV_VAR1" "1000"
-- >>> readEnvDef "TEST_ENV_VAR1" 5 :: IO Int
-- 1000
--
-- Try reading an environment variable that does not exist.  Returns the
-- default value:
--
-- >>> readEnvDef "THIS_ENV_VAR_WILL_NOT_EXIST" 5 :: IO Int
-- 5
--
-- Try reading an environment variable that cannot be 'read'.  Returns the
-- default value:
--
-- >>> setEnv "BAD_ENV_VAR" "not an int"
-- >>> readEnvDef "BAD_ENV_VAR" 10 :: IO Int
-- 10
--
-- Note that this __DOES NOT__ read string values as one might expect:
--
-- >>> setEnv "TEST_ENV_VAR2" "some string 1"
-- >>> readEnvDef "TEST_ENV_VAR2" "def val" :: IO String
-- "def val"
--
-- It will read string values as if they were Haskell strings:
--
-- >>> setEnv "TEST_ENV_VAR3" "\"some string 1\""
-- >>> readEnvDef "TEST_ENV_VAR3" "def val" :: IO String
-- "some string 1"
readEnvDef
  :: (MonadIO m, Read a)
  => String -- ^ environment variable to lookup
  -> a -- ^ default value to return if the environment variable
       -- either does not exist, or cannot be 'read'
  -> m a
readEnvDef envVar def = do
  maybeEnv <- readEnv envVar
  return $ fromMaybe def maybeEnv

-- | Lookup a value from an environment variable and read it in with
-- 'readMaybe'.
--
-- Read an environment variable that exists:
--
-- >>> setEnv "TEST_ENV_VAR" "2000"
-- >>> readEnv "TEST_ENV_VAR" :: IO (Maybe Int)
-- Just 2000
--
-- Try reading an environment variable that does not exist.  Returns 'Nothing':
--
-- >>> readEnv "THIS_ENV_VAR_WILL_NOT_EXIST" :: IO (Maybe Int)
-- Nothing
--
-- Try reading an environment variable that cannot be 'read'.  Returns
-- 'Nothing':
--
-- >>> setEnv "BAD_ENV_VAR" "not an int"
-- >>> readEnv "BAD_ENV_VAR" :: IO (Maybe Int)
-- Nothing
--
-- Note that this __DOES NOT__ read string values as one might expect:
--
-- >>> setEnv "TEST_ENV_VAR2" "some string 1"
-- >>> readEnv "TEST_ENV_VAR2" :: IO (Maybe String)
-- Nothing
--
-- It will read string values as if they were Haskell strings:
--
-- >>> setEnv "TEST_ENV_VAR3" "\"some string 1\""
-- >>> readEnv "TEST_ENV_VAR3" :: IO (Maybe String)
-- Just "some string 1"
readEnv
  :: (MonadIO m, Read a)
  => String -- ^ environment variable to lookup
  -> m (Maybe a)
readEnv envVar = do
  maybeEnv <- liftIO (Env.lookupEnv envVar)
  return $ maybeEnv >>= readMaybe

-- | Like 'lookupEnv' but take a default value.
--
-- Lookup an environment variable that exists:
--
-- >>> setEnv "TEST_ENV_VAR" "foo"
-- >>> lookupEnvDef "TEST_ENV_VAR" "bar" :: IO String
-- "foo"
--
-- Lookup an environment variable that doesn't exist.  Return the default
-- value:
--
-- >>> lookupEnvDef "THIS_ENV_VAR_WILL_NOT_EXIST" "bar" :: IO String
-- "bar"
lookupEnvDef
  :: (IsString a, MonadIO m)
  => String -- ^ environment variable to lookup
  -> a -- ^ default value to return if environment variable not defined
  -> m a
lookupEnvDef envVar defaultValue = do
  maybeEnv <- liftIO $ Env.lookupEnv envVar
  return $ maybe defaultValue fromString maybeEnv

-- | Like "System.Environment"\'s 'Env.lookupEnv', but using 'IsString' to make
-- it more general.
--
-- Lookup an environment variable that exists:
--
-- >>> setEnv "TEST_ENV_VAR" "foo"
-- >>> lookupEnv "TEST_ENV_VAR" :: IO (Maybe String)
-- Just "foo"
--
-- Lookup an environment variable that doesn't exist.  Return 'Nothing':
--
-- >>> lookupEnv "THIS_ENV_VAR_WILL_NOT_EXIST" :: IO (Maybe String)
-- Nothing
lookupEnv
  :: (IsString a, MonadIO m)
  => String
  -> m (Maybe a)
lookupEnv envVar = do
  maybeEnv <- liftIO $ Env.lookupEnv envVar
  return $ fmap fromString maybeEnv

-- | 'Exception' thrown by 'lookupEnvEx' and 'readEnvEx' when the
-- environment variable being read doesn't exist.
data EnvVarDoesNotExistException =
  -- | The 'String' is the name of the environment variable that does not
  -- exist.
  EnvVarDoesNotExistException String
  deriving (Data, Eq, Ord, Read, Show, Typeable)

instance Exception EnvVarDoesNotExistException

-- | Like 'lookupEnv', but instead of returning a 'Maybe', throw an
-- 'EnvVarDoesNotExistException' if the environment variable does not exist.
-- The exception is thrown with 'throwM' from 'MonadThrow'.
--
-- Lookup an environment variable that exists:
--
-- >>> setEnv "TEST_ENV_VAR" "foo"
-- >>> lookupEnvEx "TEST_ENV_VAR" :: IO String
-- "foo"
--
-- Lookup an environment variable that doesn't exist.  Throws
-- 'EnvVarDoesNotExistException'.
--
-- >>> lookupEnvEx "THIS_ENV_VAR_WILL_NOT_EXIST" :: IO String
-- *** Exception: EnvVarDoesNotExistException "THIS_ENV_VAR_WILL_NOT_EXIST"
lookupEnvEx
  :: (IsString a, MonadIO m, MonadThrow m)
  => String
  -> m a
lookupEnvEx envVar = do
  maybeEnv <- liftIO (Env.lookupEnv envVar)
  case maybeEnv of
    Nothing -> throwM $ EnvVarDoesNotExistException envVar
    Just env -> return $ fromString env

-- | Lookup a value from an environment variable and read it in with
-- 'readMaybe'.  Throw an 'EnvVarDoesNotExistException' if the environment
-- variable does not exist.  The exception is thrown with 'throwM' from
-- 'MonadThrow'.
--
-- Read an environment variable that exists:
--
-- >>> setEnv "TEST_ENV_VAR" "2000"
-- >>> readEnvEx "TEST_ENV_VAR" :: IO (Maybe Int)
-- Just 2000
--
-- Try reading an environment variable that does not exist.  Throws
-- 'EnvVarDoesNotExistException':
--
-- >>> readEnvEx "THIS_ENV_VAR_WILL_NOT_EXIST" :: IO (Maybe Int)
-- *** Exception: EnvVarDoesNotExistException "THIS_ENV_VAR_WILL_NOT_EXIST"
--
-- Try reading an environment variable that cannot be 'read'.  Returns
-- 'Nothing':
--
-- >>> setEnv "BAD_ENV_VAR" "not an int"
-- >>> readEnvEx "BAD_ENV_VAR" :: IO (Maybe Int)
-- Nothing
--
-- Note that this __DOES NOT__ read string values as one might expect:
--
-- >>> setEnv "TEST_ENV_VAR2" "some string 1"
-- >>> readEnvEx "TEST_ENV_VAR2" :: IO (Maybe String)
-- Nothing
--
-- It will read string values as if they were Haskell strings:
--
-- >>> setEnv "TEST_ENV_VAR3" "\"some string 1\""
-- >>> readEnvEx "TEST_ENV_VAR3" :: IO (Maybe String)
-- Just "some string 1"
readEnvEx
  :: (MonadIO m, Read a, MonadThrow m)
  => String -- ^ environment variable to lookup
  -> m (Maybe a)
readEnvEx envVar = do
  maybeEnv <- liftIO (Env.lookupEnv envVar)
  case maybeEnv of
    Nothing -> throwM $ EnvVarDoesNotExistException envVar
    Just env -> return $ readMaybe env

-- | 'Exception' thrown by 'readEnvEx'' when the environment variable
-- cannot be 'read'.
data EnvVarCannotBeReadException =
  -- | The first 'String' is the name of the environment variable that cannot
  -- be 'read'.
  EnvVarCannotBeReadException String
  deriving (Data, Eq, Ord, Read, Show, Typeable)

instance Exception EnvVarCannotBeReadException

-- | Just like 'readEnvEx', but also throw an exception when the environment
-- variable cannot be 'read'.
--
-- This can throw both 'EnvVarCannotBeReadException' and
-- 'EnvVarDoesNotExistException' with 'throwM'.
--
-- Read an environment variable that exists:
--
-- >>> setEnv "TEST_ENV_VAR" "2000"
-- >>> readEnvEx' "TEST_ENV_VAR" :: IO Int
-- 2000
--
-- Try reading an environment variable that does not exist.  Throws
-- 'EnvVarDoesNotExistException':
--
-- >>> readEnvEx' "THIS_ENV_VAR_WILL_NOT_EXIST" :: IO Int
-- *** Exception: EnvVarDoesNotExistException "THIS_ENV_VAR_WILL_NOT_EXIST"
--
-- Try reading an environment variable that cannot be 'read'.  Throws
-- 'EnvVarCannotBeReadException':
--
-- >>> setEnv "BAD_ENV_VAR" "not an int"
-- >>> readEnvEx' "BAD_ENV_VAR" :: IO Int
-- *** Exception: EnvVarCannotBeReadException "BAD_ENV_VAR"
--
-- Note that this __DOES NOT__ read string values as one might expect:
--
-- >>> setEnv "TEST_ENV_VAR2" "some string 1"
-- >>> readEnvEx' "TEST_ENV_VAR2" :: IO String
-- *** Exception: EnvVarCannotBeReadException "TEST_ENV_VAR2"
--
-- It will read string values as if they were Haskell strings:
--
-- >>> setEnv "TEST_ENV_VAR3" "\"some string 1\""
-- >>> readEnvEx' "TEST_ENV_VAR3" :: IO String
-- "some string 1"
readEnvEx'
  :: (MonadIO m, Read a, MonadThrow m)
  => String -- ^ environment variable to lookup
  -> m a
readEnvEx' envVar = do
  maybeEnv <- readEnvEx envVar
  case maybeEnv of
    Nothing -> throwM $ EnvVarCannotBeReadException envVar
    Just env -> return env
