{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Web.Envelope
Copyright   : (c) Dennis Gosnell, 2016
License     : BSD-style (see LICENSE file)
Maintainer  : cdep.illabout@gmail.com
Stability   : experimental
Portability : POSIX

This module contains the 'Envelope' type and helper functions. The 'Envelope'
type can be used to wrap responses from a JSON API.

The following is an example of using this package.

First, we will import some needed modules.

>>> import qualified Data.ByteString.Lazy.Char8 as C8
>>> import Data.Aeson (decode, encode)

Let's look at how a success reponse is encoded and decoded.  It is encoded as
an object with a single member: @\"data\"@.

>>> let successEnvelope = toSuccessEnvelope 3 :: Envelope Text Int
>>> C8.putStrLn $ encode successEnvelope
{"data":3}
>>> decode "{\"data\":3}" :: Maybe (Envelope Text Int)
Just (EnvelopeSuccess (Success 3))

Now lets look at how an error response is encoded and decoded.  It is encoded
as an object with two members: @\"extra\"@ and @\"error\"@.

>>> let errorEnvelope = toErrEnvelope "DB_ERROR" "there was an error with the database" :: Envelope String Int
>>> C8.putStrLn $ encode errorEnvelope
{"extra":"there was an error with the database","error":"DB_ERROR"}
>>> decode "{\"extra\":\"there was an error with the database\",\"error\":\"DB_ERROR\"}" :: Maybe (Envelope String Int)
Just (EnvelopeErr (Err {errErr = "DB_ERROR", errExtra = Just "there was an error with the database"}))

The 'Success' and 'Err' types are used within the 'Envelope' type synonym.
-}

module Web.Envelope
    ( Envelope
    , Envelope'(..)
    , Success(..)
    , Err(..)
    , toErr
    , toErr'
    , throwEnvelopeErr
    , throwEnvelopeErr'
    , toSuccessEnvelope
    , toErrEnvelope
    , toErrEnvelope'
    ) where

import Prelude

import Control.Monad.Except (MonadError, throwError)
import Control.Applicative ((<|>))
import Data.Aeson
       ((.=), (.:), FromJSON(..), ToJSON(..), Value(..), object)
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Data (Data)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Web.FormUrlEncoded
       (FromForm(fromForm), ToForm(toForm), fromEntriesByKey, lookupMaybe,
        lookupUnique, parseUnique)
import Web.HttpApiData
       (FromHttpApiData, ToHttpApiData(toQueryParam))
import Web.Internal.FormUrlEncoded (Form)

-- | Main type to be used.  Wrapper around responses from an API, mainly used
-- with a JSON API.
--
-- Type synonym around 'Envelope''.
type Envelope e a = Envelope' (Err e) (Success a)

-- | Wrapper around either a success or an error.  Isomorphic to 'Either'.
--
-- The only interesting part of this type is the 'ToJSON' and 'FromJSON'
-- instances.
data Envelope' e a = EnvelopeErr e | EnvelopeSuccess a
    deriving (Data, Eq, Generic, Show, Typeable)

instance (ToJSON e, ToJSON a) => ToJSON (Envelope' e a) where
    toJSON :: Envelope' e a -> Value
    toJSON (EnvelopeErr appErr) = toJSON appErr
    toJSON (EnvelopeSuccess successResp) = toJSON successResp

-- | Tries to parse a successful response.  If you are using the 'Envelope'
-- type synonym, this will use the 'FromJSON' instance for 'Success'.
--
-- If that fails, try to parse an error response.  If you are using the
-- 'Envelope' type synonym, this will use the 'FromJSON' instance for 'Err'.
instance (FromJSON e, FromJSON a) => FromJSON (Envelope' e a) where
    parseJSON :: Value -> Parser (Envelope' e a)
    parseJSON v = EnvelopeSuccess <$> parseJSON v
              <|> EnvelopeErr     <$> parseJSON v
              <|> typeMismatch "Envelope'" v

-- | Uses the underlying 'ToForm' instance for both the 'EnvelopeErr' case and
-- the 'EnvelopeSuccess' case.
instance (ToForm a, ToForm e) =>
         ToForm (Envelope' e a) where
  toForm :: Envelope' e a -> Form
  toForm (EnvelopeErr err) = toForm err
  toForm (EnvelopeSuccess succ) = toForm succ

-- | Looks for the key @\"error\"@ in the 'Form'.  If it is found, assume this
-- form is an @'Err e'@.  If it is not found, assume this 'Form' is an @a@.
--
-- __WARNING__: If the @a@ is encoded with a key @\"error\"@, this 'Form' will
-- be decoded as a 'EnvelopeErr' instead of a 'EnvelopeSuccess'.  This is
-- probably not what you want.
instance (FromForm a, FromHttpApiData e) =>
         FromForm (Envelope' (Err e) a) where
  fromForm :: Form -> Either Text (Envelope' (Err e) a)
  fromForm form =
    case lookupUnique "error" form of
      -- Key "error" doesn't exist, so assume this is an @a@.
      Left _ -> EnvelopeSuccess <$> fromForm form
      -- Key "error exists, so assume this is an @'Err' e@.
      Right _ -> EnvelopeErr <$> fromForm form

-- | Newtype wrapper to be able to provide specific instances. Used with
-- 'Envelope'.
newtype Success a = Success a
    deriving (Data, Eq, Generic, Show, Typeable)

-- | For @'Success' a@, wrap the @a@ in an object with a @\"data\"@ field.
--
-- The resulting JSON object will look like this:
--
-- @
--  { \"data\": ... }
-- @
instance (ToJSON a) => ToJSON (Success a) where
    toJSON :: Success a -> Value
    toJSON (Success a) = object ["data" .= a]

-- | Parse the JSON object produced by the 'ToJSON' instance.
instance (FromJSON e) => FromJSON (Success e) where
    parseJSON :: Value -> Parser (Success e)
    parseJSON (Object v) = Success <$> v .: "data"
    parseJSON invalid    = typeMismatch "Success" invalid

-- | Use the 'ToForm' instance of the underlying datatype.
instance (ToForm a) =>
         ToForm (Success a) where
  toForm :: Success a -> Form
  toForm (Success a) = toForm a

-- | Use the 'FromForm' instance of the underlying datatype.
instance (FromForm a) =>
         FromForm (Success a) where
  fromForm :: Form -> Either Text (Success a)
  fromForm form = Success <$> fromForm form

-- | Wrapper to add an extra field with info about the error.  Used with
-- 'Envelope'.
data Err e = Err { errErr   :: e          -- ^ Actual error information we want to send.
                 , errExtra :: Maybe Text -- ^ Additional error information in plain text.
                 }
    deriving (Data, Eq, Generic, Show, Typeable)

-- | For @'Err' e@, wrap the @e@ in an object with @\"extra\"@ and @\"error\"@ fields.
--
-- The resulting JSON object will look like this:
--
-- @
--  { \"extra\": ..., \"error\": .... }
-- @
instance (ToJSON e) => ToJSON (Err e) where
    toJSON :: Err e -> Value
    toJSON (Err e extra) = object ["error" .= e, "extra" .= extra]

-- | Parse the JSON object produced by the 'ToJSON' instance.
instance (FromJSON e) => FromJSON (Err e) where
    parseJSON :: Value -> Parser (Err e)
    parseJSON (Object v) = Err <$> v .: "error"
                               <*> v .: "extra"
    parseJSON invalid    = typeMismatch "Err" invalid

-- | Just use the 'ToForm' instance of the underlying datatype.
--
-- The resulting Form object will look like this:
--
-- @
--  [(\"extra\", ...), (\"error\", ....)]
-- @
instance (ToHttpApiData e) =>
         ToForm (Err e) where
  toForm :: Err e -> Form
  toForm (Err err maybeExtra) =
    fromEntriesByKey
      [("error" :: Text, [toQueryParam err]), ("extra", maybeToList maybeExtra)]

-- | Parse a form produced by the 'ToForm' instance.  Use 'FromHttpApiData's
-- 'Web.HttpApiData.parseQueryParam' to parse the @error@ parameter.
instance (FromHttpApiData e) =>
         FromForm (Err e) where
  fromForm :: Form -> Either Text (Err e)
  fromForm form = Err <$> parseUnique "error" form <*> lookupMaybe "extra" form

-- | Smart constructor for 'Err'.
--
-- >>> toErr "DB_ERROR" "an error occurred"
-- Err {errErr = "DB_ERROR", errExtra = Just "an error occurred"}
toErr :: e -> Text -> Err e
toErr e extra = Err e (Just extra)

-- | Smart constructor for 'Err' that doesn't use an 'errExtra'.
--
-- >>> toErr' "DB_ERROR"
-- Err {errErr = "DB_ERROR", errExtra = Nothing}
toErr' :: e -> Err e
toErr' e = Err e Nothing

-- | Wrap an @a@ in a success 'Envelope'.
--
-- >>> toSuccessEnvelope 3 :: Envelope String Int
-- EnvelopeSuccess (Success 3)
toSuccessEnvelope :: a -> Envelope e a
toSuccessEnvelope = EnvelopeSuccess . Success

-- | Wrap an @a@ and an additional message in an error 'Envelope'.
--
-- >>> toErrEnvelope "DB_ERROR" "there was an error with the database" :: Envelope String Int
-- EnvelopeErr (Err {errErr = "DB_ERROR", errExtra = Just "there was an error with the database"})
toErrEnvelope :: e -> Text -> Envelope e a
toErrEnvelope e extra = EnvelopeErr . Err e $ Just extra

-- | Wrap an @a@ in an error 'Envelope'.
--
-- >>> toErrEnvelope' "DB_ERROR" :: Envelope String Int
-- EnvelopeErr (Err {errErr = "DB_ERROR", errExtra = Nothing})
toErrEnvelope' :: e -> Envelope e a
toErrEnvelope' e = EnvelopeErr $ Err e Nothing

-- | Throw an @'Err e'@ using 'throwError' in a 'Monad' that implements
-- 'MonadError'.
--
-- If you have @'ExceptT' ('Err' e)@ somewhere inside your monad transformer
-- stacks, this function can be used to throw an error (return early) in a
-- function.
--
-- >>> import Control.Monad.Except (runExcept)
-- >>> throwEnvelopeErr "BAD_ERROR" "a very bad error occurred!" :: Either (Err String) Int
-- Left (Err {errErr = "BAD_ERROR", errExtra = Just "a very bad error occurred!"})
--
-- Here is a longer example using a monad stack.
--
-- > type MyMonadStack = ReaderT Int (ExceptT (Err String) IO)
-- >
-- > doSomething :: Int -> MyMonadStack Int
-- > doSomething int =
-- >     if int < 0
-- >         then
-- >             throwEnvelopeErr "INT_TOO_SMALL" "the integer passed to doSomething is too small"
-- >         else
-- >             return (int + 1)
throwEnvelopeErr :: (MonadError (Err e) m) => e -> Text -> m a
throwEnvelopeErr e text = throwError $ Err e (Just text)

-- | Like 'throwEnvelopeErr' but without providing a message.
--
-- >>> import Control.Monad.Except (runExcept)
-- >>> throwEnvelopeErr "BAD_ERROR" "a very bad error occurred!" :: Either (Err String) Int
-- Left (Err {errErr = "BAD_ERROR", errExtra = Just "a very bad error occurred!"})
throwEnvelopeErr' :: (MonadError (Err e) m) => e -> m a
throwEnvelopeErr' e = throwError $ Err e Nothing
