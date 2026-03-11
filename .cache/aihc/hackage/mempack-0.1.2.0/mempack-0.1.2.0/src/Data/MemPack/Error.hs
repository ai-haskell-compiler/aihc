{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Data.MemPack.Error
-- Copyright   : (c) Alexey Kuleshevich 2024
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Data.MemPack.Error where

import Control.Exception
import Data.List.NonEmpty as NE
import Data.Text (Text)
import Data.Typeable
import GHC.Exts

data SomeError where
  SomeError :: (Typeable e, Error e) => e -> SomeError

instance Show SomeError where
  showsPrec p (SomeError e) = showsPrec p e

instance Exception SomeError

-- | Very similar interface to `Exceptions`, except not intended for runtime exceptions.
class Show e => Error e where
  toSomeError :: e -> SomeError
  default toSomeError :: Typeable e => e -> SomeError
  toSomeError = SomeError

  fromSomeError :: SomeError -> Maybe e
  default fromSomeError :: Typeable e => SomeError -> Maybe e
  fromSomeError (SomeError t) = cast t

instance Error SomeError where
  toSomeError = id
  fromSomeError = Just

newtype TextError = TextError Text
  deriving newtype (Eq, Show, IsString)

instance Error TextError

instance IsString SomeError where
  fromString = toSomeError . TextError . fromString

newtype ManyErrors = ManyErrors (NonEmpty SomeError)
  deriving (Show)

instance Error ManyErrors

data UnknownError = UnknownError
  deriving (Show)

instance Error UnknownError

fromMultipleErrors :: [SomeError] -> SomeError
fromMultipleErrors es =
  case es of
    [] -> toSomeError UnknownError
    [e] -> e
    e : rest -> toSomeError $ ManyErrors (e :| rest)
{-# NOINLINE fromMultipleErrors #-}


data RanOutOfBytesError = RanOutOfBytesError
  { ranOutOfBytesRead :: Int
  , ranOutOfBytesAvailable :: Int
  , ranOutOfBytesRequested :: Int
  }
  deriving (Eq, Ord)

instance Show RanOutOfBytesError where
  show RanOutOfBytesError{..} =
    "Ran out of bytes. Read "
      <> showBytes ranOutOfBytesRead
      <> " out of "
      <> showBytes ranOutOfBytesAvailable
      <> ". Requested to read "
      <> showBytes ranOutOfBytesRequested
      <> " more."

instance Error RanOutOfBytesError

data NotFullyConsumedError = NotFullyConsumedError
  { notFullyConsumedRead :: Int
  , notFullyConsumedAvailable :: Int
  , notFullyConsumedTypeName :: String
  }
  deriving (Eq, Ord)

instance Show NotFullyConsumedError where
  show NotFullyConsumedError{..} =
    "Buffer of length " <> showBytes notFullyConsumedAvailable
      ++ " was not fully consumed while unpacking '" <> notFullyConsumedTypeName
      ++ "'. Unconsumed " <> showBytes (notFullyConsumedAvailable - notFullyConsumedRead)
      ++ " was leftover."

instance Error NotFullyConsumedError

showBytes :: Int -> String
showBytes 1 = "1 byte"
showBytes n = show n ++ " bytes"
