{-# LANGUAGE GADTs #-}
-- | Defines Exceptions that can occur while using the call-alloy library

module Language.Alloy.Exceptions (
  CallAlloyException (..),
  AlloyLookupFailed (..),
  AlloyObjectNameMismatch (..),
  AlloyResponseFailure (..),
  UnexpectedAlloyRelation (..),
  Alternatives (..),
  Expected (..),
  Got (..),
  RelationName (..),
  ) where

import qualified Data.Map                         as M (keys)

import Control.Exception (
  Exception (fromException, toException),
  SomeException,
  )
import Data.List                        (intercalate)
import Data.Typeable                    (cast)
import Language.Alloy.Types (
  AlloyInstance,
  Signature (..),
  showSignature,
  )
import Text.Trifecta.Result             (ErrInfo)

{-|
Any exception that might be raised by @call-alloy@.
-}
data CallAlloyException where
  CallAlloyException :: Exception e => e -> CallAlloyException

instance Show CallAlloyException where
    show (CallAlloyException e) = show e

instance Exception CallAlloyException

alloyExceptionToException :: Exception e => e -> SomeException
alloyExceptionToException = toException . CallAlloyException

alloyExceptionFromException :: Exception e => SomeException -> Maybe e
alloyExceptionFromException x = do
  CallAlloyException a <- fromException x
  cast a

{-|
An unexpected Alloy response.

This would usually happen when @call-alloy@ does not know how to interpret a
response that was received from Alloy.
In this case an issue should be opened, describing in detail what was tried
and the received error response.
-}
newtype AlloyResponseFailure
  = ParsingAlloyResponseFailed ErrInfo
  deriving Show

instance Exception AlloyResponseFailure where
  toException = alloyExceptionToException
  fromException = alloyExceptionFromException

{-|
Containing an expectation.
-}
newtype Expected = Expected {unExpected :: String}

{-|
Containing what we actually got.
-}
newtype Got = Got {unGot :: String}

{-|
An object name does not match an expectation.
-}
data AlloyObjectNameMismatch
  = AlloyObjectNameMismatch !Expected !Got

instance Show AlloyObjectNameMismatch where
  show (AlloyObjectNameMismatch expected got)
    = "AlloyObjectNameMismatch: "
    <> "expected an object of name " <> unExpected expected
    <> " but got an object of name " <> unGot got

instance Exception AlloyObjectNameMismatch where
  toException = alloyExceptionToException
  fromException = alloyExceptionFromException

{-|
How to refer to a specific relation.
-}
newtype RelationName = RelationName {unRelationName :: String}

{-|
Possible alternatives.
-}
newtype Alternatives a = Alternatives {unAlternatives :: [a]}

{-|
A lookup within an 'AlloyInstance' failed.
-}
data AlloyLookupFailed
  = LookupAlloySignatureFailed !Signature !AlloyInstance
  | LookupAlloyRelationFailed !RelationName !(Alternatives RelationName)

instance Show AlloyLookupFailed where
  show (LookupAlloySignatureFailed s model) = "LookupAlloySignatureFailed: "
    <> showSignature s
    <> " is missing in the Alloy instance"
    <> "; available are: \""
    <> intercalate "\", " (showSignature <$> M.keys model)
  show (LookupAlloyRelationFailed rel xs) = "LookupAlloyRelationFailed: "
    <> "relation " ++ unRelationName rel
    <> " is missing in the Alloy instance"
    <> "; available are: "
    <> intercalate ", " (map unRelationName $ unAlternatives xs)

instance Exception AlloyLookupFailed where
  toException = alloyExceptionToException
  fromException = alloyExceptionFromException

{-|
A relation type does not match.
-}
data UnexpectedAlloyRelation
  = ExpectedIdenticalRelationship
  | ExpectedSingleRelationship
  | ExpectedDoubleRelationship
  | ExpectedTripleRelationship

instance Show UnexpectedAlloyRelation where
  show ExpectedIdenticalRelationship
    = "ExpectedIdenticalRelationship: "
    <> "Relation is (unexpectedly) not exactly a single element"
  show ExpectedSingleRelationship
    = "ExpectedSingleRelationship: "
    <> "Relation is (unexpectedly) a mapping"
  show ExpectedDoubleRelationship
    = "ExpectedDoubleRelationship: "
    <> "Relation is not a binary mapping"
  show ExpectedTripleRelationship
    = "ExpectedTripleRelationship: "
    <> "Relation is not a ternary mapping"

instance Exception UnexpectedAlloyRelation where
  toException = alloyExceptionToException
  fromException = alloyExceptionFromException
