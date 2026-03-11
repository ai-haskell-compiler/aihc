{-# LANGUAGE OverloadedStrings #-}

-- | A modified version of Web.Bugzilla.Search to support
--   the list fields in Red Hat's modified bugzilla API.

module Web.RedHatBugzilla.Search
(
  -- * Search operators
  (.==.)
, (./=.)
, (.<.)
, (.<=.)
, (.>.)
, (.>=.)
, (.=~.)
, (./=~.)
, equalsAny
, contains
, containsCase
, containsAny
, containsAll
, changedBefore
, changedAfter
, changedSince
, changedUntil
, changedRange
, changedFrom
, changedTo
, changedBy
, contentMatches
, isEmpty
, isNotEmpty
, (.&&.)
, (.||.)
, not'

  -- * Search expressions
, Field (..)
, SearchExpression
, evalSearchExpr
) where

import qualified Data.Text as T
import Data.Time.Clock (UTCTime(..))

import Web.RedHatBugzilla.Internal.Search
import Web.RedHatBugzilla.Internal.Types

(.==.) :: FieldType a => Field a -> a -> SearchExpression
(.==.) = (Term .) . BinaryOp "equals"
infix 4 .==.

(./=.) :: FieldType a => Field a -> a -> SearchExpression
(./=.) = (Term .) . BinaryOp "notequals"
infix 4 ./=.

(.<.) :: FieldType a => Field a -> a -> SearchExpression
(.<.) = (Term .) . BinaryOp "lessthan"
infix 4 .<.

(.<=.) :: FieldType a => Field a -> a -> SearchExpression
(.<=.) = (Term .) . BinaryOp "lessthaneq"
infix 4 .<=.

(.>.) :: FieldType a => Field a -> a -> SearchExpression
(.>.) = (Term .) . BinaryOp "greaterthan"
infix 4 .>.

(.>=.) :: FieldType a => Field a -> a -> SearchExpression
(.>=.) = (Term .) . BinaryOp "greaterthaneq"
infix 4 .>=.

(.=~.) :: FieldType a => Field a -> a -> SearchExpression
(.=~.) = (Term .) . BinaryOp "regexp"

(./=~.) :: FieldType a => Field a -> a -> SearchExpression
(./=~.) = (Term .) . BinaryOp "notregexp"

equalsAny :: FieldType a => Field a -> [a] -> SearchExpression
equalsAny = (Term .) . BinaryOp "anyexact"

contains :: Field T.Text -> T.Text -> SearchExpression
contains = (Term .) . BinaryOp "substring"

containsCase :: Field T.Text -> T.Text -> SearchExpression
containsCase = (Term .) . BinaryOp "casesubstring"

containsAny :: Field T.Text -> [T.Text] -> SearchExpression
containsAny = (Term .) . BinaryOp "anywordssubstr"

containsAll :: Field T.Text -> [T.Text] -> SearchExpression
containsAll = (Term .) . BinaryOp "allwordssubstr"

changedBefore :: FieldType a => Field a -> UTCTime -> SearchExpression
changedBefore = (Term .) . BinaryOp "changedbefore"

changedAfter :: FieldType a => Field a -> UTCTime -> SearchExpression
changedAfter = (Term .) . BinaryOp "changedafter"

-- | Filter bug changed since UTCTime
changedSince :: UTCTime -> SearchExpression
changedSince ts = Term $ EqTerm (CustomField "chfieldfrom") ts

-- | Filter bug changed until UTCTime
changedUntil :: UTCTime -> SearchExpression
changedUntil ts = Term $ EqTerm (CustomField "chfieldto") ts

-- | Filter bug changed in range
changedRange :: UTCTime -> UTCTime -> SearchExpression
changedRange from to = changedSince from .&&. changedUntil to

changedFrom :: FieldType a => Field a -> a -> SearchExpression
changedFrom = (Term .) . BinaryOp "changedfrom"

changedTo :: FieldType a => Field a -> a -> SearchExpression
changedTo = (Term .) . BinaryOp "changedto"

changedBy :: FieldType a => Field a -> UserEmail -> SearchExpression
changedBy = (Term .) . BinaryOp "changedby"

contentMatches :: T.Text -> SearchExpression
contentMatches = Term . BinaryOp "matches" ContentField

isEmpty :: FieldType a => Field a -> SearchExpression
isEmpty = Term . UnaryOp "isempty"

isNotEmpty :: FieldType a => Field a -> SearchExpression
isNotEmpty = Term . UnaryOp "isnotempty"

(.&&.) :: SearchExpression -> SearchExpression -> SearchExpression
(.&&.) (And as) (And bs) = And (as ++ bs)
(.&&.) (And as) a = And (as ++ [a])
(.&&.) a (And as) = And (a:as)
(.&&.) a b        = And [a, b]
infixr 3 .&&.

(.||.) :: SearchExpression -> SearchExpression -> SearchExpression
(.||.) (Or as) (Or bs) = Or (as ++ bs)
(.||.) a (Or as) = Or (a:as)
(.||.) (Or as) a = Or (as ++ [a])
(.||.) a b       = Or [a, b]
infixr 2 .||.

not' :: SearchExpression -> SearchExpression
not' (Not a) = a
not' a       = Not a
