{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Operators which can be used to construct queries for Bugzilla.
--   These operators are intended to be typesafe: you should not be
--   able to construct a query that causes Bugzilla to return an
--   error. If you *are* able to construct an erroneous query, please
--   report a bug.
module Web.RedHatBugzilla.Internal.Search
( FieldType
, SearchTerm (..)
, SearchExpression (..)
, evalSearchExpr
) where

import Data.List
import qualified Data.Text as T
import Data.Time.Clock (UTCTime(..))
import Data.Time.ISO8601 (formatISO8601)

import Web.RedHatBugzilla.Internal.Network
import Web.RedHatBugzilla.Internal.Types

class FieldType a where fvAsText :: a -> T.Text

instance FieldType T.Text where fvAsText = id
instance FieldType Int where fvAsText = T.pack . show
instance FieldType UTCTime where fvAsText = T.pack . formatISO8601

instance FieldType Bool where
  fvAsText True  = "true"
  fvAsText False = "false"

instance FieldType a => FieldType [a] where
  fvAsText = T.intercalate "," . map fvAsText

data SearchTerm where
  UnaryOp  :: FieldType a => T.Text -> Field a -> SearchTerm
  BinaryOp :: (FieldType a, FieldType b) => T.Text -> Field a -> b -> SearchTerm
  EqTerm   :: (FieldType a, FieldType b) => Field a -> b -> SearchTerm

-- | A Boolean expression which can be used to query Bugzilla.
data SearchExpression
  = And [SearchExpression]
  | Or [SearchExpression]
  | Not SearchExpression
  | Term SearchTerm

taggedQueryPart :: Int -> Char -> T.Text -> QueryPart
taggedQueryPart t k v = (T.cons k . T.pack . show $ t, Just v)

termQuery :: FieldType b => Int -> Field a -> T.Text -> b -> [QueryPart]
termQuery t f o v = [taggedQueryPart t 'f' (searchFieldName f),
                     taggedQueryPart t 'o' o,
                     taggedQueryPart t 'v' (fvAsText v)]

evalSearchTerm :: Int -> SearchTerm -> [QueryPart]
evalSearchTerm t (UnaryOp op field)          = termQuery t field op ("" :: T.Text)
evalSearchTerm t (BinaryOp op field val)     = termQuery t field op val
evalSearchTerm _ (EqTerm field val)          = [(searchFieldName field, Just . fvAsText $ val)]

evalSearchExpr :: SearchExpression -> [QueryPart]
evalSearchExpr e = snd $ evalSearchExpr' 1 e
  where
    evalExprGroup :: Int -> [SearchExpression] -> (Int, [QueryPart])
    evalExprGroup t es =
      let (subExprT, subExprQs) = foldl' evalSubExpr (t + 1, []) es
          qs = taggedQueryPart t 'f' "OP" :
               taggedQueryPart subExprT 'f' "CP" :
               subExprQs
      in (subExprT + 1, qs)

    evalSubExpr :: (Int, [QueryPart]) -> SearchExpression -> (Int, [QueryPart])
    evalSubExpr (t, qs) expr = let (nextT, qs') = evalSearchExpr' t expr
                               in  (nextT, qs ++ qs')

    evalSearchExpr' :: Int -> SearchExpression -> (Int, [QueryPart])
    evalSearchExpr' t (And es) = evalExprGroup t es

    evalSearchExpr' t (Or es) =
      let (groupT, groupQs) = evalExprGroup t es
          qs = taggedQueryPart t 'j' "OR" : groupQs
      in (groupT + 1, qs)

    evalSearchExpr' t (Not es) =
      let (groupT, groupQs) = evalSearchExpr' t es
          qs = taggedQueryPart t 'n' "1" : groupQs
      in (groupT + 1, qs)

    evalSearchExpr' t (Term term) = (t + 1, evalSearchTerm t term)
