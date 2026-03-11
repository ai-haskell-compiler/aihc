{-# LANGUAGE RecordWildCards #-}
module Data.Aeson.JSONPath.Query.Filter
  ( filterOrExpr
  , filterOrExprLocated )
  where

import Data.Aeson                      (Value)
import Data.Vector                     (Vector)

import qualified Data.Aeson            as JSON
import qualified Data.Aeson.KeyMap     as KM
import qualified Data.Aeson.Key        as K
import qualified Data.Vector           as V

import Data.Aeson.JSONPath.Types

import Prelude

filterOrExpr :: LogicalOrExpr Query -> QueryState -> Vector Value
filterOrExpr expr qS@QueryState{ curVal=(JSON.Object obj) } = V.filter (\cur -> evaluateLogicalOrExpr expr qS{ curVal=cur}) (V.fromList $ KM.elems obj)
filterOrExpr expr qS@QueryState{ curVal=(JSON.Array arr) } = V.filter (\cur -> evaluateLogicalOrExpr expr qS{ curVal=cur }) arr
filterOrExpr _ _ = V.empty

filterOrExprLocated :: LogicalOrExpr Query -> QueryState -> String -> Vector (String,Value)
filterOrExprLocated expr  qS@QueryState{ curVal=(JSON.Object obj) } loc = V.filter (\(_,x) -> evaluateLogicalOrExpr expr qS{ curVal=x}) (V.fromList $ zip locsWithKeys (KM.elems obj))
  where
    locsWithKeys = map (\x -> loc ++ "['" ++ K.toString x ++ "']") (KM.keys obj)
filterOrExprLocated expr qS@QueryState{ curVal=(JSON.Array arr) } loc = V.filter (\(_,x) -> evaluateLogicalOrExpr expr qS{ curVal=x}) (V.zip (V.fromList locsWithIdxs) arr)
  where
    locsWithIdxs = map (\x -> loc ++ "[" ++ show x ++ "]") [0..(V.length arr - 1)]
filterOrExprLocated _ _ _ = V.empty


evaluateLogicalOrExpr :: LogicalOrExpr Query -> QueryState -> Bool
evaluateLogicalOrExpr (LogicalOr exprs) qS = any (`evaluateLogicalAndExpr` qS) exprs


evaluateLogicalAndExpr :: LogicalAndExpr Query -> QueryState -> Bool
evaluateLogicalAndExpr (LogicalAnd exprs) qS = all (`evaluateBasicExpr` qS) exprs


evaluateBasicExpr :: BasicExpr Query -> QueryState -> Bool
evaluateBasicExpr (Paren expr)      qS = evaluateLogicalOrExpr expr qS
evaluateBasicExpr (NotParen expr)   qS = not $ evaluateLogicalOrExpr expr qS
evaluateBasicExpr (Test expr)       qS = evaluateTestExpr expr qS
evaluateBasicExpr (NotTest expr)    qS = not $ evaluateTestExpr expr qS
evaluateBasicExpr (Comparison expr) qS = evaluateCompExpr expr qS


evaluateTestExpr :: TestExpr Query -> QueryState -> Bool
evaluateTestExpr (FilterQuery expr) qS@QueryState{..} = not $ null $ executeQuery expr qS


evaluateCompExpr :: ComparisonExpr -> QueryState -> Bool
evaluateCompExpr (Comp leftC op rightC) qS  = compareVals op (getComparableVal leftC qS) (getComparableVal rightC qS)


compareVals :: ComparisonOp -> Maybe Value -> Maybe Value -> Bool
compareVals Less (Just (JSON.String s1)) (Just (JSON.String s2)) = s1 < s2
compareVals Less (Just (JSON.Number n1)) (Just (JSON.Number n2)) = n1 < n2
compareVals Less _  _ = False

compareVals LessOrEqual    o1 o2 = compareVals Less o1 o2 || compareVals Equal o1 o2
compareVals Greater        o1 o2 = compareVals Less o2 o1
compareVals GreaterOrEqual o1 o2 = compareVals Less o2 o1 || compareVals Equal o1 o2
compareVals Equal          o1 o2 = o1 == o2
compareVals NotEqual       o1 o2 = o1 /= o2


getComparableVal :: Comparable -> QueryState -> Maybe Value
getComparableVal (CompLit lit) _ = 
  case lit of
    LitString txt -> Just $ JSON.String txt
    LitNum num    -> Just $ JSON.Number num
    LitBool bool  -> Just $ JSON.Bool bool
    LitNull       -> Just JSON.Null

getComparableVal (CompSQ SingularQuery{..}) QueryState{..} = case singularQueryType of
  RootSQ -> traverseSingularQSegs (Just rootVal) singularQuerySegments
  CurrentSQ -> traverseSingularQSegs (Just curVal) singularQuerySegments


traverseSingularQSegs :: Maybe Value -> [SingularQuerySegment] -> Maybe Value
traverseSingularQSegs = foldl lookupSingleQSeg


lookupSingleQSeg :: Maybe Value -> SingularQuerySegment -> Maybe Value
lookupSingleQSeg (Just (JSON.Object obj)) (NameSQSeg txt) = KM.lookup (K.fromText txt) obj
lookupSingleQSeg (Just (JSON.Array arr)) (IndexSQSeg idx) = (V.!?) arr idx
lookupSingleQSeg _ _ = Nothing
