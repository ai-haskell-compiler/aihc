{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Shared System FC dictionary-layout operations.
module Aihc.Fc.Desugar.Dictionary
  ( classMethodFieldType,
    checkedConstraintType,
    defaultMethodName,
    peelForAlls,
    peelQuals,
    predType,
  )
where

import Aihc.Fc.Desugar.Expr (DsM, DsState (..), desugarBug)
import Aihc.Fc.Syntax (legacyTyCon)
import Aihc.Tc.Types (Pred (..), TcType (..), TyCon (..), TyVarId, typeKindInEnv, pattern KConstraint)
import Control.Monad.Trans.State.Strict (gets)
import Data.Text (Text)
import Data.Text qualified as T

defaultMethodName :: Text -> Text
defaultMethodName methodName = "$dm" <> methodName

peelForAlls :: TcType -> ([TyVarId], TcType)
peelForAlls (TcForAllTy tv rest) =
  let (tvs, inner) = peelForAlls rest
   in (tv : tvs, inner)
peelForAlls ty = ([], ty)

peelQuals :: TcType -> ([Pred], TcType)
peelQuals (TcQualTy preds body) = (preds, body)
peelQuals ty = ([], ty)

predType :: Pred -> TcType
predType (ClassPred classTyCon args) = TcTyCon classTyCon args
predType (EqPred left right) = TcTyCon (legacyTyCon "~" 2) [left, right]

checkedConstraintType :: String -> TcType -> DsM TcType
checkedConstraintType context ty = do
  kindEnv <- gets dsKindEnv
  case typeKindInEnv kindEnv ty of
    Left problem -> desugarBug (context <> " has no kind: " <> problem)
    Right KConstraint -> pure ty
    Right kind -> desugarBug (context <> " does not have the checked Constraint kind: " <> show kind)

classMethodFieldType :: Text -> [TyVarId] -> TcType -> DsM TcType
classMethodFieldType className classTyVars methodType = do
  remainingPredicates <-
    case removeClassPredicate predicates of
      Just result -> pure result
      Nothing -> desugarBug ("class method lacks its class predicate for " <> T.unpack className)
  let extraTyVars = filter (`notElem` classTyVars) methodTyVars
      qualifiedBody =
        if null remainingPredicates
          then body
          else TcQualTy remainingPredicates body
  pure (foldr TcForAllTy qualifiedBody extraTyVars)
  where
    (methodTyVars, afterForAlls) = peelForAlls methodType
    (predicates, body) = peelQuals afterForAlls
    removeClassPredicate [] = Nothing
    removeClassPredicate (predicate : rest) =
      case predicate of
        ClassPred predicateClass _
          | tyConName predicateClass == className -> Just rest
        _ -> (predicate :) <$> removeClassPredicate rest
