{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Compulsory System FC lowering that establishes forms required by every
-- evaluator and backend, independently of optional optimization.
module Aihc.Fc.Lower
  ( lowerPseudoOps,
    seqPseudoOpName,
  )
where

import Aihc.Fc.Subst (substExprVar, substType)
import Aihc.Fc.Syntax
import Aihc.Tc.Types (TcType (..), Unique (..))
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

-- | An internal name used only between expression desugaring and the
-- compulsory pseudo-op expansion. It cannot collide with a source name.
seqPseudoOpName :: Text
seqPseudoOpName = "$aihc.seq"

-- | Lower every pseudo-op to ordinary System FC. This pass is compulsory:
-- @seq@ must be gone before FC reaches evaluation or GRIN lowering because
-- its second argument may be unlifted, so representing a saturated call as an
-- ordinary function call could evaluate that argument before the first one.
lowerPseudoOps :: FcProgram -> FcProgram
lowerPseudoOps program = evalState (lowerProgram program) (nextUnique program)

type ExpandM = State Int

lowerProgram :: FcProgram -> ExpandM FcProgram
lowerProgram (FcProgram moduleId topBinds) = FcProgram moduleId <$> mapM expandTopBind topBinds

expandTopBind :: FcTopBind -> ExpandM FcTopBind
expandTopBind topBind =
  case topBind of
    FcTopBind bind -> FcTopBind <$> expandBind bind
    _ -> pure topBind

expandBind :: FcBind -> ExpandM FcBind
expandBind bind =
  case bind of
    FcNonRec binder rhs -> FcNonRec binder <$> expandExpr rhs
    FcRec bindings -> FcRec <$> mapM (\(binder, rhs) -> (binder,) <$> expandExpr rhs) bindings

expandExpr :: FcExpr -> ExpandM FcExpr
expandExpr expression =
  case collectSeqApplication expression of
    Just (headExpression, arguments) -> do
      arguments' <- mapM expandExpr arguments
      expandSeqApplication headExpression arguments'
    Nothing ->
      case expression of
        FcVar {} -> pure expression
        FcLit {} -> pure expression
        FcApp function argument -> FcApp <$> expandExpr function <*> expandExpr argument
        FcTyApp function ty -> (`FcTyApp` ty) <$> expandExpr function
        FcLam binder body -> FcLam binder <$> expandExpr body
        FcTyLam tyVar body -> FcTyLam tyVar <$> expandExpr body
        FcLet bind body -> FcLet <$> expandBind bind <*> expandExpr body
        FcCase scrutinee binder resultType alternatives ->
          FcCase <$> expandExpr scrutinee <*> pure binder <*> pure resultType <*> mapM expandAlt alternatives
        FcCast inner coercion -> (`FcCast` coercion) <$> expandExpr inner
        FcCallForeign foreignCall arguments -> FcCallForeign foreignCall <$> mapM expandExpr arguments

expandAlt :: FcAlt -> ExpandM FcAlt
expandAlt alternative = do
  rhs <- expandExpr (altRhs alternative)
  pure alternative {altRhs = rhs}

collectSeqApplication :: FcExpr -> Maybe (FcExpr, [FcExpr])
collectSeqApplication = go []
  where
    go arguments (FcApp function argument) = go (argument : arguments) function
    go arguments headExpression
      | isSeqHead headExpression = Just (headExpression, arguments)
      | otherwise = Nothing

isSeqHead :: FcExpr -> Bool
isSeqHead expression =
  case expression of
    FcVar var -> varName var == seqPseudoOpName
    FcTyApp function _ -> isSeqHead function
    _ -> False

expandSeqApplication :: FcExpr -> [FcExpr] -> ExpandM FcExpr
expandSeqApplication headExpression arguments = do
  (firstType, secondType) <-
    case expressionType headExpression of
      Just (TcFunTy first (TcFunTy second _)) -> pure (first, second)
      other -> error ("FC pseudo-op expansion found malformed seq type: " <> show other)
  case arguments of
    [] -> do
      first <- freshVar "$seq_first" firstType
      second <- freshVar "$seq_second" secondType
      FcLam first . FcLam second <$> seqCase (FcVar first) (FcVar second)
    [first] -> do
      second <- freshVar "$seq_second" secondType
      FcLam second <$> seqCase first (FcVar second)
    first : second : extraArguments ->
      seqCase first (foldl FcApp second extraArguments)

seqCase :: FcExpr -> FcExpr -> ExpandM FcExpr
seqCase first second = do
  binder <-
    case expressionType first of
      Just ty -> freshVar "$seq_whnf" ty
      Nothing -> error "FC pseudo-op expansion could not determine seq's first argument type"
  let result =
        case first of
          FcVar source -> substExprVar source binder second
          _ -> second
  resultType <-
    case expressionType result of
      Just ty -> pure ty
      Nothing -> error "FC pseudo-op expansion could not determine seq's result type"
  pure (FcCase first binder resultType [FcAlt DefaultAlt [] result])

expressionType :: FcExpr -> Maybe TcType
expressionType expression =
  case expression of
    FcVar var -> Just (varType var)
    FcLit _ ty -> Just ty
    FcApp function _ -> expressionType function >>= functionResultType
    FcTyApp function ty -> do
      functionType <- expressionType function
      case functionType of
        TcForAllTy tyVar body -> Just (substType (Map.singleton tyVar ty) body)
        _ -> Nothing
    FcLam binder body -> TcFunTy (varType binder) <$> expressionType body
    FcTyLam tyVar body -> TcForAllTy tyVar <$> expressionType body
    FcLet _ body -> expressionType body
    FcCase _ _ resultType _ -> Just resultType
    FcCast inner _ -> expressionType inner
    FcCallForeign foreignCall _ -> Just (fcForeignCallResultType (fcForeignCallSignature foreignCall))
  where
    functionResultType = \case
      TcFunTy _ result -> Just result
      TcQualTy [] body -> functionResultType body
      TcQualTy (_ : predicates) body -> Just (if null predicates then body else TcQualTy predicates body)
      _ -> Nothing

freshVar :: Text -> TcType -> ExpandM Var
freshVar name ty = do
  unique <- get
  put (unique + 1)
  pure (Var name (Unique unique) ty)

nextUnique :: FcProgram -> Int
nextUnique (FcProgram _ topBinds) = maximum (0 : concatMap topBindUniques topBinds) + 1

topBindUniques :: FcTopBind -> [Int]
topBindUniques topBind =
  case topBind of
    FcExternal {} -> []
    FcData {} -> []
    FcAxiom {} -> []
    FcNewtype {} -> []
    FcPrimitive var _ -> varUniques var
    FcForeignImport _ -> []
    FcTopBind bind -> bindUniques bind

bindUniques :: FcBind -> [Int]
bindUniques bind =
  case bind of
    FcNonRec binder rhs -> varUniques binder <> exprUniques rhs
    FcRec bindings -> concatMap (\(binder, rhs) -> varUniques binder <> exprUniques rhs) bindings

exprUniques :: FcExpr -> [Int]
exprUniques expression =
  case expression of
    FcVar var -> varUniques var
    FcLit {} -> []
    FcApp function argument -> exprUniques function <> exprUniques argument
    FcTyApp function _ -> exprUniques function
    FcLam binder body -> varUniques binder <> exprUniques body
    FcTyLam _ body -> exprUniques body
    FcLet bind body -> bindUniques bind <> exprUniques body
    FcCase scrutinee binder _ alternatives ->
      exprUniques scrutinee <> varUniques binder <> concatMap altUniques alternatives
    FcCast inner _ -> exprUniques inner
    FcCallForeign _ arguments -> concatMap exprUniques arguments

altUniques :: FcAlt -> [Int]
altUniques alternative = concatMap varUniques (altBinders alternative) <> exprUniques (altRhs alternative)

varUniques :: Var -> [Int]
varUniques var = [uniqueValue (varUnique var)]

uniqueValue :: Unique -> Int
uniqueValue (Unique unique) = unique
