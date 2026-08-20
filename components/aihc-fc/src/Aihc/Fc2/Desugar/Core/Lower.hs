{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Compulsory System FC lowering that establishes forms required by every
-- evaluator and backend, independently of optional optimization.
module Aihc.Fc2.Desugar.Core.Lower
  ( lowerPseudoOps,
    seqPseudoOpName,
  )
where

import Aihc.Fc2.Desugar.Core.Subst (substExprVar, substType)
import Aihc.Fc2.Desugar.Core.Syntax
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
lowerPseudoOps :: CoreProgram -> CoreProgram
lowerPseudoOps program = evalState (lowerProgram program) (nextUnique program)

type ExpandM = State Int

lowerProgram :: CoreProgram -> ExpandM CoreProgram
lowerProgram (CoreProgram moduleId topBinds) = CoreProgram moduleId <$> mapM expandTopBind topBinds

expandTopBind :: CoreTopBind -> ExpandM CoreTopBind
expandTopBind topBind =
  case topBind of
    CoreTopBind bind -> CoreTopBind <$> expandBind bind
    _ -> pure topBind

expandBind :: CoreBind -> ExpandM CoreBind
expandBind bind =
  case bind of
    CoreNonRec binder rhs -> CoreNonRec binder <$> expandExpr rhs
    CoreRec bindings -> CoreRec <$> mapM (\(binder, rhs) -> (binder,) <$> expandExpr rhs) bindings

expandExpr :: CoreExpr -> ExpandM CoreExpr
expandExpr expression =
  case collectSeqApplication expression of
    Just (headExpression, arguments) -> do
      arguments' <- mapM expandExpr arguments
      expandSeqApplication headExpression arguments'
    Nothing ->
      case expression of
        CoreVar {} -> pure expression
        CoreLit {} -> pure expression
        CoreApp function argument -> CoreApp <$> expandExpr function <*> expandExpr argument
        CoreTyApp function ty -> (`CoreTyApp` ty) <$> expandExpr function
        CoreLam binder body -> CoreLam binder <$> expandExpr body
        CoreTyLam tyVar body -> CoreTyLam tyVar <$> expandExpr body
        CoreLet bind body -> CoreLet <$> expandBind bind <*> expandExpr body
        CoreCase scrutinee binder alternatives ->
          CoreCase <$> expandExpr scrutinee <*> pure binder <*> mapM expandAlt alternatives
        CoreCast inner coercion -> (`CoreCast` coercion) <$> expandExpr inner
        CoreCallForeign foreignCall arguments -> CoreCallForeign foreignCall <$> mapM expandExpr arguments

expandAlt :: CoreAlt -> ExpandM CoreAlt
expandAlt alternative = do
  rhs <- expandExpr (altRhs alternative)
  pure alternative {altRhs = rhs}

collectSeqApplication :: CoreExpr -> Maybe (CoreExpr, [CoreExpr])
collectSeqApplication = go []
  where
    go arguments (CoreApp function argument) = go (argument : arguments) function
    go arguments headExpression
      | isSeqHead headExpression = Just (headExpression, arguments)
      | otherwise = Nothing

isSeqHead :: CoreExpr -> Bool
isSeqHead expression =
  case expression of
    CoreVar var -> varName var == seqPseudoOpName
    CoreTyApp function _ -> isSeqHead function
    _ -> False

expandSeqApplication :: CoreExpr -> [CoreExpr] -> ExpandM CoreExpr
expandSeqApplication headExpression arguments = do
  (firstType, secondType) <-
    case expressionType headExpression of
      Just (TcFunTy first (TcFunTy second _)) -> pure (first, second)
      other -> error ("FC pseudo-op expansion found malformed seq type: " <> show other)
  case arguments of
    [] -> do
      first <- freshVar "$seq_first" firstType
      second <- freshVar "$seq_second" secondType
      CoreLam first . CoreLam second <$> seqCase (CoreVar first) (CoreVar second)
    [first] -> do
      second <- freshVar "$seq_second" secondType
      CoreLam second <$> seqCase first (CoreVar second)
    first : second : extraArguments ->
      seqCase first (foldl CoreApp second extraArguments)

seqCase :: CoreExpr -> CoreExpr -> ExpandM CoreExpr
seqCase first second = do
  binder <-
    case expressionType first of
      Just ty -> freshVar "$seq_whnf" ty
      Nothing -> error "FC pseudo-op expansion could not determine seq's first argument type"
  let result =
        case first of
          CoreVar source -> substExprVar source binder second
          _ -> second
  pure (CoreCase first binder [CoreAlt DefaultAlt [] result])

expressionType :: CoreExpr -> Maybe TcType
expressionType expression =
  case expression of
    CoreVar var -> Just (varType var)
    CoreLit _ ty -> Just ty
    CoreApp function _ -> expressionType function >>= functionResultType
    CoreTyApp function ty -> do
      functionType <- expressionType function
      case functionType of
        TcForAllTy tyVar body -> Just (substType (Map.singleton tyVar ty) body)
        _ -> Nothing
    CoreLam binder body -> TcFunTy (varType binder) <$> expressionType body
    CoreTyLam tyVar body -> TcForAllTy tyVar <$> expressionType body
    CoreLet _ body -> expressionType body
    CoreCase _ _ alternatives ->
      case alternatives of
        alternative : _ -> expressionType (altRhs alternative)
        [] -> Nothing
    CoreCast inner _ -> expressionType inner
    CoreCallForeign foreignCall _ -> Just (coreForeignCallResultType (coreForeignCallSignature foreignCall))
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

nextUnique :: CoreProgram -> Int
nextUnique (CoreProgram _ topBinds) = maximum (0 : concatMap topBindUniques topBinds) + 1

topBindUniques :: CoreTopBind -> [Int]
topBindUniques topBind =
  case topBind of
    CoreExternal {} -> []
    CoreData {} -> []
    CoreAxiom {} -> []
    CoreNewtype {} -> []
    CorePrimitive var _ -> varUniques var
    CoreForeignImport _ -> []
    CoreTopBind bind -> bindUniques bind

bindUniques :: CoreBind -> [Int]
bindUniques bind =
  case bind of
    CoreNonRec binder rhs -> varUniques binder <> exprUniques rhs
    CoreRec bindings -> concatMap (\(binder, rhs) -> varUniques binder <> exprUniques rhs) bindings

exprUniques :: CoreExpr -> [Int]
exprUniques expression =
  case expression of
    CoreVar var -> varUniques var
    CoreLit {} -> []
    CoreApp function argument -> exprUniques function <> exprUniques argument
    CoreTyApp function _ -> exprUniques function
    CoreLam binder body -> varUniques binder <> exprUniques body
    CoreTyLam _ body -> exprUniques body
    CoreLet bind body -> bindUniques bind <> exprUniques body
    CoreCase scrutinee binder alternatives ->
      exprUniques scrutinee <> varUniques binder <> concatMap altUniques alternatives
    CoreCast inner _ -> exprUniques inner
    CoreCallForeign _ arguments -> concatMap exprUniques arguments

altUniques :: CoreAlt -> [Int]
altUniques alternative = concatMap varUniques (altBinders alternative) <> exprUniques (altRhs alternative)

varUniques :: Var -> [Int]
varUniques var = [uniqueValue (varUnique var)]

uniqueValue :: Unique -> Int
uniqueValue (Unique unique) = unique
