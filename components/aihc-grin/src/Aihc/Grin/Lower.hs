{-# LANGUAGE OverloadedStrings #-}

-- | Lowering from non-strict System FC to strict, runtime-explicit GRIN.
module Aihc.Grin.Lower
  ( lowerProgram,
  )
where

import Aihc.Fc.Syntax
import Aihc.Grin.Syntax
import Aihc.Tc.Types (Unique (..))
import Control.Monad.Trans.State.Strict (State, gets, modify', runState)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

data LowerState = LowerState
  { lowerNextUnique :: !Int,
    lowerNextFunction :: !Int,
    lowerFunctionsRev :: ![GrinFunction],
    lowerPrimitiveNames :: !(Set Text)
  }

type LowerM = State LowerState

data LoweredTop = LoweredTop
  { loweredConstructors :: ![(Text, Int)],
    loweredPrimitives :: ![(GrinVar, Int)],
    loweredForeignCalls :: ![GrinForeignCall],
    loweredCafs :: ![(GrinVar, GrinNode)]
  }

instance Semigroup LoweredTop where
  left <> right =
    LoweredTop
      { loweredConstructors = loweredConstructors left <> loweredConstructors right,
        loweredPrimitives = loweredPrimitives left <> loweredPrimitives right,
        loweredForeignCalls = loweredForeignCalls left <> loweredForeignCalls right,
        loweredCafs = loweredCafs left <> loweredCafs right
      }

instance Monoid LoweredTop where
  mempty = LoweredTop [] [] [] []

-- | Erase FC types and coercions, closure-convert lambdas and thunks, and make
-- evaluation, application, allocation, and exception control explicit.
lowerProgram :: FcProgram -> GrinProgram
lowerProgram program =
  GrinProgram
    { grinConstructors = loweredConstructors tops,
      grinPrimitives = loweredPrimitives tops,
      grinForeignCalls = loweredForeignCalls tops,
      grinCafs = loweredCafs tops,
      grinFunctions = reverse (lowerFunctionsRev finalState)
    }
  where
    initialState =
      LowerState
        { lowerNextUnique = maximum (0 : map sourceUnique (programVars program)) + 1,
          lowerNextFunction = 0,
          lowerFunctionsRev = [],
          lowerPrimitiveNames =
            Set.fromList
              [ varName var
              | FcPrimitive var _ <- fcTopBinds program
              ]
        }
    (topParts, finalState) = runState (mapM lowerTopBind (fcTopBinds program)) initialState
    tops = mconcat topParts

lowerTopBind :: FcTopBind -> LowerM LoweredTop
lowerTopBind topBind =
  case topBind of
    FcData _ _ constructors ->
      pure mempty {loweredConstructors = [(name, length fields) | (name, fields) <- constructors]}
    FcNewtype _ _ constructor _ ->
      pure mempty {loweredConstructors = [(constructor, 1)]}
    FcPrimitive var arity ->
      pure mempty {loweredPrimitives = [(lowerVar var, arity)]}
    FcForeignImport foreignCall ->
      pure mempty {loweredForeignCalls = [lowerForeignCall foreignCall]}
    FcTopBind bind -> do
      cafs <- lowerCafBind bind
      pure mempty {loweredCafs = cafs}

lowerCafBind :: FcBind -> LowerM [(GrinVar, GrinNode)]
lowerCafBind bind =
  case bind of
    FcNonRec var expr -> do
      topVar <- freshTopVar var
      node <- makeThunk expr
      pure [(topVar, node)]
    FcRec bindings ->
      mapM lowerBinding bindings
  where
    lowerBinding (var, expr) = do
      topVar <- freshTopVar var
      node <- makeThunk expr
      pure (topVar, node)

lowerExpr :: FcExpr -> LowerM GrinExpr
lowerExpr expr = do
  primitiveNames <- gets lowerPrimitiveNames
  case primitiveApplication primitiveNames expr of
    Just ("raise#", [exception]) ->
      lowerStrict "exception" exception (pure . GrinThrow)
    Just ("catch#", [action, handler, state]) ->
      lowerDelayed action $ \actionValue ->
        lowerDelayed handler $ \handlerValue ->
          lowerDelayed state $ \stateValue ->
            pure (GrinCatch actionValue handlerValue stateValue)
    _ -> lowerOrdinaryExpr expr

lowerOrdinaryExpr :: FcExpr -> LowerM GrinExpr
lowerOrdinaryExpr expr =
  case expr of
    FcVar var ->
      pure (GrinEval (GrinVarValue (lowerVar var)))
    FcLit literal ->
      pure (GrinReturn (GrinLitValue (lowerLiteral literal)))
    FcApp function argument ->
      lowerApplication function argument
    FcDictApp function argument ->
      lowerApplication function argument
    FcTyApp inner _ ->
      lowerExpr inner
    FcLam var body ->
      lowerLambda var body
    FcTyLam _ body ->
      lowerExpr body
    FcDictLam var body ->
      lowerLambda var body
    FcDict fields ->
      lowerDelayedMany fields $ \values ->
        pure (GrinReturn (GrinNodeValue (GrinNode GrinDictionary values)))
    FcDictSelect dictionary index ->
      lowerStrict "dictionary" dictionary $ \value ->
        pure (GrinDictSelect value index)
    FcLet bind body ->
      lowerLet bind body
    FcCase scrutinee binder alternatives ->
      lowerStrict "scrutinee" scrutinee $ \value -> do
        loweredAlternatives <- mapM lowerAlt alternatives
        pure (GrinCase value (lowerVar binder) loweredAlternatives)
    FcCast inner _ ->
      lowerExpr inner

lowerApplication :: FcExpr -> FcExpr -> LowerM GrinExpr
lowerApplication function argument =
  lowerStrict "function" function $ \functionValue ->
    lowerDelayed argument $ \argumentValue ->
      pure (GrinApply functionValue argumentValue)

lowerLambda :: Var -> FcExpr -> LowerM GrinExpr
lowerLambda binder body = do
  let captures = map lowerVar (Set.toAscList (freeVars (FcLam binder body)))
  functionName <- freshFunction "closure"
  loweredBody <- lowerExpr body
  emitFunction
    GrinFunction
      { grinFunctionName = functionName,
        grinFunctionParameters = captures <> [lowerVar binder],
        grinFunctionBody = loweredBody
      }
  pure
    ( GrinReturn
        ( GrinNodeValue
            (GrinNode (GrinClosure functionName) (map GrinVarValue captures))
        )
    )

lowerLet :: FcBind -> FcExpr -> LowerM GrinExpr
lowerLet bind body =
  case bind of
    FcNonRec var rhs -> do
      node <- makeThunk rhs
      loweredBody <- lowerExpr body
      pure (GrinBind (lowerVar var) (GrinStore node) loweredBody)
    FcRec bindings -> do
      nodes <- mapM lowerBinding bindings
      loweredBody <- lowerExpr body
      pure (GrinStoreRec nodes loweredBody)
  where
    lowerBinding (var, rhs) = do
      node <- makeThunk rhs
      pure (lowerVar var, node)

lowerAlt :: FcAlt -> LowerM GrinAlt
lowerAlt alt = do
  rhs <- lowerExpr (altRhs alt)
  pure
    GrinAlt
      { grinAltCon = lowerAltCon (altCon alt),
        grinAltBinders = map lowerVar (altBinders alt),
        grinAltRhs = rhs
      }

makeThunk :: FcExpr -> LowerM GrinNode
makeThunk expr = do
  let captures = map lowerVar (Set.toAscList (freeVars expr))
  functionName <- freshFunction "thunk"
  body <- lowerExpr expr
  emitFunction
    GrinFunction
      { grinFunctionName = functionName,
        grinFunctionParameters = captures,
        grinFunctionBody = body
      }
  pure (GrinNode (GrinThunk functionName) (map GrinVarValue captures))

lowerStrict :: Text -> FcExpr -> (GrinValue -> LowerM GrinExpr) -> LowerM GrinExpr
lowerStrict hint expr continuation = do
  valueVar <- freshVar hint
  valueExpr <- lowerExpr expr
  rest <- continuation (GrinVarValue valueVar)
  pure (GrinBind valueVar valueExpr rest)

lowerDelayed :: FcExpr -> (GrinValue -> LowerM GrinExpr) -> LowerM GrinExpr
lowerDelayed expr continuation = do
  pointerVar <- freshVar "thunk"
  node <- makeThunk expr
  rest <- continuation (GrinVarValue pointerVar)
  pure (GrinBind pointerVar (GrinStore node) rest)

lowerDelayedMany :: [FcExpr] -> ([GrinValue] -> LowerM GrinExpr) -> LowerM GrinExpr
lowerDelayedMany expressions continuation =
  case expressions of
    [] -> continuation []
    first : rest ->
      lowerDelayed first $ \firstValue ->
        lowerDelayedMany rest $ \restValues ->
          continuation (firstValue : restValues)

freshVar :: Text -> LowerM GrinVar
freshVar hint = do
  unique <- gets lowerNextUnique
  modify' $ \state -> state {lowerNextUnique = unique + 1}
  pure (GrinVar ("$grin_" <> hint <> "_" <> T.pack (show unique)) unique)

freshTopVar :: Var -> LowerM GrinVar
freshTopVar var = do
  unique <- gets lowerNextUnique
  modify' $ \state -> state {lowerNextUnique = unique + 1}
  pure (GrinVar (varName var) unique)

freshFunction :: Text -> LowerM FunctionName
freshFunction kind = do
  index <- gets lowerNextFunction
  modify' $ \state -> state {lowerNextFunction = index + 1}
  pure (FunctionName ("$grin_" <> kind <> "_" <> T.pack (show index)))

emitFunction :: GrinFunction -> LowerM ()
emitFunction function =
  modify' $ \state -> state {lowerFunctionsRev = function : lowerFunctionsRev state}

primitiveApplication :: Set Text -> FcExpr -> Maybe (Text, [FcExpr])
primitiveApplication primitiveNames expr =
  case collectApplications expr of
    (FcVar var, arguments)
      | varName var `Set.member` primitiveNames -> Just (varName var, arguments)
    _ -> Nothing

collectApplications :: FcExpr -> (FcExpr, [FcExpr])
collectApplications expr =
  case expr of
    FcApp function argument ->
      let (headExpr, arguments) = collectApplications function
       in (headExpr, arguments <> [argument])
    FcDictApp function argument ->
      let (headExpr, arguments) = collectApplications function
       in (headExpr, arguments <> [argument])
    FcTyApp inner _ -> collectApplications inner
    _ -> (expr, [])

freeVars :: FcExpr -> Set Var
freeVars expr =
  case expr of
    FcVar var -> Set.singleton var
    FcLit _ -> Set.empty
    FcApp function argument -> freeVars function <> freeVars argument
    FcDictApp function argument -> freeVars function <> freeVars argument
    FcTyApp inner _ -> freeVars inner
    FcLam var body -> Set.delete var (freeVars body)
    FcTyLam _ body -> freeVars body
    FcDictLam var body -> Set.delete var (freeVars body)
    FcDict fields -> foldMap freeVars fields
    FcDictSelect dictionary _ -> freeVars dictionary
    FcLet bind body -> freeVarsBind bind body
    FcCase scrutinee binder alternatives ->
      freeVars scrutinee
        <> Set.delete binder (foldMap freeVarsAlt alternatives)
    FcCast inner _ -> freeVars inner

freeVarsBind :: FcBind -> FcExpr -> Set Var
freeVarsBind bind body =
  case bind of
    FcNonRec var rhs -> freeVars rhs <> Set.delete var (freeVars body)
    FcRec bindings ->
      let binders = Set.fromList (map fst bindings)
          allFree = foldMap (freeVars . snd) bindings <> freeVars body
       in allFree `Set.difference` binders

freeVarsAlt :: FcAlt -> Set Var
freeVarsAlt alt =
  freeVars (altRhs alt) `Set.difference` Set.fromList (altBinders alt)

lowerVar :: Var -> GrinVar
lowerVar var = GrinVar (varName var) (sourceUnique var)

sourceUnique :: Var -> Int
sourceUnique var =
  case varUnique var of
    Unique unique -> unique

lowerLiteral :: Literal -> GrinLiteral
lowerLiteral literal =
  case literal of
    LitInt value -> GrinLitInt value
    LitChar value -> GrinLitChar value
    LitString value -> GrinLitString value

lowerAltCon :: FcAltCon -> GrinAltCon
lowerAltCon altCon =
  case altCon of
    DataAlt name -> GrinDataAlt name
    LitAlt literal -> GrinLitAlt (lowerLiteral literal)
    DefaultAlt -> GrinDefaultAlt

lowerForeignCall :: FcForeignCall -> GrinForeignCall
lowerForeignCall foreignCall =
  GrinForeignCall
    { grinForeignCallName = varName (fcForeignCallVar foreignCall),
      grinForeignCallSymbol = fcForeignCallSymbol foreignCall,
      grinForeignCallSignature = lowerForeignSignature (fcForeignCallSignature foreignCall)
    }

lowerForeignSignature :: FcForeignSignature -> GrinForeignSignature
lowerForeignSignature signature =
  GrinForeignSignature
    { grinForeignArgumentTypes = map lowerForeignType (fcForeignArgumentTypes signature),
      grinForeignResult = lowerForeignResult (fcForeignResult signature)
    }

lowerForeignResult :: FcForeignResult -> GrinForeignResult
lowerForeignResult result =
  case result of
    FcForeignPure foreignType -> GrinForeignPure (lowerForeignType foreignType)
    FcForeignIO foreignType -> GrinForeignIO (lowerForeignType foreignType)

lowerForeignType :: FcForeignType -> GrinForeignType
lowerForeignType foreignType =
  case foreignType of
    FcForeignCInt -> GrinForeignCInt

programVars :: FcProgram -> [Var]
programVars program = concatMap topVars (fcTopBinds program)

topVars :: FcTopBind -> [Var]
topVars topBind =
  case topBind of
    FcData {} -> []
    FcNewtype {} -> []
    FcPrimitive var _ -> [var]
    FcForeignImport foreignCall -> [fcForeignCallVar foreignCall]
    FcTopBind bind -> bindVars bind

bindVars :: FcBind -> [Var]
bindVars bind =
  case bind of
    FcNonRec var expr -> var : exprVars expr
    FcRec bindings -> concatMap (\(var, expr) -> var : exprVars expr) bindings

exprVars :: FcExpr -> [Var]
exprVars expr =
  case expr of
    FcVar var -> [var]
    FcLit _ -> []
    FcApp function argument -> exprVars function <> exprVars argument
    FcDictApp function argument -> exprVars function <> exprVars argument
    FcTyApp inner _ -> exprVars inner
    FcLam var body -> var : exprVars body
    FcTyLam _ body -> exprVars body
    FcDictLam var body -> var : exprVars body
    FcDict fields -> concatMap exprVars fields
    FcDictSelect dictionary _ -> exprVars dictionary
    FcLet bind body -> bindVars bind <> exprVars body
    FcCase scrutinee binder alternatives ->
      exprVars scrutinee <> (binder : concatMap altVars alternatives)
    FcCast inner _ -> exprVars inner

altVars :: FcAlt -> [Var]
altVars alt = grinAltBinders' <> exprVars (altRhs alt)
  where
    grinAltBinders' = altBinders alt
