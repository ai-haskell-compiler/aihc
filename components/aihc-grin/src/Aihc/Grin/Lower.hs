{-# LANGUAGE OverloadedStrings #-}

-- | Lowering from non-strict System FC to strict, runtime-explicit GRIN.
module Aihc.Grin.Lower
  ( lowerProgram,
    lowerPrograms,
  )
where

import Aihc.Fc.Newtype (lowerNewtypes)
import Aihc.Fc.Subst (substType)
import Aihc.Fc.Syntax
import Aihc.Grin.Syntax
import Aihc.Tc.Prim (PrimOp, primOpArity, primOpName, schedulerPrimOp)
import Aihc.Tc.Types
  ( RuntimeRep,
    TcType (..),
    Unique (..),
    liftedRuntimeRep,
    runtimeRepOfType,
    tyConArity,
    tyConName,
  )
import Control.Monad (filterM)
import Control.Monad.Trans.State.Strict (State, gets, modify', runState)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

data LowerState = LowerState
  { lowerNextUnique :: !Int,
    lowerNextFunction :: !Int,
    lowerFunctionsRev :: ![GrinFunction],
    lowerPrimitiveOps :: !(Map.Map Text PrimOp),
    lowerGlobalNames :: !(Set Text),
    lowerWhnfGlobalNames :: !(Set Text),
    lowerLocalVars :: !(Set (Text, Unique))
  }

type LowerM = State LowerState

data LoweredTop = LoweredTop
  { loweredConstructors :: ![(Text, [RuntimeRep])],
    loweredPrimitives :: ![(GrinVar, PrimOp)],
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

-- | Erase FC types and coercions while retaining their runtime
-- representations, closure-convert lambdas and thunks, and make evaluation,
-- application, allocation, and exception control explicit.
lowerProgram :: FcProgram -> GrinProgram
lowerProgram sourceProgram = lowerProgramWithEnvironment (programEnvironment [program]) program
  where
    program = lowerNewtypes sourceProgram

-- | Lower separately compiled FC units with the complete set of global names
-- supplied by the compilation closure. A unit may refer to a constructor,
-- primitive, foreign import, or CAF defined in another unit, but generated
-- functions and local variables remain private to that unit.
lowerPrograms :: [FcProgram] -> [GrinProgram]
lowerPrograms sourcePrograms = map (lowerProgramWithEnvironment environment) programs
  where
    programs = splitPrograms sourcePrograms (lowerNewtypes (concatPrograms sourcePrograms))
    environment = programEnvironment programs

concatPrograms :: [FcProgram] -> FcProgram
concatPrograms programs = FcProgram (concatMap fcTopBinds programs)

splitPrograms :: [FcProgram] -> FcProgram -> [FcProgram]
splitPrograms sourcePrograms (FcProgram topBinds) = go sourcePrograms topBinds
  where
    go [] _ = []
    go (FcProgram sourceTopBinds : rest) remaining =
      let (unitTopBinds, remaining') = splitAt (length sourceTopBinds) remaining
       in FcProgram unitTopBinds : go rest remaining'

data ProgramEnvironment = ProgramEnvironment
  { programEnvironmentGlobals :: !(Set Text),
    programEnvironmentWhnfGlobals :: !(Set Text),
    programEnvironmentPrimitives :: !(Map.Map Text PrimOp)
  }

programEnvironment :: [FcProgram] -> ProgramEnvironment
programEnvironment programs =
  ProgramEnvironment
    { programEnvironmentGlobals = Set.fromList (map fst builtinConstructors <> concatMap programGlobalNames programs),
      programEnvironmentWhnfGlobals = Set.fromList (map fst builtinConstructors <> concatMap programWhnfGlobalNames programs),
      programEnvironmentPrimitives =
        Map.fromList
          [ (varName var, primOp)
          | program <- programs,
            FcPrimitive var primOp <- fcTopBinds program
          ]
    }

lowerProgramWithEnvironment :: ProgramEnvironment -> FcProgram -> GrinProgram
lowerProgramWithEnvironment environment program =
  GrinProgram
    { grinConstructors = loweredConstructors tops,
      grinPrimitives = loweredPrimitives tops,
      grinForeignCalls = loweredForeignCalls tops,
      grinIoCafs = programIoCafs program,
      grinCafs = loweredCafs tops,
      grinFunctions = reverse (lowerFunctionsRev finalState)
    }
  where
    initialState =
      LowerState
        { lowerNextUnique = maximum (0 : map sourceUnique (programVars program)) + 1,
          lowerNextFunction = 0,
          lowerFunctionsRev = [],
          lowerPrimitiveOps = programEnvironmentPrimitives environment,
          lowerGlobalNames = programEnvironmentGlobals environment,
          lowerWhnfGlobalNames = programEnvironmentWhnfGlobals environment,
          lowerLocalVars = Set.empty
        }
    (topParts, finalState) = runState (mapM lowerTopBind (fcTopBinds program)) initialState
    tops = mconcat topParts

lowerTopBind :: FcTopBind -> LowerM LoweredTop
lowerTopBind topBind =
  case topBind of
    FcData _ _ constructors ->
      pure mempty {loweredConstructors = [(name, map typeRuntimeRep fields) | (name, fields) <- constructors]}
    FcNewtype {} ->
      pure mempty
    FcPrimitive var primOp ->
      pure mempty {loweredPrimitives = [(lowerGlobalVar var, primOp)]}
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
  primitiveOps <- gets lowerPrimitiveOps
  localVars <- gets lowerLocalVars
  case primitiveApplication primitiveOps localVars expr of
    Just (primOp, [exception])
      | primOpNameIs "raise#" primOp ->
          lowerStrict "exception" exception (pure . GrinThrow)
    Just (primOp, [action, handler, state]) | primOpNameIs "catch#" primOp ->
      lowerArgument action $ \actionValue ->
        lowerArgument handler $ \handlerValue ->
          lowerArgument state $ \stateValue ->
            pure (GrinCatch (exprRuntimeRep expr) actionValue handlerValue stateValue)
    Just (primOp, arguments)
      | length arguments == primOpArity primOp,
        Just schedulerOp <- schedulerPrimOp primOp ->
          lowerArgumentMany arguments $ \values ->
            pure (GrinScheduler (exprRuntimeRep expr) schedulerOp values)
    _ -> lowerOrdinaryExpr expr
  where
    primOpNameIs expected primOp = primOpName primOp == expected

lowerOrdinaryExpr :: FcExpr -> LowerM GrinExpr
lowerOrdinaryExpr expr =
  case expr of
    FcVar var ->
      do
        direct <- lowerDirectValue expr
        case direct of
          Just value -> pure (GrinReturn value)
          Nothing -> do
            isGlobal <- isGlobalVar var
            let resultRep = typeRuntimeRep (varType var)
                runtimeVar = if isGlobal then lowerGlobalVar var else lowerVar var
            pure (GrinEval resultRep (GrinVarValue runtimeVar))
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
      lowerArgumentMany fields $ \values ->
        pure (GrinReturn (GrinNodeValue (GrinNode GrinDictionary values)))
    FcDictSelect dictionary index ->
      lowerStrict "dictionary" dictionary $ \value ->
        pure (GrinDictSelect liftedRuntimeRep value index)
    FcLet bind body ->
      lowerLet bind body
    FcCase scrutinee binder alternatives ->
      lowerStrict "scrutinee" scrutinee $ \value -> do
        loweredAlternatives <- mapM (lowerAlt binder) alternatives
        pure (GrinCase value (lowerVar binder) loweredAlternatives)
    FcCast inner _ ->
      lowerExpr inner
    FcCallForeign foreignCall arguments ->
      lowerStrictMany arguments $ \values ->
        pure (GrinForeignCallExpr (lowerForeignCall foreignCall) values)

lowerApplication :: FcExpr -> FcExpr -> LowerM GrinExpr
lowerApplication function argument =
  lowerStrict "function" function $ \functionValue ->
    lowerArgument argument $ \argumentValue ->
      pure (GrinApply (applicationResultRep function) functionValue argumentValue)

lowerLambda :: Var -> FcExpr -> LowerM GrinExpr
lowerLambda binder body = do
  captures <- capturesFor (FcLam binder body)
  functionName <- freshFunction "closure"
  loweredBody <- withLocalVars [binder] (lowerExpr body)
  emitFunction
    GrinFunction
      { grinFunctionName = functionName,
        grinFunctionParameters = captures <> [lowerVar binder],
        grinFunctionResultRep = exprRuntimeRep body,
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
      loweredBody <- withLocalVars [var] (lowerExpr body)
      if typeRuntimeRep (varType var) == liftedRuntimeRep
        then do
          node <- makeThunk rhs
          pure (GrinBind (lowerVar var) (GrinStore node) loweredBody)
        else do
          loweredRhs <- lowerExpr rhs
          pure (GrinBind (lowerVar var) loweredRhs loweredBody)
    FcRec bindings -> do
      withLocalVars (map fst bindings) $ do
        nodes <- mapM lowerBinding bindings
        loweredBody <- lowerExpr body
        pure (GrinStoreRec nodes loweredBody)
  where
    lowerBinding (var, rhs) = do
      node <- makeThunk rhs
      pure (lowerVar var, node)

lowerAlt :: Var -> FcAlt -> LowerM GrinAlt
lowerAlt caseBinder alt = do
  rhs <- withLocalVars (caseBinder : altBinders alt) (lowerExpr (altRhs alt))
  pure
    GrinAlt
      { grinAltCon = lowerAltCon (altCon alt),
        grinAltBinders = map lowerVar (altBinders alt),
        grinAltRhs = rhs
      }

makeThunk :: FcExpr -> LowerM GrinNode
makeThunk expr = do
  captures <- capturesFor expr
  functionName <- freshFunction "thunk"
  body <- lowerExpr expr
  emitFunction
    GrinFunction
      { grinFunctionName = functionName,
        grinFunctionParameters = captures,
        grinFunctionResultRep = exprRuntimeRep expr,
        grinFunctionBody = body
      }
  pure (GrinNode (GrinThunk functionName) (map GrinVarValue captures))

lowerStrict :: Text -> FcExpr -> (GrinValue -> LowerM GrinExpr) -> LowerM GrinExpr
lowerStrict hint expr continuation = do
  direct <- lowerDirectValue expr
  case direct of
    Just value -> continuation value
    Nothing -> do
      valueVar <- freshVar hint (exprRuntimeRep expr)
      valueExpr <- lowerExpr expr
      rest <- continuation (GrinVarValue valueVar)
      pure (GrinBind valueVar valueExpr rest)

lowerDelayed :: FcExpr -> (GrinValue -> LowerM GrinExpr) -> LowerM GrinExpr
lowerDelayed expr continuation = do
  pointerVar <- freshVar "thunk" liftedRuntimeRep
  node <- makeThunk expr
  rest <- continuation (GrinVarValue pointerVar)
  pure (GrinBind pointerVar (GrinStore node) rest)

lowerArgument :: FcExpr -> (GrinValue -> LowerM GrinExpr) -> LowerM GrinExpr
lowerArgument expr continuation = do
  direct <- lowerDirectValue expr
  case direct of
    Just value -> continuation value
    Nothing
      | exprRuntimeRep expr == liftedRuntimeRep -> lowerDelayed expr continuation
      | otherwise -> lowerStrict "argument" expr continuation

lowerArgumentMany :: [FcExpr] -> ([GrinValue] -> LowerM GrinExpr) -> LowerM GrinExpr
lowerArgumentMany expressions continuation =
  case expressions of
    [] -> continuation []
    first : rest ->
      lowerArgument first $ \firstValue ->
        lowerArgumentMany rest $ \restValues ->
          continuation (firstValue : restValues)

lowerStrictMany :: [FcExpr] -> ([GrinValue] -> LowerM GrinExpr) -> LowerM GrinExpr
lowerStrictMany expressions continuation =
  case expressions of
    [] -> continuation []
    first : rest ->
      lowerStrict "foreign_argument" first $ \firstValue ->
        lowerStrictMany rest $ \restValues ->
          continuation (firstValue : restValues)

freshVar :: Text -> RuntimeRep -> LowerM GrinVar
freshVar hint runtimeRep = do
  unique <- gets lowerNextUnique
  modify' $ \state -> state {lowerNextUnique = unique + 1}
  pure (GrinVar ("$grin_" <> hint <> "_" <> T.pack (show unique)) unique runtimeRep)

freshTopVar :: Var -> LowerM GrinVar
freshTopVar var = do
  unique <- gets lowerNextUnique
  modify' $ \state -> state {lowerNextUnique = unique + 1}
  pure (GrinVar (varName var) unique liftedRuntimeRep)

freshFunction :: Text -> LowerM FunctionName
freshFunction kind = do
  index <- gets lowerNextFunction
  modify' $ \state -> state {lowerNextFunction = index + 1}
  pure (FunctionName ("$grin_" <> kind <> "_" <> T.pack (show index)))

emitFunction :: GrinFunction -> LowerM ()
emitFunction function =
  modify' $ \state -> state {lowerFunctionsRev = function : lowerFunctionsRev state}

primitiveApplication :: Map.Map Text PrimOp -> Set (Text, Unique) -> FcExpr -> Maybe (PrimOp, [FcExpr])
primitiveApplication primitiveOps localVars expr =
  case collectApplications expr of
    (FcVar var, arguments)
      | varKey var `Set.notMember` localVars,
        Just primOp <- Map.lookup (varName var) primitiveOps ->
          Just (primOp, arguments)
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
    FcCallForeign _ arguments -> foldMap freeVars arguments

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
lowerVar var = GrinVar (varName var) (sourceUnique var) (typeRuntimeRep (varType var))

lowerGlobalVar :: Var -> GrinVar
lowerGlobalVar var = GrinVar (varName var) (sourceUnique var) liftedRuntimeRep

capturesFor :: FcExpr -> LowerM [GrinVar]
capturesFor expr = do
  vars <- filterM (fmap not . isGlobalVar) (Set.toAscList (freeVars expr))
  pure (map lowerVar vars)

isGlobalVar :: Var -> LowerM Bool
isGlobalVar var = do
  localVars <- gets lowerLocalVars
  globalNames <- gets lowerGlobalNames
  pure (varKey var `Set.notMember` localVars && varName var `Set.member` globalNames)

-- | Values that are already in runtime normal form can be embedded directly
-- in the surrounding GRIN operation. Constructor, primitive, and foreign
-- globals are initialized as runtime nodes; unlike CAFs, they have no thunk to
-- enter. Unboxed locals and literals are direct machine values as well.
lowerDirectValue :: FcExpr -> LowerM (Maybe GrinValue)
lowerDirectValue expr =
  case expr of
    FcVar var -> do
      isGlobal <- isGlobalVar var
      isWhnfGlobal <- isWhnfGlobalVar var
      let runtimeRep = typeRuntimeRep (varType var)
      pure $
        if isWhnfGlobal
          then Just (GrinVarValue (lowerGlobalVar var))
          else
            if not isGlobal && runtimeRep /= liftedRuntimeRep
              then Just (GrinVarValue (lowerVar var))
              else Nothing
    FcLit literal -> pure (Just (GrinLitValue (lowerLiteral literal)))
    FcTyApp inner _ -> lowerDirectValue inner
    FcCast inner _ -> lowerDirectValue inner
    _ -> pure Nothing

isWhnfGlobalVar :: Var -> LowerM Bool
isWhnfGlobalVar var = do
  localVars <- gets lowerLocalVars
  whnfGlobalNames <- gets lowerWhnfGlobalNames
  pure (varKey var `Set.notMember` localVars && varName var `Set.member` whnfGlobalNames)

withLocalVars :: [Var] -> LowerM a -> LowerM a
withLocalVars vars action = do
  previous <- gets lowerLocalVars
  modify' $ \state -> state {lowerLocalVars = Set.fromList (map varKey vars) <> previous}
  result <- action
  modify' $ \state -> state {lowerLocalVars = previous}
  pure result

varKey :: Var -> (Text, Unique)
varKey var = (varName var, varUnique var)

sourceUnique :: Var -> Int
sourceUnique var =
  case varUnique var of
    Unique unique -> unique

lowerLiteral :: Literal -> GrinLiteral
lowerLiteral literal =
  case literal of
    LitInt runtimeRep value -> GrinLitInt runtimeRep value
    LitChar runtimeRep value -> GrinLitChar runtimeRep value
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
    { grinForeignCallName = fcForeignCallName foreignCall,
      grinForeignCallSymbol = fcForeignCallSymbol foreignCall,
      grinForeignCallSignature = lowerForeignSignature (fcForeignCallSignature foreignCall)
    }

lowerForeignSignature :: FcForeignSignature -> GrinForeignSignature
lowerForeignSignature signature =
  GrinForeignSignature
    { grinForeignArgumentTypes = map lowerForeignType (fcForeignArgumentTypes signature),
      grinForeignResultType = lowerForeignType (fcForeignResultType signature),
      grinForeignEffect = lowerForeignEffect (fcForeignEffect signature)
    }

lowerForeignEffect :: FcForeignEffect -> GrinForeignEffect
lowerForeignEffect effect =
  case effect of
    FcForeignPure -> GrinForeignPure
    FcForeignRealWorld -> GrinForeignRealWorld

lowerForeignType :: FcForeignType -> GrinForeignType
lowerForeignType foreignType =
  case foreignType of
    FcForeignInt32 -> GrinForeignInt32
    FcForeignWord64 -> GrinForeignWord64

exprRuntimeRep :: FcExpr -> RuntimeRep
exprRuntimeRep expr =
  case expr of
    FcLit literal -> literalRuntimeRep literal
    FcLam {} -> liftedRuntimeRep
    FcTyLam {} -> liftedRuntimeRep
    FcDictLam {} -> liftedRuntimeRep
    FcDict {} -> liftedRuntimeRep
    FcDictSelect {} -> liftedRuntimeRep
    _ ->
      case exprType expr of
        Just ty -> typeRuntimeRep ty
        Nothing -> error ("GRIN lowering could not determine expression type: " <> show expr)

exprType :: FcExpr -> Maybe TcType
exprType expr =
  case expr of
    FcVar var -> Just (varType var)
    FcLit literal -> literalType literal
    FcApp function _ -> functionResultType =<< exprType function
    FcDictApp function _ -> functionResultType =<< exprType function
    FcTyApp function argument -> do
      functionType <- exprType function
      case functionType of
        TcForAllTy tyVar body -> Just (substType (Map.singleton tyVar argument) body)
        _ -> Just functionType
    FcLam var body -> TcFunTy (varType var) <$> exprType body
    FcTyLam tyVar body -> TcForAllTy tyVar <$> exprType body
    FcDictLam var body -> TcFunTy (varType var) <$> exprType body
    FcDict {} -> Nothing
    FcDictSelect {} -> Nothing
    FcLet _ body -> exprType body
    FcCase _ _ alternatives ->
      case alternatives of
        first : _ -> exprType (altRhs first)
        [] -> Nothing
    FcCast inner _ -> exprType inner
    FcCallForeign foreignCall _arguments ->
      Just (fcForeignCallResultType (fcForeignCallSignature foreignCall))
  where
    functionResultType functionType =
      case functionType of
        TcFunTy _ result -> Just result
        TcQualTy [] body -> functionResultType body
        TcQualTy (_ : predicates) body -> Just (if null predicates then body else TcQualTy predicates body)
        _ -> Nothing

typeRuntimeRep :: TcType -> RuntimeRep
typeRuntimeRep ty =
  case runtimeRepOfType ty of
    Right runtimeRep -> runtimeRep
    Left problem -> error ("GRIN lowering received a non-runtime type: " <> problem)

applicationResultRep :: FcExpr -> RuntimeRep
applicationResultRep function =
  case exprType function >>= functionResultType of
    Just result -> typeRuntimeRep result
    Nothing -> error ("GRIN lowering could not determine application result type: " <> show function)
  where
    functionResultType functionType =
      case functionType of
        TcFunTy _ result -> Just result
        TcQualTy [] body -> functionResultType body
        TcQualTy (_ : predicates) body -> Just (if null predicates then body else TcQualTy predicates body)
        _ -> Nothing

programVars :: FcProgram -> [Var]
programVars program = concatMap topVars (fcTopBinds program)

programGlobalNames :: FcProgram -> [Text]
programGlobalNames program = concatMap topGlobalNames (fcTopBinds program)
  where
    topGlobalNames topBind =
      case topBind of
        FcData _ _ constructors -> map fst constructors
        FcNewtype {} -> []
        FcPrimitive var _ -> [varName var]
        FcForeignImport {} -> []
        FcTopBind bind -> map (varName . fst) (topBindings bind)
    topBindings bind =
      case bind of
        FcNonRec var expr -> [(var, expr)]
        FcRec bindings -> bindings

programWhnfGlobalNames :: FcProgram -> [Text]
programWhnfGlobalNames program = concatMap topWhnfGlobalNames (fcTopBinds program)
  where
    topWhnfGlobalNames topBind =
      case topBind of
        FcData _ _ constructors -> map fst constructors
        FcNewtype {} -> []
        FcPrimitive var _ -> [varName var]
        FcForeignImport {} -> []
        FcTopBind {} -> []

programIoCafs :: FcProgram -> Set Text
programIoCafs program =
  Set.fromList
    [ varName var
    | FcTopBind bind <- fcTopBinds program,
      var <- map fst (topBindings bind),
      isIOType (varType var)
    ]
  where
    topBindings bind =
      case bind of
        FcNonRec var rhs -> [(var, rhs)]
        FcRec bindings -> bindings

    isIOType ty =
      case ty of
        TcTyCon tyCon [_] | tyConName tyCon == "IO", tyConArity tyCon == 1 -> True
        TcForAllTy _ body -> isIOType body
        TcQualTy _ body -> isIOType body
        _ -> False

topVars :: FcTopBind -> [Var]
topVars topBind =
  case topBind of
    FcData {} -> []
    FcNewtype {} -> []
    FcPrimitive var _ -> [var]
    FcForeignImport {} -> []
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
    FcCallForeign _ arguments -> concatMap exprVars arguments

altVars :: FcAlt -> [Var]
altVars alt = grinAltBinders' <> exprVars (altRhs alt)
  where
    grinAltBinders' = altBinders alt
