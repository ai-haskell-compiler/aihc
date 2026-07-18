{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Lowering from non-strict System FC to strict, runtime-explicit GRIN.
module Aihc.Grin.Lower
  ( GrinInterface,
    extractGrinInterface,
    lowerProgram,
    lowerProgramWithInterface,
  )
where

import Aihc.Fc.Newtype (lowerNewtypes)
import Aihc.Fc.Subst (substType)
import Aihc.Fc.Syntax
import Aihc.Grin.Syntax
import Aihc.Tc.Types
  ( RuntimeRep (..),
    TcType (..),
    Unique (..),
    liftedRuntimeRep,
    runtimeRepOfType,
  )
import Control.Monad.Trans.State.Strict (State, gets, modify', runState)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

data LowerState = LowerState
  { lowerNextUnique :: !Int,
    lowerNextFunction :: !Int,
    lowerFunctionsRev :: ![GrinFunction],
    lowerConstructorArities :: !(Map Text Int),
    lowerPrimitiveArities :: !(Map Text Int),
    lowerCodeInfos :: !(Map Text GrinCodeInfo),
    lowerLocalCodeNames :: !(Set Text),
    lowerReferencedExternalCodeNames :: !(Set Text),
    lowerLocalGlobalNames :: !(Set Text),
    lowerReferencedExternalGlobalNames :: !(Set Text),
    lowerGlobalNames :: !(Set Text),
    lowerWhnfGlobalNames :: !(Set Text),
    lowerLocalVars :: !(Map (Text, Unique) [GrinVar])
  }

type LowerM = State LowerState

data LoweredTop = LoweredTop
  { loweredConstructors :: ![(Text, [RuntimeRep])],
    loweredPrimitives :: ![(GrinVar, Int)],
    loweredForeignCalls :: ![GrinForeignCall],
    loweredWhnfGlobals :: ![(GrinVar, GrinNode)],
    loweredCafs :: ![(GrinVar, GrinNode)]
  }

instance Semigroup LoweredTop where
  left <> right =
    LoweredTop
      { loweredConstructors = loweredConstructors left <> loweredConstructors right,
        loweredPrimitives = loweredPrimitives left <> loweredPrimitives right,
        loweredForeignCalls = loweredForeignCalls left <> loweredForeignCalls right,
        loweredWhnfGlobals = loweredWhnfGlobals left <> loweredWhnfGlobals right,
        loweredCafs = loweredCafs left <> loweredCafs right
      }

instance Monoid LoweredTop where
  mempty = LoweredTop [] [] [] [] []

-- | Erase FC types and coercions while retaining their runtime
-- representations, closure-convert lambdas and thunks, and make evaluation,
-- application, allocation, and exception control explicit.
lowerProgram :: FcProgram -> GrinProgram
lowerProgram sourceProgram = lowerProgramWithInterface mempty program
  where
    program = lowerNewtypes sourceProgram

-- | Runtime facts exported by one compiled unit. Lowering consumers need to
-- know which external names are globals, already in WHNF, constructors, or
-- primitives, but never need the defining FC expressions.
data GrinInterface = GrinInterface
  { grinInterfaceGlobals :: !(Set Text),
    grinInterfaceWhnfGlobals :: !(Set Text),
    grinInterfaceConstructorArities :: !(Map Text Int),
    grinInterfacePrimitiveArities :: !(Map Text Int),
    grinInterfaceCodeInfos :: !(Map Text GrinCodeInfo)
  }
  deriving (Eq, Show, Read)

instance Semigroup GrinInterface where
  left <> right =
    GrinInterface
      { grinInterfaceGlobals = grinInterfaceGlobals left <> grinInterfaceGlobals right,
        grinInterfaceWhnfGlobals = grinInterfaceWhnfGlobals left <> grinInterfaceWhnfGlobals right,
        grinInterfaceConstructorArities = grinInterfaceConstructorArities left <> grinInterfaceConstructorArities right,
        grinInterfacePrimitiveArities = grinInterfacePrimitiveArities left <> grinInterfacePrimitiveArities right,
        grinInterfaceCodeInfos = grinInterfaceCodeInfos left <> grinInterfaceCodeInfos right
      }

instance Monoid GrinInterface where
  mempty = GrinInterface Set.empty Set.empty Map.empty Map.empty Map.empty

extractGrinInterface :: FcProgram -> GrinInterface
extractGrinInterface program =
  GrinInterface
    { grinInterfaceGlobals = Set.fromList (programGlobalNames program),
      grinInterfaceWhnfGlobals = Set.fromList (programWhnfGlobalNames program),
      grinInterfaceConstructorArities = Map.fromList (programConstructors program),
      grinInterfacePrimitiveArities =
        Map.fromList
          [ (varName var, arity)
          | FcPrimitive var arity <- fcTopBinds program
          ],
      grinInterfaceCodeInfos = Map.fromList (programCodeInfos program)
    }

-- | Lower one SCC using only the exported runtime facts of predecessor SCCs.
-- The supplied program must already have completed its FC transformations.
lowerProgramWithInterface :: GrinInterface -> FcProgram -> GrinProgram
lowerProgramWithInterface imported program =
  lowerProgramWithEnvironment (programEnvironment (imported <> extractGrinInterface program)) program

data ProgramEnvironment = ProgramEnvironment
  { programEnvironmentGlobals :: !(Set Text),
    programEnvironmentWhnfGlobals :: !(Set Text),
    programEnvironmentConstructorArities :: !(Map Text Int),
    programEnvironmentPrimitiveArities :: !(Map Text Int),
    programEnvironmentCodeInfos :: !(Map Text GrinCodeInfo)
  }

programEnvironment :: GrinInterface -> ProgramEnvironment
programEnvironment interface =
  ProgramEnvironment
    { programEnvironmentGlobals = Set.fromList (map fst builtinConstructors) <> grinInterfaceGlobals interface,
      programEnvironmentWhnfGlobals = Set.fromList (map fst builtinConstructors) <> grinInterfaceWhnfGlobals interface,
      programEnvironmentConstructorArities = Map.fromList builtinConstructors <> grinInterfaceConstructorArities interface,
      programEnvironmentPrimitiveArities = grinInterfacePrimitiveArities interface,
      programEnvironmentCodeInfos = grinInterfaceCodeInfos interface
    }

lowerProgramWithEnvironment :: ProgramEnvironment -> FcProgram -> GrinProgram
lowerProgramWithEnvironment environment program =
  GrinProgram
    { grinConstructors = loweredConstructors tops,
      grinPrimitives = loweredPrimitives tops,
      grinForeignCalls = loweredForeignCalls tops,
      grinExternalGlobals = Set.toAscList (lowerReferencedExternalGlobalNames finalState),
      grinExternalFunctions = externalCodeInfos,
      grinWhnfGlobals = loweredWhnfGlobals tops,
      grinCafs = loweredCafs tops,
      grinFunctions = reverse (lowerFunctionsRev finalState)
    }
  where
    initialState =
      LowerState
        { lowerNextUnique = maximum (0 : map sourceUnique (programVars program)) + 1,
          lowerNextFunction = 0,
          lowerFunctionsRev = [],
          lowerConstructorArities = programEnvironmentConstructorArities environment,
          lowerPrimitiveArities = programEnvironmentPrimitiveArities environment,
          lowerCodeInfos = programEnvironmentCodeInfos environment,
          lowerLocalCodeNames = localCodeNames,
          lowerReferencedExternalCodeNames = Set.empty,
          lowerLocalGlobalNames = Set.fromList (programGlobalNames program),
          lowerReferencedExternalGlobalNames = Set.empty,
          lowerGlobalNames = programEnvironmentGlobals environment,
          lowerWhnfGlobalNames = programEnvironmentWhnfGlobals environment,
          lowerLocalVars = Map.empty
        }
    (topParts, finalState) = runState (mapM lowerTopBind (fcTopBinds program)) initialState
    tops = mconcat topParts
    localCodeNames = Map.keysSet (Map.fromList (programCodeInfos program))
    externalCodeInfos =
      [ info
      | (name, info) <- Map.toAscList (programEnvironmentCodeInfos environment),
        name `Set.member` lowerReferencedExternalCodeNames finalState
      ]

lowerTopBind :: FcTopBind -> LowerM LoweredTop
lowerTopBind topBind =
  case topBind of
    FcData _ _ constructors ->
      pure mempty {loweredConstructors = [(name, concatMap (runtimeRepComponents . typeRuntimeRep) fields) | (name, fields) <- constructors]}
    FcNewtype {} ->
      pure mempty
    FcPrimitive var arity ->
      pure mempty {loweredPrimitives = [(lowerGlobalVar var, arity)]}
    FcForeignImport foreignCall ->
      pure mempty {loweredForeignCalls = [lowerForeignCall foreignCall]}
    FcTopBind bind -> lowerTopValueBind bind

lowerTopValueBind :: FcBind -> LowerM LoweredTop
lowerTopValueBind bind =
  case bind of
    FcNonRec var expr -> lowerBinding var expr
    FcRec bindings ->
      mconcat <$> mapM (uncurry lowerBinding) bindings
  where
    lowerBinding var expr = do
      if isDirectFunction expr
        then do
          emitTopFunction var expr
          pure mempty
        else do
          staticNode <- lowerStaticNode expr
          topVar <- freshTopVar var
          case staticNode of
            Just node -> pure mempty {loweredWhnfGlobals = [(topVar, node)]}
            Nothing -> do
              node <- makeThunk expr
              pure mempty {loweredCafs = [(topVar, node)]}

-- | A top-level RHS is already a function value exactly when reaching its
-- first runtime construct requires only erasing type abstraction or casts.
-- Any term computation before the lambda must remain an updateable CAF so its
-- result is shared.
isDirectFunction :: FcExpr -> Bool
isDirectFunction expr =
  case expr of
    FcLam {} -> True
    FcTyLam _ body -> isDirectFunction body
    FcCast inner _ -> isDirectFunction inner
    _ -> False

emitTopFunction :: Var -> FcExpr -> LowerM ()
emitTopFunction var expr = do
  let (binders, body) = collectLeadingLambdas expr
      functionName = topFunctionName var
  (parameters, loweredBody) <- withFreshLocalVars binders $ \groups -> do
    body' <- lowerExpr body
    pure (concat groups, body')
  emitFunction
    GrinFunction
      { grinFunctionName = functionName,
        grinFunctionLinkName = Just (varName var),
        grinFunctionParameters = parameters,
        grinFunctionResultRep = exprRuntimeRep body,
        grinFunctionBody = loweredBody
      }

lowerExpr :: FcExpr -> LowerM GrinExpr
lowerExpr expr = do
  constructorArities <- gets lowerConstructorArities
  primitiveArities <- gets lowerPrimitiveArities
  localVars <- gets lowerLocalVars
  case constructorApplication constructorArities (Map.keysSet localVars) expr of
    Just (constructor, arguments) ->
      lowerArgumentMany arguments $ \values ->
        pure (GrinStore (GrinNode (GrinConstructor constructor) values))
    Nothing ->
      case primitiveApplication primitiveArities (Map.keysSet localVars) expr of
        Just ("raise#", [exception]) ->
          lowerSingleOperand "exception" exception (pure . GrinThrow)
        Just ("catch#", [action, handler, _state]) ->
          lowerSingleEvaluatedOperand "catch_action" action $ \actionValue ->
            lowerCatchHandler handler $ \handlerValue ->
              pure (GrinCatch (exprRuntimeRep expr) actionValue handlerValue [])
        Just (name, arguments) ->
          case Map.lookup name primitiveArities of
            Just arity -> lowerPrimitiveApplication expr name arity arguments
            Nothing -> error "GRIN lowering lost primitive arity"
        Nothing -> lowerOrdinaryExpr expr

lowerOrdinaryExpr :: FcExpr -> LowerM GrinExpr
lowerOrdinaryExpr expr =
  case unboxedTupleArguments expr of
    Just arguments ->
      lowerArgumentMany arguments (pure . GrinConstant)
    Nothing -> lowerNonTupleExpr expr

lowerNonTupleExpr :: FcExpr -> LowerM GrinExpr
lowerNonTupleExpr expr =
  case expr of
    FcVar var ->
      do
        codeInfo <- lookupCodeInfo var
        case codeInfo of
          Just info -> pure (GrinStore (knownFunctionNode info (codeRuntimeArity info) []))
          Nothing -> do
            direct <- lowerDirectValues expr
            case direct of
              Just values -> pure (GrinConstant values)
              Nothing -> do
                runtimeVar <- lookupRuntimeVar var
                let resultRep = typeRuntimeRep (varType var)
                pure (GrinEval resultRep (GrinVarValue runtimeVar))
    FcLit literal ->
      pure (GrinConstant [GrinLitValue (lowerLiteral literal)])
    FcApp {} ->
      lowerApplication expr
    FcTyApp inner _ ->
      lowerExpr inner
    FcLam var body ->
      lowerLambda var body
    FcTyLam _ body ->
      lowerExpr body
    FcLet bind body ->
      lowerLet bind body
    FcCase scrutinee binder alternatives ->
      lowerCase scrutinee binder alternatives
    FcCast inner _ ->
      lowerExpr inner
    FcCallForeign foreignCall arguments ->
      lowerStrictMany arguments $ \values ->
        pure (GrinForeignCallExpr (lowerForeignCall foreignCall) values)

lowerApplication :: FcExpr -> LowerM GrinExpr
lowerApplication expr =
  case collectApplications expr of
    (FcVar var, arguments) -> do
      codeInfo <- lookupCodeInfo var
      case codeInfo of
        Just info -> lowerKnownApplication expr info arguments
        Nothing -> lowerUnknownApplication expr
    _ -> lowerUnknownApplication expr

lowerKnownApplication :: FcExpr -> GrinCodeInfo -> [FcExpr] -> LowerM GrinExpr
lowerKnownApplication originalExpr info arguments = do
  let (entryArguments, extraArguments) = splitAt termArity arguments
  lowerArgumentMany entryArguments $ \values ->
    case extraArguments of
      [] ->
        case compare (length values) runtimeArity of
          LT -> pure (GrinStore (knownFunctionNode info (runtimeArity - length values) values))
          EQ
            | length entryArguments == termArity ->
                pure (GrinCall (exprRuntimeRep originalExpr) (grinCodeFunctionName info) values)
            | grinCodeResultRep info == exprRuntimeRep originalExpr ->
                pure (GrinCall (grinCodeResultRep info) (grinCodeFunctionName info) values)
            | otherwise ->
                pure (GrinStore (knownFunctionNode info 0 values))
          GT -> error "GRIN lowering supplied more runtime arguments than the known function accepts"
      _ -> do
        let saturatedExpr = dropLastTermApplications (length extraArguments) originalExpr
            saturatedRep = exprRuntimeRep saturatedExpr
        resultVars <- freshVars "call" saturatedRep
        case resultVars of
          [resultVar] -> do
            rest <- lowerRemainingApplications saturatedExpr (GrinVarValue resultVar) extraArguments
            pure (bindExpr resultVars (GrinCall saturatedRep (grinCodeFunctionName info) values) rest)
          _ -> error "GRIN lowering expected an overapplied function call to return one function value"
  where
    termArity = length (grinCodeParameterLayouts info)
    runtimeArity = codeRuntimeArity info

lowerPrimitiveApplication :: FcExpr -> Text -> Int -> [FcExpr] -> LowerM GrinExpr
lowerPrimitiveApplication originalExpr name arity arguments =
  case compare suppliedArity arity of
    LT ->
      lowerArgumentMany arguments $ \values ->
        pure (GrinStore (GrinNode (GrinPrimitive name (arity - suppliedArity)) values))
    EQ ->
      lowerArgumentMany arguments $ \values ->
        pure (GrinPrimitiveCall (exprRuntimeRep originalExpr) name values)
    GT -> do
      let (saturatedArguments, extraArguments) = splitAt arity arguments
          saturatedExpr = dropLastTermApplications (suppliedArity - arity) originalExpr
          saturatedRep = exprRuntimeRep saturatedExpr
      lowerArgumentMany saturatedArguments $ \values -> do
        resultVars <- freshVars "primitive" saturatedRep
        case resultVars of
          [resultVar] -> do
            rest <- lowerRemainingApplications saturatedExpr (GrinVarValue resultVar) extraArguments
            pure (bindExpr resultVars (GrinPrimitiveCall saturatedRep name values) rest)
          _ -> error "GRIN lowering expected an overapplied primitive to return one function value"
  where
    suppliedArity = length arguments

dropLastTermApplications :: Int -> FcExpr -> FcExpr
dropLastTermApplications count expr
  | count <= 0 = expr
  | otherwise =
      case expr of
        FcApp function _ -> dropLastTermApplications (count - 1) function
        FcTyApp inner ty -> FcTyApp (dropLastTermApplications count inner) ty
        FcCast inner coercion -> FcCast (dropLastTermApplications count inner) coercion
        _ -> error "GRIN lowering could not split an overapplication"

lowerRemainingApplications :: FcExpr -> GrinValue -> [FcExpr] -> LowerM GrinExpr
lowerRemainingApplications functionExpr functionValue arguments =
  case arguments of
    [] -> pure (GrinConstant [functionValue])
    argument : rest ->
      evaluateGrinValue "function" (exprRuntimeRep functionExpr) functionValue $ \evaluatedFunction -> do
        let appliedExpr = FcApp functionExpr argument
            resultRep = exprRuntimeRep appliedExpr
        lowerArgument argument $ \argumentValues ->
          if null rest
            then pure (GrinApply resultRep evaluatedFunction argumentValues)
            else do
              resultVars <- freshVars "function" resultRep
              case resultVars of
                [resultVar] -> do
                  body <- lowerRemainingApplications appliedExpr (GrinVarValue resultVar) rest
                  pure (bindExpr resultVars (GrinApply resultRep evaluatedFunction argumentValues) body)
                _ -> error "GRIN lowering expected an intermediate application to return one function value"

lowerUnknownApplication :: FcExpr -> LowerM GrinExpr
lowerUnknownApplication expr =
  case expr of
    FcApp function argument ->
      lowerSingleEvaluatedOperand "function" function $ \functionValue ->
        lowerArgument argument $ \argumentValues ->
          pure (GrinApply (applicationResultRep function) functionValue argumentValues)
    _ -> error "GRIN lowering expected an application"

knownFunctionNode :: GrinCodeInfo -> Int -> [GrinValue] -> GrinNode
knownFunctionNode info remainingRuntimeArity =
  GrinNode
    (GrinClosure (grinCodeFunctionName info) remainingRuntimeArity)

codeRuntimeArity :: GrinCodeInfo -> Int
codeRuntimeArity = length . concat . grinCodeParameterLayouts

runtimeArityAfterTerms :: GrinCodeInfo -> Int -> Int
runtimeArityAfterTerms info suppliedTermArity =
  length (concat (drop suppliedTermArity (grinCodeParameterLayouts info)))

lowerCase :: FcExpr -> Var -> [FcAlt] -> LowerM GrinExpr
lowerCase scrutinee binder alternatives =
  case (runtimeRepComponents scrutineeRep, scrutineeRep, alternatives) of
    ([], _, [alternative]) -> do
      rhs <-
        withBindings
          ((binder, []) : [(fieldBinder, []) | fieldBinder <- altBinders alternative])
          (lowerExpr (altRhs alternative))
      scrutineeExpr <- lowerExpr scrutinee
      pure (bindExpr [] scrutineeExpr rhs)
    (_, TupleRep _, [alternative])
      | DataAlt constructor <- altCon alternative,
        isUnboxedTupleConstructor constructor ->
          lowerUnboxedTupleCase scrutinee binder alternative
    _ ->
      lowerSingleEvaluatedOperand "scrutinee" scrutinee $ \value -> do
        caseVars <- binderVars binder
        caseVar <-
          case caseVars of
            [var] -> pure var
            _ -> error "GRIN lowering expected one ordinary case binder"
        loweredAlternatives <- mapM (lowerAlt (binder, [caseVar])) alternatives
        pure (GrinCase value caseVar loweredAlternatives)
  where
    scrutineeRep = exprRuntimeRep scrutinee

lowerUnboxedTupleCase :: FcExpr -> Var -> FcAlt -> LowerM GrinExpr
lowerUnboxedTupleCase scrutinee binder alternative = do
  resultVars <- freshVars "tuple" (exprRuntimeRep scrutinee)
  let fieldBinders = altBinders alternative
      fieldWidths = map (length . runtimeRepComponents . typeRuntimeRep . varType) fieldBinders
      fieldGroups = splitWidths fieldWidths resultVars
  if sum fieldWidths /= length resultVars
    then error "GRIN lowering found inconsistent unboxed-tuple case fields"
    else do
      rhs <-
        withBindings
          ((binder, resultVars) : zip fieldBinders fieldGroups)
          (lowerExpr (altRhs alternative))
      scrutineeExpr <- lowerExpr scrutinee
      pure (bindExpr resultVars scrutineeExpr rhs)

splitWidths :: [Int] -> [value] -> [[value]]
splitWidths widths values =
  case widths of
    [] -> []
    width : rest ->
      let (field, remaining) = splitAt width values
       in field : splitWidths rest remaining

-- | Sequence an expression only when its results are used by the body.
-- Forwarding every bound result unchanged is exactly the value expression.
bindExpr :: [GrinVar] -> GrinExpr -> GrinExpr -> GrinExpr
bindExpr vars valueExpr body =
  case body of
    GrinConstant values
      | values == map GrinVarValue vars -> valueExpr
    _ -> GrinBind vars valueExpr body

lowerLambda :: Var -> FcExpr -> LowerM GrinExpr
lowerLambda binder body =
  GrinStore <$> makeClosureNode (FcLam binder body)

makeClosureNode :: FcExpr -> LowerM GrinNode
makeClosureNode expr = do
  let (binders, lambdaBody) = collectLeadingLambdas expr
  captures <- capturesFor expr
  functionName <- freshFunction "closure"
  (parameters, loweredBody) <- withFreshLocalVars binders $ \groups -> do
    body' <- lowerExpr lambdaBody
    pure (concat groups, body')
  emitFunction
    GrinFunction
      { grinFunctionName = functionName,
        grinFunctionLinkName = Nothing,
        grinFunctionParameters = captures <> parameters,
        grinFunctionResultRep = exprRuntimeRep lambdaBody,
        grinFunctionBody = loweredBody
      }
  pure (GrinNode (GrinClosure functionName (length parameters)) (map GrinVarValue captures))

lowerLet :: FcBind -> FcExpr -> LowerM GrinExpr
lowerLet bind body =
  case bind of
    FcNonRec var rhs
      | typeRuntimeRep (varType var) == liftedRuntimeRep -> do
          alias <- lookupAliasVars rhs
          case alias of
            Just values -> withBindings [(var, values)] (lowerExpr body)
            Nothing -> do
              rhsIsWhnf <- isWhnfExpr rhs
              (vars, loweredBody) <- withFreshLocalVars [var] $ \groups -> do
                body' <- lowerExpr body
                pure (concat groups, body')
              if rhsIsWhnf
                then do
                  loweredRhs <- lowerExpr rhs
                  pure (bindExpr vars loweredRhs loweredBody)
                else do
                  node <- makeThunk rhs
                  pure (bindExpr vars (GrinStore node) loweredBody)
      | otherwise -> do
          (vars, loweredBody) <- withFreshLocalVars [var] $ \groups -> do
            body' <- lowerExpr body
            pure (concat groups, body')
          loweredRhs <- lowerExpr rhs
          pure (bindExpr vars loweredRhs loweredBody)
    FcRec bindings -> do
      withFreshLocalVars (map fst bindings) $ \groups -> do
        nodes <-
          mapM
            ( \(_, rhs) ->
                if isDirectFunction rhs
                  then makeClosureNode rhs
                  else makeThunk rhs
            )
            bindings
        loweredBody <- lowerExpr body
        let vars = concat groups
        if length vars == length nodes
          then pure (GrinStoreRec (zip vars nodes) loweredBody)
          else error "GRIN lowering expected lifted recursive bindings"

lowerAlt :: (Var, [GrinVar]) -> FcAlt -> LowerM GrinAlt
lowerAlt caseBinding alt = do
  (binders, rhs) <- withBindings [caseBinding] $ withFreshLocalVars (altBinders alt) $ \groups -> do
    rhs' <- lowerExpr (altRhs alt)
    pure (concat groups, rhs')
  pure
    GrinAlt
      { grinAltCon = lowerAltCon (altCon alt),
        grinAltBinders = binders,
        grinAltRhs = rhs
      }

makeThunk :: FcExpr -> LowerM GrinNode
makeThunk expr
  | not (isLiftedRuntimeRep runtimeRep) =
      error ("GRIN lowering cannot suspend an expression with runtime representation " <> show runtimeRep)
  | otherwise = do
      captures <- capturesFor expr
      functionName <- freshFunction "thunk"
      body <- lowerExpr expr
      emitFunction
        GrinFunction
          { grinFunctionName = functionName,
            grinFunctionLinkName = Nothing,
            grinFunctionParameters = captures,
            grinFunctionResultRep = runtimeRep,
            grinFunctionBody = body
          }
      pure (GrinNode (GrinThunk functionName) (map GrinVarValue captures))
  where
    runtimeRep = exprRuntimeRep expr

lowerStaticNode :: FcExpr -> LowerM (Maybe GrinNode)
lowerStaticNode expr = do
  constructorArities <- gets lowerConstructorArities
  localVars <- gets lowerLocalVars
  case constructorApplication constructorArities (Map.keysSet localVars) expr of
    Just (constructor, arguments) -> do
      values <- mapM lowerStaticValues arguments
      pure (GrinNode (GrinConstructor constructor) . concat <$> sequence values)
    Nothing -> pure Nothing

lowerStaticValues :: FcExpr -> LowerM (Maybe [GrinValue])
lowerStaticValues expr =
  case expr of
    FcLit literal -> pure (Just [GrinLitValue (lowerLiteral literal)])
    FcVar var
      | null (runtimeRepComponents (typeRuntimeRep (varType var))) -> pure (Just [])
      | otherwise -> do
          constructorArity <- lookupConstructorArity var
          isWhnfGlobal <- isWhnfGlobalVar var
          localGlobalNames <- gets lowerLocalGlobalNames
          case constructorArity of
            Just 0 -> do
              noteExternalGlobalReference var
              pure (Just [GrinVarValue (lowerGlobalVar var)])
            _
              | isWhnfGlobal,
                varName var `Set.notMember` localGlobalNames -> do
                  noteExternalGlobalReference var
                  pure (Just [GrinVarValue (lowerGlobalVar var)])
              | otherwise -> pure Nothing
    FcTyApp inner _ -> lowerStaticValues inner
    FcCast inner _ -> lowerStaticValues inner
    _ -> pure Nothing

lowerStrict :: Text -> FcExpr -> ([GrinValue] -> LowerM GrinExpr) -> LowerM GrinExpr
lowerStrict hint expr continuation = do
  direct <- lowerDirectValues expr
  case direct of
    Just values -> continuation values
    Nothing -> do
      valueVars <- freshVars hint (exprRuntimeRep expr)
      valueExpr <- lowerExpr expr
      rest <- continuation (map GrinVarValue valueVars)
      pure (bindExpr valueVars valueExpr rest)

-- | Lower an operand for an operation that performs its own forcing. Variables
-- are passed as their existing lazy pointers; non-atomic computations are
-- evaluated only far enough to produce the operand supplied to the operation.
lowerOperand :: Text -> FcExpr -> ([GrinValue] -> LowerM GrinExpr) -> LowerM GrinExpr
lowerOperand hint expr continuation = do
  direct <- lowerLazyDirectValues expr
  case direct of
    Just values -> continuation values
    Nothing -> do
      valueVars <- freshVars hint (exprRuntimeRep expr)
      valueExpr <- lowerExpr expr
      rest <- continuation (map GrinVarValue valueVars)
      pure (bindExpr valueVars valueExpr rest)

lowerSingleOperand :: Text -> FcExpr -> (GrinValue -> LowerM GrinExpr) -> LowerM GrinExpr
lowerSingleOperand hint expr continuation =
  lowerOperand hint expr $ \case
    [value] -> continuation value
    _ -> error ("GRIN lowering expected one operand for " <> T.unpack hint)

-- | Produce the weak-head normal form required by an operation whose operand
-- is structural. GRIN operations never enter heap cells implicitly: a lifted
-- function or case scrutinee must pass through 'GrinEval' before use.
lowerSingleEvaluatedOperand :: Text -> FcExpr -> (GrinValue -> LowerM GrinExpr) -> LowerM GrinExpr
lowerSingleEvaluatedOperand hint expr continuation =
  lowerSingleOperand hint expr $ \value ->
    evaluateGrinValue hint (exprRuntimeRep expr) value continuation

evaluateGrinValue :: Text -> RuntimeRep -> GrinValue -> (GrinValue -> LowerM GrinExpr) -> LowerM GrinExpr
evaluateGrinValue hint runtimeRep value continuation
  | isLiftedRuntimeRep runtimeRep = do
      evaluated <- freshVar (hint <> "_whnf") runtimeRep
      rest <- continuation (GrinVarValue evaluated)
      pure (bindExpr [evaluated] (GrinEval runtimeRep value) rest)
  | otherwise = continuation value

-- | Keep a catch handler lazy until an exception is raised while still making
-- every function entry explicit. The wrapper is already in WHNF; when called,
-- it evaluates the captured handler and the action returned by that handler
-- before either value is applied.
lowerCatchHandler :: FcExpr -> (GrinValue -> LowerM GrinExpr) -> LowerM GrinExpr
lowerCatchHandler handler continuation =
  lowerSingleArgument handler $ \handlerValue -> do
    let handlerRep = exprRuntimeRep handler
        actionRep = applicationResultRep handler
        exceptionRep = functionArgumentRep handler
    capturedHandler <- freshVar "catch_handler" handlerRep
    exception <- freshVar "catch_exception" exceptionRep
    evaluatedHandler <- freshVar "catch_handler_whnf" handlerRep
    actionPointer <- freshVar "catch_handler_action" actionRep
    wrapperName <- freshFunction "catch_handler"
    emitFunction
      GrinFunction
        { grinFunctionName = wrapperName,
          grinFunctionLinkName = Nothing,
          grinFunctionParameters = [capturedHandler, exception],
          grinFunctionResultRep = actionRep,
          grinFunctionBody =
            bindExpr [evaluatedHandler] (GrinEval handlerRep (GrinVarValue capturedHandler)) $
              bindExpr
                [actionPointer]
                (GrinApply actionRep (GrinVarValue evaluatedHandler) [GrinVarValue exception])
                (GrinEval actionRep (GrinVarValue actionPointer))
        }
    wrapperPointer <- freshVar "catch_handler_wrapper" liftedRuntimeRep
    evaluatedWrapper <- freshVar "catch_handler_wrapper_whnf" liftedRuntimeRep
    rest <- continuation (GrinVarValue evaluatedWrapper)
    pure $
      bindExpr
        [wrapperPointer]
        (GrinStore (GrinNode (GrinClosure wrapperName 1) [handlerValue]))
        (bindExpr [evaluatedWrapper] (GrinEval liftedRuntimeRep (GrinVarValue wrapperPointer)) rest)

lowerDelayed :: FcExpr -> ([GrinValue] -> LowerM GrinExpr) -> LowerM GrinExpr
lowerDelayed expr continuation = do
  knownCall <- knownSaturatedApplication expr
  case knownCall of
    Just (info, arguments) ->
      lowerArgumentMany arguments $ \values ->
        storeDelayedNode (GrinNode (GrinThunk (grinCodeFunctionName info)) values)
    Nothing -> makeThunk expr >>= storeDelayedNode
  where
    storeDelayedNode node = do
      pointerVar <- freshVar "thunk" liftedRuntimeRep
      rest <- continuation [GrinVarValue pointerVar]
      pure (bindExpr [pointerVar] (GrinStore node) rest)

lowerArgument :: FcExpr -> ([GrinValue] -> LowerM GrinExpr) -> LowerM GrinExpr
lowerArgument expr continuation = do
  direct <- lowerLazyDirectValues expr
  case direct of
    Just values -> continuation values
    Nothing -> do
      whnf <- isWhnfExpr expr
      if whnf
        then lowerOperand "argument" expr continuation
        else
          if exprRuntimeRep expr == liftedRuntimeRep
            then lowerDelayed expr continuation
            else lowerStrict "argument" expr continuation

lowerSingleArgument :: FcExpr -> (GrinValue -> LowerM GrinExpr) -> LowerM GrinExpr
lowerSingleArgument expr continuation =
  lowerArgument expr $ \case
    [value] -> continuation value
    _ -> error "GRIN lowering expected one argument value"

lowerArgumentMany :: [FcExpr] -> ([GrinValue] -> LowerM GrinExpr) -> LowerM GrinExpr
lowerArgumentMany expressions continuation =
  case expressions of
    [] -> continuation []
    first : rest ->
      lowerArgument first $ \firstValues ->
        lowerArgumentMany rest $ \restValues ->
          continuation (firstValues <> restValues)

lowerStrictMany :: [FcExpr] -> ([GrinValue] -> LowerM GrinExpr) -> LowerM GrinExpr
lowerStrictMany expressions continuation =
  case expressions of
    [] -> continuation []
    first : rest ->
      lowerStrict "foreign_argument" first $ \firstValues ->
        lowerStrictMany rest $ \restValues ->
          continuation (firstValues <> restValues)

freshVars :: Text -> RuntimeRep -> LowerM [GrinVar]
freshVars hint = mapM (freshVar hint) . runtimeRepComponents

unboxedTupleArguments :: FcExpr -> Maybe [FcExpr]
unboxedTupleArguments expr =
  case collectApplications expr of
    (FcVar constructor, arguments)
      | isUnboxedTupleConstructor (varName constructor) ->
          case exprRuntimeRep expr of
            TupleRep fieldReps
              | length arguments == length fieldReps -> Just arguments
            _ -> Nothing
    _ -> Nothing

isUnboxedTupleConstructor :: Text -> Bool
isUnboxedTupleConstructor name =
  case T.stripPrefix "(#" name >>= T.stripSuffix "#)" of
    Just punctuation -> T.all (== ',') punctuation
    Nothing -> False

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
emitFunction function = do
  body <- ensureWhnfResult function (grinFunctionBody function)
  modify' $ \state -> state {lowerFunctionsRev = function {grinFunctionBody = body} : lowerFunctionsRev state}

-- A zero-runtime-argument closure whose entry preserves the result layout is
-- a saturated computation, not a WHNF. Enter it at function exits while
-- retaining closures in intermediate positions where they encode delayed
-- source-level application. If entry changes the layout, the closure remains
-- a genuine function value despite having no physical arguments.
ensureWhnfResult :: GrinFunction -> GrinExpr -> LowerM GrinExpr
ensureWhnfResult owner expr =
  case expr of
    GrinBind vars valueExpr body -> GrinBind vars valueExpr <$> ensureWhnfResult owner body
    GrinStore node@(GrinNode (GrinClosure functionName 0) fields) -> do
      resultRep <- lookupFunctionResultRep owner functionName
      if resultRep == grinFunctionResultRep owner
        then pure (GrinCall resultRep functionName fields)
        else pure (GrinStore node)
    GrinStore {} -> pure expr
    GrinStoreRec bindings body -> GrinStoreRec bindings <$> ensureWhnfResult owner body
    GrinCase scrutinee binder alternatives ->
      GrinCase scrutinee binder <$> mapM lowerResultAlt alternatives
    _ -> pure expr
  where
    lowerResultAlt alternative = do
      rhs <- ensureWhnfResult owner (grinAltRhs alternative)
      pure alternative {grinAltRhs = rhs}

lookupFunctionResultRep :: GrinFunction -> FunctionName -> LowerM RuntimeRep
lookupFunctionResultRep owner functionName
  | functionName == grinFunctionName owner = pure (grinFunctionResultRep owner)
  | otherwise = do
      localFunctions <- gets lowerFunctionsRev
      codeInfos <- gets lowerCodeInfos
      case [grinFunctionResultRep function | function <- localFunctions, grinFunctionName function == functionName]
        <> [grinCodeResultRep info | info <- Map.elems codeInfos, grinCodeFunctionName info == functionName] of
        resultRep : _ -> pure resultRep
        [] -> error ("GRIN lowering could not find saturated closure target " <> show functionName)

primitiveApplication :: Map Text Int -> Set (Text, Unique) -> FcExpr -> Maybe (Text, [FcExpr])
primitiveApplication primitiveArities localVars expr =
  case collectApplications expr of
    (FcVar var, arguments)
      | varKey var `Set.notMember` localVars,
        varName var `Map.member` primitiveArities ->
          Just (varName var, arguments)
    _ -> Nothing

constructorApplication :: Map Text Int -> Set (Text, Unique) -> FcExpr -> Maybe (Text, [FcExpr])
constructorApplication constructorArities localVars expr =
  case collectApplications expr of
    (FcVar var, arguments)
      | varKey var `Set.notMember` localVars,
        Just arity <- Map.lookup (varName var) constructorArities,
        arity > 0,
        length arguments <= arity ->
          Just (varName var, arguments)
    _ -> Nothing

collectApplications :: FcExpr -> (FcExpr, [FcExpr])
collectApplications expr =
  case expr of
    FcApp function argument ->
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
    FcTyApp inner _ -> freeVars inner
    FcLam var body -> Set.delete var (freeVars body)
    FcTyLam _ body -> freeVars body
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

lowerGlobalVar :: Var -> GrinVar
lowerGlobalVar var = GrinVar (varName var) (sourceUnique var) liftedRuntimeRep

capturesFor :: FcExpr -> LowerM [GrinVar]
capturesFor expr =
  fmap concat . mapM captureVars $ Set.toAscList (freeVars expr)
  where
    captureVars var = do
      codeInfo <- lookupCodeInfo var
      case codeInfo of
        Just _ -> pure []
        Nothing -> do
          primitiveArity <- lookupPrimitiveArity var
          case primitiveArity of
            Just _ -> pure []
            Nothing -> do
              global <- isGlobalVar var
              if global then pure [] else lookupLocalVars var

isGlobalVar :: Var -> LowerM Bool
isGlobalVar var = do
  localVars <- gets lowerLocalVars
  globalNames <- gets lowerGlobalNames
  pure
    ( varKey var `Map.notMember` localVars
        && (varName var `Set.member` globalNames || isUnboxedTupleConstructor (varName var))
    )

-- | Values that can be embedded directly in a non-allocating GRIN operation.
-- Dynamic constructors, closures, and primitives are deliberately excluded:
-- they must be introduced by 'GrinStore'.
lowerDirectValues :: FcExpr -> LowerM (Maybe [GrinValue])
lowerDirectValues expr =
  case expr of
    FcVar var -> do
      isGlobal <- isGlobalVar var
      isWhnfGlobal <- isWhnfGlobalVar var
      constructorArity <- lookupConstructorArity var
      primitiveArity <- lookupPrimitiveArity var
      let runtimeRep = typeRuntimeRep (varType var)
      case (constructorArity, primitiveArity) of
        _ | null (runtimeRepComponents runtimeRep) -> pure (Just [])
        (Just arity, _)
          | arity > 0 -> pure Nothing
        (_, Just arity)
          | arity > 0 -> pure Nothing
        _
          | isWhnfGlobal -> do
              noteExternalGlobalReference var
              pure (Just [GrinVarValue (lowerGlobalVar var)])
          | not isGlobal && runtimeRep /= liftedRuntimeRep ->
              Just . map GrinVarValue <$> lookupLocalVars var
          | otherwise -> pure Nothing
    FcLit literal -> pure (Just [GrinLitValue (lowerLiteral literal)])
    FcTyApp inner _ -> lowerDirectValues inner
    FcCast inner _ -> lowerDirectValues inner
    _ -> pure Nothing

-- | Values suitable for a non-strict position. Unlike 'lowerDirectValues', a
-- lifted variable is returned as its existing pointer without being forced or
-- wrapped in another thunk.
lowerLazyDirectValues :: FcExpr -> LowerM (Maybe [GrinValue])
lowerLazyDirectValues expr =
  case expr of
    FcVar var -> do
      codeInfo <- lookupCodeInfo var
      case codeInfo of
        Just _ -> pure Nothing
        Nothing -> do
          constructorArity <- lookupConstructorArity var
          primitiveArity <- lookupPrimitiveArity var
          case (constructorArity, primitiveArity) of
            (Just arity, _)
              | arity > 0 -> pure Nothing
            (_, Just arity)
              | arity > 0 -> pure Nothing
            _ -> Just . map GrinVarValue <$> lookupRuntimeVars var
    FcLit literal -> pure (Just [GrinLitValue (lowerLiteral literal)])
    FcTyApp inner _ -> lowerLazyDirectValues inner
    FcCast inner _ -> lowerLazyDirectValues inner
    _ -> pure Nothing

lookupRuntimeVars :: Var -> LowerM [GrinVar]
lookupRuntimeVars var = do
  let runtimeReps = runtimeRepComponents (typeRuntimeRep (varType var))
  if null runtimeReps
    then pure []
    else do
      global <- isGlobalVar var
      if global
        then case runtimeReps of
          [_] -> do
            noteExternalGlobalReference var
            pure [lowerGlobalVar var]
          _ -> error "GRIN lowering found a global with a multi-value runtime representation"
        else lookupLocalVars var

lookupRuntimeVar :: Var -> LowerM GrinVar
lookupRuntimeVar var =
  lookupRuntimeVars var >>= \case
    [runtimeVar] -> pure runtimeVar
    _ -> error "GRIN lowering expected one lifted runtime variable"

noteExternalGlobalReference :: Var -> LowerM ()
noteExternalGlobalReference var = do
  localGlobalNames <- gets lowerLocalGlobalNames
  if varName var `Set.member` localGlobalNames
    then pure ()
    else modify' $ \state ->
      state
        { lowerReferencedExternalGlobalNames =
            Set.insert (varName var) (lowerReferencedExternalGlobalNames state)
        }

lookupAliasVars :: FcExpr -> LowerM (Maybe [GrinVar])
lookupAliasVars expr =
  case expr of
    FcVar var -> do
      codeInfo <- lookupCodeInfo var
      case codeInfo of
        Just _ -> pure Nothing
        Nothing -> Just <$> lookupRuntimeVars var
    FcTyApp inner _ -> lookupAliasVars inner
    FcCast inner _ -> lookupAliasVars inner
    _ -> pure Nothing

lookupCodeInfo :: Var -> LowerM (Maybe GrinCodeInfo)
lookupCodeInfo var = do
  locals <- gets lowerLocalVars
  codeInfos <- gets lowerCodeInfos
  localCodeNames <- gets lowerLocalCodeNames
  if varKey var `Map.member` locals
    then pure Nothing
    else case Map.lookup (varName var) codeInfos of
      Nothing -> pure Nothing
      Just info -> do
        if varName var `Set.member` localCodeNames
          then pure ()
          else modify' $ \state ->
            state
              { lowerReferencedExternalCodeNames =
                  Set.insert (varName var) (lowerReferencedExternalCodeNames state)
              }
        pure (Just info)

lookupConstructorArity :: Var -> LowerM (Maybe Int)
lookupConstructorArity var = do
  locals <- gets lowerLocalVars
  constructorArities <- gets lowerConstructorArities
  pure
    ( if varKey var `Map.member` locals
        then Nothing
        else Map.lookup (varName var) constructorArities
    )

lookupPrimitiveArity :: Var -> LowerM (Maybe Int)
lookupPrimitiveArity var = do
  locals <- gets lowerLocalVars
  primitiveArities <- gets lowerPrimitiveArities
  pure
    ( if varKey var `Map.member` locals
        then Nothing
        else Map.lookup (varName var) primitiveArities
    )

knownSaturatedApplication :: FcExpr -> LowerM (Maybe (GrinCodeInfo, [FcExpr]))
knownSaturatedApplication expr =
  case collectApplications expr of
    (FcVar var, arguments) -> do
      codeInfo <- lookupCodeInfo var
      pure $ do
        info <- codeInfo
        if length arguments <= length (grinCodeParameterLayouts info)
          && runtimeArityAfterTerms info (length arguments) == 0
          && isLiftedRuntimeRep (grinCodeResultRep info)
          then Just (info, arguments)
          else Nothing
    _ -> pure Nothing

isWhnfExpr :: FcExpr -> LowerM Bool
isWhnfExpr expr =
  case expr of
    FcLam {} -> pure True
    FcTyLam _ body -> isWhnfExpr body
    FcCast inner _ -> isWhnfExpr inner
    FcLit literal -> pure (isLiftedRuntimeRep (literalRuntimeRep literal))
    FcVar var -> do
      codeInfo <- lookupCodeInfo var
      primitiveArity <- lookupPrimitiveArity var
      constructorArity <- lookupConstructorArity var
      pure
        ( case codeInfo of
            Just info ->
              runtimeArityAfterTerms info 0 > 0
                || grinCodeResultRep info /= exprRuntimeRep expr
            Nothing -> maybe False (> 0) primitiveArity || maybe False (> 0) constructorArity
        )
    FcApp {} -> do
      constructorArities <- gets lowerConstructorArities
      localVars <- gets lowerLocalVars
      case constructorApplication constructorArities (Map.keysSet localVars) expr of
        Just _ -> pure True
        Nothing ->
          case collectApplications expr of
            (FcVar var, arguments) -> do
              codeInfo <- lookupCodeInfo var
              pure $ case codeInfo of
                Just info ->
                  runtimeArityAfterTerms info (length arguments) > 0
                    || grinCodeResultRep info /= exprRuntimeRep expr
                Nothing -> False
            _ -> pure False
    _ -> pure False

isWhnfGlobalVar :: Var -> LowerM Bool
isWhnfGlobalVar var = do
  localVars <- gets lowerLocalVars
  whnfGlobalNames <- gets lowerWhnfGlobalNames
  pure (varKey var `Map.notMember` localVars && varName var `Set.member` whnfGlobalNames)

withFreshLocalVars :: [Var] -> ([[GrinVar]] -> LowerM a) -> LowerM a
withFreshLocalVars vars action = do
  groups <- mapM binderVars vars
  withBindings (zip vars groups) (action groups)

withBindings :: [(Var, [GrinVar])] -> LowerM a -> LowerM a
withBindings bindings action = do
  previous <- gets lowerLocalVars
  let locals = Map.fromList [(varKey var, values) | (var, values) <- bindings]
  modify' $ \state -> state {lowerLocalVars = locals <> previous}
  result <- action
  modify' $ \state -> state {lowerLocalVars = previous}
  pure result

binderVars :: Var -> LowerM [GrinVar]
binderVars var =
  case runtimeRepComponents (typeRuntimeRep (varType var)) of
    [] -> pure []
    [runtimeRep] -> pure [GrinVar (varName var) (sourceUnique var) runtimeRep]
    runtimeReps ->
      sequence
        [ freshVar (varName var <> "_" <> T.pack (show index)) runtimeRep
        | (index, runtimeRep) <- zip [0 :: Int ..] runtimeReps
        ]

lookupLocalVars :: Var -> LowerM [GrinVar]
lookupLocalVars var = do
  locals <- gets lowerLocalVars
  case Map.lookup (varKey var) locals of
    Just values -> pure values
    Nothing -> error ("GRIN lowering lost local binding for " <> T.unpack (varName var))

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
    FcTyApp function argument -> do
      functionType <- exprType function
      case functionType of
        TcForAllTy tyVar body -> Just (substType (Map.singleton tyVar argument) body)
        _ -> Just functionType
    FcLam var body -> TcFunTy (varType var) <$> exprType body
    FcTyLam tyVar body -> TcForAllTy tyVar <$> exprType body
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

functionArgumentRep :: FcExpr -> RuntimeRep
functionArgumentRep function =
  case exprType function >>= functionArgumentType of
    Just argument -> typeRuntimeRep argument
    Nothing -> error ("GRIN lowering could not determine function argument type: " <> show function)
  where
    functionArgumentType functionType =
      case functionType of
        TcFunTy argument _ -> Just argument
        TcQualTy [] body -> functionArgumentType body
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
        FcPrimitive {} -> []
        FcForeignImport {} -> []
        FcTopBind bind ->
          [ varName var
          | (var, expr) <- topBindings bind,
            not (isDirectFunction expr)
          ]
    topBindings bind =
      case bind of
        FcNonRec var expr -> [(var, expr)]
        FcRec bindings -> bindings

programConstructors :: FcProgram -> [(Text, Int)]
programConstructors program =
  [ (name, length fields)
  | FcData _ _ constructors <- fcTopBinds program,
    (name, fields) <- constructors
  ]

programWhnfGlobalNames :: FcProgram -> [Text]
programWhnfGlobalNames program = concatMap topWhnfGlobalNames (fcTopBinds program)
  where
    constructorArities = Map.fromList (builtinConstructors <> programConstructors program)
    topWhnfGlobalNames topBind =
      case topBind of
        FcData _ _ constructors -> map fst constructors
        FcNewtype {} -> []
        FcPrimitive {} -> []
        FcForeignImport {} -> []
        FcTopBind bind ->
          [ varName var
          | (var, expr) <- topBindings bind,
            isStaticWhnf constructorArities expr
          ]
    topBindings bind =
      case bind of
        FcNonRec var expr -> [(var, expr)]
        FcRec bindings -> bindings

programCodeInfos :: FcProgram -> [(Text, GrinCodeInfo)]
programCodeInfos program =
  [ (varName var, codeInfoFor var expr)
  | FcTopBind bind <- fcTopBinds program,
    (var, expr) <- topBindings bind,
    isDirectFunction expr
  ]
  where
    topBindings bind =
      case bind of
        FcNonRec var expr -> [(var, expr)]
        FcRec bindings -> bindings

codeInfoFor :: Var -> FcExpr -> GrinCodeInfo
codeInfoFor var expr =
  GrinCodeInfo
    { grinCodeSourceName = varName var,
      grinCodeFunctionName = topFunctionName var,
      grinCodeParameterLayouts =
        [ runtimeRepComponents (typeRuntimeRep (varType binder))
        | binder <- binders
        ],
      grinCodeResultRep = exprRuntimeRep body
    }
  where
    (binders, body) = collectLeadingLambdas expr

topFunctionName :: Var -> FunctionName
topFunctionName var = FunctionName ("$entry$" <> varName var)

collectLeadingLambdas :: FcExpr -> ([Var], FcExpr)
collectLeadingLambdas expr =
  case expr of
    FcLam binder body ->
      let (binders, result) = collectLeadingLambdas body
       in (binder : binders, result)
    FcTyLam _ body -> collectLeadingLambdas body
    FcCast inner _ -> collectLeadingLambdas inner
    _ -> ([], expr)

isStaticWhnf :: Map Text Int -> FcExpr -> Bool
isStaticWhnf constructorArities expr =
  case constructorApplication constructorArities Set.empty expr of
    Just (_, arguments) -> all isStaticArgument arguments
    Nothing -> False
  where
    isStaticArgument argument =
      case argument of
        FcLit {} -> True
        FcVar var ->
          null (runtimeRepComponents (typeRuntimeRep (varType var)))
            || Map.lookup (varName var) constructorArities == Just 0
        FcTyApp inner _ -> isStaticArgument inner
        FcCast inner _ -> isStaticArgument inner
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
    FcTyApp inner _ -> exprVars inner
    FcLam var body -> var : exprVars body
    FcTyLam _ body -> exprVars body
    FcLet bind body -> bindVars bind <> exprVars body
    FcCase scrutinee binder alternatives ->
      exprVars scrutinee <> (binder : concatMap altVars alternatives)
    FcCast inner _ -> exprVars inner
    FcCallForeign _ arguments -> concatMap exprVars arguments

altVars :: FcAlt -> [Var]
altVars alt = grinAltBinders' <> exprVars (altRhs alt)
  where
    grinAltBinders' = altBinders alt
