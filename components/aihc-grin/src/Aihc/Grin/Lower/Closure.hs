{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Closure conversion and strict lowering of erased FC2 runtime forms.
module Aihc.Grin.Lower.Closure
  ( GrinInterface,
    GrinLinkNames,
    linkNamesForProgram,
    extractGrinInterfaceWithLinkNames,
    lowerProgramWithInterfaceAndLinkNames,
  )
where

import Aihc.Grin.Analysis (freeExprVars)
import Aihc.Grin.Anf (normalizeGrinProgram)
import Aihc.Grin.Lower.Runtime
import Aihc.Grin.Syntax
import Control.Applicative ((<|>))
import Control.Monad.Trans.State.Strict (State, gets, modify', runState)
import Data.List (mapAccumL)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
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
    lowerLocalCodeInfosByUnique :: !(Map Unique [(RuntimeType, GrinCodeInfo)]),
    lowerCodeInfosByName :: !(Map Text GrinCodeInfo),
    lowerLocalCodeLinkNames :: !(Set Text),
    lowerReferencedExternalCodeLinkNames :: !(Set Text),
    lowerLocalGlobalNames :: !(Set Text),
    lowerReferencedExternalGlobalNames :: !(Set Text),
    lowerGlobalNames :: !(Map Text Text),
    lowerWhnfGlobalNames :: !(Map Text Text),
    lowerLocalVars :: !(Map (Text, Unique) [GrinVar]),
    lowerCurrentProvenance :: !(Maybe Text),
    lowerUseIncrementalCodeLookup :: !Bool,
    lowerLinkNames :: !GrinLinkNames,
    lowerLinkNameOccurrences :: !(Map Unique Int),
    lowerUnboxedTupleConstructors :: !(Set Text)
  }

type LowerM = State LowerState

data LoweredTop = LoweredTop
  { loweredConstructors :: ![(Text, [[RuntimeRep]])],
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

-- | Linker identities for top-level FC2 values, keyed by their compiler
-- unique. The text representation uses NUL only as an internal component
-- separator; native backends render it into a readable object symbol.
data GrinLinkNames = GrinLinkNames
  { grinNativeLinkNames :: !(Map Unique [Text]),
    grinSourceLinkNames :: !(Map Unique [Text]),
    grinConstructorNames :: !(Map Text (Text, Int))
  }
  deriving (Eq, Show, Read)

instance Semigroup GrinLinkNames where
  left <> right =
    GrinLinkNames
      { grinNativeLinkNames = Map.unionWith (<>) (grinNativeLinkNames left) (grinNativeLinkNames right),
        grinSourceLinkNames = Map.unionWith (<>) (grinSourceLinkNames left) (grinSourceLinkNames right),
        grinConstructorNames = grinConstructorNames left <> grinConstructorNames right
      }

instance Monoid GrinLinkNames where
  mempty = GrinLinkNames Map.empty Map.empty Map.empty

-- | Assign every top-level value in a program a linker identity consisting
-- of the supplied package-and-module components followed by its source name.
linkNamesForProgram :: [Text] -> [Text] -> RuntimeProgram -> GrinLinkNames
linkNamesForProgram libraryId moduleNameComponents program =
  GrinLinkNames
    { grinNativeLinkNames =
        Map.fromListWith
          (flip (<>))
          [ (varUnique var, [T.intercalate "\0" (nativeSymbolComponents ordinal var)])
          | (var, ordinal) <- topLevelVarsWithOrdinals
          ],
      grinSourceLinkNames =
        Map.fromListWith
          (flip (<>))
          [ (varUnique var, [sourceSymbolNameFor var])
          | (var, _) <- topLevelVarsWithOrdinals
          ],
      grinConstructorNames =
        Map.fromList
          [ (runtimeConstructorName (runtimeConstructorSymbolOrigin (runtimeDataConOrigin constructor)), (runtimeConstructorName (runtimeConstructorSymbolOrigin (runtimeDataConOrigin constructor)), length (runtimeDataConFields constructor)))
          | RuntimeData declaration <- runtimeTopBinds program,
            not (isUnboxedTupleData declaration),
            constructor <- runtimeDataConstructors declaration
          ]
    }
  where
    topLevelVars = [var | RuntimeTopValue bind <- runtimeTopBinds program, var <- topBindVars bind]
    nameCounts = Map.fromListWith (+) [(identityKey var, 1 :: Int) | var <- topLevelVars]
    topLevelVarsWithOrdinals = snd (mapAccumL annotate Map.empty topLevelVars)
    annotate :: Map Text Int -> RuntimeVar -> (Map Text Int, (RuntimeVar, Int))
    annotate ordinals var =
      let key = identityKey var
          ordinal = Map.findWithDefault 0 key ordinals + 1
       in (Map.insert key ordinal ordinals, (var, ordinal))
    nativeSymbolComponents ordinal var =
      case varResolvedName var of
        Just (RuntimeTopLevelOrigin _ moduleName symbolName) ->
          libraryId
            <> T.splitOn "." moduleName
            <> [symbolName]
            <> uniqueSuffix ordinal var
        _ -> libraryId <> moduleNameComponents <> symbolComponents ordinal var
    symbolComponents ordinal var =
      [varName var] <> uniqueSuffix ordinal var
    uniqueSuffix ordinal var =
      ["u" <> T.pack (show (sourceUnique var)) <> "n" <> T.pack (show ordinal) | Map.findWithDefault 0 (identityKey var) nameCounts > 1]
    identityKey var = maybe (varName var) runtimeSymbolOriginText (varResolvedName var)
    sourceSymbolNameFor var =
      case varResolvedName var of
        Just origin@RuntimeTopLevelOrigin {} -> runtimeSymbolOriginText origin
        Nothing -> sourceSymbolName (varName var)
    sourceSymbolName symbolName =
      (if packageIdentity == "" then "" else packageIdentity <> ":")
        <> T.intercalate "." (moduleNameComponents <> [symbolName])
    packageIdentity =
      fromMaybe
        ""
        (Just (runtimeModulePackageText (runtimeProgramModule program)))
    topBindVars bind =
      case bind of
        RuntimeNonRec var _ -> [var]
        RuntimeRec bindings -> map fst bindings

-- | Runtime facts exported by one compiled unit. Function facts retain their
-- unit-local compiler unique and source name; lowering never compares uniques
-- from different units.
data GrinInterface = GrinInterface
  { grinInterfaceGlobals :: !(Map Text Text),
    grinInterfaceWhnfGlobals :: !(Map Text Text),
    grinInterfaceConstructorArities :: !(Map Text Int),
    grinInterfacePrimitiveArities :: !(Map Text Int),
    grinInterfaceUnboxedTupleConstructors :: !(Set Text),
    grinInterfaceCodeInfosByUnique :: !(Map Unique [(RuntimeType, GrinCodeInfo)]),
    grinInterfaceCodeInfosByName :: !(Map Text GrinCodeInfo),
    grinInterfaceCodeInfosByLinkName :: !(Map Text GrinCodeInfo)
  }
  deriving (Eq, Show, Read)

instance Semigroup GrinInterface where
  left <> right =
    GrinInterface
      { grinInterfaceGlobals = grinInterfaceGlobals left <> grinInterfaceGlobals right,
        grinInterfaceWhnfGlobals = grinInterfaceWhnfGlobals left <> grinInterfaceWhnfGlobals right,
        grinInterfaceConstructorArities = grinInterfaceConstructorArities left <> grinInterfaceConstructorArities right,
        grinInterfacePrimitiveArities = grinInterfacePrimitiveArities left <> grinInterfacePrimitiveArities right,
        grinInterfaceUnboxedTupleConstructors = grinInterfaceUnboxedTupleConstructors left <> grinInterfaceUnboxedTupleConstructors right,
        grinInterfaceCodeInfosByUnique = Map.unionWith (<>) (grinInterfaceCodeInfosByUnique left) (grinInterfaceCodeInfosByUnique right),
        grinInterfaceCodeInfosByName = grinInterfaceCodeInfosByName left <> grinInterfaceCodeInfosByName right,
        grinInterfaceCodeInfosByLinkName = grinInterfaceCodeInfosByLinkName left <> grinInterfaceCodeInfosByLinkName right
      }

instance Monoid GrinInterface where
  mempty = GrinInterface Map.empty Map.empty Map.empty Map.empty Set.empty Map.empty Map.empty Map.empty

-- | Extract runtime facts whose native code labels identify each definition's
-- package and module. Source-level lookup keys deliberately remain unchanged.
extractGrinInterfaceWithLinkNames :: GrinLinkNames -> RuntimeProgram -> GrinInterface
extractGrinInterfaceWithLinkNames = extractPreparedGrinInterface

extractPreparedGrinInterface :: GrinLinkNames -> RuntimeProgram -> GrinInterface
extractPreparedGrinInterface linkNames program =
  GrinInterface
    { grinInterfaceGlobals =
        Map.fromList
          ( [(name, name) | (name, _) <- programConstructors program]
              <> [(sourceName, name) | (sourceName, (name, _)) <- constructorNames]
              <> [ (sourceName, linkedName)
                 | (var, qualifiedName, linkedName, _) <- globalInfos,
                   sourceName <- [varName var, qualifiedName]
                 ]
          ),
      grinInterfaceWhnfGlobals =
        Map.fromList
          ( [(name, name) | (name, arity) <- programConstructors program, arity == 0]
              <> [(sourceName, name) | (sourceName, (name, 0)) <- constructorNames]
              <> [ (sourceName, linkedName)
                 | (var, qualifiedName, linkedName, True) <- globalInfos,
                   sourceName <- [varName var, qualifiedName]
                 ]
          ),
      grinInterfaceConstructorArities =
        Map.fromList
          ( programConstructors program
              <> [(sourceName, arity) | (sourceName, (_, arity)) <- constructorNames]
          ),
      grinInterfacePrimitiveArities =
        Map.fromList
          [ (varName var, arity)
          | RuntimePrimitive var arity <- runtimeTopBinds program,
            varName var /= "seq"
          ],
      grinInterfaceUnboxedTupleConstructors =
        Set.fromList
          [ runtimeConstructorName (runtimeConstructorSymbolOrigin (runtimeDataConOrigin constructor))
          | RuntimeData declaration <- runtimeTopBinds program,
            isUnboxedTupleData declaration,
            constructor <- runtimeDataConstructors declaration
          ],
      grinInterfaceCodeInfosByUnique = Map.fromListWith (<>) [(varUnique var, [(varType var, info)]) | (var, _, info) <- codeInfos],
      grinInterfaceCodeInfosByName =
        Map.fromList
          [ (sourceName, info)
          | (var, qualifiedName, info) <- codeInfos,
            sourceName <- [varName var, qualifiedName]
          ],
      grinInterfaceCodeInfosByLinkName = Map.fromList [(grinCodeSourceName info, info) | (_, _, info) <- codeInfos]
    }
  where
    codeInfos = programCodeInfos linkNames program
    globalInfos = programGlobalInfos linkNames program
    constructorNames = Map.toList (grinConstructorNames linkNames)

-- | Lower one compilation unit against imported runtime facts with an
-- explicit linker identity for each local top-level definition.
-- Convert lambdas and thunks to closures.
-- Make evaluation, application, allocation, and exception control explicit.
lowerProgramWithInterfaceAndLinkNames :: GrinLinkNames -> GrinInterface -> RuntimeProgram -> GrinProgram
lowerProgramWithInterfaceAndLinkNames linkNames imported sourceProgram =
  lowerProgramWithEnvironment linkNames imported localInterface (programEnvironment (localInterface <> imported)) program
  where
    program = sourceProgram
    localInterface = extractPreparedGrinInterface linkNames program

data ProgramEnvironment = ProgramEnvironment
  { programEnvironmentGlobals :: !(Map Text Text),
    programEnvironmentWhnfGlobals :: !(Map Text Text),
    programEnvironmentConstructorArities :: !(Map Text Int),
    programEnvironmentPrimitiveArities :: !(Map Text Int),
    programEnvironmentUnboxedTupleConstructors :: !(Set Text),
    programEnvironmentCodeInfosByName :: !(Map Text GrinCodeInfo)
  }

programEnvironment :: GrinInterface -> ProgramEnvironment
programEnvironment interface =
  ProgramEnvironment
    { programEnvironmentGlobals = Map.fromList [(name, name) | (name, _) <- builtinConstructors] <> grinInterfaceGlobals interface,
      programEnvironmentWhnfGlobals = Map.fromList [(name, name) | (name, layouts) <- builtinConstructors, null layouts] <> grinInterfaceWhnfGlobals interface,
      programEnvironmentConstructorArities = Map.fromList [(name, length layouts) | (name, layouts) <- builtinConstructors] <> grinInterfaceConstructorArities interface,
      programEnvironmentPrimitiveArities = grinInterfacePrimitiveArities interface,
      programEnvironmentUnboxedTupleConstructors = grinInterfaceUnboxedTupleConstructors interface,
      programEnvironmentCodeInfosByName = grinInterfaceCodeInfosByName interface
    }

lowerProgramWithEnvironment :: GrinLinkNames -> GrinInterface -> GrinInterface -> ProgramEnvironment -> RuntimeProgram -> GrinProgram
lowerProgramWithEnvironment linkNames imported local environment program =
  normalizeGrinProgram
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
          lowerLocalCodeInfosByUnique = grinInterfaceCodeInfosByUnique local,
          lowerCodeInfosByName = programEnvironmentCodeInfosByName environment,
          lowerLocalCodeLinkNames = localCodeLinkNames,
          lowerReferencedExternalCodeLinkNames = Set.empty,
          lowerLocalGlobalNames =
            Set.fromList
              ( Map.elems (grinInterfaceWhnfGlobals local)
                  <> [linkedName | (_, _, linkedName, _) <- programGlobalInfos linkNames program]
              ),
          lowerReferencedExternalGlobalNames = Set.empty,
          lowerGlobalNames = programEnvironmentGlobals environment,
          lowerWhnfGlobalNames = programEnvironmentWhnfGlobals environment,
          lowerLocalVars = Map.empty,
          lowerCurrentProvenance = Nothing,
          lowerUseIncrementalCodeLookup = not (grinLinkNamesEmpty linkNames),
          lowerLinkNames = linkNames,
          lowerLinkNameOccurrences = Map.empty,
          lowerUnboxedTupleConstructors = programEnvironmentUnboxedTupleConstructors environment
        }
    (topParts, finalState) = runState (mapM lowerTopBind (runtimeTopBinds program)) initialState
    tops = mconcat topParts
    localCodeLinkNames = Set.fromList [grinCodeSourceName info | (_, _, info) <- programCodeInfos linkNames program]
    externalCodeInfos =
      [ info
      | (linkName', info) <- Map.toAscList (grinInterfaceCodeInfosByLinkName imported),
        linkName' `Set.member` lowerReferencedExternalCodeLinkNames finalState
      ]

lowerTopBind :: RuntimeTopBind -> LowerM LoweredTop
lowerTopBind topBind =
  case topBind of
    RuntimeData declaration ->
      if isUnboxedTupleData declaration
        then pure mempty
        else pure mempty {loweredConstructors = [(runtimeConstructorName (runtimeConstructorSymbolOrigin (runtimeDataConOrigin constructor)), map (runtimeRepComponents . typeRuntimeRep) (runtimeDataConFields constructor)) | constructor <- runtimeDataConstructors declaration]}
    RuntimePrimitive var _
      | varName var == "casMutVar#" ->
          pure
            mempty
              { loweredPrimitives =
                  [ (GrinVar "aihcCasMutVarFlag" (sourceUnique var) IntRep, 3)
                  ]
              }
    RuntimePrimitive var arity ->
      pure
        mempty
          { loweredPrimitives =
              [ (plainGlobalVar var, arity)
              | varName var `notElem` ["aihcExit#", "unsafeCoerce#", "raise#", "catch#", "seq"]
              ]
          }
    RuntimeForeignImport foreignCall ->
      pure mempty {loweredForeignCalls = [lowerForeignCall foreignCall]}
    RuntimeTopValue bind -> lowerTopValueBind bind

lowerTopValueBind :: RuntimeBind -> LowerM LoweredTop
lowerTopValueBind bind =
  case bind of
    RuntimeNonRec var expr -> lowerBinding var expr
    RuntimeRec bindings ->
      mconcat <$> mapM (uncurry lowerBinding) bindings
  where
    lowerBinding var expr =
      withProvenance (varName var) $ do
        linkedName <- nextLinkName var
        if isDirectFunction expr
          then do
            emitTopFunction linkedName var expr
            pure mempty
          else do
            staticNode <- lowerStaticNode expr
            topVar <- freshTopVar linkedName
            case staticNode of
              Just node -> pure mempty {loweredWhnfGlobals = [(topVar, node)]}
              Nothing -> do
                node <- makeTopThunk var expr
                pure mempty {loweredCafs = [(topVar, node)]}

-- | A top-level RHS is already a function value exactly when reaching its
-- first runtime construct requires only erasing type abstraction or casts.
-- Any term computation before the lambda must remain an updateable CAF so its
-- result is shared.
isDirectFunction :: RuntimeExpr -> Bool
isDirectFunction expr =
  case expr of
    RuntimeLam {} -> True
    RuntimeTyLam _ body -> isDirectFunction body
    _ -> False

emitTopFunction :: Text -> RuntimeVar -> RuntimeExpr -> LowerM ()
emitTopFunction linkedName _var expr = do
  let (binders, body) = collectLeadingLambdas expr
      functionName = linkedFunctionName linkedName
  (parameters, loweredBody) <- withFreshLocalVars binders $ \groups -> do
    body' <- lowerExpr body
    pure (concat groups, body')
  emitFunction
    GrinFunction
      { grinFunctionName = functionName,
        grinFunctionLinkName = Just linkedName,
        grinFunctionParameters = parameters,
        grinFunctionResultRep = exprRuntimeRep body,
        grinFunctionBody = loweredBody
      }

lowerExpr :: RuntimeExpr -> LowerM GrinExpr
lowerExpr expr = do
  constructorArities <- gets lowerConstructorArities
  primitiveArities <- gets lowerPrimitiveArities
  localVars <- gets lowerLocalVars
  case constructorApplication constructorArities (Map.keysSet localVars) expr of
    Just (constructor, arguments) ->
      lowerArgumentMany arguments $ \values ->
        let remaining = constructorArities Map.! constructor - length arguments
         in pure (GrinStore (GrinNode (GrinConstructor constructor remaining) values))
    Nothing ->
      case primitiveApplication primitiveArities (Map.keysSet localVars) expr of
        Just ("raise#", [exception]) ->
          lowerSingleOperand "exception" exception (pure . GrinThrow)
        Just ("aihcExit#", [status, _state]) ->
          lowerSingleOperand "exit_status" status (pure . GrinExit)
        Just ("catch#", [action, handler, _state]) ->
          lowerSingleArgument action $ \actionValue ->
            lowerCatchHandler (exprRuntimeRep expr) handler $ \handlerValue ->
              pure (GrinCatch (exprRuntimeRep expr) actionValue handlerValue [])
        Just ("unsafeCoerce#", argument : extraArguments) ->
          lowerUnsafeCoerceApplication expr argument extraArguments
        Just ("seq", _) ->
          error "GRIN lowering received an unexpanded seq pseudo-op"
        Just (name, arguments) ->
          case Map.lookup name primitiveArities of
            Just arity -> lowerPrimitiveApplication expr name arity arguments
            Nothing -> error "GRIN lowering lost primitive arity"
        Nothing -> lowerOrdinaryExpr expr

lowerOrdinaryExpr :: RuntimeExpr -> LowerM GrinExpr
lowerOrdinaryExpr expr = do
  tupleArguments <- unboxedTupleArguments expr
  case tupleArguments of
    Just arguments ->
      lowerArgumentMany arguments (pure . GrinConstant)
    Nothing -> lowerNonTupleExpr expr

lowerNonTupleExpr :: RuntimeExpr -> LowerM GrinExpr
lowerNonTupleExpr expr =
  case expr of
    RuntimeVarExpr var ->
      do
        codeInfo <- lookupCodeInfo var
        case codeInfo of
          Just info -> pure (GrinStore (knownFunctionNode info 0 []))
          Nothing -> do
            direct <- lowerDirectValues expr
            case direct of
              Just values -> pure (GrinConstant values)
              Nothing -> do
                runtimeVar <- lookupRuntimeVar var
                let resultRep = typeRuntimeRep (varType var)
                pure (GrinEval resultRep (GrinVarValue runtimeVar))
    RuntimeLit literal _ ->
      pure (GrinConstant [GrinLitValue (lowerLiteral literal)])
    RuntimeApp {} ->
      lowerApplication expr
    RuntimeTyApp inner _ ->
      lowerExpr inner
    RuntimeLam var body ->
      lowerLambda var body
    RuntimeTyLam _ body ->
      lowerExpr body
    RuntimeLet bind body ->
      lowerLet bind body
    RuntimeCase scrutinee binder alternatives ->
      lowerCase scrutinee binder alternatives
    RuntimeCallForeign foreignCall arguments ->
      lowerStrictMany arguments $ \values ->
        pure (GrinForeignCallExpr (lowerForeignCall foreignCall) values)

lowerApplication :: RuntimeExpr -> LowerM GrinExpr
lowerApplication expr =
  case collectApplications expr of
    (RuntimeVarExpr var, arguments) -> do
      codeInfo <- lookupCodeInfo var
      case codeInfo of
        Just info -> lowerKnownApplication expr info arguments
        Nothing -> lowerUnknownApplication expr
    _ -> lowerUnknownApplication expr

lowerKnownApplication :: RuntimeExpr -> GrinCodeInfo -> [RuntimeExpr] -> LowerM GrinExpr
lowerKnownApplication originalExpr info arguments = do
  let (entryArguments, extraArguments) = splitAt termArity arguments
  lowerArgumentMany entryArguments $ \values ->
    case extraArguments of
      [] ->
        case compare (length entryArguments) termArity of
          LT -> pure (GrinStore (knownFunctionNode info (length entryArguments) values))
          EQ -> pure (GrinCall (exprRuntimeRep originalExpr) (grinCodeFunctionName info) values)
          GT -> error "GRIN lowering supplied more logical arguments than the known function accepts"
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

lowerPrimitiveApplication :: RuntimeExpr -> Text -> Int -> [RuntimeExpr] -> LowerM GrinExpr
lowerPrimitiveApplication originalExpr name arity arguments =
  case compare suppliedArity arity of
    LT ->
      lowerArgumentMany arguments $
        fmap GrinStore . makePrimitiveClosure originalExpr name (arity - suppliedArity)
    EQ ->
      lowerArgumentMany arguments $ \values ->
        lowerSaturatedPrimitive (exprRuntimeRep originalExpr) name values
    GT -> do
      let (saturatedArguments, extraArguments) = splitAt arity arguments
          saturatedExpr = dropLastTermApplications (suppliedArity - arity) originalExpr
          saturatedRep = exprRuntimeRep saturatedExpr
      lowerArgumentMany saturatedArguments $ \values -> do
        resultVars <- freshVars "primitive" saturatedRep
        case resultVars of
          [resultVar] -> do
            rest <- lowerRemainingApplications saturatedExpr (GrinVarValue resultVar) extraArguments
            primitive <- lowerSaturatedPrimitive saturatedRep name values
            pure (bindExpr resultVars primitive rest)
          _ -> error "GRIN lowering expected an overapplied primitive to return one function value"
  where
    suppliedArity = length arguments

-- Array storage is one info-table word, one length word, and one word per
-- element. Make that dynamic reservation explicit before CPS so GC lowering
-- only has to attach its ordinary live-root set to the safepoint.
lowerSaturatedPrimitive :: RuntimeRep -> Text -> [GrinValue] -> LowerM GrinExpr
lowerSaturatedPrimitive resultRep "newArray#" arguments@[size, _] = do
  requiredWords <- freshVar "array_words" IntRep
  pure
    ( GrinBind
        [requiredWords]
        (GrinPrimitiveCall IntRep "+#" [size, GrinLitValue (GrinLitInt IntRep 2)])
        ( GrinBind
            []
            (GrinEnsureHeap (GrinVarValue requiredWords) [])
            (GrinPrimitiveCall resultRep "newArray#" arguments)
        )
    )
lowerSaturatedPrimitive resultRep "newMutVar#" arguments@[_] =
  pure
    ( GrinBind
        []
        (GrinEnsureHeap (GrinLitValue (GrinLitInt IntRep 3)) [])
        (GrinPrimitiveCall resultRep "newMutVar#" arguments)
    )
-- Native foreign calls return one machine value, while GHC's casMutVar#
-- returns both a failure flag and the final MutVar# contents. Keep the atomic
-- operation in a single internal runtime call, then recover the final value:
-- the replacement was installed on success, and a non-preemptible read
-- observes the failed CAS value on failure.
lowerSaturatedPrimitive _ "casMutVar#" arguments@[reference, _, replacement] = do
  flag <- freshVar "cas_flag" IntRep
  caseFlag <- freshVar "cas_case" IntRep
  current <- freshVar "cas_current" (grinValueRuntimeRep replacement)
  let flagValue = GrinVarValue flag
      success =
        GrinAlt
          { grinAltCon = GrinLitAlt (GrinLitInt IntRep 0),
            grinAltBinders = [],
            grinAltRhs = GrinConstant [flagValue, replacement]
          }
      failure =
        GrinAlt
          { grinAltCon = GrinDefaultAlt,
            grinAltBinders = [],
            grinAltRhs =
              GrinBind
                [current]
                (GrinPrimitiveCall (grinVarRuntimeRep current) "readMutVar#" [reference])
                (GrinConstant [flagValue, GrinVarValue current])
          }
  pure
    ( GrinBind
        [flag]
        (GrinPrimitiveCall IntRep "aihcCasMutVarFlag" arguments)
        (GrinCase flagValue caseFlag [success, failure])
    )
lowerSaturatedPrimitive resultRep name arguments =
  pure (GrinPrimitiveCall resultRep name arguments)

-- unsafeCoerce# changes only the static type of a value. FC2 has already
-- checked the application, so GRIN can erase the coercion while preserving
-- the argument's lazy evaluation. This keeps the operation out of every
-- backend, including overapplication of a coerced function value.
lowerUnsafeCoerceApplication :: RuntimeExpr -> RuntimeExpr -> [RuntimeExpr] -> LowerM GrinExpr
lowerUnsafeCoerceApplication originalExpr argument extraArguments =
  lowerSingleArgument argument $ \value ->
    case extraArguments of
      [] -> pure (GrinConstant [value])
      _ -> do
        let coercedExpr = dropLastTermApplications (length extraArguments) originalExpr
        lowerRemainingApplications coercedExpr value extraArguments

-- Primitive operations have no heap representation. Under-application is
-- represented by an ordinary closure whose generated entry makes the direct,
-- saturated primitive call once all remaining logical arguments arrive.
makePrimitiveClosure :: RuntimeExpr -> Text -> Int -> [GrinValue] -> LowerM GrinNode
makePrimitiveClosure originalExpr name remaining captured = do
  (argumentTypes, resultType) <-
    case exprType originalExpr >>= splitFunctionTypes remaining of
      Just wrapperType -> pure wrapperType
      Nothing -> error ("GRIN lowering could not construct primitive wrapper for " <> show name)
  captureParameters <- mapM (freshVar "primitive_capture" . grinValueRuntimeRep) captured
  argumentGroups <- mapM (freshVars "primitive_argument" . typeRuntimeRep) argumentTypes
  functionName <- freshFunction "primitive"
  let argumentLayouts = map (map grinVarRuntimeRep) argumentGroups
      arguments = map GrinVarValue (captureParameters <> concat argumentGroups)
      resultRep = typeRuntimeRep resultType
  body <-
    case (name, arguments) of
      ("aihcExit#", [status]) -> pure (GrinExit status)
      ("unsafeCoerce#", _) -> pure (GrinConstant arguments)
      ("raise#", [exception]) -> pure (GrinThrow exception)
      ("catch#", [action, handler]) ->
        wrapCatchHandlerValue resultRep liftedRuntimeRep liftedRuntimeRep liftedRuntimeRep handler $ \handlerValue ->
          pure (GrinCatch resultRep action handlerValue [])
      _ -> lowerSaturatedPrimitive resultRep name arguments
  emitFunction
    GrinFunction
      { grinFunctionName = functionName,
        grinFunctionLinkName = Nothing,
        grinFunctionParameters = captureParameters <> concat argumentGroups,
        grinFunctionResultRep = resultRep,
        grinFunctionBody = body
      }
  pure (GrinNode (GrinClosure functionName argumentLayouts) captured)

splitFunctionTypes :: Int -> RuntimeType -> Maybe ([RuntimeType], RuntimeType)
splitFunctionTypes count ty
  | count == 0 = Just ([], ty)
splitFunctionTypes count (RuntimeFunction argument result) = do
  (arguments, finalResult) <- splitFunctionTypes (count - 1) result
  pure (argument : arguments, finalResult)
splitFunctionTypes _ _ = Nothing

dropLastTermApplications :: Int -> RuntimeExpr -> RuntimeExpr
dropLastTermApplications count expr
  | count <= 0 = expr
  | otherwise =
      case expr of
        RuntimeApp function _ -> dropLastTermApplications (count - 1) function
        RuntimeTyApp inner ty -> RuntimeTyApp (dropLastTermApplications count inner) ty
        _ -> error "GRIN lowering could not split an overapplication"

lowerRemainingApplications :: RuntimeExpr -> GrinValue -> [RuntimeExpr] -> LowerM GrinExpr
lowerRemainingApplications functionExpr functionValue arguments =
  case arguments of
    [] -> pure (GrinConstant [functionValue])
    argument : rest ->
      evaluateGrinValue "function" (exprRuntimeRep functionExpr) functionValue $ \evaluatedFunction -> do
        let appliedExpr = RuntimeApp functionExpr argument
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

lowerUnknownApplication :: RuntimeExpr -> LowerM GrinExpr
lowerUnknownApplication expr =
  case expr of
    RuntimeApp function argument ->
      lowerSingleEvaluatedOperand "function" function $ \functionValue ->
        lowerArgument argument $ \argumentValues ->
          pure (GrinApply (applicationResultRep function) functionValue argumentValues)
    _ -> error "GRIN lowering expected an application"

knownFunctionNode :: GrinCodeInfo -> Int -> [GrinValue] -> GrinNode
knownFunctionNode info suppliedTermArity =
  GrinNode
    (GrinClosure (grinCodeFunctionName info) (drop suppliedTermArity (grinCodeParameterLayouts info)))

lowerCase :: RuntimeExpr -> RuntimeVar -> [RuntimeAlt] -> LowerM GrinExpr
lowerCase scrutinee binder alternatives =
  case (runtimeRepComponents scrutineeRep, scrutineeRep, alternatives) of
    ([], _, [alternative]) -> do
      rhs <-
        withBindings
          ((binder, []) : [(fieldBinder, []) | fieldBinder <- altBinders alternative])
          (lowerExpr (altRhs alternative))
      scrutineeExpr <- lowerExpr scrutinee
      pure (bindExpr [] scrutineeExpr rhs)
    (_, TupleRep _, alternative : _)
      | DataAlt constructor <- altCon alternative,
        unboxedTuplePunctuation (runtimeConstructorSourceName constructor) ->
          -- An unboxed tuple has exactly one constructor. The source match
          -- compiler may retain a syntactic fall-through alternative while
          -- compiling nested refutable fields, but that alternative is
          -- unreachable at the outer tuple match.
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

lowerUnboxedTupleCase :: RuntimeExpr -> RuntimeVar -> RuntimeAlt -> LowerM GrinExpr
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

-- | Remove unreachable work after a terminal expression. Otherwise, sequence
-- an expression only when its results are used by the body. Forwarding every
-- bound result unchanged is exactly the value expression.
bindExpr :: [GrinVar] -> GrinExpr -> GrinExpr -> GrinExpr
bindExpr vars valueExpr body =
  case valueExpr of
    GrinExit {} -> valueExpr
    _ ->
      case body of
        GrinConstant values
          | values == map GrinVarValue vars -> valueExpr
        _ -> GrinBind vars valueExpr body

-- | A non-recursive lifted binding whose pointer is forced immediately and is
-- dead afterward needs neither an updateable cell nor an outlined entry. The
-- caller can lower its right-hand side directly into this continuation.
discardedImmediateEval :: [GrinVar] -> GrinExpr -> Maybe ([GrinVar], GrinExpr)
discardedImmediateEval vars body =
  case (vars, body) of
    ( [pointer],
      GrinBind resultVars (GrinEval _ (GrinVarValue evaluatedPointer)) rest
      )
        | pointer == evaluatedPointer,
          pointer `Set.notMember` freeExprVars rest ->
            Just (resultVars, rest)
    _ -> Nothing

lowerLambda :: RuntimeVar -> RuntimeExpr -> LowerM GrinExpr
lowerLambda binder body =
  GrinStore <$> makeClosureNode (RuntimeLam binder body)

makeClosureNode :: RuntimeExpr -> LowerM GrinNode
makeClosureNode expr = do
  etaReduced <- etaReduceKnownFunction expr
  case etaReduced of
    Just node -> pure node
    Nothing -> do
      let (binders, lambdaBody) = collectLeadingLambdas expr
      captures <- capturesFor expr
      functionName <- freshFunction "closure"
      (parameterLayouts, parameters, loweredBody) <- withFreshLocalVars binders $ \groups -> do
        body' <- lowerExpr lambdaBody
        pure (map (map grinVarRuntimeRep) groups, concat groups, body')
      emitFunction
        GrinFunction
          { grinFunctionName = functionName,
            grinFunctionLinkName = Nothing,
            grinFunctionParameters = captures <> parameters,
            grinFunctionResultRep = exprRuntimeRep lambdaBody,
            grinFunctionBody = loweredBody
          }
      pure (GrinNode (GrinClosure functionName parameterLayouts) (map GrinVarValue captures))

-- A closure such as @\x -> pureIO x@ is only an eta-expanded view of known
-- code. Pointing at that code directly preserves partial-application behavior
-- while avoiding both the wrapper entry and an extra closure allocation.
etaReduceKnownFunction :: RuntimeExpr -> LowerM (Maybe GrinNode)
etaReduceKnownFunction expr =
  case collectLeadingLambdas expr of
    ([], _) -> pure Nothing
    (binders, body) ->
      case collectApplications body of
        (RuntimeVarExpr target, arguments)
          | map etaArgumentVar arguments == map Just binders -> do
              codeInfo <- lookupCodeInfo target
              pure $ do
                info <- codeInfo
                let binderLayouts = map (runtimeRepComponents . typeRuntimeRep . varType) binders
                if binderLayouts == take (length binders) (grinCodeParameterLayouts info)
                  then Just (knownFunctionNode info 0 [])
                  else Nothing
        _ -> pure Nothing
  where
    etaArgumentVar argument =
      case argument of
        RuntimeVarExpr var -> Just var
        _ -> Nothing

lowerLet :: RuntimeBind -> RuntimeExpr -> LowerM GrinExpr
lowerLet bind body =
  case bind of
    RuntimeNonRec var rhs
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
                  loweredRhs <- withProvenance (varName var) (lowerExpr rhs)
                  pure (bindExpr vars loweredRhs loweredBody)
                else case discardedImmediateEval vars loweredBody of
                  Just (resultVars, rest) -> do
                    loweredRhs <- withProvenance (varName var) (lowerExpr rhs)
                    pure (bindExpr resultVars loweredRhs rest)
                  Nothing -> do
                    node <- withProvenance (varName var) (makeThunk rhs)
                    pure (bindExpr vars (GrinStore node) loweredBody)
      | otherwise -> do
          (vars, loweredBody) <- withFreshLocalVars [var] $ \groups -> do
            body' <- lowerExpr body
            pure (concat groups, body')
          loweredRhs <- withProvenance (varName var) (lowerExpr rhs)
          pure (bindExpr vars loweredRhs loweredBody)
    RuntimeRec bindings -> do
      withFreshLocalVars (map fst bindings) $ \groups -> do
        nodes <-
          mapM
            ( \(var, rhs) ->
                withProvenance (varName var) $
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

lowerAlt :: (RuntimeVar, [GrinVar]) -> RuntimeAlt -> LowerM GrinAlt
lowerAlt caseBinding alt = do
  (binders, rhs) <- withBindings [caseBinding] $ withFreshLocalVars (altBinders alt) $ \groups -> do
    rhs' <- lowerExpr (altRhs alt)
    pure (concat groups, rhs')
  alternativeConstructor <- lowerAltCon (altCon alt)
  pure
    GrinAlt
      { grinAltCon = alternativeConstructor,
        grinAltBinders = binders,
        grinAltRhs = rhs
      }

makeThunk :: RuntimeExpr -> LowerM GrinNode
makeThunk = makeThunkNamed Nothing

makeTopThunk :: RuntimeVar -> RuntimeExpr -> LowerM GrinNode
makeTopThunk var = makeThunkNamed (Just (FunctionName (varName var <> "_thunk")))

makeThunkNamed :: Maybe FunctionName -> RuntimeExpr -> LowerM GrinNode
makeThunkNamed requestedName expr
  | not (isLiftedRuntimeRep runtimeRep) =
      error ("GRIN lowering cannot suspend an expression with runtime representation " <> show runtimeRep)
  | otherwise = do
      captures <- capturesFor expr
      functionName <- maybe (freshFunction "thunk") freshNamedFunction requestedName
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

lowerStaticNode :: RuntimeExpr -> LowerM (Maybe GrinNode)
lowerStaticNode expr = do
  constructorArities <- gets lowerConstructorArities
  localVars <- gets lowerLocalVars
  case constructorApplication constructorArities (Map.keysSet localVars) expr of
    Just (constructor, arguments) -> do
      values <- mapM lowerStaticValues arguments
      let remaining = constructorArities Map.! constructor - length arguments
      pure (GrinNode (GrinConstructor constructor remaining) . concat <$> sequence values)
    Nothing -> pure Nothing

lowerStaticValues :: RuntimeExpr -> LowerM (Maybe [GrinValue])
lowerStaticValues expr =
  case expr of
    RuntimeLit literal _ -> pure (Just [GrinLitValue (lowerLiteral literal)])
    RuntimeVarExpr var
      | null (runtimeRepComponents (typeRuntimeRep (varType var))) -> pure (Just [])
      | otherwise -> do
          constructorArity <- lookupConstructorArity var
          isWhnfGlobal <- isWhnfGlobalVar var
          global <- lowerGlobalVar var
          localGlobalNames <- gets lowerLocalGlobalNames
          case constructorArity of
            Just 0 -> do
              noteExternalGlobalReference var
              pure (Just [GrinVarValue global])
            _
              | isWhnfGlobal,
                grinVarName global `Set.notMember` localGlobalNames -> do
                  noteExternalGlobalReference var
                  pure (Just [GrinVarValue global])
              | otherwise -> pure Nothing
    RuntimeTyApp inner _ -> lowerStaticValues inner
    _ -> pure Nothing

lowerStrict :: Text -> RuntimeExpr -> ([GrinValue] -> LowerM GrinExpr) -> LowerM GrinExpr
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
lowerOperand :: Text -> RuntimeExpr -> ([GrinValue] -> LowerM GrinExpr) -> LowerM GrinExpr
lowerOperand hint expr continuation = do
  direct <- lowerLazyDirectValues expr
  case direct of
    Just values -> continuation values
    Nothing -> do
      valueVars <- freshVars hint (exprRuntimeRep expr)
      valueExpr <- lowerExpr expr
      rest <- continuation (map GrinVarValue valueVars)
      pure (bindExpr valueVars valueExpr rest)

lowerSingleOperand :: Text -> RuntimeExpr -> (GrinValue -> LowerM GrinExpr) -> LowerM GrinExpr
lowerSingleOperand hint expr continuation =
  lowerOperand hint expr $ \case
    [value] -> continuation value
    _ -> error ("GRIN lowering expected one operand for " <> T.unpack hint)

-- | Produce the weak-head normal form required by an operation whose operand
-- is structural. GRIN operations never enter heap cells implicitly: a lifted
-- function or case scrutinee must pass through 'GrinEval' before use.
lowerSingleEvaluatedOperand :: Text -> RuntimeExpr -> (GrinValue -> LowerM GrinExpr) -> LowerM GrinExpr
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
-- every function entry explicit. The wrapper accepts the exception and runs
-- the IO action returned by the captured source handler, so the runtime can
-- resume it directly with the catch frame's parent continuation.
lowerCatchHandler :: RuntimeRep -> RuntimeExpr -> (GrinValue -> LowerM GrinExpr) -> LowerM GrinExpr
lowerCatchHandler catchResultRep handler continuation =
  lowerSingleArgument handler $ \handlerValue ->
    wrapCatchHandlerValue
      catchResultRep
      (exprRuntimeRep handler)
      (applicationResultRep handler)
      (functionArgumentRep handler)
      handlerValue
      continuation

wrapCatchHandlerValue :: RuntimeRep -> RuntimeRep -> RuntimeRep -> RuntimeRep -> GrinValue -> (GrinValue -> LowerM GrinExpr) -> LowerM GrinExpr
wrapCatchHandlerValue catchResultRep handlerRep actionRep exceptionRep handlerValue continuation = do
  capturedHandler <- freshVar "catch_handler" handlerRep
  exception <- freshVar "catch_exception" exceptionRep
  evaluatedHandler <- freshVar "catch_handler_whnf" handlerRep
  actionPointer <- freshVar "catch_handler_action" actionRep
  evaluatedAction <- freshVar "catch_handler_action_whnf" actionRep
  wrapperName <- freshFunction "catch_handler"
  emitFunction
    GrinFunction
      { grinFunctionName = wrapperName,
        grinFunctionLinkName = Nothing,
        grinFunctionParameters = [capturedHandler, exception],
        grinFunctionResultRep = catchResultRep,
        grinFunctionBody =
          bindExpr [evaluatedHandler] (GrinEval handlerRep (GrinVarValue capturedHandler)) $
            bindExpr
              [actionPointer]
              (GrinApply actionRep (GrinVarValue evaluatedHandler) [GrinVarValue exception])
              ( bindExpr
                  [evaluatedAction]
                  (GrinEval actionRep (GrinVarValue actionPointer))
                  (GrinApply catchResultRep (GrinVarValue evaluatedAction) [])
              )
      }
  wrapperPointer <- freshVar "catch_handler_wrapper" liftedRuntimeRep
  evaluatedWrapper <- freshVar "catch_handler_wrapper_whnf" liftedRuntimeRep
  rest <- continuation (GrinVarValue evaluatedWrapper)
  pure $
    bindExpr
      [wrapperPointer]
      (GrinStore (GrinNode (GrinClosure wrapperName [runtimeRepComponents exceptionRep]) [handlerValue]))
      (bindExpr [evaluatedWrapper] (GrinEval liftedRuntimeRep (GrinVarValue wrapperPointer)) rest)

lowerDelayed :: RuntimeExpr -> ([GrinValue] -> LowerM GrinExpr) -> LowerM GrinExpr
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

lowerArgument :: RuntimeExpr -> ([GrinValue] -> LowerM GrinExpr) -> LowerM GrinExpr
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

lowerSingleArgument :: RuntimeExpr -> (GrinValue -> LowerM GrinExpr) -> LowerM GrinExpr
lowerSingleArgument expr continuation =
  lowerArgument expr $ \case
    [value] -> continuation value
    _ -> error "GRIN lowering expected one argument value"

lowerArgumentMany :: [RuntimeExpr] -> ([GrinValue] -> LowerM GrinExpr) -> LowerM GrinExpr
lowerArgumentMany expressions continuation =
  case expressions of
    [] -> continuation []
    first : rest ->
      lowerArgument first $ \firstValues ->
        lowerArgumentMany rest $ \restValues ->
          continuation (firstValues <> restValues)

lowerStrictMany :: [RuntimeExpr] -> ([GrinValue] -> LowerM GrinExpr) -> LowerM GrinExpr
lowerStrictMany expressions continuation =
  case expressions of
    [] -> continuation []
    first : rest ->
      lowerStrict "foreign_argument" first $ \firstValues ->
        lowerStrictMany rest $ \restValues ->
          continuation (firstValues <> restValues)

freshVars :: Text -> RuntimeRep -> LowerM [GrinVar]
freshVars hint = mapM (freshVar hint) . runtimeRepComponents

unboxedTupleArguments :: RuntimeExpr -> LowerM (Maybe [RuntimeExpr])
unboxedTupleArguments expr = do
  tupleConstructors <- gets lowerUnboxedTupleConstructors
  case collectApplications expr of
    (RuntimeVarExpr constructor, arguments)
      | isKnownUnboxedTupleConstructor tupleConstructors constructor ->
          case exprRuntimeRep expr of
            TupleRep fieldReps
              | length arguments == length fieldReps -> pure (Just arguments)
            _ -> pure Nothing
    _ -> pure Nothing

isKnownUnboxedTupleConstructor :: Set Text -> RuntimeVar -> Bool
isKnownUnboxedTupleConstructor knownOrigins constructor =
  case varResolvedName constructor of
    Just origin
      | runtimeSymbolOriginText origin `Set.member` knownOrigins -> True
    Just (RuntimeTopLevelOrigin _ "GHC.Types" originName) -> unboxedTuplePunctuation originName
    Nothing -> unboxedTuplePunctuation (varName constructor)
    _ -> False

unboxedTuplePunctuation :: Text -> Bool
unboxedTuplePunctuation name =
  case T.stripPrefix "(#" name >>= T.stripSuffix "#)" of
    Just punctuation -> T.all (== ',') punctuation
    Nothing -> False

freshVar :: Text -> RuntimeRep -> LowerM GrinVar
freshVar hint runtimeRep = do
  unique <- gets lowerNextUnique
  modify' $ \state -> state {lowerNextUnique = unique + 1}
  pure (GrinVar ("$grin_" <> hint <> "_" <> T.pack (show unique)) unique runtimeRep)

freshTopVar :: Text -> LowerM GrinVar
freshTopVar linkedName = do
  unique <- gets lowerNextUnique
  modify' $ \state -> state {lowerNextUnique = unique + 1}
  pure (GrinVar linkedName unique liftedRuntimeRep)

freshFunction :: Text -> LowerM FunctionName
freshFunction kind = do
  index <- freshFunctionIndex
  provenance <- gets lowerCurrentProvenance
  pure
    ( FunctionName
        ( fromMaybe "$grin" provenance
            <> "_"
            <> kind
            <> "_"
            <> T.pack (show index)
        )
    )

-- Source-derived names still reserve a generated-name index so introducing a
-- readable name does not renumber every anonymous function that follows it.
freshNamedFunction :: FunctionName -> LowerM FunctionName
freshNamedFunction name = name <$ freshFunctionIndex

freshFunctionIndex :: LowerM Int
freshFunctionIndex = do
  index <- gets lowerNextFunction
  modify' $ \state -> state {lowerNextFunction = index + 1}
  pure index

emitFunction :: GrinFunction -> LowerM ()
emitFunction function = do
  body <- ensureWhnfResult function (grinFunctionBody function)
  modify' $ \state -> state {lowerFunctionsRev = function {grinFunctionBody = body} : lowerFunctionsRev state}

-- A closure with no remaining logical arguments is a saturated computation,
-- not a WHNF. Enter it at function exits while retaining closures in
-- intermediate positions where they encode delayed source-level application.
-- Zero-width arguments remain present as empty layouts and therefore do not
-- make a closure appear saturated.
ensureWhnfResult :: GrinFunction -> GrinExpr -> LowerM GrinExpr
ensureWhnfResult owner expr =
  case expr of
    GrinBind vars valueExpr body -> GrinBind vars valueExpr <$> ensureWhnfResult owner body
    GrinStore node@(GrinNode (GrinClosure functionName []) fields) -> do
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
      codeInfos <- gets lowerCodeInfosByName
      case [grinFunctionResultRep function | function <- localFunctions, grinFunctionName function == functionName]
        <> [grinCodeResultRep info | info <- Map.elems codeInfos, grinCodeFunctionName info == functionName] of
        resultRep : _ -> pure resultRep
        [] -> error ("GRIN lowering could not find saturated closure target " <> show functionName)

primitiveApplication :: Map Text Int -> Set (Text, Unique) -> RuntimeExpr -> Maybe (Text, [RuntimeExpr])
primitiveApplication primitiveArities localVars expr =
  case collectApplications expr of
    (RuntimeVarExpr var, arguments)
      | varKey var `Set.notMember` localVars,
        varName var `Map.member` primitiveArities ->
          Just (varName var, arguments)
    _ -> Nothing

constructorApplication :: Map Text Int -> Set (Text, Unique) -> RuntimeExpr -> Maybe (Text, [RuntimeExpr])
constructorApplication constructorArities localVars expr =
  case collectApplications expr of
    (RuntimeVarExpr var, arguments)
      | varKey var `Set.notMember` localVars,
        Just constructorName <- resolveConstructorName constructorArities var,
        Just arity <- Map.lookup constructorName constructorArities,
        arity > 0,
        length arguments <= arity ->
          Just (constructorName, arguments)
    _ -> Nothing

collectApplications :: RuntimeExpr -> (RuntimeExpr, [RuntimeExpr])
collectApplications expr =
  case expr of
    RuntimeApp function argument ->
      let (headExpr, arguments) = collectApplications function
       in (headExpr, arguments <> [argument])
    RuntimeTyApp inner _ -> collectApplications inner
    _ -> (expr, [])

freeVars :: RuntimeExpr -> Set RuntimeVar
freeVars expr =
  case expr of
    RuntimeVarExpr var -> Set.singleton var
    RuntimeLit {} -> Set.empty
    RuntimeApp function argument -> freeVars function <> freeVars argument
    RuntimeTyApp inner _ -> freeVars inner
    RuntimeLam var body -> Set.delete var (freeVars body)
    RuntimeTyLam _ body -> freeVars body
    RuntimeLet bind body -> freeVarsBind bind body
    RuntimeCase scrutinee binder alternatives ->
      freeVars scrutinee
        <> Set.delete binder (foldMap freeVarsAlt alternatives)
    RuntimeCallForeign _ arguments -> foldMap freeVars arguments

freeVarsBind :: RuntimeBind -> RuntimeExpr -> Set RuntimeVar
freeVarsBind bind body =
  case bind of
    RuntimeNonRec var rhs -> freeVars rhs <> Set.delete var (freeVars body)
    RuntimeRec bindings ->
      let binders = Set.fromList (map fst bindings)
          allFree = foldMap (freeVars . snd) bindings <> freeVars body
       in allFree `Set.difference` binders

freeVarsAlt :: RuntimeAlt -> Set RuntimeVar
freeVarsAlt alt =
  freeVars (altRhs alt) `Set.difference` Set.fromList (altBinders alt)

plainGlobalVar :: RuntimeVar -> GrinVar
plainGlobalVar var = GrinVar (varName var) (sourceUnique var) liftedRuntimeRep

lowerGlobalVar :: RuntimeVar -> LowerM GrinVar
lowerGlobalVar var = do
  globalNames <- gets lowerGlobalNames
  incremental <- gets lowerUseIncrementalCodeLookup
  constructorName <- lookupConstructorName var
  let sourceName = sourceLookupName incremental var
      lookupName = fromMaybe sourceName constructorName
      linkedName = Map.findWithDefault (varName var) lookupName globalNames
  pure (GrinVar linkedName (sourceUnique var) liftedRuntimeRep)

capturesFor :: RuntimeExpr -> LowerM [GrinVar]
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

isGlobalVar :: RuntimeVar -> LowerM Bool
isGlobalVar var = do
  localVars <- gets lowerLocalVars
  globalNames <- gets lowerGlobalNames
  tupleConstructors <- gets lowerUnboxedTupleConstructors
  incremental <- gets lowerUseIncrementalCodeLookup
  let sourceName = sourceLookupName incremental var
      constructorName = resolveConstructorName globalNames var
  pure
    ( varKey var `Map.notMember` localVars
        && (sourceName `Map.member` globalNames || maybe False (`Map.member` globalNames) constructorName || isKnownUnboxedTupleConstructor tupleConstructors var)
    )

-- | Values that can be embedded directly in a non-allocating GRIN operation.
-- Dynamic constructors, closures, and primitives are deliberately excluded:
-- they must be introduced by 'GrinStore'.
lowerDirectValues :: RuntimeExpr -> LowerM (Maybe [GrinValue])
lowerDirectValues expr =
  case expr of
    RuntimeVarExpr var -> do
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
              global <- lowerGlobalVar var
              pure (Just [GrinVarValue global])
          | not isGlobal && runtimeRep /= liftedRuntimeRep ->
              Just . map GrinVarValue <$> lookupLocalVars var
          | otherwise -> pure Nothing
    RuntimeLit literal _ -> pure (Just [GrinLitValue (lowerLiteral literal)])
    RuntimeTyApp inner _ -> lowerDirectValues inner
    _ -> pure Nothing

-- | Values suitable for a non-strict position. Unlike 'lowerDirectValues', a
-- lifted variable is returned as its existing pointer without being forced or
-- wrapped in another thunk.
lowerLazyDirectValues :: RuntimeExpr -> LowerM (Maybe [GrinValue])
lowerLazyDirectValues expr =
  case expr of
    RuntimeVarExpr var -> do
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
    RuntimeLit literal _ -> pure (Just [GrinLitValue (lowerLiteral literal)])
    RuntimeTyApp inner _ -> lowerLazyDirectValues inner
    _ -> pure Nothing

lookupRuntimeVars :: RuntimeVar -> LowerM [GrinVar]
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
            globalVar <- lowerGlobalVar var
            pure [globalVar]
          _ -> error "GRIN lowering found a global with a multi-value runtime representation"
        else lookupLocalVars var

lookupRuntimeVar :: RuntimeVar -> LowerM GrinVar
lookupRuntimeVar var =
  lookupRuntimeVars var >>= \case
    [runtimeVar] -> pure runtimeVar
    _ -> error "GRIN lowering expected one lifted runtime variable"

noteExternalGlobalReference :: RuntimeVar -> LowerM ()
noteExternalGlobalReference var = do
  localGlobalNames <- gets lowerLocalGlobalNames
  global <- lowerGlobalVar var
  if grinVarName global `Set.member` localGlobalNames
    then pure ()
    else modify' $ \state ->
      state
        { lowerReferencedExternalGlobalNames =
            Set.insert (grinVarName global) (lowerReferencedExternalGlobalNames state)
        }

lookupAliasVars :: RuntimeExpr -> LowerM (Maybe [GrinVar])
lookupAliasVars expr =
  case expr of
    RuntimeVarExpr var -> do
      codeInfo <- lookupCodeInfo var
      case codeInfo of
        Just _ -> pure Nothing
        Nothing -> Just <$> lookupRuntimeVars var
    RuntimeTyApp inner _ -> lookupAliasVars inner
    _ -> pure Nothing

lookupCodeInfo :: RuntimeVar -> LowerM (Maybe GrinCodeInfo)
lookupCodeInfo var = do
  locals <- gets lowerLocalVars
  localCodeInfosByUnique <- gets lowerLocalCodeInfosByUnique
  codeInfosByName <- gets lowerCodeInfosByName
  localCodeLinkNames <- gets lowerLocalCodeLinkNames
  incremental <- gets lowerUseIncrementalCodeLookup
  let localInfo =
        case varResolvedName var of
          Nothing -> Map.lookup (varUnique var) localCodeInfosByUnique >>= lookup (varType var)
          Just _ -> Nothing
      sourceName = sourceLookupName incremental var
      sourceInfo = Map.lookup sourceName codeInfosByName
      selected = if incremental then localInfo <|> sourceInfo else sourceInfo
  if varKey var `Map.member` locals
    then pure Nothing
    else case selected of
      Nothing -> pure Nothing
      Just info -> do
        if grinCodeSourceName info `Set.member` localCodeLinkNames
          then pure ()
          else modify' $ \state ->
            state
              { lowerReferencedExternalCodeLinkNames =
                  Set.insert (grinCodeSourceName info) (lowerReferencedExternalCodeLinkNames state)
              }
        pure (Just info)

lookupConstructorArity :: RuntimeVar -> LowerM (Maybe Int)
lookupConstructorArity var = do
  constructorName <- lookupConstructorName var
  constructorArities <- gets lowerConstructorArities
  pure (constructorName >>= (`Map.lookup` constructorArities))

lookupConstructorName :: RuntimeVar -> LowerM (Maybe Text)
lookupConstructorName var = do
  locals <- gets lowerLocalVars
  constructorArities <- gets lowerConstructorArities
  pure
    ( if varKey var `Map.member` locals
        then Nothing
        else resolveConstructorName constructorArities var
    )

lookupPrimitiveArity :: RuntimeVar -> LowerM (Maybe Int)
lookupPrimitiveArity var = do
  locals <- gets lowerLocalVars
  primitiveArities <- gets lowerPrimitiveArities
  pure
    ( if varKey var `Map.member` locals
        then Nothing
        else Map.lookup (varName var) primitiveArities
    )

knownSaturatedApplication :: RuntimeExpr -> LowerM (Maybe (GrinCodeInfo, [RuntimeExpr]))
knownSaturatedApplication expr =
  case collectApplications expr of
    (RuntimeVarExpr var, arguments) -> do
      codeInfo <- lookupCodeInfo var
      pure $ do
        info <- codeInfo
        if length arguments == length (grinCodeParameterLayouts info)
          && isLiftedRuntimeRep (grinCodeResultRep info)
          then Just (info, arguments)
          else Nothing
    _ -> pure Nothing

isWhnfExpr :: RuntimeExpr -> LowerM Bool
isWhnfExpr expr =
  case expr of
    RuntimeLam {} -> pure True
    RuntimeTyLam _ body -> isWhnfExpr body
    RuntimeLit literal _ -> pure (isLiftedRuntimeRep (literalRuntimeRep literal))
    RuntimeVarExpr var -> do
      codeInfo <- lookupCodeInfo var
      primitiveArity <- lookupPrimitiveArity var
      constructorArity <- lookupConstructorArity var
      pure
        ( case codeInfo of
            Just info ->
              not (null (grinCodeParameterLayouts info))
                || grinCodeResultRep info /= exprRuntimeRep expr
            Nothing -> maybe False (> 0) primitiveArity || maybe False (> 0) constructorArity
        )
    RuntimeApp {} -> do
      constructorArities <- gets lowerConstructorArities
      localVars <- gets lowerLocalVars
      case constructorApplication constructorArities (Map.keysSet localVars) expr of
        Just _ -> pure True
        Nothing ->
          case collectApplications expr of
            (RuntimeVarExpr var, arguments) -> do
              codeInfo <- lookupCodeInfo var
              pure $ case codeInfo of
                Just info ->
                  length arguments < length (grinCodeParameterLayouts info)
                    || grinCodeResultRep info /= exprRuntimeRep expr
                Nothing -> False
            _ -> pure False
    _ -> pure False

isWhnfGlobalVar :: RuntimeVar -> LowerM Bool
isWhnfGlobalVar var = do
  localVars <- gets lowerLocalVars
  whnfGlobalNames <- gets lowerWhnfGlobalNames
  incremental <- gets lowerUseIncrementalCodeLookup
  let sourceName = sourceLookupName incremental var
      constructorName = resolveConstructorName whnfGlobalNames var
  pure (varKey var `Map.notMember` localVars && (sourceName `Map.member` whnfGlobalNames || maybe False (`Map.member` whnfGlobalNames) constructorName))

-- Builtin origins are syntax-level provenance, not linker namespaces. Their
-- runtime definitions keep the established constructor and primitive names,
-- while package symbols use their fully qualified origin during incremental
-- lowering.
sourceLookupName :: Bool -> RuntimeVar -> Text
sourceLookupName incremental var =
  case varResolvedName var of
    Just origin | incremental -> runtimeSymbolOriginText origin
    _ -> varName var

constructorLookupName :: RuntimeVar -> Text
constructorLookupName var =
  case varResolvedName var of
    Just origin -> runtimeSymbolOriginText origin
    Nothing -> varName var

runtimeConstructorName :: RuntimeSymbolOrigin -> Text
runtimeConstructorName = runtimeSymbolOriginText

resolveConstructorName :: Map Text value -> RuntimeVar -> Maybe Text
resolveConstructorName constructors var =
  case varResolvedName var of
    Just RuntimeTopLevelOrigin {} ->
      let name = constructorLookupName var
       in if name `Map.member` constructors then Just name else Nothing
    _ ->
      if varName var `Map.member` constructors
        then Just (varName var)
        else case [name | name <- Map.keys constructors, ("." <> varName var) `T.isSuffixOf` name] of
          [name] -> Just name
          _ -> Nothing

withFreshLocalVars :: [RuntimeVar] -> ([[GrinVar]] -> LowerM a) -> LowerM a
withFreshLocalVars vars action = do
  groups <- mapM binderVars vars
  withBindings (zip vars groups) (action groups)

withBindings :: [(RuntimeVar, [GrinVar])] -> LowerM a -> LowerM a
withBindings bindings action = do
  previous <- gets lowerLocalVars
  let locals = Map.fromList [(varKey var, values) | (var, values) <- bindings]
  modify' $ \state -> state {lowerLocalVars = locals <> previous}
  result <- action
  modify' $ \state -> state {lowerLocalVars = previous}
  pure result

withProvenance :: Text -> LowerM a -> LowerM a
withProvenance provenance action = do
  previous <- gets lowerCurrentProvenance
  modify' $ \state -> state {lowerCurrentProvenance = Just provenance}
  result <- action
  modify' $ \state -> state {lowerCurrentProvenance = previous}
  pure result

binderVars :: RuntimeVar -> LowerM [GrinVar]
binderVars var =
  case runtimeRepComponents (typeRuntimeRep (varType var)) of
    [] -> pure []
    [runtimeRep] -> pure [GrinVar (varName var) (sourceUnique var) runtimeRep]
    runtimeReps ->
      sequence
        [ freshVar (varName var <> "_" <> T.pack (show index)) runtimeRep
        | (index, runtimeRep) <- zip [0 :: Int ..] runtimeReps
        ]

lookupLocalVars :: RuntimeVar -> LowerM [GrinVar]
lookupLocalVars var = do
  locals <- gets lowerLocalVars
  case Map.lookup (varKey var) locals of
    Just values -> pure values
    Nothing -> error ("GRIN lowering lost local binding for " <> T.unpack (varName var) <> " (origin " <> show (varResolvedName var) <> ")")

varKey :: RuntimeVar -> (Text, Unique)
varKey var = (varName var, varUnique var)

sourceUnique :: RuntimeVar -> Int
sourceUnique var =
  case varUnique var of
    Unique unique -> unique

lowerLiteral :: Literal -> GrinLiteral
lowerLiteral literal =
  case literal of
    LitInt runtimeRep value -> GrinLitInt runtimeRep value
    LitChar runtimeRep value -> GrinLitChar runtimeRep value
    LitString value -> GrinLitString value
    LitAddr value -> GrinLitAddr value

lowerAltCon :: RuntimeAltCon -> LowerM GrinAltCon
lowerAltCon altCon =
  case altCon of
    DataAlt origin -> do
      constructorArities <- gets lowerConstructorArities
      let symbolOrigin = runtimeConstructorSymbolOrigin origin
          occurrence = (RuntimeVar (runtimeConstructorSourceName origin) (Unique (-1)) (runtimeType liftedRuntimeRep)) {varResolvedName = Just symbolOrigin}
          constructorName = fromMaybe (runtimeConstructorName symbolOrigin) (resolveConstructorName constructorArities occurrence)
      pure (GrinDataAlt constructorName)
    LitAlt literal _ -> pure (GrinLitAlt (lowerLiteral literal))
    DefaultAlt -> pure GrinDefaultAlt

lowerForeignCall :: RuntimeForeignCall -> GrinForeignCall
lowerForeignCall foreignCall =
  GrinForeignCall
    { grinForeignCallName = runtimeForeignCallName foreignCall,
      grinForeignCallSymbol = runtimeForeignCallSymbol foreignCall,
      grinForeignCallSignature = lowerForeignSignature (runtimeForeignCallSignature foreignCall)
    }

lowerForeignSignature :: RuntimeForeignSignature -> GrinForeignSignature
lowerForeignSignature signature =
  GrinForeignSignature
    { grinForeignArgumentTypes = map lowerForeignType (runtimeForeignArgumentTypes signature),
      grinForeignResultType = lowerForeignType (runtimeForeignResultType signature),
      grinForeignEffect = lowerForeignEffect (runtimeForeignEffect signature)
    }

lowerForeignEffect :: RuntimeForeignEffect -> GrinForeignEffect
lowerForeignEffect effect =
  case effect of
    RuntimeForeignPure -> GrinForeignPure
    RuntimeForeignRealWorld -> GrinForeignRealWorld

lowerForeignType :: RuntimeForeignType -> GrinForeignType
lowerForeignType foreignType =
  case foreignType of
    RuntimeForeignInt -> GrinForeignInt
    RuntimeForeignInt32 -> GrinForeignInt32
    RuntimeForeignWord64 -> GrinForeignWord64
    RuntimeForeignAddr -> GrinForeignAddr

exprRuntimeRep :: RuntimeExpr -> RuntimeRep
exprRuntimeRep expr =
  case expr of
    RuntimeLit literal _ -> literalRuntimeRep literal
    RuntimeLam {} -> liftedRuntimeRep
    RuntimeTyLam {} -> liftedRuntimeRep
    _ ->
      case exprType expr of
        Just ty -> typeRuntimeRep ty
        Nothing -> error ("GRIN lowering could not determine expression type: " <> show expr)

exprType :: RuntimeExpr -> Maybe RuntimeType
exprType expr =
  case expr of
    RuntimeVarExpr var -> Just (varType var)
    RuntimeLit _ ty -> Just ty
    RuntimeApp function _ -> functionResultType =<< exprType function
    RuntimeTyApp function argument -> do
      functionType <- exprType function
      case functionType of
        RuntimeForAll binder body -> Just (substRuntimeType binder argument body)
        _ -> Just functionType
    RuntimeLam var body -> RuntimeFunction (varType var) <$> exprType body
    RuntimeTyLam tyVar body -> RuntimeForAll tyVar <$> exprType body
    RuntimeLet _ body -> exprType body
    RuntimeCase _ _ alternatives ->
      case alternatives of
        first : _ -> exprType (altRhs first)
        [] -> Nothing
    RuntimeCallForeign foreignCall _arguments ->
      Just (runtimeForeignCallResultType (runtimeForeignCallSignature foreignCall))

-- A default class method can be specialized to its class constructor before
-- its method type variables, then applied to the instance dictionary. Preserve
-- those still-polymorphic binders while consuming the runtime argument.
functionResultType :: RuntimeType -> Maybe RuntimeType
functionResultType functionType =
  case functionType of
    RuntimeFunction _ result -> Just result
    RuntimeForAll tyVar body -> RuntimeForAll tyVar <$> functionResultType body
    _ -> Nothing

typeRuntimeRep :: RuntimeType -> RuntimeRep
typeRuntimeRep ty =
  case runtimeRepOfRuntimeType ty of
    Right runtimeRep -> runtimeRep
    Left problem -> error ("GRIN lowering received a non-runtime type: " <> problem)

applicationResultRep :: RuntimeExpr -> RuntimeRep
applicationResultRep function =
  case exprType function >>= functionResultType of
    Just result -> typeRuntimeRep result
    Nothing -> error ("GRIN lowering could not determine application result type: " <> show function)

functionArgumentRep :: RuntimeExpr -> RuntimeRep
functionArgumentRep function =
  case exprType function >>= functionArgumentType of
    Just argument -> typeRuntimeRep argument
    Nothing -> error ("GRIN lowering could not determine function argument type: " <> show function)
  where
    functionArgumentType functionType =
      case functionType of
        RuntimeFunction argument _ -> Just argument
        RuntimeForAll _ body -> functionArgumentType body
        _ -> Nothing

programVars :: RuntimeProgram -> [RuntimeVar]
programVars program = concatMap topVars (runtimeTopBinds program)

programConstructors :: RuntimeProgram -> [(Text, Int)]
programConstructors program =
  [ (runtimeConstructorName (runtimeConstructorSymbolOrigin (runtimeDataConOrigin constructor)), length (runtimeDataConFields constructor))
  | RuntimeData declaration <- runtimeTopBinds program,
    not (isUnboxedTupleData declaration),
    constructor <- runtimeDataConstructors declaration
  ]

isUnboxedTupleData :: RuntimeDataDecl -> Bool
isUnboxedTupleData = runtimeDataIsUnboxedTuple

programGlobalInfos :: GrinLinkNames -> RuntimeProgram -> [(RuntimeVar, Text, Text, Bool)]
programGlobalInfos linkNames program = concat (snd (mapAccumL buildInfo Map.empty bindings))
  where
    constructorArities =
      Map.fromList [(name, length layouts) | (name, layouts) <- builtinConstructors]
        <> Map.fromList (programConstructors program)
    bindings = [(var, expr) | RuntimeTopValue bind <- runtimeTopBinds program, (var, expr) <- topBindings bind]
    buildInfo occurrences (var, expr) =
      let (occurrences', index, linkedName) = linkNameAt linkNames occurrences var
          sourceName = sourceNameAt linkNames index var
       in (occurrences', [(var, sourceName, linkedName, isStaticWhnf constructorArities expr) | not (isDirectFunction expr)])
    topBindings bind =
      case bind of
        RuntimeNonRec var expr -> [(var, expr)]
        RuntimeRec recursiveBindings -> recursiveBindings

programCodeInfos :: GrinLinkNames -> RuntimeProgram -> [(RuntimeVar, Text, GrinCodeInfo)]
programCodeInfos linkNames program = concat (snd (mapAccumL buildInfo Map.empty bindings))
  where
    bindings = [(var, expr) | RuntimeTopValue bind <- runtimeTopBinds program, (var, expr) <- topBindings bind]
    buildInfo occurrences (var, expr) =
      let (occurrences', index, linkedName) = linkNameAt linkNames occurrences var
          sourceName = sourceNameAt linkNames index var
       in (occurrences', [(var, sourceName, codeInfoFor linkedName var expr) | isDirectFunction expr])
    topBindings bind =
      case bind of
        RuntimeNonRec var expr -> [(var, expr)]
        RuntimeRec recursiveBindings -> recursiveBindings

codeInfoFor :: Text -> RuntimeVar -> RuntimeExpr -> GrinCodeInfo
codeInfoFor linkedName _var expr =
  GrinCodeInfo
    { grinCodeSourceName = linkedName,
      grinCodeFunctionName = linkedFunctionName linkedName,
      grinCodeParameterLayouts =
        [ runtimeRepComponents (typeRuntimeRep (varType binder))
        | binder <- binders
        ],
      grinCodeResultRep = exprRuntimeRep body
    }
  where
    (binders, body) = collectLeadingLambdas expr

linkedFunctionName :: Text -> FunctionName
linkedFunctionName linkedName = FunctionName ("$entry$" <> linkedName)

nextLinkName :: RuntimeVar -> LowerM Text
nextLinkName var = do
  names <- gets lowerLinkNames
  occurrences <- gets lowerLinkNameOccurrences
  let (occurrences', _, linkedName) = linkNameAt names occurrences var
  modify' (\state -> state {lowerLinkNameOccurrences = occurrences'})
  pure linkedName

linkNameAt :: GrinLinkNames -> Map Unique Int -> RuntimeVar -> (Map Unique Int, Int, Text)
linkNameAt names occurrences var =
  (Map.insert unique (index + 1) occurrences, index, linkedName)
  where
    unique = varUnique var
    index = Map.findWithDefault 0 unique occurrences
    linkedName = fromMaybe (varName var) (Map.lookup unique (grinNativeLinkNames names) >>= atIndex index)

sourceNameAt :: GrinLinkNames -> Int -> RuntimeVar -> Text
sourceNameAt names index var =
  fromMaybe (varName var) (Map.lookup (varUnique var) (grinSourceLinkNames names) >>= atIndex index)

atIndex :: Int -> [a] -> Maybe a
atIndex position values = case drop position values of
  value : _ -> Just value
  [] -> Nothing

grinLinkNamesEmpty :: GrinLinkNames -> Bool
grinLinkNamesEmpty = Map.null . grinNativeLinkNames

collectLeadingLambdas :: RuntimeExpr -> ([RuntimeVar], RuntimeExpr)
collectLeadingLambdas expr =
  case expr of
    RuntimeLam binder body ->
      let (binders, result) = collectLeadingLambdas body
       in (binder : binders, result)
    RuntimeTyLam _ body -> collectLeadingLambdas body
    RuntimeLet bind@(RuntimeNonRec _ rhs) body
      | isRuntimeAliasExpression rhs ->
          case collectLeadingLambdas body of
            ([], _) -> ([], expr)
            (binders, result) -> (binders, RuntimeLet bind result)
    _ -> ([], expr)

-- Moving an allocation-free alias into the body of a following lambda exposes
-- the lambda to closure conversion without duplicating work or changing
-- sharing. Keep the let itself so its binder continues to carry the cast's
-- result type; GRIN deliberately does not reconstruct that type from coercion
-- axioms.
isRuntimeAliasExpression :: RuntimeExpr -> Bool
isRuntimeAliasExpression expression =
  case expression of
    RuntimeVarExpr {} -> True
    RuntimeTyApp inner _ -> isRuntimeAliasExpression inner
    _ -> False

isStaticWhnf :: Map Text Int -> RuntimeExpr -> Bool
isStaticWhnf constructorArities expr =
  case constructorApplication constructorArities Set.empty expr of
    Just (_, arguments) -> all isStaticArgument arguments
    Nothing -> False
  where
    isStaticArgument argument =
      case argument of
        RuntimeLit {} -> True
        RuntimeVarExpr var ->
          null (runtimeRepComponents (typeRuntimeRep (varType var)))
            || Map.lookup (varName var) constructorArities == Just 0
        RuntimeTyApp inner _ -> isStaticArgument inner
        _ -> False

topVars :: RuntimeTopBind -> [RuntimeVar]
topVars topBind =
  case topBind of
    RuntimeData {} -> []
    RuntimePrimitive var _ -> [var]
    RuntimeForeignImport {} -> []
    RuntimeTopValue bind -> bindVars bind

bindVars :: RuntimeBind -> [RuntimeVar]
bindVars bind =
  case bind of
    RuntimeNonRec var expr -> var : exprVars expr
    RuntimeRec bindings -> concatMap (\(var, expr) -> var : exprVars expr) bindings

exprVars :: RuntimeExpr -> [RuntimeVar]
exprVars expr =
  case expr of
    RuntimeVarExpr var -> [var]
    RuntimeLit {} -> []
    RuntimeApp function argument -> exprVars function <> exprVars argument
    RuntimeTyApp inner _ -> exprVars inner
    RuntimeLam var body -> var : exprVars body
    RuntimeTyLam _ body -> exprVars body
    RuntimeLet bind body -> bindVars bind <> exprVars body
    RuntimeCase scrutinee binder alternatives ->
      exprVars scrutinee <> (binder : concatMap altVars alternatives)
    RuntimeCallForeign _ arguments -> concatMap exprVars arguments

altVars :: RuntimeAlt -> [RuntimeVar]
altVars alt = grinAltBinders' <> exprVars (altRhs alt)
  where
    grinAltBinders' = altBinders alt
