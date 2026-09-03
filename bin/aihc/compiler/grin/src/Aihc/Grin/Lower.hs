{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Conservative lowering from System FC to GRIN.
module Aihc.Grin.Lower
  ( lowerProgram,
  )
where

import Aihc.Fc qualified as Fc
import Aihc.Fc.TypeOf qualified as TypeOf
import Aihc.Fc.Wired qualified as Wired
import Aihc.Grin.Anf (normalizeGrinProgram)
import Aihc.Grin.Syntax
import Aihc.Grin.Tidy (tidyGrinProgram)
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Types (Unique (..))
import Control.Applicative ((<|>))
import Control.Monad (zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, mapStateT, modify', runStateT)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)

data LowerEnv = LowerEnv
  { lowerTypes :: !TypeOf.TypeEnv,
    lowerLocals :: !(Map Fc.Name [GrinVar]),
    lowerTypeSubstitution :: !(Map Fc.Name Fc.Type),
    lowerGlobalNames :: !(Map Fc.Name Text),
    lowerConstructorArities :: !(Map Fc.Name Int),
    lowerLocalFunctions :: !(Map Fc.Name (Int, Maybe (FunctionName, GrinRep)))
  }

data LowerState = LowerState
  { lowerNextUnique :: !Int,
    lowerNextFunction :: !Int,
    lowerFunctionsRev :: ![GrinFunction]
  }

type LowerM = StateT LowerState (Either String)

data TopParts = TopParts
  { topConstructors :: ![(Text, [[GrinRep]])],
    topPrimitives :: ![(GrinVar, Int)],
    topForeignCalls :: ![GrinForeignCall],
    topGlobals :: ![(Text, GrinNode)]
  }

instance Semigroup TopParts where
  left <> right =
    TopParts
      { topConstructors = topConstructors left <> topConstructors right,
        topPrimitives = topPrimitives left <> topPrimitives right,
        topForeignCalls = topForeignCalls left <> topForeignCalls right,
        topGlobals = topGlobals left <> topGlobals right
      }

instance Monoid TopParts where
  mempty = TopParts [] [] [] []

lowerProgram :: Fc.Program -> Either String GrinProgram
lowerProgram program = do
  primPackage <- maybe (Left "System FC program needs a GHC.Types scope") Right (Wired.primPackageFromScopes (Fc.programScopes program))
  let types = TypeOf.typeEnvFromProgram primPackage program
      globals = globalNameTable types
      constructorArities = constructorArityTable types
      localFunctionArities = Map.map (,Nothing) (localFunctionArityTable program)
      baseEnv = LowerEnv types Map.empty Map.empty globals constructorArities localFunctionArities
      initialState = LowerState (-1000000000) 0 []
  (discoveryParts, discoveryState) <- runStateT (mconcat <$> mapM (lowerDecl baseEnv) (Fc.programDecls program)) initialState
  localFunctions <- localFunctionTable baseEnv program discoveryParts discoveryState
  let env = baseEnv {lowerLocalFunctions = localFunctions}
  (parts, finalState) <- runStateT (mconcat <$> mapM (lowerDecl env) (Fc.programDecls program)) initialState
  pure
    ( tidyGrinProgram
        ( normalizeGrinProgram
            GrinProgram
              { grinConstructors = topConstructors parts,
                grinPrimitives = topPrimitives parts,
                grinForeignCalls = topForeignCalls parts,
                grinGlobals = topGlobals parts,
                grinFunctions = reverse (lowerFunctionsRev finalState)
              }
        )
    )

lowerDecl :: LowerEnv -> Fc.Decl -> LowerM TopParts
lowerDecl env declaration =
  case declaration of
    Fc.DeclType value -> lowerTypeDecl env value
    Fc.DeclVal value -> withLowerContext ("value " <> show (Fc.valName value)) (lowerValueDecl env value)
    Fc.DeclForeignImport value -> lowerForeignDecl env value
    Fc.DeclSynonym {} -> pure mempty
    Fc.DeclAxiom {} -> pure mempty

withLowerContext :: String -> LowerM a -> LowerM a
withLowerContext context =
  mapStateT (either (Left . ((context <> ": ") <>)) Right)

lowerTypeDecl :: LowerEnv -> Fc.TypeDecl -> LowerM TopParts
lowerTypeDecl env declaration = do
  converted <- mapM lowerConstructor (Fc.typeCons declaration)
  pure
    mempty
      { topConstructors = concatMap first converted,
        topGlobals = concatMap second converted
      }
  where
    first (constructors, _) = constructors
    second (_, globals) = globals
    lowerConstructor constructor = do
      let name = Fc.conName constructor
          (typeBinders, monotype) = splitForAlls (applySubstitution env (Fc.conType constructor))
          constructorEnv = foldl extendTypeBinder env typeBinders
      if "(#" `T.isPrefixOf` Fc.nameText name
        then pure ([], [])
        else do
          fieldTypes <- liftEither (constructorArgumentTypes monotype)
          fieldLayouts <- mapM (liftEither . runtimeComponents constructorEnv) fieldTypes
          resultType <- liftEither (constructorResultType monotype)
          resultRep <- liftEither (runtimeRep constructorEnv resultType)
          case resultRep of
            TupleRep {} -> pure ([], [])
            _ -> do
              globalName <- lookupGlobalName env name
              let tag = constructorTag name
              pure ([(tag, fieldLayouts)], [(globalName, GrinNode (GrinConstructor tag (length fieldTypes)) [])])

lowerValueDecl :: LowerEnv -> Fc.ValDecl -> LowerM TopParts
lowerValueDecl env declaration = do
  representation <- liftEither (runtimeRep env (Fc.valType declaration))
  if representation /= liftedGrinRep
    then throwLower ("GRIN does not support an unlifted top-level value: " <> show (Fc.valName declaration))
    else do
      globalName <- lookupGlobalName env (Fc.valName declaration)
      node <-
        if isFunctionExpression (Fc.valBody declaration)
          then makeClosure env (Fc.valBody declaration)
          else makeThunk env (Fc.nameText (Fc.valName declaration)) (Fc.valBody declaration)
      pure mempty {topGlobals = [(globalName, node)]}

isFunctionExpression :: Fc.Expr -> Bool
isFunctionExpression = (> 0) . functionArity

functionArity :: Fc.Expr -> Int
functionArity expression =
  case expression of
    Fc.ExLam _ body -> 1 + functionArity body
    Fc.ExTyLam _ body -> functionArity body
    _ -> 0

lowerForeignDecl :: LowerEnv -> Fc.ForeignImportDecl -> LowerM TopParts
lowerForeignDecl env declaration = do
  let name = Fc.foreignImportName declaration
      sourceType = applySubstitution env (Fc.foreignImportType declaration)
      (typeBinders, monotype) = splitForAlls sourceType
      foreignEnv = defaultRuntimeReps (foldl extendTypeBinder env typeBinders) typeBinders
  axioms <- foreignAxiomDeclarations foreignEnv declaration
  let constructors = foreignConstructorNames declaration
  (argumentTypes, resultType) <- splitOperationalFunctionType foreignEnv axioms monotype
  argumentGroups <-
    mapM
      (\(index, argumentType) -> freshVarsForType foreignEnv ("foreign_argument_" <> T.pack (show index), argumentType))
      (zip [0 :: Int ..] argumentTypes)
  resultRep <- liftEither (runtimeRep foreignEnv resultType)
  functionName <- freshFunction (Fc.nameText name <> "_foreign")
  globalName <- lookupGlobalName env name
  let parameters = concat argumentGroups
      layouts = map (map grinVarRuntimeRep) argumentGroups
      valueGroups = map (map GrinVarValue) argumentGroups
      arity = length argumentTypes
  (body, primitives, foreignCalls) <-
    case Fc.foreignImportCallingConvention declaration of
      Fc.Prim -> do
        expression <- lowerPrimitiveBody resultRep (Fc.nameText name) valueGroups
        let primitive =
              [ (GrinVar (Fc.nameText name) (-2000000000 + arity) resultRep, arity)
              | Fc.nameText name `notElem` compilerPrimitives
              ]
        pure (expression, primitive, [])
      Fc.CCall specification -> do
        let foreignCall = lowerForeignCall name specification
        (expression, adapterPrimitives) <- lowerForeignBody foreignEnv axioms constructors foreignCall argumentTypes valueGroups resultType
        pure (expression, adapterPrimitives, [foreignCall])
  emitFunction
    GrinFunction
      { grinFunctionName = functionName,
        grinFunctionParameters = parameters,
        grinFunctionResultRep = resultRep,
        grinFunctionBody = body
      }
  pure
    mempty
      { topPrimitives = primitives,
        topForeignCalls = foreignCalls,
        topGlobals = [(globalName, GrinNode (GrinClosure functionName layouts) [])]
      }

foreignAxiomDeclarations :: LowerEnv -> Fc.ForeignImportDecl -> LowerM [Fc.AxiomDecl]
foreignAxiomDeclarations env declaration =
  mapM lookupAxiom [name | Fc.ForeignAxiom name <- Fc.foreignImportDependencies declaration]
  where
    lookupAxiom name =
      case Map.lookup name (TypeOf.teAxioms (lowerTypes env)) of
        Just axiom -> pure axiom
        Nothing -> throwLower ("GRIN cannot find an explicit foreign axiom: " <> show name)

foreignConstructorNames :: Fc.ForeignImportDecl -> [Fc.Name]
foreignConstructorNames declaration =
  [name | Fc.ForeignConstructor name <- Fc.foreignImportDependencies declaration]

compilerPrimitives :: [Text]
compilerPrimitives = ["aihcExit#", "unsafeCoerce#", "raise#", "catch#"]

lowerPrimitiveBody :: GrinRep -> Text -> [[GrinValue]] -> LowerM GrinExpr
lowerPrimitiveBody resultRep name valueGroups =
  case (name, valueGroups) of
    ("aihcExit#", (status : _) : _) -> pure (GrinExit status)
    ("unsafeCoerce#", values : _) -> pure (GrinConstant values)
    ("raise#", (exception : _) : _) -> pure (GrinThrow exception)
    ("catch#", (action : _) : (handler : _) : state) ->
      lowerCatch resultRep action handler (concat state)
    _ -> pure (GrinPrimitiveCall resultRep name (concat valueGroups))

-- | Lower a foreign call body. The result also lists the primitives that
-- the argument adapters use, so the module declares them.
lowerForeignBody :: LowerEnv -> [Fc.AxiomDecl] -> [Fc.Name] -> GrinForeignCall -> [Fc.Type] -> [[GrinValue]] -> Fc.Type -> LowerM (GrinExpr, [(GrinVar, Int)])
lowerForeignBody env axioms constructors foreignCall argumentTypes valueGroups resultType = do
  operands <- concat <$> zipWithM (sourceValues env) argumentTypes valueGroups
  resultValues <- sourceValueTypes env resultType
  let signature = grinForeignCallSignature foreignCall
      expectedOperands = grinForeignOperandReps signature
      resultReps = grinForeignCallResultReps signature
      adapterPrimitives =
        [ (GrinVar byteArrayContentsPrimitive (-2000000000 + 1) AddrRep, 1)
        | any (\((_, value), expectedRep) -> isByteArrayOperand value expectedRep) (zip operands expectedOperands)
        ]
  if length operands /= length expectedOperands
    then throwLower ("GRIN foreign source arguments do not match the C ABI: " <> T.unpack (grinForeignCallName foreignCall))
    else case (resultValues, resultReps) of
      ([(resultValueType, resultValueRep)], [foreignResultRep]) -> do
        expression <-
          adaptForeignOperands env axioms constructors (zip operands expectedOperands) $ \values ->
            adaptForeignResult env axioms constructors resultValueType resultValueRep foreignResultRep (GrinForeignCallExpr foreignCall values)
        pure (expression, adapterPrimitives)
      _ -> throwLower ("GRIN foreign result does not match the C ABI: " <> T.unpack (grinForeignCallName foreignCall))

-- | The primitive that gives the payload address of a byte array.
byteArrayContentsPrimitive :: Text
byteArrayContentsPrimitive = "byteArrayContents#"

-- | A byte array value that a foreign call receives as an address.
isByteArrayOperand :: GrinValue -> GrinRep -> Bool
isByteArrayOperand value expectedRep =
  grinValueRuntimeRep value == BoxedRep Unlifted && expectedRep == AddrRep

sourceValues :: LowerEnv -> Fc.Type -> [GrinValue] -> LowerM [(Fc.Type, GrinValue)]
sourceValues env sourceType values = do
  types <- sourceValueTypes env sourceType
  if length types == length values
    then pure (zip (map fst types) values)
    else throwLower ("GRIN cannot match source values to type: " <> show sourceType)

sourceValueTypes :: LowerEnv -> Fc.Type -> LowerM [(Fc.Type, GrinRep)]
sourceValueTypes env sourceType = do
  representation <- liftEither (runtimeRep env sourceType)
  case representation of
    TupleRep fields -> do
      let (_, arguments) = collectTypeApplications (reduce env sourceType)
          fieldTypes = drop (length arguments - length fields) arguments
      if length fieldTypes /= length fields
        then throwLower ("GRIN cannot find unboxed tuple fields for type: " <> show sourceType)
        else fmap concat (zipWithM sourceFieldTypes fieldTypes fields)
    _ -> pure [(sourceType, component) | component <- runtimeRepComponents representation]
  where
    sourceFieldTypes fieldType fieldRep =
      case runtimeRepComponents fieldRep of
        [] -> pure []
        [component] -> pure [(fieldType, component)]
        _ -> throwLower ("GRIN does not support a nested tuple foreign value: " <> show fieldType)

adaptForeignOperands :: LowerEnv -> [Fc.AxiomDecl] -> [Fc.Name] -> [((Fc.Type, GrinValue), GrinRep)] -> ([GrinValue] -> LowerM GrinExpr) -> LowerM GrinExpr
adaptForeignOperands env axioms constructors operands continuation = go [] operands
  where
    go values [] = continuation (reverse values)
    go values (((sourceType, value), expectedRep) : rest)
      | grinValueRuntimeRep value == expectedRep = go (value : values) rest
      -- A byte array argument passes the address of its payload.
      | isByteArrayOperand value expectedRep = do
          contents <- freshVar "foreign_contents" AddrRep
          body <- go (GrinVarValue contents : values) rest
          pure (GrinBind [contents] (GrinPrimitiveCall AddrRep byteArrayContentsPrimitive [value]) body)
      | isLiftedRuntimeRep (grinValueRuntimeRep value) = do
          (tag, fieldRep) <- findUnaryConstructor env axioms constructors sourceType expectedRep
          evaluated <- freshVar "foreign_box" liftedGrinRep
          caseBinder <- freshVar "foreign_box_case" liftedGrinRep
          field <- freshVar "foreign_field" fieldRep
          body <- go (GrinVarValue field : values) rest
          pure
            ( GrinBind
                [evaluated]
                (GrinEval liftedGrinRep value)
                ( GrinCase
                    (GrinVarValue evaluated)
                    caseBinder
                    [GrinAlt (GrinDataAlt tag) [field] body]
                )
            )
      | otherwise = throwLower ("GRIN cannot adapt a foreign argument representation: " <> show sourceType)

adaptForeignResult :: LowerEnv -> [Fc.AxiomDecl] -> [Fc.Name] -> Fc.Type -> GrinRep -> GrinRep -> GrinExpr -> LowerM GrinExpr
adaptForeignResult env axioms constructors sourceType sourceRep foreignRep foreignExpression
  | sourceRep == foreignRep = pure foreignExpression
  | isLiftedRuntimeRep sourceRep = do
      (tag, fieldRep) <- findUnaryConstructor env axioms constructors sourceType foreignRep
      result <- freshVar "foreign_result" fieldRep
      pure
        ( GrinBind
            [result]
            foreignExpression
            (GrinStore (GrinNode (GrinConstructor tag 0) [GrinVarValue result]))
        )
  | otherwise = throwLower ("GRIN cannot adapt a foreign result representation: " <> show sourceType)

findUnaryConstructor :: LowerEnv -> [Fc.AxiomDecl] -> [Fc.Name] -> Fc.Type -> GrinRep -> LowerM (Text, GrinRep)
findUnaryConstructor env axioms constructors resultType expectedRep =
  case listToMaybe (mapMaybe matchConstructor constructorEntries) of
    Just result -> pure result
    Nothing -> throwLower ("GRIN cannot find a unary constructor adapter for type: " <> show resultType)
  where
    constructorEntries =
      [ (name, constructorType)
      | name <- constructors,
        Just constructorType <- [Map.lookup name (TypeOf.teHeaders (lowerTypes env))]
      ]
    matchConstructor (name, constructorType)
      | Fc.nameSort name /= Fc.SortDataConstructor = Nothing
      | otherwise = do
          fieldTypes <- instantiateConstructorFields env axioms constructorType resultType
          case fieldTypes of
            [fieldType] ->
              case runtimeRep env fieldType of
                Right fieldRep
                  | fieldRep == expectedRep -> Just (constructorTag name, fieldRep)
                _ -> Nothing
            _ -> Nothing

instantiateConstructorFields :: LowerEnv -> [Fc.AxiomDecl] -> Fc.Type -> Fc.Type -> Maybe [Fc.Type]
instantiateConstructorFields env axioms constructorType targetType = do
  let (binders, monotype) = splitForAlls constructorType
  (fieldTypes, constructorResult) <- either (const Nothing) Just (splitFunctionType monotype)
  substitution <- matchTypeBinders env (Map.fromList [(Fc.binderName binder, Nothing) | binder <- binders]) constructorResult (applyForeignAxioms env axioms targetType)
  resolved <- sequenceA substitution
  pure (map (TypeOf.substTypes resolved) fieldTypes)

matchTypeBinders :: LowerEnv -> Map Fc.Name (Maybe Fc.Type) -> Fc.Type -> Fc.Type -> Maybe (Map Fc.Name (Maybe Fc.Type))
matchTypeBinders env substitution patternType actualType =
  case (reduce env patternType, reduce env actualType) of
    (Fc.TyVar name, actual)
      | Just current <- Map.lookup name substitution ->
          case current of
            Nothing -> Just (Map.insert name (Just actual) substitution)
            Just previous
              | TypeOf.typesEqual (lowerTypes env) previous actual -> Just substitution
              | otherwise -> Nothing
    (Fc.TyVar name, Fc.TyVar actualName)
      | name == actualName -> Just substitution
    (Fc.TyCon name, Fc.TyCon actualName)
      | name == actualName -> Just substitution
    (Fc.TyApp function argument, Fc.TyApp actualFunction actualArgument) ->
      matchTypeBinders env substitution function actualFunction
        >>= \next -> matchTypeBinders env next argument actualArgument
    (Fc.TyFun r1 r2 argument result, Fc.TyFun actualR1 actualR2 actualArgument actualResult) ->
      matchTypeBinders env substitution r1 actualR1
        >>= \s1 ->
          matchTypeBinders env s1 r2 actualR2
            >>= \s2 ->
              matchTypeBinders env s2 argument actualArgument
                >>= \s3 -> matchTypeBinders env s3 result actualResult
    (Fc.TyEq left right, Fc.TyEq actualLeft actualRight) ->
      matchTypeBinders env substitution left actualLeft
        >>= \next -> matchTypeBinders env next right actualRight
    _ -> Nothing

collectTypeApplications :: Fc.Type -> (Fc.Type, [Fc.Type])
collectTypeApplications = go []
  where
    go arguments (Fc.TyApp function argument) = go (argument : arguments) function
    go arguments function = (function, arguments)

lowerExpr :: LowerEnv -> Fc.Expr -> LowerM GrinExpr
lowerExpr env expression =
  case expression of
    Fc.ExVar name -> lowerVariable env name
    Fc.ExLit literal -> GrinConstant . pure . GrinLitValue <$> lowerLiteral env literal
    Fc.ExApp function argument -> lowerApplication env function argument
    Fc.ExTyApp (Fc.ExTyLam binder body) argument ->
      lowerExpr env {lowerTypeSubstitution = Map.insert (Fc.binderName binder) (applySubstitution env argument) (lowerTypeSubstitution env)} body
    Fc.ExTyApp function _ -> lowerExpr env function
    Fc.ExLam {} -> GrinStore <$> makeClosure env expression
    Fc.ExTyLam binder body -> lowerExpr (extendTypeBinder env binder) body
    Fc.ExLet binding body -> lowerLet env binding body
    Fc.ExRec bindings body -> lowerRec env bindings body
    Fc.ExCase scrutinee binder _ alternatives -> lowerCase env scrutinee binder alternatives
    Fc.ExCast inner _ -> lowerExpr env inner

lowerVariable :: LowerEnv -> Fc.Name -> LowerM GrinExpr
lowerVariable env name = do
  ty <- lookupNameType env name
  representation <- liftEither (runtimeRep env ty)
  let components = runtimeRepComponents representation
  case Map.lookup name (lowerLocals env) of
    Just variables ->
      if isLiftedRuntimeRep representation
        then case variables of
          [variable] -> pure (GrinEval representation (GrinVarValue variable))
          _ -> throwLower ("GRIN expected one lifted local value: " <> show name)
        else pure (GrinConstant (map GrinVarValue variables))
    Nothing
      | null components -> pure (GrinConstant [])
      | otherwise -> do
          globalName <- lookupGlobalName env name
          pure (GrinEval representation (GrinGlobalValue globalName))

lowerApplication :: LowerEnv -> Fc.Expr -> Fc.Expr -> LowerM GrinExpr
lowerApplication env function argument = do
  let application = Fc.ExApp function argument
  resultRep <- expressionRuntimeRep env application
  case (resultRep, collectApplications application) of
    (_, (Fc.ExVar name, arguments))
      | Just arity <- Map.lookup (Fc.nameText name) specialPrimitiveArities,
        length arguments == arity ->
          lowerSpecialApplication env resultRep (Fc.nameText name) arguments
    (TupleRep {}, (Fc.ExVar name, arguments))
      | "(#" `T.isPrefixOf` Fc.nameText name -> lowerTupleArguments env arguments
    (_, (Fc.ExVar name, arguments))
      | resultRep == liftedGrinRep,
        not ("(#" `T.isPrefixOf` Fc.nameText name),
        Just arity <- Map.lookup name (lowerConstructorArities env),
        length arguments <= arity ->
          lowerConstructorApplication env name (arity - length arguments) arguments
    (_, (Fc.ExVar name, arguments))
      | Just localFunction <- Map.lookup name (lowerLocalFunctions env) ->
          lowerLocalFunctionApplication env resultRep name localFunction arguments
    _ ->
      lowerLazySingle env "function" function $ \functionValue -> do
        evaluated <- freshVar "function_whnf" liftedGrinRep
        lowerArgument env argument $ \argumentValues ->
          pure
            ( GrinBind
                [evaluated]
                (GrinEval liftedGrinRep functionValue)
                (GrinApply resultRep (GrinVarValue evaluated) argumentValues)
            )

collectApplications :: Fc.Expr -> (Fc.Expr, [Fc.Expr])
collectApplications expression = go expression []
  where
    go (Fc.ExApp function argument) arguments = go function (argument : arguments)
    go (Fc.ExTyApp function _) arguments = go function arguments
    go (Fc.ExCast function _) arguments = go function arguments
    go function arguments = (function, arguments)

lowerTupleArguments :: LowerEnv -> [Fc.Expr] -> LowerM GrinExpr
lowerTupleArguments env = go []
  where
    go values [] = pure (GrinConstant values)
    go values (argument : arguments) =
      lowerArgument env argument (\newValues -> go (values <> newValues) arguments)

lowerConstructorApplication :: LowerEnv -> Fc.Name -> Int -> [Fc.Expr] -> LowerM GrinExpr
lowerConstructorApplication env name remaining = go []
  where
    go values [] = pure (GrinStore (GrinNode (GrinConstructor (constructorTag name) remaining) values))
    go values (argument : arguments) =
      lowerArgument env argument (\newValues -> go (values <> newValues) arguments)

lowerLocalFunctionApplication :: LowerEnv -> GrinRep -> Fc.Name -> (Int, Maybe (FunctionName, GrinRep)) -> [Fc.Expr] -> LowerM GrinExpr
lowerLocalFunctionApplication env resultRep name (arity, maybeFunctionEntry) arguments
  | length arguments < arity = do
      globalName <- lookupGlobalName env name
      lowerDynamicApplication env resultRep (GrinGlobalValue globalName) arguments
  | Just (functionName, functionResultRep) <- maybeFunctionEntry,
    functionResultRep == directResultRep =
      lowerArguments env saturatedArguments $ \argumentValues ->
        case remainingArguments of
          [] -> pure (GrinCall resultRep functionName argumentValues)
          _ -> do
            applied <- freshVar "function_application" liftedGrinRep
            rest <- lowerDynamicApplication env resultRep (GrinVarValue applied) remainingArguments
            pure (GrinBind [applied] (GrinCall liftedGrinRep functionName argumentValues) rest)
  | otherwise = do
      globalName <- lookupGlobalName env name
      lowerDynamicApplication env resultRep (GrinGlobalValue globalName) arguments
  where
    (saturatedArguments, remainingArguments) = splitAt arity arguments
    directResultRep
      | null remainingArguments = resultRep
      | otherwise = liftedGrinRep

lowerDynamicApplication :: LowerEnv -> GrinRep -> GrinValue -> [Fc.Expr] -> LowerM GrinExpr
lowerDynamicApplication env resultRep = go
  where
    go functionValue [argument] = lowerArgument env argument (pure . GrinApply resultRep functionValue)
    go functionValue (argument : remaining) =
      lowerArgument env argument $ \argumentValues -> do
        applied <- freshVar "function_application" liftedGrinRep
        rest <- go (GrinVarValue applied) remaining
        pure (GrinBind [applied] (GrinApply liftedGrinRep functionValue argumentValues) rest)
    go _ [] = throwLower "GRIN local function application needs an argument"

lowerArguments :: LowerEnv -> [Fc.Expr] -> ([GrinValue] -> LowerM GrinExpr) -> LowerM GrinExpr
lowerArguments env = go []
  where
    go values [] continuation = continuation values
    go values (argument : arguments) continuation =
      lowerArgument env argument (\newValues -> go (values <> newValues) arguments continuation)

specialPrimitiveArities :: Map Text Int
specialPrimitiveArities = Map.fromList [("aihcExit#", 2), ("unsafeCoerce#", 1), ("raise#", 1), ("catch#", 3)]

lowerSpecialApplication :: LowerEnv -> GrinRep -> Text -> [Fc.Expr] -> LowerM GrinExpr
lowerSpecialApplication env resultRep name arguments =
  case (name, arguments) of
    ("aihcExit#", status : state : _) ->
      lowerArgument env status $ \case
        value : _ -> lowerArgument env state (const (pure (GrinExit value)))
        [] -> throwLower "GRIN process exit requires a status value"
    ("unsafeCoerce#", value : _) -> lowerArgument env value (pure . GrinConstant)
    ("raise#", exception : _) ->
      lowerLazySingle env "exception" exception (pure . GrinThrow)
    ("catch#", action : handler : state : _) ->
      lowerLazySingle env "action" action $ \actionValue ->
        lowerLazySingle env "handler" handler $ \handlerValue ->
          lowerArgument env state (lowerCatch resultRep actionValue handlerValue)
    _ -> throwLower ("GRIN cannot lower compiler primitive application: " <> T.unpack name)

lowerCatch :: GrinRep -> GrinValue -> GrinValue -> [GrinValue] -> LowerM GrinExpr
lowerCatch resultRep action handler stateValues = do
  evaluatedHandler <- freshVar "catch_handler" liftedGrinRep
  handlerCapture <- freshVar "catch_handler_capture" liftedGrinRep
  stateCaptures <- mapM (freshVar "catch_state_capture" . grinValueRuntimeRep) stateValues
  exception <- freshVar "catch_exception" liftedGrinRep
  handlerAction <- freshVar "catch_handler_action" liftedGrinRep
  evaluatedAction <- freshVar "catch_evaluated_action" liftedGrinRep
  wrapper <- freshVar "catch_handler_wrapper" liftedGrinRep
  functionName <- freshFunction "catch_handler"
  emitFunction
    GrinFunction
      { grinFunctionName = functionName,
        grinFunctionParameters = handlerCapture : stateCaptures <> [exception],
        grinFunctionResultRep = resultRep,
        grinFunctionBody =
          GrinBind
            [handlerAction]
            (GrinApply liftedGrinRep (GrinVarValue handlerCapture) [GrinVarValue exception])
            ( GrinBind
                [evaluatedAction]
                (GrinEval liftedGrinRep (GrinVarValue handlerAction))
                (GrinApply resultRep (GrinVarValue evaluatedAction) (map GrinVarValue stateCaptures))
            )
      }
  pure
    ( GrinBind
        [evaluatedHandler]
        (GrinEval liftedGrinRep handler)
        ( GrinBind
            [wrapper]
            ( GrinStore
                ( GrinNode
                    (GrinClosure functionName [[liftedGrinRep]])
                    (GrinVarValue evaluatedHandler : stateValues)
                )
            )
            (GrinCatch resultRep action (GrinVarValue wrapper) stateValues)
        )
    )

lowerArgument :: LowerEnv -> Fc.Expr -> ([GrinValue] -> LowerM GrinExpr) -> LowerM GrinExpr
lowerArgument env expression continuation = do
  representation <- expressionRuntimeRep env expression
  if null (runtimeRepComponents representation)
    then continuation []
    else
      if isLiftedRuntimeRep representation
        then lowerLazySingle env "argument" expression (continuation . (: []))
        else bindExpression env "argument" expression continuation

lowerLazySingle :: LowerEnv -> Text -> Fc.Expr -> (GrinValue -> LowerM GrinExpr) -> LowerM GrinExpr
lowerLazySingle env hint expression continuation =
  case expression of
    Fc.ExVar name ->
      case Map.lookup name (lowerLocals env) of
        Just [variable] -> continuation (GrinVarValue variable)
        Just _ -> throwLower ("GRIN expected one lazy local value: " <> show name)
        Nothing -> lookupGlobalName env name >>= continuation . GrinGlobalValue
    Fc.ExTyApp inner _ -> lowerLazySingle env hint inner continuation
    Fc.ExCast inner _ -> lowerLazySingle env hint inner continuation
    _ -> do
      node <- makeThunk env hint expression
      pointer <- freshVar hint liftedGrinRep
      rest <- continuation (GrinVarValue pointer)
      pure (GrinBind [pointer] (GrinStore node) rest)

bindExpression :: LowerEnv -> Text -> Fc.Expr -> ([GrinValue] -> LowerM GrinExpr) -> LowerM GrinExpr
bindExpression env hint expression continuation = do
  representation <- expressionRuntimeRep env expression
  variables <- freshVars hint representation
  valueExpression <- lowerExpr env expression
  rest <- continuation (map GrinVarValue variables)
  pure (GrinBind variables valueExpression rest)

lowerLet :: LowerEnv -> Fc.Bind -> Fc.Expr -> LowerM GrinExpr
lowerLet env binding body = do
  let binder = Fc.bindBinder binding
  representation <- liftEither (runtimeRep env (applySubstitution env (Fc.binderType binder)))
  variables <- freshVars (Fc.nameText (Fc.binderName binder)) representation
  let bodyEnv = bindLocal env binder variables
  loweredBody <- lowerExpr bodyEnv body
  if isLiftedRuntimeRep representation
    then do
      node <- makeThunk env (Fc.nameText (Fc.binderName binder)) (Fc.bindRhs binding)
      pure (GrinBind variables (GrinStore node) loweredBody)
    else do
      loweredRhs <- lowerExpr env (Fc.bindRhs binding)
      pure (GrinBind variables loweredRhs loweredBody)

lowerRec :: LowerEnv -> [Fc.Bind] -> Fc.Expr -> LowerM GrinExpr
lowerRec env bindings body = do
  variables <- mapM makeVariables bindings
  let recursiveEnv = foldl bindOne env (zip bindings variables)
  nodes <- mapM (makeBindingNode recursiveEnv) bindings
  loweredBody <- lowerExpr recursiveEnv body
  pure (GrinStoreRec (zip (concat variables) nodes) loweredBody)
  where
    makeVariables binding = do
      let binder = Fc.bindBinder binding
      representation <- liftEither (runtimeRep env (applySubstitution env (Fc.binderType binder)))
      if isLiftedRuntimeRep representation
        then (: []) <$> freshVar (Fc.nameText (Fc.binderName binder)) representation
        else throwLower ("GRIN does not support an unlifted recursive binding: " <> show (Fc.binderName binder))
    bindOne current (binding, vars) = bindLocal current (Fc.bindBinder binding) vars
    makeBindingNode recursiveEnv binding = makeThunk recursiveEnv (Fc.nameText (Fc.binderName (Fc.bindBinder binding))) (Fc.bindRhs binding)

lowerCase :: LowerEnv -> Fc.Expr -> Fc.Binder -> [Fc.Alt] -> LowerM GrinExpr
lowerCase env scrutinee binder alternatives = do
  representation <- expressionRuntimeRep env scrutinee
  case representation of
    TupleRep _ -> lowerTupleCase env scrutinee binder alternatives
    _ ->
      bindExpression env "case_value" scrutinee $ \case
        [value] -> do
          caseBinder <- freshVar (Fc.nameText (Fc.binderName binder)) representation
          loweredAlternatives <- mapM (lowerAlt (bindLocal env binder [caseBinder])) alternatives
          pure (GrinCase value caseBinder loweredAlternatives)
        _ -> throwLower "GRIN case expected one scrutinee value"

lowerTupleCase :: LowerEnv -> Fc.Expr -> Fc.Binder -> [Fc.Alt] -> LowerM GrinExpr
lowerTupleCase env scrutinee binder alternatives = do
  alternative <-
    case alternatives of
      first : _ -> pure first
      [] -> throwLower "GRIN cannot lower an empty unboxed tuple case"
  let typeEnv = foldl extendTypeBinder env (Fc.altTypeBinders alternative)
  fieldVariables <- mapM (freshVarsForBinder typeEnv) (Fc.altBinders alternative)
  let values = concat fieldVariables
      binderEnv = bindLocal typeEnv binder values
      alternativeEnv = foldl bindPair binderEnv (zip (Fc.altBinders alternative) fieldVariables)
  loweredRhs <- lowerExpr alternativeEnv (Fc.altRhs alternative)
  loweredScrutinee <- lowerExpr env scrutinee
  pure (GrinBind values loweredScrutinee loweredRhs)
  where
    bindPair current (fieldBinder, vars) = bindLocal current fieldBinder vars

lowerAlt :: LowerEnv -> Fc.Alt -> LowerM GrinAlt
lowerAlt env alternative = do
  let typeEnv = foldl extendTypeBinder env (Fc.altTypeBinders alternative)
  binderGroups <- mapM (freshVarsForBinder typeEnv) (Fc.altBinders alternative)
  let bodyEnv = foldl bindPair typeEnv (zip (Fc.altBinders alternative) binderGroups)
  body <- lowerExpr bodyEnv (Fc.altRhs alternative)
  alternativeConstructor <- lowerAltCon typeEnv (Fc.altCon alternative)
  pure
    GrinAlt
      { grinAltCon = alternativeConstructor,
        grinAltBinders = concat binderGroups,
        grinAltRhs = body
      }
  where
    bindPair current (binder, vars) = bindLocal current binder vars

lowerAltCon :: LowerEnv -> Fc.AltCon -> LowerM GrinAltCon
lowerAltCon env alternative =
  case alternative of
    Fc.AltData name -> pure (GrinDataAlt (constructorTag name))
    Fc.AltLit literal -> GrinLitAlt <$> lowerLiteral env literal
    Fc.AltDefault -> pure GrinDefaultAlt

makeThunk :: LowerEnv -> Text -> Fc.Expr -> LowerM GrinNode
makeThunk env hint expression = do
  representation <- expressionRuntimeRep env expression
  if not (isLiftedRuntimeRep representation)
    then throwLower ("GRIN cannot suspend an unlifted expression with representation " <> show representation)
    else do
      let captures = capturedVariables env expression
      -- The name is allocated even when the thunk turns out to need no
      -- function of its own, so that generated function names stay identical
      -- between the discovery pass and the final pass. Only the final pass
      -- knows the entry functions that 'directThunkNode' suspends, and the
      -- calls it emits name functions the discovery pass numbered.
      functionName <- freshFunction (hint <> "_thunk")
      direct <- directThunkNode env expression
      case direct of
        Just node -> pure node
        Nothing -> do
          body <- lowerExpr env expression
          emitFunction
            GrinFunction
              { grinFunctionName = functionName,
                grinFunctionParameters = captures,
                grinFunctionResultRep = representation,
                grinFunctionBody = body
              }
          pure (GrinNode (GrinThunk functionName) (map GrinVarValue captures))

-- | Suspend a saturated call to a known function on that function itself.
--
-- A thunk node carries the values its entry function is applied to, so a call
-- whose operands are already named needs no code of its own. Giving it a
-- function that does nothing but forward those values to the callee costs a
-- function definition, an info table and an extra entry for nothing.
directThunkNode :: LowerEnv -> Fc.Expr -> LowerM (Maybe GrinNode)
directThunkNode env expression =
  case collectApplications expression of
    (Fc.ExVar name, arguments)
      | Just (arity, Just (functionName, functionResultRep)) <- Map.lookup name (lowerLocalFunctions env),
        arity == length arguments,
        functionResultRep == liftedGrinRep -> do
          values <- mapM (settledArgument env) arguments
          pure (GrinNode (GrinThunk functionName) . concat <$> sequence values)
    _ -> pure Nothing

-- | The runtime values of one argument, when naming them costs neither an
-- evaluation nor an allocation. Anything else has to run inside a thunk body.
settledArgument :: LowerEnv -> Fc.Expr -> LowerM (Maybe [GrinValue])
settledArgument env expression = do
  representation <- expressionRuntimeRep env expression
  if null (runtimeRepComponents representation)
    then pure (Just [])
    else
      if not (isLiftedRuntimeRep representation)
        then pure Nothing
        else case stripValueWrappers expression of
          Fc.ExVar name ->
            case Map.lookup name (lowerLocals env) of
              Just [variable] -> pure (Just [GrinVarValue variable])
              Just _ -> pure Nothing
              Nothing -> Just . pure . GrinGlobalValue <$> lookupGlobalName env name
          _ -> pure Nothing

-- | Drop the type applications and casts that carry no runtime value.
stripValueWrappers :: Fc.Expr -> Fc.Expr
stripValueWrappers expression =
  case expression of
    Fc.ExTyApp inner _ -> stripValueWrappers inner
    Fc.ExCast inner _ -> stripValueWrappers inner
    _ -> expression

makeClosure :: LowerEnv -> Fc.Expr -> LowerM GrinNode
makeClosure env expression = do
  let (bodyEnv0, binders, body) = collectLambdas env expression
  let captures = capturedVariables env expression
  parameterGroups <- mapM (freshVarsForBinder bodyEnv0) binders
  let bodyEnv = foldl bindPair bodyEnv0 (zip binders parameterGroups)
  bodyRep <- expressionRuntimeRep bodyEnv body
  loweredBody <- lowerExpr bodyEnv body
  functionName <- freshFunction "closure"
  emitFunction
    GrinFunction
      { grinFunctionName = functionName,
        grinFunctionParameters = captures <> concat parameterGroups,
        grinFunctionResultRep = bodyRep,
        grinFunctionBody = loweredBody
      }
  pure
    ( GrinNode
        (GrinClosure functionName (map (map grinVarRuntimeRep) parameterGroups))
        (map GrinVarValue captures)
    )
  where
    bindPair current (binder, vars) = bindLocal current binder vars

-- | An expression that is a call of a primitive that never returns.
divergingExpression :: Fc.Expr -> Bool
divergingExpression expression =
  case applicationHead expression of
    Just name -> Fc.nameText name `elem` ["raise#", "aihcExit#"]
    Nothing -> False
  where
    applicationHead current =
      case current of
        Fc.ExApp function _ -> applicationHead function
        Fc.ExTyApp function _ -> applicationHead function
        Fc.ExVar name -> Just name
        _ -> Nothing

collectLambdas :: LowerEnv -> Fc.Expr -> (LowerEnv, [Fc.Binder], Fc.Expr)
collectLambdas env expression =
  case expression of
    Fc.ExLam binder body ->
      let (bodyEnv, binders, result) = collectLambdas env body
       in (bodyEnv, binder : binders, result)
    Fc.ExTyLam binder body -> collectLambdas (extendTypeBinder env binder) body
    _ -> (env, [], expression)

capturedVariables :: LowerEnv -> Fc.Expr -> [GrinVar]
capturedVariables env expression =
  concat
    [ variables
    | name <- Set.toAscList (freeVariables expression),
      Just variables <- [Map.lookup name (lowerLocals env)]
    ]

freeVariables :: Fc.Expr -> Set Fc.Name
freeVariables expression =
  case expression of
    Fc.ExVar name -> Set.singleton name
    Fc.ExLit {} -> Set.empty
    Fc.ExApp function argument -> freeVariables function <> freeVariables argument
    Fc.ExTyApp function _ -> freeVariables function
    Fc.ExLam binder body -> Set.delete (Fc.binderName binder) (freeVariables body)
    Fc.ExTyLam _ body -> freeVariables body
    Fc.ExLet binding body -> freeVariables (Fc.bindRhs binding) <> Set.delete (Fc.binderName (Fc.bindBinder binding)) (freeVariables body)
    Fc.ExRec bindings body ->
      let names = Set.fromList (map (Fc.binderName . Fc.bindBinder) bindings)
       in (foldMap (freeVariables . Fc.bindRhs) bindings <> freeVariables body) `Set.difference` names
    Fc.ExCase scrutinee binder _ alternatives ->
      freeVariables scrutinee
        <> Set.delete (Fc.binderName binder) (foldMap freeAltVariables alternatives)
    Fc.ExCast inner _ -> freeVariables inner

freeAltVariables :: Fc.Alt -> Set Fc.Name
freeAltVariables alternative =
  freeVariables (Fc.altRhs alternative)
    `Set.difference` Set.fromList (map Fc.binderName (Fc.altBinders alternative))

expressionRuntimeRep :: LowerEnv -> Fc.Expr -> LowerM GrinRep
expressionRuntimeRep env expression =
  case expression of
    Fc.ExLit literal -> literalRep env literal
    -- A call that always raises never returns a value, so its runtime
    -- representation can stay polymorphic. This is what makes a
    -- representation-polymorphic @error@ possible.
    _ | divergingExpression expression -> pure liftedGrinRep
    _ -> expressionType env expression >>= liftEither . runtimeRep env

expressionType :: LowerEnv -> Fc.Expr -> LowerM Fc.Type
expressionType env expression =
  case expression of
    Fc.ExVar name -> lookupNameType env name
    Fc.ExLit {} -> throwLower "GRIN cannot infer a source type for this literal"
    Fc.ExApp function _ -> do
      functionType <- expressionType env function
      case reduce env functionType of
        Fc.TyFun _ _ _ result -> pure result
        other -> throwLower ("GRIN application has a non-function type: " <> show other <> " for " <> show function)
    Fc.ExTyApp function argument -> do
      functionType <- expressionType env function
      case reduce env functionType of
        Fc.TyForAll binder body -> pure (TypeOf.substType (Fc.binderName binder) (applySubstitution env argument) body)
        other -> throwLower ("GRIN type application has a non-forall type: " <> show other)
    Fc.ExLam binder body -> do
      bodyType <- expressionType (extendTypeBinder env binder) body
      argumentRep <- repType env (Fc.binderType binder)
      resultRep <- repType env bodyType
      pure (Fc.TyFun argumentRep resultRep (applySubstitution env (Fc.binderType binder)) bodyType)
    Fc.ExTyLam binder body -> Fc.TyForAll binder <$> expressionType (extendTypeBinder env binder) body
    Fc.ExLet binding body -> expressionType (extendTermBinder (Fc.bindBinder binding) env) body
    Fc.ExRec bindings body -> expressionType (foldl (flip (extendTermBinder . Fc.bindBinder)) env bindings) body
    Fc.ExCase _ _ resultType _ -> pure (applySubstitution env resultType)
    Fc.ExCast _ coercion ->
      case TypeOf.coercionEndpoints (lowerTypes env) coercion of
        Just (_, target) -> pure (applySubstitution env target)
        Nothing -> throwLower ("GRIN cannot determine coercion endpoints: " <> show coercion)

runtimeRep :: LowerEnv -> Fc.Type -> Either String GrinRep
runtimeRep env sourceType = do
  representation <-
    maybe
      (Left ("GRIN cannot find a runtime representation for type: " <> show appliedType))
      pure
      (TypeOf.repOf (lowerTypes env) appliedType)
  convertRep env representation
  where
    appliedType = applySubstitution env sourceType

repType :: LowerEnv -> Fc.Type -> LowerM Fc.Type
repType env sourceType =
  maybe
    (throwLower ("GRIN cannot find a runtime representation type for: " <> show sourceType))
    pure
    (TypeOf.repOf (lowerTypes env) (applySubstitution env sourceType))

runtimeComponents :: LowerEnv -> Fc.Type -> Either String [GrinRep]
runtimeComponents env sourceType = runtimeRepComponents <$> runtimeRep env sourceType

convertRep :: LowerEnv -> Fc.Type -> Either String GrinRep
convertRep env sourceRep =
  case reduce env sourceRep of
    Fc.TyVar name -> Left ("GRIN does not support a variable runtime representation: " <> show name)
    Fc.TyCon name -> simpleRep (Fc.nameText name)
    Fc.TyApp (Fc.TyCon name) levity
      | Fc.nameText name == "BoxedRep" -> BoxedRep <$> convertLevity levity
    Fc.TyApp (Fc.TyCon name) fields
      | Fc.nameText name == "TupleRep" -> TupleRep <$> convertRepList env fields
      | Fc.nameText name == "SumRep" -> SumRep <$> convertRepList env fields
    Fc.TyApp (Fc.TyApp (Fc.TyCon name) count) element
      | Fc.nameText name == "VecRep" -> VecRep <$> readNamed "vector count" count <*> readNamed "vector element" element
    other -> Left ("GRIN does not support runtime representation: " <> show other)

simpleRep :: Text -> Either String GrinRep
simpleRep name =
  case name of
    "LiftedRep" -> pure liftedGrinRep
    "UnliftedRep" -> pure (BoxedRep Unlifted)
    "IntRep" -> pure IntRep
    "Int8Rep" -> pure Int8Rep
    "Int16Rep" -> pure Int16Rep
    "Int32Rep" -> pure Int32Rep
    "Int64Rep" -> pure Int64Rep
    "WordRep" -> pure WordRep
    "Word8Rep" -> pure Word8Rep
    "Word16Rep" -> pure Word16Rep
    "Word32Rep" -> pure Word32Rep
    "Word64Rep" -> pure Word64Rep
    "AddrRep" -> pure AddrRep
    "FloatRep" -> pure FloatRep
    "DoubleRep" -> pure DoubleRep
    _ -> Left ("GRIN does not know runtime representation: " <> T.unpack name)

convertLevity :: Fc.Type -> Either String GrinLevity
convertLevity levity =
  case levity of
    Fc.TyCon name
      | Fc.nameText name == "Lifted" -> pure Lifted
      | Fc.nameText name == "Unlifted" -> pure Unlifted
    _ -> Left ("GRIN does not support levity: " <> show levity)

convertRepList :: LowerEnv -> Fc.Type -> Either String [GrinRep]
convertRepList env list =
  case reduce env list of
    Fc.TyApp (Fc.TyCon name) _
      | Fc.nameText name == "[]" -> pure []
    Fc.TyApp (Fc.TyApp (Fc.TyApp (Fc.TyCon name) _) item) rest
      | Fc.nameText name == ":" -> (:) <$> convertRep env item <*> convertRepList env rest
    other -> Left ("GRIN does not support this runtime representation list: " <> show other)

readNamed :: (Read value) => String -> Fc.Type -> Either String value
readNamed label ty =
  case ty of
    Fc.TyCon name ->
      maybe (Left ("GRIN does not know " <> label <> ": " <> T.unpack (Fc.nameText name))) pure (readMaybe (T.unpack (Fc.nameText name)))
    _ -> Left ("GRIN does not support " <> label <> ": " <> show ty)

literalRep :: LowerEnv -> Fc.Literal -> LowerM GrinRep
literalRep env literal =
  case literal of
    Fc.LitInt representation _ -> liftEither (convertRep env representation)
    Fc.LitChar representation _ -> liftEither (convertRep env representation)
    Fc.LitAddr {} -> pure AddrRep

lowerLiteral :: LowerEnv -> Fc.Literal -> LowerM GrinLiteral
lowerLiteral env literal =
  case literal of
    Fc.LitInt representation value -> GrinLitInt <$> liftEither (convertRep env representation) <*> pure value
    Fc.LitChar representation value -> GrinLitChar <$> liftEither (convertRep env representation) <*> pure value
    Fc.LitAddr _ value -> pure (GrinLitAddr value)

lowerForeignCall :: Fc.Name -> Fc.CCallSpec -> GrinForeignCall
lowerForeignCall name specification =
  GrinForeignCall
    { grinForeignCallName = Fc.nameText name,
      grinForeignCallSymbol = Fc.ccallSymbol specification,
      grinForeignCallSignature =
        GrinForeignSignature
          { grinForeignArgumentTypes = map lowerForeignType (Fc.ccallArgumentTypes specification),
            grinForeignResultType = lowerForeignType (Fc.ccallResultType specification),
            grinForeignEffect =
              case Fc.ccallEffect specification of
                Fc.ForeignPure -> GrinForeignPure
                Fc.ForeignRealWorld -> GrinForeignRealWorld
          }
    }

lowerForeignType :: Fc.CAbiType -> GrinForeignType
lowerForeignType foreignType =
  case foreignType of
    Fc.CAbiInt -> GrinForeignInt
    Fc.CAbiInt32 -> GrinForeignInt32
    Fc.CAbiWord64 -> GrinForeignWord64
    Fc.CAbiAddr -> GrinForeignAddr

splitFunctionType :: Fc.Type -> Either String ([Fc.Type], Fc.Type)
splitFunctionType sourceType =
  case sourceType of
    Fc.TyForAll _ body -> splitFunctionType body
    Fc.TyFun _ _ argument result -> do
      (arguments, finalResult) <- splitFunctionType result
      pure (argument : arguments, finalResult)
    _ -> pure ([], sourceType)

splitOperationalFunctionType :: LowerEnv -> [Fc.AxiomDecl] -> Fc.Type -> LowerM ([Fc.Type], Fc.Type)
splitOperationalFunctionType env axioms sourceType =
  case reduce env sourceType of
    Fc.TyForAll binder body -> splitOperationalFunctionType (extendTypeBinder env binder) axioms body
    Fc.TyFun _ _ argument result -> do
      (arguments, finalResult) <- splitOperationalFunctionType env axioms result
      pure (argument : arguments, finalResult)
    other ->
      let unwrapped = applyForeignAxioms env axioms other
       in if TypeOf.typesEqual (lowerTypes env) other unwrapped
            then pure ([], other)
            else splitOperationalFunctionType env axioms unwrapped

applyForeignAxioms :: LowerEnv -> [Fc.AxiomDecl] -> Fc.Type -> Fc.Type
applyForeignAxioms env axioms = go Set.empty
  where
    go visited sourceType
      | sourceType `Set.member` visited = sourceType
      | otherwise =
          case listToMaybe (mapMaybe (\axiom -> TypeOf.applyRepresentationalAxiom (lowerTypes env) axiom sourceType) axioms) of
            Just target -> go (Set.insert sourceType visited) target
            Nothing -> sourceType

splitForAlls :: Fc.Type -> ([Fc.Binder], Fc.Type)
splitForAlls sourceType =
  case sourceType of
    Fc.TyForAll binder body ->
      let (binders, result) = splitForAlls body
       in (binder : binders, result)
    _ -> ([], sourceType)

constructorArgumentTypes :: Fc.Type -> Either String [Fc.Type]
constructorArgumentTypes sourceType = fst <$> splitFunctionType sourceType

constructorResultType :: Fc.Type -> Either String Fc.Type
constructorResultType sourceType = snd <$> splitFunctionType sourceType

globalNameTable :: TypeOf.TypeEnv -> Map Fc.Name Text
globalNameTable types =
  Map.fromList
    [ (name, stableGlobalName name)
    | name <- Map.keys (TypeOf.teHeaders types),
      Fc.nameSort name `elem` [Fc.SortValue, Fc.SortDataConstructor]
    ]

constructorArityTable :: TypeOf.TypeEnv -> Map Fc.Name Int
constructorArityTable types =
  Map.mapMaybeWithKey constructorArity (TypeOf.teHeaders types)
  where
    constructorArity name sourceType
      | Fc.nameSort name == Fc.SortDataConstructor =
          either (const Nothing) (Just . length) (constructorArgumentTypes sourceType)
      | otherwise = Nothing

localFunctionArityTable :: Fc.Program -> Map Fc.Name Int
localFunctionArityTable program =
  Map.fromList
    [ (Fc.valName declaration, arity)
    | Fc.DeclVal declaration <- Fc.programDecls program,
      let arity = functionArity (Fc.valBody declaration),
      arity > 0
    ]

localFunctionTable :: LowerEnv -> Fc.Program -> TopParts -> LowerState -> Either String (Map Fc.Name (Int, Maybe (FunctionName, GrinRep)))
localFunctionTable env program parts state = Map.fromList <$> traverse localFunction declarations
  where
    globals = Map.fromList (topGlobals parts)
    functions = Map.fromList [(grinFunctionName function, function) | function <- lowerFunctionsRev state]
    declarations =
      [ (Fc.valName declaration, arity)
      | Fc.DeclVal declaration <- Fc.programDecls program,
        let arity = functionArity (Fc.valBody declaration),
        arity > 0
      ]
    localFunction (name, arity) = do
      globalName <- maybe (Left ("GRIN has no global name for: " <> show name)) Right (Map.lookup name (lowerGlobalNames env))
      case Map.lookup globalName globals of
        Just (GrinNode (GrinClosure functionName _) []) ->
          case Map.lookup functionName functions of
            Just function -> Right (name, (arity, Just (functionName, grinFunctionResultRep function)))
            Nothing -> Left ("GRIN has no local function definition for: " <> show name)
        _ -> Left ("GRIN has no local function entry for: " <> show name)

stableGlobalName :: Fc.Name -> Text
stableGlobalName name =
  case Fc.nameOrigin name of
    Fc.OriginTop (PackageId packageName) moduleName ->
      T.intercalate "\0" [packageName, moduleName, Fc.nameText name]
    Fc.OriginLocal (Unique unique) -> Fc.nameText name <> "\0" <> T.pack (show unique)

constructorTag :: Fc.Name -> Text
constructorTag name =
  case Fc.nameOrigin name of
    Fc.OriginTop (PackageId packageName) moduleName ->
      (if packageName == "" then "" else packageName <> ":") <> moduleName <> "." <> Fc.nameText name
    Fc.OriginLocal {} -> Fc.nameText name

lookupGlobalName :: LowerEnv -> Fc.Name -> LowerM Text
lookupGlobalName env name =
  maybe (throwLower ("GRIN has no global name for: " <> show name)) pure (Map.lookup name (lowerGlobalNames env))

lookupNameType :: LowerEnv -> Fc.Name -> LowerM Fc.Type
lookupNameType env name =
  case Map.lookup name (TypeOf.teBinders (lowerTypes env)) <|> TypeOf.lookupHeaderType (lowerTypes env) name of
    Just sourceType -> pure (applySubstitution env sourceType)
    Nothing -> throwLower ("GRIN has no type for: " <> show name)

applySubstitution :: LowerEnv -> Fc.Type -> Fc.Type
applySubstitution env = TypeOf.substTypes (lowerTypeSubstitution env)

reduce :: LowerEnv -> Fc.Type -> Fc.Type
reduce env = TypeOf.reduceType (lowerTypes env) . applySubstitution env

extendTypeBinder :: LowerEnv -> Fc.Binder -> LowerEnv
extendTypeBinder env binder = env {lowerTypes = TypeOf.extendBinder (lowerTypes env) binder}

defaultRuntimeReps :: LowerEnv -> [Fc.Binder] -> LowerEnv
defaultRuntimeReps = foldl defaultOne
  where
    defaultOne env binder =
      case reduce env (Fc.binderType binder) of
        Fc.TyCon name
          | Fc.nameText name == "RuntimeRep" ->
              env
                { lowerTypeSubstitution =
                    Map.insert
                      (Fc.binderName binder)
                      (Fc.TyCon (Wired.liftedRepName (TypeOf.tePrimPackage (lowerTypes env))))
                      (lowerTypeSubstitution env)
                }
        _ -> env

extendTermBinder :: Fc.Binder -> LowerEnv -> LowerEnv
extendTermBinder binder env = env {lowerTypes = TypeOf.extendBinder (lowerTypes env) binder}

bindLocal :: LowerEnv -> Fc.Binder -> [GrinVar] -> LowerEnv
bindLocal env binder variables =
  (extendTermBinder binder env)
    { lowerLocals = Map.insert (Fc.binderName binder) variables (lowerLocals env)
    }

freshVarsForBinder :: LowerEnv -> Fc.Binder -> LowerM [GrinVar]
freshVarsForBinder env binder = freshVarsForType env (Fc.nameText (Fc.binderName binder), applySubstitution env (Fc.binderType binder))

freshVarsForType :: LowerEnv -> (Text, Fc.Type) -> LowerM [GrinVar]
freshVarsForType env (hint, sourceType) = liftEither (runtimeRep env sourceType) >>= freshVars hint

freshVars :: Text -> GrinRep -> LowerM [GrinVar]
freshVars hint representation = mapM (freshVar hint) (runtimeRepComponents representation)

freshVar :: Text -> GrinRep -> LowerM GrinVar
freshVar hint representation = do
  state <- get
  let unique = lowerNextUnique state
  modify' (\current -> current {lowerNextUnique = unique - 1})
  pure (GrinVar hint unique representation)

freshFunction :: Text -> LowerM FunctionName
freshFunction hint = do
  state <- get
  let unique = lowerNextFunction state
  modify' (\current -> current {lowerNextFunction = unique + 1})
  pure (FunctionName ("$grin_" <> hint <> "_" <> T.pack (show unique)))

emitFunction :: GrinFunction -> LowerM ()
emitFunction function = modify' (\state -> state {lowerFunctionsRev = function : lowerFunctionsRev state})

liftEither :: Either String value -> LowerM value
liftEither = either throwLower pure

throwLower :: String -> LowerM value
throwLower = lift . Left
