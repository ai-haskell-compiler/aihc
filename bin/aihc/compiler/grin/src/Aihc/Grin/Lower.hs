{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Types (Unique (..))
import Control.Applicative ((<|>))
import Control.Monad (foldM, zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, modify', runStateT)
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
    lowerGlobalNames :: !(Map Fc.Name Text)
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
      env = LowerEnv types Map.empty Map.empty globals
      initialState = LowerState (-1000000000) 0 []
  (parts, finalState) <- runStateT (foldMapM (lowerDecl env) (Fc.programDecls program)) initialState
  pure
    ( normalizeGrinProgram
        GrinProgram
          { grinConstructors = topConstructors parts,
            grinPrimitives = topPrimitives parts,
            grinForeignCalls = topForeignCalls parts,
            grinGlobals = topGlobals parts,
            grinFunctions = reverse (lowerFunctionsRev finalState)
          }
    )

foldMapM :: (Monad monad, Monoid value) => (item -> monad value) -> [item] -> monad value
foldMapM action = foldM (\result item -> (result <>) <$> action item) mempty

lowerDecl :: LowerEnv -> Fc.Decl -> LowerM TopParts
lowerDecl env declaration =
  case declaration of
    Fc.DeclType value -> lowerTypeDecl env value
    Fc.DeclVal value -> lowerValueDecl env value
    Fc.DeclForeignImport value -> lowerForeignDecl env value
    Fc.DeclSynonym {} -> pure mempty
    Fc.DeclAxiom {} -> pure mempty

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
          fieldLayouts <- mapM (runtimeComponents constructorEnv) fieldTypes
          resultType <- liftEither (constructorResultType monotype)
          resultRep <- runtimeRep constructorEnv resultType
          case resultRep of
            TupleRep {} -> pure ([], [])
            _ -> do
              globalName <- lookupGlobalName env name
              let tag = constructorTag name
              pure ([(tag, fieldLayouts)], [(globalName, GrinNode (GrinConstructor tag (length fieldTypes)) [])])

lowerValueDecl :: LowerEnv -> Fc.ValDecl -> LowerM TopParts
lowerValueDecl env declaration = do
  representation <- runtimeRep env (Fc.valType declaration)
  if representation /= liftedGrinRep
    then throwLower ("GRIN does not support an unlifted top-level value: " <> show (Fc.valName declaration))
    else do
      globalName <- lookupGlobalName env (Fc.valName declaration)
      node <- makeThunk env (Fc.nameText (Fc.valName declaration)) (Fc.valBody declaration)
      pure mempty {topGlobals = [(globalName, node)]}

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
  resultRep <- runtimeRep foreignEnv resultType
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
        expression <- lowerForeignBody foreignEnv axioms constructors foreignCall argumentTypes valueGroups resultType
        pure (expression, [], [foreignCall])
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
compilerPrimitives = ["aihcExit#", "unsafeCoerce#", "raise#", "catch#", "seq"]

lowerPrimitiveBody :: GrinRep -> Text -> [[GrinValue]] -> LowerM GrinExpr
lowerPrimitiveBody resultRep name valueGroups =
  case (name, valueGroups) of
    ("aihcExit#", (status : _) : _) -> pure (GrinExit status)
    ("unsafeCoerce#", values : _) -> pure (GrinConstant values)
    ("raise#", (exception : _) : _) -> pure (GrinThrow exception)
    ("catch#", (action : _) : (handler : _) : state) ->
      lowerCatch resultRep action handler (concat state)
    ("seq", (first : _) : second : _) -> do
      evaluated <- freshVar "seq" liftedGrinRep
      pure (GrinBind [evaluated] (GrinEval liftedGrinRep first) (GrinConstant second))
    _ -> pure (GrinPrimitiveCall resultRep name (concat valueGroups))

lowerForeignBody :: LowerEnv -> [Fc.AxiomDecl] -> [Fc.Name] -> GrinForeignCall -> [Fc.Type] -> [[GrinValue]] -> Fc.Type -> LowerM GrinExpr
lowerForeignBody env axioms constructors foreignCall argumentTypes valueGroups resultType = do
  operands <- concat <$> zipWithM (sourceValues env) argumentTypes valueGroups
  resultValues <- sourceValueTypes env resultType
  let signature = grinForeignCallSignature foreignCall
      expectedOperands = grinForeignOperandReps signature
      resultReps = grinForeignCallResultReps signature
  if length operands /= length expectedOperands
    then throwLower ("GRIN foreign source arguments do not match the C ABI: " <> T.unpack (grinForeignCallName foreignCall))
    else case (resultValues, resultReps) of
      ([(resultValueType, resultValueRep)], [foreignResultRep]) ->
        adaptForeignOperands env axioms constructors (zip operands expectedOperands) $ \values ->
          adaptForeignResult env axioms constructors resultValueType resultValueRep foreignResultRep (GrinForeignCallExpr foreignCall values)
      _ -> throwLower ("GRIN foreign result does not match the C ABI: " <> T.unpack (grinForeignCallName foreignCall))

sourceValues :: LowerEnv -> Fc.Type -> [GrinValue] -> LowerM [(Fc.Type, GrinValue)]
sourceValues env sourceType values = do
  types <- sourceValueTypes env sourceType
  if length types == length values
    then pure (zip (map fst types) values)
    else throwLower ("GRIN cannot match source values to type: " <> show sourceType)

sourceValueTypes :: LowerEnv -> Fc.Type -> LowerM [(Fc.Type, GrinRep)]
sourceValueTypes env sourceType = do
  representation <- runtimeRep env sourceType
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
              case runStateT (runtimeRep env fieldType) (LowerState 0 0 []) of
                Right (fieldRep, _)
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
    Fc.ExCase scrutinee binder resultType alternatives -> lowerCase env scrutinee binder resultType alternatives
    Fc.ExCast inner _ -> lowerExpr env inner

lowerVariable :: LowerEnv -> Fc.Name -> LowerM GrinExpr
lowerVariable env name = do
  ty <- lookupNameType env name
  representation <- runtimeRep env ty
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
      | isLiftedRuntimeRep representation -> do
          globalName <- lookupGlobalName env name
          pure (GrinEval representation (GrinGlobalValue globalName))
      | otherwise -> throwLower ("GRIN does not support an imported unlifted value: " <> show name)

lowerApplication :: LowerEnv -> Fc.Expr -> Fc.Expr -> LowerM GrinExpr
lowerApplication env function argument = do
  let application = Fc.ExApp function argument
  resultType <- expressionType env application
  resultRep <- runtimeRep env resultType
  case (resultRep, collectApplications application) of
    (_, (Fc.ExVar name, arguments))
      | Just arity <- Map.lookup (Fc.nameText name) specialPrimitiveArities,
        length arguments == arity ->
          lowerSpecialApplication env resultRep (Fc.nameText name) arguments
    (TupleRep {}, (Fc.ExVar name, arguments))
      | "(#" `T.isPrefixOf` Fc.nameText name -> lowerTupleArguments env arguments
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

specialPrimitiveArities :: Map Text Int
specialPrimitiveArities = Map.fromList [("aihcExit#", 2), ("unsafeCoerce#", 1), ("raise#", 1), ("catch#", 3), ("seq", 2)]

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
    ("seq", first : second : _) ->
      lowerLazySingle env "seq_argument" first $ \firstValue -> do
        evaluated <- freshVar "seq" liftedGrinRep
        rest <- lowerArgument env second (pure . GrinConstant)
        pure (GrinBind [evaluated] (GrinEval liftedGrinRep firstValue) rest)
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
  representation <- runtimeRep env (applySubstitution env (Fc.binderType binder))
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
      representation <- runtimeRep env (applySubstitution env (Fc.binderType binder))
      if isLiftedRuntimeRep representation
        then (: []) <$> freshVar (Fc.nameText (Fc.binderName binder)) representation
        else throwLower ("GRIN does not support an unlifted recursive binding: " <> show (Fc.binderName binder))
    bindOne current (binding, vars) = bindLocal current (Fc.bindBinder binding) vars
    makeBindingNode recursiveEnv binding = makeThunk recursiveEnv (Fc.nameText (Fc.binderName (Fc.bindBinder binding))) (Fc.bindRhs binding)

lowerCase :: LowerEnv -> Fc.Expr -> Fc.Binder -> Fc.Type -> [Fc.Alt] -> LowerM GrinExpr
lowerCase env scrutinee binder _ alternatives = do
  representation <- expressionRuntimeRep env scrutinee
  case representation of
    TupleRep fields -> lowerTupleCase env scrutinee binder fields alternatives
    _ ->
      bindExpression env "case_value" scrutinee $ \case
        [value] -> do
          caseBinder <- freshVar (Fc.nameText (Fc.binderName binder)) representation
          loweredAlternatives <- mapM (lowerAlt (bindLocal env binder [caseBinder])) alternatives
          pure (GrinCase value caseBinder loweredAlternatives)
        _ -> throwLower "GRIN case expected one scrutinee value"

lowerTupleCase :: LowerEnv -> Fc.Expr -> Fc.Binder -> [GrinRep] -> [Fc.Alt] -> LowerM GrinExpr
lowerTupleCase env scrutinee binder _fields alternatives = do
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
      captures <- capturedVariables env expression
      functionName <- freshFunction (hint <> "_thunk")
      body <- lowerExpr env expression
      emitFunction
        GrinFunction
          { grinFunctionName = functionName,
            grinFunctionParameters = captures,
            grinFunctionResultRep = representation,
            grinFunctionBody = body
          }
      pure (GrinNode (GrinThunk functionName) (map GrinVarValue captures))

makeClosure :: LowerEnv -> Fc.Expr -> LowerM GrinNode
makeClosure env expression = do
  let (bodyEnv0, binders, body) = collectLambdas env expression
  captures <- capturedVariables env expression
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

collectLambdas :: LowerEnv -> Fc.Expr -> (LowerEnv, [Fc.Binder], Fc.Expr)
collectLambdas env expression =
  case expression of
    Fc.ExLam binder body ->
      let (bodyEnv, binders, result) = collectLambdas env body
       in (bodyEnv, binder : binders, result)
    Fc.ExTyLam binder body -> collectLambdas (extendTypeBinder env binder) body
    _ -> (env, [], expression)

capturedVariables :: LowerEnv -> Fc.Expr -> LowerM [GrinVar]
capturedVariables env expression =
  pure
    ( concat
        [ variables
        | name <- Set.toAscList (freeVariables expression),
          Just variables <- [Map.lookup name (lowerLocals env)]
        ]
    )

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
    _ -> expressionType env expression >>= runtimeRep env

expressionType :: LowerEnv -> Fc.Expr -> LowerM Fc.Type
expressionType env expression =
  case expression of
    Fc.ExVar name -> lookupNameType env name
    Fc.ExLit {} -> throwLower "GRIN cannot infer a source type for this literal"
    Fc.ExApp function _ -> do
      functionType <- expressionType env function
      case reduce env functionType of
        Fc.TyFun _ _ _ result -> pure result
        other -> throwLower ("GRIN application has a non-function type: " <> show other)
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

runtimeRep :: LowerEnv -> Fc.Type -> LowerM GrinRep
runtimeRep env sourceType = directRuntimeRep env appliedType
  where
    appliedType = applySubstitution env sourceType

directRuntimeRep :: LowerEnv -> Fc.Type -> LowerM GrinRep
directRuntimeRep env sourceType = do
  representation <-
    maybe
      (throwLower ("GRIN cannot find a runtime representation for type: " <> show sourceType))
      pure
      (TypeOf.repOf (lowerTypes env) sourceType)
  convertRep env representation

repType :: LowerEnv -> Fc.Type -> LowerM Fc.Type
repType env sourceType =
  maybe
    (throwLower ("GRIN cannot find a runtime representation type for: " <> show sourceType))
    pure
    (TypeOf.repOf (lowerTypes env) (applySubstitution env sourceType))

runtimeComponents :: LowerEnv -> Fc.Type -> LowerM [GrinRep]
runtimeComponents env sourceType = runtimeRepComponents <$> runtimeRep env sourceType

convertRep :: LowerEnv -> Fc.Type -> LowerM GrinRep
convertRep env sourceRep =
  case reduce env sourceRep of
    Fc.TyVar name -> throwLower ("GRIN does not support a variable runtime representation: " <> show name)
    Fc.TyCon name -> simpleRep (Fc.nameText name)
    Fc.TyApp (Fc.TyCon name) levity
      | Fc.nameText name == "BoxedRep" -> BoxedRep <$> convertLevity levity
    Fc.TyApp (Fc.TyCon name) fields
      | Fc.nameText name == "TupleRep" -> TupleRep <$> convertRepList env fields
      | Fc.nameText name == "SumRep" -> SumRep <$> convertRepList env fields
    Fc.TyApp (Fc.TyApp (Fc.TyCon name) count) element
      | Fc.nameText name == "VecRep" -> VecRep <$> readNamed "vector count" count <*> readNamed "vector element" element
    other -> throwLower ("GRIN does not support runtime representation: " <> show other)

simpleRep :: Text -> LowerM GrinRep
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
    _ -> throwLower ("GRIN does not know runtime representation: " <> T.unpack name)

convertLevity :: Fc.Type -> LowerM GrinLevity
convertLevity levity =
  case levity of
    Fc.TyCon name
      | Fc.nameText name == "Lifted" -> pure Lifted
      | Fc.nameText name == "Unlifted" -> pure Unlifted
    _ -> throwLower ("GRIN does not support levity: " <> show levity)

convertRepList :: LowerEnv -> Fc.Type -> LowerM [GrinRep]
convertRepList env list =
  case reduce env list of
    Fc.TyApp (Fc.TyCon name) _
      | Fc.nameText name == "[]" -> pure []
    Fc.TyApp (Fc.TyApp (Fc.TyApp (Fc.TyCon name) _) item) rest
      | Fc.nameText name == ":" -> (:) <$> convertRep env item <*> convertRepList env rest
    other -> throwLower ("GRIN does not support this runtime representation list: " <> show other)

readNamed :: (Read value) => String -> Fc.Type -> LowerM value
readNamed label ty =
  case ty of
    Fc.TyCon name ->
      maybe (throwLower ("GRIN does not know " <> label <> ": " <> T.unpack (Fc.nameText name))) pure (readMaybe (T.unpack (Fc.nameText name)))
    _ -> throwLower ("GRIN does not support " <> label <> ": " <> show ty)

literalRep :: LowerEnv -> Fc.Literal -> LowerM GrinRep
literalRep env literal =
  case literal of
    Fc.LitInt representation _ -> convertRep env representation
    Fc.LitChar representation _ -> convertRep env representation
    Fc.LitAddr {} -> pure AddrRep

lowerLiteral :: LowerEnv -> Fc.Literal -> LowerM GrinLiteral
lowerLiteral env literal =
  case literal of
    Fc.LitInt representation value -> GrinLitInt <$> convertRep env representation <*> pure value
    Fc.LitChar representation value -> GrinLitChar <$> convertRep env representation <*> pure value
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
freshVarsForType env (hint, sourceType) = runtimeRep env sourceType >>= freshVars hint

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
