{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Conservative lowering from System FC 2 to GRIN.
module Aihc.Grin.Lower
  ( lowerProgram,
  )
where

import Aihc.Fc2 qualified as Fc2
import Aihc.Fc2.TypeOf qualified as TypeOf
import Aihc.Fc2.Wired qualified as Wired
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
    lowerLocals :: !(Map Fc2.Name [GrinVar]),
    lowerTypeSubstitution :: !(Map Fc2.Name Fc2.Type),
    lowerGlobalNames :: !(Map Fc2.Name Text)
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

lowerProgram :: Fc2.Program -> Either String GrinProgram
lowerProgram program = do
  let types = TypeOf.typeEnvFromProgram program
      globals = globalNameTable types
      env = LowerEnv types Map.empty Map.empty globals
      initialState = LowerState (-1000000000) 0 []
  (parts, finalState) <- runStateT (foldMapM (lowerDecl env) (Fc2.programDecls program)) initialState
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

lowerDecl :: LowerEnv -> Fc2.Decl -> LowerM TopParts
lowerDecl env declaration =
  case declaration of
    Fc2.DeclType value -> lowerTypeDecl env value
    Fc2.DeclVal value -> lowerValueDecl env value
    Fc2.DeclForeignImport value -> lowerForeignDecl env value
    Fc2.DeclSynonym {} -> pure mempty
    Fc2.DeclAxiom {} -> pure mempty

lowerTypeDecl :: LowerEnv -> Fc2.TypeDecl -> LowerM TopParts
lowerTypeDecl env declaration = do
  converted <- mapM lowerConstructor (Fc2.typeCons declaration)
  pure
    mempty
      { topConstructors = concatMap first converted,
        topGlobals = concatMap second converted
      }
  where
    first (constructors, _) = constructors
    second (_, globals) = globals
    lowerConstructor constructor = do
      let name = Fc2.conName constructor
          (typeBinders, monotype) = splitForAlls (applySubstitution env (Fc2.conType constructor))
          constructorEnv = foldl extendTypeBinder env typeBinders
      if "(#" `T.isPrefixOf` Fc2.nameText name
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

lowerValueDecl :: LowerEnv -> Fc2.ValDecl -> LowerM TopParts
lowerValueDecl env declaration = do
  representation <- runtimeRep env (Fc2.valType declaration)
  if representation /= liftedGrinRep
    then throwLower ("GRIN does not support an unlifted top-level value: " <> show (Fc2.valName declaration))
    else do
      globalName <- lookupGlobalName env (Fc2.valName declaration)
      node <- makeThunk env (Fc2.nameText (Fc2.valName declaration)) (Fc2.valBody declaration)
      pure mempty {topGlobals = [(globalName, node)]}

lowerForeignDecl :: LowerEnv -> Fc2.ForeignImportDecl -> LowerM TopParts
lowerForeignDecl env declaration = do
  let name = Fc2.foreignImportName declaration
      sourceType = applySubstitution env (Fc2.foreignImportType declaration)
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
  functionName <- freshFunction (Fc2.nameText name <> "_foreign")
  globalName <- lookupGlobalName env name
  let parameters = concat argumentGroups
      layouts = map (map grinVarRuntimeRep) argumentGroups
      valueGroups = map (map GrinVarValue) argumentGroups
      arity = length argumentTypes
  (body, primitives, foreignCalls) <-
    case Fc2.foreignImportCallingConvention declaration of
      Fc2.Prim -> do
        expression <- lowerPrimitiveBody resultRep (Fc2.nameText name) valueGroups
        let primitive =
              [ (GrinVar (Fc2.nameText name) (-2000000000 + arity) resultRep, arity)
              | Fc2.nameText name `notElem` compilerPrimitives
              ]
        pure (expression, primitive, [])
      Fc2.CCall specification -> do
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

foreignAxiomDeclarations :: LowerEnv -> Fc2.ForeignImportDecl -> LowerM [Fc2.AxiomDecl]
foreignAxiomDeclarations env declaration =
  mapM lookupAxiom [name | Fc2.ForeignAxiom name <- Fc2.foreignImportDependencies declaration]
  where
    lookupAxiom name =
      case Map.lookup name (TypeOf.teAxioms (lowerTypes env)) of
        Just axiom -> pure axiom
        Nothing -> throwLower ("GRIN cannot find an explicit foreign axiom: " <> show name)

foreignConstructorNames :: Fc2.ForeignImportDecl -> [Fc2.Name]
foreignConstructorNames declaration =
  [name | Fc2.ForeignConstructor name <- Fc2.foreignImportDependencies declaration]

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

lowerForeignBody :: LowerEnv -> [Fc2.AxiomDecl] -> [Fc2.Name] -> GrinForeignCall -> [Fc2.Type] -> [[GrinValue]] -> Fc2.Type -> LowerM GrinExpr
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

sourceValues :: LowerEnv -> Fc2.Type -> [GrinValue] -> LowerM [(Fc2.Type, GrinValue)]
sourceValues env sourceType values = do
  types <- sourceValueTypes env sourceType
  if length types == length values
    then pure (zip (map fst types) values)
    else throwLower ("GRIN cannot match source values to type: " <> show sourceType)

sourceValueTypes :: LowerEnv -> Fc2.Type -> LowerM [(Fc2.Type, GrinRep)]
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

adaptForeignOperands :: LowerEnv -> [Fc2.AxiomDecl] -> [Fc2.Name] -> [((Fc2.Type, GrinValue), GrinRep)] -> ([GrinValue] -> LowerM GrinExpr) -> LowerM GrinExpr
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

adaptForeignResult :: LowerEnv -> [Fc2.AxiomDecl] -> [Fc2.Name] -> Fc2.Type -> GrinRep -> GrinRep -> GrinExpr -> LowerM GrinExpr
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

findUnaryConstructor :: LowerEnv -> [Fc2.AxiomDecl] -> [Fc2.Name] -> Fc2.Type -> GrinRep -> LowerM (Text, GrinRep)
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
      | Fc2.nameSort name /= Fc2.SortDataConstructor = Nothing
      | otherwise = do
          fieldTypes <- instantiateConstructorFields env axioms constructorType resultType
          case fieldTypes of
            [fieldType] ->
              case runStateT (runtimeRep env fieldType) (LowerState 0 0 []) of
                Right (fieldRep, _)
                  | fieldRep == expectedRep -> Just (constructorTag name, fieldRep)
                _ -> Nothing
            _ -> Nothing

instantiateConstructorFields :: LowerEnv -> [Fc2.AxiomDecl] -> Fc2.Type -> Fc2.Type -> Maybe [Fc2.Type]
instantiateConstructorFields env axioms constructorType targetType = do
  let (binders, monotype) = splitForAlls constructorType
  (fieldTypes, constructorResult) <- either (const Nothing) Just (splitFunctionType monotype)
  substitution <- matchTypeBinders env (Map.fromList [(Fc2.binderName binder, Nothing) | binder <- binders]) constructorResult (applyForeignAxioms env axioms targetType)
  resolved <- sequenceA substitution
  pure (map (TypeOf.substTypes resolved) fieldTypes)

matchTypeBinders :: LowerEnv -> Map Fc2.Name (Maybe Fc2.Type) -> Fc2.Type -> Fc2.Type -> Maybe (Map Fc2.Name (Maybe Fc2.Type))
matchTypeBinders env substitution patternType actualType =
  case (reduce env patternType, reduce env actualType) of
    (Fc2.TyVar name, actual)
      | Just current <- Map.lookup name substitution ->
          case current of
            Nothing -> Just (Map.insert name (Just actual) substitution)
            Just previous
              | TypeOf.typesEqual (lowerTypes env) previous actual -> Just substitution
              | otherwise -> Nothing
    (Fc2.TyVar name, Fc2.TyVar actualName)
      | name == actualName -> Just substitution
    (Fc2.TyCon name, Fc2.TyCon actualName)
      | name == actualName -> Just substitution
    (Fc2.TyApp function argument, Fc2.TyApp actualFunction actualArgument) ->
      matchTypeBinders env substitution function actualFunction
        >>= \next -> matchTypeBinders env next argument actualArgument
    (Fc2.TyFun r1 r2 argument result, Fc2.TyFun actualR1 actualR2 actualArgument actualResult) ->
      matchTypeBinders env substitution r1 actualR1
        >>= \s1 ->
          matchTypeBinders env s1 r2 actualR2
            >>= \s2 ->
              matchTypeBinders env s2 argument actualArgument
                >>= \s3 -> matchTypeBinders env s3 result actualResult
    (Fc2.TyEq left right, Fc2.TyEq actualLeft actualRight) ->
      matchTypeBinders env substitution left actualLeft
        >>= \next -> matchTypeBinders env next right actualRight
    _ -> Nothing

collectTypeApplications :: Fc2.Type -> (Fc2.Type, [Fc2.Type])
collectTypeApplications = go []
  where
    go arguments (Fc2.TyApp function argument) = go (argument : arguments) function
    go arguments function = (function, arguments)

lowerExpr :: LowerEnv -> Fc2.Expr -> LowerM GrinExpr
lowerExpr env expression =
  case expression of
    Fc2.ExVar name -> lowerVariable env name
    Fc2.ExLit literal -> GrinConstant . pure . GrinLitValue <$> lowerLiteral env literal
    Fc2.ExApp function argument -> lowerApplication env function argument
    Fc2.ExTyApp (Fc2.ExTyLam binder body) argument ->
      lowerExpr env {lowerTypeSubstitution = Map.insert (Fc2.binderName binder) (applySubstitution env argument) (lowerTypeSubstitution env)} body
    Fc2.ExTyApp function _ -> lowerExpr env function
    Fc2.ExLam {} -> GrinStore <$> makeClosure env expression
    Fc2.ExTyLam binder body -> lowerExpr (extendTypeBinder env binder) body
    Fc2.ExLet binding body -> lowerLet env binding body
    Fc2.ExRec bindings body -> lowerRec env bindings body
    Fc2.ExCase scrutinee binder resultType alternatives -> lowerCase env scrutinee binder resultType alternatives
    Fc2.ExCast inner _ -> lowerExpr env inner

lowerVariable :: LowerEnv -> Fc2.Name -> LowerM GrinExpr
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

lowerApplication :: LowerEnv -> Fc2.Expr -> Fc2.Expr -> LowerM GrinExpr
lowerApplication env function argument = do
  let application = Fc2.ExApp function argument
  resultType <- expressionType env application
  resultRep <- runtimeRep env resultType
  case (resultRep, collectApplications application) of
    (_, (Fc2.ExVar name, arguments))
      | Just arity <- Map.lookup (Fc2.nameText name) specialPrimitiveArities,
        length arguments == arity ->
          lowerSpecialApplication env resultRep (Fc2.nameText name) arguments
    (TupleRep {}, (Fc2.ExVar name, arguments))
      | "(#" `T.isPrefixOf` Fc2.nameText name -> lowerTupleArguments env arguments
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

collectApplications :: Fc2.Expr -> (Fc2.Expr, [Fc2.Expr])
collectApplications expression = go expression []
  where
    go (Fc2.ExApp function argument) arguments = go function (argument : arguments)
    go (Fc2.ExTyApp function _) arguments = go function arguments
    go (Fc2.ExCast function _) arguments = go function arguments
    go function arguments = (function, arguments)

lowerTupleArguments :: LowerEnv -> [Fc2.Expr] -> LowerM GrinExpr
lowerTupleArguments env = go []
  where
    go values [] = pure (GrinConstant values)
    go values (argument : arguments) =
      lowerArgument env argument (\newValues -> go (values <> newValues) arguments)

specialPrimitiveArities :: Map Text Int
specialPrimitiveArities = Map.fromList [("aihcExit#", 2), ("unsafeCoerce#", 1), ("raise#", 1), ("catch#", 3), ("seq", 2)]

lowerSpecialApplication :: LowerEnv -> GrinRep -> Text -> [Fc2.Expr] -> LowerM GrinExpr
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

lowerArgument :: LowerEnv -> Fc2.Expr -> ([GrinValue] -> LowerM GrinExpr) -> LowerM GrinExpr
lowerArgument env expression continuation = do
  representation <- expressionRuntimeRep env expression
  if null (runtimeRepComponents representation)
    then continuation []
    else
      if isLiftedRuntimeRep representation
        then lowerLazySingle env "argument" expression (continuation . (: []))
        else bindExpression env "argument" expression continuation

lowerLazySingle :: LowerEnv -> Text -> Fc2.Expr -> (GrinValue -> LowerM GrinExpr) -> LowerM GrinExpr
lowerLazySingle env hint expression continuation =
  case expression of
    Fc2.ExVar name ->
      case Map.lookup name (lowerLocals env) of
        Just [variable] -> continuation (GrinVarValue variable)
        Just _ -> throwLower ("GRIN expected one lazy local value: " <> show name)
        Nothing -> lookupGlobalName env name >>= continuation . GrinGlobalValue
    Fc2.ExTyApp inner _ -> lowerLazySingle env hint inner continuation
    Fc2.ExCast inner _ -> lowerLazySingle env hint inner continuation
    _ -> do
      node <- makeThunk env hint expression
      pointer <- freshVar hint liftedGrinRep
      rest <- continuation (GrinVarValue pointer)
      pure (GrinBind [pointer] (GrinStore node) rest)

bindExpression :: LowerEnv -> Text -> Fc2.Expr -> ([GrinValue] -> LowerM GrinExpr) -> LowerM GrinExpr
bindExpression env hint expression continuation = do
  representation <- expressionRuntimeRep env expression
  variables <- freshVars hint representation
  valueExpression <- lowerExpr env expression
  rest <- continuation (map GrinVarValue variables)
  pure (bindIfNeeded variables valueExpression rest)

lowerLet :: LowerEnv -> Fc2.Bind -> Fc2.Expr -> LowerM GrinExpr
lowerLet env binding body = do
  let binder = Fc2.bindBinder binding
  representation <- runtimeRep env (applySubstitution env (Fc2.binderType binder))
  variables <- freshVars (Fc2.nameText (Fc2.binderName binder)) representation
  let bodyEnv = bindLocal env binder variables
  loweredBody <- lowerExpr bodyEnv body
  if isLiftedRuntimeRep representation
    then do
      node <- makeThunk env (Fc2.nameText (Fc2.binderName binder)) (Fc2.bindRhs binding)
      pure (GrinBind variables (GrinStore node) loweredBody)
    else do
      loweredRhs <- lowerExpr env (Fc2.bindRhs binding)
      pure (bindIfNeeded variables loweredRhs loweredBody)

lowerRec :: LowerEnv -> [Fc2.Bind] -> Fc2.Expr -> LowerM GrinExpr
lowerRec env bindings body = do
  variables <- mapM makeVariables bindings
  let recursiveEnv = foldl bindOne env (zip bindings variables)
  nodes <- mapM (makeBindingNode recursiveEnv) bindings
  loweredBody <- lowerExpr recursiveEnv body
  pure (GrinStoreRec (zip (concat variables) nodes) loweredBody)
  where
    makeVariables binding = do
      let binder = Fc2.bindBinder binding
      representation <- runtimeRep env (applySubstitution env (Fc2.binderType binder))
      if isLiftedRuntimeRep representation
        then (: []) <$> freshVar (Fc2.nameText (Fc2.binderName binder)) representation
        else throwLower ("GRIN does not support an unlifted recursive binding: " <> show (Fc2.binderName binder))
    bindOne current (binding, vars) = bindLocal current (Fc2.bindBinder binding) vars
    makeBindingNode recursiveEnv binding = makeThunk recursiveEnv (Fc2.nameText (Fc2.binderName (Fc2.bindBinder binding))) (Fc2.bindRhs binding)

lowerCase :: LowerEnv -> Fc2.Expr -> Fc2.Binder -> Fc2.Type -> [Fc2.Alt] -> LowerM GrinExpr
lowerCase env scrutinee binder _ alternatives = do
  representation <- expressionRuntimeRep env scrutinee
  case representation of
    TupleRep fields -> lowerTupleCase env scrutinee binder fields alternatives
    _ ->
      bindExpression env "case_value" scrutinee $ \case
        [value] -> do
          caseBinder <- freshVar (Fc2.nameText (Fc2.binderName binder)) representation
          loweredAlternatives <- mapM (lowerAlt (bindLocal env binder [caseBinder])) alternatives
          pure (GrinCase value caseBinder loweredAlternatives)
        _ -> throwLower "GRIN case expected one scrutinee value"

lowerTupleCase :: LowerEnv -> Fc2.Expr -> Fc2.Binder -> [GrinRep] -> [Fc2.Alt] -> LowerM GrinExpr
lowerTupleCase env scrutinee binder _fields alternatives = do
  alternative <-
    case alternatives of
      first : _ -> pure first
      [] -> throwLower "GRIN cannot lower an empty unboxed tuple case"
  let typeEnv = foldl extendTypeBinder env (Fc2.altTypeBinders alternative)
  fieldVariables <- mapM (freshVarsForBinder typeEnv) (Fc2.altBinders alternative)
  let values = concat fieldVariables
      binderEnv = bindLocal typeEnv binder values
      alternativeEnv = foldl bindPair binderEnv (zip (Fc2.altBinders alternative) fieldVariables)
  loweredRhs <- lowerExpr alternativeEnv (Fc2.altRhs alternative)
  loweredScrutinee <- lowerExpr env scrutinee
  pure (bindIfNeeded values loweredScrutinee loweredRhs)
  where
    bindPair current (fieldBinder, vars) = bindLocal current fieldBinder vars

lowerAlt :: LowerEnv -> Fc2.Alt -> LowerM GrinAlt
lowerAlt env alternative = do
  let typeEnv = foldl extendTypeBinder env (Fc2.altTypeBinders alternative)
  binderGroups <- mapM (freshVarsForBinder typeEnv) (Fc2.altBinders alternative)
  let bodyEnv = foldl bindPair typeEnv (zip (Fc2.altBinders alternative) binderGroups)
  body <- lowerExpr bodyEnv (Fc2.altRhs alternative)
  alternativeConstructor <- lowerAltCon typeEnv (Fc2.altCon alternative)
  pure
    GrinAlt
      { grinAltCon = alternativeConstructor,
        grinAltBinders = concat binderGroups,
        grinAltRhs = body
      }
  where
    bindPair current (binder, vars) = bindLocal current binder vars

lowerAltCon :: LowerEnv -> Fc2.AltCon -> LowerM GrinAltCon
lowerAltCon env alternative =
  case alternative of
    Fc2.AltData name -> pure (GrinDataAlt (constructorTag name))
    Fc2.AltLit literal -> GrinLitAlt <$> lowerLiteral env literal
    Fc2.AltDefault -> pure GrinDefaultAlt

makeThunk :: LowerEnv -> Text -> Fc2.Expr -> LowerM GrinNode
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

makeClosure :: LowerEnv -> Fc2.Expr -> LowerM GrinNode
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

collectLambdas :: LowerEnv -> Fc2.Expr -> (LowerEnv, [Fc2.Binder], Fc2.Expr)
collectLambdas env expression =
  case expression of
    Fc2.ExLam binder body ->
      let (bodyEnv, binders, result) = collectLambdas env body
       in (bodyEnv, binder : binders, result)
    Fc2.ExTyLam binder body -> collectLambdas (extendTypeBinder env binder) body
    _ -> (env, [], expression)

capturedVariables :: LowerEnv -> Fc2.Expr -> LowerM [GrinVar]
capturedVariables env expression =
  pure
    ( concat
        [ variables
        | name <- Set.toAscList (freeVariables expression),
          Just variables <- [Map.lookup name (lowerLocals env)]
        ]
    )

freeVariables :: Fc2.Expr -> Set Fc2.Name
freeVariables expression =
  case expression of
    Fc2.ExVar name -> Set.singleton name
    Fc2.ExLit {} -> Set.empty
    Fc2.ExApp function argument -> freeVariables function <> freeVariables argument
    Fc2.ExTyApp function _ -> freeVariables function
    Fc2.ExLam binder body -> Set.delete (Fc2.binderName binder) (freeVariables body)
    Fc2.ExTyLam _ body -> freeVariables body
    Fc2.ExLet binding body -> freeVariables (Fc2.bindRhs binding) <> Set.delete (Fc2.binderName (Fc2.bindBinder binding)) (freeVariables body)
    Fc2.ExRec bindings body ->
      let names = Set.fromList (map (Fc2.binderName . Fc2.bindBinder) bindings)
       in (foldMap (freeVariables . Fc2.bindRhs) bindings <> freeVariables body) `Set.difference` names
    Fc2.ExCase scrutinee binder _ alternatives ->
      freeVariables scrutinee
        <> Set.delete (Fc2.binderName binder) (foldMap freeAltVariables alternatives)
    Fc2.ExCast inner _ -> freeVariables inner

freeAltVariables :: Fc2.Alt -> Set Fc2.Name
freeAltVariables alternative =
  freeVariables (Fc2.altRhs alternative)
    `Set.difference` Set.fromList (map Fc2.binderName (Fc2.altBinders alternative))

expressionRuntimeRep :: LowerEnv -> Fc2.Expr -> LowerM GrinRep
expressionRuntimeRep env expression =
  case expression of
    Fc2.ExLit literal -> literalRep env literal
    _ -> expressionType env expression >>= runtimeRep env

expressionType :: LowerEnv -> Fc2.Expr -> LowerM Fc2.Type
expressionType env expression =
  case expression of
    Fc2.ExVar name -> lookupNameType env name
    Fc2.ExLit {} -> throwLower "GRIN cannot infer a source type for this literal"
    Fc2.ExApp function _ -> do
      functionType <- expressionType env function
      case reduce env functionType of
        Fc2.TyFun _ _ _ result -> pure result
        other -> throwLower ("GRIN application has a non-function type: " <> show other)
    Fc2.ExTyApp function argument -> do
      functionType <- expressionType env function
      case reduce env functionType of
        Fc2.TyForAll binder body -> pure (TypeOf.substType (Fc2.binderName binder) (applySubstitution env argument) body)
        other -> throwLower ("GRIN type application has a non-forall type: " <> show other)
    Fc2.ExLam binder body -> do
      bodyType <- expressionType (extendTypeBinder env binder) body
      argumentRep <- repType env (Fc2.binderType binder)
      resultRep <- repType env bodyType
      pure (Fc2.TyFun argumentRep resultRep (applySubstitution env (Fc2.binderType binder)) bodyType)
    Fc2.ExTyLam binder body -> Fc2.TyForAll binder <$> expressionType (extendTypeBinder env binder) body
    Fc2.ExLet binding body -> expressionType (extendTermBinder (Fc2.bindBinder binding) env) body
    Fc2.ExRec bindings body -> expressionType (foldl (flip (extendTermBinder . Fc2.bindBinder)) env bindings) body
    Fc2.ExCase _ _ resultType _ -> pure (applySubstitution env resultType)
    Fc2.ExCast _ coercion ->
      case TypeOf.coercionEndpoints (lowerTypes env) coercion of
        Just (_, target) -> pure (applySubstitution env target)
        Nothing -> throwLower ("GRIN cannot determine coercion endpoints: " <> show coercion)

runtimeRep :: LowerEnv -> Fc2.Type -> LowerM GrinRep
runtimeRep env sourceType = directRuntimeRep env appliedType
  where
    appliedType = applySubstitution env sourceType

directRuntimeRep :: LowerEnv -> Fc2.Type -> LowerM GrinRep
directRuntimeRep env sourceType = do
  representation <-
    maybe
      (throwLower ("GRIN cannot find a runtime representation for type: " <> show sourceType))
      pure
      (TypeOf.repOf (lowerTypes env) sourceType)
  convertRep env representation

repType :: LowerEnv -> Fc2.Type -> LowerM Fc2.Type
repType env sourceType =
  maybe
    (throwLower ("GRIN cannot find a runtime representation type for: " <> show sourceType))
    pure
    (TypeOf.repOf (lowerTypes env) (applySubstitution env sourceType))

runtimeComponents :: LowerEnv -> Fc2.Type -> LowerM [GrinRep]
runtimeComponents env sourceType = runtimeRepComponents <$> runtimeRep env sourceType

convertRep :: LowerEnv -> Fc2.Type -> LowerM GrinRep
convertRep env sourceRep =
  case reduce env sourceRep of
    Fc2.TyVar name -> throwLower ("GRIN does not support a variable runtime representation: " <> show name)
    Fc2.TyCon name -> simpleRep (Fc2.nameText name)
    Fc2.TyApp (Fc2.TyCon name) levity
      | Fc2.nameText name == "BoxedRep" -> BoxedRep <$> convertLevity levity
    Fc2.TyApp (Fc2.TyCon name) fields
      | Fc2.nameText name == "TupleRep" -> TupleRep <$> convertRepList env fields
      | Fc2.nameText name == "SumRep" -> SumRep <$> convertRepList env fields
    Fc2.TyApp (Fc2.TyApp (Fc2.TyCon name) count) element
      | Fc2.nameText name == "VecRep" -> VecRep <$> readNamed "vector count" count <*> readNamed "vector element" element
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

convertLevity :: Fc2.Type -> LowerM GrinLevity
convertLevity levity =
  case levity of
    Fc2.TyCon name
      | Fc2.nameText name == "Lifted" -> pure Lifted
      | Fc2.nameText name == "Unlifted" -> pure Unlifted
    _ -> throwLower ("GRIN does not support levity: " <> show levity)

convertRepList :: LowerEnv -> Fc2.Type -> LowerM [GrinRep]
convertRepList env list =
  case reduce env list of
    Fc2.TyApp (Fc2.TyCon name) _
      | Fc2.nameText name == "[]" -> pure []
    Fc2.TyApp (Fc2.TyApp (Fc2.TyApp (Fc2.TyCon name) _) item) rest
      | Fc2.nameText name == ":" -> (:) <$> convertRep env item <*> convertRepList env rest
    other -> throwLower ("GRIN does not support this runtime representation list: " <> show other)

readNamed :: (Read value) => String -> Fc2.Type -> LowerM value
readNamed label ty =
  case ty of
    Fc2.TyCon name ->
      maybe (throwLower ("GRIN does not know " <> label <> ": " <> T.unpack (Fc2.nameText name))) pure (readMaybe (T.unpack (Fc2.nameText name)))
    _ -> throwLower ("GRIN does not support " <> label <> ": " <> show ty)

literalRep :: LowerEnv -> Fc2.Literal -> LowerM GrinRep
literalRep env literal =
  case literal of
    Fc2.LitInt representation _ -> convertRep env representation
    Fc2.LitChar representation _ -> convertRep env representation
    Fc2.LitAddr {} -> pure AddrRep

lowerLiteral :: LowerEnv -> Fc2.Literal -> LowerM GrinLiteral
lowerLiteral env literal =
  case literal of
    Fc2.LitInt representation value -> GrinLitInt <$> convertRep env representation <*> pure value
    Fc2.LitChar representation value -> GrinLitChar <$> convertRep env representation <*> pure value
    Fc2.LitAddr _ value -> pure (GrinLitAddr value)

lowerForeignCall :: Fc2.Name -> Fc2.CCallSpec -> GrinForeignCall
lowerForeignCall name specification =
  GrinForeignCall
    { grinForeignCallName = Fc2.nameText name,
      grinForeignCallSymbol = Fc2.ccallSymbol specification,
      grinForeignCallSignature =
        GrinForeignSignature
          { grinForeignArgumentTypes = map lowerForeignType (Fc2.ccallArgumentTypes specification),
            grinForeignResultType = lowerForeignType (Fc2.ccallResultType specification),
            grinForeignEffect =
              case Fc2.ccallEffect specification of
                Fc2.ForeignPure -> GrinForeignPure
                Fc2.ForeignRealWorld -> GrinForeignRealWorld
          }
    }

lowerForeignType :: Fc2.CAbiType -> GrinForeignType
lowerForeignType foreignType =
  case foreignType of
    Fc2.CAbiInt -> GrinForeignInt
    Fc2.CAbiInt32 -> GrinForeignInt32
    Fc2.CAbiWord64 -> GrinForeignWord64
    Fc2.CAbiAddr -> GrinForeignAddr

splitFunctionType :: Fc2.Type -> Either String ([Fc2.Type], Fc2.Type)
splitFunctionType sourceType =
  case sourceType of
    Fc2.TyForAll _ body -> splitFunctionType body
    Fc2.TyFun _ _ argument result -> do
      (arguments, finalResult) <- splitFunctionType result
      pure (argument : arguments, finalResult)
    _ -> pure ([], sourceType)

splitOperationalFunctionType :: LowerEnv -> [Fc2.AxiomDecl] -> Fc2.Type -> LowerM ([Fc2.Type], Fc2.Type)
splitOperationalFunctionType env axioms sourceType =
  case reduce env sourceType of
    Fc2.TyForAll binder body -> splitOperationalFunctionType (extendTypeBinder env binder) axioms body
    Fc2.TyFun _ _ argument result -> do
      (arguments, finalResult) <- splitOperationalFunctionType env axioms result
      pure (argument : arguments, finalResult)
    other ->
      let unwrapped = applyForeignAxioms env axioms other
       in if TypeOf.typesEqual (lowerTypes env) other unwrapped
            then pure ([], other)
            else splitOperationalFunctionType env axioms unwrapped

applyForeignAxioms :: LowerEnv -> [Fc2.AxiomDecl] -> Fc2.Type -> Fc2.Type
applyForeignAxioms env axioms = go Set.empty
  where
    go visited sourceType
      | sourceType `Set.member` visited = sourceType
      | otherwise =
          case listToMaybe (mapMaybe (\axiom -> TypeOf.applyRepresentationalAxiom (lowerTypes env) axiom sourceType) axioms) of
            Just target -> go (Set.insert sourceType visited) target
            Nothing -> sourceType

splitForAlls :: Fc2.Type -> ([Fc2.Binder], Fc2.Type)
splitForAlls sourceType =
  case sourceType of
    Fc2.TyForAll binder body ->
      let (binders, result) = splitForAlls body
       in (binder : binders, result)
    _ -> ([], sourceType)

constructorArgumentTypes :: Fc2.Type -> Either String [Fc2.Type]
constructorArgumentTypes sourceType = fst <$> splitFunctionType sourceType

constructorResultType :: Fc2.Type -> Either String Fc2.Type
constructorResultType sourceType = snd <$> splitFunctionType sourceType

globalNameTable :: TypeOf.TypeEnv -> Map Fc2.Name Text
globalNameTable types =
  Map.fromList
    [ (name, stableGlobalName name)
    | name <- Map.keys (TypeOf.teHeaders types),
      Fc2.nameSort name `elem` [Fc2.SortValue, Fc2.SortDataConstructor]
    ]

stableGlobalName :: Fc2.Name -> Text
stableGlobalName name =
  case Fc2.nameOrigin name of
    Fc2.OriginTop (PackageId packageName) moduleName ->
      T.intercalate "\0" [packageName, moduleName, Fc2.nameText name]
    Fc2.OriginLocal (Unique unique) -> Fc2.nameText name <> "\0" <> T.pack (show unique)

constructorTag :: Fc2.Name -> Text
constructorTag name =
  case Fc2.nameOrigin name of
    Fc2.OriginTop (PackageId packageName) moduleName ->
      (if packageName == "" then "" else packageName <> ":") <> moduleName <> "." <> Fc2.nameText name
    Fc2.OriginLocal {} -> Fc2.nameText name

lookupGlobalName :: LowerEnv -> Fc2.Name -> LowerM Text
lookupGlobalName env name =
  maybe (throwLower ("GRIN has no global name for: " <> show name)) pure (Map.lookup name (lowerGlobalNames env))

lookupNameType :: LowerEnv -> Fc2.Name -> LowerM Fc2.Type
lookupNameType env name =
  case Map.lookup name (TypeOf.teBinders (lowerTypes env)) <|> TypeOf.lookupHeaderType (lowerTypes env) name of
    Just sourceType -> pure (applySubstitution env sourceType)
    Nothing -> throwLower ("GRIN has no type for: " <> show name)

applySubstitution :: LowerEnv -> Fc2.Type -> Fc2.Type
applySubstitution env = TypeOf.substTypes (lowerTypeSubstitution env)

reduce :: LowerEnv -> Fc2.Type -> Fc2.Type
reduce env = TypeOf.reduceType (lowerTypes env) . applySubstitution env

extendTypeBinder :: LowerEnv -> Fc2.Binder -> LowerEnv
extendTypeBinder env binder = env {lowerTypes = TypeOf.extendBinder (lowerTypes env) binder}

defaultRuntimeReps :: LowerEnv -> [Fc2.Binder] -> LowerEnv
defaultRuntimeReps = foldl defaultOne
  where
    defaultOne env binder =
      case reduce env (Fc2.binderType binder) of
        Fc2.TyCon name
          | Fc2.nameText name == "RuntimeRep",
            Just package <- TypeOf.tePrimPackage (lowerTypes env) ->
              env
                { lowerTypeSubstitution =
                    Map.insert
                      (Fc2.binderName binder)
                      (Fc2.TyCon (Wired.liftedRepName package))
                      (lowerTypeSubstitution env)
                }
        _ -> env

extendTermBinder :: Fc2.Binder -> LowerEnv -> LowerEnv
extendTermBinder binder env = env {lowerTypes = TypeOf.extendBinder (lowerTypes env) binder}

bindLocal :: LowerEnv -> Fc2.Binder -> [GrinVar] -> LowerEnv
bindLocal env binder variables =
  (extendTermBinder binder env)
    { lowerLocals = Map.insert (Fc2.binderName binder) variables (lowerLocals env)
    }

freshVarsForBinder :: LowerEnv -> Fc2.Binder -> LowerM [GrinVar]
freshVarsForBinder env binder = freshVarsForType env (Fc2.nameText (Fc2.binderName binder), applySubstitution env (Fc2.binderType binder))

freshVarsForType :: LowerEnv -> (Text, Fc2.Type) -> LowerM [GrinVar]
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

bindIfNeeded :: [GrinVar] -> GrinExpr -> GrinExpr -> GrinExpr
bindIfNeeded variables valueExpression body
  | null variables = GrinBind [] valueExpression body
  | otherwise = GrinBind variables valueExpression body

liftEither :: Either String value -> LowerM value
liftEither = either throwLower pure

throwLower :: String -> LowerM value
throwLower = lift . Left
