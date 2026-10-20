{-# LANGUAGE OverloadedStrings #-}

-- | Bounded specialization on dictionary and function identities.
module Aihc.Fc.SuperSpecialise (SuperReport (..), superSpecialiseProgram) where

import Aihc.Fc.Name
import Aihc.Fc.Simplify (collectSpine, freshenExprFrom, maxLocalUnique, substExpr, substTypeExpr)
import Aihc.Fc.Syntax
import Aihc.Fc.Tidy (tidyProgram)
import Aihc.Fc.TypeOf (substType)
import Aihc.Tc.Types (Unique (..))
import Control.Monad.Trans.State.Strict (State, get, modify', runState)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T

data SuperReport = SuperReport
  { superCopies :: !Int,
    superCalls :: !Int,
    superLimits :: !Int
  }
  deriving (Eq, Show)

type Arg = Either Type Expr

type Key = (Name, [Maybe Arg])

type Definitions = Map Name Bind

-- | A region owns its copies. Local copies cannot escape their lexical scope.
data Region = Region
  { regionScope :: !(Set Name),
    regionLocal :: !Bool,
    regionCache :: !(Map Key Name),
    regionCopies :: ![Bind]
  }

data Supply = Supply
  { nextUnique :: !Int,
    remainingNodes :: !Int,
    copyCounts :: !(Map Name Int),
    currentRegion :: !Region,
    report :: !SuperReport
  }

type SpecM = State Supply

-- | The node budget is half the input size, between 256 and 100000 nodes.
-- Each original function permits at most sixteen copies across all regions.
superSpecialiseProgram :: Program -> (Program, SuperReport)
superSpecialiseProgram program =
  let declarations = programDecls program
      protected = foldMap ruleNames declarations
      definitions = Map.fromList [(valName val, Bind (Binder (valName val) (valType val)) (valBody val)) | DeclVal val <- declarations, eligible protected val]
      size = sum [nodes (valBody val) | DeclVal val <- declarations]
      initial = Supply (maxLocalUnique program + 1) (min 100000 (max 256 (size `div` 2))) Map.empty (Region Set.empty False Map.empty []) (SuperReport 0 0 0)
      (rewritten, final) = runState (mapM (onDecl definitions) declarations) initial
      copies = [DeclVal (ValDecl Private (binderName binder) (binderType binder) body InlineDefault) | Bind binder body <- reverse (regionCopies (currentRegion final))]
   in (tidyProgram program {programDecls = rewritten <> copies}, report final)
  where
    onDecl definitions decl = case decl of
      DeclVal val -> do
        body <- walk definitions Set.empty (valBody val)
        pure (DeclVal val {valBody = body})
      _ -> pure decl
    ruleNames (DeclRule rule) = exprFreeNames (ruleLhs rule) <> exprFreeNames (ruleRhs rule)
    ruleNames _ = Set.empty
    eligible protected val =
      Set.notMember (valName val) protected
        && case valInline val of
          InlineDefault -> True
          InlineAlways AlwaysActive -> True
          InlineWhenUseful AlwaysActive -> True
          _ -> False

walk :: Definitions -> Set Name -> Expr -> SpecM Expr
walk definitions scope expression = case expression of
  ExVar {} -> pure expression
  ExLit {} -> pure expression
  ExCoercion {} -> pure expression
  ExApp {} -> application
  ExTyApp {} -> application
  ExLam binder body -> ExLam binder <$> walk definitions (bindScope binder scope) body
  ExTyLam binder body -> ExTyLam binder <$> walk definitions (bindScope binder scope) body
  ExLet binding body -> do
    rhs <- walk definitions scope (bindRhs binding)
    body' <- walk (addDefinition binding definitions) (bindScope (bindBinder binding) scope) body
    pure (ExLet binding {bindRhs = rhs} body')
  ExRec bindings body -> do
    let innerDefinitions = List.foldl' (flip addDefinition) definitions bindings
        innerScope = scope <> Set.fromList (map (binderName . bindBinder) bindings)
    bindings' <- mapM (\binding -> do rhs <- walk innerDefinitions innerScope (bindRhs binding); pure binding {bindRhs = rhs}) bindings
    body' <- walk innerDefinitions innerScope body
    pure (ExRec bindings' body')
  ExCase scrutinee binder alternatives -> do
    scrutinee' <- walk definitions scope scrutinee
    let outer = scope <> foldMap (Set.singleton . binderName) binder
    alternatives' <- mapM (\alt -> do rhs <- walk definitions (outer <> Set.fromList (map binderName (altTypeBinders alt <> altBinders alt))) (altRhs alt); pure alt {altRhs = rhs}) alternatives
    pure (ExCase scrutinee' binder alternatives')
  ExAbsurd scrutinee ty -> (`ExAbsurd` ty) <$> walk definitions scope scrutinee
  ExCast body coercion -> (`ExCast` coercion) <$> walk definitions scope body
  ExForeignCall call types arguments -> ExForeignCall call types <$> mapM (walk definitions scope) arguments
  where
    application = do
      let (function, arguments) = collectSpine expression
      function' <- walk definitions scope function
      arguments' <- mapM (either (pure . Left) (fmap Right . walk definitions scope)) arguments
      case function' of
        ExVar name | Just binding <- Map.lookup name definitions -> do
          (lets, named) <- nameFunctions (bindRhs binding) arguments'
          call <- specializeCall (List.foldl' (flip addDefinition) definitions lets) (scope <> Set.fromList (map (binderName . bindBinder) lets)) function' named
          pure (foldr ExLet call lets)
        _ -> pure (rebuild function' arguments')

-- | Name a function argument once. A local copy can then capture its reference.
nameFunctions :: Expr -> [Arg] -> SpecM ([Bind], [Arg])
nameFunctions (ExTyLam binder body) (Left ty : rest) = do
  (lets, arguments) <- nameFunctions (substTypeExpr (Map.singleton (binderName binder) ty) body) rest
  pure (lets, Left ty : arguments)
nameFunctions (ExLam binder body) (Right value : rest) = do
  (lets, arguments) <- nameFunctions body rest
  if functionType (binderType binder) && lambda value
    then do
      state <- get
      let name = (binderName binder) {nameText = "$sf", nameOrigin = OriginLocal (Unique (nextUnique state))}
      modify' (\s -> s {nextUnique = nextUnique s + 1})
      pure (Bind (Binder name (binderType binder)) value : lets, Right (ExVar name) : arguments)
    else pure (lets, Right value : arguments)
  where
    lambda ExLam {} = True
    lambda ExTyLam {} = True
    lambda _ = False
nameFunctions _ arguments = pure ([], arguments)

addDefinition :: Bind -> Definitions -> Definitions
addDefinition binding = Map.insert (binderName (bindBinder binding)) binding

bindScope :: Binder -> Set Name -> Set Name
bindScope = Set.insert . binderName

specializeCall :: Definitions -> Set Name -> Expr -> [Arg] -> SpecM Expr
specializeCall definitions scope function arguments =
  case function of
    ExVar name | Just binding <- Map.lookup name definitions ->
      case selection definitions scope (bindRhs binding) arguments of
        Just choices -> do
          state <- get
          let region = currentRegion state
              required = Set.filter localName (Set.insert name (foldMap (maybe Set.empty argNames) choices))
          if required `Set.isSubsetOf` regionScope region
            then request definitions binding choices arguments
            else
              if regionLocal region
                then pure original
                else do
                  -- The call site contains every captured name, including existential types.
                  modify' (\s -> s {currentRegion = Region scope True Map.empty []})
                  call <- request definitions binding choices arguments
                  local <- currentRegion <$> get
                  modify' (\s -> s {currentRegion = region})
                  pure $ case reverse (regionCopies local) of
                    [] -> call
                    copies -> ExRec copies call
        Nothing -> pure original
    _ -> pure original
  where
    original = rebuild function arguments

-- | Reserve the copy before its body. Recursive requests then reuse its name.
request :: Definitions -> Bind -> [Maybe Arg] -> [Arg] -> SpecM Expr
request definitions binding choices arguments = do
  state <- get
  let name = binderName (bindBinder binding)
      key = (name, choices)
      region = currentRegion state
      cost = nodes (bindRhs binding)
      original = rebuild (ExVar name) arguments
      call copy = rebuild (ExVar copy) ([argument | (choice, argument) <- zip choices arguments, isNothing choice] <> drop (length choices) arguments)
  case Map.lookup key (regionCache region) of
    Just copy -> do
      countCall
      pure (call copy)
    Nothing
      | Map.findWithDefault 0 name (copyCounts state) >= 16 || cost > remainingNodes state -> do
          modify' (\s -> s {report = (report s) {superLimits = superLimits (report s) + 1}})
          pure original
      | Just ty <- specializeType choices (binderType (bindBinder binding)) -> do
          let unique = nextUnique state
              copy = name {nameText = "$ss" <> nameText name <> T.pack (show unique), nameOrigin = if regionLocal region then OriginLocal (Unique unique) else nameOrigin name}
              body = specializeBody choices (bindRhs binding)
              (freshBody, supply) = freshenExprFrom (unique + 1) body
          modify' (\s -> s {nextUnique = supply, remainingNodes = remainingNodes s - cost, copyCounts = Map.insertWith (+) name 1 (copyCounts s), currentRegion = region {regionCache = Map.insert key copy (regionCache region)}, report = (report s) {superCopies = superCopies (report s) + 1}})
          rewritten <- walk definitions (regionScope region) freshBody
          modify' (\s -> s {currentRegion = (currentRegion s) {regionCopies = Bind (Binder copy ty) rewritten : regionCopies (currentRegion s)}})
          countCall
          pure (call copy)
      | otherwise -> pure original

countCall :: SpecM ()
countCall = modify' (\s -> s {report = (report s) {superCalls = superCalls (report s) + 1}})

-- | Type-only copies have no runtime benefit. Require a static value argument.
-- Only references and type applications are substituted, so shared thunks stay shared.
selection :: Definitions -> Set Name -> Expr -> [Arg] -> Maybe [Maybe Arg]
selection definitions scope body arguments =
  let choices = List.dropWhileEnd isNothing (choose body arguments)
   in if any staticValue choices && sum (map (maybe 0 argumentNodes) choices) <= 256 then Just choices else Nothing
  where
    choose (ExTyLam binder inner) (Left ty : rest)
      | namesInScope (typeNames ty) = Just (Left ty) : choose (substTypeExpr (Map.singleton (binderName binder) ty) inner) rest
      | otherwise = Nothing : choose inner rest
    choose (ExLam binder inner) (Right value : rest) =
      let known = (dictionaryType (binderType binder) || functionType (binderType binder)) && knownReference value
       in (if known then Just (Right value) else Nothing) : choose inner rest
    choose _ _ = []
    staticValue (Just (Right _)) = True
    staticValue _ = False
    namesInScope = all (\name -> not (localName name) || Set.member name scope)
    knownReference (ExVar name) = not (localName name) || Map.member name definitions
    knownReference (ExTyApp inner ty) = knownReference inner && namesInScope (typeNames ty)
    knownReference _ = False

specializeBody :: [Maybe Arg] -> Expr -> Expr
specializeBody (choice : rest) expression = case (choice, expression) of
  (Just (Left ty), ExTyLam binder body) -> specializeBody rest (substTypeExpr (Map.singleton (binderName binder) ty) body)
  (Just (Right value), ExLam binder body) -> specializeBody rest (substExpr (Map.singleton (binderName binder) value) body)
  (Nothing, ExTyLam binder body) -> ExTyLam binder (specializeBody rest body)
  (Nothing, ExLam binder body) -> ExLam binder (specializeBody rest body)
  _ -> expression
specializeBody [] expression = expression

specializeType :: [Maybe Arg] -> Type -> Maybe Type
specializeType (choice : rest) ty = case (choice, ty) of
  (Just (Left argument), TyForAll binder body) -> specializeType rest (substType (binderName binder) argument body)
  (Just (Right _), TyFun _ _ _ result) -> specializeType rest result
  (Nothing, TyForAll binder body) -> TyForAll binder <$> specializeType rest body
  (Nothing, TyFun r1 r2 argument result) -> TyFun r1 r2 argument <$> specializeType rest result
  _ -> Nothing
specializeType [] ty = Just ty

rebuild :: Expr -> [Arg] -> Expr
rebuild = List.foldl' (\function -> either (ExTyApp function) (ExApp function))

localName :: Name -> Bool
localName name = case nameOrigin name of
  OriginLocal {} -> True
  OriginTop {} -> False

argNames :: Arg -> Set Name
argNames = either typeNames referenceNames
  where
    referenceNames (ExVar name) = Set.singleton name
    referenceNames (ExTyApp inner ty) = referenceNames inner <> typeNames ty
    referenceNames _ = Set.empty

typeNames :: Type -> Set Name
typeNames ty = case ty of
  TyVar name -> Set.singleton name
  TyApp function argument -> typeNames function <> typeNames argument
  TyFun r1 r2 argument result -> foldMap typeNames [r1, r2, argument, result]
  TyForAll binder body -> typeNames (binderType binder) <> Set.delete (binderName binder) (typeNames body)
  TyEq left right -> typeNames left <> typeNames right
  _ -> Set.empty

dictionaryType :: Type -> Bool
dictionaryType (TyApp function _) = dictionaryType function
dictionaryType (TyCon name) = "$Dict$" `T.isPrefixOf` nameText name
dictionaryType _ = False

functionType :: Type -> Bool
functionType TyFun {} = True
functionType (TyForAll _ body) = functionType body
functionType _ = False

-- | Count syntax nodes without recursive expansion or representation-dependent costs.
nodes :: Expr -> Int
nodes expression =
  1 + case expression of
    ExApp function argument -> nodes function + nodes argument
    ExTyApp function _ -> nodes function
    ExLam _ body -> nodes body
    ExTyLam _ body -> nodes body
    ExLet binding body -> nodes (bindRhs binding) + nodes body
    ExRec bindings body -> sum (map (nodes . bindRhs) bindings) + nodes body
    ExCase scrutinee _ alternatives -> nodes scrutinee + sum (fmap (nodes . altRhs) alternatives)
    ExAbsurd scrutinee _ -> nodes scrutinee
    ExCast body _ -> nodes body
    ExForeignCall _ _ arguments -> sum (map nodes arguments)
    _ -> 0

argumentNodes :: Arg -> Int
argumentNodes = either typeNodes referenceNodes
  where
    referenceNodes (ExTyApp inner ty) = referenceNodes inner + typeNodes ty
    referenceNodes _ = 1

typeNodes :: Type -> Int
typeNodes ty =
  1 + case ty of
    TyApp function argument -> typeNodes function + typeNodes argument
    TyFun r1 r2 argument result -> sum (map typeNodes [r1, r2, argument, result])
    TyForAll binder body -> typeNodes (binderType binder) + typeNodes body
    TyEq left right -> typeNodes left + typeNodes right
    _ -> 0
