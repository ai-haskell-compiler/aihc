{-# LANGUAGE OverloadedStrings #-}

-- | Declaration ownership for System FC terms.
--
-- The in-memory tree carries a type on every 'Var' occurrence for convenient
-- local checking.  The textual form does not: a declaration is the sole owner
-- of that type and occurrences are references to it.  This module centralizes
-- that distinction for top-level values, primitives, and data constructors.
module Aihc.Fc.Declaration
  ( TermDeclarations,
    declaredConstructorTypes,
    declaredTerms,
    hasDeclaredTermName,
    isDeclaredTerm,
    newtypeResultKind,
    normalizeProgramReferences,
    typesEqual,
    validateDeclarationOwnership,
  )
where

import Aihc.Fc.Subst (substType)
import Aihc.Fc.Syntax
import Aihc.Tc.Types (Kind (..), Pred (..), TcType (..), TyCon (..), Unique, tvKind, tyConArity, tyConKind, tyConName)
import Control.Monad.Trans.State.Strict (evalState, get, modify')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

data TermDeclarations = TermDeclarations
  { termBinders :: !(Map Unique Var),
    termConstructors :: !(Map Text [TcType])
  }

-- | Collect every term declaration whose metadata is owned by the program.
declaredTerms :: FcProgram -> TermDeclarations
declaredTerms program@(FcProgram tops) =
  TermDeclarations
    { termBinders =
        Map.fromList
          [ (varUnique variable, variable)
          | top <- tops,
            variable <- topVariables top
          ],
      termConstructors = declaredConstructorTypes program
    }
  where
    topVariables top =
      case top of
        FcPrimitive variable _ -> [variable]
        FcTopBind binding -> binders binding
        _ -> []

-- | Whether an occurrence is completely determined by one program
-- declaration.  A resolved name denotes an external entity and can therefore
-- never be captured by a same-spelled local constructor.
isDeclaredTerm :: TermDeclarations -> Var -> Bool
isDeclaredTerm declarations variable =
  case Map.lookup (varUnique variable) (termBinders declarations) of
    Just binder -> typesEqual (varType binder) (varType variable)
    Nothing
      | isNothing (varResolvedName variable) ->
          case Map.lookup (varName variable) (termConstructors declarations) of
            Just [declaredType] -> typesEqual declaredType (varType variable)
            _ -> False
      | otherwise -> False

hasDeclaredTermName :: TermDeclarations -> Text -> Bool
hasDeclaredTermName declarations name =
  any ((== name) . varName) (Map.elems (termBinders declarations))
    || Map.member name (termConstructors declarations)

-- | Normalize the denormalized variable occurrences emitted by desugaring.
--
-- Resolver annotations identify references to the current module with a
-- qualified name even though the corresponding FC declaration is local.  A
-- use may also have received a fresh compiler unique before its top-level
-- declaration was created.  Reconnect those occurrences to the declaration
-- that owns their metadata, strip current-module qualification from local
-- constructors, and canonicalize repeated external references by their
-- resolved identity.
normalizeProgramReferences :: Maybe Text -> FcProgram -> FcProgram
normalizeProgramReferences currentModule program@(FcProgram tops) =
  evalState (FcProgram <$> traverse normalizeTop tops) Map.empty
  where
    declarations = declaredTerms program
    localBindersByName =
      Map.fromListWith
        (<>)
        [ (varName variable, [variable])
        | variable <- Map.elems (termBinders declarations)
        ]

    normalizeTop top =
      case top of
        FcPrimitive variable arity -> pure (FcPrimitive (localBinder variable) arity)
        FcTopBind binding -> FcTopBind <$> normalizeBind Set.empty binding
        other -> pure other

    normalizeBind bound binding =
      case binding of
        FcNonRec variable expression ->
          FcNonRec (localBinder variable) <$> normalizeExpr bound expression
        FcRec bindings' -> do
          let variables = map (localBinder . fst) bindings'
              recursiveScope = bound <> Set.fromList (map varUnique variables)
          expressions <- traverse (normalizeExpr recursiveScope . snd) bindings'
          pure (FcRec (zip variables expressions))

    normalizeExpr bound expression =
      case expression of
        FcVar variable -> FcVar <$> normalizeReference bound variable
        FcLit {} -> pure expression
        FcApp function argument -> FcApp <$> normalizeExpr bound function <*> normalizeExpr bound argument
        FcTyApp function ty -> (`FcTyApp` ty) <$> normalizeExpr bound function
        FcLam variable body ->
          FcLam variable <$> normalizeExpr (Set.insert (varUnique variable) bound) body
        FcTyLam tyVar body -> FcTyLam tyVar <$> normalizeExpr bound body
        FcLet (FcNonRec variable rhs) body -> do
          rhs' <- normalizeExpr bound rhs
          body' <- normalizeExpr (Set.insert (varUnique variable) bound) body
          pure (FcLet (FcNonRec variable rhs') body')
        FcLet binding@(FcRec bindings') body -> do
          let recursiveScope = bound <> Set.fromList (map (varUnique . fst) bindings')
          binding' <- normalizeBind recursiveScope binding
          FcLet binding' <$> normalizeExpr recursiveScope body
        FcCase scrutinee binder alternatives -> do
          scrutinee' <- normalizeExpr bound scrutinee
          let caseScope = Set.insert (varUnique binder) bound
          FcCase scrutinee' binder <$> traverse (normalizeAlt caseScope) alternatives
        FcCast inner coercion -> (`FcCast` coercion) <$> normalizeExpr bound inner
        FcCallForeign foreignCall arguments -> FcCallForeign foreignCall <$> traverse (normalizeExpr bound) arguments

    normalizeAlt bound alternative = do
      let alternativeScope = bound <> Set.fromList (map varUnique (altBinders alternative))
      rhs <- normalizeExpr alternativeScope (altRhs alternative)
      pure alternative {altRhs = rhs}

    normalizeReference bound variable
      | Set.member (varUnique variable) bound = pure variable
      | Just binder <- Map.lookup (varUnique variable) (termBinders declarations) = pure binder
      | isCurrentModuleReference variable,
        Just binder <- uniqueLocalBinder variable =
          pure binder
      | isNothing (varResolvedName variable),
        Just binder <- uniqueLocalBinder variable =
          pure binder
      | isLocalConstructor variable = pure variable {varResolvedName = Nothing}
      | Just resolved <- varResolvedName variable = canonicalExternal resolved variable
      | otherwise = pure variable

    uniqueLocalBinder variable =
      case [ binder
           | binder <- Map.findWithDefault [] (varName variable) localBindersByName,
             typesEqual (varType binder) (varType variable)
           ] of
        [binder] -> Just binder
        _ -> Nothing

    isLocalConstructor variable =
      (isNothing (varResolvedName variable) || isCurrentModuleReference variable)
        && case Map.lookup (varName variable) (termConstructors declarations) of
          Just [constructorType] -> typesEqual constructorType (varType variable)
          _ -> False

    isCurrentModuleReference variable =
      case (currentModule, varResolvedName variable) of
        (Just moduleName, Just resolved) ->
          T.stripPrefix (moduleName <> ".") resolved == Just (varName variable)
        _ -> False

    localBinder variable = variable {varResolvedName = Nothing}

    canonicalExternal resolved variable = do
      externals <- get
      case Map.lookup resolved externals of
        Just canonical
          | typesEqual (varType canonical) (varType variable) -> pure canonical
        _ -> do
          modify' (Map.insert resolved variable)
          pure variable

-- | Check every fact that the textual form intentionally obtains from a
-- declaration rather than repeating at a use site.  Failure here means there
-- is no single source of truth, so a lossless non-duplicating rendering does
-- not exist.
validateDeclarationOwnership :: FcProgram -> [String]
validateDeclarationOwnership program@(FcProgram tops) =
  duplicateForeignDeclarations
    <> concatMap validateData tops
    <> concatMap validateNewtype tops
    <> concatMap validateTop tops
    <> externalTypeConflicts
  where
    declarations = declaredTerms program
    foreignDeclarations =
      Map.fromListWith
        (<>)
        [ (fcForeignCallName foreignCall, [foreignCall])
        | FcForeignImport foreignCall <- tops
        ]
    duplicateForeignDeclarations =
      [ "duplicate foreign declaration for " <> T.unpack name
      | (name, calls) <- Map.toList foreignDeclarations,
        length calls /= 1
      ]

    validateData top =
      case top of
        FcData typeName tyVars constructors -> concatMap (validateConstructor typeName (length tyVars)) constructors
        _ -> []

    validateConstructor typeName arity constructor =
      case fcDataConstructorResultType constructor of
        TcTyCon tyCon arguments
          | tyConName tyCon /= typeName -> invalid "result type constructor differs"
          | tyConArity tyCon /= arity -> invalid "result type constructor arity differs"
          | length arguments /= arity -> invalid "result application arity differs"
          | otherwise -> []
        _ -> invalid "result is not a type-constructor application"
      where
        invalid reason = ["constructor " <> T.unpack (fcDataConstructorName constructor) <> " disagrees with data declaration " <> T.unpack typeName <> ": " <> reason]

    validateNewtype top =
      case top of
        FcNewtype declaration ->
          case newtypeResultKind declaration of
            Left message -> [message]
            Right _ -> []
        _ -> []

    validateTop top =
      case top of
        FcTopBind binding -> validateBind binding
        _ -> []

    validateBind binding =
      case binding of
        FcNonRec _ expression -> validateExpr expression
        FcRec bindings' -> concatMap (validateExpr . snd) bindings'

    validateExpr expression =
      case expression of
        FcVar variable ->
          case Map.lookup (varUnique variable) (termBinders declarations) of
            Just binder
              | not (typesEqual (varType binder) (varType variable)) ->
                  ["reference type disagrees with declaration for " <> T.unpack (varName binder)]
            _ -> []
        FcLit {} -> []
        FcApp function argument -> validateExpr function <> validateExpr argument
        FcTyApp function _ -> validateExpr function
        FcLam _ body -> validateExpr body
        FcTyLam _ body -> validateExpr body
        FcLet binding body -> validateBind binding <> validateExpr body
        FcCase scrutinee _ alternatives -> validateExpr scrutinee <> concatMap (validateExpr . altRhs) alternatives
        FcCast inner _ -> validateExpr inner
        FcCallForeign foreignCall arguments -> validateForeignCall foreignCall <> concatMap validateExpr arguments

    validateForeignCall foreignCall =
      case Map.lookup (fcForeignCallName foreignCall) foreignDeclarations of
        Nothing -> ["foreign call has no declaration: " <> T.unpack (fcForeignCallName foreignCall)]
        Just [declared]
          | declared == foreignCall -> []
          | otherwise -> ["foreign call disagrees with declaration: " <> T.unpack (fcForeignCallName foreignCall)]
        Just _ -> []

    externalOccurrences =
      Map.fromListWith
        (<>)
        [ (resolved, [variable])
        | top <- tops,
          variable <- topReferences top,
          not (isDeclaredTerm declarations variable),
          Just resolved <- [varResolvedName variable]
        ]
    externalTypeConflicts =
      [ "external references disagree on type for " <> T.unpack resolved
      | (resolved, first : rest) <- Map.toList externalOccurrences,
        not (all (typesEqual (varType first) . varType) rest)
      ]

    topReferences top =
      case top of
        FcTopBind binding -> bindReferences binding
        _ -> []
    bindReferences binding =
      case binding of
        FcNonRec _ expression -> exprReferences expression
        FcRec bindings' -> concatMap (exprReferences . snd) bindings'
    exprReferences expression =
      case expression of
        FcVar variable -> [variable]
        FcLit {} -> []
        FcApp function argument -> exprReferences function <> exprReferences argument
        FcTyApp function _ -> exprReferences function
        FcLam _ body -> exprReferences body
        FcTyLam _ body -> exprReferences body
        FcLet binding body -> bindReferences binding <> exprReferences body
        FcCase scrutinee _ alternatives -> exprReferences scrutinee <> concatMap (exprReferences . altRhs) alternatives
        FcCast inner _ -> exprReferences inner
        FcCallForeign _ arguments -> concatMap exprReferences arguments

-- | The result kind declared by a newtype head after consuming the kinds of
-- its parameters. The result application itself is derived from the head and
-- is therefore not printed separately.
newtypeResultKind :: FcNewtypeDecl -> Either String Kind
newtypeResultKind declaration =
  case fcNewtypeResult declaration of
    TcTyCon tyCon arguments
      | tyConName tyCon /= fcNewtypeName declaration -> invalid "type constructor name differs"
      | tyConArity tyCon /= length tyVars -> invalid "type constructor arity differs"
      | arguments /= map TcTyVar tyVars -> invalid "type arguments differ"
      | otherwise -> consumeParameterKinds tyVars (tyConKind tyCon)
    _ -> invalid "result is not a type-constructor application"
  where
    tyVars = fcNewtypeTyVars declaration
    invalid reason = Left ("newtype result disagrees with declaration head for " <> T.unpack (fcNewtypeName declaration) <> ": " <> reason)
    consumeParameterKinds [] resultKind = Right resultKind
    consumeParameterKinds (tyVar : rest) (KFun argumentKind bodyKind)
      | argumentKind == tvKind tyVar = consumeParameterKinds rest bodyKind
      | otherwise = invalid "parameter kind differs"
    consumeParameterKinds _ _ = invalid "result kind has too few parameters"

-- | Collect the complete term-level constructor types owned by data
-- constructor signatures.
declaredConstructorTypes :: FcProgram -> Map Text [TcType]
declaredConstructorTypes (FcProgram tops) =
  Map.fromListWith
    (<>)
    [ (fcDataConstructorName constructor, [fcDataConstructorType constructor])
    | FcData _ _ constructors <- tops,
      constructor <- constructors
    ]

-- | Structural type equality with alpha-equivalence for quantified variables.
typesEqual :: TcType -> TcType -> Bool
typesEqual (TcTyVar left) (TcTyVar right) = left == right
typesEqual (TcMetaTv left) (TcMetaTv right) = left == right
typesEqual (TcTyCon leftCon leftArgs) (TcTyCon rightCon rightArgs) =
  leftCon == rightCon
    && length leftArgs == length rightArgs
    && and (zipWith typesEqual leftArgs rightArgs)
typesEqual (TcFunTy leftArg leftResult) (TcFunTy rightArg rightResult) =
  typesEqual leftArg rightArg && typesEqual leftResult rightResult
typesEqual (TcForAllTy leftVar leftBody) (TcForAllTy rightVar rightBody) =
  typesEqual leftBody (substType (Map.singleton rightVar (TcTyVar leftVar)) rightBody)
typesEqual (TcQualTy leftPredicates leftBody) (TcQualTy rightPredicates rightBody) =
  length leftPredicates == length rightPredicates
    && and (zipWith predicatesEqual leftPredicates rightPredicates)
    && typesEqual leftBody rightBody
typesEqual (TcAppTy leftFunction leftArg) (TcAppTy rightFunction rightArg) =
  typesEqual leftFunction rightFunction && typesEqual leftArg rightArg
typesEqual _ _ = False

predicatesEqual :: Pred -> Pred -> Bool
predicatesEqual (ClassPred leftClass leftArgs) (ClassPred rightClass rightArgs) =
  leftClass == rightClass
    && length leftArgs == length rightArgs
    && and (zipWith typesEqual leftArgs rightArgs)
predicatesEqual (EqPred leftA leftB) (EqPred rightA rightB) =
  typesEqual leftA rightA && typesEqual leftB rightB
predicatesEqual _ _ = False

binders :: FcBind -> [Var]
binders binding =
  case binding of
    FcNonRec variable _ -> [variable]
    FcRec bindings' -> map fst bindings'
