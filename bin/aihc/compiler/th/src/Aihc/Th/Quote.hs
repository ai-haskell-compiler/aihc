{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Change an expression quote or a type quote into an expression that
-- builds the syntax tree of its body, and a name quote into the Template
-- Haskell name.
--
-- The change needs the result of name resolution, because the form of a
-- variable in a quote depends on its binder:
--
-- * A top-level name becomes a Template Haskell name that is bound to its
--   package and its module. Thus, the name has the same meaning in each
--   module that gets the result of the quote.
-- * A variable that the quote binds becomes a name with no module.
-- * A variable that the code outside the quote binds becomes a call of
--   @lift@, which makes the syntax tree of the value of the variable.
--
-- The new expression has names that are not resolved, so name resolution
-- runs again on the module. The second time must start from the module as
-- the parser gave it: the resolver adds annotations around some syntax, and
-- the type checker does not accept such annotations two times. Thus,
-- 'desugarModuleQuotes' takes the two forms of the module. It reads the
-- resolutions from the resolved form and puts the new expressions into the
-- parsed form.
--
-- The new expression refers to the Template Haskell library through the
-- module alias of 'quoteImports'. A module that has a quote gets these
-- imports before its first name resolution.
--
-- The change handles a subset of the expressions. A quote with a construct
-- outside the subset stays as it is.
module Aihc.Th.Quote
  ( moduleHasQuote,
    quoteImports,
    desugarModuleQuotes,
    desugarSpliceBody,
    localBinderNames,
    renameQuotedPackage,
    bindSpliceNameQuotes,
  )
where

import Aihc.Parser.Parens (addExprParens)
import Aihc.Parser.Syntax
import Aihc.Resolve.Types (Entity (..), GlobalName (..), PackageId (..), ResolutionNamespace (..), binderEntity, nameEntity)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT (..), evalState, evalStateT, execState, modify', state)
import Data.Data (Data, gmapM, gmapQ)
import Data.Functor.Identity (Identity (..))
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable, eqT, (:~:) (Refl))

-- | Whether a module has an expression quote or a name quote.
moduleHasQuote :: Module -> Bool
moduleHasQuote = not . null . moduleQuotes

-- | The alias of the modules that the new expressions refer to.
quoteAlias :: Text
quoteAlias = "AihcQuoteLibrary"

-- | The imports that give the names of the new expressions.
quoteImports :: [ImportDecl]
quoteImports = map qualified ["Language.Haskell.TH.Lib", "Language.Haskell.TH.Syntax"]
  where
    qualified name =
      ImportDecl
        { importDeclAnns = [],
          importDeclLevel = Nothing,
          importDeclPackage = Nothing,
          importDeclSourcePragma = Nothing,
          importDeclSafe = False,
          importDeclQualified = True,
          importDeclQualifiedPost = False,
          importDeclModule = name,
          importDeclAs = Just quoteAlias,
          importDeclSpec = Nothing
        }

-- | A quote that this module changes.
data Quote
  = -- | @[| e |]@, with its body.
    ExpressionQuote Expr
  | -- | @'x@, with the expression of the name.
    NameQuote Expr
  | -- | @''T@, with the type of the name.
    TypeNameQuote Type
  | -- | @[t| t |]@, with its body.
    TypeQuote Type

-- | Change each quote of a module that is in the subset. The first module
-- is the parsed form, and the second module is the resolved form of the
-- same module. The result is the parsed form with the new expressions. A
-- quote in an expression quote stays, with the quote that contains it.
desugarModuleQuotes :: Module -> Module -> Module
desugarModuleQuotes = desugarQuotes Set.empty Set.empty

-- | Change each quote of a splice body that is in the subset. The second
-- body is the parsed form, and the third body is the same body that the
-- compiler resolved in the scope of the module of the splice. Thus, a name
-- in a quote of the body has the meaning that it has at the splice.
--
-- The compiler resolves the body in the top-level scope of the module, not
-- in the scope of the binders around the splice. Thus, a variable in the
-- set of names, which a local binder of the module binds, becomes a name
-- with no module. The result of the splice then resolves that name at the
-- splice, where a binder around the splice can bind it.
desugarSpliceBody :: Set Text -> Expr -> Expr -> Expr
desugarSpliceBody dynamic = desugarQuotes dynamic Set.empty

-- | The names that a local binder of a module binds: a variable of a
-- pattern (function arguments, lambdas, case alternatives, do statements,
-- and pattern bindings) and a function of a let or a where. A top-level
-- pattern binding is also in the set: its names then resolve at the splice
-- too, to the same declarations.
localBinderNames :: Module -> Set Text
localBinderNames modu =
  Set.fromList (patternNames modu <> concatMap (concat . gmapQ nestedFunctionNames) (moduleDecls modu))
  where
    patternNames :: forall value. (Data value) => value -> [Text]
    patternNames value =
      case eqT @value @Pattern of
        Just Refl ->
          case value of
            PVar binder -> unqualifiedNameText binder : concat (gmapQ patternNames value)
            PAs binder _ -> unqualifiedNameText binder : concat (gmapQ patternNames value)
            PNPlusK binder _ -> [unqualifiedNameText binder]
            _ -> concat (gmapQ patternNames value)
        Nothing -> concat (gmapQ patternNames value)
    nestedFunctionNames :: forall value. (Data value) => value -> [Text]
    nestedFunctionNames value =
      case eqT @value @ValueDecl of
        Just Refl ->
          case value of
            FunctionBind binder _ -> unqualifiedNameText binder : concat (gmapQ nestedFunctionNames value)
            _ -> concat (gmapQ nestedFunctionNames value)
        Nothing -> concat (gmapQ nestedFunctionNames value)

-- | Change the package of each bound name that a changed quote makes, from
-- the first package id to the second package id.
renameQuotedPackage :: Text -> Text -> Expr -> Expr
renameQuotedPackage from to = runIdentity . go
  where
    go :: forall value. (Data value) => value -> Identity value
    go value =
      case eqT @value @Expr of
        Just Refl ->
          case value of
            EApp function (EString identity _)
              | isBoundNameFunction function,
                identity == from ->
                  pure (EApp function (stringLiteral to))
            _ -> gmapM go value
        Nothing -> gmapM go value
    isBoundNameFunction function =
      function `elem` [library "mkNameG_v", library "mkNameG_d", library "mkNameG_tc"]

-- | Change each quote of a syntax tree that is in the subset, as
-- 'desugarModuleQuotes' does. The set has the binders of the quotes that
-- contain the syntax tree. A quote in the body of a splice of a quote can
-- use a variable that the outer quote binds. Such a variable becomes a
-- name with no module, as in the outer quote.
desugarQuotes :: (Data syntax) => Set Text -> Set Entity -> syntax -> syntax -> syntax
desugarQuotes dynamic outerBinders parsed resolved
  | length parsedQuotes == length resolvedQuotes = evalState (traverseQuotes replace parsed) replacements
  | otherwise = parsed
  where
    parsedQuotes = moduleQuotes parsed
    resolvedQuotes = moduleQuotes resolved
    replacements = zipWith replacement parsedQuotes resolvedQuotes
    replacement parsedQuote resolvedQuote =
      addExprParens
        <$> case (parsedQuote, resolvedQuote) of
          (ExpressionQuote parsedBody, ExpressionQuote resolvedBody) ->
            evalStateT (quoted dynamic (outerBinders <> localBinders resolvedBody) resolvedBody) (quoteSplices parsedBody)
          (_, NameQuote expr) -> quotedName dynamic expr
          (_, TypeNameQuote ty) -> quotedTypeName ty
          (_, TypeQuote ty) -> quotedType ty
          _ -> Nothing
    replace original _ =
      state $ \case
        Just expr : rest -> (expr, rest)
        _ : rest -> (original, rest)
        [] -> (original, [])

-- | Bind each name quote in the body of a splice to its package and its
-- module. The compiler runs a splice before it resolves the module, and
-- the module that runs the splice does not import the modules of its own
-- package. Thus, the function gives the top-level entity of a name in the
-- scope of the original module. A quote of a name that the function does
-- not know stays, and a quote in an expression quote stays.
bindSpliceNameQuotes :: Set Text -> (ResolutionNamespace -> Name -> Maybe GlobalName) -> Module -> Module
bindSpliceNameQuotes dynamic lookupName = runIdentity . outside
  where
    outside :: forall value. (Data value) => value -> Identity value
    outside value =
      case eqT @value @Expr of
        Just Refl ->
          case value of
            ETHSplice body -> pure (ETHSplice (runIdentity (inside body)))
            _ -> gmapM outside value
        Nothing ->
          case eqT @value @Decl of
            Just Refl ->
              case value of
                DeclSplice body -> pure (DeclSplice (runIdentity (inside body)))
                _ -> gmapM outside value
            Nothing -> gmapM outside value
    inside :: forall value. (Data value) => value -> Identity value
    inside value =
      case eqT @value @Expr of
        Just Refl ->
          case value of
            ETHNameQuote target | Just bound <- boundName target -> pure bound
            ETHTypeNameQuote target | Just bound <- boundTypeName target -> pure bound
            ETHExpQuote {} -> pure value
            ETHTypedQuote {} -> pure value
            ETHDeclQuote {} -> pure value
            ETHTypeQuote {} -> pure value
            ETHPatQuote {} -> pure value
            _ -> gmapM inside value
        Nothing -> gmapM inside value
    boundName expr =
      case expr of
        EAnn _ inner -> boundName inner
        EParen inner -> boundName inner
        EVar name
          | isDynamicName dynamic name -> Just (addExprParens (dynamicName (nameText name)))
          | otherwise -> addExprParens . globalName name <$> lookupName ResolutionNamespaceTerm name
        _ -> Nothing
    boundTypeName ty =
      case ty of
        TAnn _ inner -> boundTypeName inner
        TParen inner -> boundTypeName inner
        TCon name _ -> addExprParens . call "mkNameG_tc" . globalNameParts <$> lookupName ResolutionNamespaceType name
        _ -> Nothing

-- | Each quote of a syntax tree that is not in an expression quote. The
-- parsed form and the resolved form of a module give the quotes in the
-- same order: the resolver changes the structure of infix expressions
-- only, and it keeps their operands in source order.
moduleQuotes :: (Data syntax) => syntax -> [Quote]
moduleQuotes modu = reverse (execState (traverseQuotes collectQuote modu) [])
  where
    collectQuote original quote = do
      modify' (quote :)
      pure original

-- | Visit each quote that 'moduleQuotes' gives, and replace it. The
-- function gets the expression of the quote and the quote.
traverseQuotes :: forall m syntax. (Monad m, Data syntax) => (Expr -> Quote -> m Expr) -> syntax -> m syntax
traverseQuotes onQuote = go
  where
    go :: forall value. (Data value) => value -> m value
    go value =
      case eqT @value @Expr of
        Just Refl -> expression value
        Nothing -> gmapM go value
    expression :: Expr -> m Expr
    expression expr =
      case expr of
        ETHExpQuote body -> onQuote expr (ExpressionQuote body)
        ETHNameQuote name -> onQuote expr (NameQuote name)
        ETHTypeNameQuote name -> onQuote expr (TypeNameQuote name)
        ETHTypeQuote body -> onQuote expr (TypeQuote body)
        ETHTypedQuote {} -> pure expr
        ETHDeclQuote {} -> pure expr
        ETHPatQuote {} -> pure expr
        _ -> gmapM go expr

-- | The Template Haskell name of the term of a name quote. A top-level
-- term gives a name that is bound to its package and module. A local
-- variable gives a name with no module.
quotedName :: Set Text -> Expr -> Maybe Expr
quotedName dynamic expr =
  case expr of
    EAnn _ inner -> quotedName dynamic inner
    EParen inner -> quotedName dynamic inner
    EVar name ->
      case nameEntity name of
        Just (EntityGlobal global)
          | isDynamicName dynamic name -> Just (dynamicName (nameText name))
          | otherwise -> Just (globalName name global)
        Just (EntityLocal _) -> Just (dynamicName (nameText name))
        _ -> Nothing
    _ -> Nothing

-- | The Template Haskell name of the type or class of a type name quote.
quotedTypeName :: Type -> Maybe Expr
quotedTypeName ty =
  case ty of
    TAnn _ inner -> quotedTypeName inner
    TParen inner -> quotedTypeName inner
    TCon name _ ->
      case nameEntity name of
        Just (EntityGlobal global) -> Just (call "mkNameG_tc" (globalNameParts global))
        _ -> Nothing
    _ -> Nothing

-- | The expression that builds the syntax tree of a resolved type quote
-- body. A type constructor becomes a name that is bound to its package and
-- its module, and a data constructor becomes a promoted name. A type
-- variable becomes a name with no module. A kind signature, a binder of a
-- forall with a kind, a splice, and other constructs are outside the
-- subset: the quote stays as it is. The library function @sigT@ takes the
-- kind as a value and not as a computation, so a kind signature needs a
-- different form of the result.
quotedType :: Type -> Maybe Expr
quotedType = go
  where
    go ty =
      case ty of
        TAnn _ inner -> go inner
        TParen inner -> go inner
        -- The namespace of the resolution decides the form, not the tick: with
        -- DataKinds, a data constructor with no tick is also promoted.
        TCon name _ ->
          case nameEntity name of
            Just (EntityGlobal global)
              | globalNameNamespace global == ResolutionNamespaceTerm -> Just (call "promotedT" [call "mkNameG_d" (globalNameParts global)])
              | globalNameNamespace global == ResolutionNamespaceType -> Just (call "conT" [call "mkNameG_tc" (globalNameParts global)])
            _ -> Nothing
        TVar name -> Just (call "varT" [dynamicName (unqualifiedNameText name)])
        TApp function argument -> applied <$> go function <*> go argument
        TFun ArrowUnrestricted argument result -> applied . applied (library "arrowT") <$> go argument <*> go result
        TList Unpromoted [element] -> applied (library "listT") <$> go element
        TTuple Boxed Unpromoted items -> foldl applied (call "tupleT" [integer (toInteger (length items))]) <$> mapM go items
        TTypeLit (TypeLitInteger number _) -> Just (call "litT" [call "numTyLit" [integer number]])
        TTypeLit (TypeLitSymbol string _) -> Just (call "litT" [call "strTyLit" [stringLiteral string]])
        TContext constraints inner -> quantified [] constraints inner
        TForall (ForallTelescope ForallInvisible binders) inner ->
          case withoutTypeAnnotations inner of
            TContext constraints body -> quantified binders constraints body
            body -> quantified binders [] body
        _ -> Nothing
    applied function argument = call "appT" [function, argument]
    quantified binders constraints body = do
      binders' <- mapM binder binders
      constraints' <- mapM go constraints
      body' <- go body
      Just (call "forallT" [EList binders', call "cxt" [EList constraints'], body'])
    binder tyVarBinder =
      case tyVarBinderKind tyVarBinder of
        Nothing -> Just (foldl EApp (libraryConstructor "PlainTV") [dynamicName (tyVarBinderName tyVarBinder), libraryConstructor (specificity (tyVarBinderSpecificity tyVarBinder))])
        Just _ -> Nothing
    specificity flag =
      case flag of
        TyVarBInferred -> "InferredSpec"
        TyVarBSpecified -> "SpecifiedSpec"
    withoutTypeAnnotations ty =
      case ty of
        TAnn _ inner -> withoutTypeAnnotations inner
        _ -> ty

-- | The body of each splice of a quote body, in source order. A splice in
-- the body of a splice is not in the result.
quoteSplices :: Expr -> [Expr]
quoteSplices = go
  where
    go :: forall value. (Data value) => value -> [Expr]
    go value =
      case eqT @value @Expr of
        Just Refl ->
          case value of
            ETHSplice body -> [body]
            _ -> concat (gmapQ go value)
        Nothing -> concat (gmapQ go value)

-- | The local binders of a syntax tree.
localBinders :: (Data syntax) => syntax -> Set Entity
localBinders = Set.fromList . collect local
  where
    local binder =
      case binderEntity binder of
        Just entity@(EntityLocal _) -> Just entity
        _ -> Nothing

-- | The expression that builds the syntax tree of a resolved quote body.
-- The set has the binders of the quote. The state has the splice bodies of
-- the parsed form of the quote: the new expression takes each splice body
-- from there, so it has no annotation of the resolver.
quoted :: Set Text -> Set Entity -> Expr -> StateT [Expr] Maybe Expr
quoted dynamic binders = go
  where
    go expr =
      case expr of
        EAnn _ inner -> go inner
        EParen inner -> go inner
        EVar name -> lift (variable name)
        EInt number TInteger _ -> pure (literal "integerL" (integer number))
        ENegate inner
          | EInt number TInteger _ <- withoutAnnotations inner -> pure (literal "integerL" (integer (negate number)))
        EChar char source -> pure (literal "charL" (EChar char source))
        EString string _ -> pure (literal "stringL" (stringLiteral string))
        EApp function argument -> call "appE" <$> mapM go [function, argument]
        EInfix left operator right -> do
          left' <- go left
          operator' <- lift (variable operator)
          right' <- go right
          pure (call "infixApp" [left', operator', right'])
        ELambdaPats patterns body -> do
          patterns' <- lift (mapM quotedPattern patterns)
          body' <- go body
          pure (call "lamE" [EList patterns', body'])
        ETuple Boxed fields -> do
          fields' <- mapM (maybe (lift Nothing) go) fields
          pure (call "tupE" [EList fields'])
        EList elements -> (\elements' -> call "listE" [EList elements']) <$> mapM go elements
        EIf condition whenTrue whenFalse -> call "condE" <$> mapM go [condition, whenTrue, whenFalse]
        -- A splice in a quote gives the syntax tree itself. A quote in the
        -- body of the splice also changes, with the binders of this quote.
        ETHSplice resolvedBody ->
          StateT $ \case
            body : rest -> Just (EParen (desugarQuotes dynamic binders body resolvedBody), rest)
            [] -> Nothing
        _ -> lift Nothing
    variable name =
      case nameEntity name of
        Just (EntityGlobal global)
          | isDynamicName dynamic name -> Just (call "varE" [dynamicName (nameText name)])
          | otherwise -> Just (call (builder name) [globalName name global])
        Just entity@(EntityLocal _)
          | entity `Set.member` binders -> Just (call "varE" [dynamicName (nameText name)])
          | otherwise -> Just (call "lift" [EVar (mkName (nameQualifier name) (nameType name) (nameText name))])
        _ -> Nothing
    quotedPattern pattern' =
      case pattern' of
        PAnn _ inner -> quotedPattern inner
        PParen inner -> quotedPattern inner
        PVar binder -> Just (call "varP" [dynamicName (unqualifiedNameText binder)])
        PWildcard -> Just (library "wildP")
        PTuple Boxed fields -> (\fields' -> call "tupP" [EList fields']) <$> mapM quotedPattern fields
        -- A constructor pattern with no type arguments.
        PCon name [] arguments
          | Just (EntityGlobal global) <- nameEntity name ->
              (\arguments' -> call "conP" [globalName name global, EList arguments']) <$> mapM quotedPattern arguments
        _ -> Nothing

-- | The Template Haskell name of a top-level term.
globalName :: Name -> GlobalName -> Expr
globalName name global =
  call (if isConstructor name then "mkNameG_d" else "mkNameG_v") (globalNameParts global)

-- | The package, the module, and the text of a top-level name, as the
-- arguments of the functions that make a bound Template Haskell name.
globalNameParts :: GlobalName -> [Expr]
globalNameParts global =
  map stringLiteral [packageIdText (globalNamePackage global), globalNameModule global, globalNameText global]

-- | Whether a name is a variable with no qualifier in the set of names. A
-- binder around a splice can bind such a variable.
isDynamicName :: Set Text -> Name -> Bool
isDynamicName dynamic name =
  isNothing (nameQualifier name) && not (isConstructor name) && nameText name `Set.member` dynamic

-- | The Template Haskell name of a variable that the quote binds.
dynamicName :: Text -> Expr
dynamicName text = call "mkName" [stringLiteral text]

builder :: Name -> Text
builder name
  | isConstructor name = "conE"
  | otherwise = "varE"

isConstructor :: Name -> Bool
isConstructor name = nameType name `elem` [NameConId, NameConSym]

literal :: Text -> Expr -> Expr
literal constructor value = call "litE" [call constructor [value]]

integer :: Integer -> Expr
integer number
  | number < 0 = ENegate (EInt (negate number) TInteger (T.pack (show (negate number))))
  | otherwise = EInt number TInteger (T.pack (show number))

stringLiteral :: Text -> Expr
stringLiteral string = EString string (T.pack (show (T.unpack string)))

-- | An application of a function of the Template Haskell library.
call :: Text -> [Expr] -> Expr
call function = foldl EApp (library function)

library :: Text -> Expr
library function = EVar (mkName (Just quoteAlias) NameVarId function)

-- | A data constructor of the Template Haskell library.
libraryConstructor :: Text -> Expr
libraryConstructor name = EVar (mkName (Just quoteAlias) NameConId name)

withoutAnnotations :: Expr -> Expr
withoutAnnotations expr =
  case expr of
    EAnn _ inner -> withoutAnnotations inner
    _ -> expr

-- | Each value that the function gives for a node of a syntax tree.
collect :: forall syntax node result. (Data syntax, Typeable node) => (node -> Maybe result) -> syntax -> [result]
collect select = go
  where
    go :: forall value. (Data value) => value -> [result]
    go value =
      case eqT @value @node of
        Just Refl -> maybe id (:) (select value) (concat (gmapQ go value))
        Nothing -> concat (gmapQ go value)
