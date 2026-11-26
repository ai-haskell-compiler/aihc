{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | The top-level bindings and the diagnostics of a type-checked module.
--
-- This module reads the final syntax tree of "Aihc.Tc.Syntax".
module Aihc.Tc.Bindings
  ( moduleBindings,
    moduleDiagnostics,
    recStmtBinderNames,
    compStmtBinderNames,
  )
where

import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Annotations
import Aihc.Tc.Deriving.Context (typeTyVars)
import Aihc.Tc.Diagnostics (moduleDiagnostics)
import Aihc.Tc.Env (DataConSourceForm (..))
import Aihc.Tc.Generate.Decl (TcBindingResult (..), defaultMethodName)
import Aihc.Tc.Syntax
import Aihc.Tc.Types
import Aihc.Tc.Wiring (BuiltinDataCon (..), TcWiring, builtinDataCon, mkTcKinds)
import Control.Applicative ((<|>))
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)

-- | One data constructor that a declaration binds: a constructor the
-- source names, or a built-in form such as @(,)@, @(# | _ #)@ or @[]@,
-- which names nothing and whose identity the wiring supplies.
data DataConIdentity
  = DeclaredDataCon UnqualifiedName
  | BuiltinDataConIdentity BuiltinDataCon

-- | The wiring names the constructors that built-in syntax declares, such
-- as @(,)@ or @[]@; the kind vocabulary follows from it.
moduleBindings :: TcWiring -> Module -> [TcBindingResult]
moduleBindings wiring modu =
  concatMap (declBindings wiring (mkTcKinds wiring) (resolvedModuleOrigin modu)) (moduleDecls modu)

declBindings :: TcWiring -> TcKinds -> (Text, Text) -> Decl -> [TcBindingResult]
declBindings wiring kinds origin decl =
  case decl of
    DeclAnn _ inner -> declBindings wiring kinds origin inner
    DeclError _ inner -> declBindings wiring kinds origin inner
    DeclTc info inner ->
      infoBindings kinds origin info inner <> declBindings wiring kinds origin inner
    DeclData dataDecl ->
      concatMap (dataConBindings wiring origin) (dataDeclConstructors dataDecl)
        <> concatMap (recordSelectorBindings origin) (dataDeclConstructors dataDecl)
    DeclNewtype newtypeDecl ->
      maybe [] (\constructor -> dataConBindings wiring origin constructor <> recordSelectorBindings origin constructor) (newtypeDeclConstructor newtypeDecl)
    DeclDataFamilyInst familyInst ->
      concatMap (dataConBindings wiring origin) (dataFamilyInstConstructors familyInst)
        <> concatMap (recordSelectorBindings origin) (dataFamilyInstConstructors familyInst)
    _ -> []

infoBindings :: TcKinds -> (Text, Text) -> TcInfo -> Decl -> [TcBindingResult]
infoBindings kinds origin info decl =
  case info of
    InfoType tcAnn -> typeBindings origin tcAnn decl
    InfoClass classAnn -> classBindings kinds origin classAnn decl
    InfoInstance instAnn ->
      [TcBindingResult (originTermKey origin (tcInstanceDictName instAnn)) (tcInstanceDictName instAnn) (tcInstanceDictType instAnn)]
    _ -> []

typeBindings :: (Text, Text) -> TcAnnotation -> Decl -> [TcBindingResult]
typeBindings origin tcAnn decl =
  case decl of
    DeclValue (FunctionBind binder _) ->
      let (name, displayName) = binderBindingName binder
       in [TcBindingResult (originTermKey origin name) displayName (tcAnnType tcAnn)]
    -- The declaration type of a pattern binding is the type of the
    -- whole pattern, thus each binder must give its own type. The
    -- binders of @(a, b) = ...@ do not have the type of the pair.
    DeclValue (PatternBind _ pat _) ->
      [ TcBindingResult (originTermKey origin name) displayName (fromMaybe (tcAnnType tcAnn) (binderCheckedType binder))
      | binder <- patternBinderNames pat,
        let (name, displayName) = binderBindingName binder
      ]
    DeclData dataDecl ->
      let name = unqualifiedNameText (binderHeadName (dataDeclHead dataDecl))
       in [TcBindingResult (originTermKey origin name) name (tcAnnType tcAnn)]
    DeclNewtype newtypeDecl ->
      let name = unqualifiedNameText (binderHeadName (newtypeDeclHead newtypeDecl))
       in [TcBindingResult (originTermKey origin name) name (tcAnnType tcAnn)]
    DeclDataFamilyDecl familyDecl ->
      let name = unqualifiedNameText (binderHeadName (dataFamilyDeclHead familyDecl))
       in [TcBindingResult (originTermKey origin name) name (tcAnnType tcAnn)]
    DeclForeign foreignDecl ->
      let name = unqualifiedNameText (foreignName foreignDecl)
          displayName = renderBinderName (foreignName foreignDecl)
       in [TcBindingResult (originTermKey origin name) displayName (tcAnnType tcAnn)]
    _ -> []

classBindings :: TcKinds -> (Text, Text) -> TcClassAnnotation -> Decl -> [TcBindingResult]
classBindings kinds origin classAnn decl =
  case decl of
    DeclClass {} ->
      [ TcBindingResult (originTermKey origin (tcClassMethodName method)) (tcClassMethodName method) (tcClassMethodType method)
      | method <- tcClassMethods classAnn
      ]
        <> [ TcBindingResult (originTermKey origin (defaultMethodName (tcClassMethodName method))) (defaultMethodName (tcClassMethodName method)) (classDefaultWorkerType kinds classAnn method)
           | method <- tcClassMethods classAnn,
             tcClassMethodName method `elem` tcClassDefaultMethods classAnn
           ]
    _ -> []

classDefaultWorkerType :: TcKinds -> TcClassAnnotation -> TcClassMethodAnnotation -> TcType
classDefaultWorkerType kinds classAnnotation method =
  case lookup (tcClassMethodName method) (tcClassDefaultSignatures classAnnotation) of
    Just signature
      | Just classPredicate <- constraintTypeToPred kinds (tcClassMethodDictType method) ->
          let (tyVars, body) = peelForAlls signature
              qualifiedBody =
                case body of
                  TcQualTy predicates result -> TcQualTy (classPredicate : predicates) result
                  result -> TcQualTy [classPredicate] result
           in foldr TcForAllTy qualifiedBody tyVars
    _ -> tcClassMethodType method

dataConBindings :: TcWiring -> (Text, Text) -> DataConDecl -> [TcBindingResult]
dataConBindings wiring origin dataConDecl =
  case [tcAnn | InfoType tcAnn <- dataConTc dataConDecl] of
    tcAnn : _ ->
      [ TcBindingResult (originTermKey origin name) name (tcAnnType tcAnn)
      | name <- map (dataConIdentityName wiring) (dataConIdentities dataConDecl)
      ]
    [] -> []

recordSelectorBindings :: (Text, Text) -> DataConDecl -> [TcBindingResult]
recordSelectorBindings origin declaration =
  case [tcAnn | InfoType tcAnn <- dataConTc declaration] of
    tcAnn : _ -> selectorBindingsFromConstructorType (tcAnnType tcAnn) declaration
    [] -> []
  where
    selectorBindingsFromConstructorType constructorType inner =
      let (typeVariables, qualifiedConstructor) = peelForAlls constructorType
          body =
            case qualifiedConstructor of
              TcQualTy _ result -> result
              result -> result
          (fieldTypes, resultType) = splitFunctionType body
          (_, sourceFields, _) = dataConSourceLayout inner
          -- The constructor context and the existential variables do not
          -- reach the selector type. A field whose type mentions an
          -- existential variable has no selector. A kind variable of a
          -- universal variable, such as the runtime representation in
          -- @TExp (a :: TYPE r)@, is universal as well.
          universals = filter (`elem` closeOverKinds (typeTyVars resultType)) typeVariables
          closeOverKinds variables =
            let kindVariables = [variable | variable <- typeVariables, variable `notElem` variables, any (\universal -> variable `elem` typeTyVars (tvKind universal)) variables]
             in if null kindVariables then variables else closeOverKinds (variables <> kindVariables)
          selectorNames = recordSelectorNames [inner]
          -- 'registerRecordSelectors' put the checked selector type on the
          -- field name. It also covers a field whose existential variable
          -- an equality of the context fixes, and it is the type that the
          -- uses of the selector instantiate.
          checkedTypes =
            Map.fromList
              [ (unqualifiedNameText name, ty)
              | name <- recordFieldNamesOf inner,
                ty <- take 1 [selectorType | InfoSelectorType selectorType <- unqualifiedNameTc name]
              ]
          computedType fieldType
            | all (\variable -> any (sameTyVar variable) universals) (typeTyVars fieldType) = Just (foldr TcForAllTy (TcFunTy resultType fieldType) universals)
            | otherwise = Nothing
       in [ TcBindingResult (originTermKey origin selector) selector selectorType
          | ((maybeLabel, _), fieldType) <- zip sourceFields fieldTypes,
            Just label <- [maybeLabel],
            let selector = Map.findWithDefault label label selectorNames,
            Just selectorType <- [Map.lookup label checkedTypes <|> computedType fieldType]
          ]

-- | The field names of a record constructor declaration.
recordFieldNamesOf :: DataConDecl -> [UnqualifiedName]
recordFieldNamesOf declaration =
  case declaration of
    DataConAnn _ inner -> recordFieldNamesOf inner
    RecordCon _ _ _ _ fields -> concatMap fieldNames fields
    GadtCon _ _ _ _ (GadtRecordBody fields _) -> concatMap fieldNames fields
    _ -> []

-- | The type that the type checker put on a binder, when it has one.
binderCheckedType :: UnqualifiedName -> Maybe TcType
binderCheckedType name =
  listToMaybe [tcAnnType annotation | InfoType annotation <- unqualifiedNameTc name]

dataConIdentities :: DataConDecl -> [DataConIdentity]
dataConIdentities dataConDecl =
  case dataConDecl of
    DataConAnn _ inner -> dataConIdentities inner
    PrefixCon _ _ _ name _ -> [DeclaredDataCon name]
    InfixCon _ _ _ _ name _ -> [DeclaredDataCon name]
    RecordCon _ _ _ name _ -> [DeclaredDataCon name]
    GadtCon _ _ _ names _ -> map DeclaredDataCon names
    TupleCon _ _ _ flavor fields -> [BuiltinDataConIdentity (BuiltinTupleCon flavor (length fields))]
    UnboxedSumCon _ _ _ alternative arity _ -> [BuiltinDataConIdentity (BuiltinUnboxedSumCon alternative arity)]
    ListCon {} -> [BuiltinDataConIdentity BuiltinNilCon]

-- | The name a constructor has in the term environment. A declared
-- constructor is named by its source. A built-in one is named by the
-- identity the wiring gives it, which is the name every use of the form
-- looks up.
dataConIdentityName :: TcWiring -> DataConIdentity -> Text
dataConIdentityName wiring identity =
  case identity of
    DeclaredDataCon name -> unqualifiedNameText name
    BuiltinDataConIdentity builtin -> tyConName (builtinDataCon wiring builtin)

binderBindingName :: UnqualifiedName -> (Text, Text)
binderBindingName name =
  (unqualifiedNameText name, renderBinderName name)

-- | The key a binding of this module gets. The module a binding is
-- recovered from is the module that declares it, so its origin is the
-- identity every consumer indexes it by.
originTermKey :: (Text, Text) -> Text -> Entity
originTermKey (package, moduleName') = GlobalTerm (PackageId package) moduleName'

splitFunctionType :: TcType -> ([TcType], TcType)
splitFunctionType ty =
  case ty of
    TcForAllTy _ body -> splitFunctionType body
    TcFunTy argument result ->
      let (arguments, finalResult) = splitFunctionType result
       in (argument : arguments, finalResult)
    _ -> ([], ty)

peelForAlls :: TcType -> ([TyVarId], TcType)
peelForAlls (TcForAllTy tv body) =
  let (tvs, inner) = peelForAlls body
   in (tv : tvs, inner)
peelForAlls ty = ([], ty)

-- | The source form, the labelled fields, and the constructors of one
-- declaration. The names follow 'dataConIdentities'.
dataConSourceLayout :: DataConDecl -> (DataConSourceForm, [(Maybe Text, BangType)], [DataConIdentity])
dataConSourceLayout declaration =
  case declaration of
    DataConAnn _ inner -> dataConSourceLayout inner
    PrefixCon _ _ _ _ fields ->
      (PrefixDataCon, map (Nothing,) fields, identities)
    InfixCon _ _ _ left _ right ->
      (InfixDataCon, map (Nothing,) [left, right], identities)
    RecordCon _ _ _ _ fields ->
      (RecordDataCon, recordSourceFields fields, identities)
    TupleCon _ _ _ Boxed fields ->
      (SyntaxDataCon, map (Nothing,) fields, identities)
    TupleCon _ _ _ Unboxed fields ->
      (UnboxedTupleDataCon, map (Nothing,) fields, identities)
    UnboxedSumCon _ _ _ alternative arity field ->
      (UnboxedSumDataCon alternative arity, [(Nothing, field)], identities)
    ListCon {} ->
      (SyntaxDataCon, [], identities)
    GadtCon _ _ _ _ body ->
      case body of
        GadtPrefixBody fields _ -> (PrefixDataCon, map ((Nothing,) . fst) fields, identities)
        GadtRecordBody fields _ -> (RecordDataCon, recordSourceFields fields, identities)
  where
    identities = dataConIdentities declaration

recordSourceFields :: [FieldDecl] -> [(Maybe Text, BangType)]
recordSourceFields = concatMap $ \field ->
  [(Just (unqualifiedNameText label), fieldType field) | label <- fieldNames field]

-- | Render an unqualified name for display.
-- Operators (NameVarSym, NameConSym) are wrapped in parentheses.
renderBinderName :: UnqualifiedName -> Text
renderBinderName uname =
  case unqualifiedNameType uname of
    NameVarSym -> "(" <> unqualifiedNameText uname <> ")"
    NameConSym -> "(" <> unqualifiedNameText uname <> ")"
    _ -> unqualifiedNameText uname

-- | The package and the name of a checked module.
resolvedModuleOrigin :: Module -> (Text, Text)
resolvedModuleOrigin resolvedModule =
  case moduleIdentity resolvedModule of
    Just (TcModuleIdentity packageId moduleName') -> (packageIdText packageId, moduleName')
    Nothing -> ("", fromMaybe "Main" (moduleName resolvedModule))

-- | The variables that a pattern binds, in source order. Every pattern form
-- that can hold a sub-pattern is walked, so a tuple, list, record, view,
-- or signature pattern in a binding position lists the same binders as a
-- constructor pattern.
patternBinderNames :: Pattern -> [UnqualifiedName]
patternBinderNames pat =
  case pat of
    PAnn _ inner -> patternBinderNames inner
    PVar _ name -> [name]
    PTypeBinder _ _ -> []
    PTypeSyntax _ _ _ -> []
    PWildcard _ -> []
    PLit _ _ _ -> []
    PQuasiQuote _ _ _ -> []
    PTuple _ _ items -> concatMap patternBinderNames items
    PUnboxedSum _ _ _ inner -> patternBinderNames inner
    PList _ _ items -> concatMap patternBinderNames items
    PCon _ _ _ pats -> concatMap patternBinderNames pats
    PBuiltinCon _ _ _ pats -> concatMap patternBinderNames pats
    PInfix _ lhs _ rhs -> patternBinderNames lhs <> patternBinderNames rhs
    PView _ _ inner -> patternBinderNames inner
    PAs _ name inner -> name : patternBinderNames inner
    PStrict _ inner -> patternBinderNames inner
    PIrrefutable _ inner -> patternBinderNames inner
    PNegLit _ _ _ -> []
    PNPlusK _ _ name _ -> [name]
    PParen _ inner -> patternBinderNames inner
    PRecord _ _ fields _ -> concatMap (patternBinderNames . recordFieldValue) fields
    PTypeSig _ inner _ -> patternBinderNames inner
    PSplice _ _ -> []

-- | The variables that the statements of a recursive @do@ group bind, in
-- source order and without repeats. The knot of the group is a tuple of
-- these variables. The type checker and the desugarer both use this order.
recStmtBinderNames :: [DoStmt Expr] -> [UnqualifiedName]
recStmtBinderNames =
  List.nubBy (\left right -> unqualifiedNameText left == unqualifiedNameText right) . concatMap stmtBinders
  where
    stmtBinders stmt =
      case stmt of
        DoAnn _ inner -> stmtBinders inner
        DoRecKnot _ inner -> stmtBinders inner
        DoBind _ pat _ -> patternBinderNames pat
        DoLetDecls decls -> concatMap declBinders decls
        DoExpr {} -> []
        DoRecStmt _ _ stmts -> concatMap stmtBinders stmts
    declBinders decl =
      case peelDeclAnn decl of
        DeclValue (FunctionBind name _) -> [name]
        DeclValue (PatternBind _ pat _) -> patternBinderNames pat
        _ -> []

-- | The variables that the statements of one branch of a parallel list
-- comprehension bind, in source order and without repeats. The branch
-- gives a list of tuples of these variables. The type checker and the
-- desugarer both use this order.
compStmtBinderNames :: [CompStmt] -> [UnqualifiedName]
compStmtBinderNames =
  List.nubBy (\left right -> unqualifiedNameText left == unqualifiedNameText right) . concatMap stmtBinders
  where
    stmtBinders stmt =
      case stmt of
        CompAnn _ inner -> stmtBinders inner
        CompGen _ pat _ -> patternBinderNames pat
        CompLetDecls _ decls -> concatMap declBinders decls
        _ -> []
    declBinders decl =
      case peelDeclAnn decl of
        DeclValue (FunctionBind name _) -> [name]
        DeclValue (PatternBind _ pat _) -> patternBinderNames pat
        _ -> []
