{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Write derived instances as surface instance declarations.
--
-- A deriving plan with a resolved context becomes an ordinary
-- @instance ctx => C (T a) where ...@ whose method equations are built here
-- from the checked constructor layout of the datatype. The declaration then
-- goes through the same instance checker and System FC lowering as source,
-- so no later phase needs to know that it was derived.
--
-- Generated names carry resolver annotations, the way the resolver would
-- have left them: constructors and methods point at their defining module,
-- pattern variables are locals with uniques that no resolver local uses, and
-- library values come from the 'DerivingReferences' of the configuration.
module Aihc.Tc.Deriving.Generate
  ( generateDerivedInstances,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    ArrowKind (..),
    CaseAlt (..),
    Decl (..),
    Expr (..),
    InstanceDecl (..),
    InstanceDeclItem (..),
    Match (..),
    MatchHeadForm (..),
    Module (..),
    Name (..),
    NameType (..),
    NumericType (..),
    Pattern (..),
    Rhs (..),
    SourceSpan,
    StandaloneDerivingDecl (..),
    Type (..),
    TypeFamilyInst (..),
    TypeHeadForm (..),
    TypePromotion (..),
    UnqualifiedName (..),
    ValueDecl (..),
    fromAnnotation,
    mkAnnotation,
    mkUnqualifiedName,
    peelDeclAnn,
    qualifyName,
  )
import Aihc.Resolve (Identifier (..), PackageId (..), ResolutionAnnotation (..), ResolutionNamespace (..), ResolvedName (..))
import Aihc.Tc.Annotations
  ( TcCoercedDeriving (..),
    TcDerivedInstance (..),
    TcDerivingAnnotation (..),
    TcDerivingContext (..),
    TcDerivingPlan (..),
    TcDerivingStrategy (..),
  )
import Aihc.Tc.Deriving.Context (newtypeRepresentation, stockFieldTypes, stockFunctorialFields)
import Aihc.Tc.Deriving.Functorial (FieldUse (..), fieldUse)
import Aihc.Tc.Deriving.References
import Aihc.Tc.Deriving.StockClass (StockClass (..), StockMethods (..), generatesStockMethods, lookupStockClass, stockClassMethodsOf)
import Aihc.Tc.Deriving.Strategy (isGeneratedStockClass)
import Aihc.Tc.Env (AssociatedTypeInfo (..), ClassInfo (..), DataConFieldInfo (..), DataConFieldUnpack (..), DataConInfo (..), DataConSourceForm (..), DataTypeInfo (..), TyConFlavor (..))
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Monad
import Aihc.Tc.Types
import Control.Monad (forM, zipWithM)
import Data.Foldable (foldrM)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T

-- | The instance declarations for the selected deriving plans of a module. A plan
-- whose context is unresolved has already reported its error; a plan for a
-- strategy or class the generator does not support reports a warning and
-- produces no instance.
generateDerivedInstances :: (TcDerivingPlan -> Bool) -> (Text, Text) -> Module -> TcM [Decl]
generateDerivedInstances selected origin modu = do
  references <- getDerivingReferences
  primPackage <- getPrimPackage
  concat <$> mapM (declDerivedInstances selected references primPackage origin) (moduleDecls modu)

declDerivedInstances :: (TcDerivingPlan -> Bool) -> DerivingReferences -> PackageId -> (Text, Text) -> Decl -> TcM [Decl]
declDerivedInstances selected references primPackage origin decl =
  case decl of
    DeclAnn annotation inner -> do
      own <-
        case fromAnnotation @TcDerivingAnnotation annotation of
          Just derivingAnnotation -> do
            kinds <- getKinds
            catMaybes <$> mapM (generatePlan kinds references primPackage origin (peelDeclAnn inner)) (filter selected (tcDerivingPlans derivingAnnotation))
          Nothing -> pure []
      rest <- declDerivedInstances selected references primPackage origin inner
      pure (own <> rest)
    _ -> pure []

-- | The generation context of one plan.
data Gen = Gen
  { genSpan :: !(Maybe SourceSpan),
    genKinds :: !TcKinds,
    genReferences :: !DerivingReferences,
    -- | The primitive package, which most references come from.
    genPrimPackage :: !PackageId,
    genPlan :: !TcDerivingPlan,
    -- | Package and module of the class, where its methods live, and where
    -- a reference of the class package comes from.
    genClassOrigin :: !(Text, Text)
  }

generatePlan :: TcKinds -> DerivingReferences -> PackageId -> (Text, Text) -> Decl -> TcDerivingPlan -> TcM (Maybe Decl)
generatePlan kinds references primPackage origin sourceDecl plan =
  case supportedStrategy of
    Left message -> do
      emitWarning (tcDerivingSourceSpan plan) (OtherError message)
      pure Nothing
    Right () ->
      case tcDerivingContext plan of
        -- Context inference already reported why the plan has no context.
        TcDerivingInferContext -> pure Nothing
        TcDerivingExplicitContext context -> do
          available <- referencesAvailable gen
          case (available, instanceHeader context) of
            (Just missing, _) -> do
              emitError (genSpan gen) (OtherError (mechanism <> " needs " <> missing <> ", which is not available in this compilation"))
              pure Nothing
            (Nothing, Nothing) -> do
              emitError (genSpan gen) (OtherError (mechanism <> " cannot express the instance head or context as source syntax"))
              pure Nothing
            (Nothing, Just (forallBinders, surfaceContext, surfaceHead)) -> do
              items <- generateItems gen
              pure $
                items <&> \generated ->
                  markCoerced $
                    DeclAnn (mkAnnotation TcDerivedInstance) $
                      maybe id (DeclAnn . mkAnnotation) (genSpan gen) $
                        DeclInstance
                          InstanceDecl
                            { instanceDeclPragmas = [],
                              instanceDeclWarning = Nothing,
                              instanceDeclForall = forallBinders,
                              instanceDeclContext = surfaceContext,
                              instanceDeclHead = surfaceHead,
                              instanceDeclItems = generated
                            }
  where
    -- Both strategies that coerce another instance need the plan again when
    -- the generated header is checked, to prove the casts of its methods.
    markCoerced declaration =
      case tcDerivingStrategy plan of
        TcDerivingNewtype -> tagged
        TcDerivingVia {} -> tagged
        _ -> declaration
      where
        tagged = DeclAnn (mkAnnotation (TcCoercedDeriving plan)) declaration
    gen =
      Gen
        { genSpan = tcDerivingSourceSpan plan,
          genKinds = kinds,
          genReferences = references,
          genPrimPackage = primPackage,
          genPlan = plan,
          genClassOrigin = fromMaybe origin (tcDerivingClassOrigin plan)
        }
    className = T.unpack (tcDerivingClassName plan)
    datatypeDescription = maybe "datatype" (("datatype " <>) . T.unpack . dtiName) (tcDerivingDataType plan)
    mechanism =
      case tcDerivingStrategy plan of
        TcDerivingStock -> "stock " <> className <> " deriving"
        TcDerivingNewtype -> "newtype " <> className <> " deriving"
        TcDerivingAnyclass -> "anyclass " <> className <> " deriving"
        TcDerivingVia {} -> "via " <> className <> " deriving"
    supportedStrategy =
      case tcDerivingStrategy plan of
        TcDerivingAnyclass -> Right ()
        TcDerivingNewtype -> Right ()
        TcDerivingStock
          -- The class must be the one the configuration names, package
          -- included. A user class that repeats a core-library module name
          -- reaches here after the strategy check reported it, and the
          -- generator must not write method bodies for it.
          | not (isGeneratedStockClass references (tcDerivingClassName plan) (tcDerivingClassOrigin plan)) ->
              Left ("stock deriving of " <> className <> " is not available for a class outside the core libraries")
          -- A representation names the datatype applied to its arguments,
          -- and the kind arguments of a poly-kinded head are not among
          -- them: source syntax cannot write an invisible argument, so the
          -- equation would fix each one at 'Type' while the instance
          -- methods keep it a variable. Until the kind arguments can be
          -- written, such a datatype gets no instance.
          | stockClassMethodsOf (tcDerivingClassName plan) == Just StockGenericMethods,
            not (all (null . datatypeKindVariables) (tcDerivingDataType plan)) ->
              Left ("stock deriving of " <> className <> " is not supported for the poly-kinded " <> datatypeDescription <> "; no instance is generated")
          | generatesStockMethods (tcDerivingClassName plan) -> Right ()
          | otherwise -> Left ("stock deriving of " <> className <> " is not supported yet; no instance is generated")
        TcDerivingVia {} -> Right ()
    -- A standalone declaration keeps the syntax the user wrote. An attached
    -- clause renders its checked head and inferred context.
    instanceHeader context =
      case sourceDecl of
        DeclStandaloneDeriving derivingDecl ->
          Just (standaloneDerivingForall derivingDecl, standaloneDerivingContext derivingDecl, standaloneDerivingHead derivingDecl)
        _ -> do
          surfaceContext <- mapM (surfacePred (genSpan gen)) context
          headArguments <- mapM (surfaceType (genSpan gen)) (tcDerivingHeadTypes plan)
          pure ([], surfaceContext, foldl TApp (TCon (tyConNameSyntax (genSpan gen) (tcDerivingClassTyCon plan)) Unpromoted) headArguments)

-- | The method equations of a plan, or 'Nothing' after reporting why the
-- datatype cannot be derived.
generateItems :: Gen -> TcM (Maybe [InstanceDeclItem])
generateItems gen =
  case tcDerivingStrategy plan of
    TcDerivingAnyclass -> pure (Just [])
    TcDerivingNewtype ->
      case newtypeRepresentation plan of
        Left message -> failWith message
        Right representation -> associatedItems gen representation
    TcDerivingStock ->
      case (stockFieldTypes plan, tcDerivingDataType plan) of
        (Left message, _) -> failWith message
        (Right _, Just dataType) ->
          let constructors = dtiConstructors dataType
           in case stockClassMethodsOf (tcDerivingClassName plan) of
                Just StockEqMethods -> Just <$> eqItems gen constructors
                Just StockOrdMethods -> Just <$> ordItems gen constructors
                Just StockShowMethods -> Just <$> showItems gen constructors
                Just StockReadMethods -> Just <$> readItems gen constructors
                Just StockBoundedMethods -> boundedItems gen constructors
                Just StockLiftMethods -> Just <$> liftItems gen constructors
                Just StockFunctorMethods -> functorialItems gen functorItems constructors
                Just StockFoldableMethods -> functorialItems gen foldableItems constructors
                Just StockTraversableMethods -> functorialItems gen traversableItems constructors
                Just StockGenericMethods -> genericItems gen dataType
                Nothing -> failWith ("stock deriving of " <> T.unpack (tcDerivingClassName plan) <> " is not supported yet")
        (Right _, Nothing) -> failWith "stock deriving requires checked datatype metadata"
    TcDerivingVia viaType -> associatedItems gen viaType
  where
    plan = genPlan gen
    failWith message = do
      emitError (genSpan gen) (OtherError message)
      pure Nothing

-- | The first library reference the plan needs that the type environment
-- does not know, as a description for the error message.
referencesAvailable :: Gen -> TcM (Maybe String)
referencesAvailable gen = do
  present <- forM needed $ \reference -> do
    let (package, moduleName, name) = referenceIdentityOf gen reference
    binder <- lookupTermKey (TcTermGlobal package moduleName name)
    pure (reference, binder)
  pure (listToMaybe [describe reference | (reference, Nothing) <- present])
  where
    references = genReferences gen
    -- Only a stock body mentions a library name that is not a method of
    -- the class being derived; the coercing strategies mention none.
    needed =
      case tcDerivingStrategy (genPlan gen) of
        TcDerivingStock ->
          [ select references
          | Just stockClass <- [lookupStockClass (tcDerivingClassName (genPlan gen))],
            select <- stockClassReferences stockClass
          ]
        _ -> []
    describe reference = T.unpack (referenceModule reference <> "." <> referenceName reference)
    listToMaybe candidates =
      case candidates of
        [] -> Nothing
        candidate : _ -> Just candidate

-- * Eq

eqItems :: Gen -> [DataConInfo] -> TcM [InstanceDeclItem]
eqItems gen constructors = do
  matches <- mapM constructorMatch constructors
  let fallback = [simpleMatch gen [atPattern gen PWildcard, atPattern gen PWildcard] (referenceExpr gen derivingFalse) | length constructors > 1]
  pure [methodBind gen "==" (matches <> fallback)]
  where
    constructorMatch constructor = do
      lefts <- fieldLocals gen "a" constructor
      rights <- fieldLocals gen "b" constructor
      pure
        ( simpleMatch
            gen
            [constructorPattern gen constructor (map Just lefts), constructorPattern gen constructor (map Just rights)]
            (conjunction [methodApp gen "==" [localExpr gen left, localExpr gen right] | (left, right) <- zip lefts rights])
        )
    conjunction tests =
      case tests of
        [] -> referenceExpr gen derivingTrue
        [test] -> test
        test : rest ->
          caseOf
            gen
            test
            [ (referencePattern gen derivingTrue, conjunction rest),
              (referencePattern gen derivingFalse, referenceExpr gen derivingFalse)
            ]

-- * Ord

ordItems :: Gen -> [DataConInfo] -> TcM [InstanceDeclItem]
ordItems gen constructors =
  case constructors of
    [constructor] -> do
      lefts <- fieldLocals gen "a" constructor
      rights <- fieldLocals gen "b" constructor
      pure
        [ methodBind
            gen
            "compare"
            [simpleMatch gen [constructorPattern gen constructor (map Just lefts), constructorPattern gen constructor (map Just rights)] (compareFields (zip lefts rights))]
        ]
    _ -> do
      left <- freshLocal gen "x"
      right <- freshLocal gen "y"
      alternatives <- mapM (outerAlternative right) (zip [0 :: Int ..] constructors)
      pure [methodBind gen "compare" [simpleMatch gen [atPattern gen (PVar left), atPattern gen (PVar right)] (caseOf gen (localExpr gen left) alternatives)]]
  where
    lastIndex = length constructors - 1
    -- The constructors before this one compare greater, the same
    -- constructor compares its fields, and every later one compares less.
    outerAlternative right (index, constructor) = do
      lefts <- fieldLocals gen "a" constructor
      rights <- fieldLocals gen "b" constructor
      let earlier =
            [ (constructorPattern gen other (map (const Nothing) (dciFields other)), referenceExpr gen derivingGT)
            | other <- take index constructors
            ]
          same = (constructorPattern gen constructor (map Just rights), compareFields (zip lefts rights))
          later = [(atPattern gen PWildcard, referenceExpr gen derivingLT) | index < lastIndex]
      pure (constructorPattern gen constructor (map Just lefts), caseOf gen (localExpr gen right) (earlier <> [same] <> later))
    compareFields pairs =
      case pairs of
        [] -> referenceExpr gen derivingEQ
        [(left, right)] -> methodApp gen "compare" [localExpr gen left, localExpr gen right]
        (left, right) : rest ->
          caseOf
            gen
            (methodApp gen "compare" [localExpr gen left, localExpr gen right])
            [ (referencePattern gen derivingLT, referenceExpr gen derivingLT),
              (referencePattern gen derivingGT, referenceExpr gen derivingGT),
              (referencePattern gen derivingEQ, compareFields rest)
            ]

-- * Show

showItems :: Gen -> [DataConInfo] -> TcM [InstanceDeclItem]
showItems gen constructors = do
  matches <- mapM constructorMatch constructors
  pure [methodBind gen "showsPrec" matches]
  where
    constructorMatch constructor
      | null (dciFields constructor) = do
          body <- showStringExpr (prefixConstructorText constructor)
          pure (simpleMatch gen [atPattern gen PWildcard, constructorPattern gen constructor []] body)
      | otherwise = do
          precedence <- freshLocal gen "d"
          fields <- fieldLocals gen "a" constructor
          suffix <- freshLocal gen "s"
          rendering <- freshLocal gen "k"
          tail' <- freshLocal gen "t"
          let (parenPrecedence, pieces) = constructorPieces constructor fields
              -- @showParen (d >= p) k s@, with the rendering @k@ bound once
              -- so the pieces are not written out for both branches.
              body =
                lambda gen suffix $
                  applyN
                    gen
                    ( lambda gen rendering $
                        caseOf
                          gen
                          (applyN gen (referenceExpr gen derivingGreaterOrEqual) [localExpr gen precedence, intLiteral gen parenPrecedence])
                          [ (referencePattern gen derivingTrue, cons '(' (applyN gen (localExpr gen rendering) [cons ')' (localExpr gen suffix)])),
                            (referencePattern gen derivingFalse, applyN gen (localExpr gen rendering) [localExpr gen suffix])
                          ]
                    )
                    [lambda gen tail' (foldr ($) (localExpr gen tail') pieces)]
          pure (simpleMatch gen [atPattern gen (PVar precedence), constructorPattern gen constructor (map Just fields)] body)
    -- The pieces of the rendering, each applied to the rest of the output,
    -- with the precedence above which the whole needs parentheses.
    constructorPieces constructor fields =
      case dciSourceForm constructor of
        RecordDataCon ->
          ( 11,
            showStringPiece (prefixConstructorText constructor <> " {")
              : concat
                [ [showStringPiece (separator <> fieldLabelText label <> " = "), showsPrecPiece 0 field]
                | (index, (label, field)) <- zip [0 :: Int ..] (zip (map dcfiLabel (dciFields constructor)) fields),
                  let separator = if index == 0 then "" else ", "
                ]
                <> [showStringPiece "}"]
          )
        InfixDataCon
          | [left, right] <- fields ->
              -- Without fixity information every infix constructor takes
              -- the default fixity 9.
              ( 10,
                [showsPrecPiece 10 left, showStringPiece (" " <> infixConstructorText constructor <> " "), showsPrecPiece 10 right]
              )
        _ ->
          ( 11,
            showStringPiece (prefixConstructorText constructor <> " ")
              : intersperseWith (showStringPiece " ") (map (showsPrecPiece 11) fields)
          )
    -- @showString text@ as a chain of list constructors, so the rendering
    -- needs nothing beyond the primitive package.
    showStringPiece text rest = foldr cons rest (T.unpack text)
    showsPrecPiece precedence field rest = methodApp gen "showsPrec" [intLiteral gen precedence, localExpr gen field, rest]
    showStringExpr text = do
      suffix <- freshLocal gen "s"
      pure (lambda gen suffix (showStringPiece text (localExpr gen suffix)))
    cons character rest = applyN gen (referenceExpr gen derivingCons) [at gen (EChar character (T.pack (show character))), rest]
    intersperseWith separator pieces =
      case pieces of
        [] -> []
        [piece] -> [piece]
        piece : rest -> piece : separator : intersperseWith separator rest

prefixConstructorText :: DataConInfo -> Text
prefixConstructorText constructor
  | isSymbolic (dciName constructor) = "(" <> dciName constructor <> ")"
  | otherwise = dciName constructor

infixConstructorText :: DataConInfo -> Text
infixConstructorText constructor
  | isSymbolic (dciName constructor) = dciName constructor
  | otherwise = "`" <> dciName constructor <> "`"

fieldLabelText :: Maybe Text -> Text
fieldLabelText label =
  case label of
    Just text
      | isSymbolic text -> "(" <> text <> ")"
      | otherwise -> text
    Nothing -> ""

-- * Read

-- | The @Read@ methods of a datatype, in the shape GHC derives: one
-- @readPrec@ alternative for each constructor, and the list methods that
-- break the mutual recursion of the class defaults.
readItems :: Gen -> [DataConInfo] -> TcM [InstanceDeclItem]
readItems gen constructors = do
  alternatives <- mapM constructorParser constructors
  let body =
        case alternatives of
          [] -> referenceExpr gen derivingReadFail
          first : rest -> foldl alternative first rest
  -- The class defaults give the other three methods. They do not call each
  -- other, so @readPrec@ alone is a complete instance.
  pure [methodBind gen "readPrec" [simpleMatch gen [] (applyN gen (referenceExpr gen derivingReadParens) [body])]]
  where
    alternative left right = applyN gen (referenceExpr gen derivingReadAlternative) [left, right]

    -- The parser of one constructor. A nullary constructor needs no
    -- precedence context, because it consumes one lexeme.
    constructorParser constructor
      | null (dciFields constructor) =
          pure (thenParse (expectConstructorName constructor) (returnParse (constructorExpr gen constructor)))
      | otherwise = do
          fields <- fieldLocals gen "a" constructor
          let (precedence, parser) = constructorBody constructor fields
          pure (atPrecedence precedence parser)

    -- The precedence context of the alternative, and the parser inside it.
    constructorBody constructor fields =
      case dciSourceForm constructor of
        RecordDataCon ->
          ( 11,
            thenParse
              (expectConstructorName constructor)
              ( thenParse
                  (expectPunc "{")
                  (recordFields (zip (map dcfiLabel (dciFields constructor)) fields))
              )
          )
          where
            recordFields pairs =
              case pairs of
                [] -> thenParse (expectPunc "}") result
                (label, field) : rest ->
                  bindParse
                    (fieldParser label)
                    field
                    (if null rest then recordFields rest else thenParse (expectPunc ",") (recordFields rest))
            result = returnParse (applyN gen (constructorExpr gen constructor) (map (localExpr gen) fields))
        InfixDataCon
          | [left, right] <- fields ->
              -- Without fixity information every infix constructor takes
              -- the default fixity 9.
              ( 9,
                bindParse
                  stepReadPrec
                  left
                  ( thenParse
                      (expectInfixName constructor)
                      (bindParse stepReadPrec right (returnParse (applyN gen (constructorExpr gen constructor) (map (localExpr gen) fields))))
                  )
              )
        _ ->
          ( 10,
            thenParse (expectConstructorName constructor) (prefixFields fields)
          )
          where
            prefixFields remaining =
              case remaining of
                [] -> returnParse (applyN gen (constructorExpr gen constructor) (map (localExpr gen) fields))
                field : rest -> bindParse stepReadPrec field (prefixFields rest)

    -- A record field reads at the lowest precedence after its label.
    fieldParser label =
      case label of
        Just text
          | isSymbolic text -> applyN gen (referenceExpr gen derivingReadSymField) [stringExpr gen text, resetReadPrec]
          | otherwise -> applyN gen (referenceExpr gen derivingReadField) [stringExpr gen text, resetReadPrec]
        Nothing -> resetReadPrec

    -- A symbolic constructor in prefix position keeps its parentheses,
    -- which lex as separate tokens.
    expectConstructorName constructor
      | isSymbolic (dciName constructor) =
          thenParse
            (expectPunc "(")
            (thenParse (expectLexeme derivingLexemeSymbol (dciName constructor)) (expectPunc ")"))
      | otherwise = expectIdent (dciName constructor)

    -- A backquoted constructor lexes as three tokens, an operator as one.
    expectInfixName constructor
      | isSymbolic (dciName constructor) = expectLexeme derivingLexemeSymbol (dciName constructor)
      | otherwise =
          thenParse
            (expectPunc "`")
            (thenParse (expectLexeme derivingLexemeIdent (dciName constructor)) (expectPunc "`"))

    atPrecedence precedence parser =
      applyN gen (referenceExpr gen derivingReadPrecContext) [intLiteral gen precedence, parser]
    stepReadPrec = applyN gen (referenceExpr gen derivingReadStep) [methodExpr gen "readPrec"]
    resetReadPrec = applyN gen (referenceExpr gen derivingReadReset) [methodExpr gen "readPrec"]
    expectIdent = expectLexeme derivingLexemeIdent
    expectPunc = expectLexeme derivingLexemePunc
    expectLexeme select text =
      applyN gen (referenceExpr gen derivingReadExpect) [applyN gen (referenceExpr gen select) [stringExpr gen text]]
    thenParse first rest = applyN gen (referenceExpr gen derivingThen) [first, rest]
    bindParse parser binder rest =
      applyN gen (referenceExpr gen derivingBind) [parser, lambda gen binder rest]
    returnParse value = applyN gen (referenceExpr gen derivingReturn) [value]

-- | A string as an explicit list of characters, so the generated code needs
-- no literal desugaring.
stringExpr :: Gen -> Text -> Expr
stringExpr gen text = at gen (EList [at gen (EChar character (T.pack (show character))) | character <- T.unpack text])

-- * Bounded

boundedItems :: Gen -> [DataConInfo] -> TcM (Maybe [InstanceDeclItem])
boundedItems gen constructors
  | all (null . dciFields) constructors,
    first : _ <- constructors =
      pure (Just [bound "minBound" (constructorExpr gen first), bound "maxBound" (constructorExpr gen (last constructors))])
  | [constructor] <- constructors =
      pure
        ( Just
            [ bound "minBound" (applyN gen (constructorExpr gen constructor) (map (const (methodExpr gen "minBound")) (dciFields constructor))),
              bound "maxBound" (applyN gen (constructorExpr gen constructor) (map (const (methodExpr gen "maxBound")) (dciFields constructor)))
            ]
        )
  | otherwise = do
      emitError (genSpan gen) (OtherError "stock Bounded deriving requires an enumeration or a single-constructor type")
      pure Nothing
  where
    bound name body = methodBind gen name [simpleMatch gen [] body]

-- * Lift

-- | @lift@ rebuilds the value as a Template Haskell expression: the
-- constructor by its package, module and spelling, applied to the lifted
-- fields. The fields are lifted in the quoting monad, which the @Quote@
-- constraint of the method makes a @Monad@, and the expression itself is
-- built from the constructors of @Exp@. @liftTyped@ is the same expression,
-- coerced.
liftItems :: Gen -> [DataConInfo] -> TcM [InstanceDeclItem]
liftItems gen constructors = do
  matches <- mapM constructorMatch constructors
  value <- freshLocal gen "x"
  pure
    [ methodBind gen "lift" matches,
      methodBind
        gen
        "liftTyped"
        [ simpleMatch
            gen
            [atPattern gen (PVar value)]
            (applyN gen (referenceExpr gen derivingLiftCodeCoerce) [methodApp gen "lift" [localExpr gen value]])
        ]
    ]
  where
    constructorMatch constructor = do
      fields <- fieldLocals gen "a" constructor
      lifted <- mapM (const (freshLocal gen "e")) fields
      let application = foldl applyLifted (liftedConstructor constructor) lifted
          delivered = applyN gen (referenceExpr gen derivingPure) [application]
      pure
        ( simpleMatch
            gen
            [constructorPattern gen constructor (map Just fields)]
            (foldr liftField delivered (zip fields lifted))
        )
    -- Each field is lifted before the expression is assembled, because a
    -- lift happens in the quoting monad.
    liftField (field, binder) rest =
      applyN
        gen
        (referenceExpr gen derivingBind)
        [methodApp gen "lift" [localExpr gen field], lambda gen binder rest]
    applyLifted function argument =
      applyN gen (referenceExpr gen derivingLiftAppE) [function, localExpr gen argument]
    liftedConstructor constructor =
      applyN gen (referenceExpr gen derivingLiftConE) [constructorNameExpr gen constructor]

-- | The Template Haskell name of a data constructor, which carries the
-- package and the module it is declared in.
constructorNameExpr :: Gen -> DataConInfo -> Expr
constructorNameExpr gen constructor =
  applyN
    gen
    (referenceExpr gen derivingLiftDataConName)
    [stringExpr gen (packageIdText packageId), stringExpr gen moduleName', stringExpr gen (dciName constructor)]
  where
    (packageId, moduleName') = dciOrigin constructor

-- * Functor, Foldable and Traversable

-- | The equations of a functor-like class, or 'Nothing' after reporting a
-- field that uses the last datatype parameter somewhere no instance can
-- reach.
functorialItems :: Gen -> (Gen -> [(DataConInfo, [FieldUse])] -> TcM [InstanceDeclItem]) -> [DataConInfo] -> TcM (Maybe [InstanceDeclItem])
functorialItems gen build constructors =
  case functorialUses (genPlan gen) constructors of
    Left message -> do
      emitError (genSpan gen) (OtherError message)
      pure Nothing
    Right uses -> Just <$> build gen uses

-- | What every field of every constructor does with the last parameter.
functorialUses :: TcDerivingPlan -> [DataConInfo] -> Either String [(DataConInfo, [FieldUse])]
functorialUses plan constructors = do
  (parameter, fieldTypes) <- stockFunctorialFields plan
  uses <- mapM (mapM (fieldUse mechanism parameter)) fieldTypes
  pure (zip constructors uses)
  where
    mechanism = "stock " <> T.unpack (tcDerivingClassName plan) <> " deriving"

functorItems :: Gen -> [(DataConInfo, [FieldUse])] -> TcM [InstanceDeclItem]
functorItems gen constructors = do
  matches <- mapM constructorMatch constructors
  pure [methodBind gen "fmap" matches]
  where
    constructorMatch (constructor, uses) = do
      function <- freshLocal gen "f"
      fields <- fieldLocals gen "a" constructor
      mapped <- zipWithM (mapField function) uses (map (localExpr gen) fields)
      pure
        ( simpleMatch
            gen
            [atPattern gen (PVar function), constructorPattern gen constructor (map Just fields)]
            (applyN gen (constructorExpr gen constructor) mapped)
        )
    mapField function use value =
      case use of
        FieldAbsent -> pure value
        FieldParameter -> pure (applyN gen (localExpr gen function) [value])
        FieldContainer _ inner -> do
          step <- fieldFunction gen (mapField function) function inner
          pure (methodApp gen "fmap" [step, value])

foldableItems :: Gen -> [(DataConInfo, [FieldUse])] -> TcM [InstanceDeclItem]
foldableItems gen constructors = do
  matches <- mapM constructorMatch constructors
  pure [methodBind gen "foldr" matches]
  where
    constructorMatch (constructor, uses) = do
      function <- freshLocal gen "f"
      initial <- freshLocal gen "z"
      fields <- fieldLocals gen "a" constructor
      -- The fields fold from the right, so each one wraps what the fields
      -- after it have already folded.
      body <-
        foldrM
          (\(use, field) rest -> foldField function use (localExpr gen field) rest)
          (localExpr gen initial)
          (zip uses fields)
      pure
        ( simpleMatch
            gen
            [ atPattern gen (PVar function),
              atPattern gen (PVar initial),
              constructorPattern gen constructor (map Just fields)
            ]
            body
        )
    foldField function use value rest =
      case use of
        FieldAbsent -> pure rest
        FieldParameter -> pure (applyN gen (localExpr gen function) [value, rest])
        FieldContainer _ inner -> do
          step <- foldStep function inner
          pure (methodApp gen "foldr" [step, rest, value])
    -- The step of a nested fold takes the element and what follows it.
    foldStep function inner =
      case inner of
        FieldParameter -> pure (localExpr gen function)
        _ -> do
          element <- freshLocal gen "y"
          rest <- freshLocal gen "r"
          body <- foldField function inner (localExpr gen element) (localExpr gen rest)
          pure (lambda gen element (lambda gen rest body))

traversableItems :: Gen -> [(DataConInfo, [FieldUse])] -> TcM [InstanceDeclItem]
traversableItems gen constructors = do
  matches <- mapM constructorMatch constructors
  pure [methodBind gen "traverse" matches]
  where
    constructorMatch (constructor, uses) = do
      function <- freshLocal gen "f"
      fields <- fieldLocals gen "a" constructor
      visited <- zipWithM (visitField function) uses (map (localExpr gen) fields)
      let applied = applyN gen (referenceExpr gen derivingPure) [constructorExpr gen constructor]
      pure
        ( simpleMatch
            gen
            [atPattern gen (PVar function), constructorPattern gen constructor (map Just fields)]
            (foldl (\left right -> applyN gen (referenceExpr gen derivingApply) [left, right]) applied visited)
        )
    visitField function use value =
      case use of
        FieldAbsent -> pure (applyN gen (referenceExpr gen derivingPure) [value])
        FieldParameter -> pure (applyN gen (localExpr gen function) [value])
        FieldContainer _ inner -> do
          step <- fieldFunction gen (visitField function) function inner
          pure (methodApp gen "traverse" [step, value])

-- | The function a nested position is visited with: the function the method
-- was given when the position is the parameter itself, and a lambda that
-- goes one level deeper otherwise.
fieldFunction :: Gen -> (FieldUse -> Expr -> TcM Expr) -> UnqualifiedName -> FieldUse -> TcM Expr
fieldFunction gen visit function inner =
  case inner of
    FieldParameter -> pure (localExpr gen function)
    _ -> do
      element <- freshLocal gen "y"
      body <- visit inner (localExpr gen element)
      pure (lambda gen element body)

-- * Newtype

-- | Each class method at the newtype, wrapped and unwrapped around the
-- method at the representation type. A method whose type mentions the
-- class parameter somewhere the wrapper cannot reach keeps its default,
-- or is reported when it has none.
-- | Forward associated equations to the representation type.
associatedItems :: Gen -> TcType -> TcM (Maybe [InstanceDeclItem])
associatedItems gen representation = do
  info <- lookupClass (tcDerivingClassTyCon plan)
  case info of
    Nothing -> pure Nothing
    Just classInfo -> sequence <$> mapM familyItem (ciAssociatedTypes classInfo)
  where
    plan = genPlan gen
    heads = tcDerivingHeadTypes plan
    targetPosition = length heads - 1
    familyItem associated
      | Just targetPosition `notElem` atiClassParams associated = reject
      | otherwise = case sequence (atiClassParams associated) of
          Nothing -> reject
          Just positions -> do
            let sourceHeads = init heads <> [representation]
                left = TcTyCon (atiTyCon associated) [heads !! position | position <- positions]
                right = TcTyCon (atiTyCon associated) [sourceHeads !! position | position <- positions]
            case (surfaceType (genSpan gen) left, surfaceType (genSpan gen) right) of
              (Just lhs, Just rhs) -> pure (Just (InstanceItemTypeFamilyInst (TypeFamilyInst [] TypeHeadPrefix lhs rhs)))
              _ -> reject
      where
        reject = do
          emitError (genSpan gen) (OtherError "newtype deriving requires supported associated type parameters")
          pure Nothing

-- * Generic

-- | The @Rep@ equation and the @from@ and @to@ bodies of a datatype.
--
-- The representation is the shape of the datatype spelled out as a type:
-- a balanced sum of its constructors, each a balanced product of its
-- fields, with a metadata node around the datatype, around every
-- constructor and around every field. @from@ and @to@ walk that shape.
--
-- The balance matches GHC's, so that a representation aihc derives and one
-- GHC derives are the same type rather than two spellings of one datatype.
genericItems :: Gen -> DataTypeInfo -> TcM (Maybe [InstanceDeclItem])
genericItems gen dataType =
  case stockFieldTypes plan of
    Left message -> failWith message
    Right fieldTypes -> do
      maybeRepTyCon <- genericRepTyCon gen
      let surfaceHeads = mapM (surfaceType (genSpan gen)) (tcDerivingHeadTypes plan)
          surfaceFields = mapM (mapM (surfaceType (genSpan gen))) fieldTypes
      case (maybeRepTyCon, surfaceHeads, surfaceFields) of
        (Nothing, _, _) -> failWith "stock Generic deriving requires the Rep associated type of the class"
        (_, Nothing, _) -> failWith "stock Generic deriving cannot express the datatype as source syntax"
        (_, _, Nothing) -> failWith "stock Generic deriving cannot express a constructor field as source syntax"
        (Just repTyCon, Just headTypes, Just fieldSyntax) -> do
          let constructors =
                [ (constructor, zip (dciFields constructor) fields)
                | (constructor, fields) <- zip (dtiConstructors dataType) fieldSyntax
                ]
              equation =
                InstanceItemTypeFamilyInst
                  ( TypeFamilyInst
                      []
                      TypeHeadPrefix
                      (applyTypes (TCon (tyConNameSyntax (genSpan gen) repTyCon) Unpromoted) headTypes)
                      (genericRepType gen dataType constructors)
                  )
          fromMatches <- mapM (genericFromMatch gen) (repTreePaths constructors)
          toItem <- genericToItem gen constructors
          pure (Just [equation, methodBind gen "from" fromMatches, toItem])
  where
    plan = genPlan gen
    failWith message = do
      emitError (genSpan gen) (OtherError message)
      pure Nothing

-- | The kind variables a datatype is quantified over. A poly-kinded
-- datatype has some; one whose every parameter has a concrete kind has
-- none.
datatypeKindVariables :: DataTypeInfo -> [TyVarId]
datatypeKindVariables dataType =
  concatMap (variablesIn . tvKind) (dtiTyVars dataType) <> variablesIn (dtiResultKind dataType)
  where
    variablesIn = typeTyVarsWith (\tyVar -> tyVar : variablesIn (tvKind tyVar))

-- | Every type variable a type mentions, replaced by what the selector
-- makes of it.
typeTyVarsWith :: (TyVarId -> [TyVarId]) -> TcType -> [TyVarId]
typeTyVarsWith select = go
  where
    go ty =
      case ty of
        TcTyVar tyVar -> select tyVar
        TcTyCon _ arguments -> concatMap go arguments
        TcFunTy argument result -> go argument <> go result
        TcAppTy function argument -> go function <> go argument
        TcForAllTy tyVar body -> select tyVar <> go body
        TcQualTy _ body -> go body
        _ -> []

-- | The @Rep@ family of the class being derived.
genericRepTyCon :: Gen -> TcM (Maybe TyCon)
genericRepTyCon gen = do
  info <- lookupClass (tcDerivingClassTyCon (genPlan gen))
  pure $ case info of
    Just classInfo
      | [associated] <- ciAssociatedTypes classInfo -> Just (atiTyCon associated)
    _ -> Nothing

-- | The representation type of a datatype whose constructors are paired
-- with their fields and the surface syntax of each field type.
genericRepType :: Gen -> DataTypeInfo -> [(DataConInfo, [(DataConFieldInfo, Type)])] -> Type
genericRepType gen dataType constructors =
  applyTypes (genericType gen genericD1Type) [genericMetaDataType gen dataType, sum']
  where
    -- A datatype without constructors would be V1. Stock deriving refuses
    -- one before reaching here, so the default only says what the shape is.
    sum' =
      maybe
        (genericType gen genericV1Type)
        (foldRepTree (typeOperator gen genericSumType) constructorType)
        (repTree constructors)
    constructorType (constructor, fields) =
      applyTypes (genericType gen genericC1Type) [genericMetaConsType gen constructor, product']
      where
        product' =
          maybe
            (genericType gen genericU1Type)
            (foldRepTree (typeOperator gen genericProductType) fieldType)
            (repTree fields)
    fieldType (field, fieldSurface) =
      applyTypes
        (genericType gen genericS1Type)
        [genericMetaSelType gen field, TApp (genericType gen genericRec0Type) fieldSurface]

-- | @'MetaData' isNewtype@. The datatype, module and package names that GHC
-- puts before it are type-level strings, which aihc does not have yet.
genericMetaDataType :: Gen -> DataTypeInfo -> Type
genericMetaDataType gen dataType =
  TApp (genericPromoted gen genericMetaData) (promotedBool gen (dtiFlavor dataType == NewtypeTyCon))

-- | @'MetaCons' fixity isRecord@, without the constructor name.
genericMetaConsType :: Gen -> DataConInfo -> Type
genericMetaConsType gen constructor =
  applyTypes (genericPromoted gen genericMetaCons) [fixity, promotedBool gen isRecord]
  where
    isRecord = dciSourceForm constructor == RecordDataCon
    -- No fixity declaration reaches the generator, so an infix constructor
    -- takes the default fixity, which is left associative.
    fixity =
      case dciSourceForm constructor of
        InfixDataCon -> TApp (genericPromoted gen genericInfixI) (genericPromoted gen genericLeftAssociative)
        _ -> genericPromoted gen genericPrefixI

-- | @'MetaSel' unpackedness strictness decided@, without the field label.
genericMetaSelType :: Gen -> DataConFieldInfo -> Type
genericMetaSelType gen field =
  applyTypes (genericPromoted gen genericMetaSel) [unpackedness, strictness, decided]
  where
    unpackedness =
      genericPromoted gen $ case dcfiUnpack field of
        NoFieldUnpack -> genericNoSourceUnpackedness
        UnpackField -> genericSourceUnpack
        NoUnpackField -> genericSourceNoUnpack
    strictness
      | dcfiStrict field = genericPromoted gen genericSourceStrict
      | dcfiLazy field = genericPromoted gen genericSourceLazy
      | otherwise = genericPromoted gen genericNoSourceStrictness
    -- aihc unpacks nothing, so a field marked for unpacking is only strict.
    decided
      | dcfiStrict field || dcfiUnpack field == UnpackField = genericPromoted gen genericDecidedStrict
      | otherwise = genericPromoted gen genericDecidedLazy

-- | One @from@ equation: the constructor's fields, wrapped in the metadata
-- and the sum injections that lead to its leaf of the representation.
genericFromMatch :: Gen -> ((DataConInfo, [(DataConFieldInfo, Type)]), [RepSide]) -> TcM Match
genericFromMatch gen ((constructor, _), path) = do
  fields <- fieldLocals gen "a" constructor
  let product' =
        maybe
          (genericExpr gen genericU1)
          (foldRepTree (\left right -> applyN gen (genericExpr gen genericProduct) [left, right]) fieldExpr)
          (repTree fields)
      fieldExpr field =
        applyN gen (genericExpr gen genericM1) [applyN gen (genericExpr gen genericK1) [localExpr gen field]]
      injected = foldr inject (applyN gen (genericExpr gen genericM1) [product']) path
      inject side inner = applyN gen (referenceExpr gen (sideConstructor side)) [inner]
  pure
    ( simpleMatch
        gen
        [constructorPattern gen constructor (map Just fields)]
        (applyN gen (genericExpr gen genericM1) [injected])
    )

-- | The @to@ equation: one alternative of the sum for each constructor,
-- each of which takes the fields out of the product below it.
--
-- The metadata nodes are stripped with @unM1@ rather than matched. A
-- pattern would be the shorter code, but the argument has the type family
-- @Rep (T a) x@ rather than a saturated application of the newtype, and
-- the desugarer reads the arguments of a newtype pattern's axiom off its
-- checked type.
genericToItem :: Gen -> [(DataConInfo, [(DataConFieldInfo, Type)])] -> TcM InstanceDeclItem
genericToItem gen constructors = do
  representation <- freshLocal gen "r"
  alternatives <- mapM constructorAlternative (repTreePaths constructors)
  pure
    ( methodBind
        gen
        "to"
        [ simpleMatch
            gen
            [atPattern gen (PVar representation)]
            (caseOf gen (unwrap (localExpr gen representation)) alternatives)
        ]
    )
  where
    unwrap expr = applyN gen (genericExpr gen genericUnM1) [expr]
    constructorAlternative ((constructor, _), path) = do
      node <- freshLocal gen "c"
      fields <- fieldLocals gen "a" constructor
      let fieldPattern =
            maybe
              (atPattern gen PWildcard)
              (foldRepTree pairPattern (atPattern gen . PVar))
              (repTree fields)
          pairPattern left right = atPattern gen (PCon (referenceSyntax gen (genericProduct . derivingGeneric)) [] [left, right])
          field local = applyN gen (genericExpr gen genericUnK1) [unwrap (localExpr gen local)]
          body =
            caseOf
              gen
              (unwrap (localExpr gen node))
              [(fieldPattern, applyN gen (constructorExpr gen constructor) (map field fields))]
      pure (sumPattern gen node path, body)

-- | The sum pattern that reaches one leaf of the representation, binding
-- the node there.
sumPattern :: Gen -> UnqualifiedName -> [RepSide] -> Pattern
sumPattern gen node =
  foldr (\side inner -> atPattern gen (PCon (referenceSyntax gen (sideConstructor side)) [] [inner])) (atPattern gen (PVar node))

sideConstructor :: RepSide -> (DerivingReferences -> DerivingReference)
sideConstructor side =
  case side of
    RepLeft -> genericL1 . derivingGeneric
    RepRight -> genericR1 . derivingGeneric

-- | Which half of a sum a leaf of the representation sits in.
data RepSide = RepLeft | RepRight
  deriving (Eq, Show)

-- | The balanced tree that a sum or a product of a representation takes.
data RepTree a
  = RepLeaf a
  | RepBranch (RepTree a) (RepTree a)

-- | The balanced tree of a list, or 'Nothing' when it is empty. The split
-- is GHC's, so a representation aihc derives has the shape GHC gives it.
repTree :: [a] -> Maybe (RepTree a)
repTree items = go (length items) items
  where
    go _ [] = Nothing
    go 1 (item : _) = Just (RepLeaf item)
    go count items' =
      let half = count `div` 2
          (left, right) = splitAt half items'
       in RepBranch <$> go half left <*> go (count - half) right

foldRepTree :: (b -> b -> b) -> (a -> b) -> RepTree a -> b
foldRepTree branch leaf tree =
  case tree of
    RepLeaf item -> leaf item
    RepBranch left right -> branch (foldRepTree branch leaf left) (foldRepTree branch leaf right)

-- | Each item of a list with the path to its leaf of the balanced tree.
repTreePaths :: [a] -> [(a, [RepSide])]
repTreePaths items =
  maybe [] go (repTree items)
  where
    go tree =
      case tree of
        RepLeaf item -> [(item, [])]
        RepBranch left right ->
          [(item, RepLeft : path) | (item, path) <- go left]
            <> [(item, RepRight : path) | (item, path) <- go right]

applyTypes :: Type -> [Type] -> Type
applyTypes = foldl TApp

-- | A type of @GHC.Generics@, as source syntax.
genericType :: Gen -> (GenericReferences -> DerivingReference) -> Type
genericType gen select = TCon (referenceSyntax gen (select . derivingGeneric)) Unpromoted

-- | A type operator of @GHC.Generics@, as the prefix application it is
-- built up with.
typeOperator :: Gen -> (GenericReferences -> DerivingReference) -> Type -> Type -> Type
typeOperator gen select left right = applyTypes (genericType gen select) [left, right]

-- | A promoted constructor of @GHC.Generics@, as source syntax.
genericPromoted :: Gen -> (GenericReferences -> DerivingReference) -> Type
genericPromoted gen select = TCon (referenceSyntax gen (select . derivingGeneric)) Promoted

promotedBool :: Gen -> Bool -> Type
promotedBool gen value = TCon (referenceSyntax gen (if value then derivingTrue else derivingFalse)) Promoted

genericExpr :: Gen -> (GenericReferences -> DerivingReference) -> Expr
genericExpr gen select = referenceExpr gen (select . derivingGeneric)

-- * Syntax builders

-- | A method equation group, placed at the deriving clause so diagnostics
-- and annotations of the generated code point at the clause.
methodBind :: Gen -> Text -> [Match] -> InstanceDeclItem
methodBind gen name matches =
  maybe id (InstanceItemAnn . mkAnnotation) (genSpan gen) $
    InstanceItemBind (FunctionBind (UnqualifiedName (variableNameType name) name (genSpanAnns gen)) matches)

simpleMatch :: Gen -> [Pattern] -> Expr -> Match
simpleMatch gen patterns body =
  Match
    { matchAnns = genSpanAnns gen,
      matchHeadForm = MatchHeadPrefix,
      matchPats = patterns,
      matchRhs = UnguardedRhs [] body Nothing
    }

-- | Place a generated expression at the deriving clause, so every type
-- annotation the checker attaches to it has a source position.
at :: Gen -> Expr -> Expr
at gen = maybe id (EAnn . mkAnnotation) (genSpan gen)

-- | The span annotation of generated syntax, or none when the deriving
-- clause it came from had no span of its own.
genSpanAnns :: Gen -> [Annotation]
genSpanAnns = map mkAnnotation . maybeToList . genSpan

atPattern :: Gen -> Pattern -> Pattern
atPattern gen = maybe id (PAnn . mkAnnotation) (genSpan gen)

caseOf :: Gen -> Expr -> [(Pattern, Expr)] -> Expr
caseOf gen scrutinee alternatives =
  at gen $
    ECase
      scrutinee
      [ CaseAlt {caseAltAnns = genSpanAnns gen, caseAltPattern = atPattern gen pat, caseAltRhs = UnguardedRhs [] body Nothing}
      | (pat, body) <- alternatives
      ]

lambda :: Gen -> UnqualifiedName -> Expr -> Expr
lambda gen parameter body = at gen (ELambdaPats [atPattern gen (PVar parameter)] body)

applyN :: Gen -> Expr -> [Expr] -> Expr
applyN gen = foldl (\function argument -> at gen (EApp function argument))

-- | A pattern variable for each field of a constructor.
fieldLocals :: Gen -> Text -> DataConInfo -> TcM [UnqualifiedName]
fieldLocals gen prefix constructor =
  mapM (\index -> freshLocal gen (prefix <> T.pack (show index))) [1 .. length (dciFields constructor)]

-- | A local binder that the type checker makes. The negative unique does
-- not collide with a resolver local or with other synthesized binders.
freshLocal :: Gen -> Text -> TcM UnqualifiedName
freshLocal gen text = do
  Unique key <- freshUnique
  let unique = negate (1000 + key)
  pure
    ( UnqualifiedName
        NameVarId
        text
        ( genSpanAnns gen
            <> [mkAnnotation (ResolutionAnnotation (genSpan gen) (IdentifierNamed text) ResolutionNamespaceTerm (ResolvedLocal unique (mkUnqualifiedName NameVarId text)))]
        )
    )

localExpr :: Gen -> UnqualifiedName -> Expr
localExpr gen = at gen . EVar . qualifyName Nothing

-- | A constructor pattern whose fields are variables or wildcards.
constructorPattern :: Gen -> DataConInfo -> [Maybe UnqualifiedName] -> Pattern
constructorPattern gen constructor fields =
  atPattern gen (PCon (constructorName gen constructor) [] (map (atPattern gen . maybe PWildcard PVar) fields))

constructorExpr :: Gen -> DataConInfo -> Expr
constructorExpr gen constructor = at gen (EVar (constructorName gen constructor))

-- | A resolved occurrence of a constructor.
constructorName :: Gen -> DataConInfo -> Name
constructorName gen constructor =
  resolvedName (genSpan gen) packageId moduleName' (constructorNameType text) ResolutionNamespaceTerm text
  where
    (packageId, moduleName') = dciOrigin constructor
    text = dciName constructor

-- | A resolved occurrence of a method of the class being derived.
methodExpr :: Gen -> Text -> Expr
methodExpr gen name =
  at gen $ EVar (resolvedName (genSpan gen) (PackageId packageId) moduleName' (variableNameType name) ResolutionNamespaceTerm name)
  where
    (packageId, moduleName') = genClassOrigin gen

methodApp :: Gen -> Text -> [Expr] -> Expr
methodApp gen name = applyN gen (methodExpr gen name)

referenceSyntax :: Gen -> (DerivingReferences -> DerivingReference) -> Name
referenceSyntax gen select =
  resolvedName (genSpan gen) package moduleName (referenceNameType reference) (referenceNamespace reference) name
  where
    reference = select (genReferences gen)
    (package, moduleName, name) = referenceIdentityOf gen reference

-- | The identity a reference denotes in this generation context: the
-- primitive package of the configuration, or the package the derived class
-- was found in.
referenceIdentityOf :: Gen -> DerivingReference -> (PackageId, Text, Text)
referenceIdentityOf gen =
  referenceIdentity (genPrimPackage gen) (PackageId (fst (genClassOrigin gen)))

referenceExpr :: Gen -> (DerivingReferences -> DerivingReference) -> Expr
referenceExpr gen select = at gen (EVar (referenceSyntax gen select))

referencePattern :: Gen -> (DerivingReferences -> DerivingReference) -> Pattern
referencePattern gen select = atPattern gen (PCon (referenceSyntax gen select) [] [])

-- | A boxed @Int@ literal built from a primitive literal, so the value
-- needs no numeric class.
intLiteral :: Gen -> Integer -> Expr
intLiteral gen value =
  at gen $
    EApp
      (referenceExpr gen derivingIntCon)
      ( at gen $
          EAnn
            (mkAnnotation (ResolutionAnnotation (genSpan gen) (IdentifierNamed primTypeName) ResolutionNamespaceType (ResolvedTopLevel primTypePackage primTypeModule (Name Nothing NameConId primTypeName []))))
            (EInt value TIntHash (T.pack (show value) <> "#"))
      )
  where
    (primTypePackage, primTypeModule, primTypeName) =
      referenceIdentityOf gen (derivingIntPrimType (genReferences gen))

resolvedName :: Maybe SourceSpan -> PackageId -> Text -> NameType -> ResolutionNamespace -> Text -> Name
resolvedName sp packageId moduleName' nameType namespace text =
  Name
    (Just moduleName')
    nameType
    text
    ( map mkAnnotation (maybeToList sp)
        <> [mkAnnotation (ResolutionAnnotation sp (IdentifierNamed text) namespace (ResolvedTopLevel packageId moduleName' (Name Nothing nameType text [])))]
    )

tyConNameSyntax :: Maybe SourceSpan -> TyCon -> Name
tyConNameSyntax sp tyCon =
  resolvedName sp (tyConPackageId tyCon) (tyConModuleName tyCon) (constructorNameType (tyConName tyCon)) (tyConNamespace tyCon) (tyConName tyCon)

constructorNameType :: Text -> NameType
constructorNameType text
  | isSymbolic text = NameConSym
  | otherwise = NameConId

variableNameType :: Text -> NameType
variableNameType text
  | isSymbolic text = NameVarSym
  | otherwise = NameVarId

isSymbolic :: Text -> Bool
isSymbolic text =
  case T.uncons text of
    Just (first, _) -> not (isIdentifierStart first)
    Nothing -> False
  where
    isIdentifierStart character =
      character == '_' || character `elem` ['a' .. 'z'] || character `elem` ['A' .. 'Z'] || character > '\x7f'

-- * Surface types

-- | The checked type as the source syntax that the instance checker reads
-- back, or 'Nothing' for a type without a source form.
surfaceType :: Maybe SourceSpan -> TcType -> Maybe Type
surfaceType sp ty =
  case ty of
    TcTyVar tyVar -> Just (TVar (mkUnqualifiedName NameVarId (tvName tyVar)))
    TcFunTy argument result -> TFun ArrowUnrestricted <$> surfaceType sp argument <*> surfaceType sp result
    TcAppTy function argument -> TApp <$> surfaceType sp function <*> surfaceType sp argument
    TcTyCon tyCon arguments
      | tyConNamespace tyCon == ResolutionNamespaceType ->
          foldl TApp (TCon (tyConNameSyntax sp tyCon) Unpromoted) <$> mapM (surfaceType sp) arguments
    _ -> Nothing

surfacePred :: Maybe SourceSpan -> Pred -> Maybe Type
surfacePred sp predicate =
  case predicate of
    ClassPred classTyCon arguments ->
      foldl TApp (TCon (tyConNameSyntax sp classTyCon) Unpromoted) <$> mapM (surfaceType sp) arguments
    _ -> Nothing
