{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The splices of a module: the splices in expressions, the splices in
-- types, and the splices in the position of a declaration.
--
-- The compiler runs a splice before name resolution of the module that has
-- it. This module finds the splices, makes the source text of the module
-- that runs them, and puts their results into the module.
--
-- The splice module is a separate Haskell module. It has the imports of
-- the original module, because the body of a splice can only use imported
-- names. For each splice, it has one closure that applies
-- @Aihc.TH.Splice.runExpressionSplice@,
-- @Aihc.TH.Splice.runTypeSplice@, or
-- @Aihc.TH.Splice.runDeclarationsSplice@ to the body.
--
-- The compiler runs the splices one at a time, in source order. Each top-level
-- declaration splice ends a declaration group. A splice can get the facts of
-- the declaration groups above it with @reify@. A top-level declaration
-- splice also gets the facts of the declarations directly above it.
module Aihc.Th.Splices
  ( SpliceRequest (..),
    PackageFile (..),
    Splice (..),
    SpliceResult (..),
    moduleSplices,
    moduleSpliceSpans,
    replaceModuleSplices,
    replaceFirstSplice,
    firstSplicePosition,
    FactsAnswer,
    LocalFacts,
    LocalFixity,
    NameLookup,
    RunSplice,
    spliceModuleName,
    splicePackageName,
    splicePackageVersion,
    splicePackageId,
    spliceResultsPath,
    spliceClosureName,
    spliceModuleText,
  )
where

import Aihc.Parser.Pretty ()
import Aihc.Parser.Syntax
import Aihc.Resolve (OperatorFixity)
import Aihc.Resolve.Types (GlobalName, ResolutionNamespace)
import Aihc.Tc.Interface (TcInterface)
import Aihc.Th.Quote (quoteImports)
import Aihc.Th.Wire (Tree)
import Control.Applicative ((<|>))
import Control.Monad.Trans.State.Strict (evalState, execState, modify', state)
import Data.Char (isAlphaNum, toLower)
import Data.Data (Data, gmapM)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (eqT, (:~:) (Refl))
import Prettyprinter (LayoutOptions (..), PageWidth (..), hardline, indent, layoutPretty, pretty, vsep)
import Prettyprinter.Render.Text (renderStrict)
import System.FilePath ((</>))

-- | What the compiler needs to run the splices of one module.
data SpliceRequest = SpliceRequest
  { -- | The name of the package that has the module.
    spliceRequestPackage :: !Text,
    -- | The package id of the names of the package, as @name-version@.
    spliceRequestPackageId :: !Text,
    spliceRequestModule :: !Text,
    -- | The store without the directory of a target.
    spliceRequestStoreRoot :: !FilePath,
    -- | A directory for the files of this module.
    spliceRequestBuildRoot :: !FilePath,
    -- | The directory of the Cabal file of the package. A splice can read
    -- the files in it, and a relative path in a splice starts at it, as
    -- with GHC.
    spliceRequestPackageRoot :: !FilePath,
    -- | The name and the version of each package that the module can
    -- import, apart from its own package.
    spliceRequestDependencies :: ![(Text, [Int])],
    -- | The default extensions and the default language of the component.
    spliceRequestExtensions :: ![String],
    spliceRequestLanguage :: !(Maybe String),
    spliceRequestPragmas :: ![ExtensionSetting],
    -- | The imports of the module. An import of a module of its own
    -- package is only in the list when that module is in
    -- 'spliceRequestOwnModules'.
    spliceRequestImports :: ![ImportDecl],
    -- | Each source file of the package, by its path.
    spliceRequestPackageFiles :: !(Map FilePath PackageFile),
    -- | The name and the source file of each module of the package that the
    -- module imports, directly or through a different module of the
    -- package. The splice package compiles these modules again.
    spliceRequestOwnModules :: ![(Text, FilePath)],
    -- | The file with the splice results of each module of
    -- 'spliceRequestOwnModules' that has splices. The build of the package
    -- writes this file when it runs the splices of that module. The splice
    -- package then uses these results again for its copy of the module.
    -- Thus, it does not make a splice package of its own for the copy.
    spliceRequestOwnResults :: ![(Text, FilePath)],
    -- | The modules of its own package that the module imports, and that
    -- the splice package cannot have: modules of the same dependency cycle.
    -- The splice module does not import them.
    spliceRequestUnavailableImports :: ![Text],
    -- | Each splice, in the order of 'moduleSplices'.
    spliceRequestSplices :: ![Splice],
    -- | The path of the source file of the module, relative to the root
    -- directory of the package, as GHC gives it to @location@.
    spliceRequestSourceFile :: !FilePath,
    -- | The source span of each splice, in the order of 'moduleSplices'.
    spliceRequestSpans :: ![Maybe SourceSpan]
  }

-- | The settings of the component of a source file of a package.
data PackageFile = PackageFile
  { packageFileExtensions :: ![String],
    packageFileCppOptions :: ![String],
    packageFileIncludeDirs :: ![FilePath],
    -- | The name and the version of each package that the file can import,
    -- apart from its own package.
    packageFileDependencies :: ![(Text, [Int])]
  }
  deriving (Eq, Show)

-- | A splice of a module, with its body.
data Splice
  = -- | A splice in an expression. Its body gives one expression.
    ExpressionSplice Expr
  | -- | A splice in a type. Its body gives one type.
    TypeSplice Expr
  | -- | A splice in a pattern. Its body gives one pattern.
    PatternSplice Expr
  | -- | A splice in the position of a declaration. Its body gives a list
    -- of declarations.
    DeclarationSplice Expr
  deriving (Eq, Show)

-- | What the compiler puts in the position of a splice.
data SpliceResult
  = ExpressionResult Expr
  | TypeResult Type
  | PatternResult Pattern
  | DeclarationResult [Decl]
  deriving (Eq, Show)

-- | Each splice of a module, in source order. A splice in the body of a
-- splice or in a quote is not in the result: the compiler does not run it
-- for this module.
--
-- A quasi-quote is also a splice. The body of the splice of @[q|text|]@
-- applies a field of the quasi-quoter @q@ to the text: 'quasiQuoteBody'.
moduleSplices :: Module -> [Splice]
moduleSplices modu = reverse (execState (traverseSplices expression typeSplice patternSplice declaration modu) [])
  where
    expression body = do
      modify' (ExpressionSplice body :)
      pure (ETHSplice body)
    typeSplice body = do
      modify' (TypeSplice body :)
      pure (TSplice body)
    patternSplice body = do
      modify' (PatternSplice body :)
      pure (PSplice body)
    declaration decl body = do
      modify' (DeclarationSplice body :)
      pure [decl]

-- | Put one result in the position of each splice, in the order of
-- 'moduleSplices'. An expression gets parentheses, because it takes the
-- position of an atom. A type also gets parentheses. The declarations of a declaration splice take its
-- position in the list of declarations. A splice with no result of its
-- kind stays.
replaceModuleSplices :: [SpliceResult] -> Module -> Module
replaceModuleSplices results modu = evalState (traverseSplices expression typeSplice patternSplice declaration modu) results
  where
    expression body =
      state $ \case
        ExpressionResult result : rest -> (EParen result, rest)
        _ : rest -> (ETHSplice body, rest)
        [] -> (ETHSplice body, [])
    typeSplice body =
      state $ \case
        TypeResult result : rest -> (TParen result, rest)
        _ : rest -> (TSplice body, rest)
        [] -> (TSplice body, [])
    patternSplice body =
      state $ \case
        PatternResult result : rest -> (PParen result, rest)
        _ : rest -> (PSplice body, rest)
        [] -> (PSplice body, [])
    declaration decl _ =
      state $ \case
        DeclarationResult decls : rest -> (decls, rest)
        _ : rest -> ([decl], rest)
        [] -> ([decl], [])

-- | Put the result of the first splice of 'moduleSplices' in its position.
-- The other splices stay.
replaceFirstSplice :: SpliceResult -> Module -> Module
replaceFirstSplice result = replaceModuleSplices [result]

-- | The position of the first top-level declaration that has a splice, and
-- whether that declaration is a declaration splice itself.
firstSplicePosition :: Module -> Maybe (Int, Bool)
firstSplicePosition modu =
  case [(index, isDeclarationSplice decl) | (index, decl) <- zip [0 ..] (moduleDecls modu), not (null (moduleSplices modu {moduleDecls = [decl]}))] of
    first : _ -> Just first
    [] -> Nothing
  where
    isDeclarationSplice decl =
      case decl of
        DeclAnn _ inner -> isDeclarationSplice inner
        DeclSplice _ -> True
        _ -> False

-- | How the compiler answers a question about a name from the type facts
-- of the module that declares it, such as @reify@ or @reifyRoles@.
type FactsAnswer = TcInterface -> GlobalName -> Either Text Tree

-- | The facts that the compiler gives for a question about a name of the
-- module of a splice, or of a module of its own package. 'Nothing' sends
-- the question to the facts of the store.
type LocalFacts = FactsAnswer -> GlobalName -> IO (Maybe (Either Text Tree))

-- | The top-level entity of a name in the scope of a splice, in a
-- namespace. The name has no package, as a name from @mkName@.
type NameLookup = ResolutionNamespace -> Name -> Maybe GlobalName

-- | The fixity that a declaration gives to a name of a module that the
-- module of a splice can see. The inner 'Nothing' is a name with no fixity
-- declaration. The outer 'Nothing' sends the question to the facts of the
-- store.
type LocalFixity = GlobalName -> IO (Maybe (Maybe OperatorFixity))

-- | Run one splice of a request: its position in 'spliceRequestSplices',
-- with the scope, the local facts, and the local fixities for that splice.
type RunSplice = Int -> NameLookup -> LocalFacts -> LocalFixity -> IO SpliceResult

-- | Visit each splice that 'moduleSplices' gives, and replace it. The
-- function for a type splice or a pattern splice gets the body of the
-- splice. The function for a declaration splice gets the declaration and
-- its body, and it gives the declarations that take the position of the
-- splice.
traverseSplices :: forall m. (Monad m) => (Expr -> m Expr) -> (Expr -> m Type) -> (Expr -> m Pattern) -> (Decl -> Expr -> m [Decl]) -> Module -> m Module
traverseSplices onExpression onType onPattern onDeclaration =
  traverseSplicesAt (const onExpression) (const onType) (const onPattern) (const onDeclaration)

-- | The source span of each splice of 'moduleSplices', in the same order.
-- The span is the span of the syntax node of the splice, or of the
-- nearest node around it that has a span.
moduleSpliceSpans :: Module -> [Maybe SourceSpan]
moduleSpliceSpans modu = reverse (execState (traverseSplicesAt expression typeSplice patternSplice declaration modu) [])
  where
    expression here body = do
      modify' (here :)
      pure (ETHSplice body)
    typeSplice here body = do
      modify' (here :)
      pure (TSplice body)
    patternSplice here body = do
      modify' (here :)
      pure (PSplice body)
    declaration here decl _ = do
      modify' (here :)
      pure [decl]

-- | 'traverseSplices', and each function also gets the source span of the
-- splice.
traverseSplicesAt :: forall m. (Monad m) => (Maybe SourceSpan -> Expr -> m Expr) -> (Maybe SourceSpan -> Expr -> m Type) -> (Maybe SourceSpan -> Expr -> m Pattern) -> (Maybe SourceSpan -> Decl -> Expr -> m [Decl]) -> Module -> m Module
traverseSplicesAt onExpression onType onPattern onDeclaration = go Nothing
  where
    go :: forall value. (Data value) => Maybe SourceSpan -> value -> m value
    go here value =
      case eqT @value @Expr of
        Just Refl -> expression here value
        Nothing ->
          case eqT @value @Type of
            Just Refl -> typ here value
            Nothing ->
              case eqT @value @Pattern of
                Just Refl -> pattern' here value
                Nothing ->
                  case eqT @value @[Decl] of
                    Just Refl -> concat <$> mapM (declaration here) value
                    Nothing -> gmapM (go here) value
    expression :: Maybe SourceSpan -> Expr -> m Expr
    expression here expr =
      case expr of
        EAnn annotation inner -> EAnn annotation <$> expression (annotationSpan here annotation) inner
        ETHSplice body -> onExpression here body
        ETHTypedSplice body -> onExpression here (typedSpliceBody body)
        EQuasiQuote quoter text -> onExpression here (quasiQuoteBody QuoteExpression quoter text)
        ETHExpQuote {} -> pure expr
        ETHTypedQuote {} -> pure expr
        ETHDeclQuote {} -> pure expr
        ETHTypeQuote {} -> pure expr
        ETHPatQuote {} -> pure expr
        _ -> gmapM (go here) expr
    typ :: Maybe SourceSpan -> Type -> m Type
    typ here ty =
      case ty of
        TAnn annotation inner -> TAnn annotation <$> typ (annotationSpan here annotation) inner
        TSplice body -> onType here body
        TQuasiQuote quoter text -> onType here (quasiQuoteBody QuoteType quoter text)
        _ -> gmapM (go here) ty
    pattern' :: Maybe SourceSpan -> Pattern -> m Pattern
    pattern' here pat =
      case pat of
        PAnn annotation inner -> PAnn annotation <$> pattern' (annotationSpan here annotation) inner
        PSplice body -> onPattern here body
        PQuasiQuote quoter text -> onPattern here (quasiQuoteBody QuotePattern quoter text)
        _ -> gmapM (go here) pat
    declaration :: Maybe SourceSpan -> Decl -> m [Decl]
    declaration here decl =
      let here' = declarationSpan here decl
       in case declarationSpliceBody decl of
            Just body -> onDeclaration here' decl body
            Nothing -> pure <$> gmapM (go here') decl
    -- The span of the outermost annotation of a declaration.
    declarationSpan here decl =
      case decl of
        DeclAnn annotation _ -> annotationSpan here annotation
        _ -> here
    annotationSpan here annotation = fromAnnotation annotation <|> here
    declarationSpliceBody decl =
      case decl of
        DeclAnn _ inner -> declarationSpliceBody inner
        DeclSplice body -> Just (explicitSpliceBody body)
        _ -> Nothing
    -- A declaration splice with the splice syntax, @$(e)@, has the
    -- expression splice as its expression. A declaration splice without
    -- that syntax has @e@ itself.
    explicitSpliceBody body =
      case body of
        EAnn _ inner
          | ETHSplice _ <- explicitSplice inner -> explicitSpliceBody inner
          | EQuasiQuote {} <- explicitSplice inner -> explicitSpliceBody inner
        ETHSplice inner -> inner
        EQuasiQuote quoter text -> quasiQuoteBody QuoteDeclarations quoter text
        _ -> body
    explicitSplice expr =
      case expr of
        EAnn _ inner -> explicitSplice inner
        _ -> expr

-- | The body of the splice of a typed splice @$$e@: @unTypeCode e@. GHC
-- gives @e@ the type @Code m t@. The splice runs the code and puts its
-- syntax tree in the position of the splice. The type checker then checks
-- the result in that position. The splice module imports
-- @Aihc.TH.Splice@, which exports 'unTypeCode'.
typedSpliceBody :: Expr -> Expr
typedSpliceBody body =
  EApp (EVar (mkName (Just "Aihc.TH.Splice") NameVarId "unTypeCode")) (EParen body)

-- | The field of a quasi-quoter that a quasi-quote uses. The field
-- depends on the position of the quasi-quote.
data QuoteField
  = QuoteExpression
  | QuotePattern
  | QuoteType
  | QuoteDeclarations

-- | The body of the splice of a quasi-quote: the field of the quasi-quoter,
-- applied to the quasi-quoter and to the text. The splice module imports
-- @Aihc.TH.Splice@, which exports the fields.
quasiQuoteBody :: QuoteField -> Text -> Text -> Expr
quasiQuoteBody field quoter text =
  EApp (EApp (EVar (mkName (Just "Aihc.TH.Splice") NameVarId fieldName)) (EVar quoterName)) (EString text (T.pack (show (T.unpack text))))
  where
    fieldName =
      case field of
        QuoteExpression -> "quoteExp"
        QuotePattern -> "quotePat"
        QuoteType -> "quoteType"
        QuoteDeclarations -> "quoteDec"
    -- The name of the quasi-quoter can have a module qualifier.
    quoterName =
      case T.breakOnEnd "." quoter of
        ("", name) -> mkName Nothing NameVarId name
        (qualifier, name) -> mkName (Just (T.dropEnd 1 qualifier)) NameVarId name

-- | The name of the splice package. Each module has its own package, so
-- the symbols of two splice modules in one image are different.
splicePackageName :: SpliceRequest -> Text
splicePackageName request =
  T.intercalate "-" ("aihc-splice" : map part [spliceRequestPackage request, spliceRequestModule request])
  where
    -- A package name has letters, digits, and hyphens.
    part = T.map (\char -> if isAlphaNum char then toLower char else '-')

splicePackageVersion :: Text
splicePackageVersion = "0"

-- | The package id of the names of the splice package, as
-- @name-version@. The names of the copied modules of the original package
-- have this id in the splice package.
splicePackageId :: SpliceRequest -> Text
splicePackageId request = splicePackageName request <> "-" <> splicePackageVersion

-- | The file with the results of the splices of the module of a request.
spliceResultsPath :: SpliceRequest -> FilePath
spliceResultsPath request = spliceRequestBuildRoot request </> "results.tree"

-- | The name of the splice module. The package of the splice module is
-- different for each original module, so one name is sufficient.
spliceModuleName :: Text
spliceModuleName = "AihcSplice"

-- | The name of the closure that runs a splice, from its position in
-- 'moduleSplices'.
spliceClosureName :: Int -> Text
spliceClosureName index = "aihcSplice" <> T.pack (show index)

-- | The source text of the splice module of a request.
spliceModuleText :: SpliceRequest -> Text
spliceModuleText request =
  renderStrict (layoutPretty (LayoutOptions (AvailablePerLine 120 1)) (vsep (pretty header : zipWith closure [0 ..] (spliceRequestSplices request)) <> hardline))
  where
    header =
      Module
        { moduleAnns = [],
          moduleHead = Just (ModuleHead [] spliceModuleName Nothing Nothing),
          moduleLanguagePragmas = spliceRequestPragmas request,
          -- A name quote in a splice body can be a call of the quote
          -- library. The splice package always depends on
          -- @template-haskell@, so the module always has the imports of the
          -- library.
          moduleImports = spliceRequestImports request <> missingQuoteImports <> [runnerImport],
          moduleDecls = []
        }
    missingQuoteImports =
      [ quoteImport
      | quoteImport <- quoteImports,
        (importDeclModule quoteImport, importDeclAs quoteImport) `notElem` [(importDeclModule existing, importDeclAs existing) | existing <- spliceRequestImports request]
      ]
    runnerImport =
      ImportDecl
        { importDeclAnns = [],
          importDeclLevel = Nothing,
          importDeclPackage = Nothing,
          importDeclSourcePragma = Nothing,
          importDeclSafe = False,
          importDeclQualified = True,
          importDeclQualifiedPost = False,
          importDeclModule = "Aihc.TH.Splice",
          importDeclAs = Nothing,
          importDeclSpec = Nothing
        }
    closure index splice =
      vsep
        [ pretty (spliceClosureName index) <> " =",
          indent 2 ("Aihc.TH.Splice." <> runner),
          indent 4 (pretty (parenthesized body))
        ]
      where
        (runner, body) =
          case splice of
            ExpressionSplice expr -> ("runExpressionSplice", expr)
            TypeSplice expr -> ("runTypeSplice", expr)
            PatternSplice expr -> ("runPatternSplice", expr)
            DeclarationSplice expr -> ("runDeclarationsSplice", expr)
    -- The body of @$(e)@ has its parentheses, and the body of @$e@ has none.
    parenthesized body
      | hasParentheses body = body
      | otherwise = EParen body
    hasParentheses expr =
      case expr of
        EAnn _ inner -> hasParentheses inner
        EParen _ -> True
        _ -> False
