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
    replaceModuleSplices,
    replaceFirstSplice,
    firstSplicePosition,
    FactsAnswer,
    LocalFacts,
    RunSplice,
    spliceModuleName,
    splicePackageName,
    splicePackageVersion,
    splicePackageId,
    spliceClosureName,
    spliceModuleText,
  )
where

import Aihc.Parser.Pretty ()
import Aihc.Parser.Syntax
import Aihc.Resolve.Types (GlobalName)
import Aihc.Tc.Interface (TcInterface)
import Aihc.Th.Quote (quoteImports)
import Aihc.Th.Wire (Tree)
import Control.Monad.Trans.State.Strict (evalState, execState, modify', state)
import Data.Char (isAlphaNum, toLower)
import Data.Data (Data, gmapM)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (eqT, (:~:) (Refl))
import Prettyprinter (LayoutOptions (..), PageWidth (..), hardline, indent, layoutPretty, pretty, vsep)
import Prettyprinter.Render.Text (renderStrict)

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
    -- | The modules of its own package that the module imports, and that
    -- the splice package cannot have: modules of the same dependency cycle.
    -- The splice module does not import them.
    spliceRequestUnavailableImports :: ![Text],
    -- | Each splice, in the order of 'moduleSplices'.
    spliceRequestSplices :: ![Splice]
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
  | -- | A splice in the position of a declaration. Its body gives a list
    -- of declarations.
    DeclarationSplice Expr
  deriving (Eq, Show)

-- | What the compiler puts in the position of a splice.
data SpliceResult
  = ExpressionResult Expr
  | TypeResult Type
  | DeclarationResult [Decl]
  deriving (Eq, Show)

-- | Each splice of a module, in source order. A splice in the body of a
-- splice or in a quote is not in the result: the compiler does not run it
-- for this module.
moduleSplices :: Module -> [Splice]
moduleSplices modu = reverse (execState (traverseSplices expression typeSplice declaration modu) [])
  where
    expression body = do
      modify' (ExpressionSplice body :)
      pure (ETHSplice body)
    typeSplice body = do
      modify' (TypeSplice body :)
      pure (TSplice body)
    declaration decl body = do
      modify' (DeclarationSplice body :)
      pure [decl]

-- | Put one result in the position of each splice, in the order of
-- 'moduleSplices'. An expression gets parentheses, because it takes the
-- position of an atom. A type also gets parentheses. The declarations of a declaration splice take its
-- position in the list of declarations. A splice with no result of its
-- kind stays.
replaceModuleSplices :: [SpliceResult] -> Module -> Module
replaceModuleSplices results modu = evalState (traverseSplices expression typeSplice declaration modu) results
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

-- | Run one splice of a request: its position in 'spliceRequestSplices',
-- with the local facts for that splice.
type RunSplice = Int -> LocalFacts -> IO SpliceResult

-- | Visit each splice that 'moduleSplices' gives, and replace it. The
-- function for a type splice gets the body of the splice. The
-- function for a declaration splice gets the declaration and its body, and
-- it gives the declarations that take the position of the splice.
traverseSplices :: forall m. (Monad m) => (Expr -> m Expr) -> (Expr -> m Type) -> (Decl -> Expr -> m [Decl]) -> Module -> m Module
traverseSplices onExpression onType onDeclaration = go
  where
    go :: forall value. (Data value) => value -> m value
    go value =
      case eqT @value @Expr of
        Just Refl -> expression value
        Nothing ->
          case eqT @value @Type of
            Just Refl -> typ value
            Nothing ->
              case eqT @value @[Decl] of
                Just Refl -> concat <$> mapM declaration value
                Nothing -> gmapM go value
    expression :: Expr -> m Expr
    expression expr =
      case expr of
        ETHSplice body -> onExpression body
        ETHExpQuote {} -> pure expr
        ETHTypedQuote {} -> pure expr
        ETHDeclQuote {} -> pure expr
        ETHTypeQuote {} -> pure expr
        ETHPatQuote {} -> pure expr
        _ -> gmapM go expr
    typ :: Type -> m Type
    typ ty =
      case ty of
        TSplice body -> onType body
        _ -> gmapM go ty
    declaration :: Decl -> m [Decl]
    declaration decl =
      case declarationSpliceBody decl of
        Just body -> onDeclaration decl body
        Nothing -> pure <$> gmapM go decl
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
        ETHSplice inner -> inner
        _ -> body
    explicitSplice expr =
      case expr of
        EAnn _ inner -> explicitSplice inner
        _ -> expr

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
