{-# LANGUAGE OverloadedStrings #-}

-- | Prepare the splices of a module for its splice module, and resolve a
-- unit that has quotes.
--
-- The command line and the playground helper use the same steps:
--
-- 1. Bind the name quotes of the splice bodies to the names in the scope
--    of the module.
-- 2. Resolve the splice bodies in the scope of the module, and change
--    their quotes into calls of the quote library.
-- 3. Keep only the imports that the splice bodies need.
module Aihc.Th.Prepare
  ( PreparedSplices (..),
    prepareSplices,
    resolveUnitQuotes,
  )
where

import Aihc.Parser.Syntax (Expr, ImportDecl (..), Module (..), SourceSpan)
import Aihc.Resolve
  ( Builtins,
    ModuleExports,
    ModuleUnit (..),
    ResolveFailure,
    ResolvedModule (..),
    ResolvedUnit (..),
    moduleTopLevelName,
    resolveSpliceBodies,
    resolveUnit,
    spliceBodyImports,
  )
import Aihc.Resolve.Types (GlobalName (..), PackageId (..))
import Aihc.Th.Quote (bindSpliceNameQuotes, desugarModuleQuotes, desugarSpliceBody, localBinderNames, moduleHasQuote, quoteImports, renameQuotedPackage)
import Aihc.Th.Splices (Splice (..), moduleSpliceSpans, moduleSplices)
import Data.Text (Text)

-- | The splices of a module, ready for its splice module.
data PreparedSplices = PreparedSplices
  { -- | The module with the name quotes of its splice bodies bound. The
    -- splices of this module are in the order of the other fields.
    preparedModule :: !Module,
    -- | Each splice, with its quotes changed into calls of the quote
    -- library.
    preparedSplices :: ![Splice],
    -- | The source span of each splice.
    preparedSpans :: ![Maybe SourceSpan],
    -- | The imports of the module that the splice bodies need.
    preparedImports :: ![ImportDecl]
  }

-- | Prepare the splices of a module unit. The arguments are the scope of
-- the unit, the package id of the module, the package id of its splice
-- package, and the imports that the splice module can have.
--
-- A name quote in a splice body gets its package and its module from the
-- scope of the module. A name of the package of the module gets the
-- package id of the splice package. The names of the copies of the
-- modules of the package and the facts that @reify@ gives in the splice
-- have the same id.
prepareSplices :: Builtins -> ModuleExports -> ModuleUnit -> PackageId -> Text -> [ImportDecl] -> PreparedSplices
prepareSplices builtinScope exports unit ownPackage splicePackage imports =
  PreparedSplices
    { preparedModule = bound,
      preparedSplices = desugared,
      preparedSpans = moduleSpliceSpans bound,
      -- The splice module gets only the imports that the splice bodies
      -- need. Thus, the splice package does not compile a package that only
      -- the other code of the module uses.
      preparedImports = spliceBodyImports builtinScope exports boundUnit (map spliceBody desugared) imports
    }
  where
    original = moduleUnitAst unit
    inSplicePackage global
      | globalNamePackage global == ownPackage = global {globalNamePackage = PackageId splicePackage}
      | otherwise = global
    localNames = localBinderNames original
    bound = bindSpliceNameQuotes localNames (\namespace -> fmap inSplicePackage . moduleTopLevelName builtinScope exports unit namespace) original
    boundUnit = unit {moduleUnitAst = bound}
    splices = moduleSplices bound
    -- A quote in a splice body names what it names at the splice: the
    -- compiler resolves the bodies in the scope of the module, and changes
    -- their quotes before the splice module gets them.
    resolvedBodies = resolveSpliceBodies builtinScope exports boundUnit (map spliceBody splices)
    desugared =
      zipWith
        (\splice resolved -> withBody splice (renameQuotedPackage (packageIdText ownPackage) splicePackage (desugarSpliceBody localNames (spliceBody splice) resolved)))
        splices
        resolvedBodies

spliceBody :: Splice -> Expr
spliceBody splice =
  case splice of
    ExpressionSplice body -> body
    DeclarationSplice body -> body
    TypeSplice body -> body
    PatternSplice body -> body

withBody :: Splice -> Expr -> Splice
withBody splice body =
  case splice of
    ExpressionSplice _ -> ExpressionSplice body
    DeclarationSplice _ -> DeclarationSplice body
    TypeSplice _ -> TypeSplice body
    PatternSplice _ -> PatternSplice body

-- | Resolve a unit, and change the expression quotes of its modules. A
-- quote needs the result of name resolution, and its new expression has
-- names that are not resolved. Thus, a unit with a quote resolves two
-- times. The second time starts from the parsed modules, with the new
-- expressions in the positions of the quotes.
resolveUnitQuotes :: Builtins -> ModuleExports -> [ModuleUnit] -> Either ResolveFailure ResolvedUnit
resolveUnitQuotes builtinScope exports modules =
  case resolveUnit builtinScope exports modules of
    Right (ResolvedUnit resolved)
      | any (hasQuoteImports . moduleUnitAst) modules ->
          resolveUnit builtinScope exports [parsed {moduleUnitAst = desugarModuleQuotes (moduleUnitAst parsed) (resolvedModuleSyntax modu)} | (parsed, modu) <- zip modules resolved]
    result -> result
  where
    hasQuoteImports modu = any ((`elem` map importDeclModule quoteImports) . importDeclModule) (moduleImports modu) && moduleHasQuote modu
