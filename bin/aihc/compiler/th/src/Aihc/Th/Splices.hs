{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The expression splices of a module.
--
-- The compiler runs a splice before name resolution of the module that has
-- it. This module finds the splices, makes the source text of the module
-- that runs them, and puts their results into the module.
--
-- The splice module is a separate Haskell module. It has the imports of
-- the original module, because the body of a splice can only use imported
-- names. For each splice, it has one closure that applies
-- @Aihc.TH.Splice.runExpressionSplice@ to the body.
module Aihc.Th.Splices
  ( SpliceRequest (..),
    moduleSplices,
    replaceModuleSplices,
    spliceModuleName,
    spliceClosureName,
    spliceModuleText,
  )
where

import Aihc.Parser.Pretty ()
import Aihc.Parser.Syntax
import Control.Monad.Trans.State.Strict (evalState, execState, modify', state)
import Data.Data (Data, gmapM)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (eqT, (:~:) (Refl))
import Prettyprinter (LayoutOptions (..), PageWidth (..), hardline, indent, layoutPretty, pretty, vsep)
import Prettyprinter.Render.Text (renderStrict)

-- | What the compiler needs to run the splices of one module.
data SpliceRequest = SpliceRequest
  { -- | The name of the package that has the module.
    spliceRequestPackage :: !Text,
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
    -- | The imports of the module, without the modules of its own package.
    spliceRequestImports :: ![ImportDecl],
    -- | The body of each splice, in the order of 'moduleSplices'.
    spliceRequestBodies :: ![Expr]
  }

-- | The body of each expression splice of a module, in source order. A
-- splice in the body of a splice or in a quote is not in the result: the
-- compiler does not run it for this module.
moduleSplices :: Module -> [Expr]
moduleSplices modu = reverse (execState (traverseSplices collect modu) [])
  where
    collect body = do
      modify' (body :)
      pure (ETHSplice body)

-- | Put one expression in the position of each splice, in the order of
-- 'moduleSplices'. The expression gets parentheses, because it takes the
-- position of an atom. A splice with no expression stays.
replaceModuleSplices :: [Expr] -> Module -> Module
replaceModuleSplices results modu = evalState (traverseSplices replace modu) results
  where
    replace body =
      state $ \case
        result : rest -> (EParen result, rest)
        [] -> (ETHSplice body, [])

-- | Visit each splice that 'moduleSplices' gives, and replace it.
traverseSplices :: forall m. (Monad m) => (Expr -> m Expr) -> Module -> m Module
traverseSplices onSplice = go
  where
    go :: forall value. (Data value) => value -> m value
    go value =
      case eqT @value @Expr of
        Just Refl -> expression value
        Nothing -> gmapM go value
    expression :: Expr -> m Expr
    expression expr =
      case expr of
        ETHSplice body -> onSplice body
        ETHExpQuote {} -> pure expr
        ETHTypedQuote {} -> pure expr
        ETHDeclQuote {} -> pure expr
        ETHTypeQuote {} -> pure expr
        ETHPatQuote {} -> pure expr
        _ -> gmapM go expr

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
  renderStrict (layoutPretty (LayoutOptions (AvailablePerLine 120 1)) (vsep (pretty header : zipWith closure [0 ..] (spliceRequestBodies request)) <> hardline))
  where
    header =
      Module
        { moduleAnns = [],
          moduleHead = Just (ModuleHead [] spliceModuleName Nothing Nothing),
          moduleLanguagePragmas = spliceRequestPragmas request,
          moduleImports = spliceRequestImports request <> [runnerImport],
          moduleDecls = []
        }
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
    closure index body =
      vsep
        [ pretty (spliceClosureName index) <> " =",
          indent 2 "Aihc.TH.Splice.runExpressionSplice",
          indent 4 (pretty (parenthesized body))
        ]
    -- The body of @$(e)@ has its parentheses, and the body of @$e@ has none.
    parenthesized body
      | hasParentheses body = body
      | otherwise = EParen body
    hasParentheses expr =
      case expr of
        EAnn _ inner -> hasParentheses inner
        EParen _ -> True
        _ -> False
