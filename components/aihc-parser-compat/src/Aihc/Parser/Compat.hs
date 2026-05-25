{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Aihc.Parser.Compat
  ( toGhcHsDecl,
    toGhcHsExpr,
    toGhcHsModuleDecls,
    dumpGhcAst,
    renderGhcAst,
    sameGhcAst,
  )
where

import Aihc.Parser.Compat.Internal.Convert qualified as Convert
import Aihc.Parser.Compat.Internal.Ghc (normalizeGhcAst)
import Aihc.Parser.Syntax (Decl, Expr, Module, moduleDecls)
import Data.ByteString (ByteString)
import Data.Data (Data (..), cast, gmapQ, showConstr, toConstr)
import Data.List (isPrefixOf)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Data.FastString (FastString)
import GHC.Hs (GhcPs, HsLocalBindsLR (..), HsValBindsLR (..), LHsCmd, LHsExpr, StmtLR (..))
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Utils.Outputable (Outputable, ppr, showSDocUnsafe)
import Language.Haskell.Syntax.Basic (FieldLabelString)
import Language.Haskell.Syntax.Decls (HsDecl)
import Language.Haskell.Syntax.Expr (HsExpr)

-- | Convert an aihc-parser declaration into the matching ghc-lib-parser AST.
--
-- All locations and exact-print comments in the result are empty/default GHC
-- values.
toGhcHsDecl :: Decl -> HsDecl GhcPs
toGhcHsDecl = Convert.toGhcHsDecl

-- | Convert an aihc-parser expression into the matching ghc-lib-parser AST.
--
-- All locations and exact-print comments in the result are empty/default GHC
-- values.
toGhcHsExpr :: Expr -> HsExpr GhcPs
toGhcHsExpr = Convert.toGhcHsExpr

-- | Convert every declaration in an aihc-parser module into ghc-lib-parser AST
-- declarations.
--
-- Module headers and imports are intentionally outside the current
-- compatibility conversion surface.
toGhcHsModuleDecls :: Module -> [HsDecl GhcPs]
toGhcHsModuleDecls = map toGhcHsDecl . moduleDecls

-- | Compare GHC AST values after normalizing locations and exact-print
-- annotations.
sameGhcAst :: (Data a, Data b) => a -> b -> Bool
sameGhcAst left right =
  structuralEq (normalizeGhcAst left) (normalizeGhcAst right)

data SomeData = forall a. (Data a) => SomeData a

structuralEq :: (Data a, Data b) => a -> b -> Bool
structuralEq left right =
  primitiveEq left right
    || emptyLocalBindsEq left right
    || stmtEq left right
    || (isIgnoredAnnotationPayload left && isIgnoredAnnotationPayload right)
    || ( sameCon
           && case (conName, leftChildren, rightChildren) of
             ("L", [_, leftPayload], [_, rightPayload]) -> structuralEqSome leftPayload rightPayload
             _ -> length leftChildren == length rightChildren && and (zipWith structuralEqSome leftChildren rightChildren)
       )
  where
    conName = showConstr (toConstr left)
    sameCon = conName == showConstr (toConstr right)
    leftChildren = gmapQ SomeData left
    rightChildren = gmapQ SomeData right

structuralEqSome :: SomeData -> SomeData -> Bool
structuralEqSome (SomeData left) (SomeData right) =
  structuralEq left right

emptyLocalBindsEq :: (Data a, Data b) => a -> b -> Bool
emptyLocalBindsEq left right =
  case (cast left, cast right) of
    (Just (left' :: HsLocalBindsLR GhcPs GhcPs), Just (right' :: HsLocalBindsLR GhcPs GhcPs)) ->
      emptyLocalBindsLike left' && emptyLocalBindsLike right'
    _ -> False

stmtEq :: (Data a, Data b) => a -> b -> Bool
stmtEq left right =
  exprStmtEq || cmdStmtEq
  where
    exprStmtEq =
      case (cast left, cast right) of
        (Just (left' :: StmtLR GhcPs GhcPs (LHsExpr GhcPs)), Just (right' :: StmtLR GhcPs GhcPs (LHsExpr GhcPs))) ->
          eqStmtBodies structuralEq left' right'
        _ -> False
    cmdStmtEq =
      case (cast left, cast right) of
        (Just (left' :: StmtLR GhcPs GhcPs (LHsCmd GhcPs)), Just (right' :: StmtLR GhcPs GhcPs (LHsCmd GhcPs))) ->
          eqStmtBodies structuralEq left' right'
        _ -> False

eqStmtBodies :: (body -> body -> Bool) -> StmtLR GhcPs GhcPs body -> StmtLR GhcPs GhcPs body -> Bool
eqStmtBodies eqBody left right =
  case (left, right) of
    (LastStmt _ bodyA _ _, LastStmt _ bodyB _ _) -> eqBody bodyA bodyB
    (LastStmt _ bodyA _ _, BodyStmt _ bodyB _ _) -> eqBody bodyA bodyB
    (BodyStmt _ bodyA _ _, LastStmt _ bodyB _ _) -> eqBody bodyA bodyB
    (BodyStmt _ bodyA _ _, BodyStmt _ bodyB _ _) -> eqBody bodyA bodyB
    _ -> False

emptyLocalBindsLike :: HsLocalBindsLR GhcPs GhcPs -> Bool
emptyLocalBindsLike binds =
  case binds of
    EmptyLocalBinds _ -> True
    HsValBinds _ (ValBinds _ [] []) -> True
    _ -> False

isIgnoredAnnotationPayload :: (Data a) => a -> Bool
isIgnoredAnnotationPayload value =
  case (showConstr (toConstr value), gmapQ SomeData value) of
    (name, _) | isIgnoredAnnotationCon name -> True
    ("Nothing", []) -> True
    ("Just", [payload]) -> isIgnoredAnnotationSome payload
    ("[]", []) -> True
    (":", [headPayload, tailPayload]) -> isIgnoredAnnotationSome headPayload && isIgnoredAnnotationSome tailPayload
    ("(,)", payloads) -> all isIgnoredAnnotationSome payloads
    _ -> False

isIgnoredAnnotationCon :: String -> Bool
isIgnoredAnnotationCon name =
  any (`isPrefixOf` name) ["Ann", "Ep", "Epa", "NameAnn", "NoEp"]
    || name
      `elem` [ "Anchor",
               "NameSquare",
               "NameParens",
               "NameBackquotes",
               "NoComments",
               "NormalSyntax",
               "UnicodeSyntax",
               "NoSourceText",
               "SourceText"
             ]

isIgnoredAnnotationSome :: SomeData -> Bool
isIgnoredAnnotationSome (SomeData value) =
  isIgnoredAnnotationPayload value

primitiveEq :: (Data a, Data b) => a -> b -> Bool
primitiveEq left right =
  primRdrName
    || prim (Proxy @Bool)
    || prim (Proxy @Char)
    || prim (Proxy @Int)
    || prim (Proxy @Integer)
    || prim (Proxy @Rational)
    || prim (Proxy @String)
    || prim (Proxy @Text)
    || prim (Proxy @ByteString)
    || prim (Proxy @FastString)
    || prim (Proxy @FieldLabelString)
  where
    primRdrName =
      case (cast left, cast right) of
        (Just (left' :: RdrName), Just (right' :: RdrName)) -> rdrNameOcc left' == rdrNameOcc right'
        _ -> False

    prim :: forall c. (Eq c, Data c) => Proxy c -> Bool
    prim _ =
      case (cast left, cast right) of
        (Just (left' :: c), Just (right' :: c)) -> left' == right'
        _ -> False

-- | Render a normalized comparison tree for diagnostics.
dumpGhcAst :: (Data a) => a -> String
dumpGhcAst = unlines . dumpLines 0 . normalizeGhcAst

dumpLines :: (Data a) => Int -> a -> [String]
dumpLines depth value =
  let conName = showConstr (toConstr value)
      children = gmapQ SomeData value
   in if isIgnoredDumpPayload value
        then []
        else case (primitiveValue value, conName, children) of
          (Just leaf, _, _) -> [indent depth <> leaf]
          (_, "L", [_, payload]) -> dumpSome depth payload
          _ ->
            let childLines = concatMap (dumpSome (depth + 1)) children
             in if null childLines
                  then [indent depth <> conName]
                  else (indent depth <> conName) : childLines

dumpSome :: Int -> SomeData -> [String]
dumpSome depth (SomeData value) =
  dumpLines depth value

isIgnoredDumpPayload :: (Data a) => a -> Bool
isIgnoredDumpPayload value =
  isIgnoredAnnotationPayload value
    || showConstr (toConstr value)
      `elem` [ "NoExtField",
               "NoExtCon",
               "NoAnnSortKey"
             ]

primitiveValue :: (Data a) => a -> Maybe String
primitiveValue value =
  prim (Proxy @Bool)
    <> prim (Proxy @Char)
    <> prim (Proxy @Int)
    <> prim (Proxy @Integer)
    <> prim (Proxy @Rational)
    <> prim (Proxy @String)
    <> prim (Proxy @Text)
    <> prim (Proxy @ByteString)
    <> prim (Proxy @FastString)
    <> primRdrName
  where
    primRdrName =
      case cast value of
        Just (value' :: RdrName) -> Just (renderGhcAst (rdrNameOcc value'))
        Nothing -> Nothing

    prim :: forall c. (Show c, Data c) => Proxy c -> Maybe String
    prim _ =
      case cast value of
        Just (value' :: c) -> Just (show value')
        Nothing -> Nothing

indent :: Int -> String
indent depth = replicate (depth * 2) ' '

-- | Render a GHC AST value with GHC's pretty-printer.
renderGhcAst :: (Outputable a) => a -> String
renderGhcAst = showSDocUnsafe . ppr
