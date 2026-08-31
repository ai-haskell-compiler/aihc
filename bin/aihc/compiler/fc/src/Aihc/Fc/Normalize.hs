-- | Apply small System FC normalization rules.
module Aihc.Fc.Normalize
  ( normalizeProgram,
  )
where

import Aihc.Fc.Syntax

normalizeProgram :: Program -> Program
normalizeProgram program =
  program {programDecls = map normalizeDecl (programDecls program)}

normalizeDecl :: Decl -> Decl
normalizeDecl decl =
  case decl of
    DeclVal declaration ->
      DeclVal declaration {valBody = normalizeExpr (valBody declaration)}
    _ -> decl

normalizeExpr :: Expr -> Expr
normalizeExpr expr =
  case expr of
    ExApp function argument ->
      normalizeApp (normalizeExpr function) (normalizeExpr argument)
    ExTyApp function argument -> ExTyApp (normalizeExpr function) argument
    ExLam binder body -> ExLam binder (normalizeExpr body)
    ExTyLam binder body -> ExTyLam binder (normalizeExpr body)
    ExLet bind body ->
      ExLet
        bind {bindRhs = normalizeExpr (bindRhs bind)}
        (normalizeExpr body)
    ExRec binds body ->
      ExRec
        (map normalizeBind binds)
        (normalizeExpr body)
    ExCase scrutinee binder resultType alternatives ->
      ExCase
        (normalizeExpr scrutinee)
        binder
        resultType
        (map normalizeAlt alternatives)
    ExCast body coercion -> ExCast (normalizeExpr body) coercion
    _ -> expr

normalizeApp :: Expr -> Expr -> Expr
normalizeApp function argument =
  case function of
    ExLam binder (ExCast (ExVar name) coercion)
      | name == binderName binder -> ExCast argument coercion
    _ -> ExApp function argument

normalizeBind :: Bind -> Bind
normalizeBind bind = bind {bindRhs = normalizeExpr (bindRhs bind)}

normalizeAlt :: Alt -> Alt
normalizeAlt alternative =
  alternative {altRhs = normalizeExpr (altRhs alternative)}
