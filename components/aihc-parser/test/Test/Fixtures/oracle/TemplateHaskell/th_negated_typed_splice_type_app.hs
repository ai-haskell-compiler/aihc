{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module TH_Negated_Typed_Splice_TypeApp where

-- Regression test: negation of a type-applied typed splice.
-- The pretty-printer must parenthesize the argument of ENegate when it
-- starts with a TH typed splice ($$), even after a type application is
-- applied to it, so that the lexer does not merge '-' and '$$' into the
-- single operator token '-$$'.
--
-- Without the fix, addDeclParens produced  f = -$$("") @C
-- which the lexer tokenised as TkVarSym "-$$", causing a parse failure.
-- With the fix it produces  f = -($$("") @C)  which round-trips correctly.
f = -($$("") @C)
g = -($$expr @T)
