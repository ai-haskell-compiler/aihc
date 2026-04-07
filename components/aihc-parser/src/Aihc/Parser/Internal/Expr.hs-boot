{-# LANGUAGE Haskell2010 #-}

-- | Boot file for Aihc.Parser.Internal.Expr
-- This breaks the circular dependency between Expr.hs and Decl.hs.
-- Decl.hs imports this boot file, and Expr.hs imports Decl.hs.
-- GHC resolves the cycle by compiling the boot file first.
--
-- IMPORTANT: When adding or changing exported function signatures in Expr.hs,
-- this boot file must be updated accordingly.
module Aihc.Parser.Internal.Expr where

import Aihc.Parser.Internal.Common (TokParser)
import Aihc.Parser.Syntax (Expr, Pattern, Rhs, Type)

-- | Parse a full expression
exprParser :: TokParser Expr

-- | Parse a type expression
typeParser :: TokParser Type

-- | Parse a pattern
patternParser :: TokParser Pattern

-- | Parse the right-hand side of an equation (guarded or unguarded)
equationRhsParser :: TokParser Rhs

-- | Parse a simple pattern (no top-level context or where clause)
simplePatternParser :: TokParser Pattern

-- | Parse a type application (type followed by type arguments)
typeAppParser :: TokParser Type

-- | Parse a type atom (single type, not an application)
typeAtomParser :: TokParser Type

-- | Lookahead check: does the input start with a type signature?
startsWithTypeSig :: TokParser Bool

-- | Lookahead check: does the input start with a context (=>)?
startsWithContextType :: TokParser Bool
