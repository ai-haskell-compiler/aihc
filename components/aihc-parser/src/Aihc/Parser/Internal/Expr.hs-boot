{-# LANGUAGE Haskell2010 #-}

-- | Boot file for Aihc.Parser.Internal.Expr
-- This breaks circular dependencies between Expr.hs and modules that
-- depend on it (Decl.hs, Type.hs, Pattern.hs, Cmd.hs).
--
-- IMPORTANT: When adding or changing exported function signatures in Expr.hs,
-- this boot file must be updated accordingly.
module Aihc.Parser.Internal.Expr where

import Aihc.Parser.Internal.Common (TokParser)
import Aihc.Parser.Syntax (Decl, Expr, Rhs, Type)

-- | Parse a full expression
exprParser :: TokParser Expr
exprParserWithTypeSigParser :: TokParser Type -> TokParser Expr
atomExprParser :: TokParser Expr

-- | Parse an expression without consuming arrow tail operators.
-- Used in command contexts where -< / -<< should be left for the command parser.
exprParserNoArrowTail :: TokParser Expr

-- | Parse the right-hand side of an equation (guarded or unguarded)
equationRhsParser :: TokParser (Rhs Expr)

-- | Parse a case-style right-hand side with a custom body parser.
caseRhsParserWithBodyParser :: TokParser body -> TokParser (Rhs body)

-- | Parse let declarations (keyword 'let' followed by braced or plain decls)
parseLetDeclsParser :: TokParser [Decl]

-- | Parse let declarations for statement context (no 'in' following)
parseLetDeclsStmtParser :: TokParser [Decl]
