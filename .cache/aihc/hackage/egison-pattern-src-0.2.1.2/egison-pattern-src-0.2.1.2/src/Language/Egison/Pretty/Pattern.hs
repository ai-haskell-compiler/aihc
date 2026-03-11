-- |
--
-- Module:      Language.Egison.Pretty.Pattern
-- Description: Pretty printer for Egison patterns
-- Stability:   experimental
--
-- A pretty printer for Egison patterns.

module Language.Egison.Pretty.Pattern
  ( prettyExpr
  -- * Re-exports
  , module X
  )
where

-- re-exports
import           Language.Egison.Pretty.Pattern.Error
                                               as X
                                                ( Error(..) )
import           Language.Egison.Pretty.Pattern.PrintMode
                                               as X
                                                ( ExtPrinter
                                                , PrintMode(..)
                                                , PageMode(..)
                                                , PrintFixity(..)
                                                )
import           Language.Egison.Syntax.Pattern.Fixity
                                               as X
                                                ( Fixity(..)
                                                , Associativity(..)
                                                , Precedence(..)
                                                )

-- main
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import           Control.Monad.Except           ( MonadError(..) )
import           Language.Egison.Pretty.Pattern.Prim
                                                ( Doc
                                                , hsep
                                                , list
                                                , tupled
                                                , text
                                                , parens
                                                , (<+>)
                                                , renderDoc
                                                )
import           Language.Egison.Pretty.Pattern.Error
                                                ( Error(UnknownInfixOperator) )
import           Language.Egison.Pretty.Pattern.External
                                                ( name
                                                , varName
                                                , valueExpr
                                                )
import           Language.Egison.Pretty.Pattern.Print
                                                ( Print
                                                , runPrint
                                                , askContext
                                                , withContext
                                                , operatorOf
                                                )
import           Language.Egison.Pretty.Pattern.Context
                                                ( Context(..)
                                                , Side(..)
                                                )
import           Language.Egison.Pretty.Pattern.Operator
                                                ( Operator(..) )
import qualified Language.Egison.Syntax.Pattern.Fixity.Primitive
                                               as PrimOp
import           Language.Egison.Syntax.Pattern ( Expr(..) )


parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

parensWhen :: (Context -> Bool) -> Doc -> Print n v e Doc
parensWhen f doc = do
  ctx <- askContext
  pure $ parensIf (f ctx) doc

smartParens :: Operator -> Doc -> Print n v e Doc
smartParens opr = parensWhen (check opr)
 where
  check _          World = False
  check PrefixOp{} Atom  = False
  check _          Atom  = True
  check InfixOp { precedence, associativity } (Under uPrec side)
    | uPrec > precedence = True
    | uPrec == precedence && not (matching associativity side) = True
    | otherwise          = False
  check PrefixOp { precedence } (Under uPrec _) | uPrec >= precedence = True
                                                | otherwise           = False
  matching AssocRight RightSide = True
  matching AssocLeft  LeftSide  = True
  matching _          _         = False

expr :: Ord n => Expr n v e -> Print n v e Doc
expr Wildcard     = pure "_"
expr (Variable v) = do
  dv <- varName v
  pure $ "$" <> dv
expr (Value e) = do
  de <- valueExpr e
  pure $ "#" <> de
expr (Predicate e) = do
  de <- valueExpr e
  pure $ "?" <> de
expr (And e1 e2) = do
  d1 <- withContext (Under PrimOp.andPrecedence LeftSide) $ expr e1
  d2 <- withContext (Under PrimOp.andPrecedence RightSide) $ expr e2
  smartParens opr $ d1 <+> "&" <+> d2
 where
  opr = InfixOp { precedence    = PrimOp.andPrecedence
                , associativity = PrimOp.andAssociativity
                , symbol        = "&"
                }
expr (Or e1 e2) = do
  d1 <- withContext (Under PrimOp.orPrecedence LeftSide) $ expr e1
  d2 <- withContext (Under PrimOp.orPrecedence RightSide) $ expr e2
  smartParens opr $ d1 <+> "|" <+> d2
 where
  opr = InfixOp { precedence    = PrimOp.orPrecedence
                , associativity = PrimOp.orAssociativity
                , symbol        = "|"
                }
expr (Not e) = do
  d <- withContext Atom $ expr e
  pure $ "!" <> d
expr (Tuple      es) = tupled <$> traverse expr es
expr (Collection es) = list <$> traverse expr es
expr (Infix n e1 e2) = do
  opr <- operatorOf n
  case opr of
    InfixOp { precedence, symbol } -> do
      d1 <- withContext (Under precedence LeftSide) $ expr e1
      d2 <- withContext (Under precedence RightSide) $ expr e2
      smartParens opr $ d1 <+> text symbol <+> d2
    _ -> throwError $ UnknownInfixOperator n
expr (Pattern n []) = name n
expr (Pattern n es) = do
  dn <- name n
  ds <- withContext Atom $ traverse expr es
  parensWhen check $ dn <+> hsep ds
 where
  check Atom = True
  check _    = False

-- | Pretty print 'Expr'.
prettyExpr
  :: (MonadError (Error n) m, Ord n) => PrintMode n v e -> Expr n v e -> m Text
prettyExpr mode e = do
  doc <- runPrint (expr e) mode
  pure $ renderDoc mode doc
