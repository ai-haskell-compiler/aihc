-- |
--
-- Module:      Language.Egison.Parser.Pattern.Expr
-- Description: Helpers to build a parser for expressions
-- Stability:   experimental
--
-- A helper module to build a parser for expressions.

module Language.Egison.Parser.Pattern.Expr
  ( ExprL
  , exprParser
  , atomParser
  , Table(..)
  , initTable
  , addInfix
  , addPrefix
  -- * Re-exports
  , module X
  )
where

-- re-exports
import           Language.Egison.Syntax.Pattern.Fixity
                                               as X

-- main
import qualified Data.IntMap                   as IntMap
                                                ( toDescList
                                                , fromList
                                                , alter
                                                )
import           Data.Bifunctor                 ( first )
import           Control.Monad.Reader           ( MonadReader(..) )
import           Control.Comonad.Cofree         ( Cofree(..) )
import           Control.Applicative            ( Alternative((<|>)) )
import           Control.Applicative.Combinators
                                                ( choice
                                                , optional
                                                )

import           Language.Egison.Syntax.Pattern.Base
                                                ( ExprF(..) )
import           Language.Egison.Syntax.Pattern.Fixity
                                                ( Fixity(..)
                                                , Precedence(..)
                                                , Associativity(..)
                                                )
import qualified Language.Egison.Syntax.Pattern.Fixity.Precedence
                                               as Prec
                                                ( toInt )
import           Language.Egison.Parser.Pattern.Prim
                                                ( ParseFixity(..)
                                                , ParseMode(..)
                                                , Source
                                                , Parse
                                                , extParser
                                                , lexeme
                                                , Locate(..)
                                                , Location(..)
                                                )


-- | A list of operators with same precedence.
data Table m f a
  = Table { infixRight :: [m (a -> a -> f a)]
          , infixLeft :: [m (a -> a -> f a)]
          , infixNone :: [m (a -> a -> f a)]
          , prefix :: [m (a -> f a)]
          }

-- | Initial 'Table' with no operator.
initTable :: Table m f a
initTable = Table [] [] [] []

-- | Add an infix operator to 'Table'.
addInfix :: Associativity -> m (a -> a -> f a) -> Table m f a -> Table m f a
addInfix AssocRight op table@Table { infixRight } =
  table { infixRight = op : infixRight }
addInfix AssocLeft op table@Table { infixLeft } =
  table { infixLeft = op : infixLeft }
addInfix AssocNone op table@Table { infixNone } =
  table { infixNone = op : infixNone }

-- | Add a prefix operator to 'Table'.
addPrefix :: m (a -> f a) -> Table m f a -> Table m f a
addPrefix op table@Table { prefix } = table { prefix = op : prefix }

locate :: Locate m => m (f (Cofree f Location)) -> m (Cofree f Location)
locate m = do
  (x, loc) <- getLocation m
  pure (loc :< x)

makeExprParser
  :: (Alternative m, Locate m)
  => m (f (Cofree f Location))
  -> [Table m f (Cofree f Location)]
  -> m (Cofree f Location)
makeExprParser atom = foldl addPrecLevel $ locate atom

addPrecLevel
  :: (Alternative m, Locate m)
  => m (Cofree f Location)
  -> Table m f (Cofree f Location)
  -> m (Cofree f Location)
addPrecLevel atom table = do
  begin <- getPosition
  x     <- atom'
  choice [right begin x, left begin x, none begin x, pure x]
 where
  Table { infixRight, infixLeft, infixNone, prefix } = table
  atom' = pPrefix (choice prefix)
  right = pInfixR (choice infixRight) atom'
  left  = pInfixL (choice infixLeft) atom'
  none  = pInfixN (choice infixNone) atom'
  pPrefix op = do
    begin <- getPosition
    mpre  <- optional op
    x     <- atom
    case mpre of
      Nothing -> pure x
      Just f  -> ends begin $ f x
  pInfixR op p begin x = do
    f <- op
    y <- do
      begin' <- getPosition
      r      <- p
      pInfixR op p begin' r <|> pure r
    ends begin $ f x y
  pInfixL op p begin x = do
    r <- do
      f <- op
      y <- p
      ends begin $ f x y
    pInfixL op p begin r <|> pure r
  pInfixN op p begin x = do
    f <- op
    y <- p
    ends begin $ f x y
  ends begin x = do
    end <- getPosition
    let location = Location { begin, end }
    pure (location :< x)

-- | 'Language.Egison.Syntax.Pattern.Expr.Expr' with locations annotated.
type ExprL n v e = Cofree (ExprF n v e) Location

-- | Build an operator table from primitive operator table and 'ParseMode' context.
-- Note that the behavior is undefined when the supplied 'ParseMode' contains some fixities that conflict with ones provided via the parameter.
buildOperatorTable
  :: (MonadReader (ParseMode n v e s) m, Source s)
  => [(Precedence, Table (Parse n v e s) (ExprF n v e) (ExprL n v e))]
  -> m [Table (Parse n v e s) (ExprF n v e) (ExprL n v e)]
buildOperatorTable primInfixes = do
  ParseMode { fixities } <- ask
  pure . map snd . IntMap.toDescList $ foldr go prim fixities
 where
  go (ParseFixity (Fixity assoc prec n) p) =
    addPrecToTable prec . addInfix assoc $ makeOperator n p
  makeOperator n p = InfixF n <$ lexeme (extParser p)
  prim = IntMap.fromList $ map (first Prec.toInt) primInfixes
  addPrecToTable prec f = adjustWithDefault f (f initTable) $ Prec.toInt prec
  adjustWithDefault f def = IntMap.alter (Just . maybe def f)

-- | Build an expression parser with location information, from an atom parser.
exprParser
  :: Source s
  => [(Precedence, Table (Parse n v e s) (ExprF n v e) (ExprL n v e))]
  -> Parse n v e s (ExprF n v e (ExprL n v e))
  -> Parse n v e s (ExprL n v e)
exprParser primInfixes atom = do
  ops <- buildOperatorTable primInfixes
  makeExprParser atom ops

-- | Build an atom parser with location information, from an atom parser. (i.e. just adds a location information)
atomParser
  :: Source s
  => Parse n v e s (ExprF n v e (ExprL n v e))
  -> Parse n v e s (ExprL n v e)
atomParser = locate
