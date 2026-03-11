-- |
--
-- Module:      Language.Egison.Syntax.Pattern.Parser.Combinator
-- Description: Useful combinators to manipulate Egison pattern ASTs
-- Stability:   experimental
--
-- Useful combinators to manipulate Egison pattern ASTs.

module Language.Egison.Syntax.Pattern.Combinator
  ( unAnnotate
  , foldExpr
  , mapName
  , mapVarName
  , mapValueExpr
  , variables
  )
where

import           Control.Applicative            ( Alternative(..) )
import           Data.Foldable                  ( asum )
import           Data.Functor.Foldable          ( cata
                                                , embed
                                                )
import           Control.Comonad.Cofree         ( Cofree )
import           Control.Comonad.Trans.Cofree   ( CofreeF(..) )

import           Language.Egison.Syntax.Pattern.Expr
                                                ( Expr(..) )
import           Language.Egison.Syntax.Pattern.Base
                                                ( ExprF(..) )


-- | Unwrap annotations from AST.
unAnnotate :: Cofree (ExprF n v e) a -> Expr n v e
unAnnotate = cata go where go (_ :< x) = embed x

-- | fold an v expr.
--
-- Note that this is just a type specialization of 'cata'.
foldExpr :: (ExprF n v e a -> a) -> Expr n v e -> a
foldExpr = cata

-- TODO: Implement 'mapName' and 'mapValueExpr' by adding newtype wrapper for them and making them instances of 'MonoFunctor'

-- | Map over @n@ in @Expr n v e@.
mapName :: (n -> n') -> Expr n v e -> Expr n' v e
mapName f = cata go
 where
  go (InfixF n a b )  = Infix (f n) a b
  go (PatternF n ps)  = Pattern (f n) ps
  -- TODO: omit these verbose matches
  go WildcardF        = Wildcard
  go (VariableF  n  ) = Variable n
  go (ValueF     e  ) = Value e
  go (PredicateF e  ) = Predicate e
  go (AndF p1 p2    ) = And p1 p2
  go (OrF  p1 p2    ) = Or p1 p2
  go (NotF        p1) = Not p1
  go (CollectionF ps) = Collection ps
  go (TupleF      ps) = Tuple ps

-- | Map over @v@ in @Expr n v e@.
mapVarName :: (v -> v') -> Expr n v e -> Expr n v' e
mapVarName f = cata go
 where
  go (VariableF v)    = Variable (f v)
  -- TODO: omit these verbose matches
  go WildcardF        = Wildcard
  go (ValueF     e  ) = Value e
  go (PredicateF e  ) = Predicate e
  go (AndF p1 p2    ) = And p1 p2
  go (OrF  p1 p2    ) = Or p1 p2
  go (NotF        p1) = Not p1
  go (CollectionF ps) = Collection ps
  go (TupleF      ps) = Tuple ps
  go (InfixF n a b  ) = Infix n a b
  go (PatternF n ps ) = Pattern n ps

-- | Map over @e@ in @Expr n v e@.
mapValueExpr :: (e -> e') -> Expr n v e -> Expr n v e'
mapValueExpr f = cata go
 where
  go (ValueF     e)   = Value (f e)
  go (PredicateF e)   = Predicate (f e)
  -- TODO: omit these verbose matches
  go WildcardF        = Wildcard
  go (VariableF   n ) = Variable n
  go (CollectionF ps) = Collection ps
  go (TupleF      ps) = Tuple ps
  go (InfixF n p1 p2) = Infix n p1 p2
  go (PatternF n  ps) = Pattern n ps
  go (AndF     p1 p2) = And p1 p2
  go (OrF      p1 p2) = Or p1 p2
  go (NotF p1       ) = Not p1

-- | List bound pattern variables in a pattern.
variables :: Alternative f => Expr n v e -> f v
variables = cata go
 where
  go (VariableF n)    = pure n
  -- TODO: omit these verbose matches
  go WildcardF        = empty
  go (ValueF     _  ) = empty
  go (PredicateF _  ) = empty
  go (AndF p1 p2    ) = p1 <|> p2
  go (OrF  p1 p2    ) = p1 <|> p2
  go (NotF p1       ) = p1
  go (InfixF _ a b  ) = a <|> b
  go (PatternF _ ps ) = asum ps
  go (CollectionF ps) = asum ps
  go (TupleF      ps) = asum ps
