module Data.Expression
    ( Expression(..)
    , expressionSym
    , runValueExpression
    , runMatchExpression
    ) where

data Expression a g f r
    = ClosedExpression (f r)
    | OpenExpression a
                     (Expression a g f (g r))

instance (Functor f, Functor g) => Functor (Expression a g f) where
    fmap pq (ClosedExpression fp) = ClosedExpression (fmap pq fp)
    fmap pq (OpenExpression a egp) = OpenExpression a (fmap (fmap pq) egp)

ffmap :: (Applicative f, Functor g) => f (p -> q) -> Expression a g f p -> Expression a g f q
ffmap fpq (ClosedExpression fp) = ClosedExpression (fpq <*> fp)
ffmap fpq (OpenExpression a ebp) = OpenExpression a (ffmap (fmap fmap fpq) ebp)

instance (Applicative f, Functor g) => Applicative (Expression a g f) where
    pure t = ClosedExpression (pure t)
    (ClosedExpression fpq) <*> ep = ffmap fpq ep
    (OpenExpression a egpq) <*> ep = OpenExpression a ((\p -> fmap (\pq -> pq p)) <$> ep <*> egpq)

expressionSym :: a -> f (g r) -> Expression a g f r
expressionSym a fbr = OpenExpression a (ClosedExpression fbr)

runValueExpression :: (Functor f) => Expression a ((->) b) f r -> f ((a -> b) -> r)
runValueExpression (ClosedExpression fr) = fmap (\r _ab -> r) fr
runValueExpression (OpenExpression a0 ebr) = fmap (\abbr ab -> abbr ab (ab a0)) (runValueExpression ebr)

runMatchExpression :: (Functor f) => Expression a ((,) b) f r -> f ([(a, b)], r)
runMatchExpression (ClosedExpression fr) = fmap (\r -> ([], r)) fr
runMatchExpression (OpenExpression a ebr) = fmap (\(ab, (b, r)) -> ((a, b) : ab, r)) (runMatchExpression ebr)
