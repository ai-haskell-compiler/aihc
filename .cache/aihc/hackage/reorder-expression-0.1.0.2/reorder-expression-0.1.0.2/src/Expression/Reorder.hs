{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE ScopedTypeVariables    #-}

{- |
Module      : Expression.Reorder
Copyright   : (c) 2021 comp
License     : MIT
Maintainer  : onecomputer00@gmail.com
Stability   : stable
Portability : portable

Reorders expressions in a syntax tree so that prefix, postfix, and infix operator chains are correct according to their
associativity and precedence.

Get started by creating a 'SyntaxTree' instance for your syntax types.
-}
module Expression.Reorder
    ( -- * Syntax tree reordering
      SyntaxTree(..)
    , reorder
    , Node(..)
    , Validation(..)
      -- * Operator properties
    , Fixity(..)
    , Assoc(..)
    , Precedence
    , Ambiguity(..)
      -- * Example usage
      -- $example
    ) where

import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)

{- | Typeclass for syntax trees @t@ with ambiguity errors @e@.

The reason for the error type is because there may be different types of expressions, e.g. value expressions and pattern
matching patterns, so there is no way to return the offending expression without combining the types first.
-}
class SyntaxTree t e | t -> e where
    {- | Applies 'reorder' to all children of this node that may have expressions to reorder.

    This is usually in the form of a traversal over the children, which will aggregate errors via 'Validation'.
    -}
    reorderChildren :: t -> Validation (NonEmpty e) t

    {- | Gets the structure of a node. -}
    structureOf :: t -> Node t

    {- | Builds an error for the ambiguous expression given. -}
    makeError :: Ambiguity -> t -> e

{- | Reorders a syntax tree to have correct precedence and associativity.

Returns either the reordered tree or a list of ambiguous expression errors.
-}
reorder :: forall t e. SyntaxTree t e => t -> Validation (NonEmpty e) t
reorder = reorderChildren `thenValidate` goReorder
    where
        goReorder :: t -> Validation (NonEmpty e) t
        goReorder expr = case structureOf expr of
            NodeLeaf -> pure expr
            NodePrefix p1 inner op1 -> case structureOf inner of
                NodeInfix f2 pivot x op2 ->
                    goOpenRight expr (Fixity AssocLeft p1) f2 op1 (`op2` x) pivot
                NodePostfix p2 pivot op2 ->
                    goOpenRight expr (Fixity AssocLeft p1) (Fixity AssocRight p2) op1 op2 pivot
                _closedLeft -> pure expr
            NodePostfix p1 inner op1 -> case structureOf inner of
                NodeInfix f2 x pivot op2 ->
                    goOpenLeft expr (Fixity AssocRight p1) f2 op1 (x `op2`) pivot
                NodePrefix p2 pivot op2 ->
                    goOpenLeft expr (Fixity AssocRight p1) (Fixity AssocLeft p2) op1 op2 pivot
                _closedRight -> pure expr
            NodeInfix f1 lhs rhs op1 -> case (structureOf lhs, structureOf rhs) of
                -- Where both sides are open.
                (NodeInfix f2 x pivotx op2, NodeInfix f3 pivoty y op3) ->
                    goOpenBoth expr f1 f2 f3 op1 lhs rhs (x `op2`) (`op3` y) pivotx pivoty
                (NodeInfix f2 x pivotx op2, NodePostfix p3 pivoty op3) ->
                    goOpenBoth expr f1 f2 (Fixity AssocRight p3) op1 lhs rhs (x `op2`) op3 pivotx pivoty
                (NodePrefix p2 pivotx op2, NodeInfix f3 pivoty y op3) ->
                    goOpenBoth expr f1 (Fixity AssocLeft p2) f3 op1 lhs rhs op2 (`op3` y) pivotx pivoty
                (NodePrefix p2 pivotx op2, NodePostfix p3 pivoty op3) ->
                    goOpenBoth expr f1 (Fixity AssocLeft p2) (Fixity AssocRight p3) op1 lhs rhs op2 op3 pivotx pivoty
                -- Where only the left side is open.
                (NodeInfix f2 x pivot op2, _rightIsClosed) ->
                    goOpenLeft expr f1 f2 (`op1` rhs) (x `op2`) pivot
                (NodePrefix p2 pivot op2, _rightIsClosed) ->
                    goOpenLeft expr f1 (Fixity AssocLeft p2) (`op1` rhs) op2 pivot
                -- Where only the right side is open.
                (_leftIsClosed, NodeInfix f3 pivot y op3) ->
                    goOpenRight expr f1 f3 (lhs `op1`) (`op3` y) pivot
                (_leftIsClosed, NodePostfix p3 pivot op3) ->
                    goOpenRight expr f1 (Fixity AssocRight p3) (lhs `op1`) op3 pivot
                -- Both sides are closed.
                (_leftIsClosed, _rightIsClosed) -> pure expr

        goOpenBoth
            :: t             -- ^ Original expression
            -> Fixity        -- ^ Fixity of root node
            -> Fixity        -- ^ Fixity of LHS
            -> Fixity        -- ^ Fixity of RHS
            -> (t -> t -> t) -- ^ Rebuild root node
            -> t             -- ^ LHS
            -> t             -- ^ RHS
            -> (t -> t)      -- ^ Rebuild LHS with new inner RHS
            -> (t -> t)      -- ^ Rebuild RHS with new inner LHS
            -> t             -- ^ The inner RHS of LHS
            -> t             -- ^ The inner LHS of RHS
            -> Validation (NonEmpty e) t
        goOpenBoth expr (Fixity a1 p1) (Fixity a2 p2) (Fixity a3 p3) op lhs rhs prefix suffix pivotx pivoty
            -- Side precedences are equal to root, so associativity will tiebreak.
            | p1 == p2 && p1 == p3 = if
                | a1 /= a2 && a1 /= a3 && a2 /= a3 -> failure $ makeError AmbiguityMismatchAssoc expr
                | a1 /= a2 -> failure $ makeError AmbiguityMismatchAssoc (lhs `op` pivoty)
                | a1 /= a3 -> failure $ makeError AmbiguityMismatchAssoc (pivotx `op` rhs)
                | AssocNone <- a1 -> failure $ makeError AmbiguityAssocNone expr
                | AssocLeft <- a1 -> suffix <$> reorder (lhs `op` pivoty)
                | AssocRight <- a1 -> prefix <$> reorder (pivotx `op` rhs)
            -- Left-hand side has equal precedence to root, but not right-hand side.
            | p1 == p2 = if
                | a1 /= a2 -> failure $ makeError AmbiguityMismatchAssoc (lhs `op` pivoty)
                | AssocNone <- a1 -> failure $ makeError AmbiguityAssocNone (lhs `op` pivoty)
                | AssocLeft <- a1 -> if p1 < p3
                    then pure expr
                    else suffix <$> reorder (lhs `op` pivoty)
                | AssocRight <- a1 -> if p1 < p3
                    then prefix <$> reorder (pivotx `op` rhs)
                    else suffix . prefix <$> reorder (pivotx `op` pivoty)
            -- Similar to previous, but opposite direction.
            | p1 == p3 = if
                | a1 /= a3 -> failure $ makeError AmbiguityMismatchAssoc (pivotx `op` rhs)
                | AssocNone <- a1 -> failure $ makeError AmbiguityAssocNone (pivotx `op` rhs)
                | AssocRight <- a1 -> if p1 < p2
                    then pure expr
                    else prefix <$> reorder (pivotx `op` rhs)
                | AssocLeft <- a1 -> if p1 < p2
                    then suffix <$> reorder (lhs `op` pivoty)
                    else prefix . suffix <$> reorder (pivotx `op` pivoty)
            -- From here on, the two side precedences are different from the root.
            | p1 > p2 && p1 > p3 = if
                -- Two side precedences are equal, so associativity will tiebreak.
                | p2 == p3 -> if
                    | a2 /= a3 -> failure $ makeError AmbiguityMismatchAssoc expr
                    | AssocNone <- a2 -> failure $ makeError AmbiguityAssocNone expr
                    | AssocLeft <- a2 -> suffix . prefix <$> reorder (pivotx `op` pivoty)
                    | AssocRight <- a2 -> prefix . suffix <$> reorder (pivotx `op` pivoty)
                | p2 > p3 -> suffix . prefix <$> reorder (pivotx `op` pivoty)
                | otherwise -> prefix . suffix <$> reorder (pivotx `op` pivoty)
            | p1 > p2 && p1 < p3 = prefix <$> reorder (pivotx `op` rhs)
            | p1 < p2 && p1 > p3 = suffix <$> reorder (lhs `op` pivoty)
            | otherwise = pure expr

        goOpenLeft
            :: t        -- ^ Original expression
            -> Fixity   -- ^ Fixity of root node
            -> Fixity   -- ^ Fixity of LHS
            -> (t -> t) -- ^ Rebuild root node
            -> (t -> t) -- ^ Rebuild LHS with new inner RHS
            -> t        -- ^ The inner RHS of LHS
            -> Validation (NonEmpty e) t
        goOpenLeft expr (Fixity a1 p1) (Fixity a2 p2) op prefix pivot
            | p1 == p2 = if
                | a1 /= a2 -> failure $ makeError AmbiguityMismatchAssoc expr
                | AssocNone <- a1 -> failure $ makeError AmbiguityAssocNone expr
                | AssocLeft <- a1 -> pure expr
                | AssocRight <- a1 -> prefix <$> reorder (op pivot)
            | p1 > p2 = prefix <$> reorder (op pivot)
            | otherwise = pure expr

        goOpenRight
            :: t        -- ^ Original expression
            -> Fixity   -- ^ Fixity of root node
            -> Fixity   -- ^ Fixity of RHS
            -> (t -> t) -- ^ Rebuild root node
            -> (t -> t) -- ^ Rebuild RHS with new inner LHS
            -> t        -- ^ The inner LHS of RHS
            -> Validation (NonEmpty e) t
        goOpenRight expr (Fixity a1 p1) (Fixity a3 p3) op suffix pivot
            | p1 == p3 = if
                | a1 /= a3 -> failure $ makeError AmbiguityMismatchAssoc expr
                | AssocNone <- a1 -> failure $ makeError AmbiguityAssocNone expr
                | AssocRight <- a1 -> pure expr
                | AssocLeft <- a1 -> suffix <$> reorder (op pivot)
            | p1 > p3 = suffix <$> reorder (op pivot)
            | otherwise = pure expr

{- | The structure of a node in a syntax tree in regards to operations.

A non-leaf node is made up of:

* An operator (associativity and precedence for infix nodes, just precedence for unary nodes).
* The open children of the node i.e. the children that may have reordering happen.
* A rebuilding function, which replaces the children of node and rebuilds it e.g. updating source locations.

Note that the arity referred to is the number of open children, not the arity of the operation itself.
-}
data Node t
    {- | A prefix operator, where only the right-hand side is open, e.g. @-n@ or @if p then x else y@. -}
    = NodePrefix Precedence t (t -> t)
    {- | A postfix operator, where only the left-hand side is open, e.g. @obj.field@ or @xs[n]@. -}
    | NodePostfix Precedence t (t -> t)
    {- | An infix operator, where both sides are open, e.g. @x + y@ or @p ? x : y@. -}
    | NodeInfix Fixity t t (t -> t -> t)
    {- | A leaf node where expressions may be contained, but are not open, e.g. @(x + y)@ or @do { x }@. -}
    | NodeLeaf

{- | Validation applicative, similar to 'Either' but aggregates errors. -}
data Validation e a
    = Success a
    | Failure e
    deriving (Show, Eq, Generic)

instance Functor (Validation e) where
    fmap f (Success a) = Success (f a)
    fmap _ (Failure e) = Failure e

instance Bifunctor Validation where
    bimap _ f (Success x) = Success (f x)
    bimap f _ (Failure x) = Failure (f x)

instance Semigroup e => Applicative (Validation e) where
    pure x = Success x

    Success f <*> Success a = Success (f a)
    Failure e <*> Success _ = Failure e
    Success _ <*> Failure e = Failure e
    Failure x <*> Failure y = Failure (x <> y)

failure :: e -> Validation (NonEmpty e) a
failure = Failure . pure

thenValidate :: (a -> Validation e b) -> (b -> Validation e c) -> a -> Validation e c
thenValidate f g x = case f x of
    Failure e -> Failure e
    Success y -> g y

{- | The fixity of an operator. -}
data Fixity = Fixity
    { fixityAssoc :: Assoc
    , fixityPrec :: Precedence
    }
    deriving (Show, Eq, Generic)

{- | The associativity of an operator. -}
data Assoc
    {- | Associates to the left: @(a * b) * c@. -}
    = AssocLeft
    {- | Associates to the right: @a * (b * c)@. -}
    | AssocRight
    {- | Does not associate at all: @a * b * c@ would be ambiguous. -}
    | AssocNone
    deriving (Show, Eq, Generic)

{- | The precedence of the operator.

Higher precedence binds tighter.
-}
type Precedence = Double

{- | An ambiguity in the operator chain. -}
data Ambiguity
    {- | Multiple operators with same precedence but different associativities in a chain. -}
    = AmbiguityMismatchAssoc
    {- | Multiple non-associative infix operators in a chain e.g. @1 == 2 == 3@. -}
    | AmbiguityAssocNone
    deriving (Show, Eq, Generic)

{- $example

First, we implement the 'SyntaxTree' class for our expression type:

> data Expr
>   = ExprBinary BinOp Expr Expr
>   | ExprPrefix PreOp Expr
>   | ExprTuple [Expr]
>   | ExprInt Int
>
> fixityOf :: BinOp -> Fixity
> precOf :: PreOp -> Precedence
>
> instance SyntaxTree Expr String where
>     reorderChildren expr = case expr of
>         ExprBinary op l r -> ExprBinary op <$> reorder l <*> reorder r
>         ExprPrefix op x -> ExprPrefix op <$> reorder x
>         ExprTuple xs -> ExprTuple <$> traverse reorder xs
>         _ -> pure expr
>
>     structureOf expr = case expr of
>         ExprBinary binop l r -> NodeInfix (fixityOf binop) l r (ExprBinary binop)
>         ExprPrefix preop x -> NodePrefix (precOf preop) x (ExprPrefix preop)
>         _ -> NodeLeaf
>
>     makeError err _ = show err

Writing the traversals manually for 'reorderChildren' can be tedious, but can easily be done with other libraries, such
as @types@ from @generic-lens@ or @gplate@ from @optics@.

Then, use 'reorder' to apply the reordering to a tree:

>>> reorder $ ExprBinary BinOpMul (ExprBinary BinOpAdd (ExprInt 1) (ExprInt 2)) (ExprInt 3) -- (1 + 2) * 3
ExprBinary BinOpAdd (ExprInt 1) (ExprBinary BinOpMul (ExprInt 2) (ExprInt 3))               -- 1 + (2 * 3)

If your syntax tree is annotated with e.g. source positions, you can rebuild those in the function of 'Node':

> (<~>) :: (HasSourcePos a, HasSourcePos b) => a -> b -> SourcePos
>
> structureOf (Located _ expr) = case expr of
>     ExprBinary binop l r -> NodeInfix (fixityOf binop) l r (\l' r' -> Located (l' <~> r') $ ExprBinary binop l' r')
>     ExprPrefix preop x -> NodePrefix (precOf preop) x (\x' -> Located (preop <~> x') $ ExprPrefix preop x')
>     _ -> NodeLeaf

Higher arity operations, where at most two child expressions are open, are supported; they can be treated as a prefix,
postfix, or infix operator depending on how many open child expressions there are:

> structureOf expr = case expr of
>     ExprTernary x y z -> NodeInfix ternaryFixity x z (\x' z' -> ExprTernary x' y z')   -- x ? y : z
>     ExprIfThenElse x y z -> NodePrefix ifThenElsePrec z (\z' -> ExprIfThenElse x y z') -- if x then y else z
>     ExprIndex x y -> NodePostfix indexPrec x (\x' -> ExprIndex x' y)                   -- x[y]
>     _ -> NodeLeaf
-}
