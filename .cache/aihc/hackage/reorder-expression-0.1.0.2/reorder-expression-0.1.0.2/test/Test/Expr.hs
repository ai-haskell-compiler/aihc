{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

module Test.Expr
    ( Pos(..)
    , Loc(..)
    , (@@)
    , (<~>)
    , OpIndex(..)
    , LExpr
    , LOp
    , Expr(..)
    , parenthesize
    , parenthesizeVerbose
    ) where

import Data.List (intercalate)
import GHC.Generics (Generic)
import Optics
import Expression.Reorder

{- | Span in source (by char count). -}
data Pos = Pos Int Int
    deriving (Show, Eq, Generic)

instance Semigroup Pos where
    Pos a b <> Pos c d = Pos (min a c) (max b d)

{- | An item with source location. -}
data Loc a = Loc
    { locPos  :: Pos
    , locItem :: a
    }
    deriving (Show, Eq, Generic)

{- | Alias for @flip Loc@. -}
infix 1 @@
(@@) :: a -> Pos -> Loc a
x @@ p = Loc p x

{- | Alias for @(<>) `on` locPos@. -}
infix 2 <~>
(<~>) :: Loc a -> Loc b -> Pos
Loc p1 _ <~> Loc p2 _ = p1 <> p2

type LExpr = Loc Expr

type LOp = Loc Fixity

{- | Operation nodes are labelled with an index, for testing purposes. -}
newtype OpIndex = OpIndex Int
    deriving (Show, Eq, Generic)

data Expr
    = ExprAtom Char
    | ExprGroup LExpr
    | ExprPrefix OpIndex LOp LExpr
    | ExprPostfix OpIndex LOp LExpr
    | ExprBinary OpIndex LOp LExpr LExpr
    | ExprIndex OpIndex LExpr (Loc [LExpr])
    deriving (Show, Eq, Generic)

instance SyntaxTree LExpr (Ambiguity, LExpr) where
    -- We can write all the traversals manually, but that's too much effort!
    reorderChildren = traverseOf (gplate @LExpr) reorder

    structureOf (Loc _ expr) = case expr of
        ExprBinary i op l r -> NodeInfix (locItem op) l r (\l2 r2 -> ExprBinary i op l2 r2 @@ l2 <~> r2)
        ExprPrefix i op x -> NodePrefix (fixityPrec $ locItem op) x (\x2 -> ExprPrefix i op x2 @@ op <~> x2)
        ExprPostfix i op x -> NodePostfix (fixityPrec $ locItem op) x (\x2 -> ExprPostfix i op x2 @@ x2 <~> op)
        ExprIndex i x xs -> NodePostfix 99 x (\x2 -> ExprIndex i x2 xs @@ x2 <~> xs)
        _ -> NodeLeaf

    makeError = (,)

{- | Prints the tree with parentheses everywhere. -}
parenthesize :: LExpr -> String
parenthesize (Loc _ expr) = case expr of
    ExprAtom c -> [c]
    ExprGroup x -> concat ["{", parenthesize x, "}"]
    ExprPrefix _ (Loc _ (Fixity _ p)) x ->
        concat ["(", "~", showRounded p, ".", " ", parenthesize x, ")"]
    ExprPostfix _ (Loc _ (Fixity _ p)) x ->
        concat ["(", parenthesize x, " ", "!", showRounded p, ".", ")"]
    ExprBinary _ (Loc _ (Fixity a p)) l r ->
        concat ["(", parenthesize l, " ", showBinA a, showRounded p, ".", " ", parenthesize r, ")"]
    ExprIndex _ x (Loc _ xs) ->
        concat ["(", parenthesize x, " ", "@{", intercalate ", " $ map parenthesize xs, "}", ")"]
    where
        showBinA a = case a of
            AssocLeft -> "<"
            AssocRight -> ">"
            AssocNone -> "="
        showRounded = show @Int . round

{- | Prints the tree with parentheses everywhere.

For testing purposes, also prints:

* The index of an operator node.
* The span of an operator node.
-}
parenthesizeVerbose :: String -> LExpr -> String
parenthesizeVerbose s (Loc w expr) = case expr of
    ExprAtom c -> [c]
    ExprGroup x -> concat ["{", parenthesizeVerbose s x, "}"]
    ExprPostfix i (Loc _ (Fixity _ p)) x ->
        concat ["(", parenthesizeVerbose s x, " ", "!", showRounded p, ".", showPos i w, ")"]
    ExprPrefix i (Loc _ (Fixity _ p)) x ->
        concat ["(", "~", showRounded p, ".", showPos i w, " ", parenthesizeVerbose s x, ")"]
    ExprBinary i (Loc _ (Fixity a p)) l r ->
        concat ["(", parenthesizeVerbose s l, " ", showBinA a, showRounded p, ".", showPos i w, " ", parenthesizeVerbose s r, ")"]
    ExprIndex i x (Loc _ xs) ->
        concat ["(", parenthesizeVerbose s x, " ", "@", showPos i w, "{", intercalate ", " . map (parenthesizeVerbose s) $ xs, "}", ")"]
    where
        showBinA a = case a of
            AssocLeft -> "<"
            AssocRight -> ">"
            AssocNone -> "="
        showRounded = show @Int . round
        showPos (OpIndex i) (Pos a b) = "[" <> show i <> ":" <> show a <> "-" <> show b <> "]"
