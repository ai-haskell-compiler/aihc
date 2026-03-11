{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns -fno-warn-incomplete-patterns #-}

-- | The contents of this module originate from module
--  [HSE.Bracket](https://github.com/ndmitchell/hlint/blob/master/src/HSE/Bracket.hs)
--  in Neil Mitchell's HLint

module Language.Haskell.Exts.Bracket
  ( Brackets(..)
  , paren
  , transformBracket
  , rebracket1
  , appsBracket
  ) where

import           Control.Monad.Trans.State
import           Data.Data
import           Data.Default
import           Data.Generics.Uniplate.Data
import           Language.Haskell.Exts.Syntax
import           Language.Haskell.Exts.Util.Internal


class Brackets a where
    remParen :: a -> Maybe a -- remove one paren, or Nothing if there is no paren
    addParen :: a -> a -- write out a paren

    -- | Is this item lexically requiring no bracketing ever
    --   i.e. is totally atomic
    isAtom :: a -> Bool

    -- | Is the child safe free from brackets in the parent position.
    --   Err on the side of caution, True = don't know
    needBracket :: Int -> a -> a -> Bool


instance (Data l, Default l) => Brackets (Exp l) where
    remParen (Paren _ x) = Just x
    remParen _           = Nothing
    addParen = Paren def

    isAtom x = case x of
        Var{}                -> True
        Con{}                -> True
        Paren{}              -> True
        Tuple{}              -> True
        List{}               -> True
        LeftSection{}        -> True
        RightSection{}       -> True
        TupleSection{}       -> True
        RecConstr{}          -> True
        ListComp{}           -> True
        EnumFrom{}           -> True
        EnumFromTo{}         -> True
        EnumFromThen{}       -> True
        EnumFromThenTo{}     -> True
        OverloadedLabel{}    -> True
        ParArray{}           -> True
        ParComp{}            -> True
        XTag{}               -> True
        IPVar{}              -> True
        UnboxedSum{}         -> True
        RecUpdate{}          -> True
        ParArrayFromTo{}     -> True
        ParArrayFromThenTo{} -> True
        ParArrayComp{}       -> True
        VarQuote{}           -> True
        TypQuote{}           -> True
        BracketExp{}         -> True
        SpliceExp{}          -> True
        QuasiQuote{}         -> True
        TypeApp{}            -> True
        XETag{}              -> True
        XExpTag{}            -> True
        Lit _ x | not $ isNegative x -> True
        _                    -> False
        where
            isNegative (Int _ x _) = x < 0
            isNegative (Frac _ x _) = x < 0
            isNegative (PrimInt _ x _) = x < 0
            isNegative (PrimFloat _ x _) = x < 0
            isNegative (PrimDouble _ x _) = x < 0
            isNegative _ = False

    -- note: i is the index in children, not in the AST
    needBracket i parent child
        | isAtom child = False
        | InfixApp{} <- parent, App{} <- child = False
        | isSection parent, App{} <- child = False
        | Let{} <- parent, App{} <- child = False
        | ListComp{} <- parent = False
        | List{} <- parent = False
        | Tuple{} <- parent = False
        | TupleSection{} <- parent = False
        | If{} <- parent, isAnyApp child = False
        | App{} <- parent, i == 0, App{} <- child = False
        | ExpTypeSig{} <- parent, i == 0, isApp child = False
        | Paren{} <- parent = False
        | RecConstr{} <- parent = False
        | RecUpdate{} <- parent, i /= 0 = False
        | Case{} <- parent, i /= 0 || isAnyApp child = False
        | Lambda{} <- parent = False -- might be either the RHS of a PViewPat, or the lambda body (neither needs brackets)
        | Do{} <- parent = False
        | otherwise = True


instance Default l => Brackets (Type l) where
    remParen (TyParen _ x) = Just x
    remParen _             = Nothing
    addParen = TyParen def

    isAtom x = case x of
        TyParen{}      -> True
        TyTuple{}      -> True
        TyList{}       -> True
        TyVar{}        -> True
        TyCon{}        -> True
        TyPromoted{}   -> True
        TyUnboxedSum{} -> True
        TyParArray{}   -> True
        TyKind{}       -> True
        TySplice{}     -> True
        TyWildCard{}   -> True
        TyQuasiQuote{} -> True
        _              -> False

    needBracket _ parent child
        | isAtom child = False
-- a -> (b -> c) is not a required bracket, but useful for documentation about arity etc.
--        | TyFun{} <- parent, i == 1, TyFun{} <- child = False
        | TyFun{} <- parent, TyApp{} <- child = False
        | TyTuple{} <- parent = False
        | TyList{} <- parent = False
        | TyInfix{} <- parent, TyApp{} <- child = False
        | TyParen{} <- parent = False
        | otherwise = True


instance Default l => Brackets (Pat l) where
    remParen (PParen _ x) = Just x
    remParen _            = Nothing
    addParen = PParen def

    isAtom x = case x of
        PParen{}      -> True
        PTuple{}      -> True
        PList{}       -> True
        PRec{}        -> True
        PVar{}        -> True
        PApp _ _ []   -> True
        PWildCard{}   -> True
        PUnboxedSum{} -> True
        PAsPat{}      -> True
        PIrrPat{}     -> True
        PXETag{}      -> True
        PXPatTag{}    -> True
        PSplice{}     -> True
        PQuasiQuote{} -> True
        PLit _ Signless{} _ -> True
        _             -> False

    needBracket _ parent child
        | isAtom child = False
        | PTuple{} <- parent = False
        | PList{} <- parent = False
        | PInfixApp{} <- parent, PApp{} <- child = False
        | PParen{} <- parent = False
        | otherwise = True


-- | Add a Paren around something if it is not atomic
paren :: (Data l, Default l) => Exp l -> Exp l
paren x = if isAtom x then x else addParen x


-- | Descend, and if something changes then add/remove brackets appropriately
descendBracket :: (Data l, Default l) => (Exp l -> (Bool, Exp l)) -> Exp l -> Exp l
descendBracket op x = descendIndex g x
    where
        g i y = if a then f i b else b
            where (a,b) = op y

        f i (Paren _ y) | not $ needBracket i x y = y
        f i y           | needBracket i x y = addParen y
        f _ y           = y


transformBracket :: (Data l, Default l) => (Exp l -> Maybe (Exp l)) -> Exp l -> Exp l
transformBracket op = snd . g
    where
        g = f . descendBracket g
        f x = maybe (False,x) ((,) True) (op x)


-- | Add/remove brackets as suggested needBracket at 1-level of depth
rebracket1 :: (Data l, Default l) => Exp l -> Exp l
rebracket1 = descendBracket (\x -> (True,x))


-- a list of application, with any necessary brackets
appsBracket :: (Data l, Default l) => [Exp l] -> Exp l
appsBracket = foldl1 (\x -> rebracket1 . App def x)

descendIndex :: Data a => (Int -> a -> a) -> a -> a
descendIndex f x = flip evalState 0 $ flip descendM x $ \y -> do
    i <- get
    modify (+1)
    return $ f i y
