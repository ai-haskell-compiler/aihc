{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | The contents of this module originate from module
--  [HSE.FreeVars](https://github.com/ndmitchell/hlint/blob/master/src/HSE/FreeVars.hs)
--  in Neil Mitchell's HLint.
module Language.Haskell.Exts.FreeVars
  ( FreeVars(..)
  , Vars(..)
  , AllVars(..)
  ) where

import           Control.Monad
import           Data.Data
import           Data.Generics.Uniplate.Data
import           Data.Monoid (Monoid(..))
import           Data.Semigroup (Semigroup(..))
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Language.Haskell.Exts
import           Prelude

(^+) :: (Data s, Ord s) => Set (Name s) -> Set (Name s) -> Set (Name s)
(^+) = Set.union
(^-) :: (Data s, Ord s) => Set (Name s) -> Set (Name s) -> Set (Name s)
(^-) = Set.difference

data Vars = Vars {bound :: Set (Name ()), free :: Set (Name ())}

instance Semigroup Vars where
    Vars x1 x2 <> Vars y1 y2 = Vars (x1 ^+ y1) (x2 ^+ y2)

instance Monoid Vars where
    mempty = Vars Set.empty Set.empty
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif
    mconcat fvs = Vars (Set.unions $ map bound fvs) (Set.unions $ map free fvs)

class AllVars a where
    -- | Return the variables, erring on the side of more free variables
    allVars :: a -> Vars

class FreeVars a where
    -- | Return the variables, erring on the side of more free variables
    freeVars :: a -> Set (Name ())

freeVars_ :: (FreeVars a) => a -> Vars
freeVars_ = Vars Set.empty . freeVars

inFree
  :: (AllVars a, FreeVars b)
  => a -> b -> Set (Name ())
inFree a b = free aa ^+ (freeVars b ^- bound aa)
    where aa = allVars a

inVars
  :: (AllVars a, AllVars b)
  => a -> b -> Vars
inVars a b = Vars (bound aa ^+ bound bb) (free aa ^+ (free bb ^- bound aa))
    where aa = allVars a
          bb = allVars b

unqualNames :: QName s -> [Name ()]
unqualNames (UnQual _ x) = [withNoLoc x]
unqualNames _            = []

unqualOp :: QOp s -> [Name ()]
unqualOp (QVarOp _ x) = unqualNames x
unqualOp (QConOp _ x) = unqualNames x

withNoLoc :: Name s -> Name ()
withNoLoc = void

instance (Data s, Ord s) => FreeVars (Set (Name s)) where
    freeVars = Set.map withNoLoc

instance AllVars Vars where
    allVars = id

instance (Data s, Ord s) => FreeVars (Exp s) where -- never has any bound variables
    freeVars (Var _ x) = Set.fromList $ unqualNames x
    freeVars (VarQuote l x) = freeVars $ Var l x
    freeVars (SpliceExp _ (IdSplice l x)) = Set.fromList [withNoLoc $ Ident l x]
    freeVars (InfixApp _ a op b) =
      freeVars a ^+ Set.fromList (unqualOp op) ^+ freeVars b
    freeVars (LeftSection _ a op) = freeVars a ^+ Set.fromList (unqualOp op)
    freeVars (RightSection _ op b) = Set.fromList (unqualOp op) ^+ freeVars b
    freeVars (Lambda _ p x) = inFree p x
    freeVars (Let _ bind x) = inFree bind x
    freeVars (Case _ x alts) = freeVars x `mappend` freeVars alts
    freeVars (Do _ xs) = free $ allVars xs
    freeVars (MDo l xs) = freeVars $ Do l xs
    freeVars (RecConstr _ _ a) = Set.unions $ map freeVars a
    freeVars (RecUpdate _ a b) = Set.unions $ freeVars a : map freeVars b
    freeVars (ParComp _ x xs) = free xfv ^+ (freeVars x ^- bound xfv)
        where xfv = mconcat $ map allVars xs
    freeVars (ListComp l x xs) = freeVars $ ParComp l x [xs]
    freeVars x = freeVars $ children x

instance (Data s, Ord s) => FreeVars (FieldUpdate s) where
    freeVars (FieldUpdate _ _ x) = freeVars x
    freeVars (FieldPun _ x) = Set.fromList $ unqualNames x
    freeVars (FieldWildcard _) = Set.empty -- have no idea what's in here

instance (Data s, Ord s) => FreeVars [Exp s] where
    freeVars = Set.unions . map freeVars

instance (Data s, Ord s) => AllVars (Pat s) where
    allVars (PVar _ x)       = Vars (Set.singleton $ withNoLoc x) Set.empty
    allVars (PNPlusK l x _)  = allVars (PVar l x)
    allVars (PAsPat l n x)   = allVars (PVar l n) `mappend` allVars x
    allVars (PRec _ _ x)     = allVars x
    allVars (PViewPat _ e p) = freeVars_ e `mappend` allVars p
    allVars x                = allVars $ children x

instance (Data s, Ord s) => AllVars [Pat s] where
    allVars = mconcat . map allVars

instance (Data s, Ord s) => AllVars (PatField s) where
    allVars (PFieldPat _ _ x) = allVars x
    allVars (PFieldPun _ (UnQual _ x)) = Vars (Set.singleton $ withNoLoc x) Set.empty
    allVars (PFieldPun _ (Qual _ _ x)) = Vars (Set.singleton $ withNoLoc x) Set.empty
    allVars (PFieldWildcard _) = mempty -- explicitly cannot guess what might be bound here

instance (Data s, Ord s) => AllVars [PatField s] where
    allVars = mconcat . map allVars

instance (Data s, Ord s) => FreeVars (Alt s) where
    freeVars (Language.Haskell.Exts.Alt _ pat alt bind) = inFree pat $ inFree bind alt

instance (Data s, Ord s) => FreeVars [Alt s] where
    freeVars = mconcat . map freeVars

instance (Data s, Ord s) => FreeVars (Rhs s) where
    freeVars (UnGuardedRhs _ x) = freeVars x
    freeVars (GuardedRhss _ xs) = mconcat $ map freeVars xs

instance (Data s, Ord s) => FreeVars (GuardedRhs s) where
    freeVars (GuardedRhs _ stmt exp) = inFree stmt exp

instance (Data s, Ord s) => AllVars (QualStmt s) where
    allVars (QualStmt _ x) = allVars x
    allVars x              = freeVars_ (childrenBi x :: [Exp s])

instance (Data s, Ord s) => AllVars [QualStmt s] where
    allVars (x:xs) = inVars x xs
    allVars []     = mempty

instance (Data s, Ord s) => AllVars [Stmt s] where
    allVars (x:xs) = inVars x xs
    allVars []     = mempty

instance (Data s, Ord s) => AllVars (Stmt s) where
    allVars (Generator _ pat exp) = allVars pat `mappend` freeVars_ exp
    allVars (Qualifier _ exp)     = freeVars_ exp
    allVars (LetStmt _ binds)     = allVars binds
    allVars (RecStmt _ stmts)     = allVars stmts

instance (Data s, Ord s) => AllVars (Maybe (Binds s)) where
    allVars = maybe mempty allVars

instance (Data s, Ord s) => AllVars (Binds s) where
    allVars (BDecls _ decls)  = allVars decls
    allVars (IPBinds _ binds) = freeVars_ binds

instance (Data s, Ord s) => AllVars [Decl s] where
    allVars = mconcat . map allVars

instance (Data s, Ord s) => AllVars (Decl s) where
    allVars (FunBind _ m) = allVars m
    allVars (PatBind _ pat rhs bind) = allVars pat `mappend` freeVars_ (inFree bind rhs)
    allVars _ = mempty

instance (Data s, Ord s) => AllVars [Match s] where
    allVars = mconcat . map allVars

instance (Data s, Ord s) => AllVars (Match s) where
    allVars (Match l name pat rhs binds) = allVars (PVar l name) `mappend` freeVars_ (inFree pat (inFree binds rhs))
    allVars (InfixMatch l p1 name p2 rhs binds) = allVars $ Match l name (p1:p2) rhs binds

instance (Data s, Ord s) => FreeVars [IPBind s] where
    freeVars = mconcat . map freeVars

instance (Data s, Ord s) => FreeVars (IPBind s) where
    freeVars (IPBind _ _ exp) = freeVars exp
