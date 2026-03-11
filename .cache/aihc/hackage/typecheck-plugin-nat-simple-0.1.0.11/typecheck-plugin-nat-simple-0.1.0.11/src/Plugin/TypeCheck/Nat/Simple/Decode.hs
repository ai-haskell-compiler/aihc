{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple.Decode (
	-- * DECODE CT
	decodeAll, decode, lookupOrdCondCompare ) where

import GHC.Tc.Types.Constraint (Ct)
import GHC.Builtin.Types.Literals (
	typeNatAddTyCon, typeNatSubTyCon, typeNatCmpTyCon) -- , typeNatLeqTyCon )
import GHC.Builtin.Types (promotedFalseDataCon, promotedTrueDataCon)
import GHC.Core.TyCo.Rep (Type(..), TyLit(..))
import GHC.Types.Var (Var)
import GHC.Utils.Outputable (ppr, text, (<+>))
import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Control.Monad.Try (Try, throw, rights, Set)
import Data.Log (IsSDoc, fromSDoc)
import Data.Derivation.Expression (Exp(..), ExpType(..))
import Plugin.TypeCheck.Nat.Simple.UnNomEq (unNomEq)

import GHC.Tc.Plugin
import GHC.TcPluginM.Extra
import GHC.Data.FastString
import GHC.Unit.Module
import GHC.Types.Name.Occurrence
import GHC.Core.TyCon

---------------------------------------------------------------------------

-- * DECODE
-- * BOOLEAN AND NUMBER

---------------------------------------------------------------------------
-- DECODE
---------------------------------------------------------------------------

decodeAll :: (Monoid w, IsSDoc w, Set w w) => (TyCon, TyCon) -> [Ct] -> Try w w [Exp Var 'Boolean]
decodeAll occ = rights . (decode occ <$>)

decode :: (Monoid w, IsSDoc w) => (TyCon, TyCon) -> Ct -> Try w w (Exp Var 'Boolean)
decode occ = uncurry (decodeTs occ) <=< unNomEq

decodeTs :: (Monoid w, IsSDoc w) => (TyCon, TyCon) -> Type -> Type -> Try w w (Exp Var 'Boolean)
decodeTs _ (TyVarTy l) (TyVarTy r) = pure $ Var l :== Var r
decodeTs occ l r = (:==) <$> exBool occ l <*> exBool occ r <|> (:==) <$> exNum l <*> exNum r

---------------------------------------------------------------------------
-- BOOLEAN AND NUMBER
---------------------------------------------------------------------------

exBool :: (Monoid s, IsSDoc e) => (TyCon, TyCon) -> Type -> Try e s (Exp Var 'Boolean)
exBool _ (TyVarTy v) = pure $ Var v
exBool _ (TyConApp tc [])
	| tc == promotedFalseDataCon = pure $ Bool False
	| tc == promotedTrueDataCon = pure $ Bool True
-- exBool (TyConApp tc [l, r])
--	| tc == typeNatLeqTyCon = (:<=) <$> exNum l <*> exNum r
exBool (oc, cmp) (TyConApp tc [_,
		TyConApp cmpNatTc [l, r],
		TyConApp t1 [], TyConApp t2 [], TyConApp f1 []])
	| tc == oc, cmpNatTc == typeNatCmpTyCon
	, t1 == promotedTrueDataCon, t2 == promotedTrueDataCon
	, f1 == promotedFalseDataCon = (:<=) <$> exNum l <*> exNum r
exBool _ t = throw . fromSDoc $ text "exBool: not boolean:" <+> ppr t

exNum :: (Monoid s, IsSDoc e) => Type -> Try e s (Exp Var 'Number)
exNum (TyVarTy v) = pure $ Var v
exNum (LitTy (NumTyLit n)) = pure $ Const n
exNum (TyConApp tc [l, r])
	| tc == typeNatAddTyCon = (:+) <$> exNum l <*> exNum r
	| tc == typeNatSubTyCon = (:-) <$> exNum l <*> exNum r
exNum t = throw . fromSDoc $ text "exNum: not number:" <+> ppr t

lookupOrdCondCompare :: TcPluginM (TyCon, TyCon)
lookupOrdCondCompare = do
	md2 <- lookupModule ordModule basePackage
	(,) <$> look md2 "OrdCond" <*> look md2 "Compare"

ordModule :: ModuleName
ordModule = mkModuleName "GHC.Internal.Data.Type.Ord"

basePackage :: FastString
basePackage = fsLit "base"

look :: Module -> String -> TcPluginM TyCon
look md s = tcLookupTyCon =<< lookupName md (mkTcOcc s)
