{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple.UnNomEq (unNomEq) where

import GHC.Tc.Types.Constraint (Ct, ctEvidence, ctEvPred)
-- import GHC.Core.Type (Type)
import GHC.Core.Predicate(Pred(..), EqRel(..), classifyPredType)
import GHC.Utils.Outputable (ppr, text, (<+>))
import Control.Monad.Try (Try, throw)
import Data.Log (IsSDoc, fromSDoc)

import GHC.Core.TyCo.Rep (Type(..))
import GHC.Core.TyCon

import GHC.Types.Name
import GHC.Builtin.Types

unNomEq :: (Monoid w, IsSDoc e) => Ct -> Try e w (Type, Type)
unNomEq ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq l r -> pure (l, r)
	IrredPred pt -> case pt of
		TyConApp (nameStableString . tyConName ->
				"$ghc-internal$GHC.Internal.TypeError$Assert") [c, _e] ->
			pure (c, TyConApp promotedTrueDataCon [])
		_ -> throw . fromSDoc $ text "unNomEq: IrredPred" <+> ppr pt
	_ -> throw . fromSDoc
		$ text "unNomEq: no match to EqPred NomEq l r:" <+> ppr ct
