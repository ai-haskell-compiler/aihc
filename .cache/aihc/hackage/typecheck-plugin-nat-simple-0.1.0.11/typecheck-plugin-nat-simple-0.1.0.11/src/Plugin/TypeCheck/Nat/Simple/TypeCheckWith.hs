{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple.TypeCheckWith (
	-- * TYPE CHECK WITH
	typeCheckWith ) where

import GHC.Plugins (
	Plugin(..), defaultPlugin, Expr(..), mkUnivCo,
	Outputable, ppr, text, (<+>) )
import GHC.Tc.Plugin (TcPluginM, tcPluginTrace)
import GHC.Tc.Types (TcPlugin(..), TcPluginSolveResult(..))
import GHC.Tc.Types.Constraint (Ct)
import GHC.Tc.Types.Evidence (EvTerm(..), EvBindsVar)
import GHC.Core.TyCo.Rep (UnivCoProvenance(..))
import Control.Monad.Try (Try, gatherSuccess, throw, Set)
import Data.Bool (bool)
import Data.Log (IsSDoc, fromSDoc)
import Plugin.TypeCheck.Nat.Simple.UnNomEq (unNomEq)

import Plugin.TypeCheck.Nat.Simple.Decode (lookupOrdCondCompare)
import GHC.Core.TyCon
import GHC.Types.Unique.FM

---------------------------------------------------------------------------

typeCheckWith :: (Monoid w, Outputable w, IsSDoc w, Set w w) =>
	String -> ((TyCon, TyCon) -> [Ct] -> [Ct] -> Ct -> Try w w Bool) -> Plugin
typeCheckWith hd ck = defaultPlugin { tcPlugin = const $ Just TcPlugin {
	tcPluginInit = pure (),
	tcPluginSolve = const $ solve hd ck,
	tcPluginRewrite = const emptyUFM,
	tcPluginStop = const $ pure () } }

solve :: (Monoid w, Outputable w, IsSDoc w, Set w w) =>
	String -> ((TyCon, TyCon) -> [Ct] -> [Ct] -> Ct -> Try w w Bool) ->
	EvBindsVar -> [Ct] -> [Ct] -> TcPluginM TcPluginSolveResult
solve hd ck _s gs ws = do
	occ <- lookupOrdCondCompare
	let	(rs, lgs) = gatherSuccess $ result hd (ck occ) gs [] <$> ws
	tcPluginTrace hd $ "given:" <+> ppr gs
	tcPluginTrace hd $ "wanted:" <+> ppr ws
	TcPluginOk rs [] <$ tcPluginTrace hd (ppr lgs)

result :: (Monoid s, IsSDoc e) =>
	String -> ([Ct] -> [Ct] -> Ct -> Try e s Bool) ->
	[Ct] -> [Ct] -> Ct -> Try e s (EvTerm, Ct)
result hd ck gs ds w = unNomEq w >>= \(l, r) ->
	bool (throw em) (pure (et l r, w)) =<< ck gs ds w
	where
	em = fromSDoc $ text "result: type checker: return False"
	et = ((EvExpr . Coercion) .) . mkUnivCo (PluginProv hd) Nominal
