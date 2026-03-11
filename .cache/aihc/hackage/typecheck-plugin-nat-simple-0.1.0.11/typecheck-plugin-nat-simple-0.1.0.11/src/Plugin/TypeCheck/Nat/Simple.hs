{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Plugin.TypeCheck.Nat.Simple (
	-- * PLUGIN
	plugin ) where

import Prelude hiding (log)

import GHC.Plugins (Plugin, Var, ppr)
import Control.Monad.Try (tell)
import Data.Log (Log, (.+.), fromSDoc, SDocStr)
import Data.Derivation.CanDerive (canDerive, givens, wanted)
import Plugin.TypeCheck.Nat.Simple.TypeCheckWith (typeCheckWith)
import Plugin.TypeCheck.Nat.Simple.Decode (decode, decodeAll)

type L = Log SDocStr Var

-- | > type L = Log SDocStr Var
--   >
--   > plugin :: Plugin
--   > plugin = typeCheckWith @L "Plugin.TypeCheck.Nat.Simple" \gs _ w ->
--   >	tell @L $ "givens: " .+. fromSDoc (ppr gs)
--   >	tell @L $ "wanted: " .+. fromSDoc (ppr w)
--   >	uncurry canDerive
--   >		=<< (,) <$> (givens =<< decodeAll gs) <*> (wanted =<< decode w)

plugin :: Plugin
plugin = typeCheckWith @L "Plugin.TypeCheck.Nat.Simple" \occ gs d w -> do
	tell @L $ "givens: " .+. fromSDoc (ppr gs)
	tell @L $ "derived: " .+. fromSDoc (ppr d)
	tell @L $ "wanted: " .+. fromSDoc (ppr w)
	uncurry canDerive
		=<< (,) <$> (givens =<< decodeAll occ gs) <*> (wanted =<< decode occ w)
