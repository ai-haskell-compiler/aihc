module GhcApi.GhcPlugins (module GhcPlugins, TcTyVar, EvTerm(..)) where

import GhcPlugins hiding (mkSubst)
import TcType (TcTyVar)
import TcEvidence (EvTerm(..))
