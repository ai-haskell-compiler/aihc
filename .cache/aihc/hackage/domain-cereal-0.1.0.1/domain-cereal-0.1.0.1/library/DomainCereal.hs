module DomainCereal where

import DomainCereal.Prelude
import qualified DomainCereal.TH as TH
import qualified DomainCore.Deriver as Deriver

serializeDeriver :: Deriver.Deriver
serializeDeriver =
  Deriver.effectless (pure . TH.serializeInstanceD)
