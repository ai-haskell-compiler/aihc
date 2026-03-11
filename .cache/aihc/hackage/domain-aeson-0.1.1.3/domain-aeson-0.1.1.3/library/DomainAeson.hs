module DomainAeson where

import DomainAeson.Prelude
import qualified DomainAeson.TH as TH
import qualified DomainCore.Deriver as Deriver

toJsonDeriver :: Deriver.Deriver
toJsonDeriver =
  Deriver.effectless (pure . TH.toJsonDec)

fromJsonDeriver :: Deriver.Deriver
fromJsonDeriver =
  Deriver.effectless (pure . TH.fromJsonDec)
