module DomainOptics where

import qualified DomainCore.Deriver as Deriver
import qualified DomainOptics.InstanceDecs as InstanceDecs

-- |
-- Generates 'LabelOptic' instances for enums, products and sums,
-- automatically choosing the appropriate optic type.
--
-- Requires to have the @UndecidableInstances@ extension enabled.
labelOpticDeriver :: Deriver.Deriver
labelOpticDeriver =
  Deriver.effectless InstanceDecs.labelOptic
