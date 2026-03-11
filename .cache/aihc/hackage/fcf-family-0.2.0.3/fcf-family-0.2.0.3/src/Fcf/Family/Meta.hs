{-# LANGUAGE DataKinds, PolyKinds, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | 'Eval' and friends as part of the family of families.
module Fcf.Family.Meta () where

import Fcf.Family
import Fcf.Family.TH (fcfify)

fcfify ''Eval
fcfify ''Params
-- fcfify ''Args
-- fcfify ''Res
