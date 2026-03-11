{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      TextShow.Trace.Hpc
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for data types in the @hpc@ library.

/Since: 2/
-}
module TextShow.Trace.Hpc () where

import Prelude ()
import Prelude.Compat

import TextShow (TextShow(..), FromStringShow(..), singleton)
import TextShow.Data.Time ()
import TextShow.TH (deriveTextShow)

import Trace.Hpc.Mix (Mix, BoxLabel, CondBox)
import Trace.Hpc.Tix (Tix, TixModule)
import Trace.Hpc.Util (HpcPos, Hash, fromHpcPos)

-- | /Since: 2/
instance TextShow HpcPos where
    showb hp = case fromHpcPos hp of
        (l1, c1, l2, c2) -> showb l1
               <> (singleton ':' <> showb c1)
               <> (singleton '-' <> showb l2)
               <> (singleton ':' <> showb c2)
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow Hash where
    showb = showb . FromStringShow
    {-# INLINE showb #-}

-- | /Since: 2/
$(deriveTextShow ''TixModule)
-- | /Since: 2/
$(deriveTextShow ''CondBox)
-- | /Since: 2/
$(deriveTextShow ''BoxLabel)
-- | /Since: 2/
$(deriveTextShow ''Mix)
-- | /Since: 2/
$(deriveTextShow ''Tix)
