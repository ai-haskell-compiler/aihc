{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      TextShow.Data.UnorderedContainers
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for 'HashMap' and 'HashSet'.

/Since: 2/
-}
module TextShow.Data.UnorderedContainers () where

import qualified Data.HashMap.Lazy as HM (toList)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashSet as HS (toList)
import           Data.HashSet (HashSet)

import           TextShow (TextShow(..), TextShow1(..), TextShow2(..), showbPrec1)
import           TextShow.Utils (showbUnaryListWith)

-- | /Since: 2/
instance (TextShow k, TextShow v) => TextShow (HashMap k v) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow k => TextShow1 (HashMap k) where
    liftShowbPrec = liftShowbPrec2 showbPrec showbList
    {-# INLINE liftShowbPrec #-}

-- | /Since: 2/
instance TextShow2 HashMap where
    liftShowbPrec2 sp1 _ sp2 _ p =
        showbUnaryListWith (liftShowbList2 (const (sp1 0)) undefined
                                           (const (sp2 0)) undefined) p . HM.toList
    {-# INLINE liftShowbPrec2 #-}

-- | /Since: 2/
instance TextShow a => TextShow (HashSet a) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow1 HashSet where
    liftShowbPrec _ sl p = showbUnaryListWith sl p . HS.toList
    {-# INLINE liftShowbPrec #-}
