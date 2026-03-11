{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wno-orphans #-}
{-|
Module:      TextShow.Data.Containers
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for data types in the @containers@ library.

/Since: 2/
-}
module TextShow.Data.Containers () where

import qualified Data.Foldable as F
import           Data.Graph (SCC)
import qualified Data.IntMap as IM
import           Data.IntMap (IntMap)
import qualified Data.IntSet as IS
import           Data.IntSet (IntSet)
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Sequence (Seq, ViewL, ViewR)
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Tree (Tree)

import           TextShow (TextShow(..), TextShow1(..), TextShow2(..), showbPrec1)
import           TextShow.Data.Integral ()
import           TextShow.TH (deriveTextShow, deriveTextShow1)
import           TextShow.Utils (showbUnaryListWith)

-- | /Since: 2/
instance TextShow v => TextShow (IntMap v) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow1 IntMap where
    liftShowbPrec sp _ p =
        showbUnaryListWith (liftShowbList2 showbPrec      undefined
                                           (const (sp 0)) undefined) p . IM.toList
    {-# INLINE liftShowbPrec #-}

-- | /Since: 2/
instance TextShow IntSet where
    showbPrec p = showbUnaryListWith showbList p . IS.toList
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance (TextShow k, TextShow v) => TextShow (Map k v) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow k => TextShow1 (Map k) where
    liftShowbPrec = liftShowbPrec2 showbPrec showbList
    {-# INLINE liftShowbPrec #-}

-- | /Since: 2/
instance TextShow2 Map where
    liftShowbPrec2 sp1 _ sp2 _ p =
        showbUnaryListWith (liftShowbList2 (const (sp1 0)) undefined
                                           (const (sp2 0)) undefined) p . M.toList
    {-# INLINE liftShowbPrec2 #-}

-- | /Since: 2/
instance TextShow a => TextShow (Seq a) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow1 Seq where
    liftShowbPrec _ sl p = showbUnaryListWith sl p . F.toList
    {-# INLINE liftShowbPrec #-}

-- | /Since: 2/
$(deriveTextShow  ''ViewL)
-- | /Since: 2/
$(deriveTextShow1 ''ViewL)

-- | /Since: 2/
$(deriveTextShow  ''ViewR)
-- | /Since: 2/
$(deriveTextShow1 ''ViewR)

-- | /Since: 2/
instance TextShow a => TextShow (Set a) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow1 Set where
    liftShowbPrec _ sl p = showbUnaryListWith sl p . Set.toList
    {-# INLINE liftShowbPrec #-}

-- | /Since: 3.6/
$(deriveTextShow  ''SCC)
-- | /Since: 3.6/
$(deriveTextShow1 ''SCC)

-- | /Since: 2/
$(deriveTextShow  ''Tree)
-- | /Since: 2/
$(deriveTextShow1 ''Tree)
