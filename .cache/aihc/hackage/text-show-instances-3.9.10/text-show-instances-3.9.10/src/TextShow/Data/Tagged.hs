{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}

{-# OPTIONS -Wno-orphans #-}
{-|
Module:      TextShow.Data.Tagged
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for 'Tagged'.

/Since: 2/
-}
module TextShow.Data.Tagged () where

import Data.Tagged (Tagged(..))
import TextShow (TextShow(..), TextShow1(..), TextShow2(..),
                 Builder, showbPrec1, showbUnaryWith)

-- | Convert a 'Tagged' value to a 'Builder' with the given show function and precedence.
liftShowbTaggedPrec :: (Int -> b -> Builder) -> Int -> Tagged s b -> Builder
liftShowbTaggedPrec sp p (Tagged b) = showbUnaryWith sp "Tagged" p b
{-# INLINE liftShowbTaggedPrec #-}

-- | /Since: 2/
instance TextShow b => TextShow (Tagged s b) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow1 (Tagged s) where
    liftShowbPrec sp _ = liftShowbTaggedPrec sp
    {-# INLINE liftShowbPrec #-}

-- | /Since: 2/
instance TextShow2 Tagged where
    liftShowbPrec2 _ _ sp _ = liftShowbTaggedPrec sp
    {-# INLINE liftShowbPrec2 #-}
