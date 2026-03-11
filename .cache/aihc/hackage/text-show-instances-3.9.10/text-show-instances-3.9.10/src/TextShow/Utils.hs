{-# LANGUAGE OverloadedStrings #-}

{-|
Module:      TextShow.Utils
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Miscellaneous utility functions.
-}
module TextShow.Utils (liftShowbUnaryWith, showbUnaryListWith) where

import TextShow (Builder, TextShow1(..), showbUnaryWith)

-- | This pattern is used frequently when showing transformer types.
liftShowbUnaryWith :: TextShow1 m
                   => (Int -> a -> Builder) -> ([a] -> Builder)
                   -> Builder -> Int -> m a -> Builder
liftShowbUnaryWith sp sl = showbUnaryWith (liftShowbPrec sp sl)
{-# INLINE liftShowbUnaryWith #-}

-- | This pattern is used frequently when showing container types.
showbUnaryListWith :: ([a] -> Builder) -> Int -> [a] -> Builder
showbUnaryListWith sl p = showbUnaryWith (const sl) "fromList" p
{-# INLINE showbUnaryListWith #-}
