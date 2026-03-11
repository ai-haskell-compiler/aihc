{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      TextShow.Control.Monad.Trans
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for monad transformers.

/Since: 2/
-}
module TextShow.Control.Monad.Trans () where

import           Control.Monad.Trans.Except              (ExceptT(..))
import           Control.Monad.Trans.Identity            (IdentityT(..))
import           Control.Monad.Trans.Maybe               (MaybeT(..))
import qualified Control.Monad.Trans.Writer.Lazy   as WL (WriterT(..))
import qualified Control.Monad.Trans.Writer.Strict as WS (WriterT(..))

import           TextShow (TextShow(..), TextShow1(..), TextShow2(..),
                           Builder, showbPrec1, showbUnaryWith)
import           TextShow.Utils (liftShowbUnaryWith)

liftMShowbUnaryWith :: (TextShow1 m, TextShow1 f)
                    => (Int -> a -> Builder) -> ([a] -> Builder)
                    -> Builder -> Int -> m (f a) -> Builder
liftMShowbUnaryWith sp sl name p m =
    showbUnaryWith (liftShowbPrec (liftShowbPrec sp sl)
                                  (liftShowbList sp sl)) name p m
{-# INLINE liftMShowbUnaryWith #-}

liftShowbWriterTPrec :: (TextShow1 m, TextShow w)
                     => (Int -> a -> Builder) -> ([a] -> Builder)
                     -> Int -> m (a, w) -> Builder
liftShowbWriterTPrec sp sl p m =
    showbUnaryWith (liftShowbPrec (liftShowbPrec2 sp sl showbPrec showbList)
                                  (liftShowbList2 sp sl showbPrec showbList))
                   "WriterT" p m
{-# INLINE liftShowbWriterTPrec #-}

-- | /Since: 2/
instance (TextShow e, TextShow1 m, TextShow a) => TextShow (ExceptT e m a) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance (TextShow e, TextShow1 m) => TextShow1 (ExceptT e m) where
    liftShowbPrec sp sl p (ExceptT m) = liftMShowbUnaryWith sp sl "ExceptT" p m
    {-# INLINE liftShowbPrec #-}

-- | /Since: 2/
instance (TextShow1 f, TextShow a) => TextShow (IdentityT f a) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow1 f => TextShow1 (IdentityT f) where
    liftShowbPrec sp sl p (IdentityT m) = liftShowbUnaryWith sp sl "IdentityT" p m
    {-# INLINE liftShowbPrec #-}

-- | /Since: 2/
instance (TextShow1 m, TextShow a) => TextShow (MaybeT m a) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow1 m => TextShow1 (MaybeT m) where
    liftShowbPrec sp sl p (MaybeT m) = liftMShowbUnaryWith sp sl "MaybeT" p m
    {-# INLINE liftShowbPrec #-}

-- | /Since: 2/
instance (TextShow w, TextShow1 m, TextShow a) => TextShow (WL.WriterT w m a) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance (TextShow w, TextShow1 m) => TextShow1 (WL.WriterT w m) where
    liftShowbPrec sp sl p (WL.WriterT m) = liftShowbWriterTPrec sp sl p m
    {-# INLINE liftShowbPrec #-}

-- | /Since: 2/
instance (TextShow w, TextShow1 m, TextShow a) => TextShow (WS.WriterT w m a) where
    showbPrec = showbPrec1
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance (TextShow w, TextShow1 m) => TextShow1 (WS.WriterT w m) where
    liftShowbPrec sp sl p (WS.WriterT m) = liftShowbWriterTPrec sp sl p m
    {-# INLINE liftShowbPrec #-}
