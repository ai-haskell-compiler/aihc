{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-orphans               #-}
{-|
Module:      Instances.Control.Monad.Trans
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for monad transformers.
-}
module Instances.Control.Monad.Trans () where

import           Control.Monad.Trans.Except              (ExceptT(..))
import           Control.Monad.Trans.Identity            (IdentityT(..))
import qualified Control.Monad.Trans.Writer.Lazy   as WL (WriterT(..))
import qualified Control.Monad.Trans.Writer.Strict as WS (WriterT(..))

import           Test.QuickCheck (Arbitrary)

deriving instance Arbitrary (m (Either e a)) => Arbitrary (ExceptT e m a)
deriving instance Arbitrary (f a)            => Arbitrary (IdentityT f a)
deriving instance Arbitrary (m (a, w))       => Arbitrary (WL.WriterT w m a)
deriving instance Arbitrary (m (a, w))       => Arbitrary (WS.WriterT w m a)
