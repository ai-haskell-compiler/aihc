{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.MessagePack.Types.Instances () where

import           Data.Void                      (Void)

import           Data.MessagePack.Types.Class   (MessagePack)
import           Data.MessagePack.Types.Generic ()


instance MessagePack a => MessagePack (Maybe a)

instance (MessagePack a, MessagePack b) => MessagePack (Either a b)

instance MessagePack Void
