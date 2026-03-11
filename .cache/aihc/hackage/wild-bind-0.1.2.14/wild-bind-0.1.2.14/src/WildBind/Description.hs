-- |
-- Module: WildBind.Description
-- Description: Types about ActionDescription
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
module WildBind.Description
    ( ActionDescription
    , Describable (..)
    ) where

import           Data.Text (Text)

-- | Human-readable description of an action. 'ActionDescription' is
-- used to describe the current binding to the user.
type ActionDescription = Text

-- | Class for something describable.
class Describable d where
  describe :: d -> ActionDescription

-- | @since 0.1.1.0
instance (Describable a, Describable b) => Describable (Either a b) where
  describe = either describe describe
