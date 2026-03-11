-- |
-- Module: WildBind.FrontEnd
-- Description: Data types and type classes about front-ends.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- Data types and type classes about front-ends.
--
-- You have to look at this module if you want to create a front-end
-- implementation.
module WildBind.FrontEnd
    ( FrontEvent (..)
    , FrontEnd (..)
    ) where

import           WildBind.Description (ActionDescription)

-- | Event from the front-end. @s@ is the state of the front-end. @i@ is the input.
data FrontEvent s i
  = FEInput i -- ^ An event that a new input is made.
  | FEChange s -- ^ An event that the front-end state is changed.
  deriving (Eq, Ord, Show)

-- | Interface to the front-end. @s@ is the state of the front-end,
-- @i@ is the input.
data FrontEnd s i
  = FrontEnd
      { frontDefaultDescription :: i -> ActionDescription
        -- ^ Default 'ActionDescription' for inputs
      , frontSetGrab            :: i -> IO ()
        -- ^ Action to grab (or capture) the specified input symbol on the device.
      , frontUnsetGrab          :: i -> IO ()
        -- ^ Action to release the grab for the input symbol.
      , frontNextEvent          :: IO (FrontEvent s i)
        -- ^ Action to retrieve the next event. It should block if no event is queued.
      }
