{-|
Module      : FRP.Netwire.Input.Util
Description : Utility functions used but not exported by netwire-input.
Copyright   : (c) Bradley Hardy, 2015
License     : MIT
Maintainer  : Krajcevski@gmail.com
Stability   : experimental
Portability : POSIX

This module contains utility functions used but not exported by
'FRP.Netwire.Input'.

-}
module FRP.Netwire.Input.Util where

import Prelude hiding (id, (.))
import Control.Wire

-- | Take an wire that produces an arbitrary event, and change the contents
-- of those events to instead be the same as the input value at the time of
-- the event's occurrence.
eventToId :: Monad m => Wire s e m a (Event b) -> Wire s e m a (Event a)
eventToId wire = uncurry (fmap . const) <$> (id &&& wire)
