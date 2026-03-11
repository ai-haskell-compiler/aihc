{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.GLib.Pointerable (Pointerable(..)) where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

class Pointerable a where
	withPtr :: a -> (Ptr a -> IO b) -> IO b; fromPtr :: Ptr a -> IO a

instance {-# OVERLAPPABLE #-} Storable a => Pointerable a where
	withPtr x f = alloca \p -> poke p x >> f p
	fromPtr = peek
