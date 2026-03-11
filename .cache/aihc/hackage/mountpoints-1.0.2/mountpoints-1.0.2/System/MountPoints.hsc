-- | 
-- Copyright: 2012 Joey Hess <id@joeyh.name>
-- License: LGPL 2.1 or higher
-- 
-- Derived from hsshellscript, originally written by
-- Volker Wysk <hsss@volker-wysk.de>

{-# LANGUAGE ForeignFunctionInterface, CPP #-}

module System.MountPoints (
	Mntent(..),
	getMounts,
	getProcMounts,
) where

#include "libmounts.h"

import Control.Monad
import Control.Exception
import Data.Maybe
import Control.Applicative
import Foreign
import Foreign.C
import Prelude

-- | This is a stripped down mntent, containing only fields available
-- everywhere.
data Mntent = Mntent
	{ mnt_fsname :: String -- ^ what's mounted
	, mnt_dir :: FilePath  -- ^ where it's mounted
	, mnt_type :: String   -- ^ what sort of filesystem is mounted
	} deriving (Show, Eq, Ord)

-- | Get currently mounted filesystems.
--
-- This uses eiher getmntent or getmntinfo, depending on the OS.
getMounts :: IO [Mntent]
#ifndef linux_android_HOST_OS
getMounts = do
	h <- c_mounts_start
	when (h == nullPtr) $
		throwErrno "getMounts"
	mntent <- getmntent h []
	_ <- c_mounts_end h
	return mntent
  where
	getmntent h c = do
		ptr <- c_mounts_next h
		if ptr == nullPtr
			then return (reverse c)
			else do
				mnt_fsname_str <- #{peek struct mntent, mnt_fsname} ptr >>= peekCString
				mnt_dir_str <- #{peek struct mntent, mnt_dir} ptr >>= peekCString
				mnt_type_str <- #{peek struct mntent, mnt_type} ptr >>= peekCString
				let ent = Mntent
					{ mnt_fsname = mnt_fsname_str
					, mnt_dir = mnt_dir_str
					, mnt_type = mnt_type_str
					}
				getmntent h (ent:c)
#else
getMounts = getProcMounts
#endif

#ifndef linux_android_HOST_OS
-- Using unsafe imports because the C functions are belived to never block.
-- Note that getmntinfo is called with MNT_NOWAIT to avoid possibly blocking;
-- while getmntent only accesses a file in /etc (or /proc) that should not
-- block.
foreign import ccall unsafe "libmounts.h mounts_start" c_mounts_start
        :: IO (Ptr ())
foreign import ccall unsafe "libmounts.h mounts_next" c_mounts_next
        :: Ptr () -> IO (Ptr ())
foreign import ccall unsafe "libmounts.h mounts_end" c_mounts_end
        :: Ptr () -> IO CInt
#endif

-- | Read </proc/mounts> to get currently mounted filesystems.
-- 
-- This works on Linux and related systems, including Android. 

-- Note that on Android, `getMounts` calls this function.
getProcMounts :: IO [Mntent]
getProcMounts = do
	v <- try go :: IO (Either SomeException [Mntent])
	return (either (const []) id v)
  where
	go = mapMaybe (parse . words) . lines <$> readFile "/proc/mounts"
  	parse (device:mountpoint:fstype:_rest) = Just $ Mntent
		{ mnt_fsname = device
		, mnt_dir = mountpoint
		, mnt_type = fstype
		}
	parse _ = Nothing
