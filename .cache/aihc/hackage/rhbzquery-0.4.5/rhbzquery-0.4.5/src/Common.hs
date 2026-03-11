{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: GPL-2.0-or-later

module Common (error') where

#if MIN_VERSION_simple_cmd(0,1,4)
import SimpleCmd (error')
#else
error' :: String -> a
#if MIN_VERSION_base(4,9,0)
error' = errorWithoutStackTrace
#else
error' = error
#endif
#endif
