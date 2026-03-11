{-|
Module      : System.Directory.Glob.GNU
Copyright   : (c) Reuben D'Netto 2016
License     : Apache 2.0
Maintainer  : rdnetto@gmail.com
Portability : GNU

This module exports 'GlobFlag' values which are only supported on platforms using the GNU implementation of glob.
Using them on non-GNU platforms will result in a compile-time failure.
If you wish to defer the failure to run-time, you should also import "System.Directory.Glob.GNU.Compat".
-}
module System.Directory.Glob.GNU
#ifdef linux_HOST_OS
    (
        globBrace,
        globNoMagic,
        globOnlyDir,
        globPeriod,
        globTilde,
        globTildeCheck
    ) where

import System.Directory.Glob.Internal

#else
    () where

#endif
