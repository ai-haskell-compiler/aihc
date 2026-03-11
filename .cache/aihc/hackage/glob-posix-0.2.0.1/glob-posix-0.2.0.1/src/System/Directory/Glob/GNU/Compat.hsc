{-|
Module      : System.Directory.Glob.GNU.Compat
Copyright   : (c) Reuben D'Netto 2016
License     : Apache 2.0
Maintainer  : rdnetto@gmail.com

On non-GNU platforms, this module exports values for 'GlobFlag' which will throw an exception on use.
They can be used to defer the failure to runtime, when you wish to avoid adding @#ifdef@ checks to your code.
-}
module System.Directory.Glob.GNU.Compat where

#ifndef linux_HOST_OS
import System.Directory.Glob.Internal (GlobFlag)

globBrace, globNoMagic, globOnlyDir, globPeriod, globTilde, globTildeCheck :: GlobFlag
globBrace      = error "Unsupported: GLOB_BRACE is a GNU extension"
globNoMagic    = error "Unsupported: GLOB_NOMAGIC is a GNU extension"
globOnlyDir    = error "Unsupported: GLOB_ONLYDIR is a GNU extension"
globPeriod     = error "Unsupported: GLOB_PERIOD is a GNU extension"
globTilde      = error "Unsupported: GLOB_TILDE is a GNU extension"
globTildeCheck = error "Unsupported: GLOB_TILDE_CHECK is a GNU extension"
#endif

