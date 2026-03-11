{-# LANGUAGE CPP #-}
{- |
Module      :  System.FilePath.ByteString
Copyright   :  (c) Neil Mitchell 2005-2014, (c) Joey Hess 2019
License     :  BSD3

Maintainer  :  id@joeyh.name
Stability   :  stable
Portability :  portable

A library for 'RawFilePath' manipulations, using Posix or Windows filepaths
depending on the platform.

Both "System.FilePath.Posix.ByteString"
and "System.FilePath.Windows.ByteString" provide the
same interface. See either for examples and a list of the available
functions.
-}


#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
module System.FilePath.ByteString(module System.FilePath.Windows.ByteString) where
import System.FilePath.Windows.ByteString
#else
module System.FilePath.ByteString(module System.FilePath.Posix.ByteString) where
import System.FilePath.Posix.ByteString
#endif
