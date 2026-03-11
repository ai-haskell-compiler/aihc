{-# LANGUAGE CPP             #-}

#if !defined(mingw32_HOST_OS)
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#endif

{-|
Module:      TextShow.System.Posix
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for data types in the @unix@ library.
Only provided if using a Unix-like operating system (i.e., not Windows).

/Since: 2/
-}
module TextShow.System.Posix () where

#if !defined(mingw32_HOST_OS)
import System.Posix.DynamicLinker (RTLDFlags, DL)
import System.Posix.Process (ProcessStatus)
# if MIN_VERSION_unix(2,8,0)
import System.Posix.User.ByteString
# else
import System.Posix.User
# endif
  (GroupEntry, UserEntry)

import TextShow.TH (deriveTextShow)

-- | /Since: 2/
$(deriveTextShow ''RTLDFlags)
-- | /Since: 2/
$(deriveTextShow ''DL)
-- | /Since: 2/
$(deriveTextShow ''ProcessStatus)
-- | /Since: 2/
$(deriveTextShow ''GroupEntry)
-- | /Since: 2/
$(deriveTextShow ''UserEntry)
#endif
