{-# LANGUAGE CPP             #-}

#if !defined(mingw32_HOST_OS)
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#endif

{-|
Module:      TextShow.System.Console.Terminfo
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for data types in the @terminfo@ library.
Only provided if using a Unix-like operating system (i.e., not Windows).

/Since: 2/
-}
module TextShow.System.Console.Terminfo () where

#if !defined(mingw32_HOST_OS)
import System.Console.Terminfo.Base (SetupTermError)
import System.Console.Terminfo.Color (Color)

import TextShow (TextShow(..), FromStringShow(..))
import TextShow.TH (deriveTextShow)

-- | /Since: 2/
$(deriveTextShow ''Color)

-- | /Since: 2/
instance TextShow SetupTermError where
    showb = showb . FromStringShow
    {-# INLINE showb #-}
#endif
