{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      TextShow.System.Console.Haskeline
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for data types in the @haskeline@ library.

/Since: 2/
-}
module TextShow.System.Console.Haskeline () where

import System.Console.Haskeline (Interrupt, Prefs)
import System.Console.Haskeline.Completion (Completion)
import System.Console.Haskeline.History (History, historyLines)

import TextShow (TextShow(..), FromStringShow(..))
import TextShow.TH (deriveTextShow)
import TextShow.Utils (showbUnaryListWith)

-- | /Since: 2/
$(deriveTextShow ''Interrupt)

-- | /Since: 2/
instance TextShow Prefs where
    showbPrec p = showbPrec p . FromStringShow
    {-# INLINE showbPrec #-}

-- | /Since: 2/
$(deriveTextShow ''Completion)

-- | /Since: 2/
instance TextShow History where
    showb = showbUnaryListWith showb 0 . historyLines
    {-# INLINE showb #-}
