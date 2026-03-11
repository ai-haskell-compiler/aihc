{- |
Copyright   :  (c) Henning Thielemann 2007

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98
-}
module Data.EventList.Absolute.TimeMixed
   (snocBody, snocTime, -- (/.), (./),
    viewTimeR,   viewBodyR,
    switchTimeR, switchBodyR,
    mapTimeInit,
    ) where

import Data.EventList.Absolute.TimeTimePrivate
