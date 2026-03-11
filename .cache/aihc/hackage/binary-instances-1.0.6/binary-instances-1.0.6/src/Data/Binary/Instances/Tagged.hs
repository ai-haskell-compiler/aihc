{-# LANGUAGE CPP       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif
module Data.Binary.Instances.Tagged where

import Data.Binary         (Binary, get, put)
import Data.Binary.Orphans ()

import qualified Data.Tagged as Tagged

instance Binary b => Binary (Tagged.Tagged s b) where
    put = put . Tagged.unTagged
    get = fmap Tagged.Tagged get
