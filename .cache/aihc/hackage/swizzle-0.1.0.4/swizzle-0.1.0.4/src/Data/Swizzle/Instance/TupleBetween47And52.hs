{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Data.Swizzle.Instance.TupleBetween47And52 () where

import Data.Swizzle.Class.Base
import Data.Swizzle.Class.TH

concat <$> instanceSwizzleTuple `mapM` [47 .. 52]
