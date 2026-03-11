{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Data.Swizzle.Instance.Tuple58OrMore () where

import GHC.Exts
import Data.Swizzle.Class.Base
import Data.Swizzle.Class.TH

concat <$> instanceSwizzleTuple `mapM` [58 .. maxTupleSize]
