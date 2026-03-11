{- |
   Module     : Path.Binary
   License    : MIT
   Stability  : experimental

Binary instance for `Path`.
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
module Path.Binary where

import Data.Binary
import Path

deriving instance Binary (Path Rel File)
deriving instance Binary (Path Rel Dir)
deriving instance Binary (Path Abs Dir)
deriving instance Binary (Path Abs File)
