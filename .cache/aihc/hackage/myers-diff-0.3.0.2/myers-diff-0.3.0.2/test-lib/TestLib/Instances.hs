{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestLib.Instances () where

import Control.DeepSeq
import Data.Diff.Types
import GHC.Generics


deriving instance Generic Position
deriving instance NFData Position

deriving instance Generic Range
deriving instance NFData Range

deriving instance Generic ChangeEvent
deriving instance NFData ChangeEvent

deriving instance Generic Edit
deriving instance NFData Edit
