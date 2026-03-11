{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Language.Lua.Utils where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Data (Data,Typeable)

data NumberType = IntNum | FloatNum
                  deriving (Show,Eq,Data,Typeable,Generic)

instance NFData NumberType

