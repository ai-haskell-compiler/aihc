module Foreign.C.Types
  ( CInt (..),
  )
where

import Data.Int (Int32)

newtype CInt = CInt Int32
