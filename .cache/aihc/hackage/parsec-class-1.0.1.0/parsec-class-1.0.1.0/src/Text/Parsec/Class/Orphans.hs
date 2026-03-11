{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Parsec.Class.Orphans where

import Control.Exception
import Text.Parsec.Error

instance Exception ParseError
