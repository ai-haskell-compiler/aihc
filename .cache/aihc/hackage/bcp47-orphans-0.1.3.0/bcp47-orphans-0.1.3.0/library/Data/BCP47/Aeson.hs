{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.BCP47.Aeson () where

import Autodocodec (Autodocodec (..))
import Data.Aeson
import Data.BCP47
import Data.BCP47.Autodocodec ()

deriving via (Autodocodec BCP47) instance FromJSON BCP47
deriving via (Autodocodec BCP47) instance ToJSON BCP47
