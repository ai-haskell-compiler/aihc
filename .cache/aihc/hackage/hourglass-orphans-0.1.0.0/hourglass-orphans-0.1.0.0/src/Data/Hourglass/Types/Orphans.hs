{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Hourglass.Types.Orphans
  ( 
   -- * Re-exports
   module Data.Hourglass.Types
  ) where

import Data.Hourglass.Types
import Data.Aeson
import Data.Aeson.TH
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
import Data.Monoid (mempty)
#endif

deriveJSON defaultOptions ''Month

deriveJSON defaultOptions ''WeekDay

deriveJSON defaultOptions ''NanoSeconds

deriveJSON defaultOptions ''Seconds

deriveJSON defaultOptions ''Minutes

deriveJSON defaultOptions ''Hours

deriveJSON defaultOptions ''TimezoneOffset

deriveJSON defaultOptions ''Elapsed

instance ToJSON ElapsedP where
  toJSON (ElapsedP e n) =
    object ["seconds" .= toJSON e, "nanoseconds" .= toJSON n]

instance FromJSON ElapsedP where
  parseJSON (Object v) = ElapsedP <$> (v .: "seconds") <*> (v .: "nanoseconds")
  parseJSON _ = mempty

deriveJSON defaultOptions ''Date

deriveJSON defaultOptions ''TimeOfDay

deriveJSON defaultOptions ''DateTime
