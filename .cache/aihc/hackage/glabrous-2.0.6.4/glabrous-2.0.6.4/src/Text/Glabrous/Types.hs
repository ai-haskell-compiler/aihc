{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Glabrous.Types where

import           Control.Arrow       (second)
import           Data.Aeson
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap   as KM
#endif
import qualified Data.HashMap.Strict as H
import           Data.Serialize
import           Data.Serialize.Text ()
import qualified Data.Text           as T
import           GHC.Generics

data Token
  = Tag !T.Text
  | Literal !T.Text
  deriving (Eq, Show, Generic)

instance Serialize Token

newtype Template =
  Template
    { content :: [Token] }
    deriving (Eq, Show, Generic)

instance Serialize Template

newtype Context =
  Context
    { variables :: H.HashMap T.Text T.Text }
    deriving (Eq, Show)

instance ToJSON Context where
  toJSON (Context h) =
#if MIN_VERSION_aeson(2,0,0)
    object (second String <$> KM.toList (KM.fromHashMapText h))
#else
    object (second String <$> H.toList h)
#endif

instance FromJSON Context where
  parseJSON (Object o) = do
#if MIN_VERSION_aeson(2,0,0)
    let t = KM.toHashMapText o
#else
    let t = o
#endif
    pure Context { variables = H.fromList (fromJSONString <$> H.toList t) }
    where
      fromJSONString (k,String v) = (k,v)
      fromJSONString _            = error "Expected a JSON String in second element of pair"
  parseJSON _          = fail "Expected a JSON object"

data Result
  = Final !T.Text
  | Partial { template :: !Template, context :: !Context }
  deriving (Eq, Show)

