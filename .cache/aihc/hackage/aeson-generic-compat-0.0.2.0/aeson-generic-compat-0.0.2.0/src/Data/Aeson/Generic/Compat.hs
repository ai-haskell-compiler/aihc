{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Aeson.Generic.Compat (
  objectKeys,
  GFromJSON0, GToJSON0,
  ) where

import Data.Text (Text)

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
#else
import qualified Data.HashMap.Strict as HashMap
#endif

import Data.Aeson (Object, GFromJSON, GToJSON)
#if MIN_VERSION_aeson(1,0,0)
import Data.Aeson (Zero)
#endif


#if MIN_VERSION_aeson(1,0,0)
type GFromJSON0  =  GFromJSON Zero
type GToJSON0    =  GToJSON Zero
#else
type GFromJSON0  =  GFromJSON
type GToJSON0    =  GToJSON
#endif

objectKeys :: Object -> [Text]
objectKeys =
#if MIN_VERSION_aeson(2,0,0)
  map Key.toText . KeyMap.keys
#else
  HashMap.keys
#endif
