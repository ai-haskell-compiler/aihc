{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|

This module provides a 'JSONStyle' Named style that can be used for JSON
encoding/decoding.  It also provides conversion to and from that style from the
regular 'UTF8' style, as well as an "aeson" 'ToJSON' and 'FromJSON' instance.

-}

module Data.Name.JSON where

import Data.Aeson
import Data.Aeson.Types
import Data.Functor.Contravariant ( (>$<) )
import Data.Hashable ( Hashable )
import Data.Name
import Data.Name.Internal
import Data.String ( IsString(fromString) )


-- | The JSONStyle of Named objects can be directly transformed to and from JSON
-- (via Aeson's ToJSON and FromJSON classes).  The Named nameOf is not
-- represented in the JSON form; field names are expected to be provided by the
-- Named field name itself.  Bi-directional conversions between the JSON style
-- and the UTF8 style is automatic.

type JSONStyle = "JSON" :: NameStyle

instance NameText JSONStyle

-- JSON names have no special considerations, so standard instances are
-- sufficient:

deriving instance Eq (Named JSONStyle nameOf)
deriving instance Ord (Named JSONStyle nameOf)
deriving instance Hashable (Named JSONStyle nameOf)

instance ConvertNameStyle JSONStyle UTF8 nameOf
instance ConvertNameStyle UTF8 JSONStyle nameOf

instance ConvertNameStyle JSONStyle CaseInsensitive nameOf
instance ConvertNameStyle CaseInsensitive JSONStyle nameOf


-- -- The generic instance results in an object: { "name": "..." } This
-- -- instance declaration avoids that and causes the JSON form to be a simple
-- -- string.  Currently there's no FromJSON, although it's likely the generic
-- -- instance would successfully work under OverloadedStrings
instance ToJSON (Named JSONStyle nameTy) where
  toJSON = toJSON . nameText

instance ToJSONKey (Named JSONStyle nameTy) where
  toJSONKey = toJSONKeyText nameText

instance FromJSON (Named JSONStyle nameTy) where
  parseJSON j = fromString <$> parseJSON j

instance FromJSONKey (Named JSONStyle nameTy) where
  fromJSONKey = FromJSONKeyText fromText


instance ToJSON (Name nameTy) where
  toJSON = toJSON . convertStyle @UTF8 @JSONStyle

instance ToJSONKey (Name nameTy) where
  toJSONKey = convertStyle @UTF8 @JSONStyle >$< toJSONKey

instance FromJSON (Name nameTy) where
  parseJSON j = convertStyle @JSONStyle @UTF8 . fromString <$> parseJSON j

instance FromJSONKey (Name nameTy) where
  fromJSONKey = convertStyle @JSONStyle @UTF8 <$> fromJSONKey


instance ToJSON (Named CaseInsensitive nameTy) where
  toJSON = toJSON . convertStyle @CaseInsensitive @JSONStyle

instance ToJSONKey (Named CaseInsensitive nameTy) where
  toJSONKey = convertStyle @CaseInsensitive @JSONStyle >$< toJSONKey

instance FromJSON (Named CaseInsensitive nameTy) where
  parseJSON j = convertStyle @JSONStyle @CaseInsensitive . fromString <$> parseJSON j

instance FromJSONKey (Named CaseInsensitive nameTy) where
  fromJSONKey = convertStyle @JSONStyle @CaseInsensitive <$> fromJSONKey
