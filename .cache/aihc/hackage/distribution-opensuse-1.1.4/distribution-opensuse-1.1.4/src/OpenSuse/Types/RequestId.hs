{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module OpenSuse.Types.RequestId
  ( RequestId, mkRequestId, unRequestId
  , ReleaseRequestId, MaintenanceRequestId
  )
  where

import OpenSuse.Prelude

-- An OBS request identifier that is, essentially, a natural number.
newtype RequestId = RequestId Natural
  deriving (Show, Eq, Ord, Enum, Generic, Hashable, Binary, NFData)

-- | Constructor function for typed request identifiers.
mkRequestId :: Natural -> RequestId
mkRequestId = RequestId

-- | Accessor function for the underlying natural number.
unRequestId :: RequestId -> Natural
unRequestId (RequestId n) = n

-- | Type synonym for convenience.
type MaintenanceRequestId = RequestId

-- | Type synonym for convenience.
type ReleaseRequestId = RequestId

instance FromJSON RequestId
instance ToJSON RequestId

instance IsString RequestId where
  fromString = parse "request id"

instance Pretty RequestId where
  pPrint = pPrint . unRequestId

instance HasParser RequestId where
  parser = mkRequestId <$> parser
