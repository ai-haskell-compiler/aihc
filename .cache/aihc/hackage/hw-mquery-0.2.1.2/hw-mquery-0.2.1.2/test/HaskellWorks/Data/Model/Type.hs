module HaskellWorks.Data.Model.Type where

data Storage
  = StorageDisk
    { storageDevice :: Maybe String
    , storageLabel  :: Maybe String
    }
  | StorageTmpFs
    { storageSizeMB :: Int
    }
  | StorageNfs
    { storageServer     :: String
    , storageRemotePath :: String
    } deriving (Eq, Show)

-- storageDisk :: Storage
-- storageDisk = Storage
--   { storageDevice = Nothing
--   , storageLabel  = Nothing
--   }

-- storageTmpFs :: Int -> StorageTmpFs
-- storageTmpFs = StorageTmpFs

-- storageNfs :: String -> String -> Storage
-- storageNfs server remotePath = StorageNfs
--   { storageServer     = server
--   , storageRemotePath = remotePath
--   }

data Mount = Mount
  { mountPoint    :: String
  , mountStorage  :: Storage
  , mountFsType   :: String
  , mountReadOnly :: Bool
  , mountOptions  :: [String]
  } deriving (Eq, Show)

-- mount :: String -> Storage -> String -> Mount
-- mount point storage fsType = MountPoint
--   { mountPoint    = point
--   , mountStorage  = storage
--   , mountFsType   = fsType
--   , mountReadOnly = False
--   , mountOptions  = []
--   }
