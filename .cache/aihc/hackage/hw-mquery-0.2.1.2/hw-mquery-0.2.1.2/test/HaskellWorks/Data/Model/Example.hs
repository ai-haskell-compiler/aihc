module HaskellWorks.Data.Model.Example where

import HaskellWorks.Data.Model.Type

exampleMounts :: [Mount]
exampleMounts =
  [ Mount
    { mountPoint    = "/"
    , mountStorage  = StorageDisk
      { storageDevice = Just "/dev/sda1"
      , storageLabel  = Nothing
      }
    , mountFsType   = "btrfs"
    , mountReadOnly = True
    , mountOptions  = []
    }
  , Mount
    { mountPoint = "/var"
    , mountStorage = StorageDisk
      { storageLabel  = Just "8f3ba6f4-5c70-46ec-83af-0d5434953e5f"
      , storageDevice = Nothing
      }
    , mountFsType   = "ext4"
    , mountReadOnly = False
    , mountOptions  = ["nosuid", "noauto"]
    }
  , Mount
    { mountPoint    = ""
    , mountStorage = StorageTmpFs
      { storageSizeMB = 64
      }
    , mountFsType   = ""
    , mountReadOnly = False
    , mountOptions  = []
    }
  , Mount
    { mountPoint    = "/mypath"
    , mountStorage = StorageNfs
      { storageServer     = "my.nfs.server"
      , storageRemotePath = "/exports/mypath"
      }
    , mountFsType   = ""
    , mountReadOnly = False
    , mountOptions  = ["noexec"]
    }
  ]
