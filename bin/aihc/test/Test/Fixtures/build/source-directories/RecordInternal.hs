module RecordInternal where

data Settings = Settings {width :: Int, wrap :: Bool}

defaultSettings :: Settings
defaultSettings = Settings 80 True
