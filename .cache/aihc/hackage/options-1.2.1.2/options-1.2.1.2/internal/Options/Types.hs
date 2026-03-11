-- |
-- Module: Options.Types
-- License: MIT
module Options.Types
  ( OptionDefinitions (..),
    Group (..),
    OptionKey (..),
    Location (..),
    OptionInfo (..),
  )
where

data OptionDefinitions = OptionDefinitions [OptionInfo] [(String, [OptionInfo])]

data Group = Group
  { groupName :: String,
    -- | A short title for the group, which is used when printing
    -- @--help@ output.
    groupTitle :: String,
    -- | A description of the group, which is used when printing
    -- @--help@ output.
    groupDescription :: String
  }
  deriving (Eq, Show)

data OptionKey
  = OptionKey String
  | OptionKeyHelpSummary
  | OptionKeyHelpGroup String
  | OptionKeyGenerated Integer
  | OptionKeyIgnored
  deriving (Eq, Ord, Show)

data Location = Location
  { locationPackage :: String,
    locationModule :: String,
    locationFilename :: String,
    locationLine :: Integer
  }
  deriving (Eq, Show)

data OptionInfo = OptionInfo
  { optionInfoKey :: OptionKey,
    optionInfoShortFlags :: [Char],
    optionInfoLongFlags :: [String],
    optionInfoDefault :: String,
    optionInfoUnary :: Bool,
    optionInfoUnaryOnly :: Bool, -- used only for --help and friends
    optionInfoDescription :: String,
    optionInfoGroup :: Maybe Group,
    optionInfoLocation :: Maybe Location,
    optionInfoTypeName :: String
  }
  deriving (Eq, Show)
