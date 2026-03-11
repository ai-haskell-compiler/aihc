module PureSAT.LBool (
    LBool (..),
) where

-------------------------------------------------------------------------------
-- LBool
-------------------------------------------------------------------------------

data LBool = LFalse | LTrue | LUndef
  deriving (Eq, Ord, Show)
