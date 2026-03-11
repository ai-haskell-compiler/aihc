module Hwk.Types (cleanupType) where

import qualified Data.List.Extra as L

cleanupType :: String -> String
cleanupType = L.replace "FilePath" "String" . L.replace "[Char]" "String"
