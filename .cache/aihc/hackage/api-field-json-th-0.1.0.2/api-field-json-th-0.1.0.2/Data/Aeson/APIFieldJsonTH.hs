-- |
--
-- Utils for using aeson's deriveJSON with lens's makeFields

module Data.Aeson.APIFieldJsonTH (
    -- * How to use this library
    -- $use
    deriveApiFieldJSON
    ) where

import Prelude
import Data.Aeson.TH
import Control.Lens
import qualified Data.Char as C (toLower)
import Language.Haskell.TH (Q, Dec, Name)
import Data.List.Split

-- $use
--
-- > data SomeQuery = SomeQuery {
-- >     _someQueryPage :: Int
-- >     , _someQueryText :: String
-- > } deriving (Eq, Show)
-- > makeFields ''SomeQuery
-- > deriveApiFieldJSON ''SomeQuery
--
-- This is compatible with the next json
--
-- > {"page": 3, "text": "foo"}
--

-- | レコード名を落としてjsonインスタンス化
deriveApiFieldJSON :: Name -> Q[Dec]
deriveApiFieldJSON name = deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropPrefix name} name

firstLower :: String -> String
firstLower (x:xs) = (C.toLower x):xs

typeName :: String -> String
typeName = view _last . splitOn "."

dropPrefix :: Name -> String -> String
dropPrefix = drop . (1 + ) . length . typeName . show
