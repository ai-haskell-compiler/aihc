-- | Template Haskell macros to derive ToJSON/FromJSON instances in a more prefix-friendly manner.
--   See <https://gitlab.com/igrep/deriveJsonNoPrefix#readme>.

module Data.Aeson.DeriveNoPrefix
  ( deriveJsonNoTypeNamePrefix
  , deriveToJsonNoTypeNamePrefix
  , deriveFromJsonNoTypeNamePrefix
  , dropPrefix
  ) where


import           Data.Aeson.TH
                   ( deriveJSON
                   , deriveToJSON
                   , deriveFromJSON
                   )
import qualified Data.Aeson as Json
import           Data.Aeson.Types
                   ( Options(fieldLabelModifier)
                   )
import           Data.Char
                   ( toLower
                   )
import           Language.Haskell.TH
                   ( Name
                   , Dec
                   , Q
                   , nameBase
                   )


deriveXNoTypeNamePrefix :: (Options -> Name -> Q [Dec]) -> Name -> Q [Dec]
deriveXNoTypeNamePrefix deriver name =
  deriver Json.defaultOptions { fieldLabelModifier = dropPrefix name } name


deriveJsonNoTypeNamePrefix :: Name -> Q [Dec]
deriveJsonNoTypeNamePrefix = deriveXNoTypeNamePrefix deriveJSON


deriveToJsonNoTypeNamePrefix :: Name -> Q [Dec]
deriveToJsonNoTypeNamePrefix = deriveXNoTypeNamePrefix deriveToJSON


deriveFromJsonNoTypeNamePrefix :: Name -> Q [Dec]
deriveFromJsonNoTypeNamePrefix = deriveXNoTypeNamePrefix deriveFromJSON


dropPrefix :: Name -> String -> String
dropPrefix name = firstLower . drop (length $ nameBase name)


firstLower :: String -> String
firstLower (x:xs) = toLower x : xs
firstLower _ = error "firstLower: Assertion failed: empty string"
