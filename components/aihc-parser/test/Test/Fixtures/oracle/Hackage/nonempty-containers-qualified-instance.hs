{- ORACLE_TEST pass -}
module A where
import qualified Data.Aeson as A
instance A.ToJSON a => A.ToJSON [a] where
  toJSON = undefined
