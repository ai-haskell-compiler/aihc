{- ORACLE_TEST xfail qualified class name in instance head loses module qualifier during roundtrip -}
module A where
import qualified Data.Aeson as A
instance A.ToJSON a => A.ToJSON [a] where
  toJSON = undefined
