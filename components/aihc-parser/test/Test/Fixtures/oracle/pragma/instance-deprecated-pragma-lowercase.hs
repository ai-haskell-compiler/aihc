{- ORACLE_TEST pass -}
module InstanceDeprecatedPragmaLowercase where

data T1 = T1

instance {-#deprecated "Do not use Show T1" #-} Show T1 where
  show _ = "T1"
