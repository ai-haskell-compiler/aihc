{- ORACLE_TEST xfail WarningText does not carry pragmaRawText so the pretty-printer normalises to {-# DEPRECATED #-} -}
module InstanceDeprecatedPragmaLowercase where

data T1 = T1

instance {-#deprecated "Do not use Show T1" #-} Show T1 where
  show _ = "T1"
