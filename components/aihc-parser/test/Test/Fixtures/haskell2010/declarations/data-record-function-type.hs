{- ORACLE_TEST
id: decls-data-record-function-type
category: declarations
expected: pass
-}
module M where
data Target a = Target {
    lTarget  :: a -> Double
  , glTarget :: Maybe (a -> a)
  }
