{- ORACLE_TEST
id: decls-data-record-strict-field
category: declarations
expected: pass
-}
module D21 where
data StrictRec = StrictRec { payload :: !Int }
