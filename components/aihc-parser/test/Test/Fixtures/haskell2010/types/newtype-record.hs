{- ORACLE_TEST
id: types-newtype-record
category: types
expected: pass
-}
module T3 where
newtype UserId = UserId { unUserId :: Int }
