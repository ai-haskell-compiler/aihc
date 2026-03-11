
module Main (main) where

import Prelude hiding (lookup)
import Database.MySQL.Simple qualified as SQL
import Database.MySQL.JSONTable
import Data.Ratio (Ratio, (%))
import Control.Monad (unless)
import Conduit (runResourceT)
import Data.Conduit qualified as Conduit

main :: IO ()
main = do
  conn <- SQL.connect (SQL.defaultConnectInfo
    { SQL.connectDatabase = "json-table-test"
    , SQL.connectUser = "root"
    , SQL.connectPassword = "root"
      })
  table0 <- createTable conn True "create-delete-table"
  _ <- createTable conn False $ tableName table0
  deleteTable conn True table0
  deleteTable conn False table0
  table <- createTable conn True "int-table"
  let half :: Ratio Int
      half = 1 % 2
  i <- insert conn table half
  x <- lookup conn table i
  unless (x == Just half) $ fail $ "Insert-Lookup test failed: got " ++ show x
  adjust conn table (pure . (+1)) i
  y <- lookup conn table i
  unless (y == Just (half + 1)) $ fail $ "Update-Lookup test failed: got " ++ show y
  delete conn table i
  z <- lookup conn table i
  unless (z == Nothing) $ fail $ "Delete-Lookup test failed: got " ++ show z
  i1 <- insert conn table 1
  i2 <- insert conn table 2
  i3 <- insert conn table 3
  rs <- runResourceT $ Conduit.sourceToList $ sourceRows conn table
  unless (rs == [Row i1 1, Row i2 2, Row i3 3]) $
    fail $ "Stream test failed: got " ++ show rs
