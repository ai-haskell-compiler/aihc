{-# LANGUAGE OverloadedStrings #-}

module Database.DBCleaner.PostgreSQLSimple (withConnection) where

import           Control.Exception                (bracket, finally)
import           Control.Monad                    (void)
import           Data.Monoid                      (mconcat)
import           Data.Text                        (Text)
import           Database.PostgreSQL.Simple       (Connection, Only (..), begin,
                                                   execute, query_, rollback)
import           Database.PostgreSQL.Simple.Types (Identifier (..))

import           Database.DBCleaner.Types         (Strategy (..))

-- | Connection wrapper that cleans up the database using the strategy specified.
withConnection :: Strategy -> (Connection -> IO a) -> Connection -> IO a
withConnection Transaction f c = bracket (begin c >> return c) rollback f
withConnection Truncation f c = finally (f c) $ listTables c >>= mapM_ (truncateTable c)

listTables :: Connection -> IO [Text]
listTables c = map fromOnly `fmap` query_ c q
  where
    q = mconcat [ "SELECT c.relname FROM pg_catalog.pg_class c"
                , " LEFT JOIN pg_catalog.pg_namespace n"
                , " ON c.relnamespace = n.oid"
                , " WHERE c.relkind IN ('r', '')"
                , " AND n.nspname <> 'pg_catalog'"
                , " AND n.nspname <> 'information_schema'"
                , " AND n.nspname !~ '^pg_toast'"
                , " AND pg_catalog.pg_table_is_visible(c.oid)"
                ]

truncateTable :: Connection -> Text -> IO ()
truncateTable c = void . execute c q . Only . Identifier
  where
    q = "TRUNCATE ? CASCADE"
