module Database.DBCleaner.Types where

data Strategy = Transaction -- ^ Wraps all the queries inside a transaction and
                            --   rollback everything once the execution is done.
              | Truncation  -- ^ Truncate all the tables after the execution of
                            --   the queries.
