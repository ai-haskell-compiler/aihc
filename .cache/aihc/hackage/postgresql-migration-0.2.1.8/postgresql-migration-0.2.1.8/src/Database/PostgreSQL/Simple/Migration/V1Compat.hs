-- |
-- Module      : Database.PostgreSQL.Simple.Migration
-- Copyright   : (c) 2014 Andreas Meingast <ameingast@gmail.com>
--
-- License     : BSD-style
-- Maintainer  : andre@andrevdm.com
-- Stability   : experimental
-- Portability : GHC
--
-- A migration library for postgresql-simple.
--
-- For usage, see Readme.markdown.

{-# LANGUAGE OverloadedStrings #-}

module Database.PostgreSQL.Simple.Migration.V1Compat
    (
    -- * Migration actions
      runMigration
    , runMigrations
    , V2.sequenceMigrations

    -- * Migration types
    , MigrationContext(..)

    , V2.MigrationCommand(..)
    , V2.MigrationResult(..)
    , V2.ScriptName
    , V2.Checksum

    -- * Migration result actions
    , V2.getMigrations

    -- * Migration result types
    , V2.SchemaMigration(..)
    ) where


import           Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple.Migration as V2


runMigration :: MigrationContext -> IO (V2.MigrationResult [Char])
runMigration (MigrationContext cmd verbose con) = runMigrations verbose con [cmd]


runMigrations
    :: Bool
       -- ^ Run in verbose mode
    -> Connection
       -- ^ The postgres connection to use
    -> [V2.MigrationCommand]
       -- ^ The commands to run
    -> IO (V2.MigrationResult String)
runMigrations verbose con commands = do
  let opts = V2.defaultOptions
       { V2.optVerbose = if verbose then V2.Verbose else V2.Quiet
       , V2.optTableName = "schema_migrations"
       , V2.optTransactionControl = V2.NoNewTransaction
       }
  V2.runMigrations con opts commands


-- | The 'MigrationContext' provides an execution context for migrations.
data MigrationContext = MigrationContext
  { migrationContextCommand :: V2.MigrationCommand
  -- ^ The action that will be performed by 'runMigration'
  , migrationContextVerbose :: Bool
  -- ^ Verbosity of the library.
  , migrationContextConnection :: Connection
  -- ^ The PostgreSQL connection to use for migrations.
  }
