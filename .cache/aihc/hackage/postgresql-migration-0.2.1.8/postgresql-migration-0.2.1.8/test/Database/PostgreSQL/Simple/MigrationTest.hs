-- |
-- Module      : Database.PostgreSQL.Simple.MigrationTest
-- Copyright   : (c) 2014 Andreas Meingast <ameingast@gmail.com>
--
-- License     : BSD-style
-- Maintainer  : andre@andrevdm.com
-- Stability   : experimental
-- Portability : GHC
--
-- A collection of postgresql-migration specifications.

{-# LANGUAGE OverloadedStrings #-}

module Database.PostgreSQL.Simple.MigrationTest where

import           Data.IORef
import           Database.PostgreSQL.Simple           (Connection)
import           Database.PostgreSQL.Simple.Migration (MigrationCommand (..),
                                                       MigrationOptions (..),
                                                       MigrationResult (..),
                                                       SchemaMigration (..),
                                                       TransactionControl (..),
                                                       Verbosity(..),
                                                       getMigrations,
                                                       runMigration,
                                                       defaultOptions)
import           Database.PostgreSQL.Simple.Util      (existsTable)
import           Test.Hspec                           (Spec, describe, it, shouldBe)

migrationSpec :: Connection -> Spec
migrationSpec con = describe "Migrations" $ do
  let
    migrationScript = MigrationScript "test.sql" q
    migrationScriptAltered = MigrationScript "test.sql" ""
    migrationDir = MigrationDirectory "share/test/scripts"
    migrationFile = MigrationFile "s.sql" "share/test/script.sql"

  it "asserts that the schema_migrations table does not exist" $ do
    r <- existsTable con "schema_migrations"
    r `shouldBe` False

  it "validates an initialization on an empty database" $ do
    r <- runMigration' (MigrationValidation MigrationInitialization)
    r `shouldBe` MigrationError "No such table: schema_migrations"

  it "initializes a database" $ do
    r <- runMigration' MigrationInitialization
    r `shouldBe` MigrationSuccess

  it "creates the schema_migrations table" $ do
    r <- existsTable con "schema_migrations"
    r `shouldBe` True

  it "executes a migration script" $ do
    r <- runMigration' migrationScript
    r `shouldBe` MigrationSuccess

  it "creates the table from the executed script" $ do
    r <- existsTable con "t1"
    r `shouldBe` True

  it "skips execution of the same migration script" $ do
    r <- runMigration' migrationScript
    r `shouldBe` MigrationSuccess

  it "reports an error on a different checksum for the same script" $ do
    r <- runMigration' migrationScriptAltered
    r `shouldBe` MigrationError "test.sql"

  it "executes migration scripts inside a folder" $ do
    r <- runMigration' migrationDir
    r `shouldBe` MigrationSuccess

  it "creates the table from the executed scripts" $ do
    r <- existsTable con "t2"
    r `shouldBe` True

  it "executes a file based migration script" $ do
    r <- runMigration' migrationFile
    r `shouldBe` MigrationSuccess

  it "creates the table from the executed scripts" $ do
    r <- existsTable con "t3"
    r `shouldBe` True

  it "validates initialization" $ do
    r <- runMigration' $ MigrationValidation MigrationInitialization
    r `shouldBe` MigrationSuccess

  it "validates an executed migration script" $ do
    r <- runMigration' $ MigrationValidation migrationScript
    r `shouldBe` MigrationSuccess

  it "validates all scripts inside a folder" $ do
    r <- runMigration' $ MigrationValidation migrationDir
    r `shouldBe` MigrationSuccess

  it "validates an executed migration file" $ do
    r <- runMigration' $ MigrationValidation migrationFile
    r `shouldBe` MigrationSuccess

  it "gets a list of executed migrations" $ do
    r <- getMigrations con
    map schemaMigrationName r `shouldBe` ["test.sql", "1.sql", "s.sql"]

  it "log can be redirected" $ do
    ref <- newIORef mempty
    let logWrite = modifyIORef ref . (<>) . show
    let opts = defaultOptions{ optLogWriter = logWrite, optVerbose = Verbose, optTransactionControl = NoNewTransaction }
    _ <- runMigration con opts MigrationInitialization
    readIORef ref >>= (`shouldBe` "Right \"Initializing schema\"")

  where
    q = "create table t1 (c1 varchar);"
    runMigration' =
      runMigration con defaultOptions{optTransactionControl = NoNewTransaction}

