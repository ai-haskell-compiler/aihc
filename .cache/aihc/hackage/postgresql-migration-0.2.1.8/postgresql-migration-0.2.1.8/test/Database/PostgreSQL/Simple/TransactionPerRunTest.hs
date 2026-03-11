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

module Database.PostgreSQL.Simple.TransactionPerRunTest where

import           Database.PostgreSQL.Simple           (Connection)
import           Database.PostgreSQL.Simple.Migration (MigrationCommand (..),
                                                       MigrationOptions (..),
                                                       MigrationResult (..),
                                                       TransactionControl (..),
                                                       runMigration,
                                                       runMigrations,
                                                       defaultOptions)
import           Database.PostgreSQL.Simple.Util      (existsTable)
import           Test.Hspec                           (Spec, describe, it, shouldBe, shouldThrow, anyException)

migrationSpec :: Connection -> Spec
migrationSpec con = describe "Migrations" $ do
  let
    scriptCreate1 = MigrationScript "create1.sql" "create table trn1 (c2 varchar);"
    scriptError1 = MigrationScript "errorScript.sql" "NOT SQL!"

  it "asserts that the test table does not exist" $ do
    r <- existsTable con "trn1"
    r `shouldBe` False

  it "asserts that the test table does not exist" $ do
    r <- existsTable con "trn2"
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

  -- This test shoud thow an exception since the second migration script is invalid SQL
  it "executes a migration script" $ (`shouldThrow` anyException) $ do
    let opts = defaultOptions{optTransactionControl = TransactionPerRun}
    runMigrations con opts [scriptCreate1, scriptError1]

  -- The second migration script failed, so the changes from the first should be rolled back (TransactionPerRun)
  it "creates the table from the executed script" $ do
    r <- existsTable con "trn1"
    r `shouldBe` False


  where
    runMigration' =
      runMigration con defaultOptions{optTransactionControl = NoNewTransaction}
