-- |
-- Module      : Main
-- Copyright   : (c) 2014 Andreas Meingast <ameingast@gmail.com>
--
-- License     : BSD-style
-- Maintainer  : andre@andrevdm.com
-- Stability   : experimental
-- Portability : GHC
--
-- The test entry-point for postgresql-migration.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main
    ( main
    ) where

import           Database.PostgreSQL.Simple               (connectPostgreSQL)
import qualified Database.PostgreSQL.Simple.MigrationTestV1Compat as V1
import qualified Database.PostgreSQL.Simple.MigrationTest as V2
import qualified Database.PostgreSQL.Simple.TransactionPerRunTest as V2TrnRun
import qualified Database.PostgreSQL.Simple.TransactionPerStepTest as V2TrnStep
import           Database.PostgreSQL.Simple.Util          (withTransactionRolledBack)
import qualified System.Environment as Env

import           Test.Hspec                               (hspec)

main :: IO ()
main = do
  Env.getArgs >>= \case
    ("psql":as) -> Env.withArgs as $ do
      conRollback <- connectPostgreSQL ""
      withTransactionRolledBack conRollback (hspec (V2.migrationSpec conRollback))
      withTransactionRolledBack conRollback (hspec (V1.migrationSpec conRollback))
      withTransactionRolledBack conRollback (hspec (V2TrnRun.migrationSpec conRollback))

      conPerStep <- connectPostgreSQL ""
      hspec (V2TrnStep.migrationSpec conPerStep)

    _ -> do
      putStrLn "Skipping tests, no 'psql' argument provided"
      putStrLn "  To run the tests please use one of the following make commands"
      putStrLn "   make cabal-test"
      putStrLn "   make stack-test"
      putStrLn "  or directly"
      putStrLn "   stack test --ta psql"
      putStrLn "   cabal run --test-show-details=direct test:tests psql"

