module Yggdrasil.Test.Yggdrasil where

import Data.Text qualified as T
import Data.UUID.V4
import Relude
import System.Directory
import Test.Hspec
import Yggdrasil

yggdrasilSpec :: SpecWith ()
yggdrasilSpec = describe "yggdrasil" $ do
  it "can parse sql DDL statements for SQLite" $ do
    f <- readFileBS "./resources/test/migration-files/1-some-valid-sql.sql"
    case decodeUtf8' f of
      Left e -> print e >> error (show e)
      Right t -> do
        let parsed = parseTextToSqlStatements t
        length parsed `shouldBe` 3
  it "can explode migration filename properly" $ do
    explodeMigrationPath "0-init.sql" `shouldBe` Just ("0", "0-init.sql")
    explodeMigrationPath "999999-init.sql" `shouldBe` Just ("999999", "999999-init.sql")
    explodeMigrationPath "1-init🔗❤️👍.sql" `shouldBe` Just ("1", "1-init🔗❤️👍.sql")
    explodeMigrationPath "23-init with spaces and weird symbols$.sql" `shouldBe` Just ("23", "23-init with spaces and weird symbols$.sql")
  it "gets files in the right order" $ do
    xs <- getSortedMigrationFiles defaultYggdrasil {migrationsDirectoryPath = "./resources/test/migration-files/"}
    xs `shouldBe` [(0, "0-yggdrasil.sql"), (1, "1-some-valid-sql.sql"), (2, "2-more-valid-sql-$$.sql"), (3, "3-empty.sql")]
  it "does run migrations correctly on clean DB" $ do
    someUUID <- liftIO nextRandom
    let dbPath = "./resources/test/" <> (T.pack . show $ someUUID) <> ".sqlite"
        yggdrasil =
          Yggdrasil
            { databaseFilePath = dbPath,
              migrationsDirectoryPath = "./resources/test/migration-files/",
              engine = SQLite,
              runMigrations = True
            }
    _ <- runYggdrasil yggdrasil
    ms <- getRanMigrations yggdrasil
    _ <- liftIO $ removeFile (fromString . T.unpack $ dbPath)
    ms `shouldBe` [(0, "0-yggdrasil.sql"), (1, "1-some-valid-sql.sql"), (2, "2-more-valid-sql-$$.sql"), (3, "3-empty.sql")]

  it "does skip migrations correctly on used DB" $ do
    someUUID <- liftIO nextRandom
    let dbPath = "./resources/test/" <> (T.pack . show $ someUUID) <> ".sqlite"
        yggdrasil =
          Yggdrasil
            { databaseFilePath = dbPath,
              migrationsDirectoryPath = "./resources/test/migration-files/",
              engine = SQLite,
              runMigrations = True
            }
    _ <- runMigration yggdrasil (0, "0-yggdrasil.sql")
    _ <- runMigration yggdrasil (1, "1-some-valid-sql.sql")
    _ <- runYggdrasil yggdrasil
    ms <- getRanMigrations yggdrasil
    _ <- liftIO $ removeFile (fromString . T.unpack $ dbPath)
    ms `shouldBe` [(0, "0-yggdrasil.sql"), (1, "1-some-valid-sql.sql"), (2, "2-more-valid-sql-$$.sql"), (3, "3-empty.sql")]
  it "does not do anything when runMigrations is false" $ do
    someUUID <- liftIO nextRandom
    let dbPath = "./resources/test/" <> (T.pack . show $ someUUID) <> ".sqlite"
        yggdrasil =
          Yggdrasil
            { databaseFilePath = dbPath,
              migrationsDirectoryPath = "./resources/test/migration-files/",
              engine = SQLite,
              runMigrations = False
            }
    _ <- runYggdrasil yggdrasil
    ms <- getRanMigrations yggdrasil
    _ <- liftIO $ removeFile (fromString . T.unpack $ dbPath)
    ms `shouldBe` []
