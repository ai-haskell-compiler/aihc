{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Yggdrasil where

import Control.Exception
import Data.List (minimumBy)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Time
import Data.UUID.V4
import Database.SQLite.Simple
import NeatInterpolation
import Optics
import Relude
import System.Directory

data YggdrasilEngine = SQLite | PostgreSQL | MySQL deriving (Generic, Eq, Show)

data Yggdrasil = Yggdrasil
  { databaseFilePath :: Text,
    migrationsDirectoryPath :: Text,
    runMigrations :: Bool,
    engine :: YggdrasilEngine
  }
  deriving (Generic, Eq, Show)

makeFieldLabelsNoPrefix ''Yggdrasil

data RanMigration = RanMigration Text Int Text UTCTime
  deriving (Generic, Eq, Show)

instance FromRow RanMigration where
  fromRow = RanMigration <$> field <*> field <*> field <*> field

defaultYggdrasil :: Yggdrasil
defaultYggdrasil =
  Yggdrasil
    { databaseFilePath = "./resources/test/db.sqlite",
      migrationsDirectoryPath = "./resources/migrations/sqlite/",
      runMigrations = True,
      engine = SQLite
    }

runYggdrasil :: (MonadIO m) => Yggdrasil -> m ()
runYggdrasil yggdrasil = when (yggdrasil ^. #runMigrations) $ do
  sortedFiles <- getSortedMigrationFiles yggdrasil
  case nonEmpty sortedFiles of
    Nothing -> error "No valid migration files found!"
    Just ordersAndFiles -> do
      ranMigrations <- liftIO $ catch (getRanMigrations yggdrasil) handler

      let execReqHighest = minimumBy (comparing Down) $ NE.map fst ordersAndFiles
          isThereHigher = isJust . find (\p -> fst p > execReqHighest) $ ranMigrations
      when isThereHigher $ error "Detected a migration order mismatch! History contains newer items than in migrations folder!"

      let toRun = NE.filter (\t -> isNothing $ find (== t) ranMigrations) ordersAndFiles

      case nonEmpty toRun of
        Nothing -> print ("All migrations up to date!" :: String)
        Just toRun' -> do
          _ <- print ("About to run migrations!" :: String)
          _ <- print toRun'
          mapM_ (runMigration yggdrasil) toRun'
  where
    handler :: SQLError -> IO [(Int, Text)]
    handler _ = pure []

getSortedMigrationFiles :: (MonadIO m) => Yggdrasil -> m [(Int, Text)]
getSortedMigrationFiles yggdrasil = do
  files <- liftIO $ listDirectory (fromString . T.unpack $ yggdrasil ^. #migrationsDirectoryPath)
  let rawFs = mapMaybe (explodeMigrationPath . T.pack) files
      rawFilesWithOrder = mapMaybe (liftTupleMaybeFromFst . toIntOrder) rawFs
      sortedFiles = sortOn fst rawFilesWithOrder
  pure sortedFiles

toIntOrder :: (Text, Text) -> (Maybe Int, Text)
toIntOrder = first (readMaybe . T.unpack)

explodeMigrationPath :: Text -> Maybe (Text, Text)
explodeMigrationPath filePath =
  let k = (fmap head . nonEmpty . T.split (== '-')) filePath
   in liftTupleMaybeFromFst (k, filePath)

liftTupleMaybeFromFst :: (Maybe a, b) -> Maybe (a, b)
liftTupleMaybeFromFst (Nothing, _) = Nothing
liftTupleMaybeFromFst (Just j, x) = Just (j, x)

parseTextToSqlStatements :: Text -> [Text]
parseTextToSqlStatements = filter (/= "") . map T.strip . T.split (== ';')

runMigration :: (MonadIO m) => Yggdrasil -> (Int, Text) -> m ()
runMigration yggdrasil (ord', fPath) = do
  f <- readFileBS (T.unpack (yggdrasil ^. #migrationsDirectoryPath <> fPath))
  let maybeFileContents = decodeUtf8' f
  case maybeFileContents of
    Left e -> print e >> error (show e) -- migration errors should be critical
    Right fileContents' -> do
      conn <- liftIO $ open (T.unpack $ yggdrasil ^. #databaseFilePath)
      -- run statements from parsed file  in order
      _ <-
        liftIO
          $ mapM
            (\qqq -> execute_ conn (fromString . T.unpack $ qqq))
            (parseTextToSqlStatements fileContents')
      -- record in migrations table that we ran migration so as to not run again
      someUUID <- liftIO nextRandom
      now <- liftIO getCurrentTime
      -- TODO: add the Yggdrasil versio number when as library
      _ <- liftIO $ execute conn (fromString . T.unpack $ insertYggdrasilMigrationQuery) (T.pack . show $ someUUID, ord', fPath, now)
      _ <- liftIO $ close conn
      pure ()
  where
    insertYggdrasilMigrationQuery =
      [trimming|
      INSERT INTO yggdrasil (identifier, order_value, file_name, ran_at) VALUES (?,?,?,?)
      |]

getRanMigrations :: (MonadIO m) => Yggdrasil -> m [(Int, Text)]
getRanMigrations yggdrasil = do
  conn <- liftIO $ open (fromString . T.unpack $ yggdrasil ^. #databaseFilePath)
  (ms :: [RanMigration]) <- liftIO $ catch (query_ conn "SELECT identifier, order_value, file_name, ran_at from yggdrasil") handler
  pure . sortOn fst . map mapRanMigration $ ms
  where
    mapRanMigration (RanMigration _ orderValue fileName _) = (orderValue, fileName)
    handler :: SQLError -> IO [RanMigration]
    handler _ = pure []
