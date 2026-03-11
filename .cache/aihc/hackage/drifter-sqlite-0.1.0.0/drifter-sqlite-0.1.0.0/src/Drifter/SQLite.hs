module Drifter.SQLite
    ( SQLiteMigration
    , Method(..)
    , DBConnection(..)
    , ChangeHistory(..)
    , runMigrations
    , getChangeHistory
    , getChangeNameHistory
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative              as A
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.Time
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Drifter
-------------------------------------------------------------------------------



data SQLiteMigration


data instance Method SQLiteMigration =
    MigrationQuery Query
    -- ^ Run a query against the database
  | MigrationCode (Connection -> IO (Either String ()))
                                  -- ^ Run any arbitrary IO code


data instance DBConnection SQLiteMigration = DBConnection SQLiteMigrationConnection


data SQLiteMigrationConnection = SQLiteMigrationConnection (Set ChangeName) Connection


instance Drifter SQLiteMigration where
  migrateSingle (DBConnection migrationConn) change = do
    runExceptT (migrateChange migrationConn change)


-------------------------------------------------------------------------------
-- Change History Tracking
-------------------------------------------------------------------------------
newtype ChangeId = ChangeId Int deriving (Eq, Ord, Show, FromField)


data ChangeHistory = ChangeHistory {
      histId          :: ChangeId
    , histName        :: ChangeName
    , histDescription :: Maybe Description
    , histTime        :: UTCTime
    } deriving (Show)


instance Eq ChangeHistory where
    a == b = (histName a) == (histName b)


instance Ord ChangeHistory where
    compare a b = compare (histId a) (histId b)


instance FromRow ChangeHistory where
    fromRow = ChangeHistory
      <$> field
      <*> (ChangeName <$> field)
      <*> field
      <*> field


-------------------------------------------------------------------------------
-- Queries
-------------------------------------------------------------------------------
bootstrapQ :: Query
bootstrapQ = "\
\  CREATE TABLE IF NOT EXISTS schema_migrations ( \
\    id              INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,\
\    name            TEXT        NOT NULL UNIQUE ON CONFLICT ROLLBACK,\
\    description     TEXT,\
\    time            DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP\
\  );"


-------------------------------------------------------------------------------
changeHistoryQ :: Query
changeHistoryQ =
  "SELECT id, name, description, time FROM schema_migrations ORDER BY id;"


-------------------------------------------------------------------------------
changeNameHistoryQ :: Query
changeNameHistoryQ =
  "SELECT name FROM schema_migrations ORDER BY id;"


-------------------------------------------------------------------------------
insertLogQ :: Query
insertLogQ =
  "INSERT INTO schema_migrations (name, description, time) VALUES (?, ?, ?);"


-------------------------------------------------------------------------------
migrateChange :: SQLiteMigrationConnection -> Change SQLiteMigration -> ExceptT String IO ()
migrateChange (SQLiteMigrationConnection hist c) change = do
  if Set.member cn hist
    then lift (putStrLn ("Skipping: " ++ show (changeNameText cn)))
    else do
      runMethod c (changeMethod change)
      logChange c change
      lift (putStrLn ("Committed: " ++ show cn))
  where
    cn = changeName change


-------------------------------------------------------------------------------
runMethod :: Connection -> Method SQLiteMigration -> ExceptT String IO ()
runMethod c (MigrationQuery q) =
  void (ExceptT ((Right <$> execute_ c q) `catches` errorHandlers))
runMethod c (MigrationCode f) =
  ExceptT (f c `catches` errorHandlers)


  -------------------------------------------------------------------------------
logChange :: Connection -> Change SQLiteMigration -> ExceptT String IO ()
logChange c change = do
    now <- lift getCurrentTime
    void (ExceptT ((Right <$> go now) `catches` errorHandlers))
  where
    go now = execute c insertLogQ (changeNameText (changeName change), changeDescription change, now)


-------------------------------------------------------------------------------
errorHandlers :: [Handler (Either String b)]
errorHandlers =
  [ Handler (\(ex::SQLError) -> return (Left (show ex)))
  , Handler (\(ex::FormatError) -> return (Left (show ex)))
  , Handler (\(ex::ResultError) -> return (Left (show ex)))
  ]


-------------------------------------------------------------------------------
-- | Takes a connection and builds the state to thread throughout the migration.
-- This includes bootstrapping the migration tables and collecting all the
-- migrations that have already been committed.
makePGMigrationConnection :: Connection -> IO SQLiteMigrationConnection
makePGMigrationConnection conn = do
  void (execute_ conn bootstrapQ)
  hist <- getChangeNameHistory conn
  return (SQLiteMigrationConnection (Set.fromList hist) conn)


-------------------------------------------------------------------------------
-- | Takes the list of all migrations, removes the ones that have
-- already run and runs them. Use this instead of 'migrate'.
runMigrations :: Connection -> [Change SQLiteMigration] -> IO (Either String ())
runMigrations conn changesList = handle (\(RolledBack e) -> pure (Left e)) $ fmap Right $ do
  withTransaction conn $ do
    migrationConn <- makePGMigrationConnection conn
    res <- migrate (DBConnection migrationConn) changesList
    case res of
      Right _ -> pure ()
      Left e -> throw (RolledBack e)


-------------------------------------------------------------------------------
data RolledBack = RolledBack String
  deriving (Show)

instance Exception RolledBack


-------------------------------------------------------------------------------
-- | Get all changes from schema_migrations table for all the migrations that
-- have previously run.
getChangeHistory :: Connection -> IO [ChangeHistory]
getChangeHistory conn = query_ conn changeHistoryQ


-------------------------------------------------------------------------------
-- | Get just the names of all changes from schema_migrations for migrations
-- that have previously run.
getChangeNameHistory :: Connection -> IO [ChangeName]
getChangeNameHistory conn = fmap (\(Only nm) -> ChangeName nm)
  A.<$> query_ conn changeNameHistoryQ
