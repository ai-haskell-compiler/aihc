{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Specs where

import Control.Monad (replicateM_)
import Control.Exception (catchJust, try, catch)
import Control.Exception.Lifted (bracket)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as X
import System.FilePath ((</>))
import Hedgehog (checkParallel, discover, Property, PropertyT, property, forAll, (===),
  assert, eval, footnote, failure, withTests, assert)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified System.IO.Temp as Temp
import Squeather (SQLData(SQLInteger))
import qualified Squeather
import System.Directory (listDirectory, removeDirectoryRecursive)
import qualified Generators as TG
import qualified Squeather.Internal as Internal
import qualified Foreign

tests :: IO Bool
tests = checkParallel $$(discover)

mkTempDirectory :: (FilePath -> PropertyT IO ()) -> PropertyT IO ()
mkTempDirectory callback = do
  tmpdir <- liftIO $ Temp.getCanonicalTemporaryDirectory
  bracket
    (liftIO $ Temp.createTempDirectory tmpdir "squeather-test")
    (liftIO . removeDirectoryRecursive)
    callback


-- | open creates a file with the given name.
prop_openCreatesFile :: Property
prop_openCreatesFile = withTests 1 . property $ mkTempDirectory $ \tmpDir -> do
  let fn = "db"
  _ <- liftIO $ Squeather.open (X.pack $ tmpDir </> fn)
  dir <- liftIO $ listDirectory tmpDir
  dir === [fn]

-- | open does not create a file when memory is used.
prop_openNoCreateFileOnMemory :: Property
prop_openNoCreateFileOnMemory = withTests 1 . property $ mkTempDirectory $ \tmpDir -> do
  let flgs = Squeather.openFlags { Squeather.memory = True }
      fn = "db"
  _ <- liftIO $ (Squeather.openWithFlags flgs (X.pack $ tmpDir </> fn))
  dir <- liftIO $ listDirectory tmpDir
  dir === []

-- | open does not create a file when NoCreate is used.
prop_openNoCreateFileOnNoCreate :: Property
prop_openNoCreateFileOnNoCreate = withTests 1 . property $ mkTempDirectory $ \tmpDir -> do
  let fn = "db"
      flgs = Squeather.openFlags { Squeather.writeMode = Squeather.ReadWrite Squeather.NoCreate }
      create = liftIO (Squeather.openWithFlags flgs (X.pack $ tmpDir </> fn)) >> return ()
      pd e = Squeather.errorFlag e == Left Squeather.SQLITE_CANTOPEN
  liftIO $ catchJust (\e -> if pd e then Just () else Nothing) create (const (return ()))
  dir <- liftIO $ listDirectory tmpDir
  dir === []

-- | open does not create a file when ReadOnly is used.
prop_openNoCreateFileOnReadOnly :: Property
prop_openNoCreateFileOnReadOnly = withTests 1 . property $ mkTempDirectory $ \tmpDir -> do
  let fn = "db"
      flgs = Squeather.openFlags { Squeather.writeMode = Squeather.ReadOnly }
      create = liftIO (Squeather.openWithFlags flgs (X.pack $ tmpDir </> fn)) >> return ()
      pd e = Squeather.errorFlag e == Left Squeather.SQLITE_CANTOPEN
  liftIO $ catchJust (\e -> if pd e then Just () else Nothing) create (const (return ()))
  dir <- liftIO $ listDirectory tmpDir
  dir === []

-- | The library is compiled with support for threads.
prop_isThreadsafe :: Property
prop_isThreadsafe = withTests 1 . property $ do
  res <- liftIO $ Internal.sqlite3_threadsafe
  res === 1

inMemory :: (Squeather.Database -> IO a) -> IO a
inMemory cback =
  Squeather.openWithFlags (Squeather.openFlags { Squeather.memory = True }) "" >>= cback

-- Only the code after the last forAll is guaranteed to run every
-- test


-- | Insert with exec and then retrieve
prop_insertWithExec :: Property
prop_insertWithExec = property $ do
  sd <- forAll TG.sqlData
  r <- liftIO $ inMemory $ \db -> do
        Squeather.exec db "CREATE TABLE t(c1)"
        _ <- Squeather.executeNamed db "INSERT INTO t VALUES (:c)" [(":c", sd)]
        Squeather.execute db "SELECT c1 FROM t"
  r === [[sd]]

-- | lastInsertRowId works
prop_lastInsertRowId :: Property
prop_lastInsertRowId = property $ do
  nInserts <- forAll $ Gen.int (Range.exponential 1 1000)
  r <- liftIO $ inMemory $ \db -> do
        Squeather.exec db "CREATE TABLE t(c1)"
        replicateM_ nInserts (Squeather.exec db "INSERT INTO t(c1) VALUES (0)")
        Squeather.lastInsertRowId db
  r === fromIntegral nInserts

-- | reset works
prop_reset :: Property
prop_reset = property $ do
  sd <- forAll TG.sqlData
  r <- liftIO $ inMemory $ \db -> do
        Squeather.exec db "CREATE TABLE t(c1)"
        _ <- Squeather.executeNamed db "INSERT INTO t VALUES (:c)" [(":c", sd)]
        stmt <- Squeather.prepare db "SELECT c1 FROM t"
        _ <- Squeather.allRows stmt
        Squeather.reset stmt
        Squeather.allRows stmt
  r === [[sd]]

-- | clearBindings works
prop_clearBindings :: Property
prop_clearBindings = property $ do
  sd <- forAll TG.sqlData
  sd' <- forAll TG.sqlData
  r <- liftIO $ inMemory $ \db -> do
        Squeather.exec db "CREATE TABLE t(c1)"
        stmt <- Squeather.prepare db "INSERT INTO t VALUES (:c)"
        Squeather.bindParams stmt [(":c", sd)]
        Squeather.clearBindings stmt
        Squeather.bindParams stmt [(":c", sd')]
        _ <- Squeather.allRows stmt
        Squeather.execute db "SELECT c1 FROM t"
  r === [[sd']]

-- | executeNamed works with variable number of columns and rows
prop_executeNamedVariable :: Property
prop_executeNamedVariable = property $ do
  nCols <- forAll $ Gen.int (Range.exponential 1 20)
  rows <- forAll (Gen.list (Range.exponential 1 20) (Gen.list (Range.singleton nCols) TG.sqlData))
  rows' <- liftIO $ inMemory $ \db -> do
        Squeather.exec db (TG.createTableStatement nCols)
        Squeather.exec db "BEGIN;"
        mapM_ (Squeather.executeNamed db (TG.insertStatement nCols))
          . fmap TG.addColumnLabels $ rows
        Squeather.exec db "COMMIT;"
        Squeather.execute db "SELECT * FROM t ORDER BY rowid"
  rows === rows'

-- | Error messages are generated properly
prop_errorMessages :: Property
prop_errorMessages = withTests 1 . property $ do
  ei <- liftIO . try . inMemory $ \db -> Squeather.execute db "ERROR"
  case ei of
    Left e -> do
      assert $ X.length (Squeather.errorContext e) > 0
      case Squeather.errorFlag e of
        Left err -> () <$ eval err
        Right g -> () <$ eval g
      assert $ X.length (Squeather.errorText e) > 0
      fmap (const ()) . eval . Squeather.errorFilename $ e
    Right _ -> footnote "bad function returned successfully" >> failure

-- | Backup function works
prop_backup :: Property
prop_backup = property $ do
  nCols <- forAll $ Gen.int (Range.exponential 1 20)
  rows <- forAll (Gen.list (Range.exponential 1 20) (Gen.list (Range.singleton nCols) TG.sqlData))
  rows' <- liftIO $ do
        db1 <- Squeather.open ":memory:"
        db2 <- Squeather.open ":memory:"
        Squeather.exec db1 (TG.createTableStatement nCols)
        Squeather.exec db1 "BEGIN;"
        mapM_ (Squeather.executeNamed db1 (TG.insertStatement nCols))
          . fmap TG.addColumnLabels $ rows
        Squeather.exec db1 "COMMIT;"
        Squeather.backup (Squeather.Source db1 "main") (Squeather.Destination db2 "main")
        Squeather.execute db2 "SELECT * FROM t ORDER BY rowid"
  rows === rows'

-- | changes function works
prop_changes :: Property
prop_changes = withTests 1 . property $ do
  r <- liftIO $ inMemory $ \db ->  do
        Squeather.exec db "CREATE TABLE t(c1); INSERT INTO t VALUES (0), (1), (2);"
        Squeather.changes db
  r === 3

-- | columnNames works as it should
prop_columnNames :: Property
prop_columnNames = withTests 1 . property $ do
  r <- liftIO $ inMemory $ \db -> do
        stmt <- Squeather.prepare db "SELECT 1 AS One, 2 AS Two, 3 AS Three"
        Squeather.columnNames stmt
  r === ["One", "Two", "Three"]

-- | executeNamedWithColumns works as it should
prop_executeNamedWithColumns :: Property
prop_executeNamedWithColumns = withTests 1 . property $ do
  r <- liftIO $ inMemory $ \db -> do
        Squeather.exec db "CREATE TABLE t(c1, c2); INSERT INTO t VALUES (0, 5), (1, 6), (2, 7);"
        Squeather.executeNamedWithColumns db
          "SELECT c1 AS C1, c2 AS C2 FROM t WHERE c1 > :val" [(":val", SQLInteger 0)]
  r === (["C1", "C2"], [[SQLInteger 1, SQLInteger 6], [SQLInteger 2, SQLInteger 7]])

-- | finalizing databases doesn't wreak havoc
prop_finalizeDatabase :: Property
prop_finalizeDatabase = withTests 1 . property $ do
  db <- liftIO $ Squeather.open ":memory:"
  liftIO $ Foreign.finalizeForeignPtr (Internal.dbPointer db)

-- | finalizing statements doesn't wreak havoc
prop_finalizeStatement :: Property
prop_finalizeStatement = withTests 1 . property $ do
  db <- liftIO $ Squeather.open ":memory:"
  stmt <- liftIO $ Squeather.prepare db "CREATE TABLE t(c1);"
  liftIO $ Foreign.finalizeForeignPtr (Internal.stmtPointer stmt)

-- | double-quoted non-existent identifiers cause SQLite to fail and
-- causes Squeather to throw an exception.  This is to test that the
-- @-DSQLITE_DQS=0@ compile-time option is working.  Preparing the
-- statement (not even running it) should throw an exception.
prop_doubleQuotedIdentifier :: Property
prop_doubleQuotedIdentifier = withTests 1 . property $ do
  r <- liftIO $ do
    db <- Squeather.open ":memory:"
    Squeather.exec db "CREATE TABLE mytable(mycolumn);"
    let badName = "squeather_column_does_not_exist"
        run = Squeather.prepare db ("SELECT \"" <> badName <> "\" FROM mytable;")
          >> return False
        catcher e = return $ badName `X.isInfixOf` Squeather.errorText e
    catch run catcher
  assert r

-- | The version number is as expected.
prop_version :: Property
prop_version = withTests 1 . property $ Squeather.sqliteVersion === "3.35.5"
