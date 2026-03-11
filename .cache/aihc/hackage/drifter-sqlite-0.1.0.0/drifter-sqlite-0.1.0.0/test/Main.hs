module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative       as A
import           Control.Exception
import           Data.IORef
import           Data.Text                 (Text)
import           Database.SQLite.Simple
import           Drifter
import           System.Directory
import           System.IO.Error
import           Test.Tasty
import           Test.Tasty.HUnit
-------------------------------------------------------------------------------
import           Drifter.SQLite
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "drifter-postgresql"
  [
    withResource setup teardown $ \getConn -> testCase "migrations" $ do
       c <- getConn
       c3Calls <- newIORef 0
       let migrate' = runMigrations c . changeSequence
       res <- migrate' [c1, c2]
       res @?= Right ()

       rows <- query_ c "SELECT x FROM c1;"
       rows @?= ([Only "val"] :: [Only Text])

       res' <- migrate' [c1, c2, c3 c3Calls]
       res' @?= Right ()
       calls <- readIORef c3Calls
       calls @?= 1

       res'' <- migrate' [c1, c2, c3 c3Calls]
       res'' @?= Right ()

       calls' <- readIORef c3Calls
       calls' @?= 1
  ]


-------------------------------------------------------------------------------
c1 :: Change SQLiteMigration
c1 = Change (ChangeName "c1") (Just "create table") [] meth
  where
    meth = MigrationQuery q
    q = "CREATE TABLE c1 (id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, x text NOT NULL);"


c2 :: Change SQLiteMigration
c2 = Change (ChangeName "c2") (Just "insert value") [] meth
  where
    meth = MigrationQuery q
    q = "INSERT INTO c1 (x) VALUES ('val');"

-------------------------------------------------------------------------------
c3 :: IORef Int -> Change SQLiteMigration
c3 ref = Change (ChangeName "c3") (Just "bump an IORef") [changeName c1] meth
  where
    meth = MigrationCode (\_ -> Right A.<$> modifyIORef' ref succ)


-------------------------------------------------------------------------------
setup :: IO Connection
setup = open testFile


-------------------------------------------------------------------------------
teardown :: Connection -> IO ()
teardown conn = close conn `finally` dropFile
  where
    dropFile = catchJust
      (\e -> if isDoesNotExistError e then Just () else Nothing)
      (removeFile testFile)
      pure


-------------------------------------------------------------------------------
testFile :: FilePath
testFile = "test.db"
