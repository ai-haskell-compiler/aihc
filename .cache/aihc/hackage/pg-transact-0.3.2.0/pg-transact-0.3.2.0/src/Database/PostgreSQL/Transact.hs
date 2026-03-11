module Database.PostgreSQL.Transact where
import Control.Monad.Trans.Reader
import qualified Database.PostgreSQL.Simple as Simple
import Database.PostgreSQL.Simple (ToRow, FromRow, Connection, SqlError (..))
import Database.PostgreSQL.Simple.Types as Simple
import qualified Database.PostgreSQL.Simple.Transaction as Simple
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Catch
import Data.Int
import Control.Monad
import qualified Data.ByteString as BS
import qualified Control.Monad.Fail as Fail
import Control.Applicative
import Data.Typeable

newtype DBT m a = DBT { unDBT :: ReaderT Connection m a }
  deriving (MonadTrans, MonadThrow)

instance (Applicative m, Semigroup a) => Semigroup (DBT m a) where
  (<>) = liftA2 (<>)

instance (Applicative m, Monoid a) => Monoid (DBT m a) where
  mempty = pure mempty
  mappend = (<>)

type DB = DBT IO

instance Functor m => Functor (DBT m) where
  fmap f = DBT . fmap f . unDBT

instance Applicative m => Applicative (DBT m) where
  pure = DBT . pure
  f <*> v = DBT $ unDBT f <*> unDBT v

instance MonadIO m => MonadIO (DBT m) where
  liftIO = lift . liftIO

instance Monad m => Monad (DBT m) where
  return = lift . return
  DBT m >>= k = DBT $ m >>= unDBT . k

instance Fail.MonadFail m => Fail.MonadFail (DBT m) where
  fail = lift . Fail.fail

isClass25 :: SqlError -> Bool
isClass25 SqlError{..} = BS.take 2 sqlState == "25"

isNoTransaction :: SqlError -> Bool
isNoTransaction SqlError{..} = sqlState == "25P01"

instance (MonadIO m, MonadMask m) => MonadCatch (DBT m) where
  catch (DBT act) handler = DBT $ mask $ \restore -> do
    conn <- ask
    sp   <- liftIO $ Simple.newSavepoint conn
    let setup = catch (restore act) $ \e -> do
                  liftIO $ Simple.rollbackToSavepoint conn sp
                    `catch` (\re -> if isNoTransaction re then pure () else throwM re)
                  if typeRep (Proxy @Abort) == typeOf e
                    then (throwM Abort)
                    else unDBT $ handler e

    setup `finally` liftIO (tryJust (guard . isClass25) (Simple.releaseSavepoint conn sp))

instance (MonadIO m, MonadMask m) => MonadMask (DBT m) where
  mask a = DBT $ mask $ \u -> unDBT (a $ q u)
    where q :: (ReaderT Connection m a -> ReaderT Connection m a) -> DBT m a -> DBT m a
          q u (DBT b) = DBT $ u b

  uninterruptibleMask a =
    DBT $ uninterruptibleMask $ \u -> unDBT (a $ q u)
      where q :: (ReaderT Connection m a -> ReaderT Connection m a) -> DBT m a -> DBT m a
            q u (DBT b) = DBT $ u b

  generalBracket acquire release use = DBT $
    generalBracket
      (unDBT acquire)
      (\resource exitCase -> unDBT (release resource exitCase))
      (\resource -> unDBT (use resource))

getConnection :: Monad m => DBT m Connection
getConnection = DBT ask

runDBT :: MonadBaseControl IO m => DBT m a -> Simple.IsolationLevel -> Connection -> m a
runDBT action level conn
  = control
  $ \run -> Simple.withTransactionLevel level conn
  $ run
  $ runReaderT (unDBT action) conn

runDBTSerializable :: MonadBaseControl IO m => DBT m a -> Connection -> m a
runDBTSerializable action conn
  = control
  $ \run -> Simple.withTransactionSerializable conn
  $ run
  $ runReaderT (unDBT action) conn

runDBTNoTransaction :: DBT m a -> Connection -> m a
runDBTNoTransaction action conn = runReaderT (unDBT action) conn

-- | Perform a @SELECT@ or other SQL query that is expected to return
-- results. All results are retrieved and converted before this
-- function returns.
--
-- When processing large results, this function will consume a lot of
-- client-side memory.  Consider using 'fold' instead.
--
-- Exceptions that may be thrown:
--
-- * 'FormatError': the query string could not be formatted correctly.
--
-- * 'QueryError': the result contains no columns (i.e. you should be
--   using 'execute' instead of 'query').
--
-- * 'ResultError': result conversion failed.
--
-- * 'SqlError':  the postgresql backend returned an error,  e.g.
--   a syntax or type error,  or an incorrect table or column name.
query :: (ToRow a, FromRow b, MonadIO m) => Query -> a -> DBT m [b]
query q x = getConnection >>= \conn -> liftIO $ Simple.query conn q x

-- | A version of 'query' that does not perform query substitution.
query_ :: (FromRow b, MonadIO m) => Query -> DBT m [b]
query_ q = getConnection >>= \conn -> liftIO $ Simple.query_ conn q

-- | Execute an @INSERT@, @UPDATE@, or other SQL query that is not
-- expected to return results.
--
-- Returns the number of rows affected.
--
-- Throws 'FormatError' if the query could not be formatted correctly, or
-- a 'SqlError' exception if the backend returns an error.
execute :: (ToRow q, MonadIO m) => Query -> q -> DBT m Int64
execute q x = getConnection >>= \conn -> liftIO $ Simple.execute conn q x

-- | A version of execute that does not perform query substitution.
execute_ :: MonadIO m => Query -> DBT m Int64
execute_ q = getConnection >>= \conn -> liftIO $ Simple.execute_ conn q

-- | Execute a multi-row @INSERT@, @UPDATE@, or other SQL query that is not
-- expected to return results.
--
-- Returns the number of rows affected.   If the list of parameters is empty,
-- this function will simply return 0 without issuing the query to the backend.
-- If this is not desired, consider using the 'Values' constructor instead.
--
-- Throws 'FormatError' if the query could not be formatted correctly, or
-- a 'SqlError' exception if the backend returns an error.
--
-- For example,  here's a command that inserts two rows into a table
-- with two columns:
--
-- @
-- executeMany [sql|
--     INSERT INTO sometable VALUES (?,?)
--  |] [(1, \"hello\"),(2, \"world\")]
-- @
--
-- Here's an canonical example of a multi-row update command:
--
-- @
-- executeMany [sql|
--     UPDATE sometable
--        SET sometable.y = upd.y
--       FROM (VALUES (?,?)) as upd(x,y)
--      WHERE sometable.x = upd.x
--  |] [(1, \"hello\"),(2, \"world\")]
-- @

executeMany :: (ToRow q, MonadIO m) => Query -> [q] -> DBT m Int64
executeMany q xs = getConnection >>= \conn -> liftIO $ Simple.executeMany conn q xs

-- | Execute @INSERT ... RETURNING@, @UPDATE ... RETURNING@, or other SQL
-- query that accepts multi-row input and is expected to return results.
-- Note that it is possible to write
--    @'query' conn "INSERT ... RETURNING ..." ...@
-- in cases where you are only inserting a single row,  and do not need
-- functionality analogous to 'executeMany'.
--
-- If the list of parameters is empty,  this function will simply return @[]@
-- without issuing the query to the backend.   If this is not desired,
-- consider using the 'Values' constructor instead.
--
-- Throws 'FormatError' if the query could not be formatted correctly.
returning :: (ToRow q, FromRow r, MonadIO m) => Query -> [q] -> DBT m [r]
returning q xs = getConnection >>= \conn -> liftIO $ Simple.returning conn q xs


-- | Format a query string.
--
-- This function is exposed to help with debugging and logging. Do not
-- use it to prepare queries for execution.
--
-- String parameters are escaped according to the character set in use
-- on the 'Connection'.
--
-- Throws 'FormatError' if the query string could not be formatted
-- correctly.
formatQuery :: (ToRow q, MonadIO m) => Query -> q -> DBT m BS.ByteString
formatQuery q xs = getConnection >>= \conn -> liftIO $ Simple.formatQuery conn q xs

queryOne :: (MonadIO m, ToRow a, FromRow b) => Query -> a -> DBT m (Maybe b)
queryOne q x = do
  rows <- query q x
  case rows of
    []  -> return Nothing
    [a] -> return $ Just a
    _   -> return Nothing

queryOne_ :: (MonadIO m, FromRow b) => Query -> DBT m (Maybe b)
queryOne_ q = do
  rows <- query_ q
  case rows of
    []  -> return Nothing
    [x] -> return $ Just x
    _   -> return Nothing

-- | Create a 'Savepoint'.
savepoint :: MonadIO m => DBT m Savepoint
savepoint = getConnection >>= liftIO . Simple.newSavepoint

-- | Release the 'Savepoint' and discard the effects.
rollbackToAndReleaseSavepoint :: MonadIO m => Savepoint -> DBT m ()
rollbackToAndReleaseSavepoint sp = getConnection >>= liftIO . flip Simple.rollbackToAndReleaseSavepoint sp

-- | Run an action and discard the effects but return the result
rollback :: (MonadMask m, MonadIO m) => DBT m a -> DBT m a
rollback actionToRollback = mask $ \restore -> do
  sp <- savepoint
  restore actionToRollback `finally` rollbackToAndReleaseSavepoint sp

data Abort = Abort
  deriving (Show, Eq, Typeable)

instance Exception Abort

-- | A 'abort' is a similar to 'rollback' but calls 'ROLLBACK' to abort the
--   transaction. 'abort's is global. It affects everything before and after
--   it is called. Duplicate 'abort's do nothing.
--   Calling 'abort' throws an 'Abort' exception that is not caught
--   by the transaction running functions. If you call 'abort' you need to
--   also be prepared to handle the 'Abort' exception.
abort :: (MonadMask m, MonadIO m) => DBT m a -> DBT m a
abort = flip finally (throwM Abort)
