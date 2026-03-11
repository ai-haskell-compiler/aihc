{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}

module Test.Database.CQL.IO (tests) where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.IO.Class
import Data.Decimal
import Data.Int
import Data.IP
import Data.List (sort)
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Data.UUID
import Database.CQL.Protocol
import Database.CQL.IO as Client
import System.Environment
import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ

import qualified Data.Set as Set

-----------------------------------------------------------------------------
-- Test Setup

type TestHost = String

tests :: IO TestTree
tests = do
    h <- fromMaybe "localhost" <$> lookupEnv "CASSANDRA_HOST"
    initSchema h
    fmap (testGroup "Database.CQL.IO") $
        forM versions (\v -> do
            c <- Client.init (settings h v)
            return $ testGroup (show v) (cqlTests c))

versions :: [Version]
versions = [V3, V4]

settings :: TestHost -> Version -> Settings
settings h v = setContacts h []
             . setProtocolVersion v
             . setLogger (stdoutLogger LogInfo)
             $ defSettings

initSchema :: TestHost -> IO ()
initSchema h = do
    c <- Client.init (settings h V4)
    runClient c $ do
        dropKeyspace
        createKeyspace
        createTables
    shutdown c

test :: ClientState -> String -> Client () -> TestTree
test c name runTest =
    testCase name $
        runClient c $ do
            truncateTables
            runTest

-----------------------------------------------------------------------------
-- Test Schema

-- Columns of cqltest.table1
type Ty1 =
    ( Int64
    , Ascii
    , Blob
    , Bool
    , Decimal
    , Double
    , Float
    , Int32
    , UTCTime
    , UUID
    , Text
    , Integer
    , TimeUuid
    , IP
    )

-- Columns of cqltest.table2
type Ty2 =
    ( Int64
    , [Int32]
    , Set Ascii
    , Map Ascii Int32
    , Maybe Int32
    , (Bool, Ascii, Int32)
    , Map Int32 (Map Int32 (Set Ascii))
    )

createKeyspace :: Client ()
createKeyspace = void $ schema cql (params ())
  where
    cql :: QueryString S () ()
    cql = [r| create keyspace if not exists cqltest
                with replication = {
                    'class': 'SimpleStrategy',
                    'replication_factor': '1'
                } |]

dropKeyspace :: Client ()
dropKeyspace = void $ schema cql (params ())
  where
    cql :: QueryString S () ()
    cql = "drop keyspace if exists cqltest"

createTables :: Client ()
createTables = forM_ [cql1, cql2, cql3] $ \q ->
    void $ schema q (params ())
  where
    cql1, cql2, cql3 :: QueryString S () ()
    cql1 = [r|
        create table if not exists cqltest.test1
            ( a bigint
            , b ascii
            , c blob
            , d boolean
            , e decimal
            , f double
            , g float
            , h int
            , i timestamp
            , j uuid
            , k varchar
            , l varint
            , m timeuuid
            , n inet
            , primary key (a)
            ) |]

    cql2 = [r|
        create table if not exists cqltest.test2
            ( a bigint
            , b list<int>
            , c set<ascii>
            , d map<ascii,int>
            , e int
            , f tuple<boolean,ascii,int>
            , g map<int,frozen<map<int,set<ascii>>>>
            , primary key (a)
            ) |]

    cql3 = [r|
        create table if not exists cqltest.counters
            ( a bigint
            , n counter
            , primary key (a)
            ) |]

truncateTables :: Client ()
truncateTables = forM_ [cql1, cql2, cql3] $ \q ->
    void $ schema q (params ())
  where
    cql1, cql2, cql3 :: QueryString S () ()
    cql1 = "truncate table cqltest.test1"
    cql2 = "truncate table cqltest.test2"
    cql3 = "truncate table cqltest.counters"

-----------------------------------------------------------------------------
-- Tests

cqlTests :: ClientState -> [TestTree]
cqlTests c =
    [ test c "write-read" testWriteRead
    , test c "write-read-ttl" testWriteReadTtl
    , test c "trans" testTrans
    , test c "paging" testPaging
    , test c "batch" testBatch
    , test c "batch-counter" testBatchCounter
    ]

testWriteRead :: Client ()
testWriteRead = do
    t <- liftIO $ fmap (\x -> x { utctDayTime = secondsToDiffTime 3600 }) getCurrentTime
    let a = ( 4835637638
            , "hello world"
            , Blob "blooooooooooooooooooooooob"
            , False
            , 1.2342342342423423423423423442
            , 433243.13
            , 1.23
            , 2342342
            , t
            , fromJust (fromString "af93aafe-dea5-4427-bea4-8d7872507efb")
            , "sdfsdžȢぴせそぼξλж҈Ҵאבג"
            , 8763847563478568734687345683765873458734
            , TimeUuid . fromJust $ fromString "559ab19e-52d8-11e3-a847-270bf6910c08"
            , read "127.0.0.1"
            )
    let b = ( 4835637638
            , [1,2,3]
            , Set ["peter", "paul", "mary"]
            , Map [("peter", 1), ("paul", 2), ("mary", 3)]
            , Just 42
            , (True, "ascii", 42)
            , Map [(1, Map [(1, Set ["ascii"])])
                  ,(2, Map [(2, Set ["ascii", "text"])])
                  ]
            )
    write ins1 (params a)
    write ins2 (params b)
    x <- fromJust <$> query1 get1 (params (Identity 4835637638))
    y <- fromJust <$> query1 get2 (params (Identity 4835637638))
    liftIO $ do
        a @=? x
        b @=? y
  where
    ins1 :: PrepQuery W Ty1 ()
    ins1 = [r|
        insert into cqltest.test1
            (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
        values
            (?,?,?,?,?,?,?,?,?,?,?,?,?,?) |]

    ins2 :: PrepQuery W Ty2 ()
    ins2 = [r|
        insert into cqltest.test2
            (a,b,c,d,e,f,g)
        values
            (?,?,?,?,?,?,?) |]

    get1 :: PrepQuery R (Identity Int64) Ty1
    get1 = "select a,b,c,d,e,f,g,h,i,j,k,l,m,n from cqltest.test1 where a = ?"

    get2 :: PrepQuery R (Identity Int64) Ty2
    get2 = "select a,b,c,d,e,f,g from cqltest.test2 where a = ?"

testWriteReadTtl :: Client ()
testWriteReadTtl = do
    write ins (params (1000, True))
    (d, ttl) <- fromJust <$> query1 get (params (Identity 1000))
    liftIO $ do
        assertBool "d == False" d
        assertBool "TTL > 0" (ttl > Just 0)
  where
    ins :: PrepQuery W (Int64, Bool) ()
    ins = "insert into cqltest.test1 (a,d) values (?,?) using ttl 3600"

    get :: PrepQuery R (Identity Int64) (Bool, Maybe Int32)
    get = "select d, ttl(d) from cqltest.test1 where a = ?"

testTrans :: Client ()
testTrans = do
    -- 1st insert (success)
    _row <- head <$> trans ins (params (1, "ascii-1"))
    assertApplied _row

    -- 2nd insert (conflict)
    _row <- head <$> trans ins (params (1, "ascii-1"))
    liftIO $ do
        rowLength _row @?= 15 -- [applied] + full existing row
        fromRow 0 _row @?= Right (Just False)             -- [applied]
        fromRow 1 _row @?= Right (Just (1 :: Int64))      -- a
        fromRow 2 _row @?= Right (Just (Ascii "ascii-1")) -- b
        -- remaining columns with null values (since none were inserted)
        let vnull = Nothing :: Maybe Blob -- type irrelevant
        map (($ _row) . fromRow) [3..14] @?= replicate 12 (Right vnull)

    -- 1st update (success)
    _row <- head <$> trans upd (params ("ascii-2", 1, "ascii-1"))
    assertApplied _row

    -- 2nd update (conflict)
    _row <- head <$> trans upd (params ("ascii-2", 1, "ascii-1"))
    liftIO $ do
        rowLength _row @?= 2 -- [applied] + conflicting value
        fromRow 0 _row @?= Right (Just False)             -- [applied]
        fromRow 1 _row @?= Right (Just (Ascii "ascii-2")) -- b
  where
    ins :: PrepQuery W (Int64, Text) Row
    ins = "insert into cqltest.test1 (a,b) values (?,?) if not exists"

    upd :: PrepQuery W (Text, Int64, Text) Row
    upd = "update cqltest.test1 set b = ? where a = ? if b = ?"

    assertApplied row = liftIO $ do
        rowLength row @?= 1 -- [applied]
        fromRow 0 row @?= Right (Just True)

testPaging :: Client ()
testPaging = do
    let dat = zip [1..101] (repeat "b")
    mapM_ (write ins . params) dat
    p <- paginate qry $ (params ()) { pageSize = Just 10 }
    assertPages 11 (Set.fromList dat) p
  where
    ins :: PrepQuery W (Int64, Ascii) ()
    ins = "insert into cqltest.test1 (a,b) values (?,?)"

    qry :: PrepQuery R () (Int64, Ascii)
    qry = "select a,b from cqltest.test1"

testBatch :: Client ()
testBatch = do
    exec $ setType BatchLogged
    exec $ setType BatchUnLogged
    exec $ setSerialConsistency SerialConsistency
    exec $ setSerialConsistency LocalSerialConsistency
  where
    exec configure = do
        batch $ configure >> forM_ dat (addQuery ins)
        rs <- query qry (params ())
        liftIO $ sort rs @?= dat
        truncateTables

    dat :: [(Int64, Ascii)]
    dat = [(1, "1"), (2, "2"), (3, "3")]

    ins :: QueryString W (Int64, Ascii) ()
    ins = "insert into cqltest.test1 (a,b) values (?,?)"

    qry :: PrepQuery R () (Int64, Ascii)
    qry = "select a,b from cqltest.test1"

testBatchCounter :: Client ()
testBatchCounter = exec >> total 3 >> exec >> total 6
  where
    exec = batch $ do
        setType BatchCounter
        addQuery upd (Identity 1)
        addQuery upd (Identity 2)
        addQuery upd (Identity 3)

    total n = do
        rs <- query qry (params ())
        let n' = sum (map (fromCounter . runIdentity) rs)
        liftIO $ n @=? n'

    upd :: QueryString W (Identity Int64) ()
    upd = "update cqltest.counters set n = n + 1 where a = ?"

    qry :: PrepQuery R () (Identity Counter)
    qry = "select n from cqltest.counters"

-----------------------------------------------------------------------------
-- Utilities

assertPages :: (Ord a, Show a) => Int -> Set.Set a -> Page a -> Client ()
assertPages numPages expected p = do
    let got = Set.fromList (result p)
    let remaining = Set.difference expected got
    liftIO $ hasMore p @?= numPages > 1
    liftIO $ got `Set.isSubsetOf` expected @?= True
    if numPages > 1
        then nextPage p >>= assertPages (numPages - 1) remaining
        else liftIO $ remaining @?= Set.empty

params :: Tuple a => a -> QueryParams a
params p = QueryParams
    { consistency       = One
    , skipMetaData      = False
    , values            = p
    , pageSize          = Nothing
    , queryPagingState  = Nothing
    , serialConsistency = Nothing
    , enableTracing     = Nothing
    }

