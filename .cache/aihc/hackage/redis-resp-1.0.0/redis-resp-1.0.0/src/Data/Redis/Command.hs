-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}

module Data.Redis.Command
    ( -- * Types
      Redis
    , PubSub
    , Command       (..)
    , PubSubCommand (..)
    , PushMessage   (..)
    , Result
    , RedisError    (..)
    , RedisType     (..)
    , TTL           (..)
    , Side          (..)
    , Choose        (..)
    , Aggregate     (..)
    , Min           (..)
    , Max           (..)
    , ScoreList     (..)
    , Seconds       (..)
    , Milliseconds  (..)
    , Timestamp     (..)
    , Field
    , Index
    , Key           (..)

    -- ** Cursor
    , Cursor
    , zero

    -- ** Non-empty lists
    , one

    -- ** Options
    , Opts
    , none

    -- ** Bit
    , BitStart
    , BitEnd
    , start
    , end

    -- * Commands
    -- ** Connection
    , auth
    , echo
    , ping
    , quit
    , select

    -- ** Server
    , bgrewriteaof
    , bgsave
    , dbsize
    , flushall
    , flushdb
    , lastsave
    , save

    -- ** Transactions
    , discard
    , exec
    , multi
    , unwatch
    , watch

    -- ** Keys
    , del
    , dump
    , exists
    , expire
    , expireat
    , keys
    , persist
    , randomkey
    , rename
    , renamenx
    , ttl
    , typeof

    -- ** Strings
    , append
    , decr
    , decrby
    , get
    , getrange
    , getset
    , incr
    , incrby
    , incrbyfloat
    , mget
    , mset
    , msetnx
    , set, ex, px, xx, nx
    , setrange
    , strlen

    -- ** Bits
    , bitand
    , bitcount, range
    , bitnot
    , bitor
    , bitpos
    , bitxor
    , getbit
    , setbit

    -- ** Hashes
    , hdel
    , hexists
    , hget
    , hgetall
    , hincrby
    , hincrbyfloat
    , hkeys
    , hlen
    , hmget
    , hmset
    , hset
    , hsetnx
    , hvals

    -- ** Lists
    , blpop
    , brpop
    , brpoplpush
    , lindex
    , linsert
    , llen
    , lpop
    , lpush
    , lpushx
    , lrange
    , lrem
    , lset
    , ltrim
    , rpop
    , rpoplpush
    , rpush
    , rpushx

    -- ** Sets
    , sadd
    , scard
    , sdiff
    , sdiffstore
    , sinter
    , sinterstore
    , sismember
    , smembers
    , smove
    , spop
    , srandmember
    , srem
    , sunion
    , sunionstore

    -- ** Sorted Sets
    , zadd
    , zcard
    , zcount
    , zincrby
    , zinterstore
    , zlexcount
    , zrange
    , zrangebylex
    , zrangebyscore
    , zrank
    , zrem
    , zremrangebylex
    , zremrangebyrank
    , zremrangebyscore
    , zrevrangebyscore
    , zrevrange
    , zrevrank
    , zscore
    , zunionstore

    -- ** HyperLogLog
    , pfadd
    , pfcount
    , pfmerge

    -- ** Scan
    , scan, match, count
    , hscan
    , sscan
    , zscan

    -- ** Sort
    , sort, by, limit, getkey, asc, desc, alpha, store

    -- ** Pub/Sub
    , publish
    , subscribe
    , psubscribe
    , unsubscribe
    , punsubscribe

    -- * Response Reading
    , readInt
    , readInt'Null
    , readBool
    , readTTL
    , readBulk'Null
    , readBulk
    , readListOfMaybes
    , readList
    , readScoreList
    , readFields
    , readKeyValue
    , readBulk'Array
    , readScan
    , matchStr
    , readType
    , fromSet
    , anyStr
    , readPushMessage

    -- * Re-exports
    , NonEmpty (..)
    , NE.nonEmpty
    ) where

import Control.Exception (Exception)
import Control.Monad.Operational
import Data.ByteString.Builder (int64Dec)
import Data.ByteString.Builder.Extra
import Data.ByteString.Conversion
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.DList (DList, cons)
import Data.Double.Conversion.ByteString (toShortest)
import Data.Foldable (toList, foldr)
import Data.Int
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.List.Split (chunksOf)
import Data.Maybe (maybeToList)
import Data.Redis.Resp
import Data.Semigroup (Semigroup, (<>))
import Data.String
import Data.Typeable
import GHC.TypeLits
import Prelude hiding (foldr, readList)

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.DList           as DL
import qualified Data.List.NonEmpty   as NE

-- | Redis error type.
data RedisError
    = RedisError !ByteString
        -- ^ General error case.
    | InvalidResponse !String
        -- ^ The received response is invalid or unexpected (e.g. a bulk
        -- string instead of an integer).
    | InvalidConversion !String
        -- ^ ByteString conversion using 'FromByteString' failed.
    deriving (Eq, Ord, Show, Typeable)

instance Exception RedisError

type Redis  = ProgramT Command
type PubSub = ProgramT PubSubCommand
type Result = Either RedisError

-- | Redis commands.
data Command :: * -> * where
    -- Connection
    Ping   :: Resp -> Command ()
    Echo   :: FromByteString a => Resp -> Command a
    Auth   :: Resp -> Command ()
    Quit   :: Resp -> Command ()
    Select :: Resp -> Command ()

    -- Server
    BgRewriteAOF :: Resp -> Command ()
    BgSave       :: Resp -> Command ()
    Save         :: Resp -> Command ()
    DbSize       :: Resp -> Command Int64
    FlushAll     :: Resp -> Command ()
    FlushDb      :: Resp -> Command ()
    LastSave     :: Resp -> Command Int64

    -- Transactions
    Multi   :: Resp -> Command ()
    Watch   :: Resp -> Command ()
    Unwatch :: Resp -> Command ()
    Discard :: Resp -> Command ()
    Exec    :: Resp -> Command ()

    -- Keys
    Del       :: Resp -> Command Int64
    Dump      :: Resp -> Command (Maybe ByteString)
    Exists    :: Resp -> Command Bool
    Expire    :: Resp -> Command Bool
    ExpireAt  :: Resp -> Command Bool
    Persist   :: Resp -> Command Bool
    Keys      :: Resp -> Command [Key]
    RandomKey :: Resp -> Command (Maybe Key)
    Rename    :: Resp -> Command ()
    RenameNx  :: Resp -> Command Bool
    Sort      :: FromByteString a => Resp -> Command [a]
    Ttl       :: Resp -> Command (Maybe TTL)
    Type      :: Resp -> Command (Maybe RedisType)
    Scan      :: FromByteString a => Resp -> Command (Cursor, [a])

    -- Strings
    Append   :: Resp -> Command Int64
    Get      :: FromByteString a => Resp -> Command (Maybe a)
    GetRange :: FromByteString a => Resp -> Command a
    GetSet   :: FromByteString a => Resp -> Command (Maybe a)
    MGet     :: FromByteString a => Resp -> Command [Maybe a]
    MSet     :: Resp -> Command ()
    MSetNx   :: Resp -> Command Bool
    Set      :: Resp -> Command Bool
    SetRange :: Resp -> Command Int64
    StrLen   :: Resp -> Command Int64

    -- Bits
    BitAnd   :: Resp -> Command Int64
    BitCount :: Resp -> Command Int64
    BitNot   :: Resp -> Command Int64
    BitOr    :: Resp -> Command Int64
    BitPos   :: Resp -> Command Int64
    BitXOr   :: Resp -> Command Int64
    GetBit   :: Resp -> Command Int64
    SetBit   :: Resp -> Command Int64

    -- Numeric
    Decr        :: Resp -> Command Int64
    DecrBy      :: Resp -> Command Int64
    Incr        :: Resp -> Command Int64
    IncrBy      :: Resp -> Command Int64
    IncrByFloat :: Resp -> Command Double

    -- Hashes
    HDel         :: Resp -> Command Int64
    HExists      :: Resp -> Command Bool
    HGet         :: FromByteString a => Resp -> Command (Maybe a)
    HGetAll      :: FromByteString a => Resp -> Command [(Field, a)]
    HIncrBy      :: Resp -> Command Int64
    HIncrByFloat :: Resp -> Command Double
    HKeys        :: Resp -> Command [Field]
    HLen         :: Resp -> Command Int64
    HMGet        :: FromByteString a => Resp -> Command [Maybe a]
    HMSet        :: Resp -> Command ()
    HSet         :: Resp -> Command Bool
    HSetNx       :: Resp -> Command Bool
    HVals        :: FromByteString a => Resp -> Command [a]
    HScan        :: FromByteString a => Resp -> Command (Cursor, [a])

    -- Lists
    BLPop      :: FromByteString a => Int64 -> Resp -> Command (Maybe (Key, a))
    BRPop      :: FromByteString a => Int64 -> Resp -> Command (Maybe (Key, a))
    BRPopLPush :: FromByteString a => Int64 -> Resp -> Command (Maybe a)
    LIndex     :: FromByteString a => Resp -> Command (Maybe a)
    LInsert    :: Resp -> Command Int64
    LLen       :: Resp -> Command Int64
    LPop       :: FromByteString a => Resp -> Command (Maybe a)
    LPush      :: Resp -> Command Int64
    LPushX     :: Resp -> Command Int64
    LRange     :: FromByteString a => Resp -> Command [a]
    LRem       :: Resp -> Command Int64
    LSet       :: Resp -> Command ()
    LTrim      :: Resp -> Command ()
    RPop       :: FromByteString a => Resp -> Command (Maybe a)
    RPopLPush  :: FromByteString a => Resp -> Command (Maybe a)
    RPush      :: Resp -> Command Int64
    RPushX     :: Resp -> Command Int64

    -- Sets
    SAdd        :: Resp -> Command Int64
    SCard       :: Resp -> Command Int64
    SDiff       :: FromByteString a => Resp -> Command [a]
    SDiffStore  :: Resp -> Command Int64
    SInter      :: FromByteString a => Resp -> Command [a]
    SInterStore :: Resp -> Command Int64
    SIsMember   :: Resp -> Command Bool
    SMembers    :: FromByteString a => Resp -> Command [a]
    SMove       :: Resp -> Command Bool
    SPop        :: FromByteString a => Resp -> Command (Maybe a)
    SRandMember :: FromByteString a => Choose -> Resp -> Command [a]
    SRem        :: Resp -> Command Int64
    SScan       :: FromByteString a => Resp -> Command (Cursor, [a])
    SUnion      :: FromByteString a => Resp -> Command [a]
    SUnionStore :: Resp -> Command Int64

    -- Sorted Sets
    ZAdd             :: Resp -> Command Int64
    ZCard            :: Resp -> Command Int64
    ZCount           :: Resp -> Command Int64
    ZIncrBy          :: Resp -> Command Double
    ZInterStore      :: Resp -> Command Int64
    ZLexCount        :: Resp -> Command Int64
    ZRange           :: FromByteString a => Bool -> Resp -> Command (ScoreList a)
    ZRangeByLex      :: FromByteString a => Resp -> Command [a]
    ZRangeByScore    :: FromByteString a => Bool -> Resp -> Command (ScoreList a)
    ZRank            :: Resp -> Command (Maybe Int64)
    ZRem             :: Resp -> Command Int64
    ZRemRangeByLex   :: Resp -> Command Int64
    ZRemRangeByRank  :: Resp -> Command Int64
    ZRemRangeByScore :: Resp -> Command Int64
    ZRevRange        :: FromByteString a => Bool -> Resp -> Command (ScoreList a)
    ZRevRangeByScore :: FromByteString a => Bool -> Resp -> Command (ScoreList a)
    ZRevRank         :: Resp -> Command (Maybe Int64)
    ZScan            :: FromByteString a => Resp -> Command (Cursor, [a])
    ZScore           :: Resp -> Command (Maybe Double)
    ZUnionStore      :: Resp -> Command Int64

    -- HyperLogLog
    PfAdd   :: Resp -> Command Bool
    PfCount :: Resp -> Command Int64
    PfMerge :: Resp -> Command ()

    -- Pub/Sub
    Publish :: Resp -> Command Int64

-- | Pub/Sub commands.
data PubSubCommand r where
    Subscribe    :: Resp -> PubSubCommand ()
    Unsubscribe  :: Resp -> PubSubCommand ()
    PSubscribe   :: Resp -> PubSubCommand ()
    PUnsubscribe :: Resp -> PubSubCommand ()

-- | Messages which are published to subscribers.
data PushMessage
    = SubscribeMessage
        { channel       :: !ByteString
        , subscriptions :: !Int64
        }
    | UnsubscribeMessage
        { channel       :: !ByteString
        , subscriptions :: !Int64
        }
    | Message
        { channel :: !ByteString
        , message :: !ByteString
        }
    | PMessage
        { pattern :: !ByteString
        , channel :: !ByteString
        , message :: !ByteString
        }
    deriving (Eq, Ord, Show)

-- | The types redis reports via <http://redis.io/commands/type type>.
data RedisType
    = RedisString
    | RedisList
    | RedisSet
    | RedisZSet
    | RedisHash
    deriving (Eq, Ord, Show)

-- | A type representing time-to-live values.
data TTL = NoTTL | TTL !Int64
    deriving (Eq, Ord, Show)

-- | Used in 'linsert' to specify the insertion point.
data Side = Before | After
    deriving (Eq, Ord, Show)

data Choose
    = One         -- ^ Exactly one element
    | Dist !Int64 -- ^ @n@ distint elements
    | Arb  !Int64 -- ^ @n@ arbitrary (i.e. potentially repeated) elements
    deriving (Eq, Ord, Show)

data Aggregate
    = None -- ^ no aggregation
    | Min  -- ^ take the minimum score
    | Max  -- ^ take the maximum score
    | Sum  -- ^ addition of scores
    deriving (Eq, Ord, Show)

data Min
    = MinIncl !ByteString -- ^ lower bound (inclusive)
    | MinExcl !ByteString -- ^ lower bound (exclusive)
    | MinInf              -- ^ infinite lower bound
    deriving (Eq, Ord, Show)

data Max
    = MaxIncl !ByteString -- ^ upper bound (inclusive)
    | MaxExcl !ByteString -- ^ upper bound (exclusive)
    | MaxInf              -- ^ infinite upper bound
    deriving (Eq, Ord, Show)

data ScoreList a = ScoreList
    { scores   :: [Double]
    , elements :: [a]
    } deriving (Eq, Ord, Show)

newtype Cursor = Cursor
    { cursor :: ByteString
    } deriving (Eq, Ord, Show, FromByteString)

zero :: Cursor
zero = Cursor "0"

type Field = ByteString
type Index = Int64

-- | Redis key type
newtype Key = Key
    { key :: ByteString
    } deriving (Eq, Ord, Show, FromByteString)

instance IsString Key where
    fromString = Key . fromString

-- | Command options
data Opts (a :: Symbol) = Opts { len :: !Int, opts :: DList ByteString }

instance Semigroup (Opts a) where
    Opts x a <> Opts y b = Opts (x + y) (a `DL.append` b)

instance Monoid (Opts a) where
    mempty = Opts 0 DL.empty
    mappend = (<>)

none :: Monoid m => m
none = mempty

newtype Seconds      = Seconds Int64
newtype Milliseconds = Milliseconds Int64
newtype Timestamp    = Timestamp Int64
newtype BitStart     = BitStart ByteString
newtype BitEnd       = BitEnd   ByteString

instance Semigroup BitStart where
    _ <> b = b

instance Monoid BitStart where
    mempty  = BitStart ""
    mappend = (<>)

instance Semigroup BitEnd where
    _ <> b = b

instance Monoid BitEnd where
    mempty  = BitEnd ""
    mappend = (<>)

start :: Int64 -> BitStart
start = BitStart . int2bytes

end :: Int64 -> BitEnd
end = BitEnd . int2bytes

one :: a -> NonEmpty a
one a = a :| []

-----------------------------------------------------------------------------
-- Connection

ping :: Monad m => Redis m ()
ping = singleton $ Ping $ cmd 1 ["PING"]

echo :: (Monad m, ToByteString a, FromByteString a) => a -> Redis m a
echo x = singleton $ Echo $ cmd 2 ["ECHO", toByteString x]

auth :: Monad m => ByteString -> Redis m ()
auth x = singleton $ Auth $ cmd 2 ["AUTH", x]

quit :: Monad m => Redis m ()
quit = singleton $ Quit $ cmd 1 ["QUIT"]

select :: Monad m => Int64 -> Redis m ()
select x = singleton $ Select $ cmd 2 ["SELECT", int2bytes x]

-----------------------------------------------------------------------------
-- Server

bgrewriteaof :: Monad m => Redis m ()
bgrewriteaof = singleton $ BgRewriteAOF $ cmd 1 ["BGREWRITEAOF"]

bgsave :: Monad m => Redis m ()
bgsave = singleton $ BgSave $ cmd 1 ["BGSAVE"]

save :: Monad m => Redis m ()
save = singleton $ Save $ cmd 1 ["SAVE"]

flushall :: Monad m => Redis m ()
flushall = singleton $ FlushAll $ cmd 1 ["FLUSHALL"]

flushdb :: Monad m => Redis m ()
flushdb = singleton $ FlushDb $ cmd 1 ["FLUSHDB"]

lastsave :: Monad m => Redis m Int64
lastsave = singleton $ LastSave $ cmd 1 ["LASTSAVE"]

dbsize :: Monad m => Redis m Int64
dbsize = singleton $ DbSize $ cmd 1 ["DBSIZE"]

-----------------------------------------------------------------------------
-- Transactions

-- | Note that all commands following 'multi' and until 'exec' are queued by
-- a Redis server. Therefore the result of any such command is not available
-- until the exec command completes. For example, the following is an invalid
-- 'Redis' program:
--
-- @
--  multi
--  x <- hexists \"FOO\" \"BAR\"
--  unless x (void $ hset \"FOO\" \"BAR\" 1)
--  exec
-- @
--
-- This pattern is usually indicative of the desire for a transactional
-- check-and-set operation, which may be achieved instead by the following
-- valid command sequence:
--
-- @
--  watch (\"FOO\" R.:| [])
--  x <- hexists \"FOO\" \"BAR\"
--  multi
--  unless x (void $ hset \"FOO\" \"BAR\" 1)
--  exec
-- @
--
--
-- For more information on Redis transactions and conditional updates,
-- see https://redis.io/topics/transactions.
multi :: Monad m => Redis m ()
multi = singleton $ Multi $ cmd 1 ["MULTI"]

discard :: Monad m => Redis m ()
discard = singleton $ Discard $ cmd 1 ["DISCARD"]

unwatch :: Monad m => Redis m ()
unwatch = singleton $ Unwatch $ cmd 1 ["UNWATCH"]

watch :: Monad m => NonEmpty Key -> Redis m ()
watch kk = singleton $ Watch $ cmd (1 + NE.length kk) $ "WATCH" : map key (toList kk)

exec :: Monad m => Redis m ()
exec = singleton $ Exec $ cmd 1 ["EXEC"]

-----------------------------------------------------------------------------
-- Keys

dump :: Monad m => Key -> Redis m (Maybe ByteString)
dump k = singleton $ Dump $ cmd 2 ["DUMP", key k]

exists :: Monad m => Key -> Redis m Bool
exists k = singleton $ Exists $ cmd 2 ["EXISTS", key k]

del :: Monad m => NonEmpty Key -> Redis m Int64
del kk = singleton $ Del $ cmd (1 + NE.length kk) $ "DEL" : map key (toList kk)

expire :: Monad m => Key -> Seconds -> Redis m Bool
expire k (Seconds n) = singleton $ Expire $ cmd 3 ["EXPIRE", key k, int2bytes n]

expireat :: Monad m => Key -> Timestamp -> Redis m Bool
expireat k (Timestamp n) = singleton $ ExpireAt $ cmd 3 ["EXPIREAT", key k, int2bytes n]

persist :: Monad m => Key -> Redis m Bool
persist k = singleton $ Persist $ cmd 2 ["PERSIST", key k]

keys :: Monad m => ByteString -> Redis m [Key]
keys pat = singleton $ Keys $ cmd 2 ["KEYS", pat]

randomkey :: Monad m => Redis m (Maybe Key)
randomkey = singleton $ RandomKey $ cmd 1 ["RANDOMKEY"]

rename :: Monad m => Key -> Key -> Redis m ()
rename a b = singleton $ Rename $ cmd 3 ["RENAME", key a, key b]

renamenx :: Monad m => Key -> Key -> Redis m Bool
renamenx a b = singleton $ RenameNx $ cmd 3 ["RENAMENX", key a, key b]

ttl :: Monad m => Key -> Redis m (Maybe TTL)
ttl k = singleton $ Ttl $ cmd 2 ["TTL", key k]

typeof :: Monad m => Key -> Redis m (Maybe RedisType)
typeof k = singleton $ Type $ cmd 2 ["TYPE", key k]

-----------------------------------------------------------------------------
-- Strings

get :: (Monad m, FromByteString a) => Key -> Redis m (Maybe a)
get k = singleton $ Get $ cmd 2 ["GET", key k]

set :: (Monad m, ToByteString a) => Key -> a -> Opts "SET" -> Redis m Bool
set k v o = singleton $ Set $ cmd (3 + len o) $ "SET" : key k : toByteString v : toList (opts o)

ex :: Seconds -> Opts "SET"
ex (Seconds i) = Opts 2 $ "EX" `cons` DL.singleton (int2bytes i)

px :: Milliseconds -> Opts "SET"
px (Milliseconds i) = Opts 2 $ "PX" `cons` DL.singleton (int2bytes i)

xx :: Opts "SET"
xx = Opts 1 $ DL.singleton "XX"

nx :: Opts "SET"
nx = Opts 1 $ DL.singleton "NX"

getset :: (Monad m, ToByteString a, FromByteString b) => Key -> a -> Redis m (Maybe b)
getset k v = singleton $ GetSet $ cmd 3 ["GETSET", key k, toByteString v]

mget :: (Monad m, FromByteString a) => NonEmpty Key -> Redis m [Maybe a]
mget kk = singleton $ MGet $ cmd (1 + NE.length kk) $ "MGET" : map key (toList kk)

mset :: (Monad m, ToByteString a) => NonEmpty (Key, a) -> Redis m ()
mset kv = singleton $ MSet $ cmd (1 + 2 * NE.length kv) $ "MSET" : foldr f [] kv
  where
    f (k, v) acc = key k : toByteString v : acc

msetnx :: (Monad m, ToByteString a) => NonEmpty (Key, a) -> Redis m Bool
msetnx kv = singleton $ MSetNx $ cmd (1 + 2 * NE.length kv) $ "MSETNX" : foldr f [] kv
  where
    f (k, v) acc = key k : toByteString v : acc

getrange :: (Monad m, FromByteString a) => Key -> Int64 -> Int64 -> Redis m a
getrange k a b = singleton $ GetRange $ cmd 4 ["GETRANGE", key k, int2bytes a, int2bytes b]

setrange :: (Monad m, ToByteString a) => Key -> Int64 -> a -> Redis m Int64
setrange k i a = singleton $ SetRange $ cmd 4 ["SETRANGE", key k, int2bytes i, toByteString a]

append :: (Monad m, ToByteString a) => Key -> a -> Redis m Int64
append k v = singleton $ Append $ cmd 3 ["APPEND", key k, toByteString v]

strlen :: Monad m => Key -> Redis m Int64
strlen k = singleton $ StrLen $ cmd 2 ["STRLEN", key k]

decr :: Monad m => Key -> Redis m Int64
decr k = singleton $ Decr $ cmd 2 ["DECR", key k]

decrby :: Monad m => Key -> Int64 -> Redis m Int64
decrby k v = singleton $ DecrBy $ cmd 3 ["DECRBY", key k, int2bytes v]

incr :: Monad m => Key -> Redis m Int64
incr k = singleton $ Incr $ cmd 2 ["INCR", key k]

incrby :: Monad m => Key -> Int64 -> Redis m Int64
incrby k v = singleton $ IncrBy $ cmd 3 ["INCRBY", key k, int2bytes v]

incrbyfloat :: Monad m => Key -> Double -> Redis m Double
incrbyfloat k v = singleton $ IncrByFloat $ cmd 3 ["INCRBYFLOAT", key k, dbl2bytes v]

-----------------------------------------------------------------------------
-- Bits

bitcount :: Monad m => Key -> Opts "RANGE" -> Redis m Int64
bitcount k o = singleton $ BitCount $ cmd (2 + len o) $ "BITCOUNT" : key k : toList (opts o)

range :: Int64 -> Int64 -> Opts "RANGE"
range x y = Opts 2 $ int2bytes x `cons` DL.singleton (int2bytes y)

bitand :: Monad m => Key -> NonEmpty Key -> Redis m Int64
bitand k kk = singleton $ BitAnd $ bitop "AND" (k <| kk)

bitor :: Monad m => Key -> NonEmpty Key -> Redis m Int64
bitor k kk = singleton $ BitOr $ bitop "OR" (k <| kk)

bitxor :: Monad m => Key -> NonEmpty Key -> Redis m Int64
bitxor k kk = singleton $ BitXOr $ bitop "XOR" (k <| kk)

bitnot :: Monad m => Key -> Key -> Redis m Int64
bitnot k l = singleton $ BitNot $ bitop "NOT" (k :| [l])

bitop :: ByteString -> NonEmpty Key -> Resp
bitop o kk = cmd (2 + NE.length kk) $ "BITOP" : o : map key (toList kk)
{-# INLINE bitop #-}

bitpos :: Monad m => Key -> Bool -> BitStart -> BitEnd -> Redis m Int64
bitpos k b (BitStart s) (BitEnd e) =
    let args = filter (not . Lazy.null) [s, e] in
    singleton $ BitPos $ cmd (3 + length args) $ "BITPOS" : key k : toBit b : args
  where
    toBit True  = "1"
    toBit False = "0"

getbit :: Monad m => Key -> Int64 -> Redis m Int64
getbit k o = singleton $ GetBit $ cmd 3 ["GETBIT", key k, int2bytes o]

setbit :: Monad m => Key -> Int64 -> Bool -> Redis m Int64
setbit k o b = singleton $ SetBit $ cmd 4 ["SETBIT", key k, int2bytes o, toBit b]
  where
    toBit True  = "1"
    toBit False = "0"

-----------------------------------------------------------------------------
-- Hashes

hget :: (Monad m, FromByteString a) => Key -> Field -> Redis m (Maybe a)
hget h k = singleton $ HGet $ cmd 3 ["HGET", key h, k]

hgetall :: (Monad m, FromByteString a) => Key -> Redis m [(Field, a)]
hgetall h = singleton $ HGetAll $ cmd 2 ["HGETALL", key h]

hvals :: (Monad m, FromByteString a) => Key -> Redis m [a]
hvals h = singleton $ HVals $ cmd 2 ["HVALS", key h]

hmget :: (Monad m, FromByteString a) => Key -> NonEmpty Field -> Redis m [Maybe a]
hmget h kk = singleton $ HMGet $ cmd (2 + NE.length kk) $ "HMGET" : key h : toList kk

hmset :: (Monad m, ToByteString a) => Key -> NonEmpty (Field, a) -> Redis m ()
hmset h kv = singleton $ HMSet $ cmd (2 + 2 * NE.length kv) $ "HMSET" : key h : foldr f [] kv
  where
    f (k, v) acc = k : toByteString v : acc

hset :: (Monad m, ToByteString a) => Key -> Field -> a -> Redis m Bool
hset h k v = singleton $ HSet $ cmd 4 ["HSET", key h, k, toByteString v]

hsetnx :: (Monad m, ToByteString a) => Key -> Field -> a -> Redis m Bool
hsetnx h k v = singleton $ HSetNx $ cmd 4 ["HSETNX", key h, k, toByteString v]

hincrby :: Monad m => Key -> Field -> Int64 -> Redis m Int64
hincrby h k v = singleton $ HIncrBy $ cmd 4 ["HINCRBY", key h, k, int2bytes v]

hincrbyfloat :: Monad m => Key -> Field -> Double -> Redis m Double
hincrbyfloat h k v = singleton $ HIncrByFloat $ cmd 4 ["HINCRBYFLOAT", key h, k, dbl2bytes v]

hdel :: Monad m => Key -> NonEmpty Field -> Redis m Int64
hdel h kk = singleton $ HDel $ cmd (2 + NE.length kk) $ "HDEL" : key h : toList kk

hexists :: Monad m => Key -> Field -> Redis m Bool
hexists h k = singleton $ HExists $ cmd 3 ["HEXISTS", key h, k]

hkeys :: Monad m => Key -> Redis m [Field]
hkeys h = singleton $ HKeys $ cmd 2 ["HKEYS", key h]

hlen :: Monad m => Key -> Redis m Int64
hlen h = singleton $ HLen $ cmd 2 ["HLEN", key h]

-----------------------------------------------------------------------------
-- Lists

lindex :: (Monad m, FromByteString a) => Key -> Index -> Redis m (Maybe a)
lindex k i = singleton $ LIndex $ cmd 3 ["LINDEX", key k, int2bytes i]

lpop :: (Monad m, FromByteString a) => Key -> Redis m (Maybe a)
lpop k = singleton $ LPop $ cmd 2 ["LPOP", key k]

rpop :: (Monad m, FromByteString a) => Key -> Redis m (Maybe a)
rpop k = singleton $ RPop $ cmd 2 ["RPOP", key k]

rpoplpush :: (Monad m, FromByteString a) => Key -> Key -> Redis m (Maybe a)
rpoplpush a b = singleton $ RPopLPush $ cmd 3 ["RPOPLPUSH", key a, key b]

brpoplpush :: (Monad m, FromByteString a) => Key -> Key -> Seconds -> Redis m (Maybe a)
brpoplpush a b (Seconds t) = singleton $ BRPopLPush t $ cmd 4 ["BRPOPLPUSH", key a, key b, int2bytes t]

blpop :: (Monad m, FromByteString a) => NonEmpty Key -> Seconds -> Redis m (Maybe (Key, a))
blpop kk (Seconds t) = singleton $ BLPop t $ cmd (2 + NE.length kk) $
    "BLPOP" : map key (toList kk) ++ [int2bytes t]

brpop :: (Monad m, FromByteString a) => NonEmpty Key -> Seconds -> Redis m (Maybe (Key, a))
brpop kk (Seconds t) = singleton $ BRPop t $ cmd (2 + NE.length kk) $
    "BRPOP" : map key (toList kk) ++ [int2bytes t]

lrange :: (Monad m, FromByteString a) => Key -> Int64 -> Int64 -> Redis m [a]
lrange k a b = singleton $ LRange $ cmd 4 ["LRANGE", key k, int2bytes a, int2bytes b]

linsert :: (Monad m, ToByteString a) => Key -> Side -> a -> a -> Redis m Int64
linsert k s p v = singleton $ LInsert $ cmd 5 ["LINSERT", key k, side2bytes s, toByteString p, toByteString v]

lpush :: (Monad m, ToByteString a) => Key -> NonEmpty a -> Redis m Int64
lpush k vv = singleton $ LPush $ cmd (2 + NE.length vv) $ "LPUSH" : key k : map toByteString (toList vv)

rpush :: (Monad m, ToByteString a) => Key -> NonEmpty a -> Redis m Int64
rpush k vv = singleton $ RPush $ cmd (2 + NE.length vv) $ "RPUSH" : key k : map toByteString (toList vv)

lset :: (Monad m, ToByteString a) => Key -> Int64 -> a -> Redis m ()
lset k i v = singleton $ LSet $ cmd 4 ["LSET", key k, int2bytes i, toByteString v]

ltrim :: Monad m => Key -> Int64 -> Int64 -> Redis m ()
ltrim k i j = singleton $ LTrim $ cmd 4 ["LTRIM", key k, int2bytes i, int2bytes j]

lrem :: (Monad m, ToByteString a) => Key -> Int64 -> a -> Redis m Int64
lrem k c v = singleton $ LRem $ cmd 4 ["LREM", key k, int2bytes c, toByteString v]

lpushx :: (Monad m, ToByteString a) => Key -> a -> Redis m Int64
lpushx k v = singleton $ LPushX $ cmd 3 ["LPUSHX", key k, toByteString v]

rpushx :: (Monad m, ToByteString a) => Key -> a -> Redis m Int64
rpushx k v = singleton $ RPushX $ cmd 3 ["RPUSHX", key k, toByteString v]

llen :: Monad m => Key -> Redis m Int64
llen k = singleton $ LLen $ cmd 2 ["LLEN", key k]

-----------------------------------------------------------------------------
-- Sets

spop :: (Monad m, FromByteString a) => Key -> Redis m (Maybe a)
spop k = singleton $ SPop $ cmd 2 ["SPOP", key k]

sismember :: (Monad m, ToByteString a) => Key -> a -> Redis m Bool
sismember k v = singleton $ SIsMember $ cmd 3 ["SISMEMBER", key k, toByteString v]

smove :: (Monad m, ToByteString a) => Key -> Key -> a -> Redis m Bool
smove a b v = singleton $ SMove $ cmd 4 ["SMOVE", key a, key b, toByteString v]

sdiff :: (Monad m, FromByteString a) => NonEmpty Key -> Redis m [a]
sdiff kk = singleton $ SDiff $ cmd (1 + NE.length kk) $ "SDIFF" : map key (toList kk)

sinter :: (Monad m, FromByteString a) => NonEmpty Key -> Redis m [a]
sinter kk = singleton $ SInter $ cmd (1 + NE.length kk) $ "SINTER" : map key (toList kk)

smembers :: (Monad m, FromByteString a) => Key -> Redis m [a]
smembers k = singleton $ SMembers $ cmd 2 ["SMEMBERS", key k]

srandmember :: (Monad m, FromByteString a) => Key -> Choose -> Redis m [a]
srandmember k One = singleton $ SRandMember One $ cmd 2 ["SRANDMEMBER", key k]
srandmember k chs = singleton $ SRandMember chs $ cmd 3 ["SRANDMEMBER", key k, choose chs]
  where
    choose (Dist n) = int2bytes n
    choose (Arb  n) = int2bytes n
    choose One      = "1"

sdiffstore :: Monad m => Key -> NonEmpty Key -> Redis m Int64
sdiffstore k kk = singleton $ SDiffStore $ cmd (2 + NE.length kk) $
    "SDIFFSTORE" : key k : map key (toList kk)

sinterstore :: Monad m => Key -> NonEmpty Key -> Redis m Int64
sinterstore k kk = singleton $ SInterStore $ cmd (2 + NE.length kk) $
    "SINTERSTORE" : key k : map key (toList kk)

sunionstore :: Monad m => Key -> NonEmpty Key -> Redis m Int64
sunionstore k kk = singleton $ SUnionStore $ cmd (2 + NE.length kk) $
    "SUNIONSTORE" : key k : map key (toList kk)

sadd :: (Monad m, ToByteString a) => Key -> NonEmpty a -> Redis m Int64
sadd k v = singleton $ SAdd $ cmd (2 + NE.length v) $ "SADD" : key k : map toByteString (toList v)

srem :: (Monad m, ToByteString a) => Key -> NonEmpty a -> Redis m Int64
srem k vv = singleton $ SRem $ cmd (2 + NE.length vv) $ "SREM" : key k : map toByteString (toList vv)

sunion :: (Monad m, FromByteString a) => NonEmpty Key -> Redis m [a]
sunion kk = singleton $ SUnion $ cmd (1 + NE.length kk) $ "SUNION" : map key (toList kk)

scard :: Monad m => Key -> Redis m Int64
scard k = singleton $ SCard $ cmd 2 ["SCARD", key k]

-----------------------------------------------------------------------------
-- Sorted Sets

zrange :: (Monad m, FromByteString a) => Key -> Int64 -> Int64 -> Bool -> Redis m (ScoreList a)
zrange k a b s =
    let args = ["ZRANGE", key k, int2bytes a, int2bytes b, "WITHSCORES"]
    in if s then singleton $ ZRange s $ cmd 5 args
            else singleton $ ZRange s $ cmd 4 (init args)

zrevrange :: (Monad m, FromByteString a) => Key -> Int64 -> Int64 -> Bool -> Redis m (ScoreList a)
zrevrange k a b s =
    let args = ["ZREVRANGE", key k, int2bytes a, int2bytes b, "WITHSCORES"]
    in if s then singleton $ ZRevRange s $ cmd 5 args
            else singleton $ ZRevRange s $ cmd 4 (init args)

zrangebyscore :: (Monad m, FromByteString a) => Key -> Double -> Double -> Bool -> Opts "LIMIT" -> Redis m (ScoreList a)
zrangebyscore k a b s o =
    let args = ["ZRANGEBYSCORE", key k, dbl2bytes a, dbl2bytes b, "WITHSCORES"] in
    if s then singleton $ ZRangeByScore s $ cmd (5 + len o) $ args ++ toList (opts o)
         else singleton $ ZRangeByScore s $ cmd (4 + len o) $ init args ++ toList (opts o)

zrevrangebyscore :: (Monad m, FromByteString a) => Key -> Double -> Double -> Bool -> Opts "LIMIT" -> Redis m (ScoreList a)
zrevrangebyscore k a b s o =
    let args = ["ZREVRANGEBYSCORE", key k, dbl2bytes a, dbl2bytes b, "WITHSCORES"] in
    if s then singleton $ ZRevRangeByScore s $ cmd (5 + len o) $ args ++ toList (opts o)
         else singleton $ ZRevRangeByScore s $ cmd (4 + len o) $ init args ++ toList (opts o)

zadd :: (Monad m, ToByteString a) => Key -> NonEmpty (Double, a) -> Redis m Int64
zadd k v = singleton $ ZAdd $ cmd (2 + 2 * NE.length v) $ "ZADD" : key k : foldr f [] v
  where
    f (i, x) acc = dbl2bytes i : toByteString x : acc

zinterstore :: Monad m => Key -> NonEmpty Key -> [Int64] -> Aggregate -> Redis m Int64
zinterstore = _interstore ZInterStore "ZINTERSTORE"

zunionstore :: Monad m => Key -> NonEmpty Key -> [Int64] -> Aggregate -> Redis m Int64
zunionstore = _interstore ZUnionStore "ZUNIONSTORE"

_interstore :: (Resp -> Command Int64)
            -> ByteString
            -> Key
            -> NonEmpty Key
            -> [Int64]
            -> Aggregate
            -> Redis m Int64
_interstore c n k kk ww a =
    let ww'     = map int2bytes ww ++ repeat "1"
        aggr    = aggr2bytes a
        nkeys   = NE.length kk
        ntotal  = 3 + 2 * nkeys + 1 + length aggr
        keys'   = map key (toList kk)
        weights = "WEIGHTS" : take nkeys ww'
    in singleton $ c $ cmd ntotal $ toList $ n
        `cons` key k
        `cons` int2bytes (fromIntegral nkeys)
        `cons` DL.fromList keys'
        <>     DL.fromList weights
        <>     DL.fromList aggr
  where
    aggr2bytes :: Aggregate -> [ByteString]
    aggr2bytes None = []
    aggr2bytes Min  = ["AGGREGATE", "MIN"]
    aggr2bytes Max  = ["AGGREGATE", "MAX"]
    aggr2bytes Sum  = ["AGGREGATE", "SUM"]
{-# INLINE _interstore #-}

zlexcount :: Monad m => Key -> Min -> Max -> Redis m Int64
zlexcount k a b = singleton $ ZLexCount $ cmd 4 ["ZLEXCOUNT", key k, min2bytes a, max2bytes b]

min2bytes :: Min -> ByteString
min2bytes MinInf      = "-"
min2bytes (MinIncl x) = "[" <> x
min2bytes (MinExcl x) = "(" <> x

max2bytes :: Max -> ByteString
max2bytes MaxInf      = "+"
max2bytes (MaxIncl x) = "[" <> x
max2bytes (MaxExcl x) = "(" <> x

zrangebylex :: (Monad m, FromByteString a) => Key -> Min -> Max -> Opts "LIMIT" -> Redis m [a]
zrangebylex k a b o = singleton $ ZRangeByLex $ cmd (4 + len o) $
    "ZRANGEBYLEX" : key k : min2bytes a : max2bytes b : toList (opts o)

zremrangebylex :: Monad m => Key -> Min -> Max -> Redis m Int64
zremrangebylex k a b = singleton $ ZRemRangeByLex $ cmd 4
    ["ZREMRANGEBYLEX", key k, min2bytes a, max2bytes b]

zremrangebyrank :: Monad m => Key -> Int64 -> Int64 -> Redis m Int64
zremrangebyrank k a b = singleton $ ZRemRangeByRank $ cmd 4
    ["ZREMRANGEBYRANK", key k, int2bytes a, int2bytes b]

zremrangebyscore :: Monad m => Key -> Double -> Double -> Redis m Int64
zremrangebyscore k a b = singleton $ ZRemRangeByScore $ cmd 4
    ["ZREMRANGEBYSCORE", key k, dbl2bytes a, dbl2bytes b]

zrem :: (Monad m, ToByteString a) => Key -> NonEmpty a -> Redis m Int64
zrem k vv = singleton $ ZRem $ cmd (2 + NE.length vv) $ "ZREM" : key k : map toByteString (toList vv)

zincrby :: (Monad m, ToByteString a) => Key -> Double -> a -> Redis m Double
zincrby k i v = singleton $ ZIncrBy $ cmd 4 ["ZINCRBY", key k, dbl2bytes i, toByteString v]

zscore :: (Monad m, ToByteString a) => Key -> a -> Redis m (Maybe Double)
zscore k v = singleton $ ZScore $ cmd 3 ["ZSCORE", key k, toByteString v]

zcard :: Monad m => Key -> Redis m Int64
zcard k = singleton $ ZCard $ cmd 2 ["ZCARD", key k]

zcount :: Monad m => Key -> Double -> Double -> Redis m Int64
zcount k a b = singleton $ ZCount $ cmd 4 ["ZCOUNT", key k, dbl2bytes a, dbl2bytes b]

zrank :: (Monad m, ToByteString a) => Key -> a -> Redis m (Maybe Int64)
zrank k a = singleton $ ZRank $ cmd 3 ["ZRANK", key k, toByteString a]

zrevrank :: (Monad m, ToByteString a) => Key -> a -> Redis m (Maybe Int64)
zrevrank k a = singleton $ ZRevRank $ cmd 3 ["ZREVRANK", key k, toByteString a]

-----------------------------------------------------------------------------
-- HyperLogLog

pfadd :: (Monad m, ToByteString a) => Key -> NonEmpty a -> Redis m Bool
pfadd k v = singleton $ PfAdd $ cmd (2 + NE.length v) $ "PFADD" : key k : map toByteString (toList v)

pfcount :: Monad m => NonEmpty Key -> Redis m Int64
pfcount kk = singleton $ PfCount $ cmd (1 + NE.length kk) $ "PFCOUNT" : map key (toList kk)

pfmerge :: Monad m => Key -> NonEmpty Key -> Redis m ()
pfmerge k kk = singleton $ PfMerge $ cmd (2 + NE.length kk) $ "PFMERGE" : key k : map key (toList kk)

-----------------------------------------------------------------------------
-- Scan

scan :: (Monad m, FromByteString a) => Cursor -> Opts "SCAN" -> Redis m (Cursor, [a])
scan c o = singleton $ Scan $ cmd (2 + len o) $ "SCAN" : cursor c : toList (opts o)

hscan :: (Monad m, FromByteString a) => Key -> Cursor -> Opts "SCAN" -> Redis m (Cursor, [a])
hscan h c o = singleton $ HScan $ cmd (3 + len o) $ "HSCAN" : key h : cursor c : toList (opts o)

sscan :: (Monad m, FromByteString a) => Key -> Cursor -> Opts "SCAN" -> Redis m (Cursor, [a])
sscan h c o = singleton $ SScan $ cmd (3 + len o) $ "SSCAN" : key h : cursor c : toList (opts o)

zscan :: (Monad m, FromByteString a) => Key -> Cursor -> Opts "SCAN" -> Redis m (Cursor, [a])
zscan h c o = singleton $ ZScan $ cmd (3 + len o) $ "ZSCAN" : key h : cursor c : toList (opts o)

match :: ByteString -> Opts "SCAN"
match pat = Opts 2 $ "MATCH" `cons` DL.singleton pat

count :: Int64 -> Opts "SCAN"
count n = Opts 2 $ "COUNT" `cons` DL.singleton (int2bytes n)

-----------------------------------------------------------------------------
-- Sort

sort :: (Monad m, FromByteString a) => Key -> Opts "SORT" -> Redis m [a]
sort k o = singleton $ Sort $ cmd (2 + len o) $ "SORT" : key k : toList (opts o)

by :: ByteString -> Opts "SORT"
by pat = Opts 2 $ "BY" `cons` DL.singleton pat

limit :: Int64 -> Int64 -> Opts o
limit off cnt = Opts 3 $ DL.fromList ["LIMIT", int2bytes off, int2bytes cnt]

getkey :: NonEmpty ByteString -> Opts "SORT"
getkey pats = foldr f none (toList pats)
  where
    f p acc = Opts 2 ("GET" `cons` DL.singleton p) <> acc

asc :: Opts "SORT"
asc = Opts 1 $ DL.singleton "ASC"

desc :: Opts "SORT"
desc = Opts 1 $ DL.singleton "DESC"

alpha :: Opts "SORT"
alpha = Opts 1 $ DL.singleton "ALPHA"

store :: Key -> Opts "SORT"
store k = Opts 2 $ "STORE" `cons` DL.singleton (key k)

-----------------------------------------------------------------------------
-- Pub/Sub

publish :: (Monad m, ToByteString a) => ByteString -> a -> Redis m Int64
publish c m = singleton $ Publish $ cmd 3 $ ["PUBLISH", c, toByteString m]

subscribe :: Monad m => NonEmpty ByteString -> PubSub m ()
subscribe cs = singleton $ Subscribe $ cmd (1 + NE.length cs) $ "SUBSCRIBE" : toList cs

psubscribe :: Monad m => NonEmpty ByteString -> PubSub m ()
psubscribe cs = singleton $ PSubscribe $ cmd (1 + NE.length cs) $ "PSUBSCRIBE" : toList cs

unsubscribe :: Monad m => [ByteString] -> PubSub m ()
unsubscribe cs = singleton $ Unsubscribe $ cmd (1 + length cs) $ "UNSUBSCRIBE" : toList cs

punsubscribe :: Monad m => [ByteString] -> PubSub m ()
punsubscribe cs = singleton $ PUnsubscribe $ cmd (1 + length cs) $ "PUNSUBSCRIBE" : toList cs

-----------------------------------------------------------------------------
-- Responses

readInt'Null :: String -> Resp -> Result (Maybe Int64)
readInt'Null _ (Int i)  = Right $ Just i
readInt'Null _ NullBulk = Right Nothing
readInt'Null s _        = Left $ InvalidResponse s

readInt :: String -> Resp -> Result Int64
readInt _ (Int i) = Right i
readInt s _       = Left $ InvalidResponse s

readTTL :: String -> Resp -> Result (Maybe TTL)
readTTL s r = toTTL <$> readInt s r
  where
    toTTL (-2) = Nothing
    toTTL (-1) = Just NoTTL
    toTTL n    = Just $ TTL n

readBool :: String -> Resp -> Result Bool
readBool s r = readInt s r >>= toBool
  where
    toBool 0 = Right False
    toBool 1 = Right True
    toBool _ = Left $ InvalidResponse s

readListOfMaybes :: FromByteString a => String -> Resp -> Result [Maybe a]
readListOfMaybes n (Array _ r) = foldr f (Right []) r
  where
    f _        x@(Left _)  = x
    f NullBulk (Right acc) = Right $ Nothing : acc
    f (Bulk s) (Right acc) = (:acc) . Just <$> readStr s
    f (Str  s) (Right acc) = (:acc) . Just <$> readStr s
    f _        _           = Left $ InvalidResponse n
readListOfMaybes n _ = Left $ InvalidResponse n

readList :: FromByteString a => String -> Resp -> Result [a]
readList r (Array _ v) = foldr f (Right []) v
  where
    f _        x@(Left _)  = x
    f (Bulk s) (Right acc) = (:acc) <$> readStr s
    f (Str  s) (Right acc) = (:acc) <$> readStr s
    f _        _           = Left $ InvalidResponse r
readList r _ = Left $ InvalidResponse r

readScoreList :: FromByteString a => String -> Bool -> Resp -> Result (ScoreList a)
readScoreList r False a           = ScoreList [] <$> readList r a
readScoreList r True  (Array _ v) = toScoreList . unzip <$> foldr f (Right []) (chunksOf 2 v)
  where
    f _                x@(Left _)  = x
    f [Bulk x, Bulk d] (Right acc) = (\a b -> (a, b):acc) <$> readStr x <*> readStr d
    f _        _                   = Left $ InvalidResponse r
    toScoreList (a, s) = ScoreList s a
readScoreList r _ _ = Left $ InvalidResponse r

readFields :: FromByteString a => String -> Resp -> Result [(Field, a)]
readFields r (Array _ v) = foldr f (Right []) (chunksOf 2 v)
  where
    f _                x@(Left _)  = x
    f [Bulk k, Bulk x] (Right acc) = (:acc) . (k,) <$> readStr x
    f _                _           = Left $ InvalidResponse r
readFields r _ = Left $ InvalidResponse r

readKeyValue :: FromByteString a => String -> Resp -> Result (Maybe (Key, a))
readKeyValue _ NullArray                  = return Nothing
readKeyValue _ (Array _ [Bulk k, Bulk v]) = Just . (Key k,) <$> readStr v
readKeyValue r _                          = Left $ InvalidResponse r

readBulk'Null :: FromByteString a => String -> Resp -> Result (Maybe a)
readBulk'Null _ NullBulk = Right Nothing
readBulk'Null _ (Bulk s) = Just <$> readStr s
readBulk'Null n _        = Left $ InvalidResponse n

readBulk :: FromByteString a => String -> Resp -> Result a
readBulk _ (Bulk s) = readStr s
readBulk n _        = Left $ InvalidResponse n

readBulk'Array :: FromByteString a => String -> Choose -> Resp -> Result [a]
readBulk'Array n One r = maybeToList <$> readBulk'Null n r
readBulk'Array n _   r = readList n r

readScan :: FromByteString a => String -> Resp -> Result (Cursor, [a])
readScan n (Array 2 (Bulk c:v:[])) = (Cursor c,) <$> readList n v
readScan n _ = Left $ InvalidResponse n

matchStr :: String -> ByteString -> Resp -> Result ()
matchStr n x (Str s)
    | x == s    = Right ()
    | otherwise = Left $ InvalidResponse n
matchStr n _ _ = Left $ InvalidResponse n

anyStr :: String -> Resp -> Result ()
anyStr _ (Str _) = Right ()
anyStr n _       = Left $ InvalidResponse n

readType :: String -> Resp -> Result (Maybe RedisType)
readType _ (Str s) = case s of
    "string" -> return $ Just RedisString
    "hash"   -> return $ Just RedisHash
    "list"   -> return $ Just RedisList
    "set"    -> return $ Just RedisSet
    "zset"   -> return $ Just RedisZSet
    "none"   -> return Nothing
    _        -> Left $ InvalidConversion ("unknown redis type: " ++ show s)
readType n _ = Left $ InvalidResponse n

fromSet :: Resp -> Result Bool
fromSet (Str "OK") = Right True
fromSet NullBulk   = Right False
fromSet _          = Left $ InvalidResponse "SET"

readPushMessage :: Resp -> Result PushMessage
readPushMessage (Array 3 (Bulk "message":Bulk c:Bulk m:[])) =
    Right $ Message c m
readPushMessage (Array 4 (Bulk "pmessage":Bulk p:Bulk c:Bulk m:[])) =
    Right $ PMessage p c m
readPushMessage (Array 3 (Bulk "subscribe":Bulk c:Int n:[])) =
    Right $ SubscribeMessage c n
readPushMessage (Array 3 (Bulk "unsubscribe":Bulk c:Int n:[])) =
    Right $ UnsubscribeMessage c n
readPushMessage (Array 3 (Bulk "psubscribe":Bulk c:Int n:[])) =
    Right $ SubscribeMessage c n
readPushMessage (Array 3 (Bulk "punsubscribe":Bulk c:Int n:[])) =
    Right $ UnsubscribeMessage c n
readPushMessage _ = Left $ InvalidResponse "pub/sub"

-----------------------------------------------------------------------------
-- Helpers

cmd :: Int -> [ByteString] -> Resp
cmd n a = Array n (map Bulk a)
{-# INLINE cmd #-}

int2bytes :: Int64 -> ByteString
int2bytes = toLazyByteStringWith (safeStrategy 20 8) mempty . int64Dec
{-# INLINE int2bytes #-}

dbl2bytes :: Double -> ByteString
dbl2bytes = fromStrict . toShortest
{-# INLINE dbl2bytes #-}

side2bytes :: Side -> ByteString
side2bytes Before = "BEFORE"
side2bytes After  = "AFTER"
{-# INLINE side2bytes #-}

readStr :: FromByteString a => ByteString -> Result a
readStr s = either (Left . InvalidConversion) Right $ runParser' parser s
{-# INLINE readStr #-}
