{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | A tuple represents the types of multiple cassandra columns. It is used
-- to check that column-types match.
module Database.CQL.Protocol.Tuple
    ( Tuple
    , count
    , check
    , tuple
    , store
    , Row
    , mkRow
    , fromRow
    , columnTypes
    , rowLength
    ) where

import Control.Applicative
import Control.Monad
import Data.Functor.Identity
import Data.Serialize
import Data.Vector (Vector, (!?))
import Data.Word
import Database.CQL.Protocol.Class
import Database.CQL.Protocol.Codec (putValue, getValue)
import Database.CQL.Protocol.Tuple.TH
import Database.CQL.Protocol.Types
import Prelude

import qualified Data.Vector as Vec

-- Row ----------------------------------------------------------------------

-- | A row is a vector of 'Value's.
data Row = Row
    { types  :: ![ColumnType]
    , values :: !(Vector Value)
    } deriving (Eq, Show)

-- | Convert a row element.
fromRow :: Cql a => Int -> Row -> Either String a
fromRow i r =
    case values r !? i of
        Nothing -> Left "out of bounds access"
        Just  v -> fromCql v

mkRow :: [(Value, ColumnType)] -> Row
mkRow xs = let (v, t) = unzip xs in Row t (Vec.fromList v)

rowLength :: Row -> Int
rowLength r = Vec.length (values r)

columnTypes :: Row -> [ColumnType]
columnTypes = types

-- Tuples -------------------------------------------------------------------

-- Database.CQL.Protocol.Tuple does not export 'PrivateTuple' but only
-- 'Tuple' effectively turning 'Tuple' into a closed type-class.
class PrivateTuple a where
    count :: Tagged a Int
    check :: Tagged a ([ColumnType] -> [ColumnType])
    tuple :: Version -> [ColumnType] -> Get a
    store :: Version -> Putter a

class PrivateTuple a => Tuple a

-- Manual instances ---------------------------------------------------------

instance PrivateTuple () where
    count     = Tagged 0
    check     = Tagged $ const []
    tuple _ _ = return ()
    store _   = const $ return ()

instance Tuple ()

instance Cql a => PrivateTuple (Identity a) where
    count     = Tagged 1
    check     = Tagged $ typecheck [untag (ctype :: Tagged a ColumnType)]
    tuple v _ = Identity <$> element v ctype
    store v (Identity a) = do
        put (1 :: Word16)
        putValue v (toCql a)

instance Cql a => Tuple (Identity a)

instance PrivateTuple Row where
    count     = Tagged (-1)
    check     = Tagged $ const []
    tuple v t = Row t . Vec.fromList <$> mapM (getValue v . MaybeColumn) t
    store v r = do
        put (fromIntegral (rowLength r) :: Word16)
        Vec.mapM_ (putValue v) (values r)

instance Tuple Row

-- Implementation helpers ---------------------------------------------------

element :: Cql a => Version -> Tagged a ColumnType -> Get a
element v t = getValue v (untag t) >>= either fail return . fromCql

typecheck :: [ColumnType] -> [ColumnType] -> [ColumnType]
typecheck rr cc = if checkAll (===) rr cc then [] else rr
  where
    checkAll f as bs = and (zipWith f as bs)

    checkField (a, b) (c, d) = a == c && b === d

    TextColumn       === VarCharColumn    = True
    VarCharColumn    === TextColumn       = True
    (MaybeColumn  a) === b                = a === b
    (ListColumn   a) === (ListColumn   b) = a === b
    (SetColumn    a) === (SetColumn    b) = a === b
    (MapColumn  a b) === (MapColumn  c d) = a === c && b === d
    (UdtColumn a as) === (UdtColumn b bs) = a == b && checkAll checkField as bs
    (TupleColumn as) === (TupleColumn bs) = checkAll (===) as bs
    a                === b                = a == b

genInstances 48
