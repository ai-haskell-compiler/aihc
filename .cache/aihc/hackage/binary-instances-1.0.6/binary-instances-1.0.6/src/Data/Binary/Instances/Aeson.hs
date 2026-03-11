{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Binary.Instances.Aeson where

import Data.Binary         (Binary, Get, get, put)
import Data.Binary.Orphans ()

import Data.Binary.Instances.Scientific ()
import Data.Binary.Instances.Text ()
import Data.Binary.Instances.UnorderedContainers ()
import Data.Binary.Instances.Vector ()

import qualified Data.Aeson        as A
import qualified Data.Aeson.Key    as Key
import qualified Data.Aeson.KeyMap as KM

instance Binary A.Value where
    get = do
        t <- get :: Get Int
        case t of
            0 -> fmap A.Object get
            1 -> fmap A.Array get
            2 -> fmap A.String get
            3 -> fmap A.Number get
            4 -> fmap A.Bool get
            5 -> return A.Null
            _ -> fail $ "Invalid Value tag: " ++ show t

    put (A.Object v) = put (0 :: Int) >> put v
    put (A.Array v)  = put (1 :: Int) >> put v
    put (A.String v) = put (2 :: Int) >> put v
    put (A.Number v) = put (3 :: Int) >> put v
    put (A.Bool v)   = put (4 :: Int) >> put v
    put A.Null       = put (5 :: Int)

instance Binary Key.Key where
    get = Key.fromText <$> get
    put = put . Key.toText

instance Binary v => Binary (KM.KeyMap v) where
    get = fmap KM.fromList get
    put = put . KM.toList
