{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.BList (BList(..)) where

import Data.Binary.Get
import Data.Binary

import Data.Endian

newtype BList a = BList { fromBList :: [a] } deriving Functor

instance Binary a => Binary (BList a) where
    put (BList (a:as)) = put a >> put (BList as)
    put (BList []) = return ()
    get = do
        e <- isEmpty
        if e then return $ BList [] else do
            a <- get
            (BList as) <- get
            return $ BList $ a : as

instance Binary (Be a) => Binary (Be (BList a)) where
    put (Be (BList l)) = put $ BList $ fmap Be l
    get = Be . fmap fromBe <$> get

instance Binary (Le a) => Binary (Le (BList a)) where
    put (Le (BList l)) = put $ BList $ fmap Le l
    get = Le . fmap fromLe <$> get
