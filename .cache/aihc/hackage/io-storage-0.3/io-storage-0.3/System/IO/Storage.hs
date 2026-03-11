{- |
Conceptually, this library provides a way to arbitrarily extend the
global state represented by the IO monad. Viewed another way, this
library provides a basic facility for setting and retrieving values
from global variables.

The interface takes the form of a very basic key-value store, with
multiple different stores made available through the 'withStore'
function. Stores are referenced by arbitrary strings, and keys
within those stores are treated likewise. The 'putValue', 'getValue',
and 'delValue' functions allow you to store, retrieve, and delete
data from the store.

Internally, data is stored within an IORef which is created using the
'unsafePerformIO hack', but this is hidden within the library so that
it can easily be modified if and when a more 'proper' solution is
implemented.
-}
module System.IO.Storage
  ( withStore
  , putValue
  , getValue
  , getDefaultValue
  , delValue
  ) where

import Data.IORef        ( IORef, newIORef, modifyIORef, readIORef )
import Data.List as L    ( lookup, deleteFirstsBy )
import Data.Map as M     ( Map, empty, lookup, insert, delete )
import Data.Dynamic      ( Dynamic, toDyn, fromDyn, fromDynamic )
import Data.Typeable     ( Typeable )
import Data.Function     ( on )
import Control.Exception ( bracket )
import System.IO.Unsafe  ( unsafePerformIO )

type ValueStore = M.Map String Dynamic

-- | This is the magic bit that makes the data-stores global to the
--   entire program. Sure, it cheats a little, but who doesn't?
globalPeg :: IORef [(String, IORef ValueStore)]
{-# NOINLINE globalPeg #-}
globalPeg = unsafePerformIO (newIORef [])

-- | Create a named key-value store, and then execute the given
--   IO action within its extent. Calls to 'withStore' can be
--   nested, and calling it again with the name of a data-store
--   that has already been initialized will cause the original
--   to be shadowed for the duration of the call to 'withStore'.
withStore :: String -> IO a -> IO a
withStore storeName action = do
    store <- newIORef M.empty
    let emptyStore = (storeName, store)
    let create = modifyIORef globalPeg (emptyStore:)
    let delete = modifyIORef globalPeg deleteStore
    bracket create (const delete) (const action)
  where deleteStore xs = deleteFirstsBy ((==) `on` fst) xs dummyStore
        dummyStore = [(storeName, undefined)]

getPrimitive :: String -> String -> IO (Maybe Dynamic)
getPrimitive storeName key = do
    storeList <- readIORef globalPeg
    case storeName `L.lookup` storeList of
         Nothing -> return Nothing
         Just st -> do map <- readIORef st
                       return $ key `M.lookup` map

-- | Get a value from the given data-store, if it exists. If it
--   doesn't exist, obviously, 'Nothing' will be returned.
getValue :: Typeable a => String -> String -> IO (Maybe a)
getValue storeName key = do
    value <- getPrimitive storeName key
    case value of
         Nothing -> return $ Nothing
         Just dy -> return $ fromDynamic dy

-- | Get a value from the given store, with a default if it
--   doesn't exist.
getDefaultValue :: Typeable a => String -> String -> a -> IO a
getDefaultValue storeName key val = do
    value <- getPrimitive storeName key
    case value of
         Nothing -> return $ val
         Just dy -> return $ fromDyn dy val

-- | Put a value into the given data-store.
putValue :: Typeable a => String -> String -> a -> IO ()
putValue storeName key value = do
    storeList <- readIORef globalPeg
    case storeName `L.lookup` storeList of
         Nothing -> return ()
         Just st -> modifyIORef st . M.insert key . toDyn $ value

-- | Delete a value from the given data-store.
delValue :: String -> String -> IO ()
delValue storeName key = do
    storeList <- readIORef globalPeg
    case storeName `L.lookup` storeList of
         Nothing -> return ()
         Just st -> modifyIORef st . M.delete $ key
