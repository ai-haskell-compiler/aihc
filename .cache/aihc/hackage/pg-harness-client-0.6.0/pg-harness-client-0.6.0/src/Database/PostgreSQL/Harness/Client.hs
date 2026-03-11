{-
    Copyright (c) 2015, Bardur Arantsson
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are
    met:

    1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
{-|

  Client library for <https://hackage.haskell.org/package/pg-harness-server pg-harness-server>.

  Use the 'createTemporaryDatabase' function to create databases.

-}
module Database.PostgreSQL.Harness.Client
    ( ConnectionInformation(..)
    , createTemporaryDatabase
    , toConnectionString
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Network.HTTP (simpleHTTP, postRequest, getResponseBody)

-- | Connection information to use for connecting to a database.
data ConnectionInformation = ConnectionInformation
    { ciHost :: String
    , ciPort :: String
    , ciDatabaseName :: String
    , ciUser :: String
    , ciPassword :: String
    }

-- | Create temporary database using the given URL to a
-- running @pg-harness-server@ REST service. Returns the connection
-- information for connecting to the newly created temporary
-- database.
createTemporaryDatabase :: String -> IO ConnectionInformation
createTemporaryDatabase url = do
  rsp <- simpleHTTP (postRequest url)
  body <- getResponseBody rsp
  return $ parse body
  where
    parse :: String -> ConnectionInformation
    parse s =
      case lines s of
        [ user, password, host, port, databaseName ] ->
            ConnectionInformation host port databaseName user password
        _ ->
            error $ "Invalid response from server: " ++ s

-- | Convert connection information to a @libpq@ or
-- @postgresql-simple@ compatible connection string.
toConnectionString :: ConnectionInformation -> ByteString
toConnectionString (ConnectionInformation host port databaseName user password) =
  B8.pack $
    "host=" ++ host ++ " " ++
    "port=" ++ port ++ " " ++
    "dbname=" ++ databaseName ++ " " ++
    "user=" ++ user ++ " " ++
    "password=" ++ password
