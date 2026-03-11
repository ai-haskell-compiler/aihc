--------------------------------------------------------------------------------
-- Logstash client for Haskell                                                --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Logstash.Connection (
    LogstashConnection(..),
    LogstashPool
) where

--------------------------------------------------------------------------------

import qualified Data.ByteString.Lazy as BSL
import Data.Pool

--------------------------------------------------------------------------------

-- | Represents an abstract interface for Logstash connections that hides
-- details about the nature of the connection.
data LogstashConnection = LogstashConnection {
    -- | A computation which sends data to the logstash server.
    writeData :: BSL.ByteString -> IO (),
    -- | A computation which closes the connection.
    closeConnection :: IO ()
}

-- | For convenience so that importing modules do not have to import 
-- `Data.Pool`, a type alias for a `Pool` of `LogstashConnection`.
type LogstashPool = Pool LogstashConnection

--------------------------------------------------------------------------------
