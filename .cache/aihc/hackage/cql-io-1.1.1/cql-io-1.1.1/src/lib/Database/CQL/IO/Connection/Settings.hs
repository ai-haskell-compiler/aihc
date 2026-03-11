-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Database.CQL.IO.Connection.Settings
    ( ConnectionSettings
    , ConnId (..)
    , defSettings
    , defKeyspace
    , compression
    , tlsContext

    -- * Timeouts
    , Milliseconds (..)
    , connectTimeout
    , sendTimeout
    , responseTimeout

    -- * Limits
    , maxStreams
    , maxRecvBuffer

    -- * Authentication
    , authenticators
    , AuthMechanism (..)
    , Authenticator (..)
    , AuthContext   (..)
    , authConnId
    , authHost
    , passwordAuthenticator
    , AuthUser (..)
    , AuthPass (..)
    ) where

import Control.Lens (makeLenses)
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.String
import Data.Text (Text)
import Data.Unique
import Database.CQL.Protocol
import Database.CQL.IO.Cluster.Host
import OpenSSL.Session (SSLContext)

import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.Text.Lazy             as Lazy
import qualified Data.Text.Lazy.Encoding    as Lazy

newtype Milliseconds = Ms { ms :: Int }
    deriving (Eq, Show)

newtype ConnId = ConnId Unique deriving (Eq, Ord)

instance Hashable ConnId where
    hashWithSalt _ (ConnId u) = hashUnique u

data ConnectionSettings = ConnectionSettings
    { _connectTimeout  :: !Milliseconds
    , _sendTimeout     :: !Milliseconds
    , _responseTimeout :: !Milliseconds
    , _maxStreams      :: !Int
    , _compression     :: !Compression
    , _defKeyspace     :: !(Maybe Keyspace)
    , _maxRecvBuffer   :: !Int
    , _tlsContext      :: !(Maybe SSLContext)
    , _authenticators  :: !(HashMap AuthMechanism Authenticator)
    }

-- | Context information given to 'Authenticator's when
-- the server requests authentication on a connection.
-- See 'authOnRequest'.
data AuthContext = AuthContext
    { _authConnId :: !ConnId
    , _authHost   :: !InetAddr
    }

-- | The (unique) name of a SASL authentication mechanism.
--
-- In the case of Cassandra, this is currently always the fully-qualified
-- Java class name of the configured server-side @IAuthenticator@
-- implementation.
newtype AuthMechanism = AuthMechanism Text
    deriving (Eq, Ord, Show, IsString, Hashable)

-- | A client authentication handler.
--
-- The fields of an 'Authenticator' must implement the client-side
-- of an (SASL) authentication mechanism as follows:
--
--    * When a Cassandra server requests authentication on a new connection,
--      'authOnRequest' is called with the 'AuthContext' of the
--      connection.
--
--    * If additional challenges are posed by the server,
--      'authOnChallenge' is called, if available, otherwise an
--      'AuthenticationError' is thrown, i.e. every challenge must be
--      answered.
--
--    * Upon successful authentication 'authOnSuccess' is called.
--
-- The existential type @s@ is chosen by an implementation and can
-- be used to thread arbitrary state through the sequence of callback
-- invocations during an authentication exchange.
--
-- See also:
-- <https://tools.ietf.org/html/rfc4422 RFC4422>
-- <https://docs.datastax.com/en/cassandra/latest/cassandra/configuration/secureInternalAuthenticationTOC.html Authentication>
data Authenticator = forall s. Authenticator
    { authMechanism :: !AuthMechanism
        -- ^ The (unique) name of the (SASL) mechanism that the callbacks
        -- implement.
    , authOnRequest :: AuthContext -> IO (AuthResponse, s)
        -- ^ Callback for initiating an authentication exchange.
    , authOnChallenge :: Maybe (s -> AuthChallenge -> IO (AuthResponse, s))
        -- ^ Optional callback for additional challenges posed by the server.
        -- If the authentication mechanism does not require additional
        -- challenges, it should be set to 'Nothing'. Otherwise every
        -- challenge must be answered with a response.
    , authOnSuccess :: s -> AuthSuccess -> IO ()
        -- ^ Callback for successful completion of an authentication exchange.
    }

makeLenses ''AuthContext
makeLenses ''ConnectionSettings

newtype AuthUser = AuthUser Lazy.Text
newtype AuthPass = AuthPass Lazy.Text

-- | A password authentication handler for use with Cassandra's
-- @PasswordAuthenticator@.
--
-- See: <https://docs.datastax.com/en/cassandra/latest/cassandra/configuration/secureConfigNativeAuth.html Configuring Authentication>
passwordAuthenticator :: AuthUser -> AuthPass -> Authenticator
passwordAuthenticator (AuthUser u) (AuthPass p) = Authenticator
    { authMechanism   = "org.apache.cassandra.auth.PasswordAuthenticator"
    , authOnChallenge = Nothing
    , authOnSuccess   = \() _ -> return ()
    , authOnRequest   = \_ctx ->
        let user = Lazy.encodeUtf8 u
            pass = Lazy.encodeUtf8 p
            resp = AuthResponse (Char8.concat ["\0", user, "\0", pass])
        in return (resp, ())
    }

defSettings :: ConnectionSettings
defSettings =
    ConnectionSettings (Ms 5000)     -- connect timeout
                       (Ms 3000)     -- send timeout
                       (Ms 10000)    -- response timeout
                       128           -- max streams per connection
                       noCompression -- compression
                       Nothing       -- keyspace
                       16384         -- receive buffer size
                       Nothing       -- no tls by default
                       HashMap.empty -- no authentication

