module Hasql.OptparseApplicative
  ( poolConfig,
    poolSettings,
    connectionSettings,
  )
where

import qualified Attoparsec.Time.Text as AttoparsecTime
import qualified Data.Attoparsec.Text as Attoparsec
import Data.Text (Text)
import qualified Hasql.Connection.Setting as Connection.Setting
import qualified Hasql.Connection.Setting.Connection as Connection.Setting.Connection
import qualified Hasql.Connection.Setting.Connection.Param as Connection.Setting.Connection.Param
import Hasql.OptparseApplicative.Prelude
import qualified Hasql.Pool.Config as Pool.Config
import qualified Hasql.Pool.Config.Defaults as Pool.Config.Defaults
import Options.Applicative

-- * Pool

-- | Given a function, which updates the long names, produces a parser of
-- a compiled config.
poolConfig ::
  -- | Option long name modifier.
  --
  -- You can use this function to prefix the name or you can just specify 'id',
  -- if you don't want it changed.
  (String -> String) ->
  Parser Pool.Config.Config
poolConfig modifyName =
  Pool.Config.settings <$> poolSettings modifyName

-- | Given a function, which updates the long names, produces a parser of
-- a list of settings, which you can extend upon or override, and compile to 'Pool.Config.Config' on your own.
poolSettings ::
  -- | Option long name modifier.
  --
  -- You can use this function to prefix the name or you can just specify 'id',
  -- if you don't want it changed.
  (String -> String) ->
  Parser [Pool.Config.Setting]
poolSettings modifyName =
  sequenceA
    [ Pool.Config.size <$> poolSize modifyName,
      Pool.Config.acquisitionTimeout <$> acquisitionTimeout modifyName,
      Pool.Config.agingTimeout <$> connectionLifetime modifyName,
      Pool.Config.idlenessTimeout <$> connectionIdleTime modifyName,
      Pool.Config.staticConnectionSettings <$> connectionSettings modifyName
    ]

poolSize :: (String -> String) -> Parser Int
poolSize modifyName =
  option auto
    . mconcat
    $ [ long (modifyName "pool-size"),
        value Pool.Config.Defaults.size,
        showDefault,
        help "Amount of connections in the pool"
      ]

acquisitionTimeout :: (String -> String) -> Parser DiffTime
acquisitionTimeout modifyName =
  attoparsedOption AttoparsecTime.diffTime
    . mconcat
    $ [ long (modifyName "pool-acquisition-timeout"),
        value Pool.Config.Defaults.acquisitionTimeout,
        showDefault,
        help "How long it takes until the attempt to connect is considered timed out"
      ]

connectionLifetime :: (String -> String) -> Parser DiffTime
connectionLifetime modifyName =
  attoparsedOption AttoparsecTime.diffTime
    . mconcat
    $ [ long (modifyName "pool-connection-lifetime"),
        value Pool.Config.Defaults.agingTimeout,
        showDefault,
        help "Maximal lifetime for connections. Allows to periodically clean up the connection resources to avoid server-side leaks"
      ]

connectionIdleTime :: (String -> String) -> Parser DiffTime
connectionIdleTime modifyName =
  attoparsedOption AttoparsecTime.diffTime
    . mconcat
    $ [ long (modifyName "pool-connection-idle-time"),
        value Pool.Config.Defaults.idlenessTimeout,
        showDefault,
        help "Maximal connection idle time"
      ]

-- * Connection

-- | Given a function, which updates the long names produces a parser
-- of @Hasql.Connection.'Connection.Settings'@.
connectionSettings ::
  -- | Option long name modifier.
  --
  -- You can use this function to prefix the name or you can just specify 'id',
  -- if you don't want it changed.
  (String -> String) ->
  Parser [Connection.Setting.Setting]
connectionSettings modifyName =
  sequenceA
    [ Connection.Setting.connection
        . Connection.Setting.Connection.params
        <$> sequenceA
          [ Connection.Setting.Connection.Param.host <$> host modifyName,
            Connection.Setting.Connection.Param.port <$> port modifyName,
            Connection.Setting.Connection.Param.user <$> user modifyName,
            Connection.Setting.Connection.Param.password <$> password modifyName,
            Connection.Setting.Connection.Param.dbname <$> database modifyName
          ],
      Connection.Setting.usePreparedStatements <$> usePreparedStatements modifyName
    ]

host :: (String -> String) -> Parser Text
host modifyName =
  fmap fromString
    $ strOption
    $ mconcat
      [ long (modifyName "host"),
        value "127.0.0.1",
        showDefault,
        help "Server host"
      ]

port :: (String -> String) -> Parser Word16
port modifyName =
  option auto
    $ mconcat
      [ long (modifyName "port"),
        value 5432,
        showDefault,
        help "Server port"
      ]

user :: (String -> String) -> Parser Text
user modifyName =
  fmap fromString
    $ strOption
    $ mconcat
      [ long (modifyName "user"),
        value "postgres",
        showDefault,
        help "Username"
      ]

password :: (String -> String) -> Parser Text
password modifyName =
  fmap fromString
    $ strOption
    $ mconcat
      [ long (modifyName "password"),
        value "",
        showDefault,
        help "Password"
      ]

database :: (String -> String) -> Parser Text
database modifyName =
  fmap fromString
    $ strOption
    $ mconcat
      [ long (modifyName "database"),
        help "Database name"
      ]

usePreparedStatements :: (String -> String) -> Parser Bool
usePreparedStatements modifyName =
  fmap not
    $ switch
    $ mconcat
      [ long (modifyName "no-prepared-statements"),
        help "Avoid using prepared statements",
        showDefault
      ]

-- * Helpers

attoparsedOption :: Attoparsec.Parser a -> Mod OptionFields a -> Parser a
attoparsedOption parser =
  option $ eitherReader $ Attoparsec.parseOnly (parser <* Attoparsec.endOfInput) . fromString
