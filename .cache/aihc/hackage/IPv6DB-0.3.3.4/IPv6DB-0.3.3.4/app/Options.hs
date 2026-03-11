
module Options where

import           Data.ByteString
import           Options.Applicative

data Options =
  Options
    { appHost       :: String
    , appPort       :: Int
    , logFile       :: String
    , redisHost     :: String
    , redisPort     :: Integer
    , redisDatabase :: Integer
    , redisAuth     :: Maybe ByteString
    }

opts :: ParserInfo Options
opts = info (options <**> helper)
  ( fullDesc
    <> progDesc "RESTful Web Service for IPv6 related data"
    <> header "IPv6DB v0.3.3 APIv1, (c) Michel Boucey 2017-2022" )

options :: Parser Options
options =
  Options
    <$>
      strOption
        ( short 'h'
          <> long "host"
          <> help "Alternative host"
          <> showDefault
          <> value "::" )
    <*>
      option auto
        ( short 'p'
          <> long "port"
          <> help "Alternative listening port"
          <> showDefault
          <> value 4446 )
    <*>
      strOption
        ( short 'l'
          <> long "log-file"
          <> help "Log file"
          <> showDefault
          <> value "/var/log/ipv6db.log" )
    <*>
      strOption
        ( short 'o'
          <> long "redis-host"
          <> help "Redis host"
          <> showDefault
          <> value "localhost" )
    <*>
      option auto
        ( short 'r'
          <> long "redis-port"
          <> help "Redis listening port"
          <> showDefault
          <> value 6379 )
    <*>
      option auto
        ( short 'd'
          <> long "redis-database"
          <> help "Redis database"
          <> showDefault
          <> value 0 )
    <*>
      option auto
        ( short 'a'
          <> long "redis-auth"
          <> help "Redis authentication password"
          <> value Nothing )

