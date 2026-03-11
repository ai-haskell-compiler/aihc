{-| A postgresql connection options type and related functions. -}

module Database.PostgreSQL.Simple.Options
  ( Options(..)
  , defaultOptions
  , toConnectionString
  , parseConnectionString
  ) where
import Data.Maybe (Maybe, maybeToList)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Data.Monoid.Generic
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import URI.ByteString as URI
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Monoid
import Control.Monad ((<=<), foldM)
import Control.Applicative

-- | A postgresql connection options type.
data Options = Options
  { host                    :: Last String
  , hostaddr                :: Last String
  , port                    :: Last Int
  , user                    :: Last String
  , password                :: Last String
  , dbname                  :: Last String
  , connectTimeout          :: Last Int
  , clientEncoding          :: Last String
  , options                 :: Last String
  , fallbackApplicationName :: Last String
  , keepalives              :: Last Int
  , keepalivesIdle          :: Last Int
  , keepalivesCount         :: Last Int
  , sslmode                 :: Last String
  , requiressl              :: Last Int
  , sslcompression          :: Last Int
  , sslcert                 :: Last String
  , sslkey                  :: Last String
  , sslrootcert             :: Last String
  , requirepeer             :: Last String
  , krbsrvname              :: Last String
  , gsslib                  :: Last String
  , service                 :: Last String
  } deriving stock (Show, Eq, Read, Ord, Generic, Typeable)
    deriving Semigroup via GenericSemigroup Options
    deriving Monoid    via GenericMonoid Options

-- | Make a key value postgresql option string.
toConnectionString :: Options -> ByteString
toConnectionString Options {..} = BSC.pack $ unwords $ map (\(k, v) -> k <> "=" <> v)
  $  maybeToPairStr "host" host
  <> maybeToPairStr "hostaddr" hostaddr
  <> maybeToPairStr "dbname" dbname
  <> maybeToPair "port" port
  <> maybeToPairStr "password" password
  <> maybeToPairStr "user" user
  <> maybeToPair "connect_timeout" connectTimeout
  <> maybeToPairStr "client_encoding" clientEncoding
  <> maybeToPairStr "options" options
  <> maybeToPairStr "fallback_applicationName" fallbackApplicationName
  <> maybeToPair "keepalives" keepalives
  <> maybeToPair "keepalives_idle" keepalivesIdle
  <> maybeToPair "keepalives_count" keepalivesCount
  <> maybeToPairStr "sslmode" sslmode
  <> maybeToPair "requiressl" requiressl
  <> maybeToPair "sslcompression" sslcompression
  <> maybeToPairStr "sslcert" sslcert
  <> maybeToPairStr "sslkey" sslkey
  <> maybeToPairStr "sslrootcert" sslrootcert
  <> maybeToPairStr "requirepeer" requirepeer
  <> maybeToPairStr "krbsrvname" krbsrvname
  <> maybeToPairStr "gsslib" gsslib
  <> maybeToPairStr "service" service
  where
  maybeToPairStr :: String -> Last String -> [(String, String)]
  maybeToPairStr k mv = (k,) <$> maybeToList (getLast mv)

  maybeToPair :: Show a => String -> Last a -> [(String, String)]
  maybeToPair k mv = (\v -> (k, show v)) <$> maybeToList (getLast mv)

{-| Default options.

 @
   defaultOptions :: Options
   defaultOptions = mempty
    { host     = pure "localhost"
    , port     = pure 5432
    , user     = pure "postgres"
    , dbname   = pure "postgres"
    }
 @
-}
defaultOptions :: Options
defaultOptions = mempty
  { host     = pure "localhost"
  , port     = pure 5432
  , user     = pure "postgres"
  , dbname   = pure "postgres"
  }

userInfoToptions :: UserInfo -> Options
userInfoToptions UserInfo {..} = mempty { user = return $ BSC.unpack uiUsername } <> if BS.null uiPassword
  then mempty
  else mempty { password = return $ BSC.unpack uiPassword }

authorityToOptions :: Authority -> Options
authorityToOptions Authority {..} = maybe mempty userInfoToptions authorityUserInfo <>
  mempty { host = return $ BSC.unpack $ hostBS authorityHost } <>
  maybe mempty (\p -> mempty { port = return $ portNumber p }) authorityPort

pathToptions :: ByteString -> Options
pathToptions path = case drop 1 $ BSC.unpack path of
  "" -> mempty
  x  -> mempty {dbname = return x }

parseInt :: String -> String -> Either String Int
parseInt msg v = maybe (Left (msg <> " value of: " <> v <> " is not a number")) Right $
      readMaybe v

parseString :: String -> Maybe String
parseString x = readMaybe x <|> unSingleQuote x <|> Just x

unSingleQuote :: String -> Maybe String
unSingleQuote (x : xs@(_ : _))
  | x == '\'' && last xs == '\'' = Just $ init xs
  | otherwise                    = Nothing
unSingleQuote _                  = Nothing

keywordToptions :: String -> String -> Either String Options
keywordToptions k v = case k of
  "host" -> return $ mempty { host = return v }
  "hostaddress" -> return $ mempty { hostaddr = return v }
  "port" -> do
    portValue <- parseInt "port" v
    return $ mempty { port = return portValue }
  "user" -> return $ mempty { user = return v }
  "password" -> return $ mempty { password = return v }
  "dbname" -> return $ mempty { dbname = return v}
  "connect_timeout" -> do
    x <- parseInt "connect_timeout" v
    return $ mempty { connectTimeout = return x }
  "client_encoding" -> return $ mempty { clientEncoding = return v }
  "options" -> return $ mempty { options = return v }
  "fallback_applicationName" -> return $ mempty { fallbackApplicationName = return v }
  "keepalives" -> do
    x <- parseInt "keepalives" v
    return $ mempty { keepalives = return x }
  "keepalives_idle" -> do
    x <- parseInt "keepalives_idle" v
    return $ mempty { keepalivesIdle = return x }
  "keepalives_count" -> do
    x <- parseInt "keepalives_count" v
    return $ mempty { keepalivesCount = return x }
  "sslmode" -> return $ mempty { sslmode = return v }
  "requiressl" -> do
    x <- parseInt "requiressl" v
    return $ mempty { requiressl = return x }
  "sslcompression" -> do
    x <- parseInt "sslcompression" v
    return $ mempty { sslcompression = return x }
  "sslcert" -> return $ mempty { sslcert = return v }
  "sslkey" -> return $ mempty { sslkey = return v }
  "sslrootcert" -> return $ mempty { sslrootcert = return v }
  "requirepeer" -> return $ mempty { requirepeer = return v }
  "krbsrvname" -> return $ mempty { krbsrvname = return v }
  "gsslib" -> return $ mempty { gsslib = return v }
  "service" -> return $ mempty { service = return v }

  x -> Left $ "Unrecongnized option: " ++ show x

queryToptions :: URI.Query -> Either String Options
queryToptions Query {..} = foldM (\acc (k, v) -> fmap (mappend acc) $ keywordToptions (BSC.unpack k) $ BSC.unpack v) mempty queryPairs

uriToptions :: URIRef Absolute -> Either String Options
uriToptions URI {..} = case schemeBS uriScheme of
  "postgres" -> options
  "postgresql" -> options
  x -> Left $ "Wrong protocol. Expected \"postgres\" or \"postgresql\" but got: " ++ show x
  where
    options = do
      queryParts <- queryToptions uriQuery
      return $ maybe mempty authorityToOptions uriAuthority <>
        pathToptions uriPath <> queryParts

parseURIStr :: String -> Either String (URIRef Absolute)
parseURIStr = left show . parseURI strictURIParserOptions . BSC.pack where
  left f = \case
    Left x -> Left $ f x
    Right x -> Right x

parseKeywords :: String -> Either String Options
parseKeywords [] = Left "Failed to parse keywords"
parseKeywords x = fmap mconcat . mapM (uncurry keywordToptions <=< toTuple . splitOn "=") $ words x where
  toTuple [k, v] = return (k, v)
  toTuple xs = Left $ "invalid opts:" ++ show (intercalate "=" xs)

-- | Parse a connection string. Can be in URI or keyword format.
parseConnectionString :: String -> Either String Options
parseConnectionString url = do
  url' <- maybe (Left "failed to parse as string") Right $ parseString url
  or (parseKeywords url') (uriToptions =<< parseURIStr url')
  where
    or (Left _) n = n
    or m        _ = m 
