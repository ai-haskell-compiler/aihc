----------------------------------------------------------------------------------------------------

-- | Hetzner Cloud API client.
--
--   More information can be found on the
--   [official documentation](https://docs.hetzner.cloud).
--
--   Although not necessary, this module was designed with
--   qualified imports in mind. For example:
--
-- > import qualified Hetzner.Cloud as Hetzner
--
--   == Pagination
--
--   Some requests use pagination. These take a page argument of
--   type @'Maybe' 'Int'@. You can use 'streamPages' to get all pages
--   through a conduit-based stream. For example, to get all servers
--   as a stream:
--
-- > streamPages $ getServers token :: ConduitT i Server m ()
--
--   Or to get all volumes as a stream:
--
-- > streamPages $ getVolumes token :: ConduitT i Volume m ()
--
--   If you are not interested in the streaming functionality, you
--   can simply use 'streamToList' to turn the stream into a list:
--
-- > streamToList $ streamPages $ getServers token :: m [Server]
--
--   == Exceptions
--
--   This library makes extensive use of exceptions. Exceptions from
--   this module have type 'CloudException'. All functions that perform
--   requests to Hetzner Cloud can throw this type of exception.
--
module Hetzner.Cloud
  ( -- * Tokens
    Token (..)
  , getTokenFromEnv
  , getTokenFromEnvThrow
    -- * Server metadata
  , Metadata (..)
  , getMetadata
    -- * Hetzner Cloud API

    -- | Sections are in the same order as in the
    --   [official documentation](https://docs.hetzner.cloud).

    -- ** Actions
  , ActionStatus (..)
  , ActionCommand (..)
  , ActionID (..)
  , Action (..)
  , getAction
  , waitForAction
  , HasActions (..)
  , waitForActions
    -- ** Datacenters
  , DatacenterID (..)
  , DatacenterServers (..)
  , Datacenter (..)
  , DatacentersWithRecommendation (..)
  , getDatacenters
  , getDatacenter
    -- ** Firewalls
  , FirewallID (..)
  , TrafficDirection (..)
  , PortRange (..)
  , FirewallRuleProtocol (..)
  , FirewallRule (..)
  , anyIPv4
  , anyIPv6
  , Firewall (..)
  , NewFirewall (..)
  , defaultNewFirewall
  , CreatedFirewall (..)
  , getFirewalls
  , getFirewall
  , createFirewall
  , deleteFirewall
  , updateFirewall
    -- *** Firewall actions
  , applyFirewall
  , removeFirewall
    -- ** Floating IPs
  , FloatingIPID (..)
    -- ** Images
  , OSFlavor (..)
  , ImageType (..)
  , ImageID (..)
  , Image (..)
  , getImages
  , getImage
    -- ** Load Balancers
  , LoadBalancerID (..)
    -- ** Locations
  , City (..)
  , LocationID (..)
  , Location (..)
  , getLocations
  , getLocation
    -- ** Primary IPs
  , PrimaryIPID (..)
  , PrimaryIP (..)
  , getPrimaryIPs
  , getPrimaryIP
  , setReverseDNS
    -- ** Networks
  , NetworkID (..)
  , Route (..)
  , SubnetType (..)
  , Subnet (..)
  , Network (..)
  , NewNetwork (..)
  , defaultNewNetwork
  , getNetworks
  , getNetwork
  , createNetwork
  , deleteNetwork
  , updateNetwork
    -- ** Pricing
  , Price (..)
  , PriceInLocation (..)
    -- ** Servers
  , ServerStatus (..)
  , ServerID (..)
  , Server (..)
  , NewServer (..)
  , defaultNewServer
  , CreatedServer (..)
  , getServers
  , getServer
  , createServer
  , deleteServer
    -- *** Server actions
  , setServerReverseDNS
  , powerOnServer
  , powerOffServer
  , shutdownServer
  , rebootServer
  , changeServerType
    -- ** Server types
  , Architecture (..)
  , StorageType (..)
  , CPUType (..)
  , ServerTypeID (..)
  , ServerType (..)
  , getServerTypes
    -- ** SSH Keys
  , SSHKeyID (..)
  , SSHKey (..)
  , getSSHKeys
  , getSSHKey
  , createSSHKey
  , deleteSSHKey
  , updateSSHKey
    -- ** Volumes
  , VolumeID (..)
  , VolumeFormat (..)
  , VolumeStatus (..)
  , Volume (..)
  , AttachToServer (..)
  , NewVolume (..)
  , CreatedVolume (..)
  , getVolumes
  , getVolume
  , createVolume
  , deleteVolume
  , updateVolume
    -- * Exceptions
  , Error (..)
  , CloudException (..)
    -- * Labels
  , LabelKey (..)
  , Label (..)
  , LabelMap
  , toLabelMap
  , fromLabelMap
  , LabelSelector (..)
  , LabelSelectorAll (..)
    -- * Other types
    -- ** Regions
  , Region (..)
    -- ** Resources
  , ResourceID (..)
    -- ** Public networks
  , FirewallStatus (..)
  , PublicIPInfo (..)
  , PublicNetwork (..)
    -- * Streaming
  , streamPages
  , streamToList
    -- * Generic interface
    -- ** Generic queries
  , cloudQuery
  , noBody
    -- ** JSON Wrappers
  , WithKey (..)
  , WithMeta (..)
    -- ** Response metadata
  , ResponseMeta (..)
  , Pagination (..)
    ) where

import Hetzner.Cloud.Fingerprint (Fingerprint, fingerprint)
-- base
import Control.Exception (Exception, throwIO)
import Control.Concurrent (threadDelay)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Proxy
import Data.String (IsString, fromString)
import Data.Void
import Data.Either (partitionEithers)
#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (forM_)
import Data.Traversable (forM)
import Data.Maybe (isNothing, fromMaybe)
import System.Environment qualified as System
import Data.List.NonEmpty (NonEmpty ((:|)))
-- ip
import Net.IPv4 (IPv4, IPv4Range)
import Net.IPv4 qualified as IPv4
import Net.IPv6 (IPv6, IPv6Range)
import Net.IPv6 qualified as IPv6
-- bytestring
import Data.ByteString (ByteString)
-- text
import Data.Text (Text)
import Data.Text qualified as Text
-- aeson
import Data.Aeson
  ( FromJSON, ToJSON
  , (.:), (.:?), (.=)
  , FromJSONKey, ToJSONKey
    )
import Data.Aeson qualified as JSON
import Data.Aeson.Types qualified as JSON
import Data.Aeson.Key qualified as JSONKey
import Data.Aeson.Encoding qualified as JSONEncoding
-- yaml
import Data.Yaml qualified as Yaml
-- http-conduit
import Network.HTTP.Simple qualified as HTTP
-- time
import Data.Time (ZonedTime)
-- country
import Country (Country)
-- megaparsec
import Text.Megaparsec qualified as Parser
import Text.Megaparsec.Char qualified as Parser
import Text.Megaparsec.Char.Lexer qualified as Parser
-- containers
import Data.Map (Map)
import Data.Map qualified as Map
-- scientific
import Data.Scientific (Scientific)
-- conduit
import Data.Conduit (ConduitT)
import Data.Conduit qualified as Conduit

-- | A token used to authenticate requests. All requests made with a token
--   will have as scope the project where the token was made.
--
--   You can obtain one through the [Hetzner Cloud Console](https://console.hetzner.cloud).
newtype Token = Token ByteString deriving (Show, Eq, Ord)

instance IsString Token where
  fromString = Token . fromString

-- | Lookup 'Token' from the environment variable @HETZNER_API_TOKEN@.
getTokenFromEnv :: IO (Maybe Token)
getTokenFromEnv = fmap fromString <$> System.lookupEnv "HETZNER_API_TOKEN"

-- | Calls 'getTokenFromEnv' and throws a 'MissingToken' exception if
--   the token is missing.
getTokenFromEnvThrow :: IO Token
getTokenFromEnvThrow = do
  mtoken <- getTokenFromEnv
  maybe (throwIO MissingToken) pure mtoken

-- | An error returned by Hetzner.
data Error = Error
  { -- | Error code.
    errorCode :: Text
    -- | Error message.
  , errorMessage :: Text
    } deriving Show

instance FromJSON Error where
  parseJSON = JSON.withObject "Error" $ \o ->
    Error <$> o .: "code" <*> o .: "message"

instance ToJSON Error where
  toJSON err = JSON.object [ "code" .= errorCode err, "message" .= errorMessage err ]

-- | Label key.
data LabelKey = LabelKey
  { -- | Optional prefix.
    labelKeyPrefix :: Maybe Text
    -- | Key name.
  , labelKeyName :: Text
    } deriving (Eq, Ord, Show)

type Parser = Parser.Parsec Void Text

labelKeyPrefixParser :: Parser Text
labelKeyPrefixParser = do
  xs <- Parser.sepBy1 (Text.pack <$> Parser.some Parser.alphaNumChar) (Parser.single '.')
  _ <- Parser.single '/'
  pure $ Text.intercalate "." xs

labelKeyNameParser :: Parser Text
labelKeyNameParser = do
  x <- Parser.alphaNumChar
  let loop :: Bool -> Parser [Char]
      loop afterSymbol = Parser.choice
        [ (:) <$> Parser.alphaNumChar <*> loop False
        , (:) <$> Parser.single '-' <*> loop True
        , (:) <$> Parser.single '_' <*> loop True
        , (:) <$> Parser.single '.' <*> loop True
        , if afterSymbol
             then fail "Label key name must end in alphanumeric character."
             else pure []
          ]
  xs <- loop False
  pure $ Text.pack $ x : xs

labelKeyParser :: Parser LabelKey
labelKeyParser = do
  prefix <- Parser.optional $ Parser.try labelKeyPrefixParser
  name <- labelKeyNameParser
  pure $ LabelKey prefix name

labelKeyRender :: LabelKey -> Text
labelKeyRender k = case labelKeyPrefix k of
  Just prefix -> Text.concat [ prefix, "/", labelKeyName k ]
  _ -> labelKeyName k

instance FromJSON LabelKey where
  parseJSON = JSON.withText "LabelKey" $ \t ->
    either (fail . Parser.errorBundlePretty) pure $
      Parser.runParser labelKeyParser "JSON" t

instance ToJSON LabelKey where
   toJSON = JSON.String . labelKeyRender

instance FromJSONKey LabelKey where
  fromJSONKey = JSON.FromJSONKeyTextParser $ \t ->
    either (fail . Parser.errorBundlePretty) pure $
      Parser.runParser (labelKeyParser <* Parser.eof) "JSON key" t

instance ToJSONKey LabelKey where
  toJSONKey =
    JSON.ToJSONKeyText
      (JSONKey.fromText . labelKeyRender)
      (JSONEncoding.text . labelKeyRender)

-- | Labels are key-value pairs that can be attached to all resources.
data Label = Label
  { labelKey :: LabelKey
  , labelValue :: Text
    } deriving (Eq, Show)

labelValueParser :: Parser Text
labelValueParser = do
  let loop :: Bool -> Parser [Char]
      loop afterSymbol = Parser.choice
        [ (:) <$> Parser.alphaNumChar <*> loop False
        , (:) <$> Parser.single '-' <*> loop True
        , (:) <$> Parser.single '_' <*> loop True
        , (:) <$> Parser.single '.' <*> loop True
        , if afterSymbol
             then fail "Label value must end in alphanumeric character."
             else pure []
          ]
  mx <- Parser.optional Parser.alphaNumChar
  case mx of
    Just x -> Text.pack . (:) x <$> loop False
    _ -> pure mempty

-- | A label map maps label keys to values.
type LabelMap = Map LabelKey Text

-- | Build a label map from a list of labels.
toLabelMap :: [Label] -> LabelMap
toLabelMap = foldr (\label -> Map.insert (labelKey label) $ labelValue label) Map.empty

-- | Get a list of labels from a label map.
fromLabelMap :: LabelMap -> [Label]
fromLabelMap = Map.foldrWithKey (\k v xs -> Label k v : xs) []

-- | Label selectors can be used to filter resources.
data LabelSelector =
    -- | Select when label is equal.
    LabelEqual Label
    -- | Select when label is not equal.
  | LabelNotEqual Label
    -- | Select when key is present.
  | KeyPresent LabelKey
    -- | Select when key is not present.
  | KeyNotPresent LabelKey
    -- | Select when label has one of the values.
  | KeyValueIn LabelKey [Text]
    -- | Select when label has none of the values.
  | KeyValueNotIn LabelKey [Text]
    deriving Show

-- | Label selector parser.
labelSelectorParser :: Parser LabelSelector
labelSelectorParser = Parser.choice
  [ Parser.single '!' *> (KeyNotPresent <$> labelKeyParser)
  , do k <- labelKeyParser
       Parser.choice
         [ do _ <- Parser.single '='
              _ <- Parser.optional $ Parser.single '='
              v <- labelValueParser
              pure $ LabelEqual $ Label k v
         , do _ <- Parser.single '!'
              _ <- Parser.single '='
              v <- labelValueParser
              pure $ LabelNotEqual $ Label k v
         , do _ <- Parser.single ' '
              Parser.choice
                [ do _ <- Parser.chunk "in ("
                     vs <- Parser.sepBy1 labelValueParser $ Parser.single ','
                     _ <- Parser.single ')'
                     pure $ KeyValueIn k vs
                , do _ <- Parser.chunk "notin ("
                     vs <- Parser.sepBy1 labelValueParser $ Parser.single ','
                     _ <- Parser.single ')'
                     pure $ KeyValueNotIn k vs
                  ]
         , pure $ KeyPresent k
           ]
    ]

renderLabelSelector :: LabelSelector -> Text
renderLabelSelector (LabelEqual l) =
  labelKeyRender (labelKey l) <> "==" <> labelValue l
renderLabelSelector (LabelNotEqual l) =
  labelKeyRender (labelKey l) <> "!=" <> labelValue l
renderLabelSelector (KeyPresent k) = labelKeyRender k
renderLabelSelector (KeyNotPresent k) = "!" <> labelKeyRender k
renderLabelSelector (KeyValueIn k vs) =
  labelKeyRender k <> " in (" <> Text.intercalate "," vs <> ")"
renderLabelSelector (KeyValueNotIn k vs) =
  labelKeyRender k <> " notin (" <> Text.intercalate "," vs <> ")"

-- | Combine a list of label selectors, giving you a selector that
--   selects labels that match /all/ selectors in the list.
newtype LabelSelectorAll = LabelSelectorAll [LabelSelector] deriving Show

instance FromJSON LabelSelectorAll where
  parseJSON = JSON.withText "LabelSelector" $ \t ->
    either (fail . Parser.errorBundlePretty) pure $
      let parser = fmap LabelSelectorAll $ Parser.sepBy1 labelSelectorParser $ Parser.single ','
      in  Parser.runParser (parser <* Parser.eof) "JSON" t

instance ToJSON LabelSelectorAll where
  toJSON (LabelSelectorAll xs) = JSON.String $
    Text.intercalate "," $ fmap renderLabelSelector xs

-- | Pagination information.
data Pagination = Pagination
  { currentPage :: Int
  , itemsPerPage :: Int
  , previousPage :: Maybe Int
  , nextPage :: Maybe Int
  , lastPage :: Maybe Int
  , totalEntries :: Maybe Int
    } deriving Show

instance FromJSON Pagination where
  parseJSON = JSON.withObject "Pagination" $ \o -> Pagination
    <$> o .:  "page"
    <*> o .:  "per_page"
    <*> o .:? "previous_page"
    <*> o .:? "next_page"
    <*> o .:? "last_page"
    <*> o .:? "total_entries"

-- | Network zones.
data Region =
    -- | Nuremberg, Falkenstein, Helsinki.
    EUCentral
    -- | Hillsboro (OR).
  | USWest
    -- | Ashburn (VA).
  | USEast deriving (Eq, Show)

instance FromJSON Region where
  parseJSON = JSON.withText "Region" $ \t -> case t of
    "eu-central" -> pure EUCentral
    "us-west" -> pure USWest
    "us-east" -> pure USEast
    _ -> fail $ "Unknown region: " ++ Text.unpack t

instance ToJSON Region where
  toJSON r = case r of
    EUCentral -> "eu-central"
    USWest -> "us-west"
    USEast -> "us-east"

-- | Metadata that any server in the Hetzner cloud can discover
--   about itself.
data Metadata = Metadata
  { -- | Server name.
    metadataName :: Text
    -- | ID of the server.
  , metadataServerID :: ServerID
    -- | Primary public IPv4 address.
  , metadataPublicIPv4 :: IPv4
    -- | Datacenter.
  , metadataDatacenter :: Text
    -- | Network zone.
  , metadataRegion :: Region
    } deriving Show

instance FromJSON Metadata where
  parseJSON = JSON.withObject "Metadata" $ \o -> Metadata
    <$> o .: "hostname"
    <*> o .: "instance-id"
    <*> o .: "public-ipv4"
    <*> o .: "availability-zone"
    <*> o .: "region"

-- | Generic metadata query.
metadataQuery
  :: FromJSON a
  => ByteString -- ^ Path
  -> IO a
metadataQuery path =
  let req = HTTP.setRequestMethod "GET"
          $ HTTP.setRequestSecure False
          $ HTTP.setRequestHost "169.254.169.254"
          $ HTTP.setRequestPort 80
          $ HTTP.setRequestPath ("/hetzner/v1/metadata" <> path)
          $ HTTP.defaultRequest
  in  HTTP.httpBS req >>= Yaml.decodeThrow . HTTP.getResponseBody

-- | Obtain metadata from running server.
--   It doesn't need a 'Token' but must be
--   run from a server in Hetzner Cloud.
getMetadata :: IO Metadata
getMetadata = metadataQuery mempty

-- | Exception produced while performing a request to Hetzner Cloud.
data CloudException =
    CloudError Error
  | JSONError (HTTP.Response ByteString) String
  | MissingToken
    deriving Show

instance Exception CloudException

-- | A firewall ID and whether the firewall is applied or not.
data FirewallStatus = FirewallStatus
  { firewallStatusID :: FirewallID
  , firewallIsApplied :: Bool
    } deriving Show

instance FromJSON FirewallStatus where
  parseJSON = JSON.withObject "FirewallStatus" $ \o -> do
    status <- o .: "status"
    liftA2 FirewallStatus (o .: "id") $ case status of
      "applied" -> pure True
      "pending" -> pure False
      _ -> fail $ "Invalid firewall status: " ++ Text.unpack status

-- | Public IP information.
data PublicIPInfo dnsptr ip = PublicIPInfo
  { -- | Reverse DNS PTR entry/entries.
    reverseDNS :: dnsptr
    -- | IP address/range.
  , publicIP :: ip
    } deriving Show

instance (FromJSON dnsptr, FromJSON ip) => FromJSON (PublicIPInfo dnsptr ip) where
  parseJSON = JSON.withObject "PublicIPInfo" $ \o -> PublicIPInfo
    <$> o .: "dns_ptr"
    <*> o .: "ip"

instance (ToJSON dnsptr, ToJSON ip) => ToJSON (PublicIPInfo dnsptr ip) where
  toJSON (PublicIPInfo dns ip) = JSON.object [ "dns_ptr" .= dns, "ip" .= ip ]

instance Functor (PublicIPInfo dnsptr) where
  fmap f (PublicIPInfo dns ip) = PublicIPInfo dns (f ip)

instance Foldable (PublicIPInfo dnsptr) where
  foldMap f (PublicIPInfo _ ip) = f ip

instance Traversable (PublicIPInfo dnsptr) where
  traverse f (PublicIPInfo dns ip) = PublicIPInfo dns <$> f ip

-- | Public network information associated with a 'Server'.
data PublicNetwork = PublicNetwork
  { publicNetworkFirewalls :: [FirewallStatus]
  , publicNetworkFloatingIPs :: [FloatingIPID]
  , publicIPv4 :: Maybe (PublicIPInfo Text IPv4)
  , publicIPv6 :: Maybe (PublicIPInfo [PublicIPInfo Text IPv6] IPv6Range)
    } deriving Show

instance FromJSON PublicNetwork where
  parseJSON = JSON.withObject "PublicNetwork" $ \o -> PublicNetwork
    <$> o .: "firewalls"
    <*> o .: "floating_ips"
    <*> o .: "ipv4"
    <*> o .: "ipv6"

-- | Generic Hetzner Cloud query.
--
--   This function is used to implement Hetzner Cloud queries.
--
--   If there is any issue while performing the request, a
--   'CloudException' will be thrown.
--
--   The page argument determines which page will be requested.
--   If not provided, it will request the first page.
--   If a page is requested outside the valid range, an empty
--   list will be returned, not a failure.
--
cloudQuery
  :: (ToJSON body, FromJSON a)
  => ByteString -- ^ Method
  -> ByteString -- ^ Path
  -> Maybe body -- ^ Request body. You may use 'noBody' to skip.
  -> Token -- ^ Authentication token
  -> Maybe Int -- ^ Page
  -> IO a
cloudQuery method path mbody (Token token) mpage = do
  let req = HTTP.setRequestMethod method
          $ HTTP.setRequestSecure True
          $ HTTP.setRequestHost "api.hetzner.cloud"
          $ HTTP.setRequestPort 443
          $ HTTP.setRequestPath ("/v1" <> path)
          $ maybe id HTTP.setRequestBodyJSON mbody
          $ HTTP.addRequestHeader "Authorization" ("Bearer " <> token)
          $ maybe id (\page -> HTTP.addToRequestQueryString
                                 [("page", Just $ fromString $ show page)]) mpage
          $ HTTP.defaultRequest
  resp <- HTTP.httpBS req
  let body = HTTP.getResponseBody resp
  case divMod (HTTP.getResponseStatusCode resp) 100 of
    (2,m) ->
      let body' = if m == 4 then "{}" else body
      in  case JSON.eitherDecodeStrict body' of
            Left err -> throwIO $ JSONError resp err
            Right x -> pure x
    _ -> case JSON.eitherDecodeStrict body of
           Left err -> throwIO $ JSONError resp err
           Right x -> throwIO $ CloudError $ withoutKey @"error" x

-- | Used to send requests without a body.
noBody :: Maybe Void
noBody = Nothing

-- | Stream results using a function that takes a page number,
--   going through all the pages.
streamPages
  :: forall key f a i m
   . (Foldable f, MonadIO m)
  -- | Function that takes page number and returns result.
  => (Maybe Int -> IO (WithMeta key (f a)))
  -- | Conduit-based stream that yields results downstream.
  -> ConduitT i a m ()
streamPages f = go Nothing
  where
    go :: Maybe Int -> ConduitT i a m ()
    go page = do
      resp <- liftIO $ f page
      -- Yield results from response
      forM_ resp $ mapM_ Conduit.yield
      -- Continue if not in last page
      let pag = pagination $ responseMeta resp
          cur = currentPage pag
      let next = case lastPage pag of
                   Just l -> if l == cur
                                then Nothing
                                else Just $ cur + 1
                   _ -> nextPage pag
      if isNothing next then pure () else go next

-- | Convenient function to turn streams into lists.
streamToList :: Monad m => ConduitT () a m () -> m [a]
streamToList = Conduit.sourceToList

-- | Wrap a value with the key of the value within a JSON object.
data WithKey (key :: Symbol) a = WithKey { withoutKey :: a } deriving Show

instance Functor (WithKey key) where
  fmap f (WithKey x) = WithKey (f x)

instance Foldable (WithKey key) where
  foldMap f = f . withoutKey

instance (KnownSymbol key, FromJSON a) => FromJSON (WithKey key a) where
  parseJSON =
    let key = symbolVal (Proxy @key)
    in  JSON.withObject ("WithKey:" ++ key) $ \o ->
          WithKey <$> o .: fromString key

-- | A value together with response metadata.
--   The type is annotated with the JSON key of the value.
data WithMeta (key :: Symbol) a = WithMeta
  { -- | Response metadata.
    responseMeta :: ResponseMeta
    -- | The value alone, without the metadata.
  , withoutMeta :: a
    } deriving Show

instance Functor (WithMeta key) where
  fmap f x = x { withoutMeta = f $ withoutMeta x }

instance Foldable (WithMeta key) where
  foldMap f = f . withoutMeta

instance (KnownSymbol key, FromJSON a) => FromJSON (WithMeta key a) where
  parseJSON =
    let key = symbolVal (Proxy @key)
    in  JSON.withObject ("WithMeta:" ++ key) $ \o ->
          WithMeta <$> o .: "meta" <*> o .: fromString key

-- | Metadata attached to a response.
data ResponseMeta = ResponseMeta
  { pagination :: Pagination
    } deriving Show

instance FromJSON ResponseMeta where
  parseJSON = JSON.withObject "ResponseMeta" $ \o ->
    ResponseMeta <$> o .: "pagination"

-- | Equivalent to 'Either', but with a different json serialization.
data EitherParser a b = LeftParser a | RightParser b

-- | Equivalent of 'either' for 'EitherParser'.
eitherParser :: (a -> c) -> (b -> c) -> EitherParser a b -> c
eitherParser f _ (LeftParser a) = f a
eitherParser _ g (RightParser b) = g b

instance (FromJSON a, FromJSON b) => FromJSON (EitherParser a b) where
  parseJSON v = fmap LeftParser (JSON.parseJSON v) <|> fmap RightParser (JSON.parseJSON v)

instance (ToJSON a, ToJSON b) => ToJSON (EitherParser a b) where
  toJSON (LeftParser a) = JSON.toJSON a
  toJSON (RightParser b) = JSON.toJSON b

toEitherParser :: Either a b -> EitherParser a b
toEitherParser = either LeftParser RightParser

fromEitherParser :: EitherParser a b -> Either a b
fromEitherParser = eitherParser Left Right

----------------------------------------------------------------------------------------------------
-- Actions
----------------------------------------------------------------------------------------------------

-- | Status of an action.
data ActionStatus =
    -- | Action is still running. The 'Int' argument is the
    --   progress percentage.
    ActionRunning Int
    -- | Action finished successfully. The finishing time is
    --   provided.
  | ActionSuccess ZonedTime
    -- | Action finished with an error. The finishing time is
    --   provided, together with the error message.
  | ActionError ZonedTime Error
    deriving Show

-- | Command performed by an action.
data ActionCommand =
    CreateServer
  | DeleteServer
  | StartServer
  | StopServer
  | ShutdownServer
  | RebootServer
  | SetFirewallRules
  | ApplyFirewall
  | CreateVolume
  | AttachVolume
  | ChangeDNSPtr
    deriving Show

instance FromJSON ActionCommand where
  parseJSON = JSON.withText "ActionCommand" $ \t -> case t of
    "create_server" -> pure CreateServer
    "delete_server" -> pure DeleteServer
    "start_server" -> pure StartServer
    "stop_server" -> pure StopServer
    "shutdown_server" -> pure ShutdownServer
    "reboot_server" -> pure RebootServer
    "set_firewall_rules" -> pure SetFirewallRules
    "apply_firewall" -> pure ApplyFirewall
    "create_volume" -> pure CreateVolume
    "attach_volume" -> pure AttachVolume
    "change_dns_ptr" -> pure ChangeDNSPtr
    _ -> fail $ "Unknown action command " ++ Text.unpack t

-- | Action identifier.
newtype ActionID = ActionID Int deriving (Eq, Ord, Show, FromJSON)

-- | A resource ID is an ID from one of the available resources.
data ResourceID =
    -- | Server ID.
    ResourceServerID ServerID
    -- | Volume ID.
  | ResourceVolumeID VolumeID
    -- | Primary IP ID.
  | ResourcePrimaryIPID PrimaryIPID
    -- | Firewall ID.
  | ResourceFirewallID FirewallID
    deriving Show

instance FromJSON ResourceID where
  parseJSON = JSON.withObject "ResourceID" $ \o -> do
    t <- o .: "type"
    case t :: Text of
      "server" -> ResourceServerID <$> o .: "id"
      "volume" -> ResourceVolumeID <$> o .: "id"
      "primary_ip" -> ResourcePrimaryIPID <$> o .: "id"
      "firewall" -> ResourceFirewallID <$> o .: "id"
      _ -> fail $ "Unknown resource type: " ++ Text.unpack t

-- | Action.
data Action = Action
  { actionID :: ActionID
  , actionCommand :: ActionCommand
  , actionStatus :: ActionStatus
  , actionStarted :: ZonedTime
    -- | Resources the action relates to.
  , actionResources :: [ResourceID]
    } deriving Show

instance FromJSON Action where
  parseJSON = JSON.withObject "Action" $ \o -> do
    status <- do statusText <- o .: "status"
                 case statusText :: Text of
                   "running" -> ActionRunning <$> o .: "progress"
                   "success" -> ActionSuccess <$> o .: "finished"
                   "error" -> ActionError <$> o .: "finished" <*> o .: "error"
                   _ -> fail $ "Unknown action status: " ++ Text.unpack statusText
    Action
     <$> o .: "id"
     <*> o .: "command"
     <*> pure status
     <*> o .: "started"
     <*> o .: "resources"

-- | Get a single action.
getAction :: Token -> ActionID -> IO Action
getAction token (ActionID i) = withoutKey @"action" <$>
  cloudQuery "GET" ("/actions/" <> fromString (show i)) noBody token Nothing

-- | Wait until an action is complete and returns the finishing time.
--   It throws a 'CloudException' if the action fails.
waitForAction :: Token -> ActionID -> IO ZonedTime
waitForAction token i = go
  where
    go :: IO ZonedTime
    go = do action <- getAction token i
            case actionStatus action of
              ActionRunning _ -> threadDelay 250000 *> go
              ActionSuccess t -> pure t
              ActionError _ err -> throwIO $ CloudError err

-- | This class provides a method to get all actions related to the given
--   argument. If a type is instance of 'HasActions', 'waitForActions' can
--   be used to wait for all related actions.
class HasActions a where
  -- | Return all related actions.
  actionsOf :: a -> [ActionID]

-- | Wait for all actions to be finished. If any of the actions throws an
--   error, a 'CloudException' will be thrown. For more control over individual
--   actions, use 'waitForAction'.
--
--   Check instances of 'HasActions' to learn on what types this can be used.
--
waitForActions :: HasActions a => Token -> a -> IO ()
waitForActions token = mapM_ (waitForAction token) . actionsOf

instance HasActions ActionID where
  actionsOf = pure

instance HasActions Action where
  actionsOf = pure . actionID

instance HasActions a => HasActions [a] where
  actionsOf = foldMap actionsOf

----------------------------------------------------------------------------------------------------
-- Datacenters
----------------------------------------------------------------------------------------------------

-- | Datacenter identifier.
newtype DatacenterID = DatacenterID Int deriving (Eq, Ord, Show, FromJSON, ToJSON)

-- | Server types available in a datacenter.
data DatacenterServers = DatacenterServers
  { availableServers :: [ServerTypeID]
  , migrationAvailableServers :: [ServerTypeID]
  , supportedServers :: [ServerTypeID]
    } deriving Show

instance FromJSON DatacenterServers where
  parseJSON = JSON.withObject "DatacenterServers" $ \o -> DatacenterServers
    <$> o .: "available"
    <*> o .: "available_for_migration"
    <*> o .: "supported"

-- | A datacenter within a location.
data Datacenter = Datacenter
  { datacenterID :: DatacenterID
  , datacenterName :: Text
  , datacenterDescription :: Text
  , datacenterLocation :: Location
  , datacenterServers :: DatacenterServers
    } deriving Show

instance FromJSON Datacenter where
  parseJSON = JSON.withObject "Datacenter" $ \o -> Datacenter
    <$> o .: "id"
    <*> o .: "name"
    <*> o .: "description"
    <*> o .: "location"
    <*> o .: "server_types"

-- | Datacenter list with a datacenter recommendation for new servers.
data DatacentersWithRecommendation = DatacentersWithRecommendation
  { datacenters :: [Datacenter]
    -- | The datacenter which is recommended to be used to create
    --   new servers.
  , datacenterRecommendation :: DatacenterID
    } deriving Show

instance FromJSON DatacentersWithRecommendation where
  parseJSON = JSON.withObject "DatacentersWithRecommendation" $ \o -> DatacentersWithRecommendation
    <$> o .: "datacenters"
    <*> o .: "recommendation"

-- | Get all datacenters.
getDatacenters :: Token -> IO DatacentersWithRecommendation
getDatacenters token = cloudQuery "GET" "/datacenters" noBody token Nothing

-- | Get a single datacenter.
getDatacenter :: Token -> DatacenterID -> IO Datacenter
getDatacenter token (DatacenterID i) = withoutKey @"datacenter" <$>
  cloudQuery "GET" ("/datacenters/" <> fromString (show i)) noBody token Nothing

----------------------------------------------------------------------------------------------------
-- Firewalls
----------------------------------------------------------------------------------------------------

-- | Firewall identifier.
newtype FirewallID = FirewallID Int deriving (Eq, Ord, Show, FromJSON, ToJSON)

-- | Traffic direction, whether incoming ('TrafficIn') or outgoing ('TrafficOut').
data TrafficDirection = TrafficIn | TrafficOut deriving (Eq, Show)

instance FromJSON TrafficDirection where
  parseJSON = JSON.withText "TrafficDirection" $ \t ->
    case t of
      "in" -> pure TrafficIn
      "out" -> pure TrafficOut
      _ -> fail $ "Invalid traffic direction: " ++ Text.unpack t

instance ToJSON TrafficDirection where
  toJSON TrafficIn = JSON.String "in"
  toJSON TrafficOut = JSON.String "out"

-- | A port range. It can contain only one port if both ends are the same.
data PortRange = PortRange Int Int deriving Show

-- | A port range containing a single port.
singlePort :: Int -> PortRange
singlePort p = PortRange p p

portRangeParser :: Parser PortRange
portRangeParser = do
  p1 <- Parser.decimal
  hasSecondPart <- (Parser.single '-' *> pure True) <|> pure False
  if hasSecondPart
     then PortRange p1 <$> Parser.decimal
     else pure $ PortRange p1 p1

instance FromJSON PortRange where
  parseJSON = JSON.withText "PortRange" $ \t ->
    either (fail . Parser.errorBundlePretty) pure $
      Parser.runParser (portRangeParser <* Parser.eof) "JSON" t

instance ToJSON PortRange where
  toJSON (PortRange p1 p2) = fromString $
    if p1 == p2
       then show p1
       else show p1 ++ "-" ++ show p2

-- | Protocol used in a 'FirewallRule'.
data FirewallRuleProtocol =
    -- | TCP protocol on the given port range.
    FirewallRuleTCP PortRange
    -- | UDP protocol on the given port range.
  | FirewallRuleUDP PortRange
    -- | ICMP protocol.
  | FirewallRuleICMP
    -- | ESP protocol.
  | FirewallRuleESP
    -- | GRE protocol.
  | FirewallRuleGRE
    deriving Show

-- | A firewall rule.
data FirewallRule = FirewallRule
  { -- | Optional description of the rule.
    firewallRuleDescription :: Maybe Text
    -- | Traffic direction the rule applies to.
  , firewallRuleDirection :: TrafficDirection
    -- | IPs the rule applies to. You can use 'anyIPv4' and/or
    --   'anyIPv6' to allow any IPs.
  , firewallRuleIPs :: NonEmpty (Either IPv4Range IPv6Range)
    -- | Protocol the rule applies to.
  , firewallRuleProtocol :: FirewallRuleProtocol
    } deriving Show

instance FromJSON FirewallRule where
  parseJSON = JSON.withObject "FirewallRule" $ \o -> do
    dir <- o .: "direction"
    ips <-
      case dir of
        TrafficIn -> o .: "source_ips"
        TrafficOut -> o .: "destination_ips"
    protocolType <- o .: "protocol"
    protocol <-
      case protocolType of
        "tcp" -> FirewallRuleTCP <$> o .: "port"
        "udp" -> FirewallRuleUDP <$> o .: "port"
        "icmp" -> pure FirewallRuleICMP
        "esp" -> pure FirewallRuleESP
        "gre" -> pure FirewallRuleGRE
        _ -> fail $ "Invalid protocol: " ++ Text.unpack protocolType
    FirewallRule
      <$> o .:? "description"
      <*> pure dir
      <*> pure (fmap fromEitherParser ips)
      <*> pure protocol

instance ToJSON FirewallRule where
  toJSON rule =
    let dir = firewallRuleDirection rule
        ips = fmap toEitherParser $ firewallRuleIPs rule
    in  JSON.object $
          maybe [] (\t -> pure $ "description" .= t) (firewallRuleDescription rule)
            ++ [ "direction" .= dir
               , case dir of
                   TrafficIn -> "source_ips" .= ips
                   TrafficOut -> "destination_ips" .= ips
                 ]
            ++ (case firewallRuleProtocol rule of
                  FirewallRuleTCP r ->
                    [ "protocol" .= ("tcp" :: Text)
                    , "port" .= r
                      ]
                  FirewallRuleUDP r ->
                    [ "protocol" .= ("udp" :: Text)
                    , "port" .= r
                      ]
                  FirewallRuleICMP -> [ "protocol" .= ("icmp" :: Text) ]
                  FirewallRuleESP -> [ "protocol" .= ("esp" :: Text) ]
                  FirewallRuleGRE -> [ "protocol" .= ("gre" :: Text) ]
                 )

-- | A firewall that can be applied to other resources, via 'applyFirewall'
--   or directly on creation.
data Firewall = Firewall
  { firewallID :: FirewallID
    -- | The firewall's name.
  , firewallName :: Text
    -- | Time the firewall was created.
  , firewallCreated :: ZonedTime
    -- | Servers the firewall has been applied to.
  , firewallServers :: [ServerID]
    -- | Label selectors used to apply the firewall automatically to
    --   matching resources.
  , firewallLabelSelectors :: [LabelSelectorAll]
    -- | Firewall rules.
  , firewallRules :: [FirewallRule]
    -- | Labels attached to the firewall.
  , firewallLabels :: LabelMap
    } deriving Show

instance FromJSON Firewall where
  parseJSON = JSON.withObject "Firewall" $ \o -> do
    xs <- o .: "applied_to"
    ys <- forM xs $ \v ->
            let f o' = do
                  t <- o' .: "type"
                  case t of
                    "server" -> do
                       v' <- o' .: "server"
                       Left <$> JSON.withObject "Server" (.: "id") v'
                    "label_selector" -> do
                       v' <- o' .: "label_selector"
                       Right <$> JSON.withObject "LabelSelector" (.: "selector") v'
                    _ -> fail $ "Invalid applied_to type: " ++ Text.unpack t
            in  JSON.withObject "AppliedTo" f v
    let (servers, labels) = partitionEithers ys
    Firewall
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "created"
      <*> pure servers
      <*> pure labels
      <*> o .: "rules"
      <*> o .: "labels"

-- | Information used to create a new firewall with 'createFirewall'.
data NewFirewall = NewFirewall
  { -- | The firewall's name.
    newFirewallName :: Text
    -- | Labels to attach to the firewall on creation.
  , newFirewallLabels :: [Label]
    -- | Firewall rules.
  , newFirewallRules :: [FirewallRule]
    -- | List of servers to apply the firewall to on creation.
  , newFirewallServers :: [ServerID]
    -- | Label selectors to apply the firewall to matching resources.
  , newFirewallLabelSelectors :: [LabelSelectorAll]
    }

-- | IPv4 range containing every IP.
anyIPv4 :: IPv4Range
anyIPv4 = IPv4.range IPv4.any 0

-- | IPv6 range containing every IP.
anyIPv6 :: IPv6Range
anyIPv6 = IPv6.range IPv6.any 0

-- | Default firewall with two rules:
--
-- * Allow SSH on default port 22 from any address.
-- * Allow ICMP from any address.
--
defaultNewFirewall
  :: Text -- ^ Firewall name.
  -> NewFirewall
defaultNewFirewall name = NewFirewall
  { newFirewallName = name
  , newFirewallLabels = []
  , newFirewallRules =
      [ FirewallRule
          { firewallRuleDescription = Just "SSH"
          , firewallRuleDirection = TrafficIn
          , firewallRuleIPs = Left anyIPv4 :| [Right anyIPv6]
          , firewallRuleProtocol = FirewallRuleTCP $ singlePort 22
            }
      , FirewallRule
          { firewallRuleDescription = Just "ICMP"
          , firewallRuleDirection = TrafficIn
          , firewallRuleIPs = Left anyIPv4 :| [Right anyIPv6]
          , firewallRuleProtocol = FirewallRuleICMP
            }
        ]
  , newFirewallServers = []
  , newFirewallLabelSelectors = []
    }

-- | Result of creating a firewall with 'createFirewall'.
data CreatedFirewall = CreatedFirewall
  { -- | Actions associated with the firewall's creation.
    createdFirewallActions :: [Action]
    -- | The firewall just created.
  , createdFirewall :: Firewall
    } deriving Show

instance HasActions CreatedFirewall where
  actionsOf = fmap actionID . createdFirewallActions

instance FromJSON CreatedFirewall where
  parseJSON = JSON.withObject "CreatedFirewall" $ \o ->
    CreatedFirewall <$> o .: "actions" <*> o .: "firewall"

-- | Get all firewalls in a project.
getFirewalls :: Token -> Maybe Int -> IO (WithMeta "firewalls" [Firewall])
getFirewalls = cloudQuery "GET" "/firewalls" noBody

-- | Get a single firewall.
getFirewall :: Token -> FirewallID -> IO Firewall
getFirewall token (FirewallID i) = withoutKey @"firewall" <$>
  cloudQuery "GET" ("/firewalls/" <> fromString (show i)) noBody token Nothing

-- | Create a firewall.
createFirewall :: Token -> NewFirewall -> IO CreatedFirewall
createFirewall token nfirewall =
  let servers = fmap (\i -> JSON.object [ "server" .= i ])
              $ newFirewallServers nfirewall
      selectors = fmap (\s -> JSON.object [ "selector" .= s ])
                $ newFirewallLabelSelectors nfirewall
      applyTo =
        fmap (\v -> JSON.object [ "type" .= ("server" :: Text)
                                , "server" .= v ]) servers
          ++ fmap (\v -> JSON.object [ "type" .= ("label_selector" :: Text)
                                     , "label_selector" .= v ]) selectors

      body = JSON.object
        [ "name" .= newFirewallName nfirewall
        , "labels" .= toLabelMap (newFirewallLabels nfirewall)
        , "rules" .= newFirewallRules nfirewall
        , "apply_to" .= applyTo
          ]
  in  cloudQuery "POST" "/firewalls" (Just body) token Nothing

-- | Delete a firewall.
deleteFirewall :: Token -> FirewallID -> IO ()
deleteFirewall token (FirewallID i) =
  cloudQuery "DELETE" ("/firewalls/" <> fromString (show i)) noBody token Nothing

-- | Update name and labels of a firewall.
updateFirewall
  :: Token
  -> FirewallID -- ^ Firewall to update.
  -> Text -- ^ New name for the firewall.
  -> [Label] -- ^ New labels for the firewall.
  -> IO Firewall
updateFirewall token (FirewallID i) name labels = withoutKey @"firewall" <$>
  let body = JSON.object
        [ "labels" .= toLabelMap labels
        , "name" .= name
          ]
  in  cloudQuery "PUT" ("/firewalls/" <> fromString (show i)) (Just body) token Nothing

-- | Apply a firewall to resources.
applyFirewall
  :: Token
  -> FirewallID -- ^ Firewall to apply.
  -> [ServerID] -- ^ Servers to apply the firewall to.
  -> [LabelSelectorAll] -- ^ Label selectors to apply.
  -> IO [Action]
applyFirewall token (FirewallID i) servers selectors = withoutKey @"actions" <$>
  let path = "/firewalls/" <> fromString (show i) <> "/actions/apply_to_resources"
      serverf server = JSON.object
        [ "type" .= ("server" :: Text)
        , "server" .= JSON.object [ "id" .= server ] ]
      selectorf selector = JSON.object
        [ "type" .= ("label_selector" :: Text)
        , "label_selector" .= JSON.object [ "selector" .= selector ] ]
      body = JSON.object
        [ "apply_to" .= (fmap serverf servers ++ fmap selectorf selectors)
          ]
  in  cloudQuery "POST" path (Just body) token Nothing

-- | Remove a firewall from the given resources. The firewall itself is not deleted.
--   For that, use 'deleteFirewall'.
removeFirewall
  :: Token
  -> FirewallID -- ^ Firewall to remove.
  -> [ServerID] -- ^ Servers to remove the firewall from.
  -> [LabelSelectorAll] -- ^ Label selectors to remove from the firewall.
  -> IO [Action]
removeFirewall token (FirewallID i) servers selectors = withoutKey @"actions" <$>
  let path = "/firewalls/" <> fromString (show i) <> "/actions/remove_from_resources"
      serverf server = JSON.object
        [ "type" .= ("server" :: Text)
        , "server" .= JSON.object [ "id" .= server ] ]
      selectorf selector = JSON.object
        [ "type" .= ("label_selector" :: Text)
        , "label_selector" .= JSON.object [ "selector" .= selector ] ]
      body = JSON.object
        [ "remove_from" .= (fmap serverf servers ++ fmap selectorf selectors)
          ]
  in  cloudQuery "POST" path (Just body) token Nothing

----------------------------------------------------------------------------------------------------
-- Floating IPs
----------------------------------------------------------------------------------------------------

-- | Floating IP identifier.
newtype FloatingIPID = FloatingIPID Int deriving (Eq, Ord, Show, FromJSON)

----------------------------------------------------------------------------------------------------
-- Images
----------------------------------------------------------------------------------------------------

-- | Image identifier.
newtype ImageID = ImageID Int deriving (Eq, Ord, Show, FromJSON, ToJSON)

-- | Flavor of operative system.
data OSFlavor = Ubuntu | CentOS | Debian | Fedora | Rocky | Alma
              | OpenSUSE | UnknownOS
                deriving Show

instance FromJSON OSFlavor where
  parseJSON = JSON.withText "OSFlavor" $ \t -> case t of
    "ubuntu"   -> pure Ubuntu
    "centos"   -> pure CentOS
    "debian"   -> pure Debian
    "fedora"   -> pure Fedora
    "rocky"    -> pure Rocky
    "alma"     -> pure Alma
    "opensuse" -> pure OpenSUSE
    "unknown"  -> pure UnknownOS
    _ -> fail $ "Unknown OS flavor: " ++ Text.unpack t

-- | Image type.
data ImageType =
    -- | System image with name.
    SystemImage Text
  | AppImage
    -- | Snapshot with size in GB.
  | Snapshot Double
  | Backup ServerID
  | Temporary
    deriving Show

-- | An image that can be mounted to a server.
data Image = Image
  { -- | Image identifier.
    imageID :: ImageID
  , imageName :: Text
  , imageDescription :: Text
  , imageOSFlavor :: OSFlavor
  , imageArchitecture :: Architecture
  , imageType :: ImageType
    -- | Size of the disk contained in the image in GB.
  , imageDiskSize :: Int
  , imageCreated :: ZonedTime
  , imageDeleted :: Maybe ZonedTime
  , imageDeprecated :: Maybe ZonedTime
  , imageLabels :: LabelMap
    } deriving Show

instance FromJSON Image where
  parseJSON = JSON.withObject "Image" $ \o -> do
    typ <- do t <- o .: "type"
              case t :: Text of
                "system" -> SystemImage <$> o .: "name"
                "app" -> pure AppImage
                "snapshot" -> Snapshot <$> o .: "image_size"
                "backup" -> Backup <$> o .: "bound_to"
                "temporary" -> pure Temporary
                _ -> fail $ "Unknown image type: " ++ Text.unpack t
    Image
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "description"
      <*> o .: "os_flavor"
      <*> o .: "architecture"
      <*> pure typ
      <*> o .: "disk_size"
      <*> o .: "created"
      <*> o .: "deleted"
      <*> o .: "deprecated"
      <*> o .: "labels"

-- | Get images.
getImages
  :: Token
  -> Maybe Int -- ^ Page.
  -> IO (WithMeta "images" [Image])
getImages = cloudQuery "GET" "/images" noBody

-- | Get a single image.
getImage :: Token -> ImageID -> IO Image
getImage token (ImageID i) = withoutKey @"image" <$>
  cloudQuery "GET" ("/images/" <> fromString (show i)) noBody token Nothing

----------------------------------------------------------------------------------------------------
-- Load Balancers
----------------------------------------------------------------------------------------------------

-- | Load balancer identifier
newtype LoadBalancerID = LoadBalancerID Int deriving (Eq, Ord, Show, FromJSON, ToJSON)

----------------------------------------------------------------------------------------------------
-- Locations
----------------------------------------------------------------------------------------------------

-- | Cities where Hetzner hosts their servers.
data City =
    Falkenstein
  | Nuremberg
  | Helsinki
  | AshburnVA
  | HillsboroOR
    deriving (Eq, Show)

instance FromJSON City where
  parseJSON = JSON.withText "City" $ \t -> case t of
    "Falkenstein" -> pure Falkenstein
    "Nuremberg" -> pure Nuremberg
    "Helsinki" -> pure Helsinki
    "Ashburn, VA" -> pure AshburnVA
    "Hillsboro, OR" -> pure HillsboroOR
    _ -> fail $ "Unknown city: " ++ Text.unpack t

-- | Location identifier.
newtype LocationID = LocationID Int deriving (Eq, Ord, Show, FromJSON, ToJSON)

-- | A location.
data Location = Location
  { locationCity :: City
  , locationCountry :: Country
  , locationDescription :: Text
  , locationID :: LocationID
  , locationLatitude :: Double
  , locationLongitude :: Double
  , locationName :: Text
  , locationRegion :: Region
    } deriving Show

instance FromJSON Location where
  parseJSON = JSON.withObject "Location" $ \o -> Location
    <$> o .: "city"
    <*> o .: "country"
    <*> o .: "description"
    <*> o .: "id"
    <*> o .: "latitude"
    <*> o .: "longitude"
    <*> o .: "name"
    <*> o .: "network_zone"

-- | Get all locations.
getLocations :: Token -> IO [Location]
getLocations token = withoutKey @"locations" <$>
  cloudQuery "GET" "/locations" noBody token Nothing

-- | Get a single location.
getLocation :: Token -> LocationID -> IO Location
getLocation token (LocationID i) = withoutKey @"location" <$>
  cloudQuery "GET" ("/locations/" <> fromString (show i)) noBody token Nothing

----------------------------------------------------------------------------------------------------
-- Primary IPs
----------------------------------------------------------------------------------------------------

-- | Primary IP identifier.
newtype PrimaryIPID = PrimaryIPID Int deriving (Eq, Ord, Show, FromJSON, ToJSON)

-- | Primary IP.
data PrimaryIP = PrimaryIP
  { -- | Resource the primary IP is assigned to.
    primaryIPAssignee :: ResourceID
    -- | This primary IP is deleted when the resource it is assigned to is deleted.
  , primaryIPAutoDelete :: Bool
  , primaryIPIsBlocked :: Bool
    -- | Point in time where the primary IP was created.
  , primaryIPCreated :: ZonedTime
  , primaryIPDatacenter :: Datacenter
  , primaryIPID :: PrimaryIPID
    -- | Primary IP together with reverse DNS information.
  , primaryIP :: Either (PublicIPInfo Text IPv4) (PublicIPInfo [PublicIPInfo Text IPv6] IPv6Range)
  , primaryIPLabels :: LabelMap
  , primaryIPName :: Text
    } deriving Show

instance FromJSON PrimaryIP where
  parseJSON = JSON.withObject "PrimaryIP" $ \o -> do
    aid <- o .: "assignee_id" :: JSON.Parser Int
    atype <- o .: "assignee_type" :: JSON.Parser Text
    iptype <- o .: "type"
    PrimaryIP
      <$> JSON.parseJSON (JSON.object [ "id" .= aid, "type" .= atype ])
      <*> o .: "auto_delete"
      <*> o .: "blocked"
      <*> o .: "created"
      <*> o .: "datacenter"
      <*> o .: "id"
      <*> (case iptype :: Text of
             "ipv4" -> Left <$> (o .: "dns_ptr" >>= JSON.parseJSON . head)
             "ipv6" -> Right <$> JSON.parseJSON (JSON.Object o)
             _ -> fail $ "Invalid ip type: " ++ Text.unpack iptype
             )
      <*> o .: "labels"
      <*> o .: "name"

-- | Get primary IPs.
getPrimaryIPs
  :: Token
  -> Maybe Int -- ^ Page.
  -> IO (WithMeta "primary_ips" [PrimaryIP])
getPrimaryIPs = cloudQuery "GET" "/primary_ips" noBody

-- | Get a single primary IP.
getPrimaryIP :: Token -> PrimaryIPID -> IO PrimaryIP
getPrimaryIP token (PrimaryIPID i) = withoutKey @"primary_ip" <$>
  cloudQuery "GET" ("/primary_ips" <> fromString (show i)) noBody token Nothing

-- | Set reverse DNS for a primary IP.
--
--   * If the primary IP corresponds to an IPv4, the reverse DNS setting's
--     IP /must/ coincide with the primary IP's IPv4.
--
--   * If the primary IP corresponds to an IPv6, the reverse DNS setting's
--     IP /must/ be within the primary IP's IPv6 range.
--
setReverseDNS
  :: Token
     -- | Primary IP to set reverse DNS for.
  -> PrimaryIPID
     -- | Reverse DNS settings.
  -> PublicIPInfo Text (Either IPv4 IPv6)
  -> IO Action
setReverseDNS token (PrimaryIPID i) (PublicIPInfo dns ip) = withoutKey @"action" <$>
  cloudQuery "POST" ("/primary_ips/" <> fromString (show i) <> "/actions/change_dns_ptr")
    (Just $ either (JSON.toJSON . PublicIPInfo dns) (JSON.toJSON . PublicIPInfo dns) ip)
    token Nothing

----------------------------------------------------------------------------------------------------
-- Networks
----------------------------------------------------------------------------------------------------

-- | Network identifier.
newtype NetworkID = NetworkID Int deriving (Eq, Ord, Show, FromJSON, ToJSON)

-- | A route that sends all packets for a given destination to
--   a given gateway.
data Route = Route
  { routeDestination :: IPv4Range
  , routeGateway :: IPv4
    } deriving Show

instance FromJSON Route where
  parseJSON = JSON.withObject "Route" $ \o -> Route
    <$> o .: "destination"
    <*> o .: "gateway"

instance ToJSON Route where
  toJSON route = JSON.object
    [ "destination" .= routeDestination route
    , "gateway" .= routeGateway route
      ]

-- | Types of subnetworks.
data SubnetType = SubnetCloud | SubnetServer | SubnetVSwitch deriving (Eq, Show)

instance FromJSON SubnetType where
  parseJSON = JSON.withText "SubnetType" $ \t -> case t of
    "cloud" -> pure SubnetCloud
    "server" -> pure SubnetServer
    "vswitch" -> pure SubnetVSwitch
    _ -> fail $ "Invalid subnet type: " ++ Text.unpack t

instance ToJSON SubnetType where
  toJSON t = case t of
    SubnetCloud -> "cloud"
    SubnetServer -> "server"
    SubnetVSwitch -> "vswitch"

-- | Subnets divide the IP range of a parent 'Network'.
data Subnet = Subnet
  { subnetGateway :: IPv4
  , subnetIPRange :: IPv4Range
  , subnetRegion :: Region
  , subnetType :: SubnetType
    } deriving Show

instance FromJSON Subnet where
  parseJSON = JSON.withObject "Subnet" $ \o -> Subnet
    <$> o .: "gateway"
    <*> o .: "ip_range"
    <*> o .: "network_zone"
    <*> o .: "type"

instance ToJSON Subnet where
  toJSON subnet = JSON.object
    [ "gateway" .= subnetGateway subnet
    , "ip_range" .= subnetIPRange subnet
    , "network_zone" .= subnetRegion subnet
    , "type" .= subnetType subnet
      ]

-- | A private network.
data Network = Network
  { networkCreated :: ZonedTime
  , networkID :: NetworkID
  , networkIPRange :: IPv4Range
  , networkLabels :: LabelMap
  , networkLoadBalancers :: [LoadBalancerID]
  , networkName :: Text
  , networkRoutes :: [Route]
  , networkServers :: [ServerID]
  , networkSubnets :: [Subnet]
    } deriving Show

instance FromJSON Network where
  parseJSON = JSON.withObject "Network" $ \o -> Network
    <$> o .: "created"
    <*> o .: "id"
    <*> o .: "ip_range"
    <*> o .: "labels"
    <*> o .: "load_balancers"
    <*> o .: "name"
    <*> o .: "routes"
    <*> o .: "servers"
    <*> o .: "subnets"

-- | Network creation configuration to be used with 'createNetwork'.
data NewNetwork = NewNetwork
  { newNetworkIPRange :: IPv4Range
  , newNetworkLabels :: [Label]
  , newNetworkName :: Text
  , newNetworkRoutes :: [Route]
  , newNetworkSubnets :: [Subnet]
    }

instance ToJSON NewNetwork where
  toJSON nnetwork = JSON.object
    [ "ip_range" .= newNetworkIPRange nnetwork
    , "labels" .= toLabelMap (newNetworkLabels nnetwork)
    , "name" .= newNetworkName nnetwork
    , "routes" .= newNetworkRoutes nnetwork
    , "subnets" .= newNetworkSubnets nnetwork
      ]

-- | Default network configuration for new networks.
defaultNewNetwork
  :: Text -- ^ Network name.
  -> IPv4Range -- ^ IP range of the network.
  -> NewNetwork
defaultNewNetwork name iprange = NewNetwork
  { newNetworkIPRange = iprange
  , newNetworkLabels = []
  , newNetworkName = name
  , newNetworkRoutes = []
  , newNetworkSubnets = []
    }

-- | Get networks.
getNetworks
  :: Token
  -> Maybe Int -- ^ Page.
  -> IO (WithMeta "networks" [Network])
getNetworks = cloudQuery "GET" "/networks" noBody

-- | Get a single network.
getNetwork :: Token -> NetworkID -> IO Network
getNetwork token (NetworkID i) = withoutKey @"network" <$>
  cloudQuery "GET" ("/networks/" <> fromString (show i)) noBody token Nothing

-- | Create a new network.
createNetwork :: Token -> NewNetwork -> IO Network
createNetwork token new = withoutKey @"network" <$>
  cloudQuery "POST" "/networks" (Just new) token Nothing

-- | Delete a network.
deleteNetwork :: Token -> NetworkID -> IO ()
deleteNetwork token (NetworkID i) =
  cloudQuery "DELETE" ("/networks/" <> fromString (show i)) noBody token Nothing

-- | Update name and labels of a network.
updateNetwork
  :: Token
  -> NetworkID -- ^ Network to update.
  -> Text -- ^ New name for the network.
  -> [Label] -- ^ New labels for the network.
  -> IO Network
updateNetwork token (NetworkID i) name labels = withoutKey @"network" <$>
  let body = JSON.object
        [ "labels" .= toLabelMap labels
        , "name" .= name
          ]
  in  cloudQuery "PUT" ("/networks/" <> fromString (show i)) (Just body) token Nothing

----------------------------------------------------------------------------------------------------
-- Pricing
----------------------------------------------------------------------------------------------------

-- | A resource's price.
data Price = Price
  { grossPrice :: Scientific
  , netPrice :: Scientific
    } deriving (Eq, Show)

-- | The 'Ord' instance can be used to compare prices.
--   Only the gross price is used for comparisons.
instance Ord Price where
  compare p p' = compare (grossPrice p) (grossPrice p')

-- | Prices are written as strings. This internal type helps
--   parsing that string in the 'FromJSON' instance.
newtype PriceString = PriceString { fromPriceString :: Scientific }

instance FromJSON PriceString where
  parseJSON = JSON.withText "PriceString" $ \t ->
   either (fail . Parser.errorBundlePretty) (pure . PriceString) $
     Parser.runParser (Parser.scientific :: Parser Scientific) "JSON" t

instance FromJSON Price where
  parseJSON = JSON.withObject "Price" $ \o ->
    liftA2 Price (fromPriceString <$> o .: "gross")
                 (fromPriceString <$> o .: "net")

-- | The price of a resource in a location.
--   Hourly pricing is unavailable for some resources.
data PriceInLocation = PriceInLocation
  { -- | Location name.
    priceLocation :: Text
    -- | Hourly price.
  , hourlyPrice :: Maybe Price
    -- | Monthly price.
  , monthlyPrice :: Price
    -- | Outgoing traffic included (in bytes).
  , includedTraffic :: Int
    } deriving Show

instance FromJSON PriceInLocation where
  parseJSON = JSON.withObject "PriceInLocation" $ \o -> PriceInLocation
    <$> o .:  "location"
    <*> o .:? "price_hourly"
    <*> o .:  "price_monthly"
    <*> o .:  "included_traffic"

----------------------------------------------------------------------------------------------------
-- Servers
----------------------------------------------------------------------------------------------------

-- | A server status.
data ServerStatus =
    Running
  | Initializing
  | Starting
  | Stopping
  | Off
  | Deleting
  | Migrating
  | Rebuilding
  | StatusUnknown
    deriving (Eq, Show)

instance FromJSON ServerStatus where
  parseJSON = JSON.withText "ServerStatus" $ \t -> case t of
    "running" -> pure Running
    "initializing" -> pure Initializing
    "starting" -> pure Starting
    "stopping" -> pure Stopping
    "off" -> pure Off
    "deleting" -> pure Deleting
    "migrating" -> pure Migrating
    "rebuilding" -> pure Rebuilding
    "unknown" -> pure StatusUnknown
    _ -> fail $ "Invalid server status: " ++ Text.unpack t

-- | Server identifier.
newtype ServerID = ServerID Int deriving (Show, FromJSON, ToJSON)

-- | A server.
data Server = Server
  { serverCreated :: ZonedTime
  , serverDatacenter :: Datacenter
  , serverID :: ServerID
  , serverImage :: Image
  , serverLabels :: LabelMap
  , serverIsLocked :: Bool
  , serverName :: Text
  , serverPublicNetwork :: PublicNetwork
  , serverType :: ServerType
  , serverStatus :: ServerStatus
    } deriving Show

instance FromJSON Server where
  parseJSON = JSON.withObject "Server" $ \o -> Server
    <$> o .: "created"
    <*> o .: "datacenter"
    <*> o .: "id"
    <*> o .: "image"
    <*> o .: "labels"
    <*> o .: "locked"
    <*> o .: "name"
    <*> o .: "public_net"
    <*> o .: "server_type"
    <*> o .: "status"

-- | Server creation configuration to be used with 'createServer'.
data NewServer = NewServer
  { -- | Automount attached volumes.
    newServerAutomount :: Bool
  , newServerLocation :: Maybe (Either DatacenterID LocationID)
  , newServerFirewalls :: [FirewallID]
  , newServerImage :: ImageID
  , newServerLabels :: [Label]
    -- | Name of the server. Must be unique per project and a valid
    --   hostname as per RFC 1123.
  , newServerName :: Text
    -- | List of networks the server will be attached to.
  , newServerNetworks :: [NetworkID]
  , newServerEnableIPv4 :: Bool
  , newServerEnableIPv6 :: Bool
  , newServerType :: ServerTypeID
  , newServerSSHKeys :: [SSHKeyID]
    -- | Whether to start the server after creation.
  , newServerStart :: Bool
    -- | Volumes to attach to the server after creation.
  , newServerVolumes :: [VolumeID]
    } deriving Show

instance ToJSON NewServer where
  toJSON nserver = JSON.object $ mconcat
    [ pure $ "automount" .= (newServerAutomount nserver && not (null $ newServerVolumes nserver))
    , maybe mempty (pure . either ("datacenter".=) ("location".=)) $ newServerLocation nserver
    , pure $ "firewalls" .=
        fmap (\fwid -> JSON.object [ "firewall" .= fwid ]) (newServerFirewalls nserver)
    , pure $ "image" .= newServerImage nserver
    , pure $ "labels" .= toLabelMap (newServerLabels nserver)
    , pure $ "name" .= newServerName nserver
    , pure $ "networks" .= newServerNetworks nserver
    , pure $ "public_net" .= JSON.object
        [ "enable_ipv4" .= newServerEnableIPv4 nserver
        , "enable_ipv6" .= newServerEnableIPv6 nserver
          ]
    , pure $ "server_type" .= newServerType nserver
    , pure $ "ssh_keys" .= newServerSSHKeys nserver
    , pure $ "start_after_create" .= newServerStart nserver
    , pure $ "volumes" .= newServerVolumes nserver
      ]

-- | Default server configuration that can be used as a starting point
--   for a custom server configuration.
--
--   Note that by default no SSH key is installed, which means you'll need the
--   password in the response in order to access the server (you will also receive an
--   e-mail with the password).
--
defaultNewServer
  :: Text -- ^ Server name.
  -> NewServer
defaultNewServer name = NewServer
  { newServerAutomount = True
  , newServerLocation = Nothing
  , newServerFirewalls = []
  , newServerImage = ImageID 67794396
  , newServerLabels = []
  , newServerName = name
  , newServerNetworks = []
  , newServerEnableIPv4 = True
  , newServerEnableIPv6 = True
  , newServerType = ServerTypeID 22
  , newServerSSHKeys = []
  , newServerStart = True
  , newServerVolumes = []
    }

-- | A server that was just created with 'createServer'.
data CreatedServer = CreatedServer
  { -- | Server creation action. You can use 'waitForAction'
    --   to wait until the server creation is finished.
    createdServerAction :: Action
    -- | Additional server actions that are run after the server
    --   is created, like mounting volumes or starting the server.
  , createdServerNextActions :: [Action]
    -- | Root password returned when no SSH keys are provided.
  , createdServerPassword :: Maybe Text
    -- | The server being created.
  , createdServer :: Server
    } deriving Show

instance HasActions CreatedServer where
  actionsOf cserver =
    actionID (createdServerAction cserver) :
      fmap actionID (createdServerNextActions cserver)

instance FromJSON CreatedServer where
  parseJSON = JSON.withObject "CreatedServer" $ \o -> CreatedServer
    <$> o .: "action"
    <*> o .: "next_actions"
    <*> o .: "root_password"
    <*> o .: "server"

-- | Get servers.
getServers
  :: Token
  -> Maybe Int -- ^ Page.
  -> IO (WithMeta "servers" [Server])
getServers = cloudQuery "GET" "/servers" noBody

-- | Get a single server.
getServer :: Token -> ServerID -> IO Server
getServer token (ServerID i) = withoutKey @"server" <$>
  cloudQuery "GET" ("/servers/" <> fromString (show i)) noBody token Nothing

-- | Create a new server.
createServer :: Token -> NewServer -> IO CreatedServer
createServer token nserver =
  cloudQuery "POST" "/servers" (Just nserver) token Nothing

-- | Delete a server.
deleteServer :: Token -> ServerID -> IO Action
deleteServer token (ServerID i) = withoutKey @"action" <$>
  cloudQuery "DELETE" ("/servers/" <> fromString (show i)) noBody token Nothing

-- | Set reverse DNS entry for a server.
setServerReverseDNS :: Token -> ServerID -> PublicIPInfo Text (Either IPv4 IPv6) -> IO Action
setServerReverseDNS token (ServerID i) ipinfo = withoutKey @"action" <$>
  let ip = either JSON.toJSON JSON.toJSON $ publicIP ipinfo
  in  cloudQuery "POST"
        ("/servers/" <> fromString (show i) <> "/actions/change_dns_ptr") 
        (Just $ ipinfo { publicIP = ip }) token Nothing

-- | Turn server on.
powerOnServer :: Token -> ServerID -> IO Action
powerOnServer token (ServerID i) = withoutKey @"action" <$>
  cloudQuery "POST" ("/servers/" <> fromString (show i) <> "/actions/poweron") noBody token Nothing

-- | Turn server off. This is not a graceful shutdown.
powerOffServer :: Token -> ServerID -> IO Action
powerOffServer token (ServerID i) = withoutKey @"action" <$>
  cloudQuery "POST" ("/servers/" <> fromString (show i) <> "/actions/poweroff") noBody token Nothing

-- | Send ACPI shutdown request to a server. Use this instead of 'powerOffServer' if you
--   wish for a graceful shutdown. However, the returned action finishes when the
--   shutdown request is sent, so 'waitForAction' won't help you to tell whether the
--   server is actually off.
shutdownServer :: Token -> ServerID -> IO Action
shutdownServer token (ServerID i) = withoutKey @"action" <$>
  cloudQuery "POST" ("/servers/" <> fromString (show i) <> "/actions/shutdown") noBody token Nothing

-- | Send ACPI reboot request to a server.
rebootServer :: Token -> ServerID -> IO Action
rebootServer token (ServerID i) = withoutKey @"action" <$>
  cloudQuery "POST" ("/servers/" <> fromString (show i) <> "/actions/reboot") noBody token Nothing

-- | Change a server's type. The target type must have equal or larger disk.
--   The server needs to be turned off before changing its type.
changeServerType
  :: Token -> ServerID -> ServerTypeID
  -> Bool -- ^ Should the disk also be upgraded? If not, it will stay the same size.
  -> IO Action
changeServerType token (ServerID i) stype upgrade = withoutKey @"action" <$>
  let body = JSON.object
        [ "server_type" .= stype
        , "upgrade_disk" .= upgrade
          ]
  in  cloudQuery "POST"
        ("/servers/" <> fromString (show i) <> "/actions/change_type")
        (Just body) token Nothing

----------------------------------------------------------------------------------------------------
-- Server Types
----------------------------------------------------------------------------------------------------

-- | Computer architecture.
data Architecture = X86 | Arm deriving (Eq, Show)

instance FromJSON Architecture where
  parseJSON = JSON.withText "Architecture" $ \t -> case t of
    "x86" -> pure X86
    "arm" -> pure Arm
    _ -> fail $ "Unknown architecture: " ++ Text.unpack t

-- | Type of server boot drive.
data StorageType = LocalStorage | NetworkStorage deriving (Eq, Show)

instance FromJSON StorageType where
  parseJSON = JSON.withText "StorageType" $ \t -> case t of
    "local" -> pure LocalStorage
    "network" -> pure NetworkStorage
    _ -> fail $ "Unknown storage type: " ++ Text.unpack t

-- | CPU types available.
data CPUType = SharedCPU | DedicatedCPU deriving (Eq, Show)

instance FromJSON CPUType where
  parseJSON = JSON.withText "CPUType" $ \t -> case t of
    "shared" -> pure SharedCPU
    "dedicated" -> pure DedicatedCPU
    _ -> fail $ "Unknown CPU type: " ++ Text.unpack t

-- | Server type identifier.
newtype ServerTypeID = ServerTypeID Int deriving (Eq, Ord, Show, FromJSON, ToJSON)

-- | Server characteristics.
data ServerType = ServerType
  { serverArchitecture :: Architecture
  , serverCores :: Int
  , serverCPUType :: CPUType
  , serverDeprecated :: Bool
  , serverTypeDescription :: Text
    -- | Disk size a server of this type has in GB.
  , serverDisk :: Int
  , serverTypeID :: ServerTypeID
    -- | Memory a server of this type has in GB.
  , serverMemory :: Int
  , serverTypeName :: Text
  , serverPricing :: [PriceInLocation]
  , serverStorageType :: StorageType
    } deriving Show

instance FromJSON ServerType where
  parseJSON = JSON.withObject "ServerType" $ \o -> ServerType
    <$> o .: "architecture"
    <*> o .: "cores"
    <*> o .: "cpu_type"
    <*> (fromMaybe False <$> o .: "deprecated")
    <*> o .: "description"
    <*> o .: "disk"
    <*> o .: "id"
    <*> o .: "memory"
    <*> o .: "name"
    <*> o .: "prices"
    <*> o .: "storage_type"

-- | Get all server types.
getServerTypes :: Token -> Maybe Int -> IO (WithMeta "server_types" [ServerType])
getServerTypes = cloudQuery "GET" "/server_types" noBody

----------------------------------------------------------------------------------------------------
-- SSH Keys
----------------------------------------------------------------------------------------------------

-- | SSH key identifier.
newtype SSHKeyID = SSHKeyID Int deriving (Eq, Ord, Show, FromJSON, ToJSON)

-- | SSH key information.
data SSHKey = SSHKey
  { sshKeyCreated :: ZonedTime
  , sshKeyFingerprint :: Fingerprint
  , sshKeyID :: SSHKeyID
  , sshKeyLabels :: LabelMap
  , sshKeyName :: Text
  , sshKeyPublicKey :: Text
    } deriving Show

instance FromJSON SSHKey where
  parseJSON = JSON.withObject "SSHKey" $ \o -> SSHKey
    <$> o .: "created"
    <*> (fingerprint <$> o .: "fingerprint")
    <*> o .: "id"
    <*> o .: "labels"
    <*> o .: "name"
    <*> o .: "public_key"

-- | Get all uploaded SSH keys.
getSSHKeys :: Token -> IO [SSHKey]
getSSHKeys token = withoutKey @"ssh_keys" <$>
  cloudQuery "GET" "/ssh_keys" noBody token Nothing

-- | Get a single SSH key.
getSSHKey :: Token -> SSHKeyID -> IO SSHKey
getSSHKey token (SSHKeyID i) = withoutKey @"ssh_key" <$>
  cloudQuery "GET" ("/ssh_keys/" <> fromString (show i)) noBody token Nothing

-- | Upload an SSH key.
createSSHKey
  :: Token
  -> Text -- ^ Name for the SSH key.
  -> Text -- ^ Public key.
  -> [Label] -- ^ List of labels to attach to the key.
  -> IO SSHKey
createSSHKey token name public labels = withoutKey @"ssh_key" <$>
  let body = JSON.object
        [ "labels" .= toLabelMap labels
        , "name" .= name
        , "public_key" .= public
          ]
  in  cloudQuery "POST" "/ssh_keys" (Just body) token Nothing

-- | Delete an SSH key.
deleteSSHKey :: Token -> SSHKeyID -> IO ()
deleteSSHKey token (SSHKeyID i) =
  cloudQuery "DELETE" ("/ssh_keys/" <> fromString (show i)) noBody token Nothing

-- | Update name and labels of an SSH key.
updateSSHKey
  :: Token
  -> SSHKeyID
  -> Text -- ^ New name for the key.
  -> [Label] -- ^ New labels for the key.
  -> IO SSHKey -- ^ Updated SSH key.
updateSSHKey token (SSHKeyID i) name labels = withoutKey @"ssh_key" <$>
  let body = JSON.object
        [ "labels" .= toLabelMap labels
        , "name" .= name
          ]
  in  cloudQuery "PUT" ("/ssh_keys/" <> fromString (show i)) (Just body) token Nothing

----------------------------------------------------------------------------------------------------
-- Volumes
----------------------------------------------------------------------------------------------------

-- | Volume identifier.
newtype VolumeID = VolumeID Int deriving (Eq, Ord, Show, FromJSON, ToJSON)

-- | Volume format.
data VolumeFormat = EXT4 | XFS deriving (Eq, Show)

instance FromJSON VolumeFormat where
  parseJSON = JSON.withText "VolumeFormat" $ \t -> case t of
    "ext4" -> pure EXT4
    "xfs" -> pure XFS
    _ -> fail $ "Invalid volume format: " ++ Text.unpack t

instance ToJSON VolumeFormat where
  toJSON EXT4 = JSON.String "ext4"
  toJSON XFS = JSON.String "xfs"

-- | Volume status.
data VolumeStatus = VolumeCreating | VolumeAvailable deriving (Eq, Show)

instance FromJSON VolumeStatus where
  parseJSON = JSON.withText "VolumeStatus" $ \t -> case t of
    "creating" -> pure VolumeCreating
    "available" -> pure VolumeAvailable
    _ -> fail $ "Invalid volume status: " ++ Text.unpack t

-- | A volume that can be attached to a server.
data Volume = Volume
  { volumeCreated :: ZonedTime
    -- | Volume format. It returns 'Nothing' if the volume hasn't been formatted yet.
  , volumeFormat :: Maybe VolumeFormat
  , volumeID :: VolumeID
  , volumeLabels :: LabelMap
    -- | Device path on the file system for the volume.
  , volumePath :: FilePath
  , volumeLocation :: Location
  , volumeName :: Text
    -- | ID of the server the volume is attached to, if any.
  , volumeServer :: Maybe ServerID
    -- | Size of the volume in GB.
  , volumeSize :: Int
  , volumeStatus :: VolumeStatus
    } deriving Show

instance FromJSON Volume where
  parseJSON = JSON.withObject "Volume" $ \o -> Volume
    <$> o .: "created"
    <*> o .: "format"
    <*> o .: "id"
    <*> o .: "labels"
    <*> o .: "linux_device"
    <*> o .: "location"
    <*> o .: "name"
    <*> o .: "server"
    <*> o .: "size"
    <*> o .: "status"

-- | Attach a volume to a server. The boolean parameter
--   indicates whether the volume will be auto-mounted.
data AttachToServer = AttachToServer ServerID Bool

-- | Volume creation configuration to be used with 'createVolume'.
data NewVolume = NewVolume
  { -- | If specified, volume will be formatted according
    --   to the given format.
    newVolumeFormat :: Maybe VolumeFormat
  , newVolumeLabels :: [Label]
    -- | You can either create a volume in a location or
    --   directly attach the volume to a server.
  , newVolumeLocation :: Either LocationID AttachToServer
  , newVolumeName :: Text
    -- | Size of the volume in GB. It must be at least 10.
  , newVolumeSize :: Int
    }

instance ToJSON NewVolume where
  toJSON nvolume = JSON.object $ mconcat
    [ maybe mempty (pure . ("format".=)) $ newVolumeFormat nvolume
    , pure $ "labels" .= toLabelMap (newVolumeLabels nvolume)
    , let f :: AttachToServer -> [JSON.Pair]
          f (AttachToServer i b) = [ "server" .= i, "automount" .= b ]
      in  either (pure . ("location".=)) f $ newVolumeLocation nvolume
    , pure $ "name" .= newVolumeName nvolume
    , pure $ "size" .= newVolumeSize nvolume
      ]

-- | A volume created with 'createVolume'.
data CreatedVolume = CreatedVolume
  { createdVolumeAction :: Action
  , createdVolumeNextActions :: [Action]
  , createdVolume :: Volume
    } deriving Show

instance HasActions CreatedVolume where
  actionsOf cvolume =
    actionID (createdVolumeAction cvolume) :
      fmap actionID (createdVolumeNextActions cvolume)

instance FromJSON CreatedVolume where
  parseJSON = JSON.withObject "CreatedVolume" $ \o -> CreatedVolume
    <$> o .: "action"
    <*> o .: "next_actions"
    <*> o .: "volume"

-- | Get volumes.
getVolumes :: Token -> Maybe Int -> IO (WithMeta "volumes" [Volume])
getVolumes = cloudQuery "GET" "/volumes" noBody

-- | Get a single volume.
getVolume :: Token -> VolumeID -> IO Volume
getVolume token (VolumeID i) = withoutKey @"volume" <$>
  cloudQuery "GET" ("/volumes/" <> fromString (show i)) noBody token Nothing

-- | Create a new volume.
createVolume :: Token -> NewVolume -> IO CreatedVolume
createVolume token nvolume =
  cloudQuery "POST" "/volumes" (Just nvolume) token Nothing

-- | Delete a volume.
deleteVolume :: Token -> VolumeID -> IO ()
deleteVolume token (VolumeID i) =
  cloudQuery "DELETE" ("/volumes/" <> fromString (show i)) noBody token Nothing

-- | Update name and labels of a volume.
updateVolume
  :: Token
  -> VolumeID
  -> Text -- ^ New name for the volume.
  -> [Label] -- ^ New labels for the volume.
  -> IO Volume -- ^ Updated volume.
updateVolume token (VolumeID i) name labels = withoutKey @"volume" <$>
  let body = JSON.object
        [ "labels" .= toLabelMap labels
        , "name" .= name
          ]
  in  cloudQuery "PUT" ("/volumes/" <> fromString (show i)) (Just body) token Nothing
