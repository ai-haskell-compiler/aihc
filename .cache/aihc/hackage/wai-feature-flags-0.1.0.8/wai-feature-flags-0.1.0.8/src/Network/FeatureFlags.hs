{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Use feature flags in WAI applications.
--
-- A feature flag is a way to toggle functionality on or off without having to
-- redeploy the application. Feature flags have many possible uses, one is
-- making it safer to release new functionality by providing a way to turn it
-- off immediately if it misbehaves.
--
-- An application using this library first needs to define which feature flags
-- is supports. This is done by creating a record type containing only boolean
-- fields and adding a 'Flags' instance to it.
--
-- > data Features
-- >   = Features
-- >       { openWindow :: Bool,
-- >         feedPigeons :: Bool
-- >       }
-- >   deriving (Generic)
-- >
-- > instance Flags Features
--
-- Then we need a place to persist flag data. This library provides a
-- 'memoryStore' but it doesn't remember flag states across restarts. For
-- production applications it's probably best to implement a 'Store' that reads
-- and writes flag data to the database or key,value store backing your project.
--
-- 'mkApplication' provides a frontend from which each feature flag can be
-- fully enabled, fully disabled, or enabled for a specific percentage of
-- traffic. It is compatible with @Wai@-based web frameworks like @spock@,
-- @scotty@, and @servant@. Setup instructions will be different for each. If
-- you're having trouble integrating this in your choice of web framework please
-- feel free to [open an issue](https://github.com/jwoudenberg/wai-feature-flags/issues).
--
-- Now you're all set up. You can use 'fetch' to read your feature flags from
-- your store and can use their values in conditionals. For a full example check
-- out this [sample application](https://github.com/jwoudenberg/wai-feature-flags/blob/trunk/example-app/Main.hs).
module Network.FeatureFlags
  ( -- * Flags
    Flags,
    fetch,

    -- * Store
    Store (..),
    memoryStore,

    -- * Feature flag frontend
    mkApplication,
  )
where

import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.IORef as IORef
import Data.Kind (Type)
import qualified Data.Maybe as Maybe
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as Read
import qualified Data.Word as Word
import qualified Debug.Trace as Debug
import GHC.Generics
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, TypeError, symbolVal)
import qualified Network.Wai as Wai
import qualified Paths_wai_feature_flags as Paths
import System.Random.SplitMix (SMGen, newSMGen, nextWord32)

-- FRONTEND

-- | Create a WAI application that serves a frontend for modifying feature flag
-- states. How you embed this into your real application depends on the web
-- framework you're using.
mkApplication :: Flags flags => Store flags -> IO Wai.Application
mkApplication store =
  application store <$> Paths.getDataFileName "frontend/index.html"

application :: Flags flags => Store flags -> FilePath -> Wai.Application
application store frontend req respond = do
  case (Wai.requestMethod req, Wai.pathInfo req) of
    ("GET", []) ->
      respond $
        Wai.responseFile
          (toEnum 200)
          [("Content-Type", "text/html; charset=UTF-8")]
          frontend
          Nothing
    ("GET", ["flags"]) -> do
      configs <- readFlagConfigs (flags store) store
      respond $
        Wai.responseLBS
          (toEnum 200)
          [("Content-Type", "application/json")]
          (Aeson.encode configs)
    ("PUT", ["flags", flagName]) -> do
      body <- Wai.lazyRequestBody req
      case Aeson.decode body of
        Nothing ->
          respond (Wai.responseLBS (toEnum 400) [] "")
        Just percent -> do
          setFlag flagName percent store
          respond (Wai.responseLBS (toEnum 200) [] "")
    _ -> respond (Wai.responseLBS (toEnum 404) [] "")

setFlag :: T.Text -> Percent -> Store flags -> IO ()
setFlag flag (Percent percentage) store =
  writeKey
    store
    (TE.encodeUtf8 flag)
    (TE.encodeUtf8 (T.pack (show (min 100 percentage))))

readFlagConfigs :: [T.Text] -> Store flags -> IO (Map.HashMap T.Text Percent)
readFlagConfigs keys store = do
  let defaults = zip keys (repeat (Percent 0))
  let keySet = Set.fromList keys
  stored' <- Maybe.catMaybes . map decodeFlagConfig <$> readKeys store
  let stored = filter (\(k, _) -> Set.member k keySet) stored'
  pure $ Map.fromList $ defaults <> stored

decodeFlagConfig :: (B.ByteString, B.ByteString) -> Maybe (T.Text, Percent)
decodeFlagConfig (flag, config) = do
  flag <- either (const Nothing) Just $ TE.decodeUtf8' flag
  enabledString <- either (const Nothing) Just $ TE.decodeUtf8' config
  (enabledInt, _) <- either (const Nothing) Just $ Read.decimal enabledString
  pure (flag, percent enabledInt)

-- STORES

-- | A type describing a store in which feature flag data can be saved. You are
-- recommended to define your own stores using a persistence mechanism of your
-- choice.
data Store flags
  = Store
      { -- | Read all key,value pairs from the store.
        readKeys :: IO [(B.ByteString, B.ByteString)],
        -- | Save a key,value pair to the store. Create the key,value pair if it
        -- does not exist yet and overwrite it otherwise.
        writeKey :: B.ByteString -> B.ByteString -> IO ()
      }

-- | An in-memory store that does not persist feature flag data across
-- application restarts. Suitable for experimentation but not recommended for
-- production use.
memoryStore :: IO (Store flags)
memoryStore = do
  ref <- IORef.newIORef Map.empty
  pure
    Store
      { readKeys = Map.toList <$> IORef.readIORef ref,
        writeKey = \key value ->
          IORef.atomicModifyIORef' ref (\xs -> (Map.insert key value xs, ()))
      }

-- | Read feature flag states out of the store. The states of flags enabled for
-- part of the traffic will be determined by die-roll.
--
-- The default state for new flags and flags we cannot find values for in the
-- store is off. This library offers no way to set other defaults to keep it as
-- simple as possibe. You are encouraged to phrase your flag names in such a
-- way that off corresponds to what you'd like the default value to be, i.e.
-- @enableExperimentalDoodad@ is likely safer than @disableExperimentalDoodad@.
fetch :: forall flags. Flags flags => Store flags -> IO flags
fetch store = do
  let keys = flags (Proxy :: Proxy flags)
  configs <- readFlagConfigs keys store
  smgen <- newSMGen
  pure . fst $ generate configs smgen

-- PERCENT

newtype Percent = Percent Word.Word32 deriving (Aeson.ToJSON, Aeson.FromJSON)

percent :: Word.Word32 -> Percent
percent = Percent . min 100

-- FLAGS

-- | The feature flags you define are described by a type you create yourself.
-- It needs to be a record though, with every field a boolean. Then we add a
-- `Flags` instance to it so this library is able to work with the type.
--
-- > data Features
-- >   = Features
-- >       { openWindow :: Bool,
-- >         feedPigeons :: Bool
-- >       }
-- >   deriving (Generic)
-- >
-- > instance Flags Features
class Flags flags where
  generate :: Map.HashMap T.Text Percent -> SMGen -> (flags, SMGen)

  flags :: proxy flags -> [T.Text]

  default generate :: (Generic flags, GFlags (Rep flags)) => Map.HashMap T.Text Percent -> SMGen -> (flags, SMGen)
  generate configs gen = first GHC.Generics.to $ ggenerate configs gen

  default flags :: (Generic flags, GFlags (Rep flags)) => proxy flags -> [T.Text]
  flags _ = gflags (Proxy :: Proxy (Rep flags))

class GFlags flags where
  ggenerate :: Map.HashMap T.Text Percent -> SMGen -> (flags g, SMGen)

  gflags :: proxy flags -> [T.Text]

instance GFlags fields => GFlags (D1 m (C1 ('MetaCons s f 'True) fields)) where
  ggenerate configs gen = first (M1 . M1) $ ggenerate configs gen

  gflags _ = gflags (Proxy :: Proxy fields)

instance (GFlags l, GFlags r) => GFlags (l :*: r) where
  ggenerate configs gen =
    let (lval, gen') = ggenerate configs gen
        (rval, gen'') = ggenerate configs gen'
     in (lval :*: rval, gen'')

  gflags _ = gflags (Proxy :: Proxy l) <> gflags (Proxy :: Proxy r)

instance
  ( KnownSymbol fieldName,
    FromBool (IsBool bool) bool
  ) =>
  GFlags (S1 ('MetaSel ('Just fieldName) su ss ds) (K1 i bool))
  where
  ggenerate configs gen =
    first (M1 . K1 . fromBool (Proxy :: Proxy (IsBool bool))) $
      case Map.lookup (T.pack $ symbolVal (Proxy :: Proxy fieldName)) configs of
        Nothing -> (False, gen)
        Just (Percent 0) -> (False, gen)
        Just (Percent 100) -> (True, gen)
        Just config -> roll gen config

  gflags _ = [T.pack $ symbolVal (Proxy :: Proxy fieldName)]

type family IsBool (b :: Type) :: Bool where
  IsBool Bool = 'True
  IsBool _ = 'False

class FromBool (b :: Bool) a where
  fromBool :: Proxy b -> Bool -> a

instance FromBool 'True Bool where
  fromBool _ = id

instance TypeError InvalidFlagsTypeMessage => FromBool 'False a where
  fromBool = error "unreachable"

instance TypeError InvalidFlagsTypeMessage => GFlags (D1 m (C1 ('MetaCons s f 'False) a)) where
  ggenerate = error "unreachable"

  gflags = error "unreachable"

type InvalidFlagsTypeMessage =
  'Text "Not a valid flags type."
    :$$: 'Text "A flags type needs to be a record with boolean fields."
    :$$: 'Text "For example:"
    :$$: 'Text "  data Flags ="
    :$$: 'Text "     Flags { showErrorPage    :: Bool"
    :$$: 'Text "           , throttleRequests :: Bool }"

roll :: SMGen -> Percent -> (Bool, SMGen)
roll gen (Percent trueChance) =
  let (randomWord32, gen') = nextWord32 gen
      between1And100 = 1 + (randomWord32 `mod` 100)
   in (trueChance >= between1And100, gen')
