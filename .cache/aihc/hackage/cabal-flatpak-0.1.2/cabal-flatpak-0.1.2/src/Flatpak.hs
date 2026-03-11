module Flatpak (
   SourceType(..), archive, Source(..),
   Module(..), ModuleItem(ModuleEmbed),
   Arch(..), archString, T(..),
   encodeJson, encodeYaml,
   PackageHook(..), Cabal(..),
   ) where

import qualified Shell.Utility.ParseArgument as ParseArg

import qualified Cabal.Plan as Plan

import qualified Data.Yaml.Pretty as YamlPretty
import qualified Data.Aeson.Encode.Pretty as JsonPretty
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson as Json
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as Text
import qualified Data.Monoid.HT as Mn
import Data.Aeson.Types (Parser, Pair)
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON, withObject)
import Data.Map (Map)
import Data.Text (Text)
import Data.Tuple.HT (mapFst)
import Data.Ord.HT (comparing)
import Data.Monoid (mempty, (<>))

import Text.Printf (printf)

import Control.Monad (mzero)
import Control.Applicative (pure, (<*>), (<$>), (<|>))


infixr 8 .:, .:?, .:*

(.:) :: FromJSON a => Json.Object -> String -> Parser a
o .: field  =  o Json..: Key.fromString field

(.:?) :: FromJSON a => Json.Object -> (String, a) -> Parser a
o .:? (field, deflt)  =  o Json..:? Key.fromString field Json..!= deflt

(.:*) :: FromJSON a => Json.Object -> String -> Parser [a]
o .:* field  =  o .:? (field, [])


infixr 8 .=, .=?, .=*

(.=) :: ToJSON a => String -> (object -> a) -> object -> [Pair]
(field .= a) obj  =  [Key.fromString field Json..= a obj]

(.=?) :: (ToJSON a, Eq a) => String -> (object -> a, a) -> object -> [Pair]
(field .=? (a, deflt)) obj  =  Mn.when (a obj /= deflt) ((field .= a) obj)

(.=*) :: (ToJSON a) => String -> (object -> [a]) -> object -> [Pair]
(field .=* a) obj  =  Mn.when (not $ null $ a obj) ((field .= a) obj)

object :: (object -> [Pair]) -> object -> Json.Value
object fields = Json.object . fields


parseEnum ::
   (Enum a, Bounded a) => (a -> String) -> String -> Json.Value -> Parser a
parseEnum fmt nm =
   Json.withText nm $ \txt ->
      case Text.unpack txt of
         str ->
            case ParseArg.enumMaybe fmt str of
               Just x -> return x
               Nothing -> fail $ printf "unknown %s type: %s" nm str


data SourceType =
      Archive {stripComponents :: Int} |
      Git | Bzr | File | Dir |
      Script {commands :: [String]} | Shell | Patch | ExtraData
   deriving (Show)

archive :: SourceType
archive = Archive {stripComponents = 1}

pairsFromSourceType :: SourceType -> [Pair]
pairsFromSourceType t =
   let simple str = [("type", Json.toJSON str)]
   in map (mapFst Key.fromString) $
      case t of
         Archive {stripComponents = strip} ->
            simple "archive" ++
            Mn.when (strip/=1) [("strip-components", Json.toJSON strip)]
         Script {commands = cmds} ->
            simple "script" ++
            [("commands", Json.toJSON cmds)]
         Git       -> simple "git"
         Bzr       -> simple "bzr"
         File      -> simple "file"
         Dir       -> simple "dir"
         Shell     -> simple "shell"
         Patch     -> simple "patch"
         ExtraData -> simple "extra-data"

parseSourceType :: Json.Object -> String -> Parser SourceType
parseSourceType obj t =
   case t of
      "archive"    -> fmap Archive $ obj .:? ("strip-components", 1)
      "git"        -> return Git
      "bzr"        -> return Bzr
      "file"       -> return File
      "dir"        -> return Dir
      "script"     -> fmap Script $ obj .: "commands"
      "shell"      -> return Shell
      "patch"      -> return Patch
      "extra-data" -> return ExtraData
      _ -> mzero

data Source =
   Source {
      typ :: SourceType,
      url :: Maybe String,
      destFilename :: Maybe String,
      sha256 :: Maybe Plan.Sha256
   } deriving (Show)

instance FromJSON Source where
   parseJSON =
      withObject "Source" $ \o -> pure Source
         <*> (parseSourceType o =<< o .: "type")
         <*> o .:? ("url", Nothing)
         <*> o .:? ("dest-filename", Nothing)
         <*> o .:? ("sha256", Nothing)

instance ToJSON Source where
   toJSON = object $
      (pairsFromSourceType . typ) <>
      ("url" .=? (url, Nothing)) <>
      ("dest-filename" .=? (destFilename, Nothing)) <>
      ("sha256" .=? (sha256, Nothing)) <>
      mempty


data Arch = I386 | X86_64 | ARM | AArch64
   deriving (Show, Enum, Bounded)

archString :: Arch -> String
archString arch =
   case arch of
      I386 -> "i386"
      X86_64 -> "x86_64"
      ARM -> "arm"
      AArch64 -> "aarch64"

instance FromJSON Arch where
   parseJSON = parseEnum archString "arch"

instance ToJSON Arch where
   toJSON = Json.toJSON . archString


data Module =
   Module {
      name :: String,
      onlyArches :: [Arch],
      buildsystem :: String,
      builddir :: Bool,
      configOpts :: [String],
      buildCommands :: [String],
      postInstall :: [String],
      cleanupModule :: [String],
      sources :: [Source]
   } deriving (Show)

instance FromJSON Module where
   parseJSON =
      withObject "Module" $ \o -> pure Module
         <*> o .:  "name"
         <*> o .:* "only-arches"
         <*> o .:  "buildsystem"
         <*> o .:  "builddir"
         <*> o .:* "config-opts"
         <*> o .:* "build-commands"
         <*> o .:* "post-install"
         <*> o .:* "cleanup"
         <*> o .:* "sources"

instance ToJSON Module where
   toJSON = object $
      ("name" .= name) <>
      ("only-arches" .=* onlyArches) <>
      ("buildsystem" .= buildsystem) <>
      ("builddir" .= builddir) <>
      ("config-opts" .=* configOpts) <>
      ("build-commands" .=* buildCommands) <>
      ("post-install" .=* postInstall) <>
      ("cleanup" .=* cleanupModule) <>
      ("sources" .=* sources) <>
      mempty


data ModuleItem = ModuleEmbed Module | ModuleInclude String
   deriving (Show)

instance FromJSON ModuleItem where
   parseJSON v =
      ModuleInclude <$> parseJSON v
      <|>
      ModuleEmbed <$> parseJSON v

instance ToJSON ModuleItem where
   toJSON m =
      case m of
         ModuleEmbed modu -> toJSON modu
         ModuleInclude includePath -> toJSON includePath


data T =
   Cons {
      appId :: String,
      runtime :: String,
      runtimeVersion :: String,
      sdk :: String,
      command :: String,
      finishArgs :: [String],
      cleanup :: [String],
      cleanupCommands :: [String],
      modules :: [ModuleItem]
   } deriving (Show)

instance FromJSON T where
   parseJSON =
      withObject "Flatpak" $ \o -> pure Cons
         <*> o .:  "app-id"
         <*> o .:  "runtime"
         <*> o .:  "runtime-version"
         <*> o .:  "sdk"
         <*> o .:  "command"
         <*> o .:  "finish-args"
         <*> o .:* "cleanup"
         <*> o .:* "cleanup-commands"
         <*> o .:  "modules"

instance ToJSON T where
   toJSON = object $
      ("app-id" .= appId) <>
      ("runtime" .= runtime) <>
      ("runtime-version" .= runtimeVersion) <>
      ("sdk" .= sdk) <>
      ("command" .= command) <>
      ("finish-args" .= finishArgs) <>
      ("cleanup" .=* cleanup) <>
      ("cleanup-commands" .=* cleanupCommands) <>
      ("modules" .= modules) <>
      mempty


{- |
A global ordering of all fields (across all substructures)
appearing in the manifest JSON.
This is an ugly hack, but is needed for get pretty JSON and also field ordering.
See <https://github.com/informatikr/aeson-pretty/issues/28>.
-}
fieldOrder :: [Text]
fieldOrder =
   map Text.pack $
      "app-id" :
      "name" :
      "runtime" :
      "runtime-version" :
      "sdk" :
      "only-arches" :
      "command" :
      "finish-args" :
      "cleanup" :
      "cleanup-commands" :
      "main-package" :
      "main-sources" :
      "package-hooks" :
      "modules" :
      "buildsystem" :
      "builddir" :
      "config-opts" :
      "build-commands" :
      "post-install" :
      "sources" :
      []

encodeJson :: T -> BL.ByteString
encodeJson =
   JsonPretty.encodePretty'
      JsonPretty.defConfig {
         JsonPretty.confCompare = JsonPretty.keyOrder fieldOrder
      }


fieldOrderMap :: Map Text Int
fieldOrderMap =
   Map.fromListWith (error "duplicate key") $ zip fieldOrder [0..]

encodeYaml :: T -> B.ByteString
encodeYaml =
   YamlPretty.encodePretty $
      YamlPretty.setConfCompare
         (comparing $ \k -> maybe (Right k) Left $ Map.lookup k fieldOrderMap) $
      YamlPretty.defConfig



data PackageHook =
   PackageHook {
      nameHook :: String,
      postInstallHook :: [String]
   } deriving (Show)

instance FromJSON PackageHook where
   parseJSON =
      withObject "PackageHook" $ \o -> pure PackageHook
         <*> o .:  "name"
         <*> o .:* "post-install"

instance ToJSON PackageHook where
   toJSON = object $
      ("name" .= nameHook) <>
      ("post-install" .=* postInstallHook) <>
      mempty


data Cabal =
   Cabal {
      base :: T,
      mainPackage :: String,
      mainSources :: [Source],
      packageHooks :: [PackageHook],
      cabalConfigureOptions, ghcOptions :: [String]
   } deriving (Show)

instance FromJSON Cabal where
   parseJSON =
      withObject "Cabal" $ \o -> pure Cabal
         <*> o .:  "base"
         <*> o .:  "main-package"
         <*> o .:* "main-sources"
         <*> o .:* "package-hooks"
         <*> o .:* "cabal-configure-options"
         <*> o .:* "ghc-options"

instance ToJSON Cabal where
   toJSON = object $
      ("base" .= base) <>
      ("main-package" .= mainPackage) <>
      ("main-sources" .=* mainSources) <>
      ("package-hooks" .=* packageHooks) <>
      ("cabal-configure-options" .=* cabalConfigureOptions) <>
      ("ghc-options" .=* ghcOptions) <>
      mempty
