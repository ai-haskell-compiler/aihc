{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}

-- | A data type representing the Bower.json package description file, together
-- with a parser and related functions.
--
-- This code is based on the specification at
-- <https://github.com/bower/bower.json-spec>.

module Web.Bower.PackageMeta.Internal where

import Control.Monad
import Control.Category ((>>>))
import Control.Monad.Error.Class (MonadError(..))
import Control.DeepSeq
import GHC.Generics
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A.Key
import qualified Data.Aeson.Types as Aeson
import Data.Aeson.BetterErrors (Parse, ParseError, asText, asString, asBool, eachInArray, eachInObjectWithKey, withText, key, keyMay, keyOrDefault, toAesonParser', toAesonParser, displayError, parse)

---------------------
-- Data types

-- | A data type representing the data stored in a bower.json package manifest
-- file.
--
-- Note that the 'ToJSON' / 'FromJSON' instances don't exactly match; for
-- example, it is not always the case that decoding from JSON and then encoding
-- to JSON will give you the exact same JSON that you started with. However, if
-- you start with a PackageMeta value, encode to JSON, and then decode, you
-- should always get the same value back.
data PackageMeta = PackageMeta
  { bowerName            :: PackageName
  , bowerDescription     :: Maybe Text
  , bowerMain            :: [FilePath]
  , bowerModuleType      :: [ModuleType]
  , bowerLicense         :: [Text]
  , bowerIgnore          :: [Text]
  , bowerKeywords        :: [Text]
  , bowerAuthors         :: [Author]
  , bowerHomepage        :: Maybe Text
  , bowerRepository      :: Maybe Repository
  , bowerDependencies    :: [(PackageName, VersionRange)]
  , bowerDevDependencies :: [(PackageName, VersionRange)]
  , bowerResolutions     :: [(PackageName, Version)]
  , bowerPrivate         :: Bool
  }
  deriving (Show, Eq, Ord, Generic)

instance NFData PackageMeta

-- | A valid package name for a Bower package.
newtype PackageName
  = PackageName Text
  deriving (Show, Eq, Ord, Generic)

instance NFData PackageName

runPackageName :: PackageName -> Text
runPackageName (PackageName s) = s

-- | A smart constructor for a PackageName. It ensures that the package name
-- satisfies the restrictions described at
-- <https://github.com/bower/bower.json-spec#name>.
mkPackageName :: Text -> Either PackageNameError PackageName
mkPackageName = fmap PackageName . validateAll validators
  where
  dashOrDot = ['-', '.']
  validateAll vs x = mapM_ (validateWith x) vs >> return x
  validateWith x (p, err)
    | p x       = Right x
    | otherwise = Left (err x)
  validChar c = isAscii c && (isLower c || isDigit c || c `elem` dashOrDot)
  validators =
      [ (not . T.null, const NotEmpty)
      , (T.all validChar, InvalidChars . T.unpack . T.filter (not . validChar))
      , (firstChar (`notElem` dashOrDot), const MustNotBeginSeparator)
      , (lastChar (`notElem` dashOrDot), const MustNotEndSeparator)
      , (not . T.isInfixOf "--", const RepeatedSeparators)
      , (not . T.isInfixOf "..", const RepeatedSeparators)
      , (T.length >>> (<= 50), TooLong . T.length)
      ]
  firstChar p str = not (T.null str) && p (T.index str 0)
  lastChar p = firstChar p . T.reverse

data Author = Author
  { authorName     :: Text
  , authorEmail    :: Maybe Text
  , authorHomepage :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic)

instance NFData Author

-- | See: <https://github.com/bower/bower.json-spec#moduletype>
data ModuleType
  = Globals
  | AMD
  | Node
  | ES6
  | YUI
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance NFData ModuleType

moduleTypes :: [(Text, ModuleType)]
moduleTypes = map (\t -> (T.toLower (T.pack (show t)), t)) [minBound .. maxBound]

data Repository = Repository
  { repositoryUrl :: Text
  , repositoryType :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance NFData Repository

newtype Version
  = Version { runVersion :: Text }
  deriving (Show, Eq, Ord, Generic)

instance NFData Version

newtype VersionRange
  = VersionRange { runVersionRange :: Text }
  deriving (Show, Eq, Ord, Generic)

instance NFData VersionRange

data BowerError
  = InvalidPackageName PackageNameError
  | InvalidModuleType Text
  deriving (Show, Eq, Ord, Generic)

instance NFData BowerError

showBowerError :: BowerError -> Text
showBowerError (InvalidPackageName err) =
  "Invalid package name: " <> showPackageNameError err
showBowerError (InvalidModuleType str) =
  "Invalid module type: " <> str <>
    ". Must be one of: " <> renderList moduleTypes
  where
  renderList =
    map (T.pack . show . fst)
      >>> T.intercalate ", "

data PackageNameError
  = NotEmpty
  | TooLong Int
  | InvalidChars [Char]
  | RepeatedSeparators
  | MustNotBeginSeparator
  | MustNotEndSeparator
  deriving (Show, Eq, Ord, Generic)

instance NFData PackageNameError

showPackageNameError :: PackageNameError -> Text
showPackageNameError err = case err of
  NotEmpty ->
    "A package name may not be empty"
  TooLong x ->
    "Package names must be no more than 50 characters, yours was " <>
      T.pack (show x)
  InvalidChars chars ->
    "The following characters are not permitted in package names: " <>
      T.intercalate " " (map T.singleton chars)
  RepeatedSeparators ->
    "The substrings \"--\" and \"..\" may not appear in "<>
      "package names"
  MustNotBeginSeparator ->
    "Package names may not begin with a dash or a dot"
  MustNotEndSeparator ->
    "Package names may not end with a dash or a dot"

displayError :: ParseError BowerError -> Text
displayError = T.unlines . Data.Aeson.BetterErrors.displayError showBowerError

-------------------------
-- Parsing

-- | Read and attempt to decode a bower.json file.
decodeFile :: FilePath -> IO (Either (ParseError BowerError) PackageMeta)
decodeFile = fmap (parse asPackageMeta) . B.readFile

-- | A parser for bower.json files, using the aeson-better-errors package.
asPackageMeta :: Parse BowerError PackageMeta
asPackageMeta =
  PackageMeta <$> key "name" (withText parsePackageName)
            <*> keyMay "description" asText
            <*> keyOrDefault "main"       [] (arrayOrSingle asString)
            <*> keyOrDefault "moduleType" [] (arrayOrSingle (withText parseModuleType))
            <*> keyOrDefault "license"    [] (arrayOrSingle asText)
            <*> keyOrDefault "ignore"     [] (eachInArray asText)
            <*> keyOrDefault "keywords"   [] (eachInArray asText)
            <*> keyOrDefault "authors"    [] (eachInArray asAuthor)
            <*> keyMay "homepage" asText
            <*> keyMay "repository" asRepository
            <*> keyOrDefault "dependencies"    [] (asAssocListOf VersionRange)
            <*> keyOrDefault "devDependencies" [] (asAssocListOf VersionRange)
            <*> keyOrDefault "resolutions"     [] (asAssocListOf Version)
            <*> keyOrDefault "private" False asBool
  where
  arrayOrSingle :: Parse e a -> Parse e [a]
  arrayOrSingle parser =
    (fmap (:[]) parser) <|> eachInArray parser
    where
    (<|>) p q = catchError p (const q)

  asAssocListOf :: (Text -> a) -> Parse BowerError [(PackageName, a)]
  asAssocListOf g =
    eachInObjectWithKey parsePackageName (g <$> asText)

parseModuleType :: Text -> Either BowerError ModuleType
parseModuleType str =
  case lookup str moduleTypes of
    Nothing -> Left (InvalidModuleType str)
    Just mt -> Right mt

parsePackageName :: Text -> Either BowerError PackageName
parsePackageName str =
  case mkPackageName str of
    Left err -> Left (InvalidPackageName err)
    Right n -> Right n

asAuthor :: Parse e Author
asAuthor = catchError asAuthorString (const asAuthorObject)

asAuthorString :: Parse e Author
asAuthorString = withText $ \s ->
  let (email, s1)    = takeDelim "<" ">" (T.words s)
      (homepage, s2) = takeDelim "(" ")" s1
  in pure (Author (T.unwords s2) email homepage)

-- | Given a prefix and a suffix, go through the supplied list, attempting
-- to extract one string from the list which has the given prefix and suffix,
-- All other strings in the list are returned as the second component of the
-- tuple.
takeDelim :: Text -> Text -> [Text] -> (Maybe Text, [Text])
takeDelim start end = foldr go (Nothing, [])
  where
  go str (Just x, strs) =
    (Just x, str : strs)
  go str (Nothing, strs) =
    case stripWrapper start end str of
      Just str' -> (Just str', strs)
      Nothing   -> (Nothing, str : strs)

-- | Like stripPrefix, but strips a suffix as well.
stripWrapper :: Text -> Text -> Text -> Maybe Text
stripWrapper start end =
  T.stripPrefix start
    >>> fmap T.reverse
    >=> T.stripPrefix (T.reverse end)
    >>> fmap T.reverse

asAuthorObject :: Parse e Author
asAuthorObject =
  Author <$> key "name" asText
         <*> keyMay "email" asText
         <*> keyMay "homepage" asText

asRepository :: Parse e Repository
asRepository =
  Repository <$> key "url" asText
             <*> key "type" asText

------------------------
-- Serializing

instance A.ToJSON PackageMeta where
  toJSON PackageMeta{..} =
    A.object $ concat
      [ [ "name" .= bowerName ]
      , maybePair "description" bowerDescription
      , maybeArrayPair "main" bowerMain
      , maybeArrayPair "moduleType" bowerModuleType
      , maybeArrayPair "license" bowerLicense
      , maybeArrayPair "ignore" bowerIgnore
      , maybeArrayPair "keywords" bowerKeywords
      , maybeArrayPair "authors" bowerAuthors
      , maybePair "homepage" bowerHomepage
      , maybePair "repository" bowerRepository
      , assoc "dependencies" bowerDependencies
      , assoc "devDependencies" bowerDevDependencies
      , assoc "resolutions" bowerResolutions
      , if bowerPrivate then [ "private" .= True ] else []
      ]

      where
      assoc :: A.ToJSON a => A.Key -> [(PackageName, a)] -> [Aeson.Pair]
      assoc = maybeArrayAssocPair (A.Key.fromText . runPackageName)

instance A.ToJSON PackageName where
  toJSON = A.toJSON . runPackageName

instance A.ToJSON ModuleType where
  toJSON = A.toJSON . map toLower . show

instance A.ToJSON Repository where
  toJSON Repository{..} =
    A.object [ "url" .= repositoryUrl
             , "type" .= repositoryType
             ]

instance A.ToJSON Author where
  toJSON Author{..} =
    A.object $
      [ "name" .= authorName ] ++
        maybePair "email" authorEmail ++
        maybePair "homepage" authorHomepage

instance A.ToJSON Version where
  toJSON = A.toJSON . runVersion

instance A.ToJSON VersionRange where
  toJSON = A.toJSON . runVersionRange

maybePair :: A.ToJSON a => A.Key -> Maybe a -> [Aeson.Pair]
maybePair k = maybe [] (\val -> [k .= val])

maybeArrayPair :: A.ToJSON a => A.Key -> [a] -> [Aeson.Pair]
maybeArrayPair _   [] = []
maybeArrayPair k xs = [k .= xs]

maybeArrayAssocPair :: A.ToJSON b => (a -> A.Key) -> A.Key -> [(a,b)] -> [Aeson.Pair]
maybeArrayAssocPair _ _   [] = []
maybeArrayAssocPair f k xs = [k .= A.object (map (\(k', v) -> f k' .= v) xs)]

-------------------------
-- FromJSON instances

instance A.FromJSON PackageMeta where
  parseJSON = toAesonParser showBowerError asPackageMeta

instance A.FromJSON PackageName where
  parseJSON = toAesonParser showBowerError (withText parsePackageName)

instance A.FromJSON ModuleType where
  parseJSON = toAesonParser showBowerError (withText parseModuleType)

instance A.FromJSON Repository where
  parseJSON = toAesonParser' asRepository

instance A.FromJSON Author where
  parseJSON = toAesonParser' asAuthor

instance A.FromJSON Version where
  parseJSON = toAesonParser' (Version <$> asText)

instance A.FromJSON VersionRange where
  parseJSON = toAesonParser' (VersionRange <$> asText)
