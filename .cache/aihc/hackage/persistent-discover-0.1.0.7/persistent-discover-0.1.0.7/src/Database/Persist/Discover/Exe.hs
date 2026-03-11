-- |  This module contains types, definitions, and the logic behind
-- finding persistent definitions.
--
-- In brief, we do an import of all the models defined in the current
-- directory or any subdirectories. These imports are "instances only"
-- imports. Then we splice in @$(discoverEntities)@ to
--
-- The result module is named @All@, and it's placed in the hierarchy where
-- you define this. So if you have a source file:
--
-- @
-- -- src/PersistentModels/All.hs
--
-- {-# OPTIONS_GHC -F -pgmF persistent-discover
-- @
--
-- Then it will translate to:
--
-- @
-- -- src/PersistentModels/All.hs
--
-- module PersistentModels.All where
--
-- import PersistentModels.Foo ()
-- import PersistentModels.Bar ()
-- import PersistentModels.Baz ()
--
-- allEntityDefs :: [EntityDef]
-- allEntityDefs = $(discoverEntities)
-- @
--
-- @since 0.1.0.0
module Database.Persist.Discover.Exe where

import System.FilePath
import Control.Monad (guard, filterM)
import Control.Monad.State
import Data.String
import Data.DList (DList(..))
import qualified Data.DList as DList
import Data.Foldable (for_)
import System.Directory
import Data.List
import Data.Char
import Control.Applicative
import Data.Maybe

newtype Source = Source FilePath

newtype Destination = Destination FilePath

data AllModelsFile = AllModelsFile
    { amfModuleBase :: Module
    , amfModuleImports :: [Module]
    }

render :: Render -> String
render action =
    unlines $ DList.toList $ execState (unRender action) mempty

renderLine :: Render -> Render
renderLine action =
    fromString $ mconcat $ DList.toList $ execState (unRender action) mempty

newtype Render' a = Render { unRender :: State (DList String) a }
    deriving newtype
        (Functor, Applicative, Monad)

type Render = Render' ()

instance (a ~ ()) => IsString (Render' a) where
    fromString str =
        Render (modify (\s -> s <> pure str))

indent :: Int -> Render -> Render
indent i doc =
    Render do
        let
            new =
                fmap (replicate i ' ' <>)
                $ execState (unRender doc) mempty
        modify (<> new)

-- |
--
-- @since 0.1.0.0
discoverModels
    :: Source
    -> Destination
    -> IO ()
discoverModels (Source src) (Destination dest) = do
    let (dir, file) = splitFileName src
    files <- filter (/= file) <$> getFilesRecursive dir
    let
        input =
            AllModelsFile
                { amfModuleBase =
                    fromJust $ pathToModule src
                , amfModuleImports =
                    mapMaybe pathToModule (fmap (dir </>) files)
                }
        output =
            renderFile input

    writeFile dest output

-- | Returns a list of relative paths to all files in the given directory.
getFilesRecursive :: FilePath      -- ^ The directory to search.
                  -> IO [FilePath]
getFilesRecursive baseDir = sort <$> go []
  where
    go :: FilePath -> IO [FilePath]
    go dir = do
      c <- map (dir </>) . filter (`notElem` [".", ".."]) <$> getDirectoryContents (baseDir </> dir)
      dirs <- filterM (doesDirectoryExist . (baseDir </>)) c >>= mapM go
      files <- filterM (doesFileExist . (baseDir </>)) c
      return (files ++ concat dirs)

renderFile
    :: AllModelsFile
    -> String
renderFile amf = render do
    let
        modName =
            moduleName $ amfModuleBase amf
    renderLine do
        "{-# LINE 1 "
        fromString $ show modName
        " #-}"
    "{-# LANGUAGE TemplateHaskell #-}"
    ""
    renderLine do
        "module "
        fromString $ modName
        " where"
    ""
    for_ (amfModuleImports amf) \mod' ->
        renderLine do
            "import "
            fromString $ moduleName mod'
            " ()"
    ""
    "import Database.Persist.TH (discoverEntities)"
    "import Database.Persist.Types (EntityDef)"
    ""
    "-- | All of the entity definitions, as discovered by the @persistent-discover@ utility."
    "allEntityDefs :: [EntityDef]"
    "allEntityDefs = $(discoverEntities)"

-- -- | Derive module name from specified path.
-- pathToModule :: FilePath -> Module
-- pathToModule f =
--     Module
--         { moduleName =
--             intercalate "." $ mapMaybe go $ splitDirectories f
--         , modulePath =
--             f
--         }
--   where
--     go :: String -> Maybe String
--     go (c:cs) =
--         Just (toUpper c : cs)
--     fileName = last $ splitDirectories f
--     m:ms = takeWhile (/='.') fileName

data Module = Module
    { moduleName :: String
    , modulePath :: FilePath
    }
    deriving (Eq, Show)

mkModulePieces
    :: FilePath
    -> [String]
mkModulePieces fp = do
    let
        extension =
            takeExtension fp
    guard (extension == ".hs" || extension == ".lhs")
    reverse
        . takeWhile (not . isLowerFirst)
        . reverse
        . filter noDots
        . splitDirectories
        . dropExtension
        $ fp
  where
    noDots x =
        "." /= x && ".." /= x

isLowerFirst :: String -> Bool
isLowerFirst [] = True
isLowerFirst (c:_) = isLower c

pathToModule
    :: FilePath
    -> Maybe Module
pathToModule file = do
    case mkModulePieces file of
        [] ->
            empty
        x : xs ->  do
            guard $ all isValidModuleName (x : xs)
            pure (Module (intercalate "." (x:xs)) file)

-- | Returns True if the given string is a valid task module name.
-- See `Cabal.Distribution.ModuleName` (http://git.io/bj34)
isValidModuleName :: String -> Bool
isValidModuleName []     = False
isValidModuleName (c:cs) = isUpper c && all isValidModuleChar cs

-- | Returns True if the given Char is a valid taks module character.
isValidModuleChar :: Char -> Bool
isValidModuleChar c = isAlphaNum c || c == '_' || c == '\''

-- | Convert a String in camel case to snake case.
casify :: String -> String
casify str = intercalate "_" $ groupBy (\a b -> isUpper a && isLower b) str

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suffix str =
    reverse <$> stripPrefix (reverse suffix) (reverse str)
