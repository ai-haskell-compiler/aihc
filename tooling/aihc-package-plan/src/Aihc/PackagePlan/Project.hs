-- |
-- Module      : Aihc.PackagePlan.Project
-- Description : The cabal.project file
--
-- A project file names the local packages of a project and the constraints
-- of its plan, as @cabal@ reads it. The file is found in the working
-- directory or in a parent directory, and the files @FILE.freeze@ and
-- @FILE.local@ add to it when they exist.
--
-- These fields have an effect:
--
-- * @packages@ and @optional-packages@: directories, @.cabal@ files, or
--   globs of them, relative to the directory of the project file. A glob
--   can use @*@ in a path segment and @{a,b}@ alternatives. An entry of
--   @packages@ must match a package. An entry of @optional-packages@ can
--   match none.
--
-- * @constraints@: version and flag constraints. The @any.@ qualifier is
--   the same as no qualifier. A constraint on a setup dependency, and a
--   constraint such as @installed@ or @test@, do not change the plan of a
--   library or an executable, so they have no effect.
--
-- * @flags@ in a @package NAME@ section: the flag constraints of NAME.
--
-- The project file does not support conditionals, @import@ and
-- @source-repository-package@. Such a file is an error, because a plan
-- without them can be a different plan. The other fields, such as @tests@
-- and @jobs@, do not change the plan, and the parser ignores them.
module Aihc.PackagePlan.Project
  ( -- * The project file
    ProjectConfig (..),
    parseProjectConfig,
    projectFileName,

    -- * Projects
    Project (..),
    ProjectPackage (..),
    ProjectSelection (..),
    selectProject,
    findProjectFile,
    loadProject,
    projectPackageAt,
    withProject,

    -- * Targets
    ProjectTarget (..),
    parseProjectTarget,
    resolveProjectTarget,
  )
where

import Aihc.Cabal (Component (..), ComponentKind (..), Package (..))
import Aihc.Hackage.Package (PackageName, mkFlagName, mkPackageName, packageNameOf, unPackageName)
import Aihc.PackagePlan (PlanRequest (..), parseConstraint, parseSourcePackageDescriptionAt)
import Aihc.PackagePlan.Solver (Constraint (..))
import Control.Monad (forM, unless, when)
import Data.Char (isAlphaNum, isSpace, toLower)
import Data.List (isPrefixOf, isSuffixOf, nub, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import System.Directory
  ( canonicalizePath,
    doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    getHomeDirectory,
    listDirectory,
  )
import System.FilePath (isAbsolute, joinPath, splitDirectories, takeDirectory, (</>))

-- | The name of the project file that @cabal@ looks for.
projectFileName :: FilePath
projectFileName = "cabal.project"

-- | The fields of a project file that change a plan.
data ProjectConfig = ProjectConfig
  { projectConfigPackages :: ![String],
    projectConfigOptionalPackages :: ![String],
    -- | The constraints of the @constraints@ field and of the @flags@ of
    -- each @package@ section, in file order.
    projectConfigConstraints :: ![Constraint]
  }
  deriving (Eq, Show)

instance Semigroup ProjectConfig where
  ProjectConfig a b c <> ProjectConfig a' b' c' = ProjectConfig (a <> a') (b <> b') (c <> c')

instance Monoid ProjectConfig where
  mempty = ProjectConfig [] [] []

-- | One line of a project file without its indentation. Comment lines and
-- blank lines are not lines.
data Line = Line !Int !String

-- | A field with its value, or a section with its header and body.
data Item
  = Field !String !String
  | Section ![String] ![Item]

-- | Parse the text of a project file.
parseProjectConfig :: String -> Either String ProjectConfig
parseProjectConfig text = mconcat <$> mapM topItem (items (projectLines text))
  where
    topItem item =
      case item of
        Field name value ->
          case name of
            "packages" -> Right mempty {projectConfigPackages = entries value}
            "optional-packages" -> Right mempty {projectConfigOptionalPackages = entries value}
            "constraints" -> (\cs -> mempty {projectConfigConstraints = cs}) . concat <$> mapM projectConstraint (constraintItems value)
            "import" -> Left "aihc does not support import in a project file"
            _ -> Right mempty
        Section header body ->
          case map (map toLower) header of
            ["package", "*"] -> Right mempty
            ["package", _] -> packageSection (header !! 1) body
            "source-repository-package" : _ -> Left "aihc does not support source-repository-package in a project file"
            keyword : _
              | keyword `elem` ["if", "elif", "else"] -> Left "aihc does not support conditionals in a project file"
            _ -> Right mempty

    packageSection name body = do
      let package = mkPackageName name
      flags <- forM [value | Field "flags" value <- body] (mapM flagWord . words)
      pure mempty {projectConfigConstraints = [ConstraintFlag package flag on | (flag, on) <- concat flags]}

    flagWord word =
      case word of
        '+' : flag@(_ : _) -> Right (mkFlagName flag, True)
        '-' : flag@(_ : _) -> Right (mkFlagName flag, False)
        flag@(c : _) | isAlphaNum c -> Right (mkFlagName flag, True)
        _ -> Left ("Invalid flag in a project file: " <> word)

    -- The entries of a list of paths, separated by white space or commas.
    entries = words . map (\c -> if c == ',' then ' ' else c)

-- | The constraints of one item of the @constraints@ field.
projectConstraint :: String -> Either String [Constraint]
projectConstraint item =
  case words item of
    [] -> Right []
    qualified : rest ->
      case qualifier qualified of
        Nothing -> Right []
        Just name
          | rest `elem` [["installed"], ["source"], ["test"], ["bench"], ["benchmarks"], ["tests"]] -> Right []
          | otherwise -> parseConstraint (unwords (name : rest))
  where
    -- The package of a qualified name, or 'Nothing' for a setup qualifier.
    qualifier name
      | Just rest <- T.stripPrefix "any." (T.pack name) = Just (T.unpack rest)
      | "setup." `isPrefixOf` name = Nothing
      | ':' `elem` name = Nothing
      | otherwise = Just name

-- | The items of a @constraints@ value: commas separate them, but not a
-- comma inside the braces of a version set such as @== { 1.0, 1.1 }@.
constraintItems :: String -> [String]
constraintItems = filter (not . all isSpace) . go (0 :: Int) ""
  where
    go _ current [] = [reverse current]
    go depth current (c : rest) =
      case c of
        ',' | depth == 0 -> reverse current : go depth "" rest
        '{' -> go (depth + 1) (c : current) rest
        '}' -> go (max 0 (depth - 1)) (c : current) rest
        _ -> go depth (c : current) rest

projectLines :: String -> [Line]
projectLines = mapMaybe line . lines
  where
    line raw =
      let text = filter (/= '\r') raw
          (indent, rest) = span (`elem` [' ', '\t']) text
       in if all isSpace rest || "--" `isPrefixOf` rest
            then Nothing
            else Just (Line (length indent) (dropWhileEnd isSpace rest))
    dropWhileEnd p = reverse . dropWhile p . reverse

-- | Group lines by indentation. A line with a field name and a colon starts
-- a field, and the lines below it with more indentation continue its
-- value. Any other line is the header of a section, and the lines below it
-- with more indentation are its body.
items :: [Line] -> [Item]
items [] = []
items (Line indent text : rest) =
  let (body, after) = span (\(Line i _) -> i > indent) rest
   in case fieldName text of
        Just (name, value) -> Field name (unlines (value : [t | Line _ t <- body])) : items after
        Nothing -> Section (words text) (items body) : items after
  where
    fieldName line =
      let (name, after) = span (\c -> isAlphaNum c || c `elem` ['-', '_']) line
       in case dropWhile (== ' ') after of
            ':' : value | not (null name) -> Just (map toLower name, value)
            _ -> Nothing

-- | A project: its file, its local packages, and the constraints its plan
-- takes.
data Project = Project
  { projectFile :: !FilePath,
    projectRoot :: !FilePath,
    projectPackages :: !(Map PackageName ProjectPackage),
    projectConstraints :: ![Constraint]
  }
  deriving (Eq, Show)

-- | A local package of a project.
data ProjectPackage = ProjectPackage
  { -- | The canonical path of the package directory.
    projectPackageDirectory :: !FilePath,
    -- | The names of the executables, buildable or not, in file order.
    projectPackageExecutables :: ![String]
  }
  deriving (Eq, Show)

-- | How a command finds its project.
data ProjectSelection
  = -- | Look for @cabal.project@ in the working directory and its parents.
    ProjectSearch
  | -- | Use this project file.
    ProjectFileAt !FilePath
  | -- | Use no project.
    ProjectIgnored
  deriving (Eq, Show)

-- | Load the project that the selection names, if there is one.
selectProject :: ProjectSelection -> IO (Maybe Project)
selectProject selection =
  case selection of
    ProjectIgnored -> pure Nothing
    ProjectFileAt path -> do
      exists <- doesFileExist path
      unless exists (ioError (userError ("The project file " <> path <> " does not exist")))
      Just <$> loadProject path
    ProjectSearch -> do
      found <- findProjectFile =<< getCurrentDirectory
      traverse loadProject found

-- | The first @cabal.project@ in the directory or in one of its parents.
findProjectFile :: FilePath -> IO (Maybe FilePath)
findProjectFile start = canonicalizePath start >>= go
  where
    go directory = do
      let candidate = directory </> projectFileName
      exists <- doesFileExist candidate
      let parent = takeDirectory directory
      if exists
        then pure (Just candidate)
        else if parent == directory then pure Nothing else go parent

-- | Read a project file, its @.freeze@ file and its @.local@ file, and find
-- its packages.
loadProject :: FilePath -> IO Project
loadProject path = do
  file <- canonicalizePath path
  let root = takeDirectory file
  configs <- forM [file, file <> ".freeze", file <> ".local"] $ \candidate -> do
    exists <- doesFileExist candidate
    if not exists
      then pure mempty
      else do
        text <- readFile candidate
        either (\problem -> ioError (userError (candidate <> ": " <> problem))) pure (parseProjectConfig text)
  let config = mconcat configs
  when (null (projectConfigPackages config) && null (projectConfigOptionalPackages config)) $
    ioError (userError ("The project file " <> file <> " names no packages"))
  required <- forM (projectConfigPackages config) $ \entry -> do
    found <- packageDirectories root entry
    when (null found) $
      ioError (userError ("The entry " <> entry <> " of " <> file <> " matches no package"))
    pure found
  optional <- mapM (packageDirectories root) (projectConfigOptionalPackages config)
  packages <- forM (nub (concat required <> concat optional)) $ \directory -> do
    (_, description) <- parseSourcePackageDescriptionAt directory
    let executables = [T.unpack name | Component (Executable name) _ <- packageComponents description]
    pure (packageNameOf description, ProjectPackage directory executables)
  let duplicates = [name | (name, count) <- Map.toList (Map.fromListWith (+) [(name, 1 :: Int) | (name, _) <- packages]), count > 1]
  unless (null duplicates) $
    ioError (userError ("The project " <> file <> " has more than one package " <> unwords (map unPackageName duplicates)))
  pure
    Project
      { projectFile = file,
        projectRoot = root,
        projectPackages = Map.fromList packages,
        projectConstraints = projectConfigConstraints config
      }

-- | The canonical package directories that one entry of @packages@ names.
packageDirectories :: FilePath -> String -> IO [FilePath]
packageDirectories root entry = do
  when ("://" `T.isInfixOf` T.pack entry || ".tar.gz" `isSuffixOf` entry) $
    ioError (userError ("aihc does not support the project package " <> entry <> ": use a local directory"))
  home <- getHomeDirectory
  let expanded = case entry of
        '~' : '/' : rest -> home </> rest
        _ -> entry
      absolute = if isAbsolute expanded then expanded else root </> expanded
  matches <- concat <$> mapM globPaths (expandBraces absolute)
  directories <- forM matches $ \match ->
    if ".cabal" `isSuffixOf` match
      then do
        isFile <- doesFileExist match
        pure [takeDirectory match | isFile]
      else do
        isDirectory <- doesDirectoryExist match
        hasCabal <- if isDirectory then any (".cabal" `isSuffixOf`) <$> listDirectory match else pure False
        pure [match | hasCabal]
  nub <$> mapM canonicalizePath (concat directories)

-- | Expand the @{a,b}@ alternatives of a glob.
expandBraces :: String -> [String]
expandBraces text =
  case break (== '{') text of
    (before, '{' : rest)
      | (inside, '}' : after) <- break (== '}') rest ->
          [before <> choice <> tail' | choice <- splitOn ',' inside, tail' <- expandBraces after]
    _ -> [text]
  where
    splitOn c s = case break (== c) s of
      (a, _ : b) -> a : splitOn c b
      (a, []) -> [a]

-- | The existing paths that a glob with @*@ in its segments matches.
globPaths :: FilePath -> IO [FilePath]
globPaths glob =
  case splitDirectories glob of
    [] -> pure []
    first : segments -> go first segments
  where
    go path [] = do
      exists <- (||) <$> doesFileExist path <*> doesDirectoryExist path
      pure [path | exists]
    go path (segment : rest)
      | segment == "." = go path rest
      | '*' `notElem` segment = go (joinPath [path, segment]) rest
      | otherwise = do
          isDirectory <- doesDirectoryExist path
          names <- if isDirectory then sort <$> listDirectory path else pure []
          let matching = [name | name <- names, not ("." `isPrefixOf` name) || "." `isPrefixOf` segment, matchSegment segment name]
          concat <$> mapM (\name -> go (path </> name) rest) matching

-- | Match one path segment against a glob in which @*@ matches any text.
matchSegment :: String -> String -> Bool
matchSegment glob name =
  case glob of
    [] -> null name
    '*' : rest -> any (matchSegment rest) (suffixes name)
    c : rest -> case name of
      n : names -> c == n && matchSegment rest names
      [] -> False
  where
    suffixes s =
      s : case s of
        [] -> []
        _ : xs -> suffixes xs

-- | The project package at a directory, if the directory is one.
projectPackageAt :: Project -> FilePath -> IO (Maybe PackageName)
projectPackageAt project directory = do
  canonical <- canonicalizePath directory
  pure (lookupDirectory canonical)
  where
    lookupDirectory canonical =
      case [name | (name, package) <- Map.toList (projectPackages project), projectPackageDirectory package == canonical] of
        name : _ -> Just name
        [] -> Nothing

-- | Give a plan request the packages and the constraints of a project. The
-- constraints of the project come before those of the command line.
withProject :: Project -> PlanRequest -> PlanRequest
withProject project request =
  request
    { requestProjectPackages = Map.map projectPackageDirectory (projectPackages project),
      requestConstraints = projectConstraints project <> requestConstraints request
    }

-- | A target as @cabal build@ names it.
data ProjectTarget
  = -- | @PKG@: a package of the project, or with no such package, the one
    -- executable of that name.
    TargetName !String
  | -- | @exe:NAME@, @PKG:exe:NAME@ or @PKG:NAME@.
    TargetExecutable !(Maybe String) !String
  deriving (Eq, Show)

-- | Parse a target. A text without a colon is a name. Other forms that
-- @cabal@ accepts, such as @lib:NAME@ and @test:NAME@, name components
-- that aihc does not build.
parseProjectTarget :: String -> Maybe ProjectTarget
parseProjectTarget text =
  case splitColons text of
    [name] | valid name -> Just (TargetName name)
    ["exe", name] | valid name -> Just (TargetExecutable Nothing name)
    [package, "exe", name] | valid package, valid name -> Just (TargetExecutable (Just package) name)
    [package, name] | valid package, valid name, package `notElem` kinds -> Just (TargetExecutable (Just package) name)
    _ -> Nothing
  where
    kinds = ["lib", "flib", "exe", "test", "bench"]
    valid name = not (null name) && all (\c -> isAlphaNum c || c `elem` ['-', '_', '.']) name
    splitColons s = case break (== ':') s of
      (a, _ : b) -> a : splitColons b
      (a, []) -> [a]

-- | The package of a target and the executables that it selects. An empty
-- list selects every executable of the package. A name that is neither a
-- package nor an executable of the project is 'Nothing', and an executable
-- target that the project does not have is an error.
resolveProjectTarget :: Project -> ProjectTarget -> Either String (Maybe (PackageName, [String]))
resolveProjectTarget project target =
  case target of
    TargetName name
      | Map.member (mkPackageName name) packages -> Right (Just (mkPackageName name, []))
      | otherwise ->
          case owners name of
            [] -> Right Nothing
            [package] -> Right (Just (package, [name]))
            several -> Left (ambiguous name several)
    TargetExecutable (Just packageText) name -> do
      let package = mkPackageName packageText
      case Map.lookup package packages of
        Nothing -> Left ("The project " <> projectFile project <> " has no package " <> packageText)
        Just info
          | name `elem` projectPackageExecutables info -> Right (Just (package, [name]))
          | otherwise -> Left ("The package " <> packageText <> " has no executable " <> name)
    TargetExecutable Nothing name ->
      case owners name of
        [] -> Left ("The project " <> projectFile project <> " has no executable " <> name)
        [package] -> Right (Just (package, [name]))
        several -> Left (ambiguous name several)
  where
    packages = projectPackages project
    owners name = [package | (package, info) <- Map.toList packages, name `elem` projectPackageExecutables info]
    ambiguous name several =
      "More than one package of the project has the executable " <> name <> ": " <> unwords (map unPackageName several) <> ". Use PKG:exe:" <> name
