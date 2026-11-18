{-# LANGUAGE OverloadedStrings #-}

-- | Read Haskell inputs and evaluate each of them in one WebAssembly
-- instance.
--
-- A session has one engine of "Aihc.Wasm.Engine". The image of the engine
-- has the guest program and the packages that an input can import. For
-- each input, the session does these steps:
--
-- 1. Write one module with the code of the input. The module imports the
--    modules of the earlier inputs, so it can use their names.
-- 2. Compile the module with the position-independent code model, as a
--    module of the session package.
-- 3. Link the object of the module as a shared module and load it into the
--    instance.
-- 4. Run the closure of the module, if the input is a statement or an
--    expression.
--
-- The instance stays alive between the inputs. Thus, a value that an input
-- binds stays in the memory of the instance, and a later input can use it.
--
-- An input is one of these:
--
-- * Declarations, such as @f x = x + 1@ or @data T = A | B@. The form
--   @let x = 1@ is also a declaration.
-- * A bind statement, such as @x <- readFile "notes.txt"@.
-- * An expression. The session shows its value. If the expression is an
--   action, the session runs it and shows its result. The name @it@ gets
--   the value or the result.
-- * An import declaration.
-- * A command that starts with a colon. See 'helpText'.
--
-- A new definition of a name hides the earlier definition. The session
-- puts the name in the @hiding@ list of each import that can supply the
-- earlier definition. An import with an import list has no @hiding@ list,
-- so a name from such an import and a later definition of the same name
-- give an ambiguous name.
--
-- The session finds the kind of an expression by compilation. It tries
-- these forms in order and uses the first form that compiles: a value with
-- a @Show@ instance, an action with the result @()@, an action with a
-- result that has a @Show@ instance, and an action with a different
-- result.
--
-- The result of an action is in a mutable cell of the module, because the
-- action must run one time only. The name of the result reads the cell.
-- The type of the name comes from the type of the action. A result with a
-- type variable, such as the result of @newIORef []@, gets a polymorphic
-- type. This is not safe for a mutable value, so give such an action a
-- type annotation.
module Aihc.Cli.Repl
  ( ReplStreams (..),
    runReplWith,
    replSession,
  )
where

import Aihc.Cli.Build (buildWithConfig)
import Aihc.Cli.Install (InstallResult (..), ModuleCompileConfig, installWithConfig)
import Aihc.Cli.Options (BuildOptions (..), InstallOptions (..), PlanOptions (..), ReplOptions (..), defaultPlanOptions)
import Aihc.Cli.Progress (ProgressReporter, quietProgress, withProgress)
import Aihc.Native (NativeTarget (..), OptimizationLevel (..))
import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Syntax (Decl (..), Extension, LanguageEdition (..), Module (..), effectiveExtensions, parseExtensionSettingName)
import Aihc.Resolve (ExportEntry (..), ModuleUnit (..), Package (..), PackageId (..), exportsEntries, moduleDeclarationExports)
import Aihc.Wasm.Engine (Engine, EngineStream (..), engineGuestPath, guestIsRunning, linkSharedModule, loadSharedModule, startGuest, tryRunClosure, withOutputEngine)
import Control.Exception (IOException, bracket, try)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing, getFileSize, getTemporaryDirectory, makeAbsolute, removePathForcibly)
import System.FilePath (takeDirectory, (<.>), (</>))
import System.IO (hFlush, hIsTerminalDevice, isEOF, stderr, stdin, stdout)
import System.IO.Error (ioeGetErrorString, isUserError)
import System.Posix.Temp (mkdtemp)

-- | The streams of a session.
data ReplStreams = ReplStreams
  { -- | The next line of the input, or 'Nothing' at the end of the input.
    -- The argument is the prompt for the line.
    replReadLine :: Text -> IO (Maybe Text),
    -- | Write bytes to the standard output.
    replWriteOutput :: ByteString -> IO (),
    -- | Write bytes to the standard error.
    replWriteError :: ByteString -> IO ()
  }

-- | Run a session on the standard streams of the process. A prompt shows
-- only when the standard input is a terminal.
runReplWith :: (ModuleCompileConfig -> ModuleCompileConfig) -> ReplOptions -> IO ()
runReplWith adjust options = do
  interactive <- hIsTerminalDevice stdin
  let readLine prompt = do
        when interactive (TIO.putStr prompt >> hFlush stdout)
        end <- isEOF
        if end then pure Nothing else Just <$> TIO.getLine
      streams =
        ReplStreams
          { replReadLine = readLine,
            replWriteOutput = \bytes -> BS.hPut stdout bytes >> hFlush stdout,
            replWriteError = \bytes -> BS.hPut stderr bytes >> hFlush stderr
          }
  withProgress stderr (\reporter -> replSession reporter adjust options streams)

-- | The state of a session.
data Session = Session
  { sessionEngine :: !Engine,
    sessionStreams :: !ReplStreams,
    sessionAdjust :: !(ModuleCompileConfig -> ModuleCompileConfig),
    sessionOptions :: !ReplOptions,
    -- | The directory with the files of the session.
    sessionRoot :: !FilePath,
    -- | The number of the next module.
    sessionNext :: !(IORef Int),
    -- | The modules that a new module imports, with the newest module
    -- first.
    sessionModules :: !(IORef [SessionModule]),
    -- | The import declarations of the user, in the order of the inputs.
    sessionImports :: !(IORef [Text]),
    -- | The settings of language extensions, in the order of the inputs. An
    -- example is @NoImplicitPrelude@.
    sessionExtensions :: !(IORef [Text])
  }

-- | A module of an earlier input.
data SessionModule = SessionModule
  { sessionModuleNumber :: !Int,
    -- | The names that the module defines.
    sessionModuleNames :: ![Text],
    -- | If the module has declarations of the user. Such a module can have
    -- instances, so a new module imports it also when a later input hides
    -- each of its names.
    sessionModuleDeclarations :: !Bool
  }

-- | Run a session. The reporter shows the build of the image.
replSession :: ProgressReporter -> (ModuleCompileConfig -> ModuleCompileConfig) -> ReplOptions -> ReplStreams -> IO ()
replSession reporter adjust options streams =
  case replBuildRoot options of
    Just root -> run root
    Nothing -> bracket (getTemporaryDirectory >>= \directory -> mkdtemp (directory </> "aihc-repl-")) removePathForcibly run
  where
    run given = do
      root <- makeAbsolute given
      createDirectoryIfMissing True (root </> "session" </> "src")
      image <- buildImage reporter adjust options root
      readable <- makeAbsolute "."
      let write stream =
            case stream of
              EngineStdout -> replWriteOutput streams
              EngineStderr -> replWriteError streams
      withOutputEngine write (Just readable) image $ \engine -> do
        startGuest engine
        session <- Session engine streams adjust options root <$> newIORef 1 <*> newIORef [] <*> newIORef [] <*> newIORef []
        loop session

-- | Read inputs until the end of the input or the command @:quit@.
loop :: Session -> IO ()
loop session = do
  next <- readInput (sessionStreams session)
  case next of
    Nothing -> pure ()
    Just text -> do
      continue <- runInput session (classifyInput text)
      when continue (loop session)

-- | Read one input. The lines between @:{@ and @:}@ are one input.
readInput :: ReplStreams -> IO (Maybe Text)
readInput streams = do
  line <- replReadLine streams "aihc> "
  case line of
    Just text | T.strip text == ":{" -> Just . T.intercalate "\n" <$> block []
    _ -> pure line
  where
    block previous = do
      line <- replReadLine streams "aihc| "
      case line of
        Just text | T.strip text /= ":}" -> block (text : previous)
        _ -> pure (reverse previous)

-- | The kinds of input.
data Input
  = InputEmpty
  | -- | A command and its argument.
    InputCommand !Text !Text
  | InputImport !Text
  | InputDeclarations !Text
  | -- | The pattern and the action of a bind statement.
    InputBind !Text !Text
  | InputExpression !Text

-- | The kind of an input. An input that the parser accepts as the
-- declarations of a module is declarations. The parser gives an expression
-- at the top level of a module as a splice, so such an input is not
-- declarations.
classifyInput :: Text -> Input
classifyInput given
  | T.null text = InputEmpty
  | Just command <- T.stripPrefix ":" text =
      let (name, argument) = T.break isSpace command
       in InputCommand name (T.strip argument)
  | firstWord == "import" = InputImport text
  | firstWord == "let" = InputDeclarations (withoutLet text)
  | isDeclarations text = InputDeclarations text
  | Just (pattern', action) <- splitBind text = InputBind pattern' action
  | otherwise = InputExpression text
  where
    text = T.dropWhileEnd isSpace (dropBlankLines given)
    firstWord = T.takeWhile (not . isSpace) text
    dropBlankLines = T.intercalate "\n" . dropWhile (T.null . T.strip) . T.lines

-- | The declarations of a @let@ input. The declarations start in the
-- first column, so the function removes the indentation that the word
-- @let@ gave to the later lines. Declarations in braces lose the braces,
-- because a module can have semicolons between its declarations.
withoutLet :: Text -> Text
withoutLet text =
  case T.lines (T.drop 3 text) of
    [] -> ""
    _
      | Just inner <- T.stripPrefix "{" (T.strip (T.drop 3 text)) >>= T.stripSuffix "}" -> T.strip inner
    first : rest ->
      let indentation = T.length first - T.length (T.stripStart first) + 3
          dedent line = fromMaybe (T.stripStart line) (T.stripPrefix (T.replicate indentation " ") line)
       in T.intercalate "\n" (T.stripStart first : map dedent rest)

isDeclarations :: Text -> Bool
isDeclarations text =
  case parseModule probeConfig ("module AihcReplProbe where\n" <> text) of
    ([], modu) -> not (null (moduleDecls modu)) && not (any isSplice (moduleDecls modu))
    _ -> False
  where
    isSplice decl =
      case decl of
        DeclAnn _ inner -> isSplice inner
        DeclSplice _ -> True
        _ -> False

probeConfig :: ParserConfig
probeConfig = defaultConfig {parserSourceName = "<interactive>", parserExtensions = probeExtensions}

-- | The extensions of the probes. A probe only finds the kind of an input
-- and the names that it defines, so the settings of the session do not
-- change it.
probeExtensions :: [Extension]
probeExtensions = effectiveExtensions GHC2021Edition []

-- | The pattern and the action of a bind statement. The arrow of the
-- statement is the first arrow that is not in brackets, in a string, in a
-- character, or in a line comment.
splitBind :: Text -> Maybe (Text, Text)
splitBind text = go (0 :: Int) (0 :: Int) ' '
  where
    size = T.length text
    at index = if index < size then T.index text index else ' '
    isSymbol = (`elem` ("!#$%&*+./<=>?@\\^|-~:" :: String))
    go depth index previous
      | index >= size = Nothing
      | otherwise =
          let current = at index
           in case current of
                '"' -> go depth (skipString (index + 1)) current
                '\''
                  | isAlphaNum previous || previous == '_' || previous == '\'' -> go depth (index + 1) current
                  | at (index + 1) == '\\' -> go depth (skipCharacter (index + 2)) current
                  | at (index + 2) == '\'' -> go depth (index + 3) current
                _
                  | current `elem` ("([{" :: String) -> go (depth + 1) (index + 1) current
                  | current `elem` (")]}" :: String) -> go (depth - 1) (index + 1) current
                  | current == '-' && at (index + 1) == '-' && not (isSymbol previous) && not (isSymbol (at (index + 2))) ->
                      go depth (skipLine index) ' '
                  | depth == 0 && current == '<' && at (index + 1) == '-' && not (isSymbol previous) && not (isSymbol (at (index + 2))) ->
                      Just (T.strip (T.take index text), T.strip (T.drop (index + 2) text))
                  | otherwise -> go depth (index + 1) current
    skipString index
      | index >= size = size
      | at index == '\\' = skipString (index + 2)
      | at index == '"' = index + 1
      | otherwise = skipString (index + 1)
    skipCharacter index
      | index >= size = size
      | at index == '\'' = index + 1
      | otherwise = skipCharacter (index + 1)
    skipLine index
      | index >= size || at index == '\n' = index
      | otherwise = skipLine (index + 1)

-- | The names that declarations define: the terms, the types, and the
-- members of the types and the classes.
definedNames :: Text -> [Text]
definedNames declarations =
  nub (concatMap names (exportsEntries (moduleDeclarationExports mempty (ModuleUnit package probeExtensions modu))))
  where
    (_, modu) = parseModule probeConfig ("module AihcReplProbe where\n" <> declarations)
    package = Package sessionPackage (PackageId sessionPackageId)
    names entry =
      case entry of
        ExportTerm name _ -> [name]
        ExportType name _ -> [name]
        ExportConstructors _ members -> members
        ExportRecordFields _ members -> members
        ExportMethods _ members -> members
        ExportAssociatedTypes _ members -> members
        ExportFixity _ _ -> []

-- | Do one input. The result is 'False' when the session must stop.
runInput :: Session -> Input -> IO Bool
runInput session input =
  case input of
    InputEmpty -> pure True
    InputCommand name argument -> runCommand session name argument
    InputImport declaration -> do
      checked <- checkSettings session (\imports extensions -> (imports <> [declaration], extensions))
      when checked (modifyIORef' (sessionImports session) (<> [declaration]))
      pure True
    InputDeclarations declarations -> do
      outcome <- compileForm session (declarationsForm declarations)
      case outcome of
        Left message -> writeError session message >> pure True
        Right (number, shared) -> do
          mapM_ (loadSharedModule (sessionEngine session)) shared
          keepModule session number (definedNames declarations) True
          pure True
    InputBind pattern' action -> do
      let names = definedNames ("(" <> pattern' <> ") = ()")
      outcome <- compileForm session (cellForm names (Just pattern') AfterNothing action)
      case outcome of
        Left message -> writeError session message >> pure True
        Right compiled -> runModule session names compiled
    InputExpression expression -> do
      outcome <- firstForm session [valueForm expression, unitActionForm expression, cellForm ["it"] Nothing AfterShow expression, cellForm ["it"] Nothing AfterNothing expression]
      case outcome of
        Left message -> writeError session message >> pure True
        Right (names, compiled) -> runModule session names compiled

-- | Compile the forms in order and give the first form that compiles, with
-- the names that it defines. If no form compiles, the result is the
-- message of the first form.
firstForm :: Session -> [Form] -> IO (Either String ([Text], (Int, Maybe FilePath)))
firstForm session forms = go forms Nothing
  where
    go remaining failure =
      case remaining of
        [] -> pure (Left (fromMaybe "The input has no form." failure))
        form : rest -> do
          outcome <- compileForm session form
          case outcome of
            Right compiled -> pure (Right (formNames form, compiled))
            Left message -> go rest (Just (fromMaybe message failure))

-- | Load a compiled module and run its closure. A module that defines
-- names stays in the session. An exception in the closure is a message,
-- and the session continues. A trap stops the guest, and the instance
-- cannot run more code. Thus, a trap is an error of the command.
runModule :: Session -> [Text] -> (Int, Maybe FilePath) -> IO Bool
runModule session names (number, shared) = do
  let engine = sessionEngine session
  mapM_ (loadSharedModule engine) shared
  outcome <- tryRunClosure engine noRequest sessionPackageId (moduleName number) (runName number)
  unless (null names) (keepModule session number names False)
  case outcome of
    Right _ -> pure True
    Left failure -> do
      running <- guestIsRunning engine
      if running
        then writeError session ("*** Exception: " <> failure) >> pure True
        else ioError (userError ("The code of the input had a fatal error, and the session cannot continue: " <> failure))
  where
    noRequest _ = ioError (userError "the code sent a request, and a session has no requests")

keepModule :: Session -> Int -> [Text] -> Bool -> IO ()
keepModule session number names declarations =
  modifyIORef' (sessionModules session) (SessionModule number names declarations :)

writeError :: Session -> String -> IO ()
writeError session message =
  replWriteError (sessionStreams session) (TE.encodeUtf8 (T.pack message <> "\n"))

writeOutput :: Session -> Text -> IO ()
writeOutput session message =
  replWriteOutput (sessionStreams session) (TE.encodeUtf8 (message <> "\n"))

-- | Do a command. The result is 'False' when the session must stop.
runCommand :: Session -> Text -> Text -> IO Bool
runCommand session name argument
  | name `elem` ["quit", "q"] = pure False
  | name `elem` ["help", "?"] = writeOutput session helpText >> pure True
  | name == "set",
    Just extension <- T.stripPrefix "-X" argument,
    Just _ <- parseExtensionSettingName extension = do
      checked <- checkSettings session (\imports extensions -> (imports, extensions <> [extension]))
      when checked (modifyIORef' (sessionExtensions session) (<> [extension]))
      pure True
  | otherwise = do
      writeError session ("The command :" <> T.unpack name <> " is not known or has an argument that is not correct. Use :help for the commands.")
      pure True

-- | The text of the command @:help@.
helpText :: Text
helpText =
  T.intercalate
    "\n"
    [ "An input is declarations, a bind statement, an expression, an import, or a command.",
      "  :{                Start an input of more than one line. The line :} is its end.",
      "  :set -XNAME       Set a language extension for the subsequent inputs.",
      "  :help, :?         Show this text.",
      "  :quit, :q         Stop the session."
    ]

-- | Compile a module that has only the settings that the function gives
-- from the imports and the extensions of the session. The settings are
-- correct if the module compiles.
checkSettings :: Session -> ([Text] -> [Text] -> ([Text], [Text])) -> IO Bool
checkSettings session change = do
  imports <- readIORef (sessionImports session)
  extensions <- readIORef (sessionExtensions session)
  let (newImports, newExtensions) = change imports extensions
  writeIORef (sessionImports session) newImports
  writeIORef (sessionExtensions session) newExtensions
  outcome <- compileForm session (Form [] (\_ _ -> ""))
  writeIORef (sessionImports session) imports
  writeIORef (sessionExtensions session) extensions
  case outcome of
    Left message -> writeError session message >> pure False
    Right _ -> pure True

-- | The code of a module for one input.
data Form = Form
  { -- | The names that the code defines.
    formNames :: ![Text],
    -- | The code, from the number of the module and the older definitions.
    formCode :: !(Int -> Older -> Text)
  }

-- | The number of the newest earlier module that defines a name.
type Older = Text -> Maybe Int

declarationsForm :: Text -> Form
declarationsForm declarations = Form (definedNames declarations) (\_ _ -> declarations)

-- | An expression with a value that has a @Show@ instance. The name @it@
-- gets the value.
valueForm :: Text -> Form
valueForm expression =
  Form ["it"] $ \number older ->
    T.unlines
      [ "it =",
        indented (withOlder older ["it"] expression),
        runDefinition number "AihcReplP.print it"
      ]

-- | An action with the result @()@.
unitActionForm :: Text -> Form
unitActionForm expression =
  Form [] $ \number _ ->
    runDefinition number ("(AihcReplP.>>=)\n" <> bracketed expression <> "\n    (\\() -> AihcReplP.pure ())")

-- | What a module does with the result of its action, after it puts the
-- result in the cell.
data After
  = AfterNothing
  | AfterShow

-- | An action with a result that the given names get. With a pattern, the
-- names are the variables of the pattern. Without a pattern, there is one
-- name, and it gets the result.
--
-- The action runs in the closure of the module, and the closure puts the
-- result in a cell. The value of the module reads the cell. The cell has
-- no type, so the value gets its type from a second copy of the action,
-- which no code evaluates.
cellForm :: [Text] -> Maybe Text -> After -> Text -> Form
cellForm names pattern' after action =
  Form names $ \number older ->
    let name base = base <> T.pack (show number)
        scoped = withOlder older names action
        cell = name "aihcReplCell"
        result = name "aihcReplResult"
        proxy = name "aihcReplType"
        value = name "aihcReplValue"
        bound = name "aihcReplBound"
        selector variable =
          case pattern' of
            Just given -> bindingName variable <> " = case " <> value <> " of { " <> given <> " -> " <> bindingName variable <> " }"
            Nothing -> bindingName variable <> " = " <> value
        check =
          case pattern' of
            Just given -> "(case " <> bound <> " of { " <> given <> " -> AihcReplP.pure () })"
            Nothing -> "(AihcReplP.pure ())"
        finish =
          case after of
            AfterNothing -> check
            AfterShow -> "(AihcReplP.print " <> bound <> ")"
     in T.unlines
          ( [ "{-# NOINLINE " <> cell <> " #-}",
              cell <> " :: AihcReplR.IORef ()",
              cell <> " = AihcReplU.unsafePerformIO (AihcReplR.newIORef ())",
              result <> " :: AihcReplP.IO a -> a",
              result <> " _ = AihcReplP.undefined",
              proxy <> " = " <> result,
              bracketed scoped,
              value <> " = AihcReplP.asTypeOf (AihcReplC.unsafeCoerce (AihcReplU.unsafePerformIO (AihcReplR.readIORef " <> cell <> "))) " <> proxy
            ]
              <> map selector names
              <> [ runDefinition
                     number
                     ( "(AihcReplP.>>=)\n"
                         <> bracketed scoped
                         <> "\n    (\\"
                         <> bound
                         <> " -> AihcReplR.writeIORef "
                         <> cell
                         <> " (AihcReplC.unsafeCoerce (AihcReplP.asTypeOf "
                         <> bound
                         <> " "
                         <> value
                         <> ")) AihcReplP.>> "
                         <> finish
                         <> ")"
                     )
                 ]
          )

-- | Code that defines names and can refer to the older definitions of the
-- same names. In a bind statement such as @x <- pure (x + 1)@, the second
-- @x@ is the older definition. The code becomes the body of a function
-- with these names as parameters, and the function gets the older
-- definitions.
withOlder :: Older -> [Text] -> Text -> Text
withOlder older names code =
  case [(name, number) | name <- names, Just number <- [older name]] of
    [] -> code
    found ->
      T.intercalate
        "\n"
        [ "(\\" <> T.unwords (map (bindingName . fst) found) <> " ->",
          indented code,
          "  ) " <> T.unwords [qualifierName number <> "." <> name | (name, number) <- found]
        ]

-- | The definition of the closure of a module. The closure does the
-- action and then writes the buffers of the standard streams.
runDefinition :: Int -> Text -> Text
runDefinition number action =
  T.intercalate
    "\n"
    [ runName number <> " :: AihcReplP.IO ()",
      runName number <> " = AihcReplE.finally",
      bracketed action,
      "    (AihcReplIO.hFlush AihcReplIO.stdout AihcReplP.>> AihcReplIO.hFlush AihcReplIO.stderr)"
    ]

-- | Code in brackets, as an argument on lines of its own. Each line of the
-- code gets the same indentation, so the layout of the code does not
-- change.
bracketed :: Text -> Text
bracketed code = "    (\n" <> indented code <> "\n    )"

indented :: Text -> Text
indented = T.intercalate "\n" . map ("      " <>) . T.lines

-- | A name as an import or a binder has it: an operator is in brackets.
bindingName :: Text -> Text
bindingName name =
  case T.uncons name of
    Just (first, _) | isAlpha first || first == '_' -> name
    _ -> "(" <> name <> ")"

sessionPackage :: Text
sessionPackage = "aihc-repl"

sessionPackageId :: Text
sessionPackageId = "aihc-repl-0"

-- | The qualifier of the names of a module of the session.
qualifierName :: Int -> Text
qualifierName number = "AihcReplM" <> T.pack (show number)

moduleName :: Int -> Text
moduleName number = "AihcRepl" <> T.pack (show number)

runName :: Int -> Text
runName number = "aihcReplRun" <> T.pack (show number)

-- | The text of the module of a form. The module hides each name that the
-- form or an earlier module defines from the imports that can supply an
-- older definition of the name.
moduleText :: [Text] -> [Text] -> [SessionModule] -> Int -> Form -> Text
moduleText extensions imports modules number form =
  T.unlines
    ( ["{-# LANGUAGE " <> extension <> " #-}" | extension <- extensions]
        <> [ "module " <> moduleName number <> " where",
             "import Prelude" <> hiding allNames,
             "import qualified Prelude as AihcReplP",
             "import qualified Control.Exception as AihcReplE",
             "import qualified Data.IORef as AihcReplR",
             "import qualified System.IO as AihcReplIO",
             "import qualified System.IO.Unsafe as AihcReplU",
             "import qualified Unsafe.Coerce as AihcReplC"
           ]
        <> map userImport imports
        <> earlierImports (formNames form) modules
        <> [formCode form number older]
    )
  where
    older name =
      case [sessionModuleNumber modu | modu <- modules, name `elem` sessionModuleNames modu] of
        found : _ -> Just found
        [] -> Nothing
    -- The modules with the older definitions of the names of the form.
    -- The code of the form refers to them with a qualifier.
    qualified = nub [found | name <- formNames form, Just found <- [older name]]
    allNames = nub (formNames form <> concatMap sessionModuleNames modules)
    hiding names
      | null names = ""
      | otherwise = " hiding (" <> T.intercalate ", " (map bindingName names) <> ")"
    -- An import with an import list, or a qualified import, gets no
    -- hiding list.
    userImport declaration
      | any (`T.isInfixOf` declaration) ["(", "qualified", "hiding"] = declaration
      | otherwise = declaration <> hiding allNames
    -- The modules have the newest module first. A module hides the names
    -- that the form and the newer modules define.
    earlierImports _ [] = []
    earlierImports newer (modu : rest) =
      let visible = filter (`notElem` newer) (sessionModuleNames modu)
          name = moduleName (sessionModuleNumber modu)
          plain = "import " <> name <> hiding (filter (`elem` sessionModuleNames modu) newer)
       in [plain | sessionModuleDeclarations modu || not (null visible)]
            <> ["import qualified " <> name <> " as " <> qualifierName (sessionModuleNumber modu) | sessionModuleNumber modu `elem` qualified]
            <> earlierImports (nub (newer <> sessionModuleNames modu)) rest

-- | Compile the module of a form and link it as a shared module. The
-- result is the number of the module and the path of the shared module,
-- or the message of the compiler. A module without code, such as a module
-- with only a type synonym, has no shared module.
compileForm :: Session -> Form -> IO (Either String (Int, Maybe FilePath))
compileForm session form = do
  number <- readIORef (sessionNext session)
  writeIORef (sessionNext session) (number + 1)
  extensions <- readIORef (sessionExtensions session)
  imports <- readIORef (sessionImports session)
  modules <- readIORef (sessionModules session)
  let root = sessionRoot session
      directory = root </> "session"
      source = directory </> "src" </> T.unpack (moduleName number) <.> "hs"
      names = map (moduleName . sessionModuleNumber) modules <> [moduleName number]
  writeFile (directory </> T.unpack sessionPackage <.> "cabal") (packageDescription (sessionOptions session) names)
  TIO.writeFile source (moduleText extensions imports modules number form)
  outcome <- try (installWithConfig (quietProgress stderr) (packageOptions (sessionOptions session) root) (sessionAdjust session))
  case outcome of
    Left failure -> do
      removePathForcibly source
      pure (Left (describeFailure source failure))
    Right installed -> do
      let object = installStorePath installed </> T.unpack (moduleName number) </> T.unpack (moduleName number) <.> "o"
          shared = root </> "modules" </> T.unpack (moduleName number) <.> "wasm"
      size <- getFileSize object
      if size == 0
        then pure (Right (number, Nothing))
        else do
          createDirectoryIfMissing True (takeDirectory shared)
          linkSharedModule shared [object] []
          pure (Right (number, Just shared))

-- | The message of a failure of the compiler. The name of the generated
-- file becomes @<interactive>@.
describeFailure :: FilePath -> IOException -> String
describeFailure source failure =
  T.unpack (T.strip (T.replace (T.pack source) "<interactive>" (T.pack message)))
  where
    message
      | isUserError failure = ioeGetErrorString failure
      | otherwise = show failure

-- | The Cabal file of the session package.
packageDescription :: ReplOptions -> [Text] -> String
packageDescription options modules =
  unlines
    ( [ "cabal-version: 3.0",
        "name: " <> T.unpack sessionPackage,
        "version: 0",
        "build-type: Simple",
        "",
        "library",
        "  hs-source-dirs: src",
        "  default-language: GHC2021",
        "  build-depends:",
        "    base,"
      ]
        <> ["    " <> constraint <> "," | constraint <- replPackageConstraints options]
        <> ["  exposed-modules:"]
        <> ["    " <> T.unpack name | name <- modules]
    )

-- | Build the image of the session and give its path. The image has the
-- guest program and the packages of the options.
buildImage :: ProgressReporter -> (ModuleCompileConfig -> ModuleCompileConfig) -> ReplOptions -> FilePath -> IO FilePath
buildImage reporter adjust options root = do
  guest <- engineGuestPath
  images <-
    buildWithConfig
      reporter
      BuildOptions
        { buildInput = guest,
          buildSourceDirectories = [takeDirectory guest],
          buildPackageConstraints = replPackageConstraints options,
          buildTarget = Wasm32Wasip3,
          buildStoreRoot = replStoreRoot options,
          buildBuildRoot = Just (root </> "image"),
          buildWorkspace = Nothing,
          buildKeepCore = False,
          buildKeepGrin = False,
          buildKeepLir = False,
          buildKeepNative = False,
          buildLint = False,
          buildCheckPrimBounds = False,
          buildProfileAllocations = False,
          buildProfileTime = False,
          buildLto = False,
          buildOptimization = O0,
          buildNoLink = False,
          buildOnlyDependencies = False,
          buildNoPie = False,
          buildWasmImage = True,
          buildVerbose = False,
          buildPrintTimings = False,
          buildOutput = Just (root </> "image.wasm"),
          buildExecutables = [],
          buildPlanOptions = sessionPlanOptions
        }
      adjust
  case images of
    [image] -> pure image
    _ -> ioError (userError "the image build gave no image")

-- | The options that install the session package for shared modules.
packageOptions :: ReplOptions -> FilePath -> InstallOptions
packageOptions options root =
  InstallOptions
    { installPackageTarget = root </> "session",
      installStoreRoot = replStoreRoot options,
      installBuildRoot = Just (root </> "session-build"),
      installImmutable = False,
      installKeepCore = False,
      installKeepGrin = False,
      installKeepNative = False,
      installLint = False,
      installCheckPrimBounds = False,
      installLto = False,
      installOptimization = O0,
      installReinstall = False,
      installNoCode = False,
      installPositionIndependent = True,
      installVerbose = False,
      installPrintTimings = False,
      installTarget = Wasm32Wasip3,
      installWorkspace = Nothing,
      installPlanOptions = sessionPlanOptions
    }

-- | The plan options of the builds of a session. The builds use no lock,
-- because a lock would be in the working directory of the user.
sessionPlanOptions :: PlanOptions
sessionPlanOptions = defaultPlanOptions {planUseLock = False}
