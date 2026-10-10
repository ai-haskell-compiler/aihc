-- | @aihc-dev explore@: a terminal explorer that shows a Haskell program
-- next to the System FC and the GRIN that the compiler makes from it.
--
-- Each optimization level is one real build into a temporary store. The
-- explorer keeps the rendered output of each module, or of the merged
-- program of a whole-program build. When the view changes to another stage
-- or level, the explorer finds the definition that is related to the
-- definition at the cursor, by name.
module Aihc.Dev.Explore
  ( ExploreOptions (..),
    runExplore,
  )
where

import Aihc.Dev.Explore.Build (BuildEvent (..), BuildResult (..), ModuleSource (..), runExplorerBuild)
import Aihc.Dev.Explore.Document
import Aihc.Dev.Explore.Haskell (haskellDocument)
import Aihc.Native (NativeTarget, OptimizationLevel (..), renderOptimizationLevel)
import Brick
import Brick.BChan (BChan, newBChan, writeBChan, writeBChanNonBlocking)
import Brick.Widgets.Border (hBorder)
import Control.Concurrent (MVar, ThreadId, forkFinally, killThread, myThreadId, newEmptyMVar, putMVar, readMVar, throwTo)
import Control.Exception (SomeException, finally, try)
import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (elemIndex, sortOn)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Graphics.Vty qualified as Vty
import System.Directory (doesPathExist, getHomeDirectory, makeAbsolute)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (IOMode (..), hClose, openFile, stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Signals (Handler (..), installHandler, sigHUP, sigTERM)
import System.Timeout (timeout)

data ExploreOptions = ExploreOptions
  { exploreInput :: !FilePath,
    exploreLevel :: !OptimizationLevel,
    exploreTarget :: !NativeTarget
  }

-- | The state of a build of one level.
data BuildStatus
  = BuildRunning !Text
  | BuildFailed !Text
  | BuildReady !BuildResult

data PromptKind
  = PromptModule
  | PromptFunction
  | PromptSearch !SearchDirection
  | -- | The path that the user wrote when the explorer told that the file
    -- exists. Enter replaces the file only if the path is the same.
    PromptSave !(Maybe FilePath)
  deriving (Eq)

-- | The direction of a search through the lines of the view.
data SearchDirection
  = Forward
  | Backward
  deriving (Eq)

-- | The last query of a prompt that searches, for @n@ and @N@.
data LastSearch
  = -- | A query that matches the names of definitions.
    SearchDefinition !Text
  | -- | A query that matches the text of lines, and the direction of the
    -- search. @n@ continues in this direction.
    SearchText !SearchDirection !Text

data Prompt = Prompt
  { promptKind :: !PromptKind,
    promptQuery :: !Text,
    promptChoice :: !Int
  }

data Explorer = Explorer
  { explorerDirectory :: !FilePath,
    explorerOptions :: !ExploreOptions,
    explorerChannel :: !(BChan BuildEvent),
    -- | The threads of the builds, and a variable that each fills when it
    -- ends. Every exit stops them before the temporary store is removed.
    explorerThreads :: !(IORef [(ThreadId, MVar ())]),
    explorerBuilds :: !(Map OptimizationLevel BuildStatus),
    -- | The level of the view. Its build is ready, or the view waits for it.
    explorerLevel :: !OptimizationLevel,
    -- | A level that the user selected, while its build runs.
    explorerPending :: !(Maybe OptimizationLevel),
    explorerModule :: !(Maybe Text),
    explorerStage :: !Stage,
    explorerDocument :: !Document,
    explorerCursor :: !Int,
    explorerTop :: !Int,
    explorerHeight :: !Int,
    explorerStatus :: !Text,
    explorerPrompt :: !(Maybe Prompt),
    -- | The last query of the function prompt or the search prompt, for @n@
    -- and @N@. The view highlights the occurrences of a text query.
    explorerLastQuery :: !(Maybe LastSearch),
    -- | The definition that the last change of view searched for, and the
    -- line it went to, when the match was not exact. While the cursor stays
    -- on that line, the next change of view searches for the same
    -- definition, so that a fallback does not move the search away from it.
    explorerTarget :: !(Maybe (Definition, Int))
  }

runExplore :: ExploreOptions -> IO ()
runExplore options = do
  input <- makeAbsolute (exploreInput options)
  -- A closed terminal or a kill ends the explorer through an exception in
  -- the main thread, so that the temporary store is removed.
  mainThread <- myThreadId
  forM_ [sigHUP, sigTERM] $ \signal ->
    installHandler signal (Catch (throwTo mainThread (ExitFailure 1))) Nothing
  withSystemTempDirectory "aihc-explore" $ \directory -> do
    channel <- newBChan 256
    threads <- newIORef []
    -- The build runs tools, such as the C compiler, that write to stderr.
    -- The log file gets their text, so that it does not damage the screen.
    savedStderr <- hDuplicate stderr
    logHandle <- openFile (directory </> "build.log") WriteMode
    hDuplicateTo logHandle stderr
    let initial =
          Explorer
            { explorerDirectory = directory,
              explorerOptions = options {exploreInput = input},
              explorerChannel = channel,
              explorerThreads = threads,
              explorerBuilds = Map.empty,
              explorerLevel = exploreLevel options,
              explorerPending = Nothing,
              explorerModule = Nothing,
              explorerStage = StageHaskell,
              explorerDocument = emptyDocument,
              explorerCursor = 0,
              explorerTop = 0,
              explorerHeight = 20,
              explorerStatus = "",
              explorerPrompt = Nothing,
              explorerLastQuery = Nothing,
              explorerTarget = Nothing
            }
    flip finally (readIORef threads >>= stopBuilds) $ do
      (_, vty) <- customMainWithDefaultVty (Just channel) explorerApp initial
      Vty.shutdown vty
    hDuplicateTo savedStderr stderr
    hClose logHandle

-- | Stop the builds and wait until their threads end, so that no build
-- writes to the temporary store while it is removed. A build that does not
-- end in ten seconds is left.
stopBuilds :: [(ThreadId, MVar ())] -> IO ()
stopBuilds builds = do
  mapM_ (killThread . fst) builds
  mapM_ (timeout 10000000 . readMVar . snd) builds

explorerApp :: App Explorer BuildEvent ()
explorerApp =
  App
    { appDraw = drawExplorer,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = do
        updateHeight
        level <- gets explorerLevel
        startBuild level,
      appAttrMap = const highlightAttrMap
    }

-- | Start the build of a level, unless it has one.
startBuild :: OptimizationLevel -> EventM () Explorer ()
startBuild level = do
  state <- get
  when (Map.notMember level (explorerBuilds state)) $ do
    let options = explorerOptions state
        channel = explorerChannel state
        directory = explorerDirectory state </> renderOptimizationLevel level
        send event = case event of
          BuildProgress {} -> void (writeBChanNonBlocking channel event)
          BuildFinished {} -> writeBChan channel event
    liftIO $ do
      done <- newEmptyMVar
      thread <- forkFinally (runExplorerBuild (exploreInput options) (exploreTarget options) directory level send) (const (putMVar done ()))
      modifyIORef' (explorerThreads state) ((thread, done) :)
    put state {explorerBuilds = Map.insert level (BuildRunning "Start") (explorerBuilds state)}

updateHeight :: EventM () Explorer ()
updateHeight = do
  vty <- getVtyHandle
  (_, height) <- liftIO (Vty.displayBounds (Vty.outputIface vty))
  -- The header and the status line take two lines each.
  modify (\state -> state {explorerHeight = max 1 (height - 4)})

handleEvent :: BrickEvent () BuildEvent -> EventM () Explorer ()
handleEvent event =
  case event of
    AppEvent (BuildProgress level message) ->
      modify (\state -> state {explorerBuilds = Map.adjust (const (BuildRunning message)) level (explorerBuilds state)})
    AppEvent (BuildFinished level outcome) -> do
      modify (\state -> state {explorerBuilds = Map.insert level (either BuildFailed BuildReady outcome) (explorerBuilds state)})
      state <- get
      if level == explorerLevel state && isNothing (explorerModule state)
        then openInitialModule
        else when (explorerPending state == Just level) $ do
          modify (\current -> current {explorerPending = Nothing})
          case outcome of
            Left _ -> setStatus ("The build of " <> levelText level <> " failed")
            Right _ -> switchView level (explorerStage state)
    VtyEvent (Vty.EvResize _ _) -> updateHeight >> scrollToCursor
    VtyEvent (Vty.EvKey key modifiers) -> do
      prompt <- gets explorerPrompt
      case prompt of
        Just current -> handlePromptKey current key modifiers
        Nothing -> handleKey key modifiers
    _ -> pure ()

handleKey :: Vty.Key -> [Vty.Modifier] -> EventM () Explorer ()
handleKey key modifiers =
  case key of
    Vty.KChar 'q' -> halt
    Vty.KEsc -> halt
    Vty.KChar 'c' | Vty.MCtrl `elem` modifiers -> halt
    Vty.KChar 'j' -> moveCursor 1
    Vty.KDown -> moveCursor 1
    Vty.KChar 'k' -> moveCursor (-1)
    Vty.KUp -> moveCursor (-1)
    Vty.KPageDown -> pageBy 1
    Vty.KChar 'd' | Vty.MCtrl `elem` modifiers -> pageBy 1
    Vty.KChar ' ' -> pageBy 1
    Vty.KPageUp -> pageBy (-1)
    Vty.KChar 'u' | Vty.MCtrl `elem` modifiers -> pageBy (-1)
    Vty.KHome -> moveCursorTo 0
    Vty.KEnd -> do
      count <- gets (V.length . documentLines . explorerDocument)
      moveCursorTo (count - 1)
    Vty.KChar '\t' -> cycleStage 1
    Vty.KBackTab -> cycleStage (-1)
    Vty.KChar character
      | Just stage <- lookup character (zip ['1' ..] [minBound .. maxBound]) -> do
          level <- gets explorerLevel
          switchView level stage
    Vty.KChar 'o' -> cycleLevel 1
    Vty.KChar 'O' -> cycleLevel (-1)
    Vty.KChar 'g' -> modify (\state -> state {explorerPrompt = Just (Prompt PromptModule "" 0)})
    Vty.KChar 'd' -> modify (\state -> state {explorerPrompt = Just (Prompt PromptFunction "" 0)})
    Vty.KChar '/' -> modify (\state -> state {explorerPrompt = Just (Prompt (PromptSearch Forward) "" 0)})
    Vty.KChar '?' -> modify (\state -> state {explorerPrompt = Just (Prompt (PromptSearch Backward) "" 0)})
    Vty.KChar 'w' -> do
      state <- get
      if isNothing (explorerModule state)
        then setStatus "There is no output to save"
        else modify (\current -> current {explorerPrompt = Just (Prompt (PromptSave Nothing) (T.pack (defaultSavePath state)) 0)})
    Vty.KChar 'n' -> repeatSearch Forward
    Vty.KChar 'N' -> repeatSearch Backward
    _ -> pure ()

handlePromptKey :: Prompt -> Vty.Key -> [Vty.Modifier] -> EventM () Explorer ()
handlePromptKey prompt key modifiers =
  case key of
    Vty.KEsc -> closePrompt
    Vty.KEnter -> case promptKind prompt of
      PromptSearch direction -> closePrompt >> searchText direction (promptQuery prompt)
      PromptSave confirmed -> closePrompt >> saveDocument confirmed (T.unpack (promptQuery prompt))
      _ -> do
        choices <- gets (promptChoices prompt)
        closePrompt
        case drop (promptChoice prompt) choices of
          choice : _ -> acceptChoice prompt choice
          [] -> pure ()
    Vty.KChar 'u' | Vty.MCtrl `elem` modifiers -> setPrompt prompt {promptQuery = "", promptChoice = 0}
    Vty.KBS -> setPrompt prompt {promptQuery = T.dropEnd 1 (promptQuery prompt), promptChoice = 0}
    Vty.KDown -> setPrompt prompt {promptChoice = promptChoice prompt + 1}
    Vty.KUp -> setPrompt prompt {promptChoice = max 0 (promptChoice prompt - 1)}
    Vty.KChar '\t' -> setPrompt prompt {promptChoice = promptChoice prompt + 1}
    Vty.KChar character -> setPrompt prompt {promptQuery = T.snoc (promptQuery prompt) character, promptChoice = 0}
    _ -> pure ()
  where
    closePrompt = modify (\state -> state {explorerPrompt = Nothing})
    setPrompt next = modify (\state -> state {explorerPrompt = Just next})

-- | What a prompt can choose, the best match first.
promptChoices :: Prompt -> Explorer -> [Text]
promptChoices prompt state =
  case promptKind prompt of
    PromptModule -> fuzzyFilter (promptQuery prompt) (maybe [] (Map.keys . resultSources) (readyResult state))
    PromptFunction ->
      -- The query matches the name, and the module is only shown.
      map definitionLabel (fuzzyFilterOn definitionName (promptQuery prompt) (uniqueDefinitions (map snd (documentDefinitions (explorerDocument state)))))
    PromptSearch _ -> []
    PromptSave _ -> []
  where
    uniqueDefinitions = Map.elems . Map.fromList . map (\definition -> (definitionLabel definition, definition))

acceptChoice :: Prompt -> Text -> EventM () Explorer ()
acceptChoice prompt choice =
  case promptKind prompt of
    PromptModule -> do
      state <- get
      modify (\current -> current {explorerModule = Just choice})
      showStage (explorerLevel state) (explorerStage state) Nothing
      -- A whole-program view starts at the first definition of the module.
      document <- gets explorerDocument
      forM_ (findRelated document choice (Definition (Just choice) "" DefinitionCode)) (jumpTo . fst)
    PromptFunction -> do
      modify (\state -> state {explorerLastQuery = Just (SearchDefinition (promptQuery prompt))})
      document <- gets explorerDocument
      forM_ (lookup choice [(definitionLabel definition, line) | (line, definition) <- documentDefinitions document]) jumpTo
    PromptSearch _ -> pure ()
    PromptSave _ -> pure ()

-- | Search the text of the view for a query, and go to the first line after
-- or before the cursor that contains it. An empty query stops the
-- highlight.
searchText :: SearchDirection -> Text -> EventM () Explorer ()
searchText direction query
  | T.null query = do
      modify (\state -> state {explorerLastQuery = Nothing})
      setStatus ""
  | otherwise = do
      modify (\state -> state {explorerLastQuery = Just (SearchText direction query)})
      repeatSearch Forward

-- | Go to the next line that matches the last query: a definition for the
-- definition prompt, or the text of a line for the search prompts. 'Forward'
-- continues in the direction of the search, and 'Backward' goes in the
-- opposite direction. A definition search goes down the view.
repeatSearch :: SearchDirection -> EventM () Explorer ()
repeatSearch step = do
  state <- get
  case explorerLastQuery state of
    Nothing -> setStatus "No search"
    Just search -> do
      let cursor = explorerCursor state
          document = explorerDocument state
          (query, matching, searchDirection) = case search of
            SearchDefinition text -> (text, [line | (line, definition) <- documentDefinitions document, isJust (fuzzyScore text (definitionName definition))], Forward)
            SearchText direction text -> (text, matchingLines text document, direction)
          next =
            -- The search goes down when both directions are the same.
            if searchDirection == step
              then listToMaybe (filter (> cursor) matching <> matching)
              else listToMaybe (reverse (filter (< cursor) matching) <> reverse matching)
      case next of
        Nothing -> setStatus ("No match for " <> query)
        Just line -> do
          jumpTo line
          case search of
            SearchDefinition _ -> setStatus ""
            SearchText _ _ ->
              setStatus (query <> ": match " <> T.pack (show (maybe 0 (+ 1) (elemIndex line matching))) <> " of " <> T.pack (show (length matching)))

-- | The file name that the save prompt starts with: the module, the level,
-- and the stage. The level is in the name, so that the saved Haskell stage
-- does not have the name of the source file.
defaultSavePath :: Explorer -> FilePath
defaultSavePath state =
  T.unpack (fromMaybe "output" (explorerModule state) <> ".O" <> T.pack (renderOptimizationLevel (explorerLevel state)) <> "." <> stageFileSuffix (explorerStage state))

-- | Write the text of the view to a file. If the file exists, the explorer
-- asks again, and a second Enter with the same path replaces the file.
saveDocument :: Maybe FilePath -> FilePath -> EventM () Explorer ()
saveDocument confirmed path
  | null path = setStatus "No file name"
  | otherwise = do
      state <- get
      target <- liftIO (expandHome path >>= makeAbsolute)
      exists <- liftIO (doesPathExist target)
      if exists && confirmed /= Just path
        then do
          modify (\current -> current {explorerPrompt = Just (Prompt (PromptSave (Just path)) (T.pack path) 0)})
          setStatus "The file exists. Push Enter again to replace it"
        else do
          let document = explorerDocument state
          written <- liftIO (try (TIO.writeFile target (documentText document)))
          case written of
            Left failure -> setStatus (T.pack (show (failure :: SomeException)))
            Right () -> setStatus ("Saved " <> T.pack (show (V.length (documentLines document))) <> " lines to " <> T.pack target)
  where
    expandHome file = case file of
      '~' : '/' : rest -> (</> rest) <$> getHomeDirectory
      _ -> pure file

definitionLabel :: Definition -> Text
definitionLabel definition =
  maybe (definitionName definition) (\moduleName -> moduleName <> "." <> definitionName definition) (definitionModule definition)

cycleStage :: Int -> EventM () Explorer ()
cycleStage step = do
  state <- get
  let stages = [minBound .. maxBound] :: [Stage]
      index = fromEnum (explorerStage state)
      stage = stages !! ((index + step) `mod` length stages)
  switchView (explorerLevel state) stage

cycleLevel :: Int -> EventM () Explorer ()
cycleLevel step = do
  state <- get
  let levels = [O0, O1, O2, Os]
      current = fromMaybe (explorerLevel state) (explorerPending state)
      index = fromMaybe 0 (elemIndex current levels)
      level = levels !! ((index + step) `mod` length levels)
  case Map.lookup level (explorerBuilds state) of
    Just (BuildReady _) -> do
      modify (\current' -> current' {explorerPending = Nothing})
      switchView level (explorerStage state)
    _ -> do
      startBuild level
      modify (\current' -> current' {explorerPending = if level == explorerLevel state then Nothing else Just level})
      setStatus (if level == explorerLevel state then "" else "Building " <> levelText level <> "; the view changes when the build is complete")

-- | Change the view to a stage of a level, and go to the definition that is
-- related to the definition at the cursor.
switchView :: OptimizationLevel -> Stage -> EventM () Explorer ()
switchView level stage = do
  state <- get
  let atCursor = definitionAt (explorerDocument state) (explorerCursor state)
      current = case explorerTarget state of
        Just (definition, line) | line == explorerCursor state -> Just definition
        _ -> atCursor
      currentModule = fromMaybe "" (explorerModule state)
      -- A definition of a whole-program view can come from another module.
      target = fmap (\definition -> definition {definitionModule = Just (fromMaybe currentModule (definitionModule definition))}) current
  case Map.lookup level (explorerBuilds state) of
    Just (BuildReady result) -> do
      forM_ (target >>= definitionModule) $ \moduleName ->
        when (stage == StageHaskell && Map.member moduleName (resultSources result)) $
          modify (\current' -> current' {explorerModule = Just moduleName})
      modify (\current' -> current' {explorerLevel = level})
      showStage level stage target
    _ -> setStatus ("The build of " <> levelText level <> " is not ready")

-- | Show a stage of the current module and go to the related definition.
showStage :: OptimizationLevel -> Stage -> Maybe Definition -> EventM () Explorer ()
showStage level stage target = do
  state <- get
  let moduleName = fromMaybe "" (explorerModule state)
  loaded <- liftIO (loadDocument state level stage moduleName)
  case loaded of
    Left message -> do
      -- The next change of view searches for the same definition.
      put state {explorerStage = stage, explorerDocument = emptyDocument, explorerCursor = 0, explorerTop = 0, explorerTarget = fmap (,0) target}
      setStatus message
    Right document -> do
      put state {explorerStage = stage, explorerDocument = document, explorerCursor = 0, explorerTop = 0, explorerTarget = Nothing}
      case target of
        Nothing -> setStatus ""
        Just definition -> do
          let name = definitionName definition
              absent = name <> ": not present in " <> stageLabel stage <> " (" <> levelText level <> ")"
              keepTarget line = modify (\current -> current {explorerTarget = Just (definition, line)})
          case findRelated document moduleName definition of
            Just (line, MatchExact) -> jumpTo line >> setStatus ""
            Just (line, MatchRelated) -> do
              jumpTo line
              keepTarget line
              setStatus (absent <> "; a related definition is shown")
            Just (line, MatchModule) -> do
              jumpTo line
              keepTarget line
              setStatus (absent <> "; the first definition of " <> fromMaybe moduleName (definitionModule definition) <> " is shown")
            Nothing -> keepTarget 0 >> setStatus absent

-- | The document of a stage of a module. The Haskell source comes from the
-- file of the module. The other stages come from the output of the module,
-- or from the merged program of a whole-program build.
loadDocument :: Explorer -> OptimizationLevel -> Stage -> Text -> IO (Either Text Document)
loadDocument state level stage moduleName =
  case Map.lookup level (explorerBuilds state) of
    Just (BuildReady result) -> case stage of
      StageHaskell -> case Map.lookup moduleName (resultSources result) of
        Nothing -> pure (Left ("No source for " <> moduleName))
        Just source -> do
          contents <- try (TIO.readFile (sourcePath source))
          pure $ case contents of
            Left failure -> Left (T.pack (show (failure :: SomeException)))
            Right text -> Right (haskellDocument text)
      _ ->
        let units = resultUnits result
            unit = case Map.lookup moduleName units of
              Just output -> Just output
              Nothing -> Map.lookup "program" units
         in pure $ case unit >>= Map.lookup stage of
              Just document -> Right document
              Nothing -> Left (moduleName <> " has no " <> stageLabel stage <> " output at " <> levelText level)
    _ -> pure (Left ("The build of " <> levelText level <> " is not ready"))

-- | Open the main module of the first executable when the first build is
-- complete.
openInitialModule :: EventM () Explorer ()
openInitialModule = do
  state <- get
  case Map.lookup (explorerLevel state) (explorerBuilds state) of
    Just (BuildReady result) -> do
      let sources = Map.toList (resultSources result)
          executableModules = [name | (name, source) <- sources, " (exe)" `T.isSuffixOf` sourcePackage source]
          initial =
            listToMaybe
              ( [name | name <- executableModules, name == "Main"]
                  <> executableModules
                  <> map fst sources
              )
      case initial of
        Nothing -> setStatus "The build has no modules"
        Just name -> do
          modify (\current -> current {explorerModule = Just name})
          showStage (explorerLevel state) StageHaskell Nothing
    Just (BuildFailed message) -> setStatus message
    _ -> pure ()

readyResult :: Explorer -> Maybe BuildResult
readyResult state =
  case Map.lookup (explorerLevel state) (explorerBuilds state) of
    Just (BuildReady result) -> Just result
    _ -> Nothing

setStatus :: Text -> EventM () Explorer ()
setStatus message = modify (\state -> state {explorerStatus = message})

moveCursor :: Int -> EventM () Explorer ()
moveCursor step = do
  cursor <- gets explorerCursor
  moveCursorTo (cursor + step)

pageBy :: Int -> EventM () Explorer ()
pageBy pages = do
  height <- gets explorerHeight
  moveCursor (pages * height)

moveCursorTo :: Int -> EventM () Explorer ()
moveCursorTo line = do
  count <- gets (V.length . documentLines . explorerDocument)
  modify (\state -> state {explorerCursor = max 0 (min (count - 1) line)})
  scrollToCursor

-- | Go to the first line of a definition, and show the definition from the
-- top of the screen.
jumpTo :: Int -> EventM () Explorer ()
jumpTo line = do
  moveCursorTo line
  modify (\state -> state {explorerTop = max 0 (explorerCursor state - scrollContext (explorerHeight state))})

scrollContext :: Int -> Int
scrollContext height = min 3 (height `div` 4)

-- | Keep the cursor on the screen, with some lines of context above it.
scrollToCursor :: EventM () Explorer ()
scrollToCursor =
  modify $ \state ->
    let cursor = explorerCursor state
        height = explorerHeight state
        top = explorerTop state
        context = scrollContext height
        top'
          | cursor < top + context = max 0 (cursor - context)
          | cursor >= top + height - context = cursor - height + context + 1
          | otherwise = top
     in state {explorerTop = max 0 top'}

levelText :: OptimizationLevel -> Text
levelText level = "-O" <> T.pack (renderOptimizationLevel level)

-- | The choices that contain the letters of the query in order, the best
-- match first.
fuzzyFilter :: Text -> [Text] -> [Text]
fuzzyFilter = fuzzyFilterOn id

-- | 'fuzzyFilter' with the text of each choice that the query matches.
fuzzyFilterOn :: (choice -> Text) -> Text -> [choice] -> [choice]
fuzzyFilterOn key query choices =
  map snd (sortOn fst [((score, T.length (key choice)), choice) | choice <- choices, Just score <- [fuzzyScore query (key choice)]])

-- | A lower score is a better match: the same text, then a choice that
-- starts or ends with the query, then a choice that contains it, then a
-- choice that contains its letters with gaps.
fuzzyScore :: Text -> Text -> Maybe Int
fuzzyScore query choice
  | T.null query = Just 0
  | lowerQuery == lowerChoice = Just 0
  | lowerQuery `T.isPrefixOf` lowerChoice || lowerQuery `T.isSuffixOf` lowerChoice = Just 1
  | lowerQuery `T.isInfixOf` lowerChoice = Just 2
  | subsequence (T.unpack lowerQuery) (T.unpack lowerChoice) = Just 3
  | otherwise = Nothing
  where
    lowerQuery = T.toLower query
    lowerChoice = T.toLower choice
    subsequence [] _ = True
    subsequence _ [] = False
    subsequence (x : xs) (y : ys)
      | x == toLower y = subsequence xs ys
      | otherwise = subsequence (x : xs) ys

drawExplorer :: Explorer -> [Widget ()]
drawExplorer state =
  [ vBox
      [ drawHeader state,
        hBorder,
        drawBody state,
        hBorder,
        drawStatus state
      ]
  ]

drawHeader :: Explorer -> Widget ()
drawHeader state =
  hBox
    ( [withAttr (if stage == explorerStage state then attrName "selected" else attrName "tab") (txt (" " <> T.pack (show (fromEnum stage + 1)) <> " " <> stageLabel stage <> " ")) | stage <- [minBound .. maxBound]]
        <> [txt "   "]
        <> [withAttr (if level == explorerLevel state then attrName "selected" else levelAttr level) (txt (" " <> levelText level <> " ")) | level <- [O0, O1, O2, Os]]
        <> [padRight Max (txt ("   " <> fromMaybe "" (explorerModule state)))]
    )
  where
    levelAttr level = case Map.lookup level (explorerBuilds state) of
      Just (BuildReady _) -> attrName "tab"
      Just (BuildRunning _) -> attrName "building"
      Just (BuildFailed _) -> attrName "failed"
      Nothing -> attrName "absent"

drawBody :: Explorer -> Widget ()
drawBody state =
  case explorerPrompt state of
    Just prompt | listsChoices (promptKind prompt) -> drawPrompt prompt state
    _
      | isNothing (explorerModule state) -> padBottom Max (padRight Max (txt (waitingText state)))
      | otherwise -> padBottom Max (vBox (map drawLine [top .. min (count - 1) (top + explorerHeight state - 1)]))
  where
    document = explorerDocument state
    count = V.length (documentLines document)
    top = explorerTop state
    width = length (show count)
    drawLine index =
      let number = T.justifyRight width ' ' (T.pack (show (index + 1)))
          text = documentLines document V.! index
          segments = fromMaybe [] (documentSegments document V.!? index)
          highlighted = T.concat (map segmentText segments) == text && not (null segments)
          pieces
            | index == explorerCursor state = [(clean text, attrName "cursor")]
            | highlighted = [(clean (segmentText segment), classAttr (segmentClass segment)) | segment <- segments]
            | otherwise = [(clean text, attrName "plain")]
          content = case explorerLastQuery state of
            Just (SearchText _ query) -> markMatches query pieces
            _ -> pieces
          lineAttr = if index == explorerCursor state then attrName "cursor" else attrName "plain"
       in hBox [withAttr (attrName "gutter") (txt (number <> " ")), withAttr lineAttr (padRight Max (hBox [withAttr attr (txt piece) | (piece, attr) <- content]))]
    clean = T.replace "\t" "    "

-- | True for a prompt that shows its choices in place of the view.
listsChoices :: PromptKind -> Bool
listsChoices kind =
  case kind of
    PromptModule -> True
    PromptFunction -> True
    PromptSearch _ -> False
    PromptSave _ -> False

-- | Give the occurrences of a query in a line the attribute of a match. The
-- pieces of the line keep their attributes in the other parts.
markMatches :: Text -> [(Text, AttrName)] -> [(Text, AttrName)]
markMatches query pieces =
  case textMatches query (T.concat (map fst pieces)) of
    [] -> pieces
    matches ->
      let inMatch position = any (\(start, size) -> position >= start && position < start + size) matches
          characters = concat [[(character, attr) | character <- T.unpack piece] | (piece, attr) <- pieces]
          marked = [(character, if inMatch position then attrName "match" else attr) | (position, (character, attr)) <- zip [0 :: Int ..] characters]
       in [(T.pack (map fst (NonEmpty.toList group)), snd (NonEmpty.head group)) | group <- NonEmpty.groupBy (\left right -> snd left == snd right) marked]

waitingText :: Explorer -> Text
waitingText state =
  case Map.lookup (explorerLevel state) (explorerBuilds state) of
    Just (BuildRunning message) -> "Building " <> levelText (explorerLevel state) <> ": " <> message
    Just (BuildFailed message) -> "The build failed:\n" <> message
    _ -> "Waiting"

drawPrompt :: Prompt -> Explorer -> Widget ()
drawPrompt prompt state =
  padBottom Max $
    vBox
      ( txt (label <> promptQuery prompt <> "_")
          : [ withAttr (if index == promptChoice prompt then attrName "cursor" else attrName "plain") (padRight Max (txt choice))
            | (index, choice) <- zip [0 ..] (take (explorerHeight state - 1) (promptChoices prompt state))
            ]
      )
  where
    label = case promptKind prompt of
      PromptModule -> "Go to module: "
      PromptFunction -> "Go to definition: "
      PromptSearch Forward -> "/"
      PromptSearch Backward -> "?"
      PromptSave _ -> "Save to file: "

drawStatus :: Explorer -> Widget ()
drawStatus state =
  case explorerPrompt state of
    Just (Prompt (PromptSearch direction) query _) -> padRight Max (txt ((if direction == Forward then "/" else "?") <> query <> "_"))
    Just (Prompt (PromptSave confirmed) path _) ->
      padRight Max (txt (maybe "" (const (explorerStatus state <> "  ")) confirmed <> "Save to file: " <> path <> "_"))
    _ ->
      hBox
        [ padRight Max (txt (explorerStatus state)),
          txt (buildText <> "  1-8/Tab stage  o level  g module  d definition  / ? search  w save  q quit")
        ]
  where
    buildText = case [(level, message) | (level, BuildRunning message) <- Map.toList (explorerBuilds state)] of
      (level, message) : _ -> "[" <> levelText level <> ": " <> message <> "]"
      [] -> ""

-- | The attribute of a highlight class. A TextMate scope such as
-- @keyword.operator.aihc-fc@ becomes a hierarchical attribute name, so the
-- attribute of the longest defined prefix applies.
classAttr :: Text -> AttrName
classAttr highlightClass
  | T.null highlightClass = attrName "plain"
  | otherwise = foldl (\name part -> name <> attrName (T.unpack part)) (attrName "hl") (T.splitOn "." highlightClass)

highlightAttrMap :: AttrMap
highlightAttrMap =
  attrMap
    Vty.defAttr
    [ (attrName "selected", Vty.black `on` Vty.cyan),
      (attrName "tab", fg Vty.white),
      (attrName "building", fg Vty.yellow),
      (attrName "failed", fg Vty.red),
      (attrName "absent", fg Vty.brightBlack),
      (attrName "gutter", fg Vty.brightBlack),
      (attrName "cursor", Vty.defAttr `Vty.withStyle` Vty.reverseVideo),
      (attrName "match", Vty.black `on` Vty.yellow),
      (hl ["comment"], fg Vty.brightBlack),
      (hl ["string"], fg Vty.green),
      (hl ["constant"], fg Vty.magenta),
      (hl ["constant", "character", "escape"], fg Vty.brightGreen),
      (hl ["keyword"], fg Vty.blue `Vty.withStyle` Vty.bold),
      (hl ["keyword", "operator"], fg Vty.yellow),
      (hl ["storage"], fg Vty.blue),
      (hl ["entity", "name"], fg Vty.cyan),
      (hl ["entity", "name", "function"], fg Vty.cyan `Vty.withStyle` Vty.bold),
      (hl ["entity", "name", "type"], fg Vty.brightYellow),
      (hl ["entity", "name", "constructor"], fg Vty.brightMagenta),
      (hl ["entity", "name", "namespace"], fg Vty.brightBlue),
      (hl ["entity", "name", "label"], fg Vty.brightCyan),
      (hl ["support"], fg Vty.cyan),
      (hl ["variable", "other", "global"], fg Vty.brightCyan),
      (hl ["meta", "preprocessor"], fg Vty.magenta),
      (hl ["punctuation"], fg Vty.white),
      (hl ["invalid"], fg Vty.red)
    ]
  where
    hl = foldl (\name part -> name <> attrName part) (attrName "hl")
