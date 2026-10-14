{-# LANGUAGE NamedFieldPuns #-}

-- | Brick dashboard for the fuzz runner.
module Aihc.Dev.Fuzz.TUI
  ( Model (..),
    runDashboard,
  )
where

import Aihc.Dev.Fuzz.Bugs (formatSeen)
import Aihc.Dev.Fuzz.Model
import Aihc.Dev.Fuzz.Registry (FuzzProperty)
import Brick
import Brick.BChan (BChan)
import Brick.Widgets.Border (borderWithLabel, hBorder, hBorderWithLabel)
import Brick.Widgets.Border.Style (unicodeRounded)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime)
import Graphics.Vty qualified as Vty

data Name
  = PropertiesViewport
  | BugsViewport
  deriving (Eq, Ord, Show)

data Model = Model
  { modelStartedAt :: UTCTime,
    modelJobs :: Int,
    modelTimeLimit :: Maybe NominalDiffTime,
    modelProperties :: [FuzzProperty],
    modelStorePath :: FilePath,
    modelSnapshot :: Snapshot,
    modelRates :: Map String Double,
    modelOutcome :: Maybe Outcome
  }

-- | Show the dashboard until the user quits, the time limit ends, or a worker crashes.
runDashboard :: BChan Event -> Model -> IO Outcome
runDashboard channel initial = do
  (final, vty) <- customMainWithDefaultVty (Just channel) dashboardApp initial
  Vty.shutdown vty
  pure (fromMaybe UserQuit (modelOutcome final))

dashboardApp :: App Model Event Name
dashboardApp =
  App
    { appDraw = drawModel,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure (),
      appAttrMap = const dashboardAttrMap
    }

handleEvent :: BrickEvent Name Event -> EventM Name Model ()
handleEvent event =
  case event of
    AppEvent (Tick snapshot) -> modify (applyTick snapshot)
    AppEvent TimeLimitEvent -> finish TimeLimitReached
    AppEvent (Crashed exception) -> finish (WorkerCrashed exception)
    VtyEvent (Vty.EvKey key modifiers) ->
      case key of
        Vty.KChar 'q' -> finish UserQuit
        Vty.KChar 'Q' -> finish UserQuit
        Vty.KEsc -> finish UserQuit
        Vty.KChar 'c' | Vty.MCtrl `elem` modifiers -> finish UserQuit
        Vty.KUp -> vScrollBy (viewportScroll PropertiesViewport) (-1)
        Vty.KDown -> vScrollBy (viewportScroll PropertiesViewport) 1
        Vty.KPageUp -> vScrollBy (viewportScroll PropertiesViewport) (-10)
        Vty.KPageDown -> vScrollBy (viewportScroll PropertiesViewport) 10
        Vty.KChar 'k' -> vScrollBy (viewportScroll BugsViewport) (-1)
        Vty.KChar 'j' -> vScrollBy (viewportScroll BugsViewport) 1
        _ -> pure ()
    _ -> pure ()
  where
    finish outcome = do
      modify (\model -> model {modelOutcome = Just outcome})
      halt

applyTick :: Snapshot -> Model -> Model
applyTick snapshot model@Model {modelSnapshot = previous, modelRates} =
  model
    { modelSnapshot = snapshot,
      modelRates = updateRates seconds (snapshotPropertyCases previous) (snapshotPropertyCases snapshot) modelRates
    }
  where
    seconds = realToFrac (diffUTCTime (snapshotTime snapshot) (snapshotTime previous))

drawModel :: Model -> [Widget Name]
drawModel model@Model {modelSnapshot = snapshot} =
  [ withBorderStyle unicodeRounded . borderWithLabel (withAttr titleAttr (str " aihc-dev fuzz ")) $
      vBox
        [ padLeftRight 1 (summaryWidget model),
          hBorderWithLabel (withAttr sectionAttr (str " properties ")),
          padLeftRight 1 (propertiesWidget model),
          hBorderWithLabel (withAttr sectionAttr (str (" known bugs · " <> show (length (snapshotBugs snapshot)) <> " "))),
          padLeftRight 1 (bugsWidget model),
          hBorder,
          padLeftRight 1 (footerWidget model)
        ]
  ]

summaryWidget :: Model -> Widget Name
summaryWidget Model {modelStartedAt, modelJobs, modelProperties, modelSnapshot = snapshot} =
  hBox
    [ withAttr runningAttr (str "● running"),
      str "   ",
      withAttr countAttr (str (formatCount (snapshotCases snapshot))),
      str " cases   ",
      withAttr countAttr (str (formatCount (snapshotBatches snapshot))),
      str " batches   ",
      withAttr countAttr (str (show modelJobs)),
      str " workers   ",
      withAttr countAttr (str (show (length modelProperties))),
      str " properties   ",
      bugsSummary,
      padLeft Max (withAttr countAttr (str (formatElapsed elapsed)))
    ]
  where
    elapsed = diffUTCTime (snapshotTime snapshot) modelStartedAt
    newBugs = Set.size (snapshotNewBugs snapshot)
    knownBugs = length (snapshotBugs snapshot)
    bugsSummary
      | knownBugs == 0 = withAttr okAttr (str "no bugs")
      | newBugs == 0 = withAttr bugAttr (str (show knownBugs <> " known bugs"))
      | otherwise = withAttr newBugAttr (str (show knownBugs <> " known bugs, " <> show newBugs <> " new"))

propertiesWidget :: Model -> Widget Name
propertiesWidget Model {modelStartedAt, modelProperties, modelSnapshot = snapshot, modelRates} =
  vBox
    [ withAttr headerAttr (propertyLine componentWidth (str "  ") (str "component") (str "property") (str "cases") (str "rate") (str "workers") (str "bugs")),
      viewport PropertiesViewport Vertical (vBox (map renderRow rows))
    ]
  where
    rows = propertyRows modelProperties snapshot modelRates
    componentWidth = maximum (9 : map (length . rowComponent) rows)
    spinner = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏" !! (floor (diffUTCTime (snapshotTime snapshot) modelStartedAt * 10) `mod` 10)
    renderRow PropertyRow {rowComponent, rowName, rowCases, rowRate, rowWorkers, rowBugs} =
      propertyLine
        componentWidth
        (if rowWorkers > 0 then withAttr activeAttr (str [spinner, ' ']) else withAttr dimAttr (str "· "))
        (withAttr dimAttr (str rowComponent))
        (str rowName)
        (withAttr countAttr (str (formatCount rowCases)))
        (if rowWorkers > 0 then str (formatCount (round rowRate) <> "/s") else withAttr dimAttr (str "–"))
        (if rowWorkers > 0 then withAttr activeAttr (str (show rowWorkers)) else withAttr dimAttr (str "–"))
        (if rowBugs > 0 then withAttr bugAttr (str (show rowBugs)) else withAttr dimAttr (str "–"))

propertyLine :: Int -> Widget Name -> Widget Name -> Widget Name -> Widget Name -> Widget Name -> Widget Name -> Widget Name -> Widget Name
propertyLine componentWidth marker component name cases rate workers bugs =
  hBox
    [ marker,
      leftCell componentWidth component,
      str "  ",
      padRight Max name,
      str "  ",
      rightCell 7 cases,
      str "  ",
      rightCell 9 rate,
      str "  ",
      rightCell 7 workers,
      str "  ",
      rightCell 4 bugs
    ]

bugsWidget :: Model -> Widget Name
bugsWidget Model {modelSnapshot = snapshot}
  | null rows = vLimit 1 (withAttr dimAttr (str "No failure is known. Each new failure is listed here and reported to GitHub."))
  | otherwise =
      vLimit (1 + min 8 (length rows)) $
        vBox
          [ withAttr headerAttr (bugLine (str "  ") (str "bug") (str "property") (str "hits") (str "first seen") (str "issue")),
            viewport BugsViewport Vertical (vBox (map renderRow rows))
          ]
  where
    rows = bugRows snapshot
    renderRow BugRow {bugRowId, bugRowProperty, bugRowHits, bugRowFirstSeen, bugRowNew, bugRowIssue, bugRowStatus} =
      bugLine
        (if bugRowNew then withAttr newBugAttr (str "● ") else withAttr bugAttr (str "○ "))
        (withAttr dimAttr (str (take 12 bugRowId)))
        (str bugRowProperty)
        (withAttr countAttr (str (show bugRowHits)))
        (str (formatSeen bugRowFirstSeen))
        (withAttr (statusAttr bugRowIssue bugRowStatus) (str bugRowStatus))
    statusAttr issue status
      | Just _ <- issue = okAttr
      | status == "reporting…" = sendingAttr
      | status == "pending" = pendingAttr
      | otherwise = blockedAttr

bugLine :: Widget Name -> Widget Name -> Widget Name -> Widget Name -> Widget Name -> Widget Name -> Widget Name
bugLine marker identifier property hits firstSeen status =
  hBox
    [ marker,
      leftCell 12 identifier,
      str "  ",
      leftCell 40 property,
      str "  ",
      rightCell 5 hits,
      str "  ",
      leftCell 16 firstSeen,
      str "  ",
      padRight Max status
    ]

footerWidget :: Model -> Widget Name
footerWidget Model {modelTimeLimit, modelStartedAt, modelStorePath, modelSnapshot = snapshot} =
  hBox
    [ withAttr dimAttr (str "q quit · ↑↓ PgUp PgDn scroll properties · j k scroll bugs"),
      str "  ",
      padLeft Max (withAttr dimAttr (str rightText))
    ]
  where
    elapsed = diffUTCTime (snapshotTime snapshot) modelStartedAt
    rightText =
      case modelTimeLimit of
        Just limit -> formatElapsed (max 0 (limit - elapsed)) <> " left · " <> shortPath
        Nothing -> shortPath
    shortPath
      | length modelStorePath <= 40 = modelStorePath
      | otherwise = "…" <> drop (length modelStorePath - 39) modelStorePath

leftCell :: Int -> Widget Name -> Widget Name
leftCell width content = hLimit width (padRight Max content)

rightCell :: Int -> Widget Name -> Widget Name
rightCell width content = hLimit width (padLeft Max content)

titleAttr, sectionAttr, headerAttr, dimAttr, activeAttr, countAttr, okAttr, bugAttr, newBugAttr, pendingAttr, sendingAttr, blockedAttr, runningAttr :: AttrName
titleAttr = attrName "title"
sectionAttr = attrName "section"
headerAttr = attrName "header"
dimAttr = attrName "dim"
activeAttr = attrName "active"
countAttr = attrName "count"
okAttr = attrName "ok"
bugAttr = attrName "bug"
newBugAttr = attrName "new-bug"
pendingAttr = attrName "pending"
sendingAttr = attrName "sending"
blockedAttr = attrName "blocked"
runningAttr = attrName "running"

dashboardAttrMap :: AttrMap
dashboardAttrMap =
  attrMap
    Vty.defAttr
    [ (titleAttr, fg Vty.cyan `Vty.withStyle` Vty.bold),
      (sectionAttr, fg Vty.white `Vty.withStyle` Vty.bold),
      (headerAttr, fg Vty.brightBlack `Vty.withStyle` Vty.underline),
      (dimAttr, fg Vty.brightBlack),
      (activeAttr, fg Vty.yellow),
      (countAttr, Vty.defAttr `Vty.withStyle` Vty.bold),
      (okAttr, fg Vty.green),
      (bugAttr, fg Vty.red),
      (newBugAttr, fg Vty.red `Vty.withStyle` Vty.bold),
      (pendingAttr, fg Vty.yellow),
      (sendingAttr, fg Vty.cyan),
      (blockedAttr, fg Vty.magenta),
      (runningAttr, fg Vty.green `Vty.withStyle` Vty.bold)
    ]
