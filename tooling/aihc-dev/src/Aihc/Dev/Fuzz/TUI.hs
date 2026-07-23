{-# LANGUAGE NamedFieldPuns #-}

-- | Pure dashboard layout and incremental ANSI rendering.
module Aihc.Dev.Fuzz.TUI
  ( Dashboard (..),
    renderDashboard,
    renderFrameUpdate,
  )
where

import Data.List (intercalate, isPrefixOf)
import Data.Time.Clock (NominalDiffTime)
import Text.Printf (printf)

data Dashboard = Dashboard
  { dashboardActive :: [(Int, String, Int)],
    dashboardBatches :: Int,
    dashboardCases :: Int,
    dashboardElapsed :: NominalDiffTime,
    dashboardJobs :: Int,
    dashboardProperties :: Int
  }

-- | Lay out a dashboard that never exceeds the usable terminal rectangle.
renderDashboard :: Int -> Int -> Dashboard -> [String]
renderDashboard rows columns dashboard
  | rows <= 0 || columns <= 1 = []
  | rows < 8 || columns < 50 = compactDashboard rows usableWidth dashboard
  | otherwise = fullDashboard rows usableWidth dashboard
  where
    -- Avoid writing in the bottom-right cell, which makes many terminals scroll.
    usableWidth = columns - 1

fullDashboard :: Int -> Int -> Dashboard -> [String]
fullDashboard rows width dashboard@Dashboard {dashboardActive, dashboardBatches, dashboardCases, dashboardElapsed, dashboardJobs, dashboardProperties} =
  map (clip width) $
    [ alignedTitle width (" AIHC FUZZ  " <> statusDot <> " RUNNING") (formatElapsed dashboardElapsed),
      replicate width '─',
      " "
        <> formatInteger dashboardCases
        <> " cases  ·  "
        <> formatInteger dashboardBatches
        <> " batches  ·  "
        <> show dashboardProperties
        <> " properties",
      " " <> show dashboardJobs <> " workers  ·  10,000 cases per batch  ·  q/esc quit",
      "",
      " ACTIVE  " <> show (length dashboardActive)
    ]
      <> activeLines (rows - 6) width dashboard

compactDashboard :: Int -> Int -> Dashboard -> [String]
compactDashboard rows width dashboard@Dashboard {dashboardCases, dashboardElapsed} =
  map (clip width) $
    take rows $
      [ "AIHC FUZZ "
          <> statusDot
          <> "  "
          <> formatElapsed dashboardElapsed
          <> "  ·  "
          <> formatInteger dashboardCases
          <> " cases"
      ]
        <> activeLines (rows - 1) width dashboard

activeLines :: Int -> Int -> Dashboard -> [String]
activeLines slots width Dashboard {dashboardActive = firstActive : remainingActive}
  | slots <= 0 = []
  | length dashboardActive <= slots = map (renderActive width) dashboardActive
  | slots == 1 = [renderCompactActive firstActive <> "  (+" <> show (length remainingActive) <> ")"]
  | otherwise =
      map (renderActive width) (take (slots - 1) dashboardActive)
        <> ["  + " <> show (length dashboardActive - slots + 1) <> " more active workers"]
  where
    dashboardActive = firstActive : remainingActive
activeLines _ _ Dashboard {dashboardActive = []} = []

renderActive :: Int -> (Int, String, Int) -> String
renderActive width active@(workerId, propertyId, successfulCases)
  | width < 50 = renderCompactActive active
  | otherwise =
      "  "
        <> leftPad 2 (show workerId)
        <> "  "
        <> progressBar barWidth successfulCases
        <> "  "
        <> leftPad 6 (formatInteger successfulCases)
        <> " / 10,000  "
        <> propertyId
  where
    barWidth = max 8 (min 18 (width `div` 5))

renderCompactActive :: (Int, String, Int) -> String
renderCompactActive (workerId, propertyId, successfulCases) =
  "[" <> show workerId <> "] " <> leftPad 3 (show percentage) <> "%  " <> propertyId
  where
    percentage = min 100 (successfulCases * 100 `div` 10000)

progressBar :: Int -> Int -> String
progressBar width successfulCases =
  replicate complete '━' <> replicate (width - complete) '─'
  where
    complete = min width (successfulCases * width `div` 10000)

alignedTitle :: Int -> String -> String -> String
alignedTitle width left right =
  left <> replicate padding ' ' <> right
  where
    padding = max 1 (width - length left - length right)

-- | Render only rows whose contents changed. The returned update never clears
-- the whole screen, so frequent progress updates do not produce visible flashes.
renderFrameUpdate :: Int -> [String] -> [String] -> String
renderFrameUpdate rows previous current =
  concatMap renderChangedRow [1 .. min rows (max (length previous) (length current))]
  where
    renderChangedRow row
      | previousLine == currentLine = ""
      | otherwise = cursorAt row <> "\ESC[2K" <> styleLine currentLine
      where
        previousLine = lineAt row previous
        currentLine = lineAt row current

lineAt :: Int -> [String] -> String
lineAt row lines' =
  case drop (row - 1) lines' of
    line : _ -> line
    [] -> ""

cursorAt :: Int -> String
cursorAt row = "\ESC[" <> show row <> ";1H"

styleLine :: String -> String
styleLine line
  | "AIHC FUZZ" `isPrefixOf` dropWhile (== ' ') line = bold <> cyan <> line <> reset
  | not (null line) && all (== '─') line = dim <> line <> reset
  | " ACTIVE" `isPrefixOf` line = bold <> line <> reset
  | "  + " `isPrefixOf` line = yellow <> line <> reset
  | otherwise = line

statusDot :: String
statusDot = "●"

bold, cyan, dim, reset, yellow :: String
bold = "\ESC[1m"
cyan = "\ESC[36m"
dim = "\ESC[2m"
yellow = "\ESC[33m"
reset = "\ESC[0m"

clip :: Int -> String -> String
clip width value
  | width <= 0 = ""
  | length value <= width = value
  | width == 1 = "…"
  | otherwise = take (width - 1) value <> "…"

formatElapsed :: NominalDiffTime -> String
formatElapsed elapsed =
  let totalSeconds = max 0 (floor elapsed :: Int)
      (hours, afterHours) = totalSeconds `divMod` 3600
      (minutes, seconds) = afterHours `divMod` 60
   in printf "%02d:%02d:%02d" hours minutes seconds

leftPad :: Int -> String -> String
leftPad width value = replicate (max 0 (width - length value)) ' ' <> value

formatInteger :: Int -> String
formatInteger value =
  reverse . intercalate "," . chunksOfThree . reverse $ show value
  where
    chunksOfThree [] = []
    chunksOfThree input = take 3 input : chunksOfThree (drop 3 input)
