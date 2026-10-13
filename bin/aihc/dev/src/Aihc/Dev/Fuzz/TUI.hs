{-# LANGUAGE NamedFieldPuns #-}

-- | Pure dashboard layout and incremental ANSI rendering.
module Aihc.Dev.Fuzz.TUI
  ( Dashboard (..),
    renderDashboard,
    renderFrameUpdate,
  )
where

import Data.List (isPrefixOf, sortOn)
import Data.Time.Clock (NominalDiffTime)
import Text.Printf (printf)

data Dashboard = Dashboard
  { dashboardActive :: [(Int, String)],
    dashboardPropertyCases :: [(String, Int)],
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
fullDashboard rows width dashboard@Dashboard {dashboardBatches, dashboardCases, dashboardElapsed, dashboardJobs, dashboardProperties} =
  map (clip width) $
    [ alignedTitle width (" AIHC FUZZ COMMAND CENTER  " <> statusDot <> " RUNNING") (formatElapsed dashboardElapsed),
      replicate width '─',
      " "
        <> formatCount dashboardCases
        <> " cases  ·  "
        <> formatCount dashboardBatches
        <> " batches  ·  "
        <> show dashboardProperties
        <> " properties",
      " " <> show dashboardJobs <> " workers  ·  10,000 cases per batch  ·  q/esc quit",
      "",
      " PROPERTIES  " <> show dashboardProperties
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
          <> formatCount dashboardCases
          <> " cases"
      ]
        <> activeLines (rows - 1) width dashboard

activeLines :: Int -> Int -> Dashboard -> [String]
activeLines slots _ Dashboard {dashboardActive, dashboardPropertyCases, dashboardElapsed}
  | slots <= 0 = []
  | length properties <= slots = map renderProperty properties
  | otherwise =
      map renderProperty (take (slots - 1) properties)
        <> ["  + " <> show (length properties - slots + 1) <> " more properties"]
  where
    workers name = length [() | (_, propertyId) <- dashboardActive, propertyId == name]
    properties = sortOn (\(name, _) -> (workers name == 0, name)) dashboardPropertyCases
    spinner = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏" !! (floor (dashboardElapsed * 10) `mod` 10)
    renderProperty (name, count) =
      "  "
        <> [if workers name > 0 then spinner else '·']
        <> "  "
        <> leftPad 6 (formatCount count)
        <> "  "
        <> name
        <> if workers name > 1 then "  [" <> show (workers name) <> " jobs]" else ""

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
  | " PROPERTIES" `isPrefixOf` line = bold <> line <> reset
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

-- | Use decimal SI prefixes for cumulative test counts.
formatCount :: Int -> String
formatCount value
  | value < 1000 = show value
  | otherwise = scale (fromIntegral value :: Double) ["K", "M", "G", "T", "P", "E"]
  where
    scale :: Double -> [String] -> String
    scale number (suffix : rest)
      | number >= 999950 && not (null rest) = scale (number / 1000) rest
      | otherwise = printf "%.1f%s" (number / 1000) suffix
    scale number [] = printf "%.0f" number
