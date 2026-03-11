-- |
-- Module: Options.Help
-- License: MIT
module Options.Help
  ( addHelpFlags,
    checkHelpFlag,
    helpFor,
    HelpFlag (..),
  )
where

import Control.Monad.Writer
import Data.Char (isSpace)
import Data.List (intercalate, partition)
import qualified Data.Map as Map
import Data.Maybe (isNothing, listToMaybe)
import qualified Data.Set as Set
import Options.Tokenize
import Options.Types

data HelpFlag = HelpSummary | HelpAll | HelpGroup String
  deriving (Eq, Show)

addHelpFlags :: OptionDefinitions -> OptionDefinitions
addHelpFlags (OptionDefinitions opts subcmds) = OptionDefinitions withHelp subcmdsWithHelp
  where
    shortFlags = Set.fromList do
      opt <- opts
      optionInfoShortFlags opt
    longFlags = Set.fromList do
      opt <- opts
      optionInfoLongFlags opt

    withHelp = optHelpSummary ++ optsGroupHelp ++ opts

    groupHelp =
      Group
        { groupName = "all",
          groupTitle = "Help Options",
          groupDescription = "Show all help options."
        }

    optSummary =
      OptionInfo
        { optionInfoKey = OptionKeyHelpSummary,
          optionInfoShortFlags = [],
          optionInfoLongFlags = [],
          optionInfoDefault = "",
          optionInfoUnary = True,
          optionInfoUnaryOnly = True,
          optionInfoDescription = "Show option summary.",
          optionInfoGroup = Just groupHelp,
          optionInfoLocation = Nothing,
          optionInfoTypeName = ""
        }

    optGroupHelp group flag =
      OptionInfo
        { optionInfoKey = OptionKeyHelpGroup (groupName group),
          optionInfoShortFlags = [],
          optionInfoLongFlags = [flag],
          optionInfoDefault = "",
          optionInfoUnary = True,
          optionInfoUnaryOnly = True,
          optionInfoDescription = groupDescription group,
          optionInfoGroup = Just groupHelp,
          optionInfoLocation = Nothing,
          optionInfoTypeName = ""
        }

    optHelpSummary =
      if Set.member 'h' shortFlags
        then
          if Set.member "help" longFlags
            then []
            else
              [ optSummary
                  { optionInfoLongFlags = ["help"]
                  }
              ]
        else
          if Set.member "help" longFlags
            then
              [ optSummary
                  { optionInfoShortFlags = ['h']
                  }
              ]
            else
              [ optSummary
                  { optionInfoShortFlags = ['h'],
                    optionInfoLongFlags = ["help"]
                  }
              ]

    optsGroupHelp = do
      let (groupsAndOpts, _) = uniqueGroups opts
      let groups = [g | (g, _) <- groupsAndOpts]
      group <- (groupHelp : groups)
      let flag = "help-" ++ groupName group
      if Set.member flag longFlags
        then []
        else [optGroupHelp group flag]

    subcmdsWithHelp = do
      (subcmdName, subcmdOpts) <- subcmds
      let subcmdLongFlags = Set.fromList do
            opt <- subcmdOpts ++ optsGroupHelp
            optionInfoLongFlags opt

      let (groupsAndOpts, _) = uniqueGroups subcmdOpts
      let groups = [g | (g, _) <- groupsAndOpts]
      let newOpts = do
            group <- groups
            let flag = "help-" ++ groupName group
            if Set.member flag (Set.union longFlags subcmdLongFlags)
              then []
              else [optGroupHelp group flag]
      return (subcmdName, newOpts ++ subcmdOpts)

checkHelpFlag :: Tokens -> Maybe HelpFlag
checkHelpFlag tokens = flag
  where
    flag = listToMaybe helpKeys
    helpKeys = do
      (k, _) <- tokensList tokens
      case k of
        [OptionKeyHelpSummary] -> return HelpSummary
        [OptionKeyHelpGroup "all"] -> return HelpAll
        [OptionKeyHelpGroup name] -> return (HelpGroup name)
        _ -> []

helpFor :: HelpFlag -> OptionDefinitions -> Maybe String -> String
helpFor flag defs subcmd = case flag of
  HelpSummary -> execWriter (showHelpSummary defs subcmd)
  HelpAll -> execWriter (showHelpAll defs subcmd)
  HelpGroup name -> execWriter (showHelpOneGroup defs name subcmd)

showOptionHelp :: OptionInfo -> Writer String ()
showOptionHelp info = do
  let safeHead xs = case xs of
        [] -> []
        (x : _) -> [x]
  let shorts = optionInfoShortFlags info
  let longs = optionInfoLongFlags info
  let optStrings = map (\x -> ['-', x]) (safeHead shorts) ++ map (\x -> "--" ++ x) (safeHead longs)
  unless (null optStrings) do
    let optStringCsv = intercalate ", " optStrings
    tell "  "
    tell optStringCsv
    unless (null (optionInfoTypeName info)) do
      tell " :: "
      tell (optionInfoTypeName info)
    tell "\n"
    unless (null (optionInfoDescription info)) do
      forM_ (wrapWords 76 (optionInfoDescription info)) \line -> do
        tell "    "
        tell line
        tell "\n"
    unless (null (optionInfoDefault info)) do
      tell "    default: "
      tell (optionInfoDefault info)
      tell "\n"

-- A simple greedy word-wrapper for fixed-width terminals, permitting overruns
-- and ragged edges.
wrapWords :: Int -> String -> [String]
wrapWords breakWidth = wrap
  where
    wrap line =
      if length line <= breakWidth
        then [line]
        else
          if any isBreak line
            then case splitAt breakWidth line of
              (beforeBreak, afterBreak) -> case reverseBreak isBreak beforeBreak of
                (beforeWrap, afterWrap) -> beforeWrap : wrap (afterWrap ++ afterBreak)
            else [line]
    isBreak c = case c of
      '\xA0' -> False -- NO-BREAK SPACE
      '\x202F' -> False -- NARROW NO-BREAK SPACE
      '\x2011' -> False -- NON-BREAKING HYPHEN
      '-' -> True
      _ -> isSpace c
    reverseBreak :: (a -> Bool) -> [a] -> ([a], [a])
    reverseBreak f xs = case break f (reverse xs) of
      (after, before) -> (reverse before, reverse after)

showHelpSummary :: OptionDefinitions -> Maybe String -> Writer String ()
showHelpSummary (OptionDefinitions mainOpts subcmds) subcmd = do
  let subcmdOptions = do
        subcmdName <- subcmd
        opts <- lookup subcmdName subcmds
        return (subcmdName, opts)

  let (groupInfos, ungroupedMainOptions) = uniqueGroups mainOpts

  -- Always print --help group
  let hasHelp = filter (\(g, _) -> groupName g == "all") groupInfos
  forM_ hasHelp showHelpGroup

  unless (null ungroupedMainOptions) do
    tell "Application Options:\n"
    forM_ ungroupedMainOptions showOptionHelp
    unless (null subcmds) (tell "\n")

  case subcmdOptions of
    Nothing -> unless (null subcmds) do
      tell "Subcommands:\n"
      forM_ subcmds \(subcmdName, _) -> do
        tell "  "
        tell subcmdName
        -- TODO: subcommand help description
        tell "\n"
      tell "\n"
    Just (n, subOpts) -> do
      -- TODO: subcommand description
      -- TODO: handle grouped options in subcommands?
      tell ("Options for subcommand " ++ show n ++ ":\n")
      forM_ subOpts showOptionHelp
      tell "\n"

showHelpAll :: OptionDefinitions -> Maybe String -> Writer String ()
showHelpAll (OptionDefinitions mainOpts subcmds) subcmd = do
  let subcmdOptions = do
        subcmdName <- subcmd
        opts <- lookup subcmdName subcmds
        return (subcmdName, opts)

  let (groupInfos, ungroupedMainOptions) = uniqueGroups mainOpts

  -- Always print --help group first, if present
  let (hasHelp, noHelp) = partition (\(g, _) -> groupName g == "all") groupInfos
  forM_ hasHelp showHelpGroup
  forM_ noHelp showHelpGroup

  tell "Application Options:\n"
  forM_ ungroupedMainOptions showOptionHelp
  unless (null subcmds) (tell "\n")

  case subcmdOptions of
    Nothing -> forM_ subcmds \(subcmdName, subcmdOpts) -> do
      -- no subcommand description
      tell ("Options for subcommand " ++ show subcmdName ++ ":\n")
      forM_ subcmdOpts showOptionHelp
      tell "\n"
    Just (n, subOpts) -> do
      -- TODO: subcommand description
      -- TODO: handle grouped options in subcommands?
      tell ("Options for subcommand " ++ show n ++ ":\n")
      forM_ subOpts showOptionHelp
      tell "\n"

showHelpGroup :: (Group, [OptionInfo]) -> Writer String ()
showHelpGroup (groupInfo, opts) = do
  tell (groupTitle groupInfo ++ ":\n")
  forM_ opts showOptionHelp
  tell "\n"

showHelpOneGroup :: OptionDefinitions -> String -> Maybe String -> Writer String ()
showHelpOneGroup (OptionDefinitions mainOpts subcmds) name subcmd = do
  let opts = case subcmd of
        Nothing -> mainOpts
        Just n -> case lookup n subcmds of
          Just infos -> mainOpts ++ infos -- both
          Nothing -> mainOpts
  let (groupInfos, _) = uniqueGroups opts

  -- Always print --help group
  let group = filter (\(g, _) -> groupName g == name) groupInfos
  forM_ group showHelpGroup

uniqueGroups :: [OptionInfo] -> ([(Group, [OptionInfo])], [OptionInfo])
uniqueGroups allOptions = (Map.elems infoMap, ungroupedOptions)
  where
    infoMap = Map.fromListWith merge do
      opt <- allOptions
      case optionInfoGroup opt of
        Nothing -> []
        Just g -> [(groupName g, (g, [opt]))]
    merge (g, opts1) (_, opts2) = (g, opts2 ++ opts1)
    ungroupedOptions = [o | o <- allOptions, isNothing (optionInfoGroup o)]
