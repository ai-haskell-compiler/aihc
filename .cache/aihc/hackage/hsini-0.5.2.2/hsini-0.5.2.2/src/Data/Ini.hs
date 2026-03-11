{-# LANGUAGE ImportQualifiedPost #-}

{- |
Module    : Data.Ini
Copyright : 2011-2014 Magnus Therning
License   : BSD3

A representation of configuration options.  It consists of /sections/,
each which can contain 0 or more /options/.  Each options is a /key/,
/value/ pair.

This module contains the API for constructing, manipulating, and querying
configurations.
-}
module Data.Ini where

-- {{{1 imports
import Data.Map qualified as M
import Data.Maybe (isJust)

import Data.Ini.Types (Config, OptionName, OptionValue, Section, SectionName)

-- {{{1 configurations

-- | Constructs an empty configuration.
emptyConfig :: Config
emptyConfig = M.empty

-- {{{1 sections

-- | Returns @True@ iff the configuration has a section with that name.
hasSection :: SectionName -> Config -> Bool
hasSection = M.member

-- | Returns the section with the given name if it exists in the configuration.
getSection :: SectionName -> Config -> Maybe Section
getSection = M.lookup

-- | Returns a list of the names of all section.
sections :: Config -> [SectionName]
sections = M.keys

-- | Removes the section if it exists.
delSection :: SectionName -> Config -> Config
delSection = M.delete

-- {{{1 options

-- | Returns @True@ if the names section has the option.
hasOption :: SectionName -> OptionName -> Config -> Bool
hasOption sn on cfg = isJust $ getSection sn cfg >>= M.lookup on

-- | Returns the value of the option, if it exists.
getOption :: SectionName -> OptionName -> Config -> Maybe OptionValue
getOption sn on cfg = getSection sn cfg >>= M.lookup on

-- | Returns a list of all options in the section.
options :: SectionName -> Config -> [OptionName]
options sn cfg = maybe [] M.keys (getSection sn cfg)

-- | Sets the value of the option, adding it if it doesn't exist.
setOption :: SectionName -> OptionName -> OptionValue -> Config -> Config
setOption sn on ov cfg = maybe (M.insert sn new_s cfg) (\sec -> M.insert sn (M.insert on ov sec) cfg) s
  where
    s = getSection sn cfg
    new_s = M.insert on ov M.empty

-- | Removes the option if it exists.  Empty sections are pruned.
delOption :: SectionName -> OptionName -> Config -> Config
delOption sn on cfg =
    if sEmptyAfterDelete
        then M.delete sn cfg
        else maybe cfg (\sec -> M.insert sn (M.delete on sec) cfg) s
  where
    s = getSection sn cfg
    sEmptyAfterDelete = maybe True (\sec -> M.empty == M.delete on sec) s

-- | Returns all options and their values of a section.
allItems :: SectionName -> Config -> [(OptionName, OptionValue)]
allItems sn cfg = maybe [] M.toList (getSection sn cfg)
